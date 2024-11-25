#' Generate digital terrain model derived covariates for sample plan development using SAGA GIS.
#'
#' @param dtm An `SpatRast` object or path to a spatial file (.tif) with digital elevation data.
#'      Should be a meter based coordinate reference system. location to raster with elevation data.
#'      Outputs layers will be produced at the same resolution and extent as input dtm
#' @param saga_path saga_path a `character` of the file to the SAGA directory on the analysts system.
#'      Use find_saga_path() function to locate your path.
#' @param out_dir A `character` string of filepath which points to output location. A default
#'      location and name are applied in line with standard workflow.
#' @param layers A `character` vector describing the covariates to be created. By default all three will
#'      be generates `c("mrvbf", "dah", "landform")`.
#' @param sieve_size a `numeric`value to define the number of pixels to be grouped during the sieving process.
#'      default is `10`.
#' @param dah_threshold a `numeric`value to define threshold at which diurnal anisotrophic heating (dah) will
#'      define catergories. Default value is `10`.
#' @param saga_param named list. Parameters that are fed to SAGA MRVBF function. IF supplied it must contain
#'      all parameters specified in the SAGA command.
#'
#' @return path to the output directory where files are written (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' #--- create all SAGA covariates ---#
#' create_samplr_covariate(
#'     dtm = dtm = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,"25m","dem.tif"),
#'     saga_path = find_saga_path()[3]
#'     out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel)
#'}
create_landscape_covariates <- function(dtm = dtm,
                                     saga_path = NULL,
                                     out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel,
                                     layers = c("mrvbf", "dah", "landform"),
                                     sieve_size = 10,
                                     dah_threshold = 0.2,
                                     saga_param = list(
                                       T_SLOPE = 64, TPCTL_V = 6, T_PCTL_R = 2,
                                       P_SLOPE = 4.0, P_PCTL = 3.0, UPDATE = 1,
                                       CLASSIFY = 1, MAX_RES = 100
                                     )) {
  # testing lines
  # dtm = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,"25m","dem.tif")
  # saga_path = "C:/Programs/saga-9.2.0_x64/saga-9.2.0_x64/saga_cmd.exe"
  # out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel
  #
  # create_samplr_covariates(dtm, saga_path)
  # layers = c("mrvbf", "dah", "landform")
  # sieve_size = 10
  # dah_threshold = 0.2
  # saga_param = list(T_SLOPE = 64, TPCTL_V = 6, T_PCTL_R = 2,
  #                  P_SLOPE = 4.0, P_PCTL = 3.0, UPDATE = 1,
  #                  CLASSIFY = 1, MAX_RES = 100)
  # end testing lines



  #--- dtm ---#

  if (inherits(dtm, c("character"))) {
    if (!file.exists(fs::path(dtm))) {
      cli::cli_abort("{.var dtm} must point to an existing dtm file")
    } else {
      dtm <- terra::rast(dtm)
    }
  }

  #--- get resolution of dtm ---#
  rn <- terra::res(dtm)[1]

  #--- SAGA ---#

  if (!is.null(saga_path)) {
    check_saga(saga_path)
    cli::cli_alert_success(
      "Your SAGA connection has been succesfuly set up. Using the {.var {saga_path}}"
    )
  } else {
    check_saga()
    cli::cli_abort("{.var saga_path} must point to the saga_path location on your computer.
                   Please check your program files or equivalent to locate the saga_path.exe file")
  }

  #--- out_dir ---#

  if (!inherits(out_dir, c("character"))) {
    cli::cli_warn("{.var out_dir} must be  path to a file")
  }

  #--- check outputs ---#
  output_dir <- fs::path(out_dir, paste0(rn, "m"), "modules_landscape")

  if (any(!dir.exists(fs::path(output_dir, layers)))) {
    purrr::walk(fs::path(output_dir, layers), dir.create, recursive = TRUE)
  }

  #--- check outputs ---#
  raw_dem_dir <- file.path(output_dir, "dem_raw")

  if (!dir.exists(file.path(raw_dem_dir))) {
    dir.create(raw_dem_dir, recursive = TRUE)
  }

  ## Convert to Saga format for processing ---------------------------------------
  sDTM <- fs::path(raw_dem_dir, "demraw.sdat")

  if (!file.exists(sDTM)) {
    terra::writeRaster(dtm, sDTM, overwrite = TRUE)
    cli::cli_alert_success(
      "Input DEM converted to .sdat and written to {.path {raw_dem_dir}}"
    )
  } else {
    cli::cli_alert_success("{sDTM} already exists")
  }


  ############ Covariate File Names #############################################

  sinksfilled <- fs::path(raw_dem_dir, "sinksfilled.sgrd")

  MRVBF <- fs::path(output_dir, "mrvbf", "mrvbf.sgrd")
  MRRTF <- fs::path(output_dir, "mrvbf", "mrrtf.sgrd")
  dah <- fs::path(output_dir, "dah", "dah.sgrd")

  if (!file.exists(sinksfilled)) {
    sysCMD <- paste(
      saga_path, "ta_preprocessor 5", "-ELEV",
      sDTM,
      "-FILLED", sinksfilled,
      "-MINSLOPE ", 0.1
    )
    system(sysCMD)
  }

  ## 1.  Landscape MRVBF
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html

  if (is.null(saga_param)) {
    saga_param <- list(
      T_SLOPE = 64, TPCTL_V = 6, T_PCTL_R = 2,
      P_SLOPE = 4.0, P_PCTL = 3.0, UPDATE = 1,
      CLASSIFY = 1, MAX_RES = 100
    )
  }

  if (!file.exists(MRVBF)) {
    sysCMD <- paste(
      saga_path, "ta_morphometry 8", "-DEM",
      sinksfilled,
      "-MRVBF", MRVBF,
      "-MRRTF", MRRTF, # Outputs
      "-T_SLOPE", saga_param$T_SLOPE,
      "-T_PCTL_V", saga_param$TPCTL_V,
      "-T_PCTL_R", saga_param$T_PCTL_R, # Default Parameters
      "-P_SLOPE", saga_param$P_SLOPE,
      "-P_PCTL", saga_param$P_PCTL,
      "-UPDATE", saga_param$UPDATE,
      "-CLASSIFY", saga_param$CLASSIFY,
      "-MAX_RES", saga_param$MAX_RES
    )
    system(sysCMD)
  }
  # sieve and then threshold

  mrvbf_r <- terra::rast(fs::path(output_dir, "mrvbf", "mrvbf.sdat")) |>
    terra::sieve(threshold = sieve_size, directions = 8)

  mrvbf_rcrop <- terra::crop(mrvbf_r, dtm)

  terra::writeRaster(mrvbf_rcrop, fs::path(output_dir, "mrvbf_LS.tif"), overwrite = TRUE)




  ## 2.  Landscape Diuranal Anisotropic Heating
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html


  if (!file.exists(dah)) {
    sysCMD <- paste(
      saga_path, "ta_morphometry 12", "-DEM",
      sinksfilled,
      "-DAH", dah, # Output
      "-ALPHA_MAX", 202.5 # Default Parameters
    )
    system(sysCMD)

    # reclass and threshold dah

    dah_r <- terra::rast(gsub(".sgrd", ".sdat", dah))

    # filter based on the 0.3 for Date Cree
    # - review the slope (style into three classes
    #                     - <25% slope or 0.43 radians,
    #                     - 45% slope or 0.43 - 0.78 radians
    #                     - >45% slope
    #                     - once this is stlyed then you can adjust the grouping on the DAH to match
    #                     - Deception = -0.2 to 0.2.
    #                     - Date Creek = -0.3 to 0.3.
    #                     - Peter Hope = -0.2 to 0.2

    m <- c(
      -10, (dah_threshold * -1), 1,
      (dah_threshold * -1), dah_threshold, 2,
      dah_threshold, 10, 3
    )

    rclmat <- matrix(m, ncol = 3, byrow = TRUE)

    rc <- terra::classify(dah_r, rclmat)

    rc <- rc |>
      terra::sieve(threshold = sieve_size, directions = 8)

    rc_crop <- terra::crop(rc, dtm)

    terra::writeRaster(rc_crop, fs::path(output_dir, "dah_LS.tif"), overwrite = TRUE)
  }

  ## 3. Landform Class

  land_class <- create_landform_classes(dtm, scale = 75, sn = 3, ln = 7, n.classes = "six")

  land_class <- land_class |>
    terra::sieve(threshold = sieve_size, directions = 8) |>
    terra::subst(from = 0, to = NA)

  land_class <- terra::crop(land_class, dtm)
  names(land_class) <- "landclass"

  terra::writeRaster(land_class, file.path(output_dir, "landform_LS.tif"), overwrite = TRUE)

  # remove temp Saga files
  # unlink(paste(output, "saga", sep = "/"), recursive = TRUE)
  # delete the files

  ## decide if want to delete these temp files.

  # to_delete <- grep(".tif", list.files(file.path(output_dir), full.names = TRUE), value = T, invert = TRUE)
  # file.remove(to_delete)

  cli::cat_line()
  cli::cli_alert_success(
    "Layers downloaded and to written to {.path {out_dir}}"
  )
  invisible(out_dir)
}


create_landform_classes <- function(dtm,
                                    scale = 75,
                                    sn = 3,
                                    ln = 7,
                                    n.classes = "ten",
                                    add.tpi = FALSE,
                                    stand.tpi = FALSE) {
  # calculate the slope from the input DTM, to be used for either the six or ten class slope position
  slp <- terra::terrain(dtm, v = "slope", unit = "degrees", neighbors = 8)

  win <- "rectangle"

  if (n.classes == "six") {
    # calculate the tpi using spatialEco::tpi function
    tp <- spatialEco::tpi(dtm, scale = scale, win = win, normalize = TRUE)

    # define the six classes on the basis of thresholds of tp and slope
    valley <- (tp <= -1)
    valley[stats::na.omit(valley)] <- 1

    lower.slp <- (tp > -1 & tp <= -0.5)
    lower.slp[stats::na.omit(lower.slp)] <- 2

    flat.slp <- (tp > -0.5 & tp < 0.5) & (slp <= 5)
    flat.slp[stats::na.omit(flat.slp)] <- 3

    middle.slp <- (tp > -0.5 & tp < 0.5) & (slp > 5)
    middle.slp[stats::na.omit(middle.slp)] <- 4

    upper.slp <- (tp > 0.5 & tp <= 1)
    upper.slp[stats::na.omit(upper.slp)] <- 5

    ridge <- (tp > 1)
    ridge[stats::na.omit(ridge)] <- 6

    # consolidate into single layer
    land_class <- valley + lower.slp + flat.slp + middle.slp + upper.slp + ridge
  } else {
    # calculate two standardized tpi, one with small neighbour, one with large neighbour
    sn <- spatialEco::tpi(dtm, scale = sn, win = win, normalize = TRUE)
    ln <- spatialEco::tpi(dtm, scale = ln, win = win, normalize = TRUE)

    # define the ten classes on the basis of thresholds of sn, sl, and slope
    canyons <- (sn <= -1) & (ln <= -1)
    canyons[stats::na.omit(canyons)] <- 1

    midslope.dr <- (sn <= -1) & (ln > -1 & ln < 1)
    midslope.dr[stats::na.omit(midslope.dr)] <- 2

    upland.dr <- (sn <= -1) & (ln >= 1)
    upland.dr[stats::na.omit(upland.dr)] <- 3

    us.valley <- (sn > -1 & sn < 1) & (ln <= -1)
    us.valley[stats::na.omit(us.valley)] <- 4

    plains <- (sn > -1 & sn < 1) & (ln > -1 & ln < 1) & (slp <= 5)
    plains[stats::na.omit(plains)] <- 5

    open.slp <- (sn > -1 & sn < 1) & (ln > -1 & ln < 1) & (slp > 5)
    open.slp[stats::na.omit(open.slp)] <- 6

    upper.slp <- (sn > -1 & sn < 1) & (ln >= 1)
    upper.slp[stats::na.omit(upper.slp)] <- 7

    local.rdg <- (sn >= 1) & (ln <= -1)
    local.rdg[stats::na.omit(local.rdg)] <- 8

    midslp.rdg <- (sn >= 1) & (ln > -1 & ln < 1)
    midslp.rdg[stats::na.omit(midslp.rdg)] <- 9

    mount.top <- (sn >= 1) & (ln >= 1)
    mount.top[stats::na.omit(mount.top)] <- 10

    # consolidate into single layer
    land_class <- canyons + midslope.dr + upland.dr + us.valley + plains + open.slp + upper.slp + local.rdg + midslp.rdg + mount.top
  }

  return(land_class)
}
