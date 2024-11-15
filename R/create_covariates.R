#' Generate digital terrain model derived covariates using SAGA GIS.
#'
#' Generates suit of dem derived covariates usind SAGA GIS executable file.
#' Depending on your system the path to `saga_path` may need to be specified.
#'
#' @param dtm An `SpatRast` object or path to a spatial file (.tif) with digital elevation data.
#'      Should be a meter based coordinate reference system. location to raster with elevation data.
#'      Outputs layers will be produced at the same resolution and extent as input dtm
#' @param saga_path a `character` of the file to the SAGA directory on the analysts system.
#'         Use find_saga_path() to locate the approraite saga installation on your machine.
#' @param out_dir afile.path. Directory where covariates will be written. If not
#'     specified uses the default from the `fid` folder structure. If files already
#'     exists they will NOT be overwritten.
#' @param layers A `character` vector. Covariates to be created. Default is `"all"`.
#' @param tile A `logical` to define if the dtm needs to be run in tiles.
#'          Currently placeholder to be developed.
#'
#' @return path to the output directory where files are written (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' #--- create all SAGA covariates ---#
#' create_covariates(
#'   dtm = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,"25m","dem.tif",
#'   saga_path = find_saga_path[3]
#'   layers = "all",
#'   out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel
#'  )
#'}
create_covariates <- function(dtm = NULL,
                              saga_path = NULL,
                              out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel,
                              layers = "all",
                              tile = FALSE # this is placeholder for tiled outputs
) {
  #  # start testing
  #dtm = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,"25m","dem.tif")
  #saga_path = "C:/Programs/saga-9.2.0_x64/saga-9.2.0_x64/saga_cmd.exe"
  #out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel
  #layers = "all"
  # tile = FALSE

  #moddir <- fs::path_package("PEMprepr", "extdata/saga_module_depends.csv")
  #moddir <- read.csv("saga_module_depends.csv")

  #  # end testing

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


  #--- layers ---#

  layer_options <- c(
    "sinksfilled", "sinkroute", "dem_preproc", "slope_aspect_curve",
    "tcatchment", "tca", "scatchment", "twi", "channelsnetwork",
    "overlandflow", "overlandflow2", "multiresflatness", "multiresflatness2",
    "multiresflatness3", "tri", "convergence", "openness",
    "dah", "tpi", "ridgevalley", "mrn", "flowaccumulation",
    "slopelength", "flowaccumulation2", "flowaccumulation3",
    "flowpathlength", "flowpathlength2", "flowpathlength3", "lsfactor",
    "solarrad", "convexity", "vertdistance", "tci_low",
    "swi", "windexp", "texture", "protection", "vrm",
    "mbi", "mscale_tpi", "relposition", "slopecurvatures",
    "steepestslope", "upslopearea"
  )


  if (!inherits(layers, "character")) {
    cli::cli_abort("{.var layers} must be a character string of the names of variables")
  }

  if (isTRUE(layers == "all")) {
    layers <- layer_options
  }

  # check the layers are correct
  errorl <- setdiff(layers, layer_options)

  if (length(errorl) > 0) {
    cli::cli_abort("Layers contains covariates that are not a valid options, please review the following layer inputs: { errorl}")
  }


  # check which modules to run and appropriate dependants are also run in order
  moddir <- utils::read.csv(fs::path_package("PEMprepr", "extdata/saga_module_depends.csv"))

  moddirs <- dplyr::filter(moddir, stringr::str_detect(moddir$module, paste(layers, collapse = "|")))
  mods <- moddirs$module
  deps <- setdiff(unique(c(moddirs$dep1, moddirs$dep2)), moddirs$module)
  deps <- deps[!grepl("^\\s*$", deps)]
  if (length(deps > 0)) {
    mods <- c(moddirs$module, deps)
  }


  # order the modules to be run
  layers_to_call <- mods[order(match(mods, moddir$module))]


  # if tiling is true give a message for artifact covariates

  if (tile) {
    artifacts <- dplyr::filter(moddir, stringr::str_detect(moddir$module, paste(mods, collapse = "|")))
    artifacts_cov <- dplyr::filter(artifacts, artifacts$tile_artifact == TRUE)
    artifacts_cov$module

    if (length(artifacts_cov$module) > 0) {
      cli::cli_alert_warning(
        "The following layers produce edge artifacts when using the tiling process, please review the output before use : {artifacts_cov$module}"
      )
    }
  }


  #--- begin procesing ---#

  #--- extract dtm basename for naming convention ---#
  #--- helps to ensure tiles are matched properly during parallel processing ---#

  # nm <- tools::file_path_sans_ext(basename(dtm))


  # OUTPUTS: ------------------------------------------------------------

  #--- check outputs ---#
  output_dir <- fs::path(out_dir, paste0(rn, "m"), "modules")

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


  #--- covariate file names  call lyr$<NAME> to call from utilities function ---#

  layers <- layers_to_call

  ############################### BEGIN PROCESSING ###############################

  ####### >> 1 -- Fill Sinks XXL (Wang and Liu)  -----------------------------

  # Fill sinks in dem to prepare base DEM for other layers:

  #### STEP 1: preprocess DEM

  ## http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_5.html
  ## Module Fill Sinks XXL (Wang & Liu)

  # This module uses an algorithm proposed by Wang & Liu to identify and fill surface depressions in digital elevation models.
  # The method was enhanced to allow the creation of hydrologic sound elevation models, i.e. not only to fill the depression(s) but also to preserve a downward slope along the flow path. If desired, this is accomplished by preserving a minimum slope gradient (and thus elevation difference) between cells.
  # This version of the module is designed to work on large data sets (e.g. LIDAR data), with smaller datasets you might like to check out the fully featured standard version of the module.

  if ("sinksfilled" %in% layers) {
    sinksfilled <- fs::path(output_dir, "sinksfilled", "sinksfilled.sgrd")

    if (!file.exists(sinksfilled)) {
      sysCMD <- paste(
        saga_path, "ta_preprocessor 5", "-ELEV",
        sDTM,
        "-FILLED", sinksfilled,
        "-MINSLOPE ", 0.1
      )
      system(sysCMD)
    }
  }


  # Generate sink drainage route detection layer to use in preprocess DEM
  # http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_1.html

  if ("sinkroute" %in% layers) {
    sinkroute <- fs::path(output_dir, "sinkroute", "sinkroute.sgrd")

    if (!file.exists(sinkroute)) {
      sysCMD <- paste(
        saga_path, "ta_preprocessor 1",
        "-ELEVATION", sDTM,
        "-SINKROUTE", sinkroute
      )
      system(sysCMD)
    }
  }

  # preproces DEM version 2:  fills sinks (input requires DEM + sink detection layer
  # generated above)/
  # http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_2.html


  if ("dem_preproc" %in% layers) {
    dem_preproc <- fs::path(output_dir, "dem_preproc", "dem_preproc.sgrd")

    if (!file.exists(dem_preproc)) {
      sysCMD <- paste(
        saga_path, "ta_preprocessor 2",
        "-DEM", sDTM,
        "-SINKROUTE", sinkroute,
        "-DEM_PREPROC", dem_preproc,
        "-METHOD", 1,
        "-THRESHOLD", 0
      )
      system(sysCMD)
    }
  }

  ##### >> 2 -- Slope Aspect and Curvature -------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html

  if ("slope_aspect_curve" %in% layers) {
    slope <- fs::path(output_dir, "slope_aspect_curve", "slope.sgrd")
    aspect <- fs::path(output_dir, "slope_aspect_curve", "aspect.sgrd")
    gencurve <- fs::path(output_dir, "slope_aspect_curve", "gencurve.sgrd")
    totcurve <- fs::path(output_dir, "slope_aspect_curve", "totcurve.sgrd")
    ls <- c(slope, aspect, gencurve, totcurve)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 0", "-ELEVATION",
        sinksfilled,
        "-SLOPE", slope,
        "-ASPECT", aspect, # Outputs
        "-C_GENE", gencurve,
        "-C_TOTA", totcurve, # Outputs
        "-METHOD", 6,
        "-UNIT_SLOPE", 0,
        "-UNIT_ASPECT", 0 # Default Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 3 -- Total Catchment Area --------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_0.html

  if ("tcatchment" %in% layers) {
    tcatchment <- fs::path(output_dir, "tcatchment", "tcatchment.sgrd")

    if (!file.exists(tcatchment)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 0", "-ELEVATION",
        sinksfilled,
        "-FLOW", tcatchment, # Output
        "-METHOD", 4 # Default Parameters
      )
      system(sysCMD)
    }
  }
  ## Note this is the same as flow Accumulation top down (#19 although less outputs included here that are included in #19


  #####################
  # Still to run - using tCatchement instead for the base tca raster for other inputs
  # This is not working properly but is the equivalent to tca - needs more work

  #  #   Following this method for calculating topographic wetness index:
  # #    https://gracilis.carleton.ca/CUOSGwiki/index.php/Enhanced_Wetness_Modelling_in_SAGA_GIS
  # #    See this paper as well for discussion on different ways to calculate TWI:
  # #   https://link.springer.com/article/10.1186/s40965-019-0066-y

  ##### >> 3a -- Total Catchment Area --------------------------------------

  if ("tca" %in% layers) {
    tca <- fs::path(output_dir, "tca", "tca1.sgrd")
    flowlength4 <- fs::path(output_dir, "tca", "flowlength1.sgrd")
    ls <- c(tca, flowlength4)


    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 1",
        "-ELEVATION",
        sinksfilled,
        # file.path(gsub("sdat","sgrd", sDTM)),
        "-FLOW", tca,
        "-FLOW_LENGTH", flowlength4,
        "-FLOW_UNIT", 1,
        "-METHOD", 3
      )
      system(sysCMD)
    }
  }

  ##### >> 4 -- Flow Width and Specific Catchment Area --------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_19.html

  if ("scatchment" %in% layers) {
    scatchment <- fs::path(output_dir, "scatchment", "scatchment.sgrd")

    if (!file.exists(scatchment)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 19", "-DEM", sinksfilled, # Input from 1
        "-SCA", scatchment, # Output
        "-TCA", tcatchment, # Input from 2
        "-METHOD", 1 # Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 5 -- Topographic Wetness Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_20.html

  if ("twi" %in% layers) {
    twi <- fs::path(output_dir, "twi", "twi.sgrd")

    if (!file.exists(twi)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 20",
        "-SLOPE", slope, # Input from 11
        "-AREA", scatchment, # Input from 3
        "-TWI", twi, # Output
        "-CONV", 1,
        "-METHOD", 1
      )
      system(sysCMD)
    }
  }

  ##### >> 6 -- Channel Network -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_0.html
  # https://sourceforge.net/projects/saga-gis/files/SAGA%20-%20Documentation/SAGA%20Documents/SagaManual.pdf/download

  if ("channelsnetwork" %in% layers) {
    channelsnetwork <- fs::path(output_dir, "channelsnetwork", "channelsnetwork.sgrd")

    if (!file.exists(channelsnetwork)) {
      sysCMD <- paste(
        saga_path, "ta_channels 0",
        "-ELEVATION", sinksfilled, # Input from 1
        "-CHNLNTWRK", channelsnetwork, # Output
        "-INIT_GRID", tcatchment, # Input from 2
        "-INIT_VALUE", 1000000,
        "-INIT_METHOD", 2, # Based on SAGA Manual Documentation, p. 119
        "-DIV_CELLS", 5.0,
        "-MINLEN", 10.0 # Default Parameters
      )
      system(sysCMD)
    }
  }


  ##### >> 7 -- Overland Flow Distance to Channel Network -----------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_4.html

  if ("overlandflow" %in% layers) {
    hdistance <- fs::path(output_dir, "overlandflow", "hdist.sgrd")
    vdistance <- fs::path(output_dir, "overlandflow", "vdist.sgrd")

    ls <- c(hdistance, vdistance)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_channels 4",
        "-ELEVATION", sinksfilled, # Input from 1
        "-CHANNELS", channelsnetwork, # Input from 4
        "-DISTANCE", hdistance,
        "-DISTVERT", vdistance, # Outputs
        "-METHOD", 1,
        "-BOUNDARY", 1 # Parameters
      )
      system(sysCMD)
    }
  }

  # note distnob created using XML script with no boundary. This shows NA for areas on the edge where
  # metrics cannot be calculated)

  if ("overlandflow2" %in% layers) {
    hdistancenob <- fs::path(output_dir, "overlandflow2", "hdistnob.sgrd")
    vdistancenob <- fs::path(output_dir, "overlandflow2", "vdistnob.sgrd")

    ls <- c(hdistancenob, vdistancenob)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_channels 4",
        "-ELEVATION", sinksfilled, # Input from 1
        "-CHANNELS", channelsnetwork, # Input from 4
        "-DISTANCE", hdistancenob,
        "-DISTVERT", vdistancenob, # Outputs
        "-METHOD", 1,
        "-BOUNDARY", 0 # Parameters
      )
      system(sysCMD)
    }
  }


  ##### >> 8 -- MRVBF -----------------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html
  if ("multiresflatness" %in% layers) {
    MRVBF <- fs::path(output_dir, "multiresflatness", "mrvbf.sgrd")
    MRRTF <- fs::path(output_dir, "multiresflatness", "mrrtf.sgrd")

    ls <- c(MRVBF, MRRTF)

    if (all(!file.exists(ls))) {
      # use defaul parameters
      sysCMD <- paste(
        saga_path, "ta_morphometry 8", "-DEM",
        # file.path(gsub("sdat","sgrd", sDTM)),
        sinksfilled,
        "-MRVBF", MRVBF,
        "-MRRTF", MRRTF, # Outputs
        "-T_SLOPE", 16,
        "-T_PCTL_V", 0.4,
        "-T_PCTL_R", 0.35, # Default Parameters
        "-P_SLOPE", 4.0,
        "-P_PCTL", 3.0,
        "-UPDATE", 0,
        "-CLASSIFY", 0,
        "-MAX_RES", 100
      )
      system(sysCMD)
    }
  }
  # Test a Variety of paramter and method versions.
  # Note these need to be converted from XML chain format to standard format

  # tested a number of MRVBF options for the t-slope parameter #ie
  # use dem_preproces for input and lowered the slope parameter from 15 to 10
  #
  if ("multiresflatness2" %in% layers) {
    MRVBF2 <- fs::path(output_dir, "multiresflatness2", "mrvbf2.sgrd")
    MRRTF2 <- fs::path(output_dir, "multiresflatness2", "mrrtf2.sgrd")

    ls <- c(MRVBF2, MRRTF2)

    if (all(!file.exists(ls))) {
      #  Adjust parameters -  Option 2.
      sysCMD <- paste(
        saga_path, "ta_morphometry 8", "-DEM",
        # file.path(gsub("sdat","sgrd", sDTM)),
        sinksfilled,
        "-MRVBF", MRVBF2,
        "-MRRTF", MRRTF2,
        "-T_SLOPE", 10,
        "-T_PCTL_V", 0.4,
        "-T_PCTL_R", 0.35,
        "-P_SLOPE", 4.0,
        "-P_PCTL", 3.0,
        "-UPDATE", 0,
        "-CLASSIFY", 0,
        "-MAX_RES", 100
      )
      system(sysCMD)
    }
  }

  ## dropped 2022-11-08 PEMr workshop ... not included in GP's 02a_DEM_SpatialLayer_Prep.R
  # #  Adjust parameters -  Option 3.
  #
  # if ("multiresflatness3" %in% layers) {
  #
  #   MRVBF5 <- "mrvbf5.sgrd"
  #   MRRTF5 <- "mrrtf5.sgrd"
  #
  #   sysCMD <- paste(saga_path, "ta_morphometry 8", "-DEM",
  #                   # file.path(gsub("sdat","sgrd", sDTM)),
  #                   sDTM,
  #                   "-MRVBF", MRVBF5,
  #                   "-MRRTF", MRRTF5,
  #                   "-T_SLOPE", 64,
  #                   "-T_PCTL_V", 0.2,
  #                   "-T_PCTL_R", 0.6,
  #                   "-P_SLOPE", 4.0,
  #                   "-P_PCTL", 3.0,
  #                   "-UPDATE", 0,
  #                   "-CLASSIFY", 0,
  #                   "-MAX_RES", 100
  #   )
  #   system(sysCMD)
  # }
  # tested other options:

  #     mrvbf = paste0(
  #       "<tool library='ta_morphometry' tool='8' name='Multiresolution Index of Valley Bottom Flatness (MRVBF)'>
  #          <input id='DEM'>dem_preproc</input>
  #          <output id='MRVBF'>mrvbf4</output>
  #          <output id='MRRTF'>mrrtf4</output>
  #          <option id='T_SLOPE'>64</option>
  #          <option id='T_PCTL_V'>0.600000</option>
  #          <option id='T_PCTL_R'>0.200000</option>
  #          <option id='P_SLOPE'>4.000000</option>
  #          <option id='P_PCTL'>3.000000</option>
  #          <option id='UPDATE'>false</option>
  #          <option id='CLASSIFY'>false</option>
  #          <option id='MAX_RES'>100.000000</option>
  #      </tool>"
  #     ),
  #
  #



  ##### >> 9 -- Terrain Ruggedness Index ----------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_16.html

  if ("tri" %in% layers) {
    tri <- fs::path(output_dir, "tri", "tri.sgrd")

    if (!file.exists(tri)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 16",
        "-DEM", sinksfilled,
        "-TRI", tri, # Output
        "-MODE", 0,
        "-RADIUS", 3.0,
        "-DW_WEIGHTING", 0 # Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 10 -- Convergence Index -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_1.html


  if ("convergence" %in% layers) {
    convergence <- fs::path(output_dir, "convergence", "convergence.sgrd")

    if (!file.exists(convergence)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 1",
        "-ELEVATION ", sinksfilled,
        "-RESULT", convergence, # Output
        "-METHOD", 1,
        "-NEIGHBOURS", 1 # Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 11 -- openness --------------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_5.html

  if ("openness" %in% layers) {
    opos <- fs::path(output_dir, "openness", "open_pos.sgrd")
    oneg <- fs::path(output_dir, "openness", "open_neg.sgrd")

    ls <- c(opos, oneg)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_lighting 5", "-DEM",
        sinksfilled,
        "-POS", opos,
        "-NEG", oneg, # Outputs
        "-RADIUS", 1000,
        "-METHOD", 0,
        "-DLEVEL", 3,
        "-NDIRS", 8
      )
      system(sysCMD)
    }
  }

  ##### >> 12 -- Diuranal Anisotropic Heating -----------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html

  if ("dah" %in% layers) {
    dah <- fs::path(output_dir, "dah", "dah.sgrd")

    if (!file.exists(dah)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 12", "-DEM",
        sinksfilled,
        "-DAH", dah, # Output
        "-ALPHA_MAX", 202.5 # Default Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 13 -- Topographic Position Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_18.html

  # Note "ta_morphology 18" -DW_IDW_OFFSET no longer a parameter in SAGA v8.4.1 (newest stable release).
  # add an if else statement

  # if (v < 8.4.1 ) {
  #    warning("SAGA-GIS is less that 7.6.  Not all covariates will generate.  Upgrade your SAGA, visit https://sourceforge.net/projects/saga-gis/files/")
  #  }


  if ("tpi" %in% layers) {
    tpi <- fs::path(output_dir, "tpi", "tpi.sgrd")


    if (!file.exists(tpi)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 18",
        "-DEM", sinksfilled,
        "-TPI", tpi, # Output
        "-STANDARD", 0,
        "-RADIUS_MIN", 0,
        "-RADIUS_MAX", 100, # Default Parameters
        "-DW_WEIGHTING", 0,
        "-DW_IDW_POWER", 1,
        # "-DW_IDW_OFFSET", "1", # NO LONGER A PARAMETER IN SAGA v8.4.1
        "-DW_BANDWIDTH", 75
      )
      system(sysCMD)
    }
  }



  #### >> 14 -- Valley Depth -------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_7.html

  if ("ridgevalley" %in% layers) {
    val_depth <- fs::path(output_dir, "ridgevalley", "val_depth.sgrd")
    ridgelevel <- fs::path(output_dir, "ridgevalley", "rid_level.sgrd")


    ls <- c(val_depth, ridgelevel)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_channels 7",
        "-ELEVATION", sinksfilled, # input DEM
        "-VALLEY_DEPTH", val_depth, # output Valley Depth
        "-RIDGE_LEVEL", ridgelevel, # output Ridge Level
        "-THRESHOLD", 1,
        "-NOUNDERGROUND", 1,
        "-ORDER", 4
      )
      system(sysCMD)
    }
  }


  #### >> 15 -- Melton Ruggedness Number -------------------------- ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_23.html

  if ("mrn" %in% layers) {
    mrncatchment <- fs::path(output_dir, "mrn", "mrn_area.sgrd")
    mrnmaxheight <- fs::path(output_dir, "mrn", "mrn_mheight.sgrd")
    mrn <- fs::path(output_dir, "mrn", "mrn.sgrd")

    ls <- c(mrncatchment, mrnmaxheight, mrn)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 23",
        "-DEM", sinksfilled, # input DEM
        "-AREA", mrncatchment, # output MRN Catchment
        "-ZMAX", mrnmaxheight, # output MRN Max Height
        "-MRN", mrn # output MRN
      )
      system(sysCMD)
    }
  }

  #### >> 16 -- Flow Accumulation (Flow Tracing)  --------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_2.html

  if ("flowaccumulation" %in% layers) {
    flowaccumft <- fs::path(output_dir, "flowaccumulation", "flow_accum_ft.sgrd")
    meanovcatch <- fs::path(output_dir, "flowaccumulation", "meanovcatch.sgrd")
    accummaterial <- fs::path(output_dir, "flowaccumulation", "accummaterial.sgrd")

    ls <- c(flowaccumft, meanovcatch, accummaterial)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 2",
        "-ELEVATION", sinksfilled, # input DEM
        "-FLOW", flowaccumft, # output Flow Accumulation
        "-VAL_MEAN", meanovcatch, # output Mean over Catchment
        "-ACCU_TOTAL", accummaterial, # output Accumulated Material
        "-FLOW_UNIT", 1,
        "-METHOD", 1,
        "-MINDQV", 0
      )
      system(sysCMD)
    }
  }

  #### >> 17 -- Slope Length ---------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_7.html

  if ("slopelength" %in% layers) {
    #
    slopelength <- fs::path(output_dir, "slopelength", "slength.sgrd")

    if (!file.exists(slopelength)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 7",
        "-DEM", sinksfilled, # input DEM
        "-LENGTH", slopelength # output Slope Length
      )
      system(sysCMD)
    }
  }


  #### >> 18 -- Flow Accumulation (Parallelizable) -------------------
  ## this tool doesn't seem to exist - SAGA version issue?
  # # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_29.html

  if ("flowaccumulation2" %in% layers) {
    flowaccump <- fs::path(output_dir, "flowaccumulation2", "flow_accum_p.sgrd")

    if (!file.exists(flowaccump)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 29",
        "-DEM", sinksfilled, # input DEM
        "-FLOW", flowaccump, # output Flow Accumulation
        "-METHOD", 2,
        "-CONVERGENCE", 1.1
      )
      system(sysCMD)
    }
  }

  #### >> 19 -- Flow Accumulation (Top-Down) ---------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_0.html

  if ("flowaccumulation3" %in% layers) {
    flowaccumtd <- fs::path(output_dir, "flowaccumulation3", "flow_accum_td.sgrd")
    meanovcatchTD <- fs::path(output_dir, "flowaccumulation3", "meanovcatchTD.sgrd")
    accummaterialTD <- fs::path(output_dir, "flowaccumulation3", "accummaterialTD.sgrd")
    flowpathlenTD <- fs::path(output_dir, "flowaccumulation3", "flowpathlenTD.sgrd")

    ls <- c(flowaccumtd, meanovcatchTD, accummaterialTD, flowpathlenTD)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 0",
        "-ELEVATION", sinksfilled, # input DEM
        "-FLOW", flowaccumtd, # output Flow Accumulation
        "-VAL_MEAN", meanovcatchTD, # output Mean over Catchment
        "-ACCU_TOTAL", accummaterialTD, # output Accumulated Material
        "-FLOW_LENGTH", flowpathlenTD, # output Flow Path Length
        "-FLOW_UNIT", 1,
        "-METHOD", 4,
        "-LINEAR_DO", 1,
        "-LINEAR_MIN", 500,
        "-CONVERGENCE", 1.1
      )
      system(sysCMD)
    }
  }

  #### >> 20 -- Stream Power Index ----------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_21.html

  # Not included as binary output

  # StreamPower = "spower.sgrd"
  # StreamPower = file.path(tmpOut, StreamPower)
  # sysCMD = paste(saga_path, "ta_hydrology 21",
  #                "-SLOPE", slope,                    # input Slope
  #                "-AREA", tcatchment,                # input Catchment Area
  #                "-SPI", StreamPower,               # output Stream Power Index
  #                "-CONV", 0
  # )
  # system(sysCMD)


  #### >> 21 -- Maximum Flow Path Length ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html

  if ("flowpathlength" %in% layers) {
    flowpathlength <- fs::path(output_dir, "flowpathlength", "max_fp_l.sgrd")

    if (!file.exists(flowpathlength)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 27",
        "-ELEVATION", sinksfilled, # input DEM
        "-DISTANCE", flowpathlength, # output Max Flow Path Length
        "-DIRECTION", 0
      )
      system(sysCMD)
    }
  }


  #### >> 21a -- Maximum Flow Path Length ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html
  #
  if ("flowpathlength2" %in% layers) {
    flowpathlength2 <- fs::path(output_dir, "flowpathlength2", "max_fp_l2.sgrd")

    if (!file.exists(flowpathlength2)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 27",
        "-ELEVATION", sinksfilled, # input DEM
        "-DISTANCE", flowpathlength2, # output Max Flow Path Length
        "-DIRECTION", 1
      )
      system(sysCMD)
    }
  }

  #### >> 22 -- Slope Limited Flow Accumulation -------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_26.html

  if ("flowpathlength3" %in% layers) {
    flowpathlength3 <- fs::path(output_dir, "flowpathlength3", "max_fp_l3.sgrd")

    if (!file.exists(flowpathlength3)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 26",
        "-DEM", sinksfilled, # input DEM
        "-FLOW", flowpathlength3, # output Flow Accumulation
        "-SLOPE_MIN", 0,
        "-SLOPE_MAX", 5,
        "-B_FLOW", 0
      )
      system(sysCMD)
    }
  }
  #### >> 23 -- LS Factor -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_22.html


  if ("lsfactor" %in% layers) {
    lsfactor <- fs::path(output_dir, "lsfactor", "ls_factor.sgrd")

    if (!file.exists(lsfactor)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 22",
        "-SLOPE", slope, # input Slope
        "-AREA", tcatchment, # input Catchment Area
        "-LS", lsfactor, # output LS Factor
        "-CONV", 0,
        "-METHOD", 0,
        "-EROSIVITY", 1,
        "-STABILITY", 0
      )
      system(sysCMD)
    }
  }



  #### >> 24 -- Solar covariates  -----------------------------------------
  # solar direct and diffuse solar radiation
  # adjust min and max limits to 4am and 22 pm to reduce processing time
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_2.html
  # Calculation of potential incoming solar radiation (insolation). Times of sunrise/sunset will only be calculated if time span is set to single day.
  if ("solarrad" %in% layers) {
    DirInsol <- fs::path(output_dir, "solarrad", "direinso.sgrd")
    DifInsol <- fs::path(output_dir, "solarrad", "diffinso.sgrd")

    ls <- c(DirInsol, DifInsol)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_lighting 2",
        "-GRD_DEM",
        # file.path(gsub("sdat","sgrd", sDTM)),# Input DTM
        sinksfilled,
        "-GRD_DIRECT", DirInsol,
        "-GRD_DIFFUS", DifInsol, # Outputs
        "-SOLARCONST", 1367,
        "-LOCALSVF", 1,
        "-SHADOW", 0, # Parameters
        "-LOCATION", 1,
        "-PERIOD", 2,
        "-DAY", "2018-02-15",
        "-DAY_STOP", "2019-02-15",
        "-DAYS_STEP", 30,
        "-HOUR_RANGE_MIN", 4,
        "-HOUR_RANGE_MAX", 22,
        "-HOUR_STEP", 0.5,
        "-METHOD", 2,
        "-LUMPED", 70
      )
      system(sysCMD)
    }
  }

  #### >> 25 -- Terrain Surface convexity ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_21.html

  if ("convexity" %in% layers) {
    convexity <- fs::path(output_dir, "convexity", "convexity.sgrd")

    if (!file.exists(convexity)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 21",
        "-DEM", sinksfilled, # input DEM
        "-CONVEXITY", convexity, # output Convexity
        "-KERNEL", 0,
        "-TYPE", 0,
        "-EPSILON", 0,
        "-SCALE", 10,
        "-METHOD", 1,
        "-DW_WEIGHTING", 0,
        "-DW_IDW_POWER", 2,
        "-DW_BANDWIDTH", 1
      )
      system(sysCMD)
    }
  }

  #### >> 26 -- Vertical Distance to Channel Network ------------- ## froze - maybe just very slow?
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_3.html

  if ("vertdistance" %in% layers) {
    vertdistance <- fs::path(output_dir, "vertdistance", "vert_dis.sgrd")

    if (!file.exists(vertdistance)) {
      sysCMD <- paste(
        saga_path, "ta_channels 3",
        "-ELEVATION", sinksfilled, # input DEM
        "-CHANNELS", channelsnetwork, # input Channel Network
        "-DISTANCE", vertdistance, # output
        "-THRESHOLD", 1,
        "-NOUNDERGROUND", 1
      )
      system(sysCMD)
    }
  }


  #### >> 27 -- TCI Low -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_24.html

  if ("tci_low" %in% layers) {
    tci_low <- fs::path(output_dir, "tci_low", "tci_low.sgrd")

    if (!file.exists(tci_low)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 24",
        "-DISTANCE", vertdistance, # input Vertical Distance to Channel Network
        "-TWI", twi, # input TWI
        "-TCILOW", tci_low # output TCI Low
      )
      system(sysCMD)
    }
  }

  #### >> 28 -- SAGA Wetness Index -------------------------------- ## works but VERY slow (~18 hours)
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_15.html

  if ("swi" %in% layers) {
    catchmentarea <- fs::path(output_dir, "swi", "swi_area.sgrd")
    catchmentslope <- fs::path(output_dir, "swi", "swi_slope.sgrd")
    modcatchmentarea <- fs::path(output_dir, "swi", "swi_area_mod.sgrd")
    topowetindex <- fs::path(output_dir, "swi", "swi_twi.sgrd")

    ls <- c(catchmentarea, catchmentslope, modcatchmentarea, topowetindex)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 15",
        "-DEM", sinksfilled, # input DEM
        "-AREA", catchmentarea, # output Catchment Area
        "-SLOPE", catchmentslope, # output Catchment Slope
        "-AREA_MOD", modcatchmentarea, # output Modified Catchment Area
        "-TWI", topowetindex, # output TWI
        "-SUCTION", 10,
        "-AREA_TYPE", 1,
        "-SLOPE_TYPE", 1,
        "-SLOPE_MIN", 0,
        "-SLOPE_OFF", 0.1,
        "-SLOPE_WEIGHT", 1
      )
      system(sysCMD)
    }
  }

  #### >> 29 -- Wind Exposition Index ------------------------------ ## works but VERY slow
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_27.html

  if ("windexp" %in% layers) {
    windexp <- fs::path(output_dir, "windexp", "wind_exp_index.sgrd")

    if (!file.exists(windexp)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 27",
        "-DEM", sinksfilled, # input DEM
        "-EXPOSITION", windexp, # output Wind Exposition Index
        "-MAXDIST", 300,
        "-STEP", 15,
        "-ACCEL", 1.5
      )
      system(sysCMD)
    }
  }

  #### >> 30 -- Terrain Surface Texture -----------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_20.html

  if ("texture" %in% layers) {
    texture <- fs::path(output_dir, "texture", "texture.sgrd")

    if (!file.exists(texture)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 20",
        "-DEM", sinksfilled, # input DEM
        "-TEXTURE", texture, # output Terrain Surface Texture
        "-EPSILON", 1,
        "-SCALE", 10,
        "-METHOD", 1,
        "-DW_WEIGHTING", 0,
        "-DW_IDW_POWER", 2,
        "-DW_BANDWIDTH", 1
      )
      system(sysCMD)
    }
  }

  #### >> 31 -- Morphometric Protection Index ----------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_7.html

  if ("protection" %in% layers) {
    protection <- fs::path(output_dir, "protection", "protection.sgrd")

    if (!file.exists(protection)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 7",
        "-DEM", sinksfilled, # input DEM
        "-PROTECTION", protection, # output Morphometric Protection Index
        "-RADIUS", 2000
      )
      system(sysCMD)
    }
  }


  #### >> 32 -- Vector Ruggedness Measure ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_17.html

  if ("vrm" %in% layers) {
    vrm <- fs::path(output_dir, "vrm", "vrm.sgrd")

    if (!file.exists(vrm)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 17",
        "-DEM", sinksfilled, # input DEM
        "-VRM", vrm, # output Vector Ruggedness Measure
        "-MODE", 1,
        "-DW_WEIGHTING", 0,
        "-DW_IDW_POWER", 2,
        "-DW_BANDWIDTH", 1
      )
      system(sysCMD)
    }
  }

  #### >> 33 -- Mass Balance Index ----------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_10.html

  if ("mbi" %in% layers) {
    mbi <- fs::path(output_dir, "mbi", "mbi.sgrd")

    if (!file.exists(mbi)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 10",
        "-DEM", sinksfilled, # input DEM
        "-HREL", vertdistance, # input Vertical Distance to Channel Network
        "-MBI", mbi, # output Mass Balance Index
        "-TSLOPE", 15,
        "-TCURVE", 0.01,
        "-THREL", 15
      )
      system(sysCMD)
    }
  }

  #### >> 34 -- Multi-Scale Topographic Position Index --------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_28.html

  if ("mscale_tpi" %in% layers) {
    mscale_tpi <- fs::path(output_dir, "mscale_tpi", "mscale_tpi.sgrd")

    if (!file.exists(mscale_tpi)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 28",
        "-DEM", sinksfilled, # input DEM
        "-TPI", mscale_tpi, # output tpi
        "-SCALE_MIN", 1,
        "-SCALE_MAX", 8,
        "-SCALE_NUM", 3
      )
      system(sysCMD)
    }
  }

  #### >> 35 -- Relative Heights and Slope Positions ----------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_14.html

  if ("relposition" %in% layers) {
    slopeheight <- fs::path(output_dir, "relposition", "slope_height.sgrd")
    valleydepth <- fs::path(output_dir, "relposition", "valleydepth.sgrd") # don't need this as created above?
    normheight <- fs::path(output_dir, "relposition", "norm_height.sgrd")
    standheight <- fs::path(output_dir, "relposition", "stand_height.sgrd")
    msposition <- fs::path(output_dir, "relposition", "ms_position.sgrd")

    ls <- c(slopeheight, valleydepth, normheight, standheight, msposition)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 14",
        "-DEM", sinksfilled, # input DEM
        "-HO", slopeheight, # output Slope Height
        "-HU", valleydepth, # output Valley Depth
        "-NH", normheight, # output Normalized Height
        "-SH", standheight, # output Standardized Height
        "-MS", msposition, # output Mid-Slope Position
        "-W", 0.5,
        "-T", 10,
        "-E", 2
      )
      system(sysCMD)
    }
  }


  #### >> 36 -- Valley and Ridge Detection (Top Hat Approach) -------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_24.html

  # not very informative (binary outputs)

  # HillHeight = "hill_height.sgrd"
  # HillHeight = file.path(tmpOut, HillHeight)
  # ValleyIndex = "valley_index.sgrd"
  # ValleyIndex = file.path(tmpOut, ValleyIndex)
  # HillIndex = "hill_index.sgrd"
  # HillIndex = file.path(tmpOut, HillIndex)
  # HillslopeIndex = "hillslope_index.sgrd"
  # HillslopeIndex = file.path(tmpOut, HillslopeIndex)
  # sysCMD = paste(saga_path, "ta_morphometry 24",
  #                "-DEM", sinksfilled,                 # input DEM
  #                "-HILL", HillHeight,                 # output Hill Height
  #                "-VALLEY_IDX", ValleyIndex,          # output Valley Index
  #                "-HILL_IDX", HillIndex,              # output Hill Index
  #                "-SLOPE_IDX", HillslopeIndex,        # output Hillslope Index
  #                "-RADIUS_VALLEY", 1000,
  #                "-RADIUS_HILL", 1000,
  #                "-THRESHOLD", 100,
  #                "-METHOD", 0
  # )
  # system(sysCMD)

  #### >> 37 -- Upslope and Downslope Curvature ---------------------

  if ("slopecurvatures" %in% layers) {
    localcurve <- fs::path(output_dir, "slopecurvatures", "local_curv.sgrd")
    upslopecurve <- fs::path(output_dir, "slopecurvatures", "upslope_curv.sgrd")
    localupcurve <- fs::path(output_dir, "slopecurvatures", "local_upslope_curv.sgrd")
    downcurve <- fs::path(output_dir, "slopecurvatures", "down_curv.sgrd")
    localdowncurve <- fs::path(output_dir, "slopecurvatures", "local_downslope_curv.sgrd")

    ls <- c(localcurve, upslopecurve, localupcurve, downcurve, localdowncurve)

    if (all(!file.exists(ls))) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 26",
        "-DEM", sinksfilled, # input DEM
        "-C_LOCAL", localcurve, # output Local Curvature
        "-C_UP", upslopecurve, # output Upslope Curvature
        "-C_UP_LOCAL", localupcurve, # output Local Upslope Curvature
        "-C_DOWN", downcurve, # output Downslope Curvature
        "-C_DOWN_LOCAL", localdowncurve, # output Local Downslope Curvature
        "-WEIGHTING", 0.5
      )
      system(sysCMD)
    }
  }

  #### >> 38 -- Steepest Slope (Slope Aspect and Curvature) --------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html


  if ("steepestslope" %in% layers) {
    steepestslope <- fs::path(output_dir, "steepestslope", "steepest_slope.sgrd")

    if (!file.exists(steepestslope)) {
      sysCMD <- paste(
        saga_path, "ta_morphometry 0",
        "-ELEVATION", sinksfilled, # input DEM
        "-SLOPE", steepestslope, # output Steepest Slope
        "-METHOD", 1, # method 1 - steepest slope
        "-UNIT_SLOPE", 0,
        "-UNIT_ASPECT", 0
      )
      system(sysCMD)
    }
  }

  # #### >> 39 -- Upslope Area -------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.2/ta_hydrology_4.html

  if ("upslopearea" %in% layers) {
    upslopearea <- fs::path(output_dir, "upslopearea", "upslopearea.sgrd")

    if (!file.exists(upslopearea)) {
      sysCMD <- paste(
        saga_path, "ta_hydrology 4",
        "-ELEVATION", sinksfilled, # input DEM
        "-SINKROUTE", sinkroute,
        "-AREA", upslopearea, # output Upslope Area
        "-METHOD", 2,
        "-CONVERGE", 1.1
      )
      system(sysCMD)
    }
  }
  cli::cat_line()
  cli::cli_alert_success(
    "Layers downloaded and to written to {.path {out_dir}}"
  )
  invisible(out_dir)
}
