#' Read folder structure file
#'
#' Read the fid file from the `_meta` directory
#'
#' @param fid_path Path to the fid file. Defaults to `"_meta/fid.RDS"`
#'
#' @return list containing folder structure
#'
#' @export
read_fid <- function(fid_path = file.path("_meta", "fid.rds")) {
  if (!fs::file_exists(fid_path)) {
    return(NULL)
  }
  readRDS(fid_path)
}

# Add NULL default operator if doesn't exist (added to base R in v4.4)
if (!exists("%||%", envir = baseenv())) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}




find_saga_path <- function(root = NULL) {

  # Set root path depending on operating system
  if (is.null(root)) {
    if (Sys.info()["sysname"] == "Windows") {
      root <- "C:/"
    } else {
      root <- "/usr"
    }
  }

  cli::cli_progress_message("Searching for saga_cmd.exe on your machine...")

  sloc <- fs::dir_ls(path = root, type = "file", glob = "*saga_cmd.exe", recurse = TRUE, fail = FALSE)


  if(length(sloc >0)) {

    cli::cat_line()
    cli::cli_alert_success( "congratulations there are matches, select one of the following for your saga_path")

  } else {
    cli::cli_abort("no matching files found.. please check you have saga installed")
  }

  print(sloc)
}




check_saga <- function(saga_path = NULL){

  if(is.null(saga_path)){

    cli::cli_abort("{.var saga_path} must be a path to the SAGA_cmd location on your computer.
                   Please check your program files or use `find_saga_path()` to locate the saga_cmd.exe file")
  }

  if(Sys.info()['sysname']=="Windows"){
    saga_cmd <- fs::path(saga_path)
    fns      <- "\\" ### file name separator
  } else {
    saga_cmd <- "saga_cmd"
    fns      <- "/" ### file name separator

  }  ;
  z <- system(paste(saga_cmd, "-v"), intern = TRUE)  ## prints that SAGA version number -- confirming it works.
  z <- print(z[1])
  v <- suppressWarnings(as.numeric(unlist(strsplit(z, "[[:punct:][:space:]]+")[1])))
  v <- v[!is.na(v)][1:2]
  v <- as.numeric(paste(v[1], v[2], sep = "."))

  if (v < 7.6) {
    warning("SAGA-GIS is less that 7.6.  Not all covariates will generate.  Upgrade your SAGA, visit https://sourceforge.net/projects/saga-gis/files/")
  }


}



#' Crop tiles
#' @family Layers
#' @rdname Layers
#' @keywords internal
#' @export
#'
read_crop <- function(f, poly, tmp) {
  #--- extract tile index to match with index in polygons - works up to 1000 tiles ---#
  index <- stringr::str_extract(
    f,
    pattern = "_([1-9]|[1-9][0-9]|[1-9][0-9][0-9]|1000)_"
  ) |>
    stringr::str_replace_all(
      index,
      pattern = "_",
      replacement = ""
    ) |>
    as.numeric()

  #--- write rasters to tmp folder ---#
  terra::rast(f) |>
    terra::crop(y = poly[index, ]) |>
    terra::writeRaster(
      paste0(tmp, basename(f)),
      overwrite = TRUE
    )
}
