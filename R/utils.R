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


# Add function to check input SpatRast location or read file

read_spatrast_if_necessary <- function(spatRast, arg = rlang::caller_arg(spatRast), call = rlang::caller_env()) {
  if (inherits(spatRast, c("character"))) {
    spatRast <- terra::rast(spatRast)
  } else if (!inherits(spatRast, c("SpatRaster"))) {
    cli::cli_abort(
      "{.arg {arg}} must be a SpatRaster or a path to SpatRaster file",
      arg = arg, call = call
    )
  }
  spatRast
}

# add check function for if input is sf object then read into memory
read_sf_if_necessary <- function(sf_obj, arg = rlang::caller_arg(sf_obj), call = rlang::caller_env()) {
  if (inherits(sf_obj, c("character"))) {
    sf_obj <- sf::st_read(sf_obj)
  } else if (!inherits(sf_obj, c("sf"))) {
    cli::cli_abort("{.arg {arg}} must be an sf object or a path to an sf object",
      arg = arg, call = call
    )
  }
  sf_obj
}
