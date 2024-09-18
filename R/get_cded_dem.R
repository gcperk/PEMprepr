#'#' Get Canadian Digital Elevation Data (CDED) to match area of interest
#'
#' This function is a wrapper for bcmaps::cded_terra() to extract data from the
#' Canadian Digital Elevation Data set to match the raster template built using
#' create_raster_template().
#'
#' @param  aoi the `SpatRast` created using the create_base_raster() on which the
#'      DEM data will be extracted, currently accepts raster (.tif) or filepath.
#' @param res resolution in meters of final project
#' @param write_output should the snapped aoi bounding box be written to disk?
#'     If `TRUE` (default), will write to `out_dir`
#' @param out_dir  the root directory to hold the cded dem spatRast file. If not
#'     specified uses the default from the `fid` folder structure.
#'
#' @return a spatRast file of CDED data
#' @export
#'
#' @examples
#' \dontrun{
#' get_cded_dem(aoi_bb = file.path(fid$shape_dir_1010[2],"aoi_snapped.gpkg"),
#'     res = 5, out_dir = fid$cov_dir_1020[2]))
#' }
get_cded_dem <- function(aoi = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_abs, "25m", "template.tif"),
                         res = 5,
                         out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_abs,
                         write_output = TRUE) {
  if (inherits(aoi, c("character"))) {
    aoi <- terra::rast(aoi)
  } else if (!inherits(aoi, c("SpatRaster"))) {
    cli::cli_abort("{.var aoi} must be an spatRast or a path to a .tif file")
  }

  if (!is.numeric(res)) {
    cli::cli_abort("{.var res} must be numeric")
  }

  output_dir <- fs::path(out_dir, paste0(res, "m"))

  cded_raw <- bcmaps::cded_terra(aoi)

  cded <- terra::project(cded_raw, aoi)


  ## check if folder exists
  if (write_output) {
    if (!fs::dir_exists(fs::path(output_dir))) {
      fs::dir_create(fs::path(output_dir), recurse = TRUE)
    }
    terra::writeRaster(cded, fs::path(output_dir, "dem.tif"), overwrite = FALSE)
    cli::cat_line()
    cli::cli_alert_success(
      "CDED dem raster written to {.path {output_dir}}"
    )
  }

  cded
}
