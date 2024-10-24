#'#' Get Canadian Digital Elevation Data (CDED) to match area of interest
#'
#' This function is a wrapper for bcmaps::cded_terra() to extract data from the
#' Canadian Digital Elevation Data set to match the raster template built using
#' create_raster_template().
#'
#' @param  aoi An `SpatRast` object or path to a spatial file (.tif) usually generated
#'      when calling [create_base_raster()]. Should be a meter based coordinate reference system.
#' @param res resolution in meters of final project. If no values specified (default is FALSE),
#'      output spatRast will match the resolution of aoi.
#' @param write_output should the cded spatRast be written to disk?
#'     If `TRUE` (default), will write to `out_dir` in a subfolder defined by resolution e.g: "25m".
#' @param out_dir  the root directory to hold the cded dem spatRast file. If not
#'     specified uses the default from the `fid` folder structure.
#' @inheritParams terra::writeRaster
#' @param ... arguments passed on to [bcmaps::cded_terra()]
#'
#' @return a spatRast file of CDED data
#' @export
#'
#' @examples
#' \dontrun{
#' get_cded_dem(
#'   aoi = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_abs, "25m", "template.tif"),
#'   res = FALSE,
#'   out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_abs,
#'   write_output = TRUE
#'   overwrite = FALSE)
#' }
get_cded_dem <- function(aoi = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_abs, "25m", "template.tif"),
                         res = FALSE,
                         out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_abs,
                         write_output = TRUE,
                         overwrite = FALSE, ...) {

  if (inherits(aoi, c("character"))) {
    aoi <- terra::rast(aoi)
  } else if (!inherits(aoi, c("SpatRaster"))) {
    cli::cli_abort("{.var aoi} must be a SpatRaster or a path to a file")
  }

  aoi_res <- terra::res(aoi)[1]

  if(res) {
    if (!is.numeric(res)) {
      cli::cli_abort("{.var res} must be numeric")
    }

    if(aoi_res != res) {

      cli::cli_alert_warning(
        "Updating cded to resolution specified"
      )

      aoi_out <- aoi
      res(aoi_out) = res
      aoi_template <- resample(aoi, aoi_out)

    } else {

      aoi_template <- aoi
      cli::cli_alert_warning(
        "Specified res matches template resolution, output cded resolution will be {.var res}"
      )
    }

    output_res <- res

  } else {

    aoi_template <- aoi
    output_res <- aoi_res
    cli::cli_alert_warning(
      "No spatial resolution specified, cded output spatial resolution will match input raster"
    )
  }

  cded_raw <- bcmaps::cded_terra(aoi_template, ...)
  #cded_raw <- bcmaps::cded_terra(aoi_template)#, ...)

  cded <- terra::project(cded_raw, aoi_template)

  if (write_output) {

    output_dir <- fs::path(out_dir, paste0(output_res, "m"))

    if (!fs::dir_exists(fs::path(output_dir))) {
      fs::dir_create(fs::path(output_dir), recurse = TRUE)
    }
    terra::writeRaster(cded, fs::path(output_dir, "dem.tif"), overwrite = overwrite)
    cli::cat_line()
    cli::cli_alert_success(
      "CDED dem raster written to {.path {output_dir}}"
    )
  }

  cded
}
