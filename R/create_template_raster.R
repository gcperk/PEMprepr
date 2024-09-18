#' Create a raster template from AOI
#'
#' Sets the baseline raster template to align and standardize all raster predictor layers
#' This package assumes to data is in a metric equal area projection most likely BCAlbers
#'
#' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregate'd back to the lowest resolution.
#' Having the aoi set 100m break-points facilitates this.
#'
#' @param aoi is a sf or terra::vect object bounding box created expanded
#'      in aoi_snap function(e.g. polygon).  Should be a meter based
#'      projection
#' @param res desired resolution of the raster (in meters)
#' @param filename text name of the output file. the default is template.tif
#' @param out_dir output path.  Note that the results will be placed in a
#'    subfolder labelled with the resolution.
#' @param write_output should the template raster be written to disk?
#'     If `TRUE` (default), will write to `out_dir`
#'
#' @return a terra raster
#' @export
#'
#' @examples
#' \dontrun{
#' create_template_raster(
#'     aoi = fs::path(PEMr::read_fid()$dir_1010_vector$path_abs, "aoi_snapped.gpkg"),
#'     res = 25,
#'     filename = "template.tif",
#'     out_dir = PEMr::read_fid()$dir_1020_covariates$path_rel,
#'     write_output = TRUE)
#'}
create_template_raster <- function(aoi = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_abs, "aoi_snapped.gpkg"),
                                   res = 25,
                                   filename = "template.tif",
                                   out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel,
                                   write_output = TRUE) {

  if (!is.numeric(res)) {
    cli::cli_abort("{.var res} must be numeric")
  }


  if (inherits(aoi, c("character"))) {
    # if(!fs::file_exists(aoi)) # check if exists and if not throw a warning.

    aoi <- sf::st_read(aoi, quiet = TRUE)
  } else if (!inherits(aoi, c("sf", "sfc"))) {
    cli::cli_abort("{.var aoi} must be an sf or an sfc object or a path to a file")
  }


  aoi_bb <- terra::vect(aoi)

  template <- terra::rast(aoi_bb, resolution = res)
  terra::values(template) <- 0

  ## check if folder exists
  if (write_output) {
    output_dir <- fs::path(out_dir, paste0(res, "m"))

    if (!fs::dir_exists(fs::path(output_dir))) {
      fs::dir_create(fs::path(output_dir), recurse = TRUE)
    }

    # check if output file already exists
    output_file <- fs::path(output_dir, filename)
    if (fs::file_exists(output_file)) {
      cli::cli_alert_warning(
        "Template raster already exists in {.path {output_file}}"
      )
    }

    terra::writeRaster(template, output_file, overwrite = FALSE)
    cli::cat_line()
    cli::cli_alert_success(
      "Template raster written to {.path {output_file}}"
    )
  }

  template
}
