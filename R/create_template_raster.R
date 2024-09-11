#' #' Create a raster template from AOI
#' #'
#' #' Sets the baseline raster template to align and standardize all raster predictor layers
#' #' This package assumes to data is in a metric equal area projection most likely BCAlbers
#' #'
#' #' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregate'd back to the lowest resolution.
#' #' Having the aoi set 100m break-points facilitates this.
#' #'
#' #' @param aoi is a sf or terra::vect object bounding box created expanded in aoi_snap function(e.g. polygon).  Should be a meter based projection
#' #' @param res desired resolution of the raster (in meters)
#' #' @param outname text name of the output file ie: template.tif
#' #' @param out_dir output path.  Note that the results will be placed in a subfolder labelled with the resolution.
#' #'
#' #' @return a terra raster and saves in correct filepath
#' #' @keywords aoi, raster, template
#' #' @export
#' #' @examples
#' #' create_template_raster(aoi = file.path(fid$shape_dir_1010[1], "aoi_snapped.gpkg"), res = 10, outpath = fid$cov_dir_1020[2], filename = "template.tif")
#'
#' create_template <- function(aoi = fs:path(read_fid()$dir_1010_vector$path_abs, "aoi_snapped.gpkg"),
#'                             res = 25,
#'                             filename = "template.tif",
#'                             out_dir = read_fid()$dir_1020_covariates$path_rel,
#'                             write_output = TRUE){
#'
#'
#'   # add same check for "character or filepath" or read in sf object
#'
#'   if (!is.numeric(res)) {
#'     cli::cli_abort("{.var res} must be numeric")
#'   }
#'
#'
#'   if (inherits(aoi, c("character"))) {
#'     #if(!fs::file_exists(aoi)) # check if exists and if not throw a warning.
#'
#'     aoi <- sf::st_read(aoi, quiet = TRUE)
#'   } else if (!inherits(aoi, c("sf", "sfc"))) {
#'     cli::cli_abort("{.var aoi} must be an sf or an sfc object or a path to a file")
#'   }
#'
#'
#'   aoi_bb  <- terra::vect(aoi)
#'
#'   template <- terra::rast(aoi_bb , resolution = res)
#'   terra::values(template) <- 0
#'
#'
#'   ## write to file
#'   if (write_output) {
#'     if (!fs::dir_exists(fs::path(out_dir, paste0(res, 'm'))) {
#'       fs::dir_create(out_dir, recurse = TRUE)
#'     }
#'     output_file <- fs::path(fs::path_abs(out_dir), "aoi_snapped.gpkg")
#'     if(fs::file_exists(output_file)){
#'       cli::cli_alert_warning(
#'         "Snapped aoi already exists in {.path {output_file}}"
#'       )
#'     }
#'     sf::st_write(box, output_file, append = FALSE, quiet = TRUE)
#'     cli::cat_line()
#'     cli::cli_alert_success(
#'       "Snapped aoi written to {.path {output_file}}"
#'     )
#'   }
#'
#'
#'   outpath <- file.path(outpath, paste0(res, 'm'))
#'   if(!exists(outpath)) {dir.create(outpath, recursive = TRUE) }
#'
#'
#'   # write out
#'   terra::writeRaster(template, file.path(outpath, filename), overwrite = TRUE)
#'
#'   # print(paste("Template raster create and saved as:", file.path(outpath, "template.tif")))
#'   return(template)
#'
#' }
#'

