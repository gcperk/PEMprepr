#' Create BGC template
#'
#' @param bec An `sf` object (e.g. polygon) or path to a spatial file of BEC within your AOI. Usually generated when calling [create_base_vectors()].
#' @param field field within bec object which defines the BEC units. Default is `"MAP_LABEL"`
#' @param template_rast A `SpatRast` template or path to a spatRast file, which will form the
#'      template resolution and extent output. The template is currently created using the
#'      create_base_raster() function.
#' @param write_output should the bec raster be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of filepath which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @return write out bgc raster and return object
#' @export
#'
#' @examples
#' \dontrun{
#' bgc_raster = create_bgc_template(
#'   bec = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_abs, "bec.gpkg"),
#'   field = "MAP_LABEL",
#'   template_rast = NULL,
#'   write_output = TRUE,
#'   out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel
#' }

create_bgc_template = function(
    bec = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_abs, "bec.gpkg"),
    field = "MAP_LABEL",
    template_rast = NULL,
    write_output = TRUE,
    out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel
){

  ## testing lines
  # template_raster <- fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel,"25m","template.tif")
  ## end testing lines

  # read in bec file
  if (inherits(bec, c("character"))) {
    bec <- sf::st_read(bec, quiet = TRUE)

  } else if (!inherits(bec, c("sf", "sfc"))) {
    cli::cli_abort("{.var bec} must be an sf or an sfc object or a path to a file")
  }

  # read in template raster
  if (inherits(template_rast, c("character"))) {
    template_rast <- terra::rast(template_rast)

  } else if (!inherits(template_rast, c("SpatRaster"))) {
    cli::cli_abort("{.var template_rast} must be a SpatRaster or a path to a file")
  }

  bec_vec <- terra::vect(bec)

  bec_rast <- terra::rasterize(bec_vec,  template_rast , field = field)

  if (write_output) {

    pixal_size = terra::res(template_rast)[1]
    full_out_dir = fs::path(fs::path_abs(out_dir),  paste0(pixal_size,"m"))

    if (!fs::dir_exists(full_out_dir)) {
      fs::dir_create(full_out_dir, recurse = TRUE)
      cli::cli_alert_warning(
        "write out folder does not exist, creating at location {.path {full_out_dir}}"
      )
    }
    output_file <- fs::path(fs::path_abs(full_out_dir), "bec.tif")
    if(fs::file_exists(output_file)){
      cli::cli_alert_warning(
        "Bec raster already exists in {.path {output_file}}"
      )
    }
    terra::writeRaster(bec_rast, fs::path(output_file), overwrite = TRUE)
    cli::cat_line()
    cli::cli_alert_success(
      "Bec Raster written to {.path {output_file}}"
    )
  }

  bec_rast
}
