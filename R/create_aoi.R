#' Create a rectangular AOI bounding box
#'
#' Create a rectangular AOI bounding box containing your AOI, that is snapped
#' to a 100m grid to facilitate alignment with rasters.
#'
#' @param aoi_dir the directory containing the AOI boundary file. If not
#'     specified uses the default from the `fid` folder structure
#' @param filename the input file name. If not specified, and there is only one
#'     spatial file in `aoi_dir`, it will use that.
#' @param out_dir the directory to hold the snapped boundary file. If not
#'     specified uses the default from the `fid` folder structure
#' @param ... Arguments passed on to [aoi_snap()]
#'
#' @return path to the snapped aoi file, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' create_aoi(filename = "my_aoi.gpkg")
#' }
create_aoi <- function(
    aoi_dir = fid$dir_0010_vector$path_abs,
    filename = NULL,
    out_dir = fid$dir_1010_vector$path_abs,
    ...) {
  if (is.null(filename)) {
    # If there is only one file there, use that
    files <- list.files(aoi_dir, pattern = "([.]gpkg)|([.]shp)$")
    if (length(files) == 1L) {
      filename <- files
      cli::cli_alert_info("File {filename} found and no {.var filename} provided. Using {filename}.")
    } else {
      cli::cli_abort("{length(files)} file{?s} found in {.path {aoi_dir} and no {.var filename}")
    }
  }

  aoi <- sf::st_read(file.path(aoi_dir, filename))

  aoi_bb <- aoi_snap(aoi, ...)

  output_file <- file.path(out_dir, "aoi_snapped.gpkg")
  sf::st_write(aoi_bb, output_file, append = FALSE)

  invisible(output_file)
}
