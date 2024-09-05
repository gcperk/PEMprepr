#' Snap Area of Interest
#'
#' Creates a rectangular polygon from the area of interest, 
#' rounded to the nearest 100m.
#' Note that this package assumes to data is in a metric equal area projection.
#'
#' This is an essential first step.  As subsequent co-variate layers will be
#' generated at multiple resolutions (e.g. 5, 10, 25m^2) and then
#' disaggregate'd back to the lowest resolution.
#' Having the AOI set 100m break-points facilitates this.
#'
#' @param aoi An `"sf"`` object (e.g. polygon) or a path to a spatial file 
#'     representing the area of interest. The bounding box of the aoi will
#'     be used to create a rectangular shape.
#' @param method Options are `"shrink"` or `"expand"`. `"shrink"` will snap 
#'     the aoi in to the nearest 100m. `"Expand"` will snap the AOI out to the 
#'     nearest 100m.
#' @param buffer Additional buffer to expand AOI bounding box. Only used when 
#'     `method = "expand"`
#' @param write_output should the snapped aoi bounding box be written to disk? 
#'     If `TRUE` (default), will write to `out_dir`
#' @param out_dir the directory to hold the snapped boundary file. If not
#'     specified uses the default from the `fid` folder structure
#'
#' @return a rectangular sf polygon
#' @export
#'
#' @examples
#' ## Load sf object
#' aoi_file <- system.file("extdata/datecreek_aoi.gpkg", package = "PEMprepr")
#' ## snap aoi to nearest 100m
#' snap_aoi(aoi_file, write_output = FALSE)
#'
snap_aoi <- function(
  aoi = NULL, 
  method = c("expand", "shrink"), 
  buffer = 0,
  write_output = TRUE,
  out_dir = read_fid()$dir_1010_vector$path_abs
) {
  
  method <- match.arg(method)

  if (!is.numeric(buffer)) {
    cli::cli_abort("{.var buffer} must be numeric")
  }

  aoi <- aoi %||% look_for_aoi()
  
  if (inherits(aoi, c("character"))) {
    aoi <- sf::st_read(aoi, quiet = TRUE)
  } else if (!inherits(aoi, c("sf", "sfc"))) {
    cli::cli_abort("{.var aoi} must be an sf or an sfc object or a path to a file")
  }

  box <- snap_bbox(aoi, method = method, buffer = buffer)
  
  ## write to file
  if (write_output) {
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
    }
    output_file <- fs::path(fs::path_abs(out_dir), "aoi_snapped.gpkg")
    sf::st_write(box, output_file, append = FALSE, quiet = TRUE)
    cli::cat_line()
    cli::cli_alert_success(
      "Snapped aoi written to {.path {output_file}}"
    )
  }
  
  box
}

look_for_aoi <- function() {
    # Look for aoi file in default directory
    aoi_dir <- read_fid()$dir_0010_vector$path_abs
    files <- list.files(
      aoi_dir,
      pattern = "([.]gpkg)|([.]shp)$"
    )
    if (length(files) == 1L) {
      # If there is only one file there, use that
      aoi <- files
      cli::cli_alert_info("No {.var aoi} provided, but file {file} found. Using {file}.")
    } else {
      cli::cli_abort("{length(files)} file{?s} found in {.path {aoi_dir}} and no {.var aoi} provided.")
    }
}

snap_bbox <- function(aoi, method, buffer) {
  bb <- sf::st_bbox(aoi)
  
  cli::cli_alert_info("Initial extent is:")
  cli::cli_dl(bb)
  cli::cat_line()
  
  if (method == "expand") {
    ## Generate expanded bbox -- expands to neared 100m
    xmin <- floor((bb["xmin"] - buffer) / 100) * 100
    xmax <- ceiling((bb["xmax"] + buffer) / 100) * 100
    ymin <- floor((bb["ymin"] - buffer) / 100) * 100
    ymax <- ceiling((bb["ymax"] + buffer) / 100) * 100
  } else if (method == "shrink") {
    xmin <- ceiling(bb["xmin"] / 100) * 100
    xmax <- floor(bb["xmax"] / 100) * 100
    ymin <- ceiling(bb["ymin"] / 100) * 100
    ymax <- floor(bb["ymax"] / 100) * 100
  }
  
  box <- matrix(
    c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin),
    ncol = 2,
    byrow = TRUE
  )
  
  box <- sf::st_polygon(list(box))
  box <- sf::st_sfc(box, crs = sf::st_crs(aoi))
  box <- sf::st_as_sf(box)

  cli::cli_alert_info(("New extent is:"))
  cli::cli_dl(sf::st_bbox(box))
  
  box
}
