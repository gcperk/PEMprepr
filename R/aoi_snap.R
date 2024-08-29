#' Snap Area of Interest
#'
#' Adjusts the area of interest to the nearest 100m.
#' Note that this package assumes to data is in a metric equal area projection.
#'
#' This is an essential first step.  As subsequent co-variate layers will be
#' generated at multiple resolutions (e.g. 5, 10, 25m^2) and then
#' disaggregate'd back to the lowest resolution.
#' Having the AOI set 100m break-points facilitates this.
#'
#' @param aoi is a sf object (e.g. polygon). The bounding box of the shape will
#'     be used to create rectangular shape.
#' @param method Options are _shrink_ or _expand_. _Shrink_ will snap the aoi in
#'     to the nearest 100m. _Expand_ will snap the AOI out to the nearest 100m.
#' @param buffer adds additional buffer to expand AOI bounding box
#'
#' @return a rectangular sf polygon
#' @export
#'
#' @examples
#' ## Load sf object
#' file <- system.file("extdata/datecreek_aoi.gpkg", package = "PEMprepr")
#' aoi_raw <- sf::st_read(dsn = file, quiet = TRUE)
#' ## snap aoi to nearest 100m
#' aoi_snap(aoi_raw)
#'
aoi_snap <- function(aoi, method = c("expand", "shrink"), buffer = 0) {
  if (!inherits(aoi, c("sf", "sfc"))) {
    stop("aoi must be an sf or an sfc object", call. = FALSE)
  }

  method <- match.arg(method)

  bb <- sf::st_bbox(aoi)

  ## Function
  print("initial extent is:")
  print(bb)

  if (buffer > 0 & method == "expand") {
    ## Generate expanded bbox -- expands to neared 100m
    xmin <- floor((bb["xmin"] - buffer) / 100) * 100
    xmax <- ceiling((bb["xmax"] + buffer) / 100) * 100
    ymin <- floor((bb["ymin"] - buffer) / 100) * 100
    ymax <- ceiling((bb["ymax"] + buffer) / 100) * 100
  } else if (method == "expand") {
    ## Generate expanded bbox -- expands to neared 100m
    xmin <- floor(bb["xmin"] / 100) * 100
    xmax <- ceiling(bb["xmax"] / 100) * 100
    ymin <- floor(bb["ymin"] / 100) * 100
    ymax <- ceiling(bb["ymax"] / 100) * 100
  } else if (method == "shrink") {
    xmin <- ceiling(bb["xmin"] / 100) * 100
    xmax <- floor(bb["xmax"] / 100) * 100
    ymin <- ceiling(bb["ymin"] / 100) * 100
    ymax <- floor(bb["ymax"] / 100) * 100
  }

  box <- matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol = 2, byrow = TRUE)
  box <- sf::st_polygon(list(box))
  box <- sf::st_sfc(box, crs = sf::st_crs(aoi))
  box <- sf::st_as_sf(box)

  ## Report and Return
  print("Extent is:")
  print(sf::st_bbox(box))
  return(box)
}
