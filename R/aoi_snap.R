#' Title
#'
#' @param aoi
#' @param method
#' @param buffer
#'
#' @return
#' @export
#'
#' @examples
aoi_snap <- function(aoi, method=c("expand","shrink"), buffer=0){
  ## testing
  # setwd("e:/workspace/2019/PEM_2020/PEMWorkFlow/")
  #aoi <- sf::st_read("../data/Block_aoi.gpkg")
  bb <- sf::st_bbox(aoi)

  ## Function
  print("initial extent is:")
  print(bb)

  if (buffer>0 & method == "expand") {
    ## Generate expanded bbox -- expands to neared 100m
    xmin <- floor((bb$xmin - buffer) / 100)*100
    xmax <- ceiling((bb["xmax" ]+ buffer) / 100) * 100
    ymin <- floor((bb$ymin - buffer) / 100)*100
    ymax <- ceiling((bb["ymax"] + buffer) / 100) * 100


  } else if (method == "expand") {
    ## Generate expanded bbox -- expands to neared 100m
    xmin <- floor(bb$xmin / 100)*100
    xmax <- ceiling(bb["xmax"] / 100) * 100
    ymin <- floor(bb$ymin / 100)*100
    ymax <- ceiling(bb["ymax"] / 100) * 100

  } else if (method == "shrink") {

    xmin <- ceiling(bb$xmin / 100)*100
    xmax <- floor(bb["xmax"] / 100) * 100
    ymin <- ceiling(bb$ymin / 100)*100
    ymax <- floor(bb["ymax"] / 100) * 100

  }

  box <- matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol = 2, byrow = TRUE)
  box <- sf::st_polygon(list(box))
  box <- sf::st_sfc(box, crs=sf::st_crs(aoi))
  box <- sf::st_as_sf(box)

  ## Report and Return
  print("Extent is:")
  print(sf::st_bbox(box))
  return(box)

}
