#' #' Generates road layers(roads and forest service roads) in vector format sample planning and modelling using bcdata package
#'
#' @param aoi An `sf` object (e.g. polygon) or path to a spatial file,
#'    which has been snapped to a common extend.
#'    This is commonly the output of the snap_aoi() function. A default location and name are
#'    applied in line with standard workflow.
#' @param out_dir A character string of filepath which points to output location. A default
#'    location and name are applied in line with standard workflow.
#'
#' @return An `sf` object with roads information for the given aoi
#' @export
#'
#' @examples
#' \dontrun{
#' #' ## Load snapped aoi object
#' aoi_file <- system.file("extdata/datecreek_aoi.gpkg", package = "PEMprepr")
#' roads <- get_roads(aoi_file, out_dir = PEMprepr::read_fid()$dir_1010_vector$path_abs)
#' }
get_roads <- function(aoi, out_dir) { #  # The main road network layer has too many roads in it. Filter it down to only
  # include named roads and combine those with actual mapped FSR's

  cli::cli_alert_info("Downloading Road network")
  roads <- bcdata::bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") |>
    bcdata::filter(bcdata::BBOX(local(sf::st_bbox(aoi)))) |> # slightly larger extent
    bcdata::select("id", "ROAD_NAME_FULL", "ROAD_CLASS", "ROAD_SURFACE", "FEATURE_LENGTH_M") |>
    bcdata::collect() |>
    dplyr::select("id", "ROAD_NAME_FULL", "ROAD_SURFACE", "ROAD_CLASS", "FEATURE_LENGTH_M")

  if (nrow(roads) > 0) {
    roads <- sf::st_intersection(roads, aoi) |>
      sf::st_cast("MULTILINESTRING")
  } else {
    cli::cli_alert_warning("No major roads data available within the study area")
  }


  fsr <- bcdata::bcdc_query_geodata("9e5bfa62-2339-445e-bf67-81657180c682") |>
    bcdata::filter(bcdata::BBOX(local(sf::st_bbox(aoi)))) |>
    bcdata::collect() |>
    dplyr::select("id", "FILE_TYPE_DESCRIPTION", "FEATURE_LENGTH_M") |>
    dplyr::rename(ROAD_CLASS = "FILE_TYPE_DESCRIPTION") |>
    dplyr::mutate(ROAD_CLASS = dplyr::case_when(
      ROAD_CLASS == "Forest Service Road" ~ "resource",
      ROAD_CLASS == "Road Permit" ~ "unclassifed"
    )) |>
    dplyr::mutate(ROAD_SURFACE = dplyr::case_when(
      ROAD_CLASS == "resource" ~ "loose",
      ROAD_CLASS == "unclassifed" ~ "rough"
    ))

  if (nrow(fsr) > 0) {
    fsr <- sf::st_intersection(fsr, aoi)
    fsr <- sf::st_cast(fsr, "MULTILINESTRING")
  } else {
    cli::cli_alert_warning("No foresty roads data available within the study area")
  }


  if (nrow(roads) > 0 & nrow(fsr) > 0) {
    road_merge <- dplyr::bind_rows(roads, fsr)
    sf::st_write(road_merge, fs::path(out_dir, "road_network.gpkg"), append = FALSE)
    cli::cat_line()
    cli::cli_alert_success("roads layers downloaded and to written to {.path {out_dir}}")
  }

  # add check for individual layer missing.
}
