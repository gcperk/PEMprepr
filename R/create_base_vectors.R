#' Generates base information vectors for sample planning and modelling using bcdata package
#'
#' @param aoi An `sf` object (e.g. polygon) or path to a spatial file,
#'    which has been snapped to a common extend.
#'    This is commonly the output of the snap_aoi() function. A default location and name are
#'    applied in line with standard workflow.
#' @param out_dir A character string of filepath which points to output location. A default
#'    location and name are applied in line with standard workflow.
#'
#' @return path to the output directory where files are written (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' #' ## Load snapped aoi object
#' aoi_file <- system.file("extdata/datecreek_aoi.gpkg", package = "PEMprepr")
#' sn_aoi <- snap_aoi(aoi_file, write_output = FALSE)
#' create_base_vectors(
#'   sn_aoi,
#'   out_dir = PEMprepr::read_fid()$dir_1010_vector$path_abs
#' )
#' }
create_base_vectors <- function(
    aoi = fs::path(
      PEMprepr::read_fid()$dir_1010_vector$path_abs,
      "aoi_snapped.gpkg"
    ),
    out_dir = PEMprepr::read_fid()$dir_1010_vector$path_abs) {
  if (is.null(out_dir)) {
    cli::cli_abort("{.var out_dir} is an invalid file path string")
  }

  UseMethod("create_base_vectors")
}

#' @export
create_base_vectors.default <- function(aoi, out_dir) {
  cli::cli_abort("No method for object of class {.var {class(aoi)}}")
}

#' @export
create_base_vectors.character <- function(aoi, out_dir) {
  aoi_sf <- sf::st_read(aoi)
  create_base_vectors(aoi_sf, out_dir)
}

#' @export
create_base_vectors.sf <- function(aoi, out_dir) {
  # Detect the CRS of the sf object
  if (is.na(sf::st_crs(aoi))) {
    cli::cli_abort("CRS is not assigned. Use {.fn sf::st_crs} to assign a valid CRS to aoi")
  }

  if (sf::st_is_longlat(aoi)) {
    cli::cli_alert_info("Input CRS is Lat/Long format. Transforming to EPSG 3005 (BC Albers) for processing")
    aoi <- bcmaps::transform_bc_albers(aoi)
  }

  aoi <- sf::st_set_agr(aoi, "constant")

  if (!is.numeric(sf::st_crs(aoi)$epsg)) {
    cli::cli_abort("There was a problem retrieving the EPSG code from the aoi. Is it assigned properly?")
  }

  # note this was an earlier flag - I dont like the idea of changing internals
  # @Andy T. any thoughts on speed improvements.

  # Adjust max download size based on AOI
  ## PROBLEMATIC -- should not be done globally -----------------
  ## see: https://r-pkgs.org/code.html#sec-code-r-landscape section 7.6.1
  # withr::local_options(
  #   options(bcdata.max_geom_pred_size = as.numeric(st_area(aoi)) + 10)
  # )

  get_BEC(aoi, out_dir)
  get_VRI(aoi, out_dir)
  get_harvest(aoi, out_dir)
  get_TEM(aoi, out_dir)
  get_water(aoi, out_dir)
  get_roads(aoi, out_dir)
  get_towns(aoi, out_dir)
  get_fires(aoi, out_dir)
  get_fire_severity(aoi, out_dir)
  get_parks(aoi, out_dir)
  get_transmission_lines(aoi, out_dir)

  cli::cat_line()
  cli::cli_alert_success(
    "Layers downloaded and to written to {.path {out_dir}}"
  )
  invisible(out_dir)
}

#' @export
create_base_vectors.sfc <- create_base_vectors.sf

### 1) Get_BEC ----------------------------
get_BEC <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading BEC layers")

  # # 1) BEC Biogeographical linework
  bec <- bcdata::bcdc_query_geodata("f358a53b-ffde-4830-a325-a5a03ff672c3") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select("MAP_LABEL") |>
    bcdata::collect() |>
    dplyr::select("MAP_LABEL")

  if (nrow(bec) > 0) {
    bec <- sf::st_intersection(bec, aoi)
  }

  if (sf::st_crs(aoi) == sf::st_crs(bec)) {
    sf::st_write(bec, fs::path(out_dir, "bec.gpkg"), append = FALSE)
    cli::cat_line()
    cli::cli_alert_success(
      "Bec layer downloaded and to written to {.path {out_dir}}"
    )
  } else {
    # add conversion
    message("convert or setting crs for bec data download")
  }
}


### 2) Download VRI -----------------------
get_VRI <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading VRI layers")

  vri <- bcdata::bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) |> # Treed sites
    bcdata::collect() |>
    dplyr::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1"))

  if (nrow(vri) > 0) {
    vri <- sf::st_intersection(vri, aoi)
  }

  sf::st_write(vri, fs::path(out_dir, "vri.gpkg"), append = FALSE)
  cli::cat_line()
  cli::cli_alert_success(
    "VRI layer downloaded and to written to {.path {out_dir}}"
  )
  # post process VRI data into classes:

  # Depending on the study area we want to focus on sampling in older areas - class 4 (60-80) or 5 (80 + )
  # To do this we will define 2 vri age classess to exclude from the sampling area
  # class 1 and 2 (0 - 40 yrs)
  # class 1-3 (0 - 60 years)

  vri_class2 <- vri |>
    dplyr::mutate(age_class = as.numeric("PROJ_AGE_CLASS_CD_1")) |>
    dplyr::filter(.data$age_class < 3)

  sf::st_write(vri_class2, fs::path(out_dir, "vri_class1_2.gpkg"), append = FALSE)

  vri_class3 <- vri |>
    dplyr::mutate(age_class = as.numeric("PROJ_AGE_CLASS_CD_1")) |>
    dplyr::filter(.data$age_class == 3)

  sf::st_write(vri_class3, fs::path(out_dir, "vri_class3.gpkg"), append = FALSE)

  #
  # ######### this section is currently in test phase (related to removal fo deciduous areas in Date Creek after these were found to be highly selected and challanging to sample)
  #
  # # vri - deciduous leading - this needs to be appled for some
  #
  # # STILL TO DO - for areas with deciduous leading (AT, EP) Aspen and paper burch these should be seperated using the code "SPECIES_CD_1" == AT|EP.
  # # ie important in Date Creek
  #
  vri_decid <- vri |>
    dplyr::filter(.data$SPECIES_CD_1 %in% c("AT", "EP")) # note might need to adjust for some areas of interest
  sf::st_write(vri_decid, fs::path(out_dir, "vri_decid.gpkg"), append = FALSE)
}


### 3) Get harvest history and FTEN --------------------------------
get_harvest <- function(aoi, out_dir) {
  # 3) Download recent cutblocks (within last 20 years)
  # Uses date filter which filters cutblock ages less than 20 years, or 7305 days

  cli::cli_alert_info("Downloading cutblock layers")
  cutblocks <- bcdata::bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select("HARVEST_YEAR", "AREA_HA") |>
    bcdata::collect()

  if (nrow(cutblocks) > 0) {
    cutblocks <- sf::st_intersection(cutblocks, aoi) |>
      dplyr::filter(as.numeric(format(Sys.time(), "%Y")) - .data$HARVEST_YEAR <= 20)
  }

  sf::st_write(cutblocks, fs::path(out_dir, "cutblocks.gpkg"), append = FALSE)

  # 4) ften  - Download latest harvest layer
  cli::cli_alert_info("Downloading ften harvest layers")

  ften <- bcdata::bcdc_query_geodata("cff7b8f7-6897-444f-8c53-4bb93c7e9f8b") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select("HARVEST_AUTH_STATUS_CODE", "ISSUE_DATE", "CURRENT_EXPIRY_DATE_CALC", "LIFE_CYCLE_STATUS_CODE", "FILE_STATUS_CODE") |>
    bcdata::collect() |>
    dplyr::select(
      "HARVEST_AUTH_STATUS_CODE", "ISSUE_DATE", "CURRENT_EXPIRY_DATE_CALC",
      "LIFE_CYCLE_STATUS_CODE", "FILE_STATUS_CODE"
    ) |>
    dplyr::filter(
      as.numeric(format(.data$ISSUE_DATE, "%Y")) > 2000
    ) # might need to adjust this to dynamic

  sf::st_write(ften, fs::path(out_dir, "ften.gpkg"), append = FALSE)

  cutblocks_ften <- dplyr::bind_rows(cutblocks, ften)

  sf::st_write(cutblocks_ften, fs::path(out_dir, "cutblocks_ften.gpkg"), append = FALSE)

  cli::cat_line()
  cli::cli_alert_success(
    "harvest layers downloaded and to written to {.path {out_dir}}"
  )
}

### 5) TEM -----------------------------
get_TEM <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading TEM layers")

  tem <- bcdata::bcdc_query_geodata("0a83163b-a62f-4ce6-a9a1-21c228b0c0a3") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(tem) > 0) {
    tem <- sf::st_intersection(tem, aoi)
    sf::st_write(tem, fs::path(out_dir, "tem.gpkg"), append = FALSE)
  }
}


### 6) Water (Lakes, Rivers, Wetlands) --------------------------------------
get_water <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading lake, river, and wetland layers")

  water_records <- c(
    "cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", # lakes
    "f7dac054-efbf-402f-ab62-6fc4b32a619e", # rivers
    "93b413d8-1840-4770-9629-641d74bd1cc6" # wetlands
  )

  get_one_water <- function(id) {
    bcdata::bcdc_query_geodata(id) |>
      bcdata::filter(bcdata::INTERSECTS(aoi)) |>
      bcdata::collect() |>
      dplyr::select("id", "WATERBODY_TYPE", "AREA_HA")
  }

  water_list <- purrr::map(water_records, get_one_water)

  all_water <- dplyr::bind_rows(water_list)

  sf::st_write(all_water, fs::path(out_dir, "water.gpkg"), append = FALSE)
}

## 7) Download road network --------------------
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
  }

  road_merge <- dplyr::bind_rows(roads, fsr)

  # might need a check here

  sf::st_write(road_merge, fs::path(out_dir, "road_network.gpkg"), append = FALSE)
}

## 8 Major Towns ---------------------------------
get_towns <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading major towns")

  towns <- bcdata::bcdc_query_geodata("b678c432-c5c1-4341-88db-0d6befa0c7f8") |>
    bcdata::collect()

  sf::st_write(towns, fs::path(out_dir, "major_towns_bc.gpkg"), append = FALSE)
}


## 9) Fire polygons  ---------------------------------
get_fires <- function(aoi, out_dir) { #  cli::cli_alert_info("Downloading recent fire disturbance (<20 years)")

  fire_records <- c(
    "cdfc2d7b-c046-4bf0-90ac-4897232619e1",
    "22c7cb44-1463-48f7-8e47-88857f207702"
  )

  get_one_fire <- function(id) {
    fires <- bcdata::bcdc_query_geodata(id) |>
      bcdata::filter(bcdata::INTERSECTS(aoi)) |>
      bcdata::collect() |>
      dplyr::select(
        "id", "FIRE_NUMBER", "VERSION_NUMBER", "FIRE_YEAR",
        "FIRE_SIZE_HECTARES", "LOAD_DATE"
      )
    # filter for recent fires
    if (nrow(fires) > 0) {
      fires <- sf::st_intersection(fires, aoi) |>
        dplyr::filter(as.numeric(format(Sys.time(), "%Y")) - .data$FIRE_YEAR <= 20)
    }
    fires
  }

  fires_list <- purrr::map(fire_records, get_one_fire)
  fires_all <- dplyr::bind_rows(fires_list)

  if (all(is.na(fires_all)) || nrow(fires_all) == 0) {
    cli::cli_alert_warning("No recent fire disturbance in area of interest")
  } else {
    sf::st_write(fires_all, fs::path(out_dir, "fire.gpkg"), append = FALSE)
  }
}

## 10. fire severity -------------------------------------
get_fire_severity <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading burn severity layer")

  fire_int <- bcdata::bcdc_query_geodata("c58a54e5-76b7-4921-94a7-b5998484e697") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select(c("FIRE_YEAR", "BURN_SEVERITY_RATING")) |>
    bcdata::collect()

  if (nrow(fire_int) > 0) {
    sf::st_write(fire_int, fs::path(out_dir, "fire_int.gpkg"), append = FALSE)
  } else {
    cli::cli_alert_warning("No burn severity in AOI")
  }
}


## 11) BC parks and National parks-----------------

get_parks <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading Parks")

  parks <- bcdata::bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(parks) > 0) {
    parks <- sf::st_intersection(parks, aoi)
  }
  if (nrow(parks) > 0) {
    sf::st_write(parks, fs::path(out_dir, "parks.gpkg"), append = FALSE)
  } else {
    cli::cli_alert_warning("no provincial parks in aoi")
  }

  # 12. National parks (if an option)
  national_parks <- bcdata::bcdc_query_geodata("88e61a14-19a0-46ab-bdae-f68401d3d0fb") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(national_parks) > 0) {
    national_parks <- sf::st_intersection(national_parks, aoi)
  }
  if (nrow(national_parks) > 0) {
    sf::st_write(national_parks, fs::path(out_dir, "natparks.gpkg"), append = FALSE)
  } else {
    cli::cli_alert_warning("no national parks in aoi")
  }
}

## 13) transmission lines -------------------------------
get_transmission_lines <- function(aoi, out_dir) {
  cli::cli_alert_info("Downloading transmission lines")

  # bcdc_search("transmission")
  trans_line <- bcdata::bcdc_query_geodata("384d551b-dee1-4df8-8148-b3fcf865096a") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(trans_line) > 0) {
    trans_line <- sf::st_intersection(trans_line, aoi)
  }
  if (nrow(trans_line) > 0) {
    sf::st_write(trans_line, fs::path(out_dir, "translines.gpkg"), append = FALSE)
  } else {
    cli::cli_alert_warning("No transmission lines in area of interest")
  }
}
