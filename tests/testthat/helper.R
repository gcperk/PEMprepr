make_test_aoi <- function(outdir) {
  snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    out_dir = fs::path(outdir, "snap")
  )
}


make_test_bec <- function(outdir) {

  aoi_snapped <- make_test_aoi(outdir)
  #out_dir = fs::path(outdir, "snap")

  bec <- bcdata::bcdc_query_geodata("f358a53b-ffde-4830-a325-a5a03ff672c3") |>
      bcdata::filter(bcdata::INTERSECTS(aoi_snapped)) |>
      bcdata::select("MAP_LABEL") |>
      bcdata::collect() |>
      dplyr::select("MAP_LABEL")

  bec <- sf::st_intersection(bec, aoi_snapped)
  sf::st_write(bec, fs::path(outdir, "bec.gpkg"), append = FALSE)
}

