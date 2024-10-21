make_test_aoi <- function(outdir) {
  snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    out_dir = fs::path(outdir, "snap")
  )
}
