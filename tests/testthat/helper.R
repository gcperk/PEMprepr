make_test_aoi <- function(outdir) {
  snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    out_dir = fs::path(outdir, "snap")
  )
}

on_ci <- function() isTRUE(as.logical(Sys.getenv("CI", "false")))

on_linux <- function() tolower(Sys.info()[["sysname"]]) == "linux"

skip_if_no_saga <- function() {
  testthat::skip_if(
    is.null(getOption("pemprepr.saga_path")) && Sys.which("saga_cmd") == ""
  )
}
