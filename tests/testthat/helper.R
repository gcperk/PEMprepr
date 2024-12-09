make_test_aoi <- function(outdir) {
  snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    out_dir = fs::path(outdir, "snap")
  )
}

on_ci <- function() isTRUE(as.logical(Sys.getenv("CI", "false")))

on_linux <- function() tolower(Sys.info()[["sysname"]]) == "linux"

skip_if_no_saga <- function() {
  # Developers should locally set the environment variable `SAGA_PATH` or 
  # options(pemprepr.saga_path = "path to saga_cmd")
  # in their .Renviron or .Rprofile file, or have it installed and on the `PATH`
  # On Linux (on GitHub Actions) it is installed with `apt install saga`, which
  # puts it on the path, so Sys.which("saga_cmd") should find it
  testthat::skip_if(
    is.null(saved_saga_path()) && is.null(getOption("pemprepr.saga_path")) && Sys.which(saga_cmd_string()) == ""
  )
}
