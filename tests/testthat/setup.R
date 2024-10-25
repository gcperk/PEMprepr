# Set a directory for bcmaps cded cache on GitHub actions so they can be reused
# across tests, otherwise each test is osolated and they are downloaded each
# time. Use default cache location when running locally so cached files persist
# across test runs
on_ci <- isTRUE(as.logical(Sys.getenv("CI", "false")))
if (on_ci) {
  cache_path <- fs::path(tempdir(), "bcmaps_cache")
  opts <- options("bcmaps.data_dir" = cache_path)

  withr::defer(options(opts), teardown_env())
  withr::defer(unlink(cache_path, recursive = TRUE), teardown_env())
}
