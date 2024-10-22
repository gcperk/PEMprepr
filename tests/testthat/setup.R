# Set a directory for bcmaps cded cache so they can be reused across tests
cache_path <- fs::path(tempdir(), "bcmaps_cache")
opts <- options("bcmaps.data_dir" = cache_path)

withr::defer(options(opts), teardown_env())
withr::defer(unlink(cache_path, recursive = TRUE), teardown_env())
