# Set a directory for bcmaps cded cache so they can be reused across tests
opts <- options("bcmaps.data_dir" = fs::path(tempdir(), "bcmaps_cache"))

withr::defer(options(opts), teardown_env())
