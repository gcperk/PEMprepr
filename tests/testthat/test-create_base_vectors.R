test_that("create_base_vectors fails with invalid input", {
  outdir <- withr::local_tempdir()

  expect_snapshot(
    snap_aoi(1, out_dir = fs::path(outdir, "snap")),
    error = TRUE
  )
})

test_that("create_base_vectors works with sf and/or path to file", {
  skip_if_offline()
  skip_on_cran()

  outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)

  # Randomly select sf or character method to test, so we only run the whole
  # function once - it is a lot of bcdata calls!
  which_method <- sample(c("sf", "character"), size = 1)
  input <- switch(
    which_method,
    "sf" = aoi_snapped,
    "character" = fs::path(outdir, "snap", "aoi_snapped.gpkg")
  )

  message("Testing ", which_method, " method of `create_base_vectors()")

  out <- create_base_vectors(
    input,
    out_dir = outdir
  )

  expect_equal(outdir, out)
  expect_snapshot(fs::path_file(fs::dir_ls(outdir)))
})
