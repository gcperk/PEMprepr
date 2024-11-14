test_that("create_covariates fails with invalid input", {
  skip_if_no_saga()

  outdir <- withr::local_tempdir()
  aoi_snapped <- make_test_aoi(outdir)
  aoi_rast <- create_template_raster(aoi_snapped, res = 50, out_dir = outdir)

  expect_error(create_covariates(dtm = 1, layers = "all", out_dir = outdir))
  expect_error(create_covariates(dtm =  aoi_rast , saga_path = NULL, layers = "all", out_dir = outdir))
  expect_error(create_covariates(dtm =  aoi_rast, layers = "madeupcovar", out_dir = outdir))

})

test_that("create_covariates() works", {
  skip_if_no_saga()

  outdir <- withr::local_tempdir()
  aoi_snapped <- make_test_aoi(outdir)
  aoi_rast <- create_template_raster(aoi_snapped, res = 50, out_dir = outdir)

  create_covariates(dtm = aoi_rast, layers = "flowaccumulation", out_dir = outdir)
  expect_snapshot(list.files(outdir, recursive = TRUE))
})
