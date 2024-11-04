test_that("create_covariates fails with invalid input", {

  outdir <- withr::local_tempdir()
  sp <- "C:/Programs/saga-9.2.0_x64/saga-9.2.0_x64/saga_cmd.exe"
  aoi_snapped <- make_test_aoi(outdir)
  aoi_rast <- create_template_raster(aoi_snapped, res = 50, out_dir = outdir)

  expect_error(create_covariates(aoi = 1, saga_path = sp, layers = "all", out_dir = outdir))
  expect_error(create_covariates(aoi =  aoi_rast , saga_path = NULL, layers = "all", out_dir = outdir))
  expect_error(create_covariates(aoi =  aoi_rast , saga_path = sp, layers = "madeupcovar", out_dir = outdir))

})
