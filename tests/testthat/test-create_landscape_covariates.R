# test_that("create_landscape_covariates fails with invalid input", {
#
#   outdir <- withr::local_tempdir()
#   sp <- "C:/Programs/saga-9.2.0_x64/saga-9.2.0_x64/saga_cmd.exe"
#   aoi_snapped <- make_test_aoi(outdir)
#   aoi_rast <- create_template_raster(aoi_snapped, res = 50, out_dir = outdir)
#   dem = rast_cded <- get_cded_dem( aoi_rast,
#                          res = 100,
#                         out_dir = outdir,
#                         write_output = TRUE,
#                         overwrite = TRUE,
#                         ask = FALSE
#   )
#
#
#   expect_error(create_landscape_covariates(dtm = 1, saga_path = sp))
#   expect_error(create_landscape_covariates(dtm =  aoi_rast , saga_path = NULL))
#
#   create_landscape_covariates(dtm = dem, saga_path = sp,  out_dir = outdir))
#
#   expect_true(
#     file.exists(fs::path(outdir, "mrvbf_LS.tif"))
#    )
#
# })

