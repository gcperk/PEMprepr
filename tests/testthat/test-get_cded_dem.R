test_that("get_cded fails with invalid input", {

   outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)

   aoi_rast <- create_template_raster(aoi_snapped, res = 50,
                                      out_dir = outdir)

  expect_error(get_cded_dem(aoi = 1, res = 1))
  expect_error(get_cded_dem(aoi = aoi_rast, res = "number"))

})

# test_that("get_cded_dem works with multiple input types", {
#   outdir <- withr::local_tempdir()
#
#   aoi_snapped <- snap_aoi(
#     fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
#     out_dir = fs::path(outdir, "snap")
#   )
#
#   aoi_rast <- create_template_raster(aoi_snapped, res = 50,
#                                      out_dir = outdir)
#
#   rast_cded <- get_cded_dem(aoi_rast,
#                             res = 50,
#                             out_dir = outdir,
#                             write_output = TRUE,
#                             overwrite = FALSE)
#
#   in_text_rast <- fs::path(outdir, "50m","template.tif")
#   text_cded <- get_cded_dem(in_text_rast,
#                             res = 50,
#                             out_dir = outdir,
#                             write_output = FALSE,
#                             overwrite = FALSE)
#
#   expect_equal(rast_cded, text_cded)
#
# })

