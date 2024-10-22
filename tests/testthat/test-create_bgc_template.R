test_that("create_bec_template works with multiple input types", {

  skip_if_offline()
  skip_on_cran()

  outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)
  bec_test <- make_test_bec(outdir)
  rast_temp <- create_template_raster(aoi_snapped, write_output = FALSE)

  bec_rast <- create_bgc_template(
    bec =  bec_test,
    field = "MAP_LABEL",
    template_rast = rast_temp,
    write_output = TRUE,
    out_dir = outdir
  )

  expect_true(
    file.exists(fs::path(outdir, "25m","bec.tif"))
  )

  expect_s4_class(bec_rast , "SpatRaster")

  in_text_bec <- fs::path(outdir, "bec.gpkg")

  bec_text <- create_bgc_template(
    bec =  in_text_bec,
    field = "MAP_LABEL",
    template_rast = rast_temp,
    write_output = FALSE,
    out_dir = outdir
  )

  expect_true(
    all.equal(bec_rast,  bec_text)
  )

})
