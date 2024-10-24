test_that("get_cded fails with invalid input", {
  skip_if_offline()
  skip_on_cran()

  outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)

  aoi_rast <- create_template_raster(aoi_snapped, res = 50, out_dir = outdir)

  expect_error(get_cded_dem(aoi = 1, res = 1))
  expect_error(get_cded_dem(aoi = aoi_rast, res = "number"))

})

test_that("get_cded_dem works with multiple input types", {
  skip_if_offline()
  skip_on_cran()

  outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)

  aoi_rast <- create_template_raster(aoi_snapped, res = 50, out_dir = outdir)

  rast_cded <- get_cded_dem(
    aoi_rast,
    res = 50,
    out_dir = outdir,
    write_output = TRUE,
    overwrite = FALSE,
    ask = FALSE
  )

  expect_true(
    file.exists(fs::path(outdir, "50m","dem.tif"))
  )

  expect_s4_class(rast_cded, "SpatRaster")

  in_text_rast <- fs::path(outdir, "50m","template.tif")

  text_cded <- get_cded_dem(
    in_text_rast,
    res = 50,
    out_dir = outdir,
    write_output = FALSE,
    check_tiles = FALSE
  )

  expect_true(
    # Use terra::all.equal() instead of testthat::expect_equal(), as the latter
    # will fail because they contain # c++ pointers, which will almost
    # always be different
    all.equal(rast_cded, text_cded)
  )

})

test_that("res works", {
  skip_if_offline()
  skip_on_cran()

  outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)

  aoi_rast <- create_template_raster(aoi_snapped, res = 100,out_dir = outdir)

  rast_cded <- get_cded_dem(
    aoi_rast,
    res = 100,
    out_dir = outdir,
    write_output = FALSE,
    ask = FALSE
  )

  expect_equal(terra::res(rast_cded), c(100, 100))
})
