test_that("create_template_raster works with file input", {
  outdir <- withr::local_tempdir()

  aoi <- fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg")

  out <- create_template_raster(aoi, write_output = FALSE)
  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::res(out), c(25, 25))

  out <- create_template_raster(aoi, res = 50, write_output = FALSE)
  expect_equal(terra::res(out), c(50, 50))

  out <- create_template_raster(aoi, out_dir = outdir, write_output = TRUE)
  expect_s4_class(out, "SpatRaster")
  expect_true(
    fs::file_exists(fs::path(outdir, "25m", "template.tif"))
  )
  expect_s4_class(
    terra::rast(fs::path(outdir, "25m", "template.tif")),
    "SpatRaster"
  )
})

test_that("create_template_raster works with sf input", {
  outdir <- withr::local_tempdir()

  aoi <- sf::read_sf(fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"))

  out <- create_template_raster(aoi, write_output = FALSE)
  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::res(out), c(25, 25))

  out <- create_template_raster(aoi, res = 50, write_output = FALSE)
  expect_equal(terra::res(out), c(50, 50))

  out <- create_template_raster(aoi, out_dir = outdir, write_output = TRUE)
  expect_s4_class(out, "SpatRaster")
  expect_true(
    fs::file_exists(fs::path(outdir, "25m", "template.tif"))
  )
  expect_s4_class(
    terra::rast(fs::path(outdir, "25m", "template.tif")),
    "SpatRaster"
  )
})
