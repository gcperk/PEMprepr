test_that("read_spatrast_if_necessary() passes through raster unaltered", {
  rast <- terra::rast(nrows=5, ncols=5, vals=1:25)

  expect_equal(read_spatrast_if_necessary(rast), rast)
})

test_that("read_spatrast_if_necessary() works on file", {
  tfile <- withr::local_tempfile(fileext = ".tif")

  rast <- terra::rast(nrows=5, ncols=5, vals=1:25)
  terra::writeRaster(rast, tfile)

  expect_true(
    all.equal(
      read_spatrast_if_necessary(tfile),
      rast,
      check.attributes = FALSE
    )
  )
})

test_that("read_spatrast_if_necessary() fails using parent function argument name", {
  r <- list()
  f <- function(my_arg) read_spatrast_if_necessary(my_arg)
  expect_snapshot(f(r), error = TRUE)
})

test_that("read_sf_if_necessary() passes through raster unaltered", {
  sf_object = sf::st_sf(a = "a", sf::st_sfc(sf::st_point(1:2)), crs = 3005)

  expect_equal(read_sf_if_necessary(sf_object), sf_object)
})

test_that("read_sf_if_necessary() works on file", {
  tfile <- withr::local_tempfile(fileext = ".gpkg")

  sf_object = sf::st_sf(a = "a", sf::st_sfc(sf::st_point(1:2)), crs = 3005)
  sf::st_write(sf_object, tfile)

  expect_true(
    all.equal(
      read_sf_if_necessary(tfile),
      sf_object,
      check.attributes = FALSE
    )
  )
})

test_that("read_sf_if_necessary() fails using parent function argument name", {
  r <- list()
  f <- function(my_arg) read_sf_if_necessary(my_arg)
  expect_snapshot(f(r), error = TRUE)
})
