test_that("snap_aoi works with sf obj and file", {
  snap1 <- snap_aoi(
    sf::st_read(fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg")),
    write_output = FALSE
  )

  snap2 <- snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    write_output = FALSE
  )

  expect_equal(snap1, snap2)
})

test_that("snap_aoi writing works", {
  dir <- withr::local_tempdir()

  snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    out_dir = dir
  )

  expect_true(file.exists(fs::path(dir, "aoi_snapped.gpkg")))
})

test_that("snap_aoi with 'method' works", {
  snap_expand <- snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    write_output = FALSE
  )

  snap_shrink <- snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    method = "shrink",
    write_output = FALSE
  )

  expect_gt(
    sf::st_area(snap_expand),
    sf::st_area(snap_shrink)
  )
})

test_that("snap_aoi buffer works", {
  snap_0 <- snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    write_output = FALSE
  )

  snap_100 <- snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    buffer = 100,
    write_output = FALSE
  )

  expect_gt(
    sf::st_area(snap_100),
    sf::st_area(snap_0)
  )
})

# TODO: Add test that creates PEMr project and test searching for AOI in folder
# structure
