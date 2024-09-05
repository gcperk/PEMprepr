test_that("snap_aoi works with sf obj and file", {
  snap1 <- snap_aoi(
    sf::st_read(
      fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg")), 
    write_output = FALSE
  )

  snap2 <- snap_aoi(
    fs::path_package("PEMprepr", "extdata/datecreek_aoi.gpkg"),
    write_output = FALSE
  )

  expect_equal(snap1, snap2)
})
