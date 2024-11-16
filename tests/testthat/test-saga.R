test_that("saga_version works", {
  skip_if_no_saga()

  saga_path <- getOption("pemprepr.saga_path", default = Sys.which("saga_cmd"))
  expect_s3_class(saga_version(saga_path), "numeric_version")
})

test_that("saga_cmd warns with insufficient version", {
  skip_if_no_saga()

  local_mocked_bindings(saga_version = function(saga_path) numeric_version("7.2"))

  expect_snapshot(saga_cmd(), transform = function(x) gsub("^.*saga_cmd.*$", "'user-specific-saga-path'", x))
})

test_that("saga_cmd() returns path to SAGA", {
  skip_if_no_saga()

  expect_message(saga_path <- saga_cmd(), "SAGA version")
  expect_type(saga_path, "character")
  expect_true(grepl("saga", tolower(saga_path)))
  expect_true(file.exists(saga_path))
})
