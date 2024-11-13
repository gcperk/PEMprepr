#' Read folder structure file
#'
#' Read the fid file from the `_meta` directory
#'
#' @param fid_path Path to the fid file. Defaults to `"_meta/fid.RDS"`
#'
#' @return list containing folder structure
#'
#' @export
read_fid <- function(fid_path = file.path("_meta", "fid.rds")) {
  if (!fs::file_exists(fid_path)) {
    return(NULL)
  }
  readRDS(fid_path)
}

# Add NULL default operator if doesn't exist (added to base R in v4.4)
if (!exists("%||%", envir = baseenv())) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

find_saga_path <- function(root = NULL) {
  # Set root path depending on operating system
  if (is.null(root)) {
    root <- switch(Sys.info()["sysname"],
      "Windows" = "C:/",
      "Darwin" = "/Applications",
      "Linux" = "/usr"
    )
  }

  # browser()
  cli::cli_progress_message("Searching for saga_cmd on your machine...")

  saga_loc <- fs::dir_ls(path = root, type = "file", regexp = "saga_cmd(\\.exe)?", recurse = TRUE, fail = FALSE)

  if (length(saga_loc) == 0) {
    cli::cli_abort("no matching files found.. please check you have saga installed")
  } else if (length(saga_loc) == 1) {
    saga_path <- saga_loc
  } else {
    cli::cat_line()
    prompt <- "Congratulations there are matches, select one of the following for your saga_path"
    choice <- utils::menu(title = prompt, choices = saga_loc)
    saga_path <- saga_loc[choice]
  }
  saga_cmd(saga_path)
}

saga_cmd <- function(saga_path = getOption("pemprepr.saga_path", default = ._pempreprenv_$saga_path)) {

  # If not specified, check for system saga
  saga_path <- saga_path %||% Sys.which("saga_cmd")

  if (saga_path == "") {
    cli::cli_abort("{.var saga_path} must be a path to the SAGA_cmd location on your computer.
                   Please check your program files or use `find_saga_path()` to locate the saga_cmd.exe file")
  }

  string_version <- system(
    paste(saga_path, "-v"),
    intern = TRUE
  )[1]

  num_version <- stringr::str_extract(string_version, "[-.0-9]{3,10}") |>
    as.numeric_version()

  if (num_version < "7.6") {
    cli::cli_warn("SAGA-GIS is version {.val {num_version}}; Not all covariates will generate.  Upgrade your SAGA, visit https://sourceforge.net/projects/saga-gis/files/")
  } else {
    cli::cli_inform(c("v" = "{.val {string_version}}"))
  }

  ._pempreprenv_$saga_path <- saga_path

  saga_path
}



#' Crop tiles
#' @family Layers
#' @rdname Layers
#' @keywords internal
#' @export
#'
read_crop <- function(f, poly, tmp) {
  #--- extract tile index to match with index in polygons - works up to 1000 tiles ---#
  index <- stringr::str_extract(
    f,
    pattern = "_([1-9]|[1-9][0-9]|[1-9][0-9][0-9]|1000)_"
  ) |>
    stringr::str_replace_all(
      index,
      pattern = "_",
      replacement = ""
    ) |>
    as.numeric()

  #--- write rasters to tmp folder ---#
  terra::rast(f) |>
    terra::crop(y = poly[index, ]) |>
    terra::writeRaster(
      paste0(tmp, basename(f)),
      overwrite = TRUE
    )
}
