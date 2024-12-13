#' Discover the path to your SAGA installation (`saga_cmd(.exe)` executable)
#'
#' Searches in common places where SAGA could be installed. If you know where it
#' is, supply the path to `root`. It also checks to see if there is a `saga_cmd`
#' on the `PATH`. After the installation is found, it validates it and checks
#' for an adequate version via [saga_cmd()].
#'
#' If you don't have SAGA installed:
#' * On Windows or MacOS, install the latest version from:
#' <https://sourceforge.net/projects/saga-gis/files/>. On MacOS, it is
#' recommended to move `SAGA.app` into your `/Applications` folder, and you may
#' need to turn on permissions for the SAGA in your System Preferences ->
#' Privacy and Security.
#' * Linux: On Ubuntu and Debian run `apt install saga`
#'
#' @param root where to start the search. The defaults are:
#' * Windows: "C:/"
#' * MacOS: "/Applications"
#' * Linux: "/usr"
#'
#' @return path to `saga_cmd` executable
#' @seealso [saga_cmd()]
#' @export
find_saga_path <- function(root = NULL) {
  # Set root path depending on operating system
  if (is.null(root)) {
    root <- switch(tolower(Sys.info()["sysname"]),
      "windows" = "C:/",
      "darwin" = "/Applications",
      "linux" = "/usr"
    )
  }

  # browser()
  cli::cli_progress_message("Searching for saga_cmd on your machine...")

  saga_loc <- fs::dir_ls(
    path = root,
    type = "file",
    regexp = saga_cmd_string(),
    recurse = TRUE,
    fail = FALSE
  )

  sys_saga <- Sys.which(saga_cmd_string())
  if (nzchar(sys_saga)) saga_loc <- unique(c(sys_saga, saga_loc))

  if (length(saga_loc) == 0) {
    cli::cli_abort(
      "no matching files found.. please check you have saga installed"
    )
  } else if (length(saga_loc) == 1) {
    saga_path <- saga_loc
  } else if (interactive()) {
    cli::cat_line()
    prompt <- "Congratulations there are matches, select one of the following for your saga_path"
    choice <- utils::menu(title = prompt, choices = saga_loc)
    saga_path <- saga_loc[choice]
  } else {
    cli::cli_abort(
      "Multiple SAGA installations found. Please set {.code options(pemprepr.saga_path = 'path to your saga_cmd file')}."
    )
  }
  saga_cmd(saga_path)
}


#' Validate and return the path to SAGA `saga_cmd(.exe)` executable
#'
#' Accepts a path to the `saga_cmd(.exe)` executable, or looks in
#' `getOption("pemprepr.saga_path")`. After being run once, sets an internal
#' variable so that the path is remembered in subsequent runs in the same R
#' session.
#'
#' It is recommended to set the option in your `.Rprofile` file with:
#' `options("pemprepr.saga_path" = "path to your saga_cmd(.exe) file")`. This
#' will ensure SAGA is always found when it is needed.
#'
#' @param saga_path Path to `saga_cmd` executable file. By default checks to 
#' see if it has been set yet this sessions, and if not consults
#' `getOption("pemprepr.saga_path")`.
#'
#' @return path to saga_cmd executable
#' @seealso [find_saga_path()]
#' @export
saga_cmd <- function(
    saga_path = saved_saga_path()
) {

  saga_cmd_string <- saga_cmd_string()
  # If not specified, check for system saga
  saga_path <- saga_path %||% getOption("pemprepr.saga_path") %||% Sys.which(saga_cmd_string)

  if (saga_path == "") {
    cli::cli_abort(
      "{.var saga_path} must be a path to the {.val {saga_cmd_string}} location on your computer.
      Please check your program files or use {.fn find_saga_path} to locate the {.val {saga_cmd_string}} file"
    )
  }

  saga_version <- saga_version(saga_path)

  if (saga_version < "7.6") {
    cli::cli_warn("Using {.val SAGA version {saga_version}}; Not all covariates will
    generate. Upgrade your SAGA, visit https://sourceforge.net/projects/saga-gis/files/")
  } else {
    cli::cli_inform(c("v" = "{.val SAGA version {saga_version}}"))
  }

  ._pempreprenv_$saga_path <- saga_path

  unname(saga_path)
}

saga_cmd_string <- function() {
  if (tolower(Sys.info()["sysname"]) == "windows") {
    "saga_cmd.exe"
  } else {
    "saga_cmd"
  }
}

saga_version <- function(saga_path) {
  string_version <- system(
    paste(saga_path, "-v"),
    intern = TRUE
  )[1]

  stringr::str_extract(string_version, "[-.0-9]{3,10}") |>
    as.numeric_version()
}

saved_saga_path <- function() ._pempreprenv_$saga_path
