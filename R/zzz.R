._pempreprenv_ <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  # Check if it's set as an environment variable first. This is undocumented,
  # but the safest path for developers as this is consulted during R CMD check,
  # but .Rprofile is not
  saga_envvar_path <- if (nzchar(Sys.getenv("SAGA_PATH"))) {
    Sys.getenv("SAGA_PATH")
  } else {
    NULL
  }

  ._pempreprenv_$saga_path <- saga_envvar_path %||% 
    getOption("pemprepr.saga_path", default = NULL) # nocov
  }
