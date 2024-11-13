._pempreprenv_ <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  ._pempreprenv_$saga_path <- getOption("pemprepr.saga_path", default = NULL) # nocov
}
