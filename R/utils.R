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
  if (!fs::file_exists(fid_path)) return(NULL)
  readRDS(fid_path)
}


if (!exists("%||%", envir = baseenv())) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}