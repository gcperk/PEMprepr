#
# # script to copy of files which have been reviewed and edits from raw to clean
#
# copy_clean_vectors <- function(raw_dir = file.path("00_raw_inputs","10_vector"),
#     out_dir = file.path("10_clean_inputs","10_vector")){
#
#     # Dont copy over raw aoi?
#     # Option to delete old files
#
#     clean_files <- list.files(fs::path(raw_dir), pattern = c("*.gpkg"),recursive = TRUE)
#
#     lapply(clean_files, function(x)
#       #x <- clean_files[1]
#       file.copy(fs::path(raw_dir, x),
#                 fs::path(out_dir, x),
#                 recursive = FALSE,
#                 copy.mode = TRUE))
#
#     cli::cat_line()
#     cli::cli_alert_success(
#       "Cleaned vector layers copied to {.path {out_dir}}"
#     )
# }
