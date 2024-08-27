create_pemr_project <- function(
    path = ".",
    aoi_name,
    rstudio = rstudioapi::isAvailable(),
    open = rlang::is_interactive()
) {
  path = fs::path(path, aoi_name)

  # Create project but don't open yet as we need to add the infrastructure
  project_path <- usethis::create_project(path, rstudio = rstudio, open = FALSE)

  # Activate the new project directory inside the scope of this function
  # so we can easily do stuff inside it
  usethis::local_project(project_path)
  
  ## Add the project-specific infrastructure
  use_directory("00_raw_inputs")

  if (open) {
    if (usethis::proj_activate(usethis::proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
    }
  }

  invisible(usethis::proj_get())
}
