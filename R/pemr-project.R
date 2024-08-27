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
  project_dirs <- create_directories(
    file = fs::path_package("PEMprepr", "extdata/directory_structure.csv")
  )
  
  lapply(project_dirs, use_directory)
  
  if (open) {
    if (usethis::proj_activate(usethis::proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
    }
  }
  
  invisible(usethis::proj_get())
}

create_directories <- function(file) {
  dir_df <- read.csv(file) 
  
  fs::path(
    dir_df$base_dir, dir_df$subdir_1, dir_df$subdir_2, dir_df$subdir_3
  )
}