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
  project_dirs <- create_directories()

  write_core_files(data = list(aoi_name = aoi_name))

  fid <- make_fid(project_dirs)

  saveRDS(fid, file.path("_meta", "fid.rds"))
  
  if (open) {
    if (usethis::proj_activate(usethis::proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
    }
  }
  
  invisible(usethis::proj_get())
}

create_directories <- function(file = fs::path_package("PEMprepr", "extdata/directory_structure.csv")) {
  dir_df <- utils::read.csv(file) 
  
  project_dirs <- fs::path(
    dir_df$base_dir, dir_df$subdir_1, dir_df$subdir_2, dir_df$subdir_3
  )

  lapply(c(project_dirs, "_meta"), use_directory)
  project_dirs
}

write_core_files <- function(data) {
  core_files <- list.files(fs::path_package("PEMprepr", "templates", "core"))
  lapply(core_files, function(x) {
    fpath <- fs::path("core", x)
    use_template(fpath, x, data = data, package = "PEMprepr")
  })
}

make_fid <- function(dirs) {
  fid <- as.list(dirs)

  names(fid) <- paste0(
    "dir_", # so doesn't start with a number
    gsub("[.-/_a-zA-Z]", "", fs::path_dir(dirs)), # only numbers for base baths
    fs::path_file(dirs) # full dirname for terminal directory
  )

  fid <- lapply(fid, function(x) {
    list(
      "path_rel" = x[1],
      "path_abs" = fs::path_abs(x[1])
    )
  })

  fid
}
