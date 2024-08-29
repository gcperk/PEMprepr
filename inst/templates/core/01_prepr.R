library(PEMr)

# load all pem packages
run_pemr()

project_name <- "{{ aoi_name }}"

fid <- read_fid()

aoi_path <- create_aoi()

## works this far...

aoi <- read_sf(aoi_path)

create_base_vectors()

v <- list.files(path = fid$shape_dir_0010[1], pattern = ".gpkg", recursive = TRUE)
v

copy_clean_vectors()

# download all vector data layers

create_base_vectors(
  in_aoi = aoi,
  out_path = fid$shape_dir_0010[1]
)

v <- list.files(
  path = fid$shape_dir_0010[1], pattern = ".gpkg",
  recursive = TRUE
)
v


###############################
# Prepare Raster Data

# create a blank raster template

r5 <- create_template(res = 5)
r25 <- create_template(res = 25)
aoi_bb <- file.path(fid$shape_dir_1010[2], "aoi_ls_snap.gpkg")
r25_ls <- create_template(aoi_bb = aoi_bb, res = 25, filename = "template_ls.tif")


# read in base DEM and align the dEM
dem <- terra::rast(file.path(fid$cov_dir_1020[2], paste0(res_scale, "m"), "dem.tif"))

## OR alternatively read in TRIM raster

get_trim_dem(
  aoi_bb = file.path(fid$shape_dir_1010[2], "aoi_snapped.gpkg"),
  res = 25, out_dir = fid$cov_dir_1020[2]
)

get_trim_dem(res = 5)

get_trim_dem(
  aoi_bb = file.path(fid$shape_dir_1010[2], "aoi_ls_snap.gpkg"),
  res = 25, filename = "dem_ls.tif"
)



layer_options <- c(
  "sinksfilled", "sinkroute", "dem_preproc", "slope_aspect_curve",
  "tcatchment", "tca", "scatchment", "twi", "channelsnetwork",
  "overlandflow", "overlandflow2", "multiresflatness", "multiresflatness2",
  "multiresflatness3", "tri", "convergence", "openness",
  "dah", "tpi", "ridgevalley", "mrn", "flowaccumulation",
  "slopelength", "flowaccumulation2", "flowaccumulation3",
  "flowpathlength", "flowpathlength2", "flowpathlength3", "lsfactor",
  "solarrad", "convexity", "vertdistance", "tci_low",
  "swi", "windexp", "texture", "protection", "vrm",
  "mbi", "mscale_tpi", "relposition", "slopecurvatures",
  "steepestslope", "upslopearea"
)

# run a test covariate
create_covariates(
  dtm = dem, ## raster created above
  SAGApath = "C:/SAGA/saga-7.7.0_x64/", ## Where SAGA GIS is installed
  output = file.path(fid$cov_dir_1020[2], "25m"), ## from the setup_folders above
  layers = "sinksfilled"
) ## test one is working

# run all covariates
create_covariates(
  dtm = dem, ## raster created above
  SAGApath = "C:/SAGA/saga-7.7.0_x64/", ## Where SAGA GIS is installed
  output = file.path(fid$cov_dir_1020[2], "25m"), ## from the setup_folders above
  layers = c("all")
) ## use all or one of the above


l <- list.files(
  path = fid$cov_dir_1020[2], pattern = ".sdat$",
  recursive = TRUE
)


# generate BEC

bec_sf <- sf::st_read(file.path(fid$shape_dir_1010[1], "bec.gpkg")) %>%
  sf::st_cast(., "MULTIPOLYGON")

bec_rast <- create_bgc_template(bec_sf,
  field = "MAP_LABEL", template = r25,
  outpath = fid$cov_dir_1020[2]
)
