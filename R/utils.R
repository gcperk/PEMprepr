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

#' Layers
#'
#' @param layers Character vector. SAGA modules to call
#' @param moddir Internal Data. Cartesian coordinates of each strata
#' @param artifact Internal Data. Determines proportion of cells to plot
#' @family Layers
#' @name Layers
#' @return Vector of recursive layers to call.
NULL

#' Recursive layers call
#' @family Layers
#' @rdname Layers
#' @keywords internal
#' @export

recursive_layers_call <- function(layers, moddir = moddir, artifact = artifacts){

  #--- which modules are being called ---#

  moddirs <- moddir |>
    dplyr::filter("module" %in% layers)

  mods <- moddirs$module

  #--- determine recursive inputs of desired modules ---#

  while(any(moddirs[,2:3] != "")){

    p1 <- unique(moddirs$par1)

    p1 <- p1[!p1 %in% mods]

    p2 <- unique(moddirs$par2)

    p2 <- p2[!p2 %in% mods]

    v1 <- c(p1,p2)

    v1 <- v1[v1 != ""]

    moddirs <- moddir |>
      dplyr::filter("module" %in% v1)

    mods <- c(mods,moddirs$module)
  }

  #--- ordered vector of modules (and their recursive inputs) to call ---#

  layers_to_call <- mods[order(match(mods, moddir$module))]


  #--- if artifact is provided report the modules that produce artifacts if they are requested ---#

  if(!is.null(artifact)){

    artifactdirs <- artifact  |>
      dplyr::filter(artifacts == TRUE) |>
      dplyr::select("metric")

    artifact_layers <- layers_to_call[layers_to_call %in% artifactdirs$metric]

    if(length(artifact_layers) != 0){

      warning(paste0(artifact_layers, collapse = ", ")," -- produce artifacts during tiled processing.", call. = FALSE)

    }

  }

  #--- modules needed to be called based on inputs ---#

  return(layers_to_call)

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

#' Covariate file paths
#' @family Layers
#' @rdname Layers
#' @keywords internal
#' @export
#'
covariate_file_names <- function(outputdir){

  sinkroute <- paste0(outputdir,"/sinkroute/","sinkroute.sgrd")
  sinksfilled <- paste0(outputdir,"/sinksfilled/","sinksfilled.sgrd")
  dem_preproc <- paste0(outputdir,"/dem_preproc/","dem_preproc.sgrd")
  slope <- paste0(outputdir,"/slope_aspect_curve/","slope.sgrd")
  aspect <- paste0(outputdir,"/slope_aspect_curve/","aspect.sgrd")
  gencurve <- paste0(outputdir,"/slope_aspect_curve/","gencurve.sgrd")
  totcurve <- paste0(outputdir,"/slope_aspect_curve/","totcurve.sgrd")
  tcatchment <- paste0(outputdir,"/tcatchment/","tcatchment.sgrd")
  tca <- paste0(outputdir,"/tca/","tca1.sgrd")
  flowlength4 <- paste0(outputdir,"/tca/","flowlength1.sgrd") ## part of tca
  scatchment <- paste0(outputdir,"/scatchment/","scatchment.sgrd")
  twi <- paste0(outputdir,"/twi/","twi.sgrd")
  channelsnetwork <- paste0(outputdir,"/channelsnetwork/","cnetwork.sgrd")
  hdistance <-  paste0(outputdir,"/overlandflow/","hdist.sgrd")
  vdistance  <- paste0(outputdir,"/overlandflow/","vdist.sgrd")
  hdistancenob <-  paste0(outputdir,"/overlandflow2/","hdistnob.sgrd")
  vdistancenob  <- paste0(outputdir,"/overlandflow2/","vdistnob.sgrd")
  MRVBF <- paste0(outputdir,"/multiresflatness/","mrvbf.sgrd")
  MRRTF <- paste0(outputdir,"/multiresflatness/","mrrtf.sgrd")
  MRVBF2 <- paste0(outputdir,"/multiresflatness2/","mrvbf2.sgrd")
  MRRTF2 <- paste0(outputdir,"/multiresflatness2/", "mrrtf2.sgrd")
  tri <- paste0(outputdir,"/tri/","tri.sgrd")
  convergence <- paste0(outputdir,"/convergence/","convergence.sgrd")
  opos <- paste0(outputdir,"/openness/","open_pos.sgrd")
  oneg <- paste0(outputdir,"/openness/","open_neg.sgrd")
  dAH <- paste0(outputdir,"/dah/","dah.sgrd")
  tpi <- paste0(outputdir,"/tpi/","tpi.sgrd")
  val_depth <- paste0(outputdir,"/ridgevalley/","val_depth.sgrd")
  ridgelevel <- paste0(outputdir,"/ridgevalley/","rid_level.sgrd")
  mrncatchment <- paste0(outputdir,"/mrn/","mrn_area.sgrd")
  mrnmaxheight <- paste0(outputdir,"/mrn/","mrn_mheight.sgrd")
  mrn <- paste0(outputdir,"/mrn/","mrn.sgrd")
  flowaccumft <- paste0(outputdir,"/flowaccumulation/","flow_accum_ft.sgrd")
  meanovcatch <- paste0(outputdir,"/flowaccumulation/","meanovcatch.sgrd")
  accummaterial <- paste0(outputdir,"/flowaccumulation/","accummaterial.sgrd")
  slopelength <- paste0(outputdir,"/slopelength/","slength.sgrd")
  flowaccump <- paste0(outputdir,"/flowaccumulation2/","flow_accum_p.sgrd")
  flowaccumtd <- paste0(outputdir,"/flowaccumulation3/","flow_accum_td.sgrd")
  meanovcatchTD <- paste0(outputdir,"/flowaccumulation3/","meanovcatchTD.sgrd")
  accummaterialTD <- paste0(outputdir,"/flowaccumulation3/","accummaterialTD.sgrd")
  flowpathlenTD <- paste0(outputdir,"/flowaccumulation3/","flowpathlenTD.sgrd")
  flowpathlength <- paste0(outputdir,"/flowpathlength/","max_fp_l.sgrd")
  flowpathlength2 <- paste0(outputdir,"/flowpathlength2/","max_fp_l2.sgrd")
  flowpathlength3 <- paste0(outputdir,"/flowpathlength3/","max_fp_l3.sgrd")
  #FloOwAccum <- paste0(outputdir,"/slope_lts_fa/","slope_lts_fa.sgrd") isnt called in create_covariates()
  lsfactor <- paste0(outputdir,"/lsfactor/","ls_factor.sgrd")
  DirInsol <- paste0(outputdir,"/solarrad/","direinso.sgrd")
  DifInsol <- paste0(outputdir,"/solarrad/","diffinso.sgrd")
  convexity <- paste0(outputdir,"/convexity/","convexity.sgrd")
  vertdistance <- paste0(outputdir,"/vertdistance/","vert_dis.sgrd")
  tci_low <- paste0(outputdir,"/tci_low/","tci_low.sgrd")
  catchmentarea <- paste0(outputdir,"/swi/","swi_area.sgrd")
  catchmentslope <- paste0(outputdir,"/swi/","swi_slope.sgrd")
  modcatchmentarea <- paste0(outputdir,"/swi/","swi_area_mod.sgrd")
  topowetindex <- paste0(outputdir,"/swi/","swi_twi.sgrd")
  windexp <- paste0(outputdir,"/windexp/","wind_exp_index.sgrd")
  texture <- paste0(outputdir,"/texture/","texture.sgrd")
  protection <- paste0(outputdir,"/protection/","protection.sgrd")
  vrm <- paste0(outputdir,"/vrm/","vrm.sgrd")
  mbi <- paste0(outputdir,"/mbi/","mbi.sgrd")
  mscale_tpi <- paste0(outputdir,"/mscale_tpi/","mscale_tpi.sgrd")
  slopeheight <- paste0(outputdir,"/relposition/","slope_height.sgrd")
  valleydepth <- paste0(outputdir,"/relposition/","valleydepth.sgrd") #don't need this as created above?
  normheight <- paste0(outputdir,"/relposition/","norm_height.sgrd")
  standheight <- paste0(outputdir,"/relposition/","stand_height.sgrd")
  msposition <- paste0(outputdir,"/relposition/","ms_position.sgrd")
  localcurve <- paste0(outputdir,"/slopecurvatures/","local_curv.sgrd")
  upslopecurve <- paste0(outputdir,"/slopecurvatures/","upslope_curv.sgrd")
  localupcurve <- paste0(outputdir,"/slopecurvatures/","local_upslope_curv.sgrd")
  downcurve <- paste0(outputdir,"/slopecurvatures/","down_curv.sgrd")
  localdowncurve <- paste0(outputdir,"/slopecurvatures/","local_downslope_curv.sgrd")
  steepestslope <- paste0(outputdir,"/steepestslope/","steepest_slope.sgrd")
  upslopearea <- paste0(outputdir,"/upslopearea/","upslopearea.sgrd")

  environment()

}
