# create_base_vectors fails with invalid input

    Code
      snap_aoi(1, out_dir = fs::path(outdir, "snap"))
    Condition
      Error in `snap_aoi()`:
      ! `aoi` must be an sf or an sfc object or a path to a file

# create_base_vectors works with sf and/or path to file

    Code
      fs::path_file(fs::dir_ls(outdir))
    Output
       [1] "bec.gpkg"            "cutblocks.gpkg"      "cutblocks_ften.gpkg"
       [4] "fire.gpkg"           "ften.gpkg"           "major_towns_bc.gpkg"
       [7] "road_network.gpkg"   "snap"                "vri.gpkg"           
      [10] "vri_class1_2.gpkg"   "vri_class3.gpkg"     "vri_decid.gpkg"     
      [13] "water.gpkg"         

