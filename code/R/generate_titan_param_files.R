## Generate TITAN parameter files by permuting over possible parameter values.
## Tim Raupach, 2019.

## Settings.
low_thresh = seq(34, 42, by=1)         ## Values for low_dbz_threshold.
dual_by = 1                            ## Increment for dbz_threshold in dual_threshold.
dual_min = 4                           ## Try from low_dbz_threshold + dual_min, for dbz_threshold.
dual_max = 12                          ## Try up to low_dbz_threshold + dual_max, for dbz_threshold.
min_size = c(25, 50, 75)               ## Values for min_storm_size (km3).

outdir = "~/titan/test_params/"        ## Output location for parameter files.
otherparams = "~/git/stormtrack/params/test_param_base.txt" ## Base parameter file.

if(!file.exists(outdir)) {
    dir.create(outdir)
} else {
    print("WARNING: output directory already exists.")
}

for(low in low_thresh) {
    ## Values for dbz_threshold in dual_threshold.
    dual_thresh = seq(low+dual_min, low+dual_max, by=dual_by) 
    
    for(dual in dual_thresh) {
      for(size in min_size) {    
        filename = paste(outdir, "/titan_params_", low, "_", dual, "_", size, ".opt", sep="")
      
        f = file(filename, "w")
        writeLines(paste("low_dbz_threshold = ", low, ";", sep=""), f)
        writeLines(paste("min_storm_size = ", size, ";", sep=""), f)
        writeLines("dual_threshold = {", f)
        writeLines(paste("   dbz_threshold = ", dual, ",", sep=""), f)
        writeLines("   min_fraction_all_parts = 0.10,", f)
        writeLines("   min_fraction_each_part = 0.005,", f)
        writeLines("   min_area_each_part = 16 };", f)
        writeLines(paste("storm_data_dir = \"./storms_", low, "_", dual, "_", size, "/\";", sep=""), f)
        close(f)
      
        system(paste("cat ", otherparams, " >> ", filename))
    } 
  }
}


