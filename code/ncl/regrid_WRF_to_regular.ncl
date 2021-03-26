;---------------------------------------------------------------------------------------------------
; regrid_WRF_to_regular.ncl
;
; Regrids selected variables from WRF data into a regular 3D grid. Converts all wrf/wrfout* in the
; current directory and writes regridded NetCDF files as the same file names with "_regridded.nc"
; added to their names, in the directory wrf_regridded. Files with existing output are skipped.
; Height levels to use are set below.
;                                                                                                  
; Author: Tim Raupach <timothy.raupach@giub.unibe.ch>                                              
; Updated: 31.08.2018                                                                              
;---------------------------------------------------------------------------------------------------

load "$NCL_LIB_PATH/WRF_regridding_functions.ncl"

begin
  ; Set height levels to interpolate to, in km.
  height_levels = fspan(1, 15, 29)  ; 0-15 km by 500 m.
  print("Interpolating to the following height levels [km]:")
  print(height_levels)

  ; Read files in "wrf" directory, remove directory name.
  files = systemfunc("ls wrf/wrfout* | sed 's.^wrf/..'")

  if(any(ismissing(files)))
    print(files)
    print("ERROR: No wrfout* files found in wrf directory.")
    exit
  end if
  
  ; Make a directory for the output.
  if(.not. fileexists("wrf_regridded"))
    system("mkdir wrf_regridded")
  end if

  ; Make a tmp directory.
  if(.not. fileexists("tmp"))
    system("mkdir tmp")
  end if

  ; Ensure tmp files don't exist.
  if(fileexists("tmp/tmp_source_grid_desc.nc") .or. \
     fileexists("tmp/tmp_dest_grid_desc.nc") .or. \
     fileexists("tmp/tmp_interp_weights.nc"))
    print("WARNING: Temporary file(s) already exist(s)!")
  end if

  i = 0
  do while(i .lt. dimsizes(files))
    srcFile = "wrf/" + files(i)
    dstFile = "wrf_regridded/" + files(i) + "_regridded.nc"

    if(fileexists(dstFile))
      print("Skipping file, output exists: " + srcFile)
    end if

    if(.not. fileexists(dstFile))
      print("Regridding file: " + srcFile)
      regrid_WRF_file(srcFile, dstFile, (/"REFL_10CM"/), (/False/), height_levels)
    end if
    i = i + 1
  end do

  ; This script generates a PET0 file; move it to the log directory.
  if(fileexists("log"))
    PETfiles = systemfunc("ls PET*")
    if(.not. any(ismissing(PETfiles)))
      system("mv PET* log/")
    end if
  end if
end

  
