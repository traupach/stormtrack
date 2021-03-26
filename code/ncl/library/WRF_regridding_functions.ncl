;---------------------------------------------------------------------------------------------------
; WRF_regridding_functions.ncl                                                                     
;                                                                                                  
; Functions for regridding WRF output to regular 3D grids.                                         
;                                                                                                  
; Author: Tim Raupach <timothy.raupach@giub.unibe.ch>                                              
; Updated: 29.01.2019                                                                              
;---------------------------------------------------------------------------------------------------

load "/usr/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
load "/usr/lib/ncarg/nclscripts/contrib/cd_inv_string.ncl"

;--- regrid_WRF_horizontal ------------------------------------------------------------------------
; Regrid a WRF grid in curvilinear coordinates to a rectilinear grid over the same area. The grid
; can be 2D or 3D. The regridding is performed using bilinear interpolation.
;
; Args:
;   nc: The WRF netCDF object.
;   dat: The data to regrid, can be 2D or 3D.   
; 
; Returns: The regridded data.
;---------------------------------------------------------------------------------------------------
function regrid_WRF_rectilinear(nc:file, dat)
local source_grid_file, dest_grid_file, weights_file, lat, lon, scrip_opt, dims, newlat, newlon, \
      weights_opt, interp
begin
  ; Filenames to use for the temporary files.
  source_grid_file = "tmp/tmp_source_grid_desc.nc"     ; File for source grid definition.
  dest_grid_file = "tmp/tmp_dest_grid_desc.nc"         ; File for dest grid definition.
  weights_file = "tmp/tmp_interp_weights.nc"           ; File for interpolation weights.

  ; For the moment, do not allow any missing values in the source field. Weights need to be
  ; determined for each field that contains missing values, but can be reused if no missing values
  ; exist.
  if(any(ismissing(dat))) then
    print("ERROR: regrid_WRF_rectilinear: missing values are not currently allowed.")
    exit
  end if
  
  ; Only need to generate weights once per grid definition. If weight files already exist, use them.
  if(.not.fileexists(source_grid_file) .or. \
     .not.fileexists(dest_grid_file) .or. \
     .not.fileexists(weights_file)) then

    ; Describe source grid.
    lat = wrf_user_getvar(nc, "lat", 0) ; Latitude [deg N].
    lon = wrf_user_getvar(nc, "lon", 0) ; Longitude [deg N].

    ; Describe the source grid and save to a SCRIP file. 
    scrip_opt = True                             
    scrip_opt@Overwrite = True ; Prompt for overwriting files.
    curvilinear_to_SCRIP(source_grid_file, lat, lon, scrip_opt)

    ; Make arrays to describe the new rectilinear grid -- evenly space the same number of points
    ; between minimum and maximum lat/long values. Save the destination grid to a SCRIP file.
    dims = dimsizes(lat)

    newlat = fspan(min(lat), max(lat), dims(0)) ; dims(0) is number of rows.
    newlon = fspan(min(lon), max(lon), dims(1)) ; dims(1) is number of cols.
    rectilinear_to_SCRIP(dest_grid_file, newlat, newlon, scrip_opt)

    ; Generate interpolation weights.
    weights_opt = True                    ; Options for interpolation.
    weights_opt@InterpMethod = "bilinear" ; Use standard bilinear interpolation.
    weights_opt@SrcRegional = True        ; Source grid is regional not global.
    weights_opt@DstRegional = True        ; Dest grid is regional not global.
    weights_opt@Overwrite = True          ; Prompt for overwrite if file exists.
    ESMF_regrid_gen_weights(source_grid_file, dest_grid_file, weights_file, weights_opt)
  end if

  ; Run the interpolation using the (pre)generated weights.
  interp = ESMF_regrid_with_weights(dat, weights_file, True)
  return(interp)
end

;--- regrid_WRF_to_heights -------------------------------------------------------------------------
; Regrid 3D WRF data to regular heights above sea level. 
;
; Args:
;   nc: The open WRF netcdf file; used to get WRF properties.
;   dat: The extracted data from the WRF file.
;   new_levels: The new height levels in km above mean sea level.
;
; Returns: The regridded data.
;---------------------------------------------------------------------------------------------------
function regrid_WRF_ASL(nc:file, dat, new_levels)
local interp
begin
  interp = wrf_user_vert_interp(nc, dat, "ght_msl", new_levels, True)
  interp&interp_levels@units = "km asl"
  return(interp)
end

;--- regrid_WRF_to_regular -------------------------------------------------------------------------
; Regrid 2D or 3D WRF data to a regular horizontal and vertical grid. No transformation is
; made to the variable (so Z in dBZ is averaged in dBZ).
;
; Args:
;   nc: The open WRF netcdf file; used to get WRF properties.
;   variable: The variable to extract and regrid.
;   new_levels: The new height levels in km above mean sea level (ignored if data is 2D).
;   wrf_var: If True, use "wrf_user_getvar" to get the variable. If False, get the variable directly
;           from the NetCDF file.
;
; Returns: The regridded data.
;---------------------------------------------------------------------------------------------------
function regrid_WRF_to_regular(nc:file, variable:string, new_levels:numeric, wrf_var:logical)  
local dat, dat_rectilinear, dat_rect_asl
begin
  if(wrf_var) then 
    dat = wrf_user_getvar(nc, variable, -1)
  else
    dat = nc->$variable$
  end if

  print("Before regridding, " + variable + " ranges from " + min(dat) + " to " + max(dat) + " " + \
        dat@units + ".")
  print("Regridding variable " + variable)
  
  ; Horizontal then vertical. This order is because vertical interpolation introduces NA values. To
  ; avoid making weights files every time, no NA values can be allowed in the horizontal
  ; interpolation.
  dat_rectilinear = regrid_WRF_rectilinear(nc, dat)

  ; Only perform vertical interpolation if data is 3D.
  if(dimsizes(dimsizes(dat)) .eq. 4) then
    dat_rect_asl = regrid_WRF_ASL(nc, dat_rectilinear, new_levels)
  else
    if(dimsizes(dimsizes(dat)) .eq. 3) then
      print("Data is 2D, not performing vertical interpolation.")
      dat_rect_asl = dat_rectilinear
    else
      print("ERROR: regrid_WRF_regular: No. of dims. must be either 4 (time+3D) or 3 (time+2D).")
      exit
    end if
  end if

  print("After regridding, " + variable + " ranges from " + min(dat_rect_asl) + " to " + \
        max(dat_rect_asl) + " " + dat_rect_asl@units + ".")
  
  return(dat_rect_asl)
end

;--- regrid_WRF_file -------------------------------------------------------------------------------
; For a single WRF netcdf file, regrid to a regular grid and output a new NetCDF file.
;
; Args:
;   srcFile: The source file.
;   dstFile: The destination file to write.
;   variables: A list of variables to regrid.
;   wrf_var: An array with an entry for each variable: if True, use "wrf_user_getvar" to get the
;           variable. If False, get the variable directly from the NetCDF file.
;   new_levels: The new height levels in km above mean sea level (ignored if data is 2D).
;---------------------------------------------------------------------------------------------------
procedure regrid_WRF_file(srcFile:string, dstFile:string, variables[*]:string, \
                          wrf_var[*]:logical, new_levels[*]:numeric)
local nc, out, timestr, year, month, day, hour, minute, second, time, i, regridded
begin
  ; Open the source file.
  print("----------------------------------")
  print("Performing interpolation for file: " + srcFile)
  nc = addfile(srcFile, "r")

  ; Open the destination file. Set it to use compression.
  setfileoption("nc", "Format", "NetCDF4Classic")
  out = addfile(dstFile, "c")
  setfileoption(out, "CompressionLevel", 5)
  if(ismissing(out)) then
    print("ERROR: regrid_WRF_file: could not create output file; does it already exist?")
    exit
  end if

  ; Write file time (note assumes there is only one time per file).
  timestr = oneDtostring(nc->Times(0,:))
  timestr = str_sub_str(timestr, ",", "")
  
  year = stringtointeger(str_get_cols(timestr, 0, 3))
  month = stringtointeger(str_get_cols(timestr, 5, 6))
  day = stringtointeger(str_get_cols(timestr, 8, 9))
  hour = stringtointeger(str_get_cols(timestr, 11, 12))
  minute = stringtointeger(str_get_cols(timestr, 14, 15))
  second = stringtointeger(str_get_cols(timestr, 17, 18))
  
  time = cd_inv_calendar(year, month, day, hour, minute, second, \
                         "seconds after 1970-01-01 00:00:00", 0) 
  out->time = time

  ; Loop through the variables, regrid, and write output.
  i = 0
  do while(i .lt. dimsizes(variables))
    regridded = regrid_WRF_to_regular(nc, variables(i), new_levels, wrf_var(i))
    out->$variables(i)$ = regridded
    delete(regridded)
    i = i + 1
  end do
end
