;---------------------------------------------------------------------------------------------------
; plot_WRF_terrain_with_radars.ncl
;                                                                                                  
; Plot the terrain map of the study region, showing areas simulated by WRF and covered by
; MeteoSwiss radars.
;                                                                                                  
; Author: Tim Raupach <timothy.raupach@giub.unibe.ch>                                              
;---------------------------------------------------------------------------------------------------

load "$NCL_LIB_PATH/plotting_functions.ncl"

begin
  ; Command line arguments.
  if(.not. isvar("wrfFile") .or. .not. isvar("outName"))
    print("Usage: ncl wrfFile='<file>' outName='<name>' plot_WRF_terrain_with_radars.ncl")
    exit
  end if
   
  ; Open example WRF file and get terrain information.
  nc = addfile(wrfFile, "r")                             
  lats = nc->XLAT(0,:,:)                                 
  lons = nc->XLONG(0,:,:)                                
  alts = nc->HGT
  
  plot_terrain(alts, lats, lons, outName, True, True, False, True)
end
