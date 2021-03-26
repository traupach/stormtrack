;---------------------------------------------------------------------------------------------------
; plot_domains.ncl
;                                                                                                  
; Plot the different study domains over Switzerland. Requires a WRF file passed as an argument.
;                                                                                                  
; Author: Tim Raupach <timothy.raupach@giub.unibe.ch>                                              
; Updated: 27.11.2018                                                                              
;---------------------------------------------------------------------------------------------------

load "$NCL_LIB_PATH/plotting_functions.ncl"

begin
  ; Command line arguments.
  if(.not. isvar("wrfFile"))
    print("Usage: ncl wrfFile='<file>' plot_WRF_terrain_with_radars.ncl")
    exit
  end if

  ; Open example WRF file and get terrain information.
  nc = addfile(wrfFile, "r")                             
  lats = nc->XLAT(0,:,:)                                 
  lons = nc->XLONG(0,:,:)                                
  alts = nc->HGT                                         
  
  plot_terrain(alts, lats, lons, "subdomains", True, False, True, True)
end
