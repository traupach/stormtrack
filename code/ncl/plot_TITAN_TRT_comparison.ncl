;---------------------------------------------------------------------------------------------------
; plot_TITAN_TRT_comparison.ncl
;                                                                                                  
; Produce a raster plot for a pair of TITAN/TRT raster files. Put the plots into panels for
; comparison.
;                                                                                                  
; Author: Tim Raupach <timothy.raupach@giub.unibe.ch>                                              
; Updated: 21.12.2018                                                                              
;---------------------------------------------------------------------------------------------------

load "$NCL_LIB_PATH/plotting_functions.ncl"

begin
  ; Command line options.
  if(.not. isvar("basename"))
    print("Usage: ncl ='<basename>' plot_TITAN_TRT_comparison.ncl")
    print(" -- basename: the start of the filenames to read, expects TRT and others.")
    print("(Args should be in both single and double quotes).")
    exit
  end if

  ; List all NetCDF files in the current directory; split into TITAN and TRT files.
  trt_file = systemfunc("ls " + basename + "_TRT*.nc")
  out_file = "comparison_" + basename + ".eps"
  titan_files = systemfunc("ls " + basename + "_WRF*.nc")
  
  ; Check files exists.
  if(.not. fileexists(trt_file))
    print("TRT or TITAN file is missing for basename " + basename)
    exit
  end if
  
  ; Open reference data file.
  trt = addfile(trt_file, "r")

  ; Get TRT data.
  trt_dat = trt->value
  trt_dat@lat2d = trt->latitude
  trt_dat@lon2d = trt->longitude
  printVarSummary(trt_dat)

  titan = addfiles(titan_files, "r")
  ListSetType (titan, "join") ; Read across all files and return first index as file number.

  ; Get default map options; size the map to cover all raster points fully by
  ; using the border locations as constraints.
  map_opts = switzerland_custom_map_opts(trt->border_lat, trt->border_lon)

  ; Set up for panelled raster plot.
  cmap = read_colormap_file("precip3_16lev")         ; Read a colour set.
  map_opts@cnFillMode = "RasterFill"                 ; Make a raster plot.
  map_opts@sfXArray = trt->border_lon                ; Border coordinates for raster cells.
  map_opts@sfYArray = trt->border_lat                ; Border coordinates for raster cells.
  map_opts@gsnDraw = False                           ; Don't draw (yet).
  map_opts@gsnFrame = False                          ; Do not advance frame.
  map_opts@lbLabelBarOn = False                      ; Turn off individual legends.
  map_opts@cnLineLabelsOn = False                    ; Don't add contour labels.
  map_opts@cnInfoLabelOn = False                     ; Turn off contour labels.
  map_opts@cnFillPalette = cmap(1:16,:)              ; Set colours to use (without 0=white).
  map_opts@cnLevelSelectionMode =  "ManualLevels"    ; Use manual levels to equalise scales.
  
  ; Automatically set level min and max values using all files.
  titan_dat = titan[:]->value
  map_opts@cnMinLevelValF = min((/min(trt_dat), min(titan_dat)/))
  map_opts@cnMaxLevelValF = max((/max(trt_dat), max(titan_dat)/))
  delete(titan_dat)
  delete(titan)
  
  ; Automatically set spacing between levels.
  map_opts@cnLevelSpacingF = round((map_opts@cnMaxLevelValF - \
                                   map_opts@cnMinLevelValF) / dimsizes(cmap), 0)  

  ; Set font sizes.
  map_opts@gsnStringFontHeightF = 0.022  
  map_opts@tmXBLabelFontHeightF = 0.018
  map_opts@tmYLLabelFontHeightF = 0.018
  
  ; Output options.
  wks_type = "eps"
  wks_type@wkPaperWidthF  = 16                       ; Paper width in inches.
  wks_type@wkPaperHeightF = 8                        ; Paper height in inches.
  wks = gsn_open_wks(wks_type, out_file)             ; Open output file.

  ; Plot TRT data.
  nplots = dimsizes(titan_files) + 1
  plot = new(nplots, graphic)
  trt_dat@units = ""
  plot(0) = gsn_csm_contour_map(wks, trt_dat, map_opts)  
  
  ; Loop through TITAN files and plot them.
  i = 0
  do while(i .le. dimsizes(titan_files)-1)
    titan = addfile(titan_files(i), "r")
    
    ; Check coordinates match between the two files.
    if((.not. all(trt->latitude .eq. titan->latitude)) .or. \
       (.not. all(trt->longitude .eq. titan->longitude)) .or. \
       (.not. all(trt->border_lon .eq. titan->border_lon)) .or. \
       (.not. all(trt->border_lat .eq. titan->border_lat)))    
      print("Coordinates do not match between TITAN and TRT files.")
    end if

    ; Get TITAN data per file.
    titan_dat = titan->value
    titan_dat@lat2d = titan->latitude
    titan_dat@lon2d = titan->longitude
    titan_dat@units = ""
    printVarSummary(titan_dat)

    plot(i+1) = gsn_csm_contour_map(wks, titan_dat, map_opts)  
    i = i + 1
  end do
    
  ; Draw the panel plot.
  panel_opts = True                       
  panel_opts@gsnPanelMainString = ""                 ; No title required.
  panel_opts@gsnPanelLabelBar = True                 ; Add legend for both plots.
  panel_opts@gsnMaximize = True                      ; Minimise whitespace.
  panel_opts@gsnPaperOrientation = "portrait"        ; Orientation of the page.
  panel_opts@lbLabelFontHeightF  = 0.012             ; Reduce size of legend font.
  panel_opts@lbTitleString = "# detections"
  panel_opts@lbTitlePosition = "Right" 
  panel_opts@lbTitleFontHeightF= .012
  panel_opts@lbTitleDirection = "Across" 

  gsn_panel(wks, plot, (/2, (nplots/2)/), panel_opts); Draw the plots into the panel.
end
