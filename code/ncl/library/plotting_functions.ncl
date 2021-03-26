;---------------------------------------------------------------------------------------------------
; plotting_functions.ncl
;                                                                                                  
; Generic helper functions for NCL plots.
;                                                                                                  
; Author: Tim Raupach <timothy.raupach@giub.unibe.ch>                                              
;---------------------------------------------------------------------------------------------------

;--- MCH_radars ------------------------------------------------------------------------------------
; Return the locations of Meteoswiss radars in lat/long coordinates.
;---------------------------------------------------------------------------------------------------
function MCH_radars()
local radars
begin
  radars = True

  ; If updating these details, remember to also update in R code in
  ; MCHRadarLocations() in R/library/TRT_functions.R.
  radars@name = (/   "Dole",  "Morte",  "Albis",   "Lema",  "Weissfluh" /)
  radars@lon  = (/  6.09941,  7.48655,  8.51200,  8.83322,      9.79446 /) 
  radars@lat  = (/ 46.42511, 46.37065, 47.28433, 46.04076,     46.83497 /)
  radars@maxDist = 245.9                          ; Maximum observed radar distance [km, Euclidean].
  return(radars)
end

;--- CH_domains ------------------------------------------------------------------------------------
; Return a shapefile object that describes the domain regions over Switzerland.
; Expects a file "domain_def.shp" in the current directory. Use the R function
; writeSwissZonesShapefile() to write this file.
;---------------------------------------------------------------------------------------------------
function CH_domains()
local sp
begin
  sp = addfile("domain_def.shp", "r")
  return(sp)
end

;--- basic_map_opts --------------------------------------------------------------------------------
; Make a generic plot options object for a basic contour plot.
; 
; Returns: plot options object.
;---------------------------------------------------------------------------------------------------
function basic_map_opts()
local opts
begin
  opts = True                                     ; Plot options object.
  opts@cnFillOn = True                            ; Use filled contours.
  opts@cnLinesOn = False                          ; Turn off contour lines.
  opts@gsnAddCyclic = False                       ; Don't join first and last longitudes (regional).
  opts@gsnMaximize = True                         ; Use up as much space with the plot as possible.
  opts@mpDataBaseVersion = "MediumRes"            ; Use medium-resolution geophysical outlines.
  opts@mpDataSetName = "Earth..4"                 ; Use a high-quality geophysical outline dataset.
  opts@mpOutlineBoundarySets = "AllBoundaries"    ; Show all boundaries on the map.
  opts@mpNationalLineThicknessF = 3               ; Thickness of national border lines.
  opts@mpGeophysicalLineThicknessF = 3            ; Thickness of geophysical lines.
  opts@pmTickMarkDisplayMode = "Always"           ; Always display tick marks.
  opts@mpFillOn = False                           ; Don't fill continents with grey.
  return(opts)
end

;--- stereographic_map_opts ------------------------------------------------------------------------
; Make a generic plot options object for a stereographic regional map plot.
; 
; Args:
;    lats: 2D array of coordinate latitudes.
;    lons: 2D array of coordinate longitudes.
;
; Returns: plot options object.
;---------------------------------------------------------------------------------------------------
function stereographic_map_opts(lats, lons)
local opts
begin
  opts = basic_map_opts()                         ; Start with basic options.
  opts@mpProjection = "Stereographic"             ; Use a stereographic projection.
  opts@mpLimitMode = "LatLon"                     ; Limit to min/max coordinates.
  opts@mpMinLatF = min(lats)                      ; Minimum latitude (zoom).
  opts@mpMaxLatF = max(lats)                      ; Maximum latitude (zoom).
  opts@mpMinLonF = min(lons)                      ; Minimum longitude (zoom).
  opts@mpMaxLonF = max(lons)                      ; Maximum longitude (zoom).
  opts@mpCenterLonF = (max(lons) + min(lons)) / 2 ; Centre longitude.
  opts@mpCenterLatF = (max(lats) + min(lats)) / 2 ; Centre latitude.
  return(opts)
end

;--- switzerland_map_opts --------------------------------------------------------------------------
; Make a generic plot options object for a stereographic regional map plot of Switzerland.
; 
; Returns: plot options object.
;---------------------------------------------------------------------------------------------------
function switzerland_map_opts()
local opts
begin
  opts = basic_map_opts()                         ; Start with basic options.
  opts@mpProjection = "Stereographic"             ; Use a stereographic projection.
  opts@mpLimitMode = "Corners"                    ; Limit to corner coordinates.
  opts@mpLeftCornerLatF = 48.4                    ; Top left latitude (zoom).
  opts@mpLeftCornerLonF = 4.9                     ; Top left longitude (zoom).
  opts@mpRightCornerLatF = 44.95                  ; Bottom right latitude (zoom).
  opts@mpRightCornerLonF = 11.3                   ; Bottom right longitude (zoom).
  
  ; The centre latitude and longitude are Zimmerwald Observatory, which is the fundamental point
  ; for the Swiss Grid CH1903+ projection. Here shown in latitude/longitude.
  opts@mpCenterLonF = 7.46500                     ; Centre latitude.
  opts@mpCenterLatF = 46.87667                    ; Centre longitude.
  return(opts)
end

;--- roi_info --------------------------------------------------------------------------------------
; Make a generic plot options object for a stereographic regional map plot of Switzerland.
; 
; Returns: plot options object.
;---------------------------------------------------------------------------------------------------
function roi_info()
local opts
begin
  opts = True

  opts@bottomLeftLon = 5.70093                    ; Bottom right longitude (zoom).
  opts@bottomLeftLat = 45.64222                   ; Bottom right latitude (zoom).
  opts@topLeftLon = 5.62372                       ; Top left longitude (zoom).
  opts@topLeftLat = 47.98025                      ; Top left latitude (zoom).
  opts@topRightLon = 10.84586                     ; Top left longitude (zoom).
  opts@topRightLat = 47.94466                     ; Top left latitude (zoom).
  opts@bottomRightLon = 10.70117                  ; Bottom right longitude (zoom).
  opts@bottomRightLat = 45.60812                  ; Bottom right latitude (zoom).
  
  return(opts)
end

;--- switzerland_custom_map_opts -------------------------------------------------------------------
; Make a generic plot options object for a stereographic regional map plot of Switzerland, with
; custom extents.
;
; Args:
;    lats: Latitudes to cover.
;    lons: Longitudes to cover.
;
; Returns: plot options object.
;---------------------------------------------------------------------------------------------------
function switzerland_custom_map_opts(lats, lons)
local opts, lons1D, lats1D, minLatIdx, maxLatIdx
begin
  opts = switzerland_map_opts()                   ; Start with Swiss options.
  opts@mpLimitMode = "Corners"                    ; Limit to min/max coordinates.

  ; Due to the projection, the corner points are not the min/max latitude and longitude (for
  ; example, the top right corner has a greater longitude than the bottom right corder). So to find
  ; the correct corner values, condition the points on where the lon/lat is also minimum.
  lons1D = ndtooned(lons)
  lats1D = ndtooned(lats)
  minLatIdx = ind(lats1D .eq. min(lats))
  minLonIdx = ind(lons1D .eq. min(lons))
  
  opts@mpLeftCornerLatF = max(lats1D(minLonIdx))  ; Top-left corner lat (lon is also min).
  opts@mpLeftCornerLonF = min(lons)               ; Top-left corner long (normal min).
  opts@mpRightCornerLatF = min(lats)              ; Bottom-right corner lat (normal min).
  opts@mpRightCornerLonF = max(lons1D(minLatIdx)) ; Bottom-right corner lon (lat is also min).
  return(opts)
end

;--- map_plot --------------------------------------------------------------------------------------
; Make a map contour plot from data with custom description/units/options.
;
; Args:
;    dat: The data to plot. 
;    lats: 2D array of latitude coordinates.
;    longs: 2D array of longitude coordinates.
;    filename: Name of the file to output (no extension).
;    description: Data description / plot title.
;    units: Data units.
;    options: Options array for the plot.
;---------------------------------------------------------------------------------------------------
procedure map_plot(dat, lats, lons, filename, description, units, options)
local wks_type, wks, plot
begin
  wks_type = "eps"                                ; Output type.
  wks = gsn_open_wks(wks_type, filename)

  ; Assign coordinate arrays, description, and units to the variable.
  dat@lat2d = lats                                
  dat@lon2d = lons
  dat@description = description                   
  dat@units = units

  options@gsnMaximize = True
  options@gsnPaperOrientation = "portrait"
  
  ; Make the plot.
  plot = gsn_csm_contour_map(wks, dat(0,:,:), options)
  delete(wks)
end

;--- order_clockwise -------------------------------------------------------------------------------
; Sort coordinates into clockwise order, so the resulting points are ready to plot as a polygon.
; 
; Args:
;    lats: Latitudes of points to sort.
;    lons: Longitudes of points to sort.
;
; Returns: an object containing 'lats' and 'lons' in order.
;---------------------------------------------------------------------------------------------------
function order_clockwise(lats, lons)
local centre_x, centre_y, x, y, angles, order, points
begin
  centre_x = avg(lons)                             ; Find a centroid point inside the polygon.
  centre_y = avg(lats)                  

  x = lons - centre_x                              ; Calculate angle between centre point and 
  y = lats - centre_y                              ; each coordinate point.
  angles = atan2(y, x)
  
  order = dim_pqsort(angles, 1)                    ; Sort in angle order.

  points = True
  points@lats = lats(order)
  points@lons = lons(order)
  return(points)
end

;--- add_bbox_to_plot -------------------------------------------------------------------------------
; Add a bounding box region to a plot.
; 
; Args:
;    wks: The workstation to add to.
;    plot: The plot to add to.
;    bbox: An object specifying the bounding box to plot with bottomLeftLat/Lon, topLeftLat/Lon,
;          topRightLat/Lon, bottomRightLat/Lon.
;    colour: Line colour.
;
; Returns: Rendered radar point markers and polyline showing their outlines.
;---------------------------------------------------------------------------------------------------
function add_bbox_to_plot(wks, plot, bbox, colour)
local xpts, ypts, line_opts, outline
begin
  ; Retrieve the bounding points of the box.
  xpts = (/ bbox@topLeftLon, bbox@topRightLon, bbox@bottomRightLon, \
           bbox@bottomLeftLon, bbox@topLeftLon /)
  ypts = (/ bbox@topLeftLat, bbox@topRightLat, bbox@bottomRightLat, \
           bbox@bottomLeftLat, bbox@topLeftLat /)
  
  ; Draw the box to the plot.
  line_opts = True
  line_opts@gsLineDashPattern = 1
  line_opts@gsLineThicknessF = 3
  line_opts@gsLineColor = colour
  outline = gsn_add_polyline(wks, plot, xpts, ypts, line_opts)
  
  ; Return the rendered object.
  return(outline)
end

;--- add_radars_to_plot ----------------------------------------------------------------------------
; Add radar points and an outline of their coverage regions to an existing plot.
; 
; Args:
;    wks: The workstation to add to.
;    plot: The plot to add to.
;    radar_points: The radar points to add, see MCH_radars() for an example.
;    color: The colour to plot in.
;
; Returns: Rendered radar point markers and polyline showing their outlines.
;---------------------------------------------------------------------------------------------------
function add_radars_to_plot(wks, plot, radar_points, colour)
local marker_opts, rendered_radars, circles, all_circle_lats, all_circle_lons, num_points, \
num_radars, dists_from_radar, i, j, dist, radar_edge_index, ordered_points, circle_opts, \
rendered_circles, return_objects
begin 
  ; Plot the radar centres.
  marker_opts = True
  marker_opts@gsMarkerIndex = 14
  marker_opts@gsMarkerThicknessF = 2
  marker_opts@gsMarkerSizeF = 0.016
  marker_opts@gsMarkerColor = colour
  rendered_radars = gsn_add_polymarker(wks, plot, radar_points@lon, radar_points@lat, marker_opts)

  ; Generate circle points around the radar centres.
  circles = geolocation_circle(radar_points@lat, radar_points@lon, \ ; Circle centre point.
                               radar_points@maxDist, \               ; Radius.
                               1, 180, False)                        ; Unit (km), numpts, opt.

  all_circle_lats = ndtooned(circles[0](:,0,:))
  all_circle_lons = ndtooned(circles[1](:,0,:))
  num_points = dimsizes(all_circle_lats)
  num_radars = dimsizes(radar_points@lat)
  dists_from_radar = new(num_points, "float")
  dists_from_radar = -1
  
  ; Loop through all circle points and calculate the distance to the closest radar point.
  do i=0,num_points-1
    ; Test distance of point i from centre of radar j.
    do j=0,num_radars-1
      dist = gc_latlon(all_circle_lats(i), all_circle_lons(i), radar_points@lat(j), \
                       radar_points@lon(j), 2, 4)

      if(dists_from_radar(i) .eq. -1 .or. \
         dist .lt. dists_from_radar(i))
        dists_from_radar(i) = dist
      end if
    end do
  end do

  ; Use the great-circle max distance to test for radius points.
  dists_from_radar = floor(dists_from_radar)
  radar_edge_index = ind(dists_from_radar .ge. max(dists_from_radar))

  ; Order the points in clockwise order for proper polygon rendering.
  ordered_points = order_clockwise(all_circle_lats(radar_edge_index), \
                                   all_circle_lons(radar_edge_index))

  ; Make the last point the same as the first to close the loop.
  ordered_points@lats := array_append_record(ordered_points@lats, ordered_points@lats(0), 0)
  ordered_points@lons := array_append_record(ordered_points@lons, ordered_points@lons(0), 0)
  
  ; Render the polygon of the radar outline.
  circle_opts = True
  circle_opts@gsLineThicknessF = 2.5
  circle_opts@gsLineColor = colour
  rendered_circles = gsn_add_polyline(wks, plot, ordered_points@lons, ordered_points@lats, \
                                      circle_opts)

  ; Make a return object. This is required so the polylines and markers are not destroyed when the
  ; function returns.
  return_objects = True
  return_objects@points = rendered_radars
  return_objects@outlines = rendered_circles

  return(return_objects)
end

;--- add_polygons_to_plot ----------------------------------------------------------------------------
; Add polygons to a plot.
; 
; Args:
;    wks: The workstation to add to.
;    plot: The plot to add to.
;    sp: A shapefile opened with addfile, defining domain regions as polygons.
;    colour: Line colour.
;    opts: Options (can include text_rotations, feature_text, text_coord_x, text_coord_y,
;          all per feature).
;
; Returns: Rendered polygons showing the domain outlines.
;---------------------------------------------------------------------------------------------------
function add_polygons_to_plot(wks, plot, sp, colour, opts)
local segments, geometries, num_features, idx_segment_start, idx_num_segments, \
idx_point_start, idx_num_points, i, s, segment_start, num_segments, points_start, points_end, \
polyline, num_segments, segment_num
begin
  segments = sp->segments                         ; Read line segments.
  geometries = sp->geometry                       ; Read geometries (features). 
  num_features = dimsizes(geometries)             ; Number of features.
  num_features := num_features(0)
  num_segments = dimsizes(segments)               ; Number of segments.
  num_segments := num_segments(0)

  idx_segment_start = sp@geom_segIndex            ; Index in geometry of segment index.
  idx_num_segments  = sp@geom_numSegs             ; Index in geometry of number of segments.
  idx_point_start = sp@segs_xyzIndex              ; Index in segments of start coordinate.
  idx_num_points  = sp@segs_numPnts               ; Index in segments of number of coordinates.
  
  lons = sp->x                                    ; Longitudes for segments. 
  lats = sp->y                                    ; Latitudes for segments.

  drawn_objects = new(num_segments*2, graphic)    ; List to hold drawn lines and labels.
  
  poly_opts = True                                ; Options for polygon drawing.
  poly_opts@gsLineThicknessF = 3                  ; Line thickness. 
  poly_opts@gsLineColor = colour                  ; Line colour.

  text_opts = True
  text_opts@txFontHeightF = 0.021
  text_opts@txFontColor = colour
  
  drawn_num = 0                                   ; Loop through objects and plot each one.
  do i=0, num_features-1

    ; Select only features set to True in opts@featureSwitch.
    if(isatt(opts, "featureSwitch"))
      if(.not. opts@featureSwitch(i))
        continue
      end if
    end if
    
    segment_start = geometries(i, idx_segment_start)
    num_segments = geometries(i, idx_num_segments)

    do s=segment_start, segment_start+num_segments-1
      points_start = segments(s, idx_point_start)
      points_end = points_start + segments(s, idx_num_points) - 1

      drawn_objects(drawn_num) = gsn_add_polyline(wks, plot, lons(points_start:points_end), \ 
                                                    lats(points_start:points_end), poly_opts)
      drawn_num = drawn_num+1
      
      if(isatt(opts, "add_text"))
        if(opts@add_text .eq. True)
          text_opts@txAngleF = opts@text_rotations(i)
          drawn_objects(drawn_num) = gsn_add_text(wks, plot, opts@feature_text(i), \
                                                  opts@text_coord_x(i), opts@text_coord_y(i), \
                                                  text_opts)
          drawn_num = drawn_num+1
        end if
      end if
    end do
  end do

  return(drawn_objects)
end

;--- add_domains_to_plot ----------------------------------------------------------------------------
; Add Swiss domain regions to a plot and crop the plot to the domain region. 
; 
; Args:
;    wks: The workstation to add to.
;    plot: The plot to add to.
;    sp: A shapefile opened with addfile, defining domain regions as polygons.
;    colour: Line colour.
;
; Returns: Rendered polygons showing the domain outlines.
;---------------------------------------------------------------------------------------------------
function add_domains_to_plot(wks, plot, sp, colour)
begin
  opts = True
  opts@text_rotations = sp->rotation
  opts@feature_text = sp->region
  opts@text_coord_x = sp->centroidX
  opts@text_coord_y = sp->centroidY
  opts@add_text = True
  return(add_polygons_to_plot(wks, plot, sp, colour, opts))
end

;--- plot_terrain ----------------------------------------------------------------------------------
; Plot the terrain for a domain.
; 
; Args:
;    heights: array of terrain heights to plot, in m ASL.
;    lats: 2D array of coordinate latitudes.
;    lons: 2D array of coordinate longitudes.
;    outname: The name of the output file to create.
;    include_roi: If True, include the region of interest from roi_domain().
;    include_radars: If True, also plot the radar coverage for Switzerland defined by MCH_radars().
;    include_domains: If True, also plot the subdomains over Switzerland using MCH_domains().
;    centre_on_ch: If True, centre the projection on Switzerland.
;---------------------------------------------------------------------------------------------------
procedure plot_terrain(heights, lats, lons, outname, include_roi, include_radars,
  include_domains, centre_on_ch)
local terrain_opts, cmap, num_colours, terrain_opts, terr, terr_wks, terrain_plot, box, radars
begin
  ; Stereographic map options.
  terrain_opts = stereographic_map_opts(lats, lons)

  if(centre_on_ch .eq. True)
    ; Center on the WRF domain center for Alps modelling.
    terrain_opts@mpCenterLonF = 8        ; Centre latitude.
    terrain_opts@mpCenterLatF = 46.8     ; Centre longitude.
  end if
 
  ; Colours to use for the terrain. Remove the last five colours to avoid plotting the highest
  ; points in white, which is also used as the colour for missing regions.
  cmap = read_colormap_file("OceanLakeLandSnow")
  num_colours = dimsizes(cmap)
  num_colours := num_colours(0) - 5
  cmap := cmap(0:num_colours,:)
  
  ; Force sea-level (height zero) to appear as blue; then scale other colours across the altitude
  ; range. Use 15 colour levels for the non-sea terrain.
  terrain_opts@cnLevelSelectionMode = "ExplicitLevels"
  terrain_opts@cnFillPalette = cmap
  terrain_opts@cnLevels = array_append_record((/ 0.001 /), tofloat(floattoint( \
                                               fspan(250, 4000, 16))), 1)

  if(max(heights) .gt. 4000)
    print("WARNING: heights greater than 4000 (max plotted height) are found.")
  end if

  ; Do not immediately plot, in case other features should be added to the map before display.
  terrain_opts@gsnFrame = False 
  terrain_opts@gsnPaperOrientation = "portrait"
  terrain_opts@lbTitleString = "m"
  terrain_opts@lbTitlePosition = "Right" 
  terrain_opts@lbTitleFontHeightF= .014
  terrain_opts@lbTitleDirection = "Across" 

  ; Set up the terrain height data to plot.
  terr = heights
  terr@lat2d = lats
  terr@lon2d = lons
  terr@description = "Terrain height"
  terr@units = ""

  ; If domains are included, crop the plot to the domain coverage area, and don't show the label
  ; bar.
  if(include_domains .eq. True)
    sp = CH_domains()
    terrain_opts@mpMinLatF := min(sp->y) - 0.1    ; Minimum latitude (zoom).
    terrain_opts@mpMaxLatF := max(sp->y) + 0.1    ; Maximum latitude (zoom).
    terrain_opts@mpMinLonF := min(sp->x) - 0.6    ; Minimum longitude (zoom).
    terrain_opts@mpMaxLonF := max(sp->x) + 0.05   ; Maximum longitude (zoom).
    terrain_opts@lbLabelBarOn = False             ; Don't show label bar.
    terrain_opts@cnLineLabelsOn = False           ; Don't add contour labels.
    terr@description = ""                         ; No title text.
    terr@units = ""                               ; No title units.
  end if
  
  ; Open a workspace, add the plot to it, and return the workspace object.
  wks_type = "eps"
  terr_wks = gsn_open_wks(wks_type, outname)
  terrain_plot = gsn_csm_contour_map(terr_wks, terr(0,:,:), terrain_opts)

  ; Optionally, include the region of interest.
  if(include_roi .eq. True)
    roi = add_bbox_to_plot(terr_wks, terrain_plot, roi_info(), "blue")
  end if

  ; Optionally, plot circles showing the area covered by Meteoswiss' radars.
  if(include_radars .eq. True)
    radars = add_radars_to_plot(terr_wks, terrain_plot, MCH_radars(), "blue")
  end if

  ; Optionally, plot the outlines and names of study domains.
  if(include_domains .eq. True)
    domains = add_domains_to_plot(terr_wks, terrain_plot, CH_domains(), "blue")
  end if

  ; Render the plot.
  draw(terr_wks)
end

