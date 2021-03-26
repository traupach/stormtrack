## shapefile_functions.R
## Shapefile helper functions.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(raster)
require(data.table)
require(rgdal)
require(rgeos)

shapeFileToCoords = function(file, outputProj=projSwiss()) {
  ## Read a shapefile and return coordinates of lines within that file.
  ##
  ## Args:
  ##  file: The filename to open (.shp).
  ##  outputProj: The output projection (CRS) to translate to 
  ##              (default: use CH1903+/LV95 Swiss coords).
  ## 
  ## Returns: data.table containing x, y, and an id for each object.
  
  coords = NULL
  shape = shapefile(file)
  for(i in seq(1, length(shape@polygons))) {
    poly = shape@polygons[[i]]@Polygons
    for(j in seq(1, length(poly))) {
      c = data.table(coordinates(poly[[j]]))
      names(c) = c("x", "y")
      c[, id := paste(i,j)]
      coords = rbind(coords, c)
    }
  }
  
  coords = reprojectDataTable(dt=coords, coordNames=c("x","y"),
                              oldProj=shape@proj4string,
                              newProj=outputProj)
  
  return(coords)
}

writeSwissZonesShapefile = function(dir, filename="domain_def") {
  ## Write a shapefile to disk that defines zones over Switzerland and 
  ## surrounding areas. To plot the resulting file, use the NCL script
  ## plot_subdomains.ncl.
  ##
  ## Args:
  ##  dir: Directory to write in.
  ##  filename: Shapefile filename to use, without extension.
  ## 
  ## Returns: void.
  
  polygons = rbindlist(list(
    data.table(name="Jura", lat=46.390, lon=6.113), 
    data.table(name="Jura", lat=46.090, lon=5.938), # new
    data.table(name="Jura", lat=46.175, lon=5.735), # new
    data.table(name="Jura", lat=46.702, lon=5.896),
    data.table(name="Jura", lat=47.556, lon=6.893), # modified
    data.table(name="Jura", lat=47.687, lon=7.532), # modified
    data.table(name="Jura", lat=47.581, lon=7.904), # modified
    data.table(name="Jura", lat=46.390, lon=6.113),
    
    data.table(name="Alps", lat=44.742, lon=7.123),
    data.table(name="Alps", lat=44.900, lon=5.825), # new
    data.table(name="Alps", lat=46.172, lon=6.869),
    data.table(name="Alps", lat=46.739, lon=8.090), # new
    data.table(name="Alps", lat=47.235, lon=9.877), # modified
    data.table(name="Alps", lat=47.250, lon=11.300), # new
    data.table(name="Alps", lat=46.200, lon=11.000), # modified
    data.table(name="Alps", lat=45.937, lon=10.366), 
    data.table(name="Alps", lat=46.300, lon=9.303), # modified
    data.table(name="Alps", lat=46.290, lon=8.342), # modified
    data.table(name="Alps", lat=45.866, lon=7.992), 
    data.table(name="Alps", lat=45.347, lon=7.310), # modified
    data.table(name="Alps", lat=44.742, lon=7.123),
    
    data.table(name="S. Prealps", lat=44.742, lon=7.123),
    data.table(name="S. Prealps", lat=45.347, lon=7.310), # modified
    data.table(name="S. Prealps", lat=45.866, lon=7.992), 
    data.table(name="S. Prealps", lat=46.290, lon=8.342), # modified
    data.table(name="S. Prealps", lat=46.300, lon=9.303), # modified
    data.table(name="S. Prealps", lat=45.937, lon=10.366), 
    data.table(name="S. Prealps", lat=46.200, lon=11.000), # new
    data.table(name="S. Prealps", lat=45.572, lon=10.872), 
    data.table(name="S. Prealps", lat=45.648, lon=10.304), # modified
    data.table(name="S. Prealps", lat=45.802, lon=9.656),  # modified
    data.table(name="S. Prealps", lat=45.776, lon=8.500),  # modified
    data.table(name="S. Prealps", lat=45.354, lon=7.666),  # modified
    data.table(name="S. Prealps", lat=44.742, lon=7.123),
    
    data.table(name="Po Valley", lat=44.742, lon=7.123),
    data.table(name="Po Valley", lat=45.354, lon=7.666),  # modified
    data.table(name="Po Valley", lat=45.776, lon=8.500),  # modified
    data.table(name="Po Valley", lat=45.802, lon=9.656),  # modified
    data.table(name="Po Valley", lat=45.648, lon=10.304), # modified
    data.table(name="Po Valley", lat=45.572, lon=10.872),
    data.table(name="Po Valley", lat=44.9444, lon=10.111), 
    data.table(name="Po Valley", lat=44.604, lon=8.358),
    data.table(name="Po Valley", lat=44.742, lon=7.123),
    
    data.table(name="Baden-Wurt.", lat=47.560, lon=9.741), # new
    data.table(name="Baden-Wurt.", lat=47.895, lon=8.522),
    data.table(name="Baden-Wurt.", lat=47.581, lon=7.904), # new
    data.table(name="Baden-Wurt.", lat=47.687, lon=7.532), # modified
    data.table(name="Baden-Wurt.", lat=48.824, lon=8.065), 
    data.table(name="Baden-Wurt.", lat=48.431, lon=10.330), 
    data.table(name="Baden-Wurt.", lat=47.560, lon=9.741), # new
    
    data.table(name="Plateau", lat=46.390, lon=6.113),
    data.table(name="Plateau", lat=47.581, lon=7.904), # new
    data.table(name="Plateau", lat=47.895, lon=8.522),
    data.table(name="Plateau", lat=47.560, lon=9.741), # modified
    data.table(name="Plateau", lat=47.055, lon=7.931), # new
    data.table(name="Plateau", lat=46.275, lon=6.450), # modified
    data.table(name="Plateau", lat=46.090, lon=6.100), # new
    data.table(name="Plateau", lat=46.090, lon=5.938), # new
    data.table(name="Plateau", lat=46.390, lon=6.113),
    
    data.table(name="N. Prealps", lat=46.275, lon=6.450), # modified
    data.table(name="N. Prealps", lat=47.055, lon=7.931), # new
    data.table(name="N. Prealps", lat=47.560, lon=9.741), # modified
    data.table(name="N. Prealps", lat=47.235, lon=9.877), # modified
    data.table(name="N. Prealps", lat=46.739, lon=8.090), # new
    data.table(name="N. Prealps", lat=46.172, lon=6.869),
    data.table(name="N. Prealps", lat=44.900, lon=5.825), # new
    data.table(name="N. Prealps", lat=45.500, lon=5.725), # new
    data.table(name="N. Prealps", lat=46.090, lon=6.100), # new
    data.table(name="N. Prealps", lat=46.275, lon=6.450),
    
    # New region.
    data.table(name="Allgau", lat=47.235, lon=9.877),
    data.table(name="Allgau", lat=47.560, lon=9.741),
    data.table(name="Allgau", lat=48.431, lon=10.330),
    data.table(name="Allgau", lat=47.250, lon=11.300),
    data.table(name="Allgau", lat=47.235, lon=9.877)
  ))
  
  polynames = rbindlist(list(
    data.table(region="Jura", rotation=0, centroidX=6.90, centroidY=47.23),
    data.table(region="Alps", rotation=0, centroidX=8.78, centroidY=46.60),
    data.table(region="S. Prealps",  rotation=0, centroidX=8.90, centroidY=45.95),
    data.table(region="Po Valley", rotation=0, centroidX=9.07, centroidY=45.45),
    data.table(region="Baden-Wurt.", rotation=0, centroidX=9.00, centroidY=48.05),
    data.table(region="Plateau", rotation=27, centroidX=7.91, centroidY=47.26),
    data.table(region="N. Prealps", rotation=27, centroidX=8.21, centroidY=46.93),
    data.table(region="Allgau", rotation=0, centroidX=10.40, centroidY=47.65)))
  row.names(polynames) = polynames[, region]
  
  polygonList = list()
  for(reg in unique(polygons[, name])) {
    polygon = Polygon(polygons[name == reg, list(lon, lat)])
    polygon = Polygons(list(polygon), ID=reg)
    polygonList = c(polygonList, polygon)
  }
  
  polygons = SpatialPolygons(polygonList, proj4string=projLatLon())
  polygons = SpatialPolygonsDataFrame(polygons, polynames)
  writeOGR(obj=polygons, dsn=dir, layer=filename, 
           driver="ESRI Shapefile", overwrite_layer=TRUE)
}

coordinateRegion = function(x, y, proj, shapeDir="~/git/stormtrack/data/domains/",
                            shapeFile="alps_domain_def") {
    ## Find the regions for each coordinate in a list.
    ## 
    ## Args:
    ##   x, y: Coordinates to match.
    ##   proj: proj4string for the coordinates.
    ##   shapeDir, shapeFile: shapefile to read with named domain polygons.
    ##
    ## Returns: the region for each coordinate.
    
    ## Make coordinates spatial.
    coords = data.frame(x=x, y=y)
    coordinates(coords) = ~x+y
    proj4string(coords) = proj

    ## Read the polygons in the shapefile.
    polygons = readOGR(dsn=shapeDir, layer=shapeFile)

    ## Transform the points to the polygons' projection.
    coords = spTransform(coords, proj4string(polygons))
    
    ## Match coordinates to regions.
    regions = as.character(over(coords, polygons)$region)
    regions[is.na(regions)] = "Other regions"
    regions = factor(regions)
    return(regions)
}

reorderOutlinePoints = function(dat, coordCols=c("x", "y")) {
    ## Take a set of coordinates and reorder them in clockwise order around the mean point.
    ## Note this may produce weird shapes if the there are concave regions.
    ##
    ## Args:
    ##   dat: Data to reorder as a data.table.
    ##   coordCols: Columns containing x and y coordinates.
    ##
    ## Returns: reordered data.table.

    stopifnot(!any(c("angle", "xDist", "ydist") %in% names(dat)))
    
    meanPoint = colMeans(dat[, coordCols, with=FALSE])
    dat[, xDist := .SD - meanPoint[1], .SDcols=coordCols[1]]
    dat[, yDist := .SD - meanPoint[2], .SDcols=coordCols[2]]

    dat[, angle := atan2(yDist, xDist) * 180/pi]
    setkey(dat, angle)
    dat[, angle := NULL]
    dat[, xDist := NULL]
    dat[, yDist := NULL]
    
    return(dat)
}

writeCellsShapefile = function(cells, outfile, outdir, cellProj, proj=projLatLon()) {
    ## Write a shapefile with TRT cell outlines for each trackID and timestamp.
    ##
    ## Args:
    ##   cells: Cells information, must contain timestamp, trackID, x, y, runDx, runDy,
    ##          and must be regularly gridded.
    ##   outfile: Output file name.
    ##   outdir: Output directory.
    ##   cellProj: Projection of input cell coordinates (default: CH1903 for TRT).
    ##   proj: Desired output projection (default: lat/long).
    ##
    ## Returns: void.

    stopifnot(c("timestamp", "trackID", "x", "y") %in% names(cells))

    cells = copy(cells)
    cells[, timestamp := as.numeric(timestamp)]
    cells[, identifier := paste(timestamp, trackID, sep="")]

    ## Cells are identified by the timestamp and the track ID.
    ## Make a lookup table to give each identifier a unique number.
    ids = data.table(id=cells[, unique(identifier)])
    ids[, cellID := seq(1, nrow(ids))]
    setkey(ids, id)

    ## Assign each coordinate to its cell ID.
    setkey(cells, identifier)
    cells[, cellID := ids[cells, cellID]]
    
    ## Set up polygon properties.
    polyprops = unique(cells[, c("cellID", "timestamp", "trackID")])
    row.names(polyprops) = polyprops[, cellID]

    cellPoints = unique(cells[, list(x, y)])
    coordinates(cellPoints) = ~x+y
    proj4string(cellPoints) = cellProj
    gridded(cellPoints) = TRUE
    blankRaster = raster(cellPoints)

    cl = makeCluster(detectCores())
    registerDoParallel(cl)   
    polygons = foreach(ts=unique(cells$timestamp),
                       .combine=rbind,
                       .packages=c("sp", "raster", "data.table")) %dopar%
        {
            tsCells = cells[timestamp == ts, list(cellID, x, y)]
            coordinates(tsCells) = ~x+y
            proj4string(tsCells) = cellProj
            cellsRaster = rasterize(tsCells, blankRaster)[[2]]
            
            polygon = rasterToPolygons(cellsRaster, dissolve=TRUE)
            polygon = spChFIDs(polygon, as.character(polygon$cellID))
            polygon
        }
    stopCluster(cl)
    registerDoSEQ()

    ## Convert to output projection coordinates.
    polygons = spTransform(polygons, proj)
    
    ## Add cell properties to the polygon data.
    polygons = SpatialPolygonsDataFrame(polygons, polyprops)
    
    ## Write the polygons as a shapefile.
    writeOGR(obj=polygons, dsn=outdir, layer=outfile,
             driver="ESRI Shapefile", overwrite_layer=TRUE)
}
