## spatial_functions.R
## Helper functions for geospatial operations.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

reprojectDataTable = function(dt, coordNames, oldProj, newProj, rename=FALSE) {
  ## Reproject coordinates in a data table.
  ## 
  ## Args:
  ##  dt: The data table to reproject.
  ##  coordNames: x and y coordinate column names (in that order).
  ##  oldProj: Current projection of dt.
  ##  newProj: New projection required.
  ##  rename: Rename coord columns to coordX and coordY? (Default: FALSE).
  ## 
  ## Returns: data.table with coordinates reprojected.
  
  if("optional" %in% names(dt)) {
    stop("reprojectDataTable: can not have 'optional' as an input column name.")
  }
  
  coordinates(dt) = coordNames
  proj4string(dt) = oldProj
  dt = spTransform(dt, newProj)
  dt = data.table(data.frame(dt))
  
  if(rename) {
    setnames(dt, coordNames[1], "coordX")
    setnames(dt, coordNames[2], "coordY")
  }
  
  dt[, optional := NULL]
  return(dt)
}

aggregateGrid = function(dt, fieldName, res, xmin, ymin, xmax, ymax, fun=mean,
                         coordCols=c("xCoord", "yCoord"), proj=projSwiss()) {
    ## Rasterize unevenly spaced points onto a grid.
    ##
    ## Args:
    ##   dt: data.table to work on. Coordinates with names in coordCols.
    ##   xmin, ymin, xmax, ymax: Coordinates giving the CORNER points of the new raster (ie not the
    ##   center of the corner pixel, but the corner itself).
    ##   fieldName: The column to assign to the grid.
    ##   res: The resolution for the grid (units of x and y).
    ##   fun: Function to use to combine points in the same raster cell (default: mean).
    ##   coordCols: Columns containing coordinates (default: coordX, coordY).
    ##   proj: The projection of the coordinates (default: Swiss CH1903+/LV95).
    ## 
    ## Returns: a list containing:
    ##    grid: a data.table of x and y coordinates (cell centres) and aggregate values per grid
    ##          point for the field.
    ##    width, height: the number of coordinates in x and y directions.
    
    stopifnot(c(fieldName, coordCols) %in% names(dt))
    if(length(res) == 1) {
        res = c(res, res)
    }
    
    stopifnot(ymin < ymax)
    stopifnot(xmin < xmax)
    
    ## Make a new raster object with the right properties.
    rast = raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, crs=proj, resolution=c(res,res))
    
    ## Make the data table spatial.
    coordinates(dt) = coordCols
    proj4string(dt) = proj
    
    ## Project onto the raster and make a data.table of results.
    rast = rasterize(dt, rast, field=fieldName, fun=fun)
    result = data.table(coordinates(rast), as.vector(rast))
    names(result) = c("x", "y", fieldName)
    
    ## Convert back to data.table.  
    return(list(grid=result, width=dim(rast)[2], height=dim(rast)[1]))
}

