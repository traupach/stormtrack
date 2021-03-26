## nc_functions.R
## NetCDF helper functions.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(ncdf4)
require(data.table)

getNCField = function(nc, name, newName, dimNames=c("x", "y", "z")) {
  ## Get a field from a NetCDF file. 
  ## 
  ## Args:
  ##   nc: The ncdf4 object to read from.
  ##   name: Field name.
  ##   newName: Name to call the field in the output data.table.
  ##   dimNames: Names of dimensions, in order.
  ##
  ## Returns: data.table containing dims and variable as newName.
  
  field = ncvar_get(nc, name)
  nDims = length(dim(field))  
  stopifnot(nDims <= length(dimNames))
  
  field = data.table(melt(field))
  names(field) = c(dimNames[1:nDims], newName)
  setkeyv(field, dimNames[1:nDims])
  return(field)
}

summariseToNC = function(dt, field, xmin, xmax, ymin, ymax, res, coordCols, proj, func, outfile,
                         varname=field, varunit="", longname="") {
    ## Rasterize unevenly spaced points onto a grid and save to NetCDF.
    ##
    ## Args:
    ##   dt: data.table to work on. Coordinates with names in coordCols.
    ##   field: The column to assign to the grid.
    ##   xmin, ymin, xmax, ymax: Coordinates giving the CORNER points of the new raster (ie not the
    ##   center of the corner pixel, but the corner itself).
    ##   res: The resolution for the grid (units of x and y).
    ##   coordCols: Columns containing coordinates (default: coordX, coordY).
    ##   proj: The projection of the coordinates (default: Swiss CH1903+/LV95).
    ##   func: Function to use to combine points in the same raster cell (default: mean),
    ##         see ?rasterize.
    ##   outfile: The output filename.
    ##   description, units: metadata for the variable in the output NetCDF.
    ##
    ## Returns: void.
    
    ## Make a raster grid summarising the field.
    rast = aggregateGrid(dt=dt, fieldName=field, res=res, xmin=xmin, ymin=ymin, xmax=xmax,
                         ymax=ymax, fun=func, coordCols=coordCols)

    ## Reproject coordinates to lat/long.
    grid = rast$grid
    coordinates(grid) = ~x+y
    proj4string(grid) = proj
    grid = spTransform(grid, projLatLon())
    grid = data.table(as.data.frame(grid))

    ## Find the borders of the grid cells and reproject them.
    borders = rast$grid[, list(x,y)]

    ## Shift center points to minimums in each cell.
    borders[, x := x - res/2]
    borders[, y := y - res/2]

    ## Add maximums for the last cells in each row and column.
    xs = borders[y == max(y), x]
    ys = borders[x == max(x), y]
    maxx = borders[, max(x)]
    maxy = borders[, max(y)]
    borders = rbind(borders, borders[, list(x=xs, y=maxy+res)])
    borders = rbind(borders, borders[, list(x=maxx+res, y=ys)])
    borders = rbind(borders, borders[, list(x=maxx+res, y=maxy+res)])
    borders[, NS := -y]
    setkey(borders, NS, x)
    borders[, NS := NULL]
    
    coordinates(borders) = ~x+y
    proj4string(borders) = proj
    borders = spTransform(borders, projLatLon())
    borders = data.table(as.data.frame(borders))

    xdim = ncdim_def(name="x", vals=seq(1, rast$width), units="")
    ydim = ncdim_def(name="y", vals=seq(1, rast$height), units="")
    borderXdim = ncdim_def(name="border_x", vals=seq(1, rast$width+1), units="")
    borderYdim = ncdim_def(name="border_y", vals=seq(1, rast$height+1), units="")
    
    longitude = ncvar_def(name="longitude", units="degrees_east", dim=list(xdim, ydim))
    latitude = ncvar_def(name="latitude", units="degrees_north", dim=list(xdim, ydim))
    border_lon = ncvar_def(name="border_lon", units="degrees_east", dim=list(borderXdim, borderYdim))
    border_lat = ncvar_def(name="border_lat", units="degrees_north", dim=list(borderXdim, borderYdim))
    variable = ncvar_def(name="value", units=varunit, longname=longname,
                         dim=list(xdim, ydim), missval=-9999)
    nc = nc_create(outfile, vars=list(longitude, latitude, border_lon, border_lat, variable))
    
    ncvar_put(nc, "longitude", matrix(grid$x, nrow=rast$height, ncol=rast$width))
    ncvar_put(nc, "latitude", matrix(grid$y, nrow=rast$height, ncol=rast$width))
    ncvar_put(nc, "border_lon", matrix(borders$x, nrow=rast$height+1, ncol=rast$width+1))
    ncvar_put(nc, "border_lat", matrix(borders$y, nrow=rast$height+1, ncol=rast$width+1))

    ncvar_put(nc, "value", matrix(grid[[field]], nrow=rast$height, ncol=rast$width))
    nc_close(nc)
}




