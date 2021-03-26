## raster_functions.R
## Raster helper functions.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(raster)
require(data.table)
require(rgdal)
require(rgeos)
require(sp)

cellToRaster = function(cell, proj) {
    ## Convert cell points to a raster object.
    ##
    ## Args:
    ##   cell: data.table, must contain x, y, and coordinates must be regularly spaced.
    ##   proj: The projection for x and y coordinates.
    ##
    ## Returns: a raster object for the cell.

    stopifnot(c("x","y") %in% names(cell))
    
    ## Set projection.
    coordinates(cell) = ~x+y
    proj4string(cell) = proj
    
    ## Set gridded to TRUE, and print any warning except a warning about empty
    ## columns/rows.
    cell = tryCatch({
        gridded(cell) <- TRUE
        cell },
        warning = function(w) {
            if(!str_detect(as.character(w), "grid has empty column/rows"))
                warning(as.character(w))
            suppressWarnings(gridded(cell) <- TRUE)
            return(cell)
        })
    stopifnot(gridded(cell) == TRUE)
    
    ## Convert the cell to a raster.
    cell = raster(cell)
    return(cell)
}
