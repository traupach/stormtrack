## ROI functions.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

ROITable = function(roi=ROICoords(), cap=paste("Corner point coordinates for the study",
                                               "domain. E and N are the Swiss coordinates",
                                               "(CH1903+/LV95) in the east and north",
                                               "directions respectively, while Lon and Lat",
                                               "are the corresponding longitude and latitude.",
                                               "L and R stand for left and right respectively."),
                    lab="tab:ROIInfo", size=NULL, capPlacement="top", booktabs=TRUE, placement="t") {
    ## Make a table showing the ROI information.
    ##
    ## Args:
    ##  roi: The region of interest (e.g. from ROICoords()).
    ##  cap, lab, size, capPlacement, bookTabs, placement: xtable options.
    ##
    ## Returns: void.

    roi[pt == "bottom-left", pt := "Bottom L"]
    roi[pt == "top-left", pt := "Top L"]
    roi[pt == "top-right", pt := "Top R"]
    roi[pt == "bottom-right", pt := "Bottom R"]

    names(roi) = c("Corner", "E [m]", "N [m]", "Lon [$^{\\circ}$]", "Lat [$^{\\circ}$]")
    print(xtable(roi, caption=cap, label=lab, digits=c(0, 0, 0, 0, 5, 5),
                 align=c("l", "l", "r", "r", "r", "r")),
          include.rownames=FALSE, sanitize.text.function=identity, size=size,
          caption.placement=capPlacement, booktabs=booktabs, table.placement=placement)
}   

ROICoords = function() {
    ## The region of interest over which to perform comparisons. If updated, the NCL function
    ## roi_info in plotting_functions.ncl should also be updated with new lat/long values. Note
    ## it's better to have these divide evenly if raster summaries are made.
    ##
    ## Returns: data table with lat/long and Swiss-coord (xCoord/yCoord) bounds for the ROI.

    ## Bottom-left coordinate.
    x = 2464500 
    y = 1056000 
    
    ## Width and height of ROI.
    width = 390000
    height = 260000
    
    roi = rbindlist(list(
        data.table(pt="bottom-left",  xCoord=x, yCoord=y),
        data.table(pt="top-left",     xCoord=x, yCoord=y+height),
        data.table(pt="top-right",    xCoord=x+width, yCoord=y+height),
        data.table(pt="bottom-right", xCoord=x+width, yCoord=y)),
        use.names=TRUE)

    roi[, lon := xCoord]
    roi[, lat := yCoord]
    roi = reprojectDataTable(dt=roi, coordNames=c("lon", "lat"), oldProj=projSwiss(),
                             newProj=projLatLon(), rename=FALSE)
    
    return(roi)
}

subsetToROI = function(x, roi=ROICoords()) {
    ## Subset a dataset to the region of interest for comparisons. 
    ##
    ## Args:
    ##   x: The data.table to be subset. Must contain xCoord and yCoord in Swiss
    ##      projection (projSwiss() gives CRS).
    ##   roi: The region of interest, from e.g. ROICoords().
    ##
    ## Returns: subset data.

    stopifnot(c("xCoord", "yCoord") %in% names(x))
    
    return(x[xCoord >= roi[pt=="bottom-left", xCoord] &
             xCoord <= roi[pt=="top-right", xCoord] &
             yCoord >= roi[pt=="bottom-left", yCoord] &
             yCoord <= roi[pt=="top-right", yCoord]])
}
