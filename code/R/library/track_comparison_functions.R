require(tables)
require(xtable)
require(gridExtra)
require(viridis)
require(data.table)

comparisonDataSummaryTable = function(dat, refSet, reg="All",
                                      caption=paste("Summary information for each tracking",
                                                    "technique, showing the number of individual",
                                                    "detections, number of tracks, and first and",
                                                    "last cell record times."),
                                      label="tab:dataSummary", placement="t", booktabs=TRUE,
                                      captionPlacement="top", simNameColName="scheme", ...) {
    ## Print a data summary table for tracker data.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   refSet: The name of the reference value of "set" in 'dat'.
    ##   reg: The region to compare ("All" by default).
    ##   caption, label, placement, booktabs: xtable options.
    ##   captionPlacement: Where to put the caption? (default: "top").
    ##   simNameColName: column name that gives simulation name.
    ##   ...: Extra options to print.xtable().
    ##
    ## Results: the summary data.

    stopifnot(c("region", "set") %in% names(dat))
    stopifnot(reg %in% dat[, region])

    dataSummary = dat[region == reg, list(totalCells=length(trackID),
                                          totalTracks=length(unique(trackID)),
                                          minTime=strftime(min(timestamp), tz="UTC",
                                                           format="%Y-%m-%d %H:%M"),
                                          maxTime=strftime(max(timestamp), tz="UTC",
                                                           format="%Y-%m-%d %H:%M")),
                      by=c("set", simNameColName)]
    
    names(dataSummary) = c("Method", "Scheme", "Num. cells", "Num. tracks", 
                           "First cell (UTC)", "Last cell (UTC)") 

    print(xtable(dataSummary[, c("Method", "Num. cells", "Num. tracks", 
                                 "First cell (UTC)", "Last cell (UTC)"), with=FALSE],
                 caption=caption, label=label), booktabs=booktabs,
          include.rownames=FALSE, table.placement=placement,
          caption.placement=captionPlacement, ...)
    
    compCells = NA
    compTracks = NA
    if(!is.na(refSet)) {
        testSets = dat[set != refSet, unique(set)]
    
        compCells = ""
        compTracks = ""

        if(length(testSets) == 1) {
                compCells = compNumbers(test=dataSummary[Method==testSets, "Num. cells", with=FALSE],
                                        than=dataSummary[Method==refSet, "Num. cells", with=FALSE],
                                        what=" individual cells")
                compTracks = compNumbers(test=dataSummary[Method==testSets,
                                                          "Num. tracks", with=FALSE],
                                         than=dataSummary[Method==refSet, "Num. tracks", with=FALSE],
                                         what=" tracks")
        } else {
            for(s in testSets) {
                compCells = paste(compCells,
                                  compNumbers(test=dataSummary[Method==s, "Num. cells", with=FALSE],
                                              than=dataSummary[Method==refSet, "Num. cells",
                                                               with=FALSE],
                                              what=" individual cells"),
                                  "for the", dataSummary[Method==s, Scheme], simNameColName)
                
                compTracks = paste(compTracks,
                                   compNumbers(test=dataSummary[Method==s, "Num. tracks", with=FALSE],
                                               than=dataSummary[Method==refSet, "Num. tracks",
                                                                with=FALSE],
                                               what=" tracks"),
                                   "for the", dataSummary[Method==s, Scheme], simNameColName)
                
                if(s != testSets[length(testSets)] &&
                   s != testSets[length(testSets)-1]) {
                    compCells = paste(compCells, ", ", sep="")
                    compTracks = paste(compTracks, ", ", sep="")
                } else if (s == testSets[length(testSets)-1]) {
                    compCells = paste(compCells, "and ")
                    compTracks = paste(compTracks, "and ")
                }
            }
        }
    }
    
    return(list(summary=dataSummary, compCells=compCells, compTracks=compTracks))
}

plotCellDetectionCompGGplot = function(dat, reg="All") {
    ## Prepare a map of density of cell detections by cell trackers, using ggplot.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   reg: The region to compare ("All" by default).
    ##
    ## Returns: ggplot object.

    stopifnot(c("region", "set") %in% names(dat))
    stopifnot(reg %in% dat[, region])
    
    plot = ggplot(dat[region == reg], aes(x=xCoord, y=yCoord)) + geom_bin2d(bins=45, colour=NA) + 
        geom_point(data=MCHRadarLocations(), aes(x=x, y=y),
                   colour="black", size=2, shape=2, stroke=0.75) +
        facet_wrap(~set) + labs(x="Swiss coord. X [m]", y="Swiss coord Y [m]") +
        scale_fill_viridis(name="# pts")

    return(plot)
}

rasterComparison = function(dat, res, filename, field="trackID", func="count",
                            subdir="nc", unit="# detections",
                            ncComparisonScript=paste("~/git/stormtrack/code/ncl/",
                                                     "plot_TITAN_TRT_comparison.ncl",
                                                     sep=""),
                            label=filename, caption=filename,
                            roi=ROICoords(), ...) {
    ## Summarise tracker data to .nc files for comparison.
    ## 
    ## Args:
    ##   dat: The data table of cell information to plot.
    ##   res: The resolution to use [m].
    ##   filename: The output filename to use. 
    ##   func: The function to summarize with, per raster grid cell.
    ##   subdir: A subdirectory to use for NC files and plot files.
    ##   unit: The resulting unit.
    ##   ncComparisonScript: The NCL script to produce the comparison plots.
    ##   label, caption: label and caption for the figure.
    ##   roi: The region of interest to use; if not specified using min/max x/y in data.
    ##   ...: Options passed to runNCL().
    ## 
    ## Returns: void.
    
    stopifnot(c("trackID", "x", "y", "set") %in% names(dat))
    
    if(!file.exists(subdir))
        dir.create(subdir)
    
    ## Set up raster options.
    if(is.null(roi)) {
        xmin = dat[, (min(x) %/% res) * res]
        ymin = dat[, (min(y) %/% res) * res]
        xmax = dat[, (max(x) %/% res) * res + res]
        ymax = dat[, (max(y) %/% res) * res + res]
    } else {
        xmin = roi[pt == "bottom-left", xCoord]
        xmax = roi[pt == "bottom-right", xCoord]
        ymin = roi[pt == "bottom-left", yCoord]
        ymax = roi[pt == "top-left", yCoord]
        stopifnot(!is.na(c(xmin, xmax, ymin, ymax)))
        stopifnot(!any(dat$x <= xmin))
        stopifnot(!any(dat$x >= xmax))
        stopifnot(!any(dat$y <= ymin))
        stopifnot(!any(dat$y >= ymax))
    }

    ## Summarise the information to raster NC files, labelled in alphabetical order.
    sets = sort(dat[, unique(set)])
    for(i in seq(1, length(sets))) {
    	s = sets[i]
        file = paste(subdir, "/", filename, "_", s, ".nc", sep="")
        
        summariseToNC(dt=dat[set==s], field=field, xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, 
                      res=res, coordCols=c("x", "y"), proj=projSwiss(), func=func, outfile=file, 
                      longname=paste("(", letters[i], ") ", s, sep=""), varunit=unit)
    }
        
    ## Plot the comparison between raster files.
    setNames = dat[set != "TRT (Observations)", unique(set)]
    args = paste("basename='\"", filename, "\"' ", ncComparisonScript, sep="")
    runNCL(args=args, dir=subdir, figname=paste("comparison_", filename, sep=""),
           label=label, caption=caption, ...)
}

plotTrackPointsComp = function(dat, trackPicker, name, reg="All", subdir="nc",
                               rasterRes=10000, ...) {
    ## For tracker points by set, pick out tracks using a function 'trackPicker',
    ## eg. earliestRecord(), plot their density with NCL, and write the latex display
    ## code.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   trackPicker: Function to select tracks to plot from each trackID.
    ##   name: NC files name.
    ##   reg: The region to compare ("All" by default).
    ##   subDir: Directory in which to put nc files and output plots (default: "nc").
    ##   caption, label: Latex label and caption (default: name).
    ##   rasterRes: The resolution to use in the raster output, m [default: 10000 m].
    ##   ...: Extra arguments to rasterComparison().
    ##
    ## Returns: void.
    
    stopifnot(c("region", "set") %in% names(dat))
    stopifnot(reg %in% dat[, region])
    
    points = dat[region == reg, trackPicker(.SD), by=c("set", "trackID")]
    setnames(points, c("xCoord", "yCoord"), c("x", "y"))
    rasterComparison(dat=points, res=rasterRes, filename=name, subdir=subdir, ...)
}

plotCellDetectionComp = function(dat, ...) {
    ## Plot a map of density of cell detection locations by tracker, using NCL.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   ...: Optional extra arguments to plotTrackPointsComp().
    ##
    ## Returns: void.
    
    plotTrackPointsComp(dat=dat, trackPicker=identity, name="allCellDetections", ...)
}

plotFirstDetectionComp = function(dat, ...) {
    ## Plot a map of density of first cell detection locations by tracker, using NCL.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   ...: Optional extra arguments to plotTrackPointsComp().
    ##
    ## Returns: void.

    earliestRecord = function(x) {
        ## Return the record with the earliest timestamp.
        return(x[which.min(timestamp)])
    }
    
    plotTrackPointsComp(dat=dat, trackPicker=earliestRecord, name="startingCoords", ...)
}

plotMaxZSpatialComp = function(dat, ...) {
    ## Plot a map of density of first cell detection locations by tracker, using NCL.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   ...: Optional extra arguments to plotTrackPointsComp().
    ##
    ## Returns: void.

    maxReflectivityRecord = function(x) {
        ## Return the record with the maximum maxZ value.
        return(x[which.max(maxZ)])
    }
    
    plotTrackPointsComp(dat=dat, trackPicker=maxReflectivityRecord, name="maxZCoords", ...)
}

plotRegions = function(dat) {
    ## Prepare a plot of the regions (except "All").
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##
    ## Returns: ggplot object.
    
    plot = ggplot(dat[region != "All"], aes(x=xCoord, y=yCoord)) +
        geom_point(aes(colour=region)) +
        geom_point(data=MCHRadarLocations(), aes(x=x, y=y),
                   colour="black", size=2, shape=2, stroke=0.75) +
        scale_colour_discrete(name="Region") + facet_wrap(~set) +
        labs(x="Swiss coord. X [m]", y="Swiss coord Y [m]")

    return(plot)
}

plotCellNumsByRegion = function(dat) {
    ## Prepare a bar plot of cells detected per region.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##
    ## Returns: ggplot object.

    plotdat = dat[, list(num=length(trackID)), by=c("set","region")]
    
    plot = ggplot(plotdat, aes(x=factor(region), y=num)) + 
        geom_bar(aes(group=set, fill=set), stat="identity", position="dodge") + 
        labs(x="Region", y="# detections") +
        theme(axis.text.x=element_text(angle=30, hjust=1)) +
        scale_fill_viridis(name="", discrete=TRUE)
    
    return(plot)
}

plotCellNumsByDay = function(dat, lineSize=0.7) {
    ## Prepare comparisons of cell numbers by day by region.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   lineSize: Percentage line line size (default: 0.7).
    ##
    ## Returns: ggplot object.

    cellsPerDay = dat[, list(cells=length(unique(trackID))), by=c("set", "day","region")]
    
    plot = ggplot(cellsPerDay, aes(x=day, y=cells)) +
        geom_line(aes(group=set, colour=set), size=lineSize) +
        scale_color_viridis(name="", discrete=TRUE) +
        facet_wrap(~region, scales="free_y", ncol=2, labeller=label_alpha) +
        labs(x="Day in 2018", y="Number of cells") +
	theme(legend.position="bottom") +
	guides(colour=guide_legend(nrow=2))

    return(plot)
}

plotCellsPerTimestep = function(dat, reg="All", lineSize=0.7) {
    ## Prepare comparisons of cell numbers by individual time step.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   lineSize: Percentage line line size (default: 0.7).
    ##
    ## Returns: ggplot object.

    cellsPerTime = dat[region == reg, list(cells=length(unique(trackID))), by=c("set", "timestamp")]
    plot = ggplot(cellsPerTime, aes(x=timestamp, y=cells, colour=set)) +
        geom_point() +
        geom_line(size=lineSize) +
        scale_color_viridis(name="", discrete=TRUE) +
        labs(x="Time stamp UTC", y="Number of cells detected")

    return(plot)
}

plotCellNumsByHourOfDay = function(dat, perc=TRUE, barLineSize=0.2, lineSize=0.7, alpha=0.6, ncol=2) {
    ## Prepare comparisons of cell numbers by hour of the day by region.
    ##
    ## Args:
    ##   dat: track/cell with "set" values to compare.
    ##   perc: If TRUE, show percentages of the total number of cells per region and set, otherwise
    ##         show raw numbers (default: TRUE).
    ##   barLineSize: Bar chart line size (default: 0.2).
    ##   lineSize: Percentage line line size (default: 0.7).
    ##   ncol: Number of columns for facets.
    ##
    ## Returns: ggplot object.
    
    cellsPerHour = dat[, list(cells=length(unique(trackID))), by=c("set", "hour", "region")]    
    cellsPerHour[, perc := cells / sum(cells) * 100, by=c("region", "set")]

    if(!perc) {
        plot = ggplot(cellsPerHour, aes(x=hour, y=cells)) +
            geom_bar(aes(fill=set, group=set), stat="identity",
                     position=position_dodge(width=0.5), 
                     width=1, colour="black", size=barLineSize, alpha=alpha) +
            labs(x="Hour of day (UTC)", y="Num. cells") +
            scale_fill_viridis(name="", discrete=TRUE)
    } else {
        plot = ggplot(cellsPerHour, aes(x=hour, y=perc)) +
            geom_line(aes(colour=set, group=set), size=lineSize) +
            geom_point(aes(colour=set, group=set), size=2*lineSize) +
            labs(x="Hour of day (UTC)", y="Cell percentage") +
            scale_color_viridis(name="", discrete=TRUE)
    }

    plot = plot + facet_wrap(~region, scales="free_y", ncol=ncol, labeller=label_alpha) +
        scale_x_continuous(breaks=seq(0, 22, by=2)) +
        theme(legend.position="bottom") +
	guides(color=guide_legend(nrow=2))

    return(plot)
}

perHourMinMaxTable = function(dat, caption=paste("Hours of minimum and maximum cell",
                                                 "numbers, by region."),
                              label="tab:minMaxByHour") {
    ## Print a table showing the hour of minimum/maximum cell numbers for each set.
    ## 
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   caption, label: Table arguments.
    ## 
    ## Returns: void.

    maxMinByHour = function(x) {
        data.table(maxHour=x[which.max(cells), hour],
                   minHour=x[which.min(cells), hour])
    }

    cellsPerHour = dat[, list(cells=length(unique(trackID))), by=c("set", "hour", "region")]
    cellsMinMax = cellsPerHour[, maxMinByHour(.SD), by=c("region", "set")]
    
    tab = tabular(Heading("Region")*region~
                      Heading("Max. hour")*identity*Heading()*maxHour*Heading()*Factor(set)+
                      Heading("Min. hour")*identity*Heading()*minHour*Heading()*Factor(set),
                  data=cellsMinMax)
    printTable(tab, cap=caption, lab=label)
}

plotDurationComp = function(dat, density=FALSE, maxDuration=120, durationBinWidth=5,
                            lineSize=0.2, alpha=0.6) {
    ## Perform comparisons of cell numbers by hour of the day by region.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   density: If FALSE, plot a normal histogram. If TRUE, plot a density so bars integrate to 1.
    ##   maxDuration: Maximum storm duration to show in duration histogram (default: 120 m).
    ##   durationBinWidth: Bin width for duration histogram [m] (default: 5).
    ##   lineSize: Bar chart line size (default: 0.2).
    ##   alpha: Alpha value (default: 0.5).
    ##
    ## Returns: a list with the plot and the percentages NOT included in the plot for each set. 

    durations = dat[, list(duration=unique(duration)), by=c("set", "region", "trackID")]
    percs = durations[region == "All",
                      list(percNotShown=round(mean(duration > maxDuration)*100, 1)), by=set]

    if(!density) {
        plot = ggplot(durations[duration <= maxDuration], aes(x=duration)) + 
            scale_fill_viridis(name="", discrete=TRUE) +
            labs(x="Duration [mins]", y="Num. cells")
    } else {
        plot = ggplot(durations[duration <= maxDuration], aes(x=duration, y=..density..)) + 
            scale_fill_viridis(name="", discrete=TRUE) +
            labs(x="Duration [mins]", y="Density") 
    }

    plot = plot +
        geom_histogram(aes(fill=set), position=position_dodge(width=durationBinWidth*0.5),
                       alpha=alpha, binwidth=durationBinWidth, boundary=2.5, size=lineSize,
                       colour="black") + 
        scale_x_continuous(breaks=seq(durationBinWidth, maxDuration, by=durationBinWidth*2)) +
        facet_wrap(~region, ncol=2, scales="free_y", labeller=label_alpha) +
	theme(legend.position="bottom")

    return(list(plot=plot, percsNotShown=percs, maxDuration=maxDuration))
}

plotDensityComp = function(dat, var, varName, varUnit, reg="All", lineSize=0.2, densityAlpha=0.6,
                           fillName="", fillBy="set", facetBy=NA, logX=FALSE, legend.pos="right") {
    ## Prepare a comparison of distributions of a variable from each set.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   var: The name of the variable to compare.
    ##   varName: Display name of variable (expression).
    ##   varUnits: Units of variable (expression).
    ##   lineSize: Density plot line size (default: 0.2).
    ##   densityAlpha: Alpha value for density fill (default: 0.5).
    ##   fillName: Name for fill scale.
    ##   fillBy: Value to fill by (default: "set").
    ##   facetBy: What to facet the plots by (default: NA, none).
    ##   logX: Use a log scale on the X axis? (default: FALSE).
    ##   legend.pos: Legend position (default: right).
    ##
    ## Returns: ggplot object.

    plot = ggplot(dat, aes_string(x=var)) +
        geom_density(aes_string(fill=fillBy), colour="black", size=lineSize, alpha=densityAlpha) + 
        scale_fill_viridis(name=fillName, discrete=TRUE) +
        labs(x=parse(text=paste(varName, "~group('[',", varUnit, ",']')", sep="")),
             y="Density") +
        theme(legend.position=legend.pos)

    if(logX)
        plot = plot + scale_x_log10()

    if(!is.na(facetBy))
        plot = plot + facet_wrap(as.formula(paste("~", facetBy, sep="")), scales="free_y", ncol=2)
    
    return(plot)
}

plotMaxZDensityComp = function(dat, reg="All", ...) {
    ## Plot a comparison of distributions of maximum Z from each method.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   reg: Region to compare (default: "All").
    ##   ...: Extra args to plotDensityComp().
    ##
    ## Returns: void.

    plotDensityComp(dat=dat[region == reg], var="maxZ",
                    varName="Cell~maximum~Z", varUnit="dBZ")
}

plotMeanZDensityComp = function(dat, reg="All", ...) {
    ## Plot a comparison of distributions of maximum Z from each method.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   reg: The region to compare ("All" by default).
    ##   ...: Extra args to plotDensityComp().
    ##
    ## Returns: void.

    plotDensityComp(dat=dat[region == reg], var="meanZ", varName="Cell~mean~Z", varUnit="dBZ", ...)
}

plotCellAreaComp = function(dat, ...) {
    ## Plot a comparison of cell areas by region.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   ...: Extra args to plotDensityComp().
    ##
    ## Returns: void.
    
    plotDensityComp(dat=dat, var="area", varName="Cell~area", varUnit="km^2", logX=TRUE,
                    legend.pos="bottom", facetBy="region", ...)
}

plotCellVelocityComp = function(dat, ...) {
    ## Plot a comparison of cell velocities by region.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   ...: Extra args to plotDensityComp().
    ##
    ## Returns: void.

    if(any(is.na(dat$speed)))
        warning("Some velocities are NaN or NA; removing them.")

    plotDensityComp(dat=dat[!is.na(speed)], var="speed", varName="Cell~velocity",
                    varUnit="km~h^{-1}", facetBy="region", legend.pos="bottom")
}

plotCellVelocityDensityComp = function(dat, ...) {
    ## Plot a comparison of cell velocities by region.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   lineSize: Density plot line size (default: 0.2).
    ##   densityAlpha: Alpha value for density fill (default: 0.5).
    ##
    ## Returns: void.

    if(any(is.na(dat$speed)))
        warning("Some velocities are NaN or NA; removing them.")

    plotDensityComp(dat=dat[!is.na(speed)], var="speed", varName="Cell~velocity",
                    varUnit="km~h^{-1}", fillBy="region", legend.pos="bottom", facetBy="set")
}

plotCellDirectionPercComp = function(dat, lineSize=0.3, densityAlpha=1, ncol=5,
                                     tableCaption="Mean advection directions by region.",
                                     tableLabel="tab:meanDirs", showTable=TRUE, byRegion=TRUE,
                                     colourEnd=1, ...) {
    ## Plot rose plots for cell directions by region, coloured by method;
    ## plots show percentage of cells in each direction.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   lineSize: Density plot line size (default: 0.4).
    ##   densityAlpha: Alpha value for density fill (default: 0.5).
    ##   ncol: Number of facet cols (default: 3).
    ##   tableCaption, tableLabel: Table properties.
    ##   showTable: Show the table of mean directions by region? (Default: TRUE).
    ##   byRegion: Facet by region? (Default: TRUE).
    ##   colourEnd: The "last" colour to use in the viridis scheme.
    ##   ...: Extra aguments to printTable.
    ##
    ## Returns: plot, and table of mean angles by region.
     
    cellDirs = dat[speed > 0, angleNames(direction, includeSub=FALSE), by=c("set", "region")]
    numPerDirection = cellDirs[, list(n=length(angle)), by=c("region","set","angleName")]
    numPerRegion = cellDirs[, list(total=length(angle)), by=c("region", "set")]
    
    setkey(numPerDirection, region, set)
    setkey(numPerRegion, region, set)
    cellPercs = numPerDirection[numPerRegion]
    cellPercs[, perc := n / total * 100]

    plot = ggplot(cellPercs, aes(x=angleName)) +
        geom_bar(stat="identity", aes(y=perc, fill=set), colour="black", size=lineSize, 
                 width=1, alpha=densityAlpha, position=position_dodge(width=0.33)) +
        coord_polar(start=-22.5 / 180 * pi) +
        scale_fill_viridis(name="", discrete=TRUE, end=colourEnd) +
        scale_colour_viridis(name="", discrete=TRUE, end=colourEnd) +
        labs(y="Percentage of cells", x="Advection direction") +
        theme(legend.position="bottom",
              axis.text.x=element_text(size=6))

    if(byRegion) {
        plot = plot + facet_wrap(~region, ncol=ncol, labeller=label_alpha)
        meanAngles = cellDirs[, list(angle=round(meanAngle(angle), 0)), by=c("set", "region")]
        tab = tabular(Heading("Region")*region~Heading("Mean angle (degrees)")*
                          identity*Heading()*angle*Heading()*Factor(set),
                      data=meanAngles)
    } else {
        meanAngles = cellDirs[, list(angle=round(meanAngle(angle), 0)), by=c("set")]
        tab = tabular(Heading()*Factor(set)~Heading("Mean angle (degrees)")*identity*Heading()*angle,
                      data=meanAngles)
    }
    
    if(showTable)
        printTable(tab, cap=tableCaption, lab=tableLabel, ...)

    return(list(plot=plot, angles=meanAngles))
}

plotCellLifecycles = function(dat, reg="All", lineSize=0.2, alpha=0.6) {
    ## Plot cell lifecycles.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   reg: Region to use (default: "All").
    ##   lineSize: Density plot line size (default: 0.2).
    ##
    ## Returns: void.
    
    lifeCycles = dat[region == reg, relativeIntensity(.SD), by=c("set", "region")]

    return(ggplot(lifeCycles, aes(x=factor(perc), y=med)) + 
           geom_boxplot(aes(ymin=q10, ymax=q90, middle=med, fill=set,
                            lower=q25, upper=q75), stat="identity", position=position_dodge(0.5), 
                        size=lineSize) +
           labs(x="Proportion of track time", y="Proportion of intensity range") +
           scale_fill_viridis(name="", discrete=TRUE, alpha=alpha) +
           scale_x_discrete(breaks=seq(0, 1, by=0.1)) +
           theme(legend.position="bottom") +
	   guides(fill=guide_legend(nrow=2)))
}

plotCellDevel = function(dat, reg="All", lineWidth=0.75, var="area", varName="Cell~area",
                         varUnits="km^2", minTracks=10) {
    ## Compare the development of cells through time, showing time since track start and the q25/q75
    ## range of values for each time.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   reg: The region to show (default: All).
    ##   lineWidth: Width of the main (q50) line.
    ##   var: The variable to plot -- q50 with IQR will be shown.
    ##   varName, varUnit: Display name and unit for the variable.
    ##   minTracks: only show results for time periods with at least this many tracks (default: 10).
    ##
    ## Returns: the plot, and the required number of tracks per time period for it to be plotted.
    
    ## Pull together intensity information for each track and set.
    dat = copy(dat[region == reg])
    
    ## Determine the time since the start of the track for each time step.
    dat[, trackLength := as.integer(difftime(timestamp, min(timestamp), units="mins")), 
        by=c("set", "trackID")]
    
    ## Compile statistics on the selected variable, for each time span.
    dat[, var := .SD, .SDcols=var]
    
    stats = dat[, list(
        q10=quantile(var, probs=0.10),
        q25=quantile(var, probs=0.25),
        q50=median(var),
        q75=quantile(var, probs=0.75),
        q90=quantile(var, probs=0.90),
        mean=mean(var),
        numTracks=length(var)), by=c("set", "trackLength")]

    stats = stats[numTracks >= minTracks]
    
    plot = ggplot(stats, aes(x=trackLength, y=q50)) + 
        geom_line(aes(colour=set), size=lineWidth) +
        geom_point(aes(colour=set))  +
        geom_ribbon(aes(ymin=q25, ymax=q75, fill=set), alpha=0.35) +
        scale_fill_viridis(name="", discrete=TRUE) +
        scale_colour_viridis(name="", discrete=TRUE) +
        labs(x="Time since track start [mins]",
             y=parse(text=paste(varName, "~group('[',", varUnits, ",']')", sep=""))) +
        theme(legend.position="bottom") +
	guides(colour=guide_legend(nrow=2), fill=guide_legend(nrow=2))

    return(list(plot=plot, minTracks=minTracks))
}

cellNumbersComparison = function(dat, reg="All", lineSize=0.7) {
    ## Prepare a plot of two simple comparisons of tracks/detections by hour.
    ##
    ## Args:
    ##   dat: track/cell data with "set" values to compare.
    ##   reg: The region to compare (default: All).
    ##   lineSize: Bar chart line size (default: 0.2).
    ##
    ## Returns: void.

    dat = dat[region == reg]

    ## Tracks and detections per hour.
    tracks = dat[, list(cells=length(unique(trackID)), name="Tracks by hour"), by=c("set", "hour")]
    detections = dat[, list(cells=length(trackID), name="Detections by hour"), by=c("set", "hour")]

    toPlot = rbind(tracks, detections)
    plot = ggplot(toPlot, aes(x=hour, y=cells)) + geom_line(aes(colour=set)) +
        geom_point(aes(colour=set)) +
        scale_colour_viridis(name="", discrete=TRUE) +
        labs(x="Hour of day (UTC)", y="Number") +
        facet_wrap(~name, ncol=2, scales="free")
    
    return(plot)
}

compareDistributions = function(dat, refSet, cols=c("area", "speed", "duration"),
                                names=c("Area", "Velocity", "Duration"),
                                units=c("km^2", "km~h^{-1}", "mins"), n=50, ...) {
    ## Produce QQ plots for distributions, with defaults for area, speed, duration.
    ## See notes for ggplotQQ for details.
    
    return(propertyQQ(dat=dat, cols=cols, names=names, units=units, refSet=refSet, n=n, ...))
}

propertyQQ = function(dat, cols, refSet, names=cols, units="",  by="region", byName="Region",
                      n=100, shape=19, size=2, lineSize=0.7, logScale=FALSE,
                      onePerTrack=c("duration"), otherName="TITAN") {
    ## Make a QQ plot with ggplot.
    ##
    ## Args:
    ##   dat: The data to plot; must contain 'set'.
    ##   refSet: The reference set name.
    ##   cols: The columns to compare.
    ##   names, units: Names and units for the columns (variable).
    ##   by: Colour by which variable?
    ##   byName: Scale name for colour.
    ##   n: Number of percentiles to plot (default: 100).
    ##   shape, size: Point parameters for geom_point.
    ##   lineSize: line size parameter.
    ##   logScale: Plot on log10 scales? (Default: FALSE).
    ##   onePerTrack: Variables for which only one value should be counted per track.
    ##   otherName: The name for the y axis in the plots.
    ##
    ## Returns: plot object.
    
    stopifnot(c("trackID", "set") %in% names(dat))
    dat = dat[, c("set", "trackID", by, cols), with=FALSE]

    stopifnot(!("axis" %in% names(dat)))

    otherSets = dat[set != refSet, unique(set)]
    dat[set == refSet, axis := "ref"]
    dat[set != refSet, axis := "other"]

    ## Deal with variables for which only one value should be counted per track.
    for(var in onePerTrack) {
        if(var %in% cols) {
            replaceWithUnique = function(x) {
                val = unique(x)
                stopifnot(length(val) == 1)
                res = rep(NA, length(x))
                res[1] = val
                return(res)
            }
            
            stopifnot(!("tomodify" %in% names(dat)))
            setnames(dat, var, "tomodify")
            dat[, (var) := replaceWithUnique(tomodify), by=c("set", "trackID")]
            setnames(dat, "tomodify", var)
        }
    }
    
    ## Remove track ID.
    dat[, trackID := NULL]

    ## Warn about NAs in variable columns.
    for(var in cols) {
        if(!(var %in% onePerTrack))
            if(any(is.na(dat[, var, with=FALSE])))
                warning(paste("propertyQQ: NAs ignored in variable", var))
    }

    getQuants = function(x, p=seq(0, 1, length.out=n)) {
        return(data.table(perc=p, quant=quantile(x, probs=p, na.rm=TRUE)))
    }

    allQuants = NULL
    for(other in otherSets) {
        quants = melt(dat[set %in% c(refSet, other)], id.vars=c("set", "axis", by))
        quants = quants[!is.na(value), getQuants(value), by=c("set", "axis", by, "variable")]
        quants[, compName := other]
        
        if(!is.null(by))
            quants =
                dcast(quants, paste("variable+perc+compName+", by, "~axis", sep=""),
                      value.var="quant")
        else
            quants = dcast(quants, "variable+perc+compName~axis", value.var="quant")
        
        for(i in seq(1, length(cols))) {
            quants[variable == cols[i],
                   variable := paste(names[i], "~group('[',", units[i], ",']')", sep="")]
        }

        allQuants = rbind(allQuants, quants)
    }
       
    allQuants[, compName := str_replace(string=compName, pattern=" ", replacement="~")]

    perc_5 = rbind(dat[set == refSet, lapply(.SD, quantile, probs=0.05, na.rm=TRUE), .SDcols=cols])
    perc_95 = rbind(dat[set == refSet, lapply(.SD, quantile, probs=0.95, na.rm=TRUE), .SDcols=cols])
    perc_5 = data.table(p=5, perc_5)
    perc_95 = data.table(p=95, perc_95)
    percBars = melt(rbind(perc_5, perc_95), id.vars="p", value.name="q", variable.factor=FALSE)
    percBars[, p := factor(p, levels=c(5,95), labels=c("5th", "95th"))]
    for(i in seq(1, length(cols))) {
        percBars[variable == cols[i],
                 variable := paste(names[i], "~group('[',", units[i], ",']')", sep="")]
    }
    namedBars = NULL
    for(n in allQuants[, unique(compName)]) {
        namedBars = rbind(namedBars, data.table(percBars, compName=n))
    }
    
    plot = ggplot(allQuants, aes_string(x="ref", y="other")) +
        geom_abline(intercept=0, slope=1) +
        geom_vline(data=namedBars, aes(xintercept=q, linetype=p), size=lineSize) +
        geom_line(aes_string(colour=by), size=lineSize) +
        geom_point(aes_string(colour=by), shape=shape, size=size) +
        facet_wrap(compName~variable, scales="free", labeller=label_alpha_parsed) +
        labs(x=refSet, y=otherName) +
        scale_colour_discrete(name=byName) +
        scale_linetype_manual(values=c(2,3), name="Percentile")

    if(logScale)
        plot = plot + scale_x_log10() + scale_y_log10()
    
    return(plot)
}

