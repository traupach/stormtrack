## TRT_TITAN_comparison_functions.R
## Functions to compare TRT and TITAN results.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(tables)
require(xtable)
require(gridExtra)
require(viridis)
require(data.table)

readTrackerData = function(titanDirs, titanNames, titanFile, trtDir, timeRes=300, minDuration=30,
                           subsetFunc=subsetToROI, copyAllRegions=TRUE, titanSetName="WRF+TITAN",
                           ...) {
    ## Read in TITAN and TRT data, ready for comparisons to be made. All TITAN
    ## data is read, TRT data for the same time period is read.
    ##
    ## Args:
    ##   titanDirs: TITAN directories to read from.
    ##   titanNames: A name for each TITAN dataset.
    ##   titanFile: The TITAN data files to read.
    ##   trtDir: Location to find TRT data files.
    ##   timeRes: Time resolution to expect [s], (default: 300 s).
    ##   minDuration: The minimum duration to keep [min] (default: 30 minutes).
    ##   subsetFunc: A function that spatially subsets tracks.
    ##   copyAllRegions: Copy data for "all" regions? (Default: TRUE).
    ##   titanSetName: The name for the TITAN set of data (default: WRF+TITAN).
    ##   ...: Optional extra arguments to readTRTforComparisons(),
    ##        readTITANSets().
    ##   
    ## Returns: list containing titan and trt data, the percentages of tracks removed for being
    ## too short, and percentages in the final dataset of tracks affected by detected splits. Also
    ## contains "all", a joined dataset with common names for common variables, to ease comparisons.

    titan = readTITANSets(titanDirs=titanDirs, titanNames=titanNames, titanFile=titanFile,
                          timeRes=timeRes, minDuration=minDuration, subsetFunc=subsetFunc, 
                          copyAllRegions=copyAllRegions, titanSetName=titanSetName, ...)
                          
    trt = readTRTforComparisons(dir=trtDir, start=titan$minTime,
                                end=titan$maxTime, subsetFunc=subsetFunc,
                                timeRes=timeRes, ...)$trt
    
    ## Only keep tracks over minDuration minutes.
    shortTracksPercTITAN = titan$shortTracksPerc
    shortTracksPercTRT = round(trt[, mean(duration < minDuration)*100], 1)
    trt = trt[duration >= minDuration]

    ## "In the final dataset, X% of tracks are modified by detected splits.
    splitChangesPercTITAN = titan$splitChangesPerc
    splitChangesPercTRT = round(trt[split == TRUE, length(unique(trackID))] /
                                trt[, length(unique(trackID))] * 100, 2)

    if(copyAllRegions) {
        ## Make a copy of all tracks for "All" regions.
        trt_all = copy(trt)
        trt_all[, region := "All"]
        trt = rbind(trt, trt_all)
    }

    ## In TRT data, -9990 is NA.
    trt[NPixMESHSgt25mm < 0, NPixMESHSgt25mm := NA]
    trt[NPixMESHSgt40mm < 0, NPixMESHSgt40mm := NA]
    
    all = rbind(titan$simple,
                trt[, list(set="TRT (Observations)", 
		           scheme="Observations", trackID, xCoord, yCoord, timestamp,
                           area=cellArea, speed, direction, meanZ=cell_dbZ_avg, maxZ,
                           duration, day, hour, region,
                           propSevereHail25=NPixMESHSgt25mm/cellArea,
                           propSevereHail40=NPixMESHSgt40mm/cellArea)])

    ## Check for no duplicated column names.
    stopifnot(!any(duplicated(names(titan))))
    stopifnot(!any(duplicated(names(trt))))
    stopifnot(!any(duplicated(names(all))))

    ## Remove cells with zero speed.
    trtRemovedNASpeed = nrow(trt[is.na(speed)]) 
    stopifnot(trtRemovedNASpeed == nrow(all[is.na(speed)]))
    trtRemovedNAPerc = round(trtRemovedNASpeed / nrow(trt) * 100, 2)
    trt = trt[!is.na(speed)]
    all = all[!is.na(speed)]
    
    return(list(titan=titan$titan, trt=trt, all=all,
                splitChangesPercTRT=splitChangesPercTRT,
                splitChangesPercTITAN=splitChangesPercTITAN,
                shortTracksPercTITAN=shortTracksPercTITAN,
                shortTracksPercTRT=shortTracksPercTRT,
                removedNASpeedTRT=trtRemovedNASpeed,
                removedNASpeedTRTPerc=trtRemovedNAPerc,
                minDuration=minDuration))
}

plotSplitLocation = function(trt, titan, reg="All") {
    ## Prepare a plot of the locations of tracks (grey) with truncated and split tracks shown in
    ## blue and red respectively.
    ##
    ## Args:
    ##   trt, titan: TRT and TITAN datasets.
    ##   reg: The region to compare ("All" by default).
    ##
    ## Returns: ggplot object.

    stopifnot(reg %in% trt[, region])
    stopifnot(reg %in% titan[, region])
    
    ## Plot to show where split tracks occurred.
    allTracks = rbind(trt[region == reg, list(xCoord, yCoord, split, trunc,
                                              trackID, set="TRT (Observations)")],
                      titan[region == reg, list(xCoord, yCoord, split, trunc,
                                                trackID, set="WRF+TITAN")])
    
    plot = ggplot(allTracks, aes(x=xCoord, y=yCoord)) + facet_wrap(~set) +
        geom_path(aes(group=trackID), colour="darkgrey") + 
        geom_path(data=allTracks[trunc==TRUE], colour="blue", size=0.75, aes(group=trackID)) +
        geom_path(data=allTracks[split==TRUE], colour="red", size=0.75, aes(group=trackID)) +
        labs(x="Swiss coord. X [m]", y="Swiss coord Y [m]") + coord_fixed()

    return(plot)
}

cellsPerDayComp = function(dat, refSet="TRT (Observations)",
                           cap=paste("Performance statistics on cells detected",
                                     "per day per region, with", refSet, "taken as the",
                                     "reference. Statistics shown are bias [d$^{-1}$],",
                                     "root mean squared error (RMSE)",
                                     "[d$^{-1}$], relative bias (RB) [\\%], median relative",
                                     "error (MRE) [\\%], interquartile range of relative",
                                     "error (RE IQR) [\\% points], and squared Pearson correlation",
                                     "($r^2$) [-]."),
                           label="tab:cellsPerDayStats", booktabs=TRUE, size=NULL, ...) {
    ## Print a table of comparison statistics on cells per day by region.
    ##
    ## Args:
    ##   dat: track data with "set" values to compare.
    ##   cap, label: Table caption and label.
    ##   booktabs: Use booktabs for the table? (default: TRUE).
    ##   size: Font size for the table (default: NULL, don't specify).
    ##   ...: Extra arguments to print.xtable().
    ##
    ## Returns: the comparison statistics.

    stopifnot(refSet %in% dat[, unique(set)])
    otherSets = dat[set != refSet, unique(set)]
    
    cellsPerDay = dat[, list(cells=length(unique(trackID))), by=c("set", "day","region")]

    stats = NULL
    comp = NULL
    
    for(other in otherSets) {
        cellsPerDayStats = setComparison(ref=cellsPerDay[set == refSet],
                                         res=cellsPerDay[set == other],
                                         col="cells", key="day", groupBy="region", missing=0)

        stats = rbind(stats, data.table(set=other, cellsPerDayStats$stats))
        comp = rbind(comp, data.table(set=other, cellsPerDayStats$raw))
    }

    # Force rounding of r2 to 2 decimal places.
    stats[, r2 := round(r2, 2)]
        
    tab = tabular(Heading()*Factor(set)*Heading()*Factor(region)~Heading()*identity*
                      (Format(digits=1)*
                        (Heading("Bias")*bias+RMSE)+
                       Format(digits=0)*
                         (Heading("RB")*meanRB+
                         Heading("MRE")*medRB+
                         Heading("RB IQR")*RBIQR)+
                       Format(texify=False, digits=2)*Heading("$r^2$")*r2),
                  data=stats)
    printTable(tab=tab, cap=cap, lab=label, useBooktabs=booktabs, size=size, ...)

    return(list(stats=stats, comp=comp))
}

plotTRTThresholds = function(trt) {
    ## Prepare a plot of the histogram of TRT detection thresholds.
    ##
    ## Args:
    ##  trt: TRT dataset.
    ##
    ## Returns: ggplot object.
    
    return(ggplot(trt[region == "All"], aes(x=minDetThresh_dBz)) + 
          geom_histogram(binwidth=1) +    
          labs(x="Min. detection threshold [dBZ]", y="Density"))
}
         
writeAllCells = function(titan, trt, trtOutlineDir,
                         trtOutfile="trt_selected_cells",
                         titanOutfile="titan_selected_cells",
                         outProj=projLatLon(),
                         outdir="./cells/", ...) {
    ## Write all TITAN and TRT cells to shapefiles (titan_cells and trt_cells) in a given directory.
    ##
    ## Args:
    ##   titan: TITAN track information.
    ##   trt: TRT track information (cell outlines will be read).
    ##   trtOutlineDir: The directory where TRT cell information is stored.
    ##   {trt|titan|out}proj: The projection (CRS) for TRT cells, TITAN cells, and output shapefile.
    ##   outdir: Output directory (default: "cells").
    ##
    ## Returns: void.
    
    ## Create the output directory.
    if(!dir.exists(outdir)) {
        dir.create(outdir)
    }
    
    ## Read and save TRT cell outline estimates.
    trtCells = getTRTCells(dat=trt, dir=trtOutlineDir)
    writeCellsShapefile(cells=trtCells$cells, outfile=trtOutfile, outdir=outdir,
                        cellProj=trtCells$proj, proj=outProj)

    ## Save TITAN cell outlines.
    titanCells = getTITANCells(dat=titan)
    writeCellsShapefile(cells=titanCells$cells, outfile=titanOutfile, outdir=outdir, 
                        cellProj=titanCells$proj, proj=outProj)
}

fixSplitTracks = function(tracks, expectedDiff=300) {
    ## Find tracks that are not continuous and split them into separate parts.
    ## 
    ## Args:
    ##   tracks: The tracks to treat.
    ##   expectedDiff: The expected difference between time steps [s].
    ## 
    ## Returns: the tracks with IDs of split tracks updated.

    ## Expect columns "trackID" and "timestamp", no "split" column.
    stopifnot(c("trackID", "timestamp") %in% names(tracks))
    stopifnot(!("split" %in% names(tracks)))
    
    ## Ensure the tracks are stored in time order.
    setkey(tracks, trackID, timestamp)
    
    ## Keep a copy of original track IDs.
    tracks[, original_trackID := trackID]

    ## Time differences in seconds. Tracks with an unusual time difference have been split.
    tracks[, split := !all(diff(as.numeric(timestamp)) == expectedDiff), by=trackID]

    ## Loop through tracks with splits.
    splitIDs = tracks[split == TRUE, unique(trackID)]
    for(splitID in splitIDs) {
        ## Get the last time in each track, ie the time just before the missing timestep(s).
        splitTimes = tracks[trackID == splitID, 
                            timestamp[which(diff(as.numeric(timestamp)) != expectedDiff)]]
        
        ## Assign new ID numbers to each subtrack.
        subtrack = 1
        for(splitTime in splitTimes) {
            newID = paste(splitID, "_", subtrack, sep="")
            tracks[trackID == splitID & timestamp <= splitTime, trackID := newID]
            subtrack = subtrack + 1
        }
        
        ## Increment the number of subtracks if required.
        if("numTracks" %in% names(tracks)) {
            sID = tracks[trackID == splitID, unique(stormID)]
            stopifnot(length(sID) == 1)
            tracks[stormID == sID, numTracks := as.integer(numTracks + length(splitTimes) + 1)]
        }

        ## Assign a new ID to the last subtrack.
        tracks[trackID == splitID, trackID := paste(trackID, "_", subtrack, sep="")]
    }

    ## Mark truncated tracks.
    tracks[, trunc := duration != (expectedDiff * length(timestamp)), by=trackID]
    tracks[split == TRUE, trunc := FALSE]

    ## Redo durations in seconds.
    tracks[, duration := NULL]
    tracks[, duration := expectedDiff * length(timestamp), by=trackID]

    ## For TITAN tracks, redo storm duration.
    if("stormDuration" %in% names(tracks)) {
        tracks[, stormDuration := NULL]
        tracks[, stormDuration := as.numeric(difftime(max(timestamp), min(timestamp), units="secs"))+
                     expectedDiff, by=stormID]
    }
    
    ## Redo number of subtracks in case some are removed.
    if(all(c("stormID", "numTracks") %in% names(tracks)))
        tracks[, numTracks := length(unique(trackID)), by=stormID]
    
    ## Remove history information, since splits affect it.
    if(any(c("history", "historyInScans", "originTime") %in% names(tracks))) {
        tracks[, history := NULL]
        tracks[, historyInScans := NULL]
        tracks[, originTime := NULL]
    }
    
    return(tracks)
}

checkBasicConsistency = function(tracks, tRes=300) {
    ## Check the simple internal consistency of a track database.
    ##
    ## Args:
    ##   tracks: The database to check. Durations in seconds.
    ##   tRes: Time resolution to expect [s] (default: 300).
    ##
    ## Returns: void, but stops if there's a problem.

    ## Function to compare a and b in x, and print if differences are found.
    t = function(x, testname) {
        if(x[, !all(a == b)]) {
            print(paste("Test failed for", testname))
            print(x[a != b])
            stop()
        }
    }
    tracks = copy(tracks)
    
    ## Check no skips in tracks.
    tracks[, numScans := length(unique(timestamp)), by=trackID]
    tracks[numScans > 1, maxDiff := max(diff(as.numeric(timestamp))), by=trackID]
    stopifnot(all(tracks[!is.na(maxDiff), maxDiff == tRes]))
    
    ## duration.
    t(tracks[, list(a=as.numeric(difftime(max(timestamp), min(timestamp), units="secs"))+tRes,
                    b=unique(duration)), by=trackID], "duration")
}

getSingleTracks = function(simdata) {
    ## From TRT and TITAN data, find "single" tracks that have no splits or merges, so their
    ## durations are well defined.
    ##
    ## Args:
    ##   simdata: Simdata containing $trt and $titan and $all.
    ##
    ## Returns: a single data.table of all single tracks that contain no splits or merges.
    
    singleTRTIds = singleTRTCellIDs(simdata$trt[trunc == FALSE])
    singleTRT = simdata$all[set == "TRT (Observations)" & trackID %in% singleTRTIds]
    singleTITANtracks = unique(simdata$titan[is.na(children) & is.na(parents) & trunc == FALSE, 
                                             list(scheme, trackID)])
    
    singleTITAN = copy(simdata$all[set != "TRT (Observations)"])
    setkey(singleTITANtracks, scheme, trackID)
    setkey(singleTITAN, scheme, trackID)
    singleTITAN = singleTITAN[singleTITANtracks]
    singleTracks = rbind(singleTRT, singleTITAN)
    return(singleTracks)
}
