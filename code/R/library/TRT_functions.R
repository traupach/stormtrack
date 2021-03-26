## TRT functions.
##
## Functions to deal with TRT data in R data.table format.
##
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>
## Last modified: 06.11.2018.

require(data.table)
require(sp)
require(RANN)
require(doParallel)
require(plyr)

TRTfileTime = function(filenames) {
    ## Convert times in TRT filenames to POSIX objects.
    ##
    ## Args:
    ##   filenames: TRT file filenames.
    ## 
    ## Returns: Times converted from filenames.
    
    fileparts = data.table(str_match(filenames, "([0-9]{2})([0-9]{3})([0-9]{2})([0-9]{2})")[,-1])
    names(fileparts) = c("year", "doy", "hour", "minute")
    times = fileparts[, as.POSIXct(paste(year, doy, hour, minute), tz="UTC", format="%y %j %H %M")]
    return(times)
}

readTRT = function(dir, start, end, buffer=(3*3600)) {
    ## Read TRT cell information within a (buffered) timeframe.
    ##
    ## Args:
    ##   dir: The directory in which to find cell files.
    ##   start: The (inclusive) start time.
    ##   end: The (inclusive) end time.
    ##   buffer: Number of seconds for which to buffer (default, 3*3600 = 3 hours).
    ##
    ## Returns: Cells, with tracks that cross the end time truncated just before the end time and
    ## tracks that cross the start time truncated at the start time.

    TRTfiles = data.table(file=list.files(dir, pattern="*.rds"))
    TRTfiles[, filetime := TRTfileTime(file)]
    
    ## Subset to requested times. TRT files contain storms that started at that time and times into
    ## the future from that start time. Use the buffer to find storms that started before the start
    ## time but crossed the start time during their track.
    TRTfiles = TRTfiles[filetime >= start - buffer & filetime <= end]
    
    ## Read each file.
    trtCells = NULL
    for(i in seq(1, nrow(TRTfiles))) {
        filename = paste(dir, TRTfiles[i, file], sep="/")
        filedat = readRDS(file=filename)
        filedat[, id := paste(basename(filename), ".", id, sep="")]
        trtCells = rbind(trtCells, filedat)
    }
    
    ## Subset cells to requested times.
    trtCells[, timestamp := as.POSIXct(t, tz="UTC", format="%Y%m%d%H%M")]
    trtCells = trtCells[timestamp >= start & timestamp <= end]

    return(trtCells)
}

makeTRTCellsUnique = function(trt) {
    ## TRT will, by default, maintain a track ID through splits or merges. TITAN does not. So to
    ## match TITAN, take TRT tracks and make the track IDs unique after splits or merges.
    ##
    ## Args:
    ##   trt: The TRT data.
    ##
    ## Returns: The data set with unique trackIDs after splits and merges.
    
    setkey(trt, trackID, timestamp)
    
    traverseTrack = function(track) {
        splitPoint = which(track[, splitNext == TRUE]) + 1 
        mergePoint = which(track[, mergedLast == TRUE])

        ## If no split or merge is found, return the whole track.
        if(length(splitPoint) == 0 & length(mergePoint) == 0)
            return(track)

        ## The point at which to break the track into separate IDs.
        breakPoint = min(c(splitPoint, mergePoint), na.rm=TRUE)
        
        ## If a split point is the last in the track, the track may have been
        ## split or truncated? Return.
        if(breakPoint > nrow(track))
            return(track)
       
        ## Add ".sm" for "split/merge" to the trackID.
        track[(breakPoint:nrow(track)), trackID := paste(trackID, ".sm", sep="")]

        if(breakPoint == nrow(track))
            return(track)

        ## Recurse.
        return(rbind(track[1:breakPoint],
                     traverseTrack(track[((breakPoint+1):nrow(track)),])))
    }

    res = NULL
    for(id in trt[, unique(trackID)])
        res = rbind(res, traverseTrack(trt[trackID == id]))

    stopifnot(nrow(trt) == nrow(res))
    return(res)
}

MCHRadarLocations = function() {
    ## Return a data.table containing the locations of the five Meteoswiss radars
    ## in Swiss coordinates. If updating these locations, remember to also update
    ## the locations listed in ncl/library/plotting_functions.ncl. 

    albis = data.table(name="Albis",                    lon=8.51200, lat=47.28433, alt=938)
    dole = data.table(name="La Dole",                   lon=6.09941, lat=46.42511, alt=1682)
    lema = data.table(name="Monte Lema",                lon=8.83322, lat=46.04076, alt=1626)
    morte = data.table(name="Point de la Plaine Morte", lon=7.48655, lat=46.37065, alt=2937)
    gipfel = data.table(name="Weissfluhgipfel",         lon=9.79446, lat=46.83497, alt=2850)
    radars = rbindlist(list(dole, morte, albis, lema, gipfel), use.names=TRUE)

    radars[, x := lon]
    radars[, y := lat]
    
    radars = reprojectDataTable(dt=radars, coordNames=c("x", "y"), oldProj=projLatLon(),
                                newProj=projSwiss())
    return(radars)
}

distanceFromNearestRadar = function(dat, coordCols, radars=MCHRadarLocations(),
                                    radarCoordCols=c("x", "y")) {
    ## Add the nearest radar and distance to that radar to a data table, by
    ## default using the MCH radars in Switzerland.
    ##
    ## Args:
    ##   dat: The data table to which to add radar information.
    ##   coordCols: The names of coordinate columns (x then y) in 'dat'.
    ##   radars: Radar information, must contain coordinates and name.
    ##   radarCoordCols: The names of coordinate columns (x then y) in 'radars' (default: x, y).
    ## 
    ## Returns: The data.table with two new columns added. "radarDist" is the Euclidean distance
    ##          in coordinate units to the nearest radar, and "nearestRadar" is the name of the
    ##          nearest radar.
    
    nearest = nn2(data=radars[, radarCoordCols, with=FALSE],
                  query=dat[, coordCols, with=FALSE], k=1)
    dat[, radarDist := nearest$nn.dists]
    dat[, nearestRadar := radars[nearest$nn.idx, name]]
    return(dat)
}

getTRTCells = function(dat, dir, latLong=FALSE) {
    ## Get TRT cell points for each trackID and timestamp.
    ##
    ## Args:
    ##   dat: TRT data (metadata) read by readTRT.
    ##   dir: Directory containing cell outline information.
    ##   latLong: Convert coordinates to latitude/longitude coordinates? (Default: FALSE).
    ##
    ## Returns: data.table with track outlines (x,y in lat/long).

    keys = dat[, list(trackID=unique(trackID))]
    keys[, filename := str_match(trackID, ".*\\.rds")[,1]]
    keys[, id := as.integer(str_match(trackID, ".*rds\\.([0-9]+)")[,2])]

    cells = NULL
    for(file in keys[, unique(filename)]) {
        cellDat = readRDS(paste(dir, file, sep="/"))
        cellDat = cellDat[id %in% keys[filename == file, unique(id)] & type == "RAD"]
        
        cellDat[, trackID := paste(file, id, sep=".")]
        cells = rbind(cells, cellDat)
    }

    ## Properly format times.
    cells[, timestamp := as.POSIXct(t, format="%Y%m%d%H%M", tz="UTC")]

    ## Subset to times in input data.
    cells = cells[timestamp %in% dat[, timestamp]]
    
    ## Convert x and y from km to m.
    cells[, x := x * 1000]
    cells[, y := y * 1000]

    ## Convert x and y from CH1903/LV03 to lat/long coordinates?
    proj = projCH1903()
    if(latLong) {
        cells = reprojectDataTable(dt=cells, coordNames=c("x","y"),
                                   oldProj=projCH1903(), newProj=projLatLon())
        proj=projLatLon()
    }
    
    return(list(cells=cells, proj=proj))
}

mostUsedTRTThreshold = function(dat) {
    ## From TRT tracking data, determine the most-often used reflectivity threshold.
    ##
    ## Args:
    ##   dat: TRT tracking data.
    ##
    ## Returns: the most-often used reflectivity threshold [dBZ].

    freq = dat[, list(num=nrow(.SD)), by=minDetThresh_dBz]
    thresh = freq[which.max(num), minDetThresh_dBz]
    return(thresh)
}

readTRTforComparisons = function(dir, start, end, proj=projSwiss(), subsetFunc=NULL,
                                 timeRes=300, ...) {
    ## Read TRT tracks and set them up for comparisons with TITAN tracks.
    ## 
    ## Args:
    ##   dir: The directory from which to read TRT files.
    ##   start, end: Time period to read (see readTRT()).
    ##   proj: Projection to return for coordinates.
    ##   subset: Optional function to subset the data (default: no subsetting).
    ##   timeRes: Time resolution to expect.
    ##   ...: Optional extra arguments to distanceFromNearestRadar (radars, e.g.).
    ## 
    ## Returns: List with trt, and maxRadarDist of any returned points.

    ## Read TRT tracks.
    trt = readTRT(dir=dir, start=start, end=end) 

    ## Rename columns to match TITAN names.
    setnames(trt, c("id", "lonCellCentrid", "latCellCentrid"), c("trackID", "xCoord", "yCoord"))
    
    ## Reproject. 
    trt = reprojectDataTable(dt=trt, coordNames=c("xCoord", "yCoord"), oldProj=projLatLon(),
                             newProj=proj)
    
    ## Add radar distances.
    trt = distanceFromNearestRadar(dat=trt, coordCols=c("xCoord", "yCoord"), ...)
    trtMaxRadarDist = trt[, max(radarDist)]

    ## Calculate speed and cell direction from velocity vectors.
    trt = trt[, speed := sqrt(cellVelX^2 + cellVelY^2)]           
    trt = trt[, direction := (atan2(cellVelY, cellVelX) * 180/pi)] ## Dir. -180 to 180 from E.
    trt = trt[, direction := 90 - direction]                       ## -90 to 270 from N.
    trt = trt[direction < 0, direction := 360 + direction]         ## 0 to 360 from N.
    trt = trt[speed == 0, direction := NA]

    ## Change duration type.
    trt[, duration := as.numeric(duration)]
    
    ## Assign regions.
    trt[, region := coordinateRegion(xCoord, yCoord, proj=projSwiss())]

    ## Set up days and hours.
    trt[, day := as.Date(timestamp)]
    trt[, hour := as.numeric(strftime(timestamp, tz="UTC", format="%H"))]

    ## Rename maxZ column.
    setnames(trt, "cell_dbZ_max", "maxZ")

    ## Subset?
    if(!is.null(subsetFunc)) {
        trt = subsetFunc(trt)
    }

    ## Convert duration from minutes to seconds, for consistency check.
    trt[, duration := duration * 60]

    ## Make the cells unique after splits/merges, to match TITAN.
    trt = makeTRTCellsUnique(trt)
    
    ## Fix any splits/truncations.
    trt = fixSplitTracks(tracks=trt, expectedDiff=timeRes)
    
    ## Consistency check.
    checkBasicConsistency(tracks=trt, tRes=timeRes)
    
    ## Convert duration from seconds to minutes.
    trt[, duration := duration / 60]
    return(list(trt=trt, maxRadarDist=trtMaxRadarDist))
}

singleTRTCellIDs = function(trt) {
    ## Return cells that did not split or merge during their lifetime from TRT data.
    ##
    ## Args:
    ##   trt: The TRT tracking results, after makeTRTCellsUnique() has been run.
    ##
    ## Returns: a lisf "single" cell IDs that contained no splits or merges.

    ids = trt[, unique(trackID)]
    if(all(is.na(str_match(ids, ".*[.]sm.*"))))
        warning("No splits or merges found; has makeTRTCellsUnique() been run?")
    ids = str_match(ids, "cells[.].*rds[.][1-9]+")
    ids = data.table(count(ids))
    ids = ids[freq == 1]
    return(as.character(ids[, x]))
}
