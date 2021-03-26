## TITAN_functions.R
## Functions to deal with TITAN outputs.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(ggplot2)
require(stringr)
require(scales)
require(doParallel)

readTITANTracks = function(trackFile, outputProj=projLatLon(), timeRes=300,
                           skipNumProj=3, skipNumRecords=43) {
    ## Read tracks produced by TITAN.
    ## 
    ## First, use Titan to produce storm files. Then run stormStats
    ## and save the output as a text file.
    ## 
    ## Args:
    ##   trackfile: The name of a file containing output from stormStats.
    ##   outputPRoj: Projection to project to (Default: NULL, no projection).
    ##   timeRes: Expected time resolution [s]. (Default: 300, five minutes).
    ##   treatOverlap: Deal with overlapping segments from multiple files? 
    ##                 (default: TRUE).
    ##   skipNumProj: Number of lines to skip at start of file before two-line projection
    ##                information is found (default: 3).
    ##   skipNumRecords: Number of lines to skip at start of file before track data is found 
    ##                   (default: 43).
    ##  
    ## Results: data.table with track information and trackID as an identifier.

    projInfo = data.table(read.table(trackFile, skip=3, nrows=1, header=TRUE,
                                     sep=",", stringsAsFactors=FALSE))
    
    tracks = data.table(read.table(trackFile, skip=skipNumRecords, header=TRUE, 
                                   sep=",", stringsAsFactors=FALSE),
                        inFile=trackFile)

    ## Format UTC times.
    tracks[, timestamp := as.POSIXct(timestamp, tz="UTC", format="%Y-%m-%dT%H:%M:%S")]
    tracks[, originTime := as.POSIXct(originTime, tz="UTC", format="%Y-%m-%dT%H:%M:%S")]
    
    ## Storms that lasted only one time step have a duration of zero; correct to assume the storm
    ## lasted the whole time step.
    tracks[duration == 0, duration := timeRes]
    tracks[stormDuration == 0, stormDuration := timeRes]
    tracks[history == 0, history := timeRes]

    ## Check vertical coordinate units.
    if(projInfo$unitz != "km")
        stop("Z coordinates are not in km.")
    
    ## Reproject as required.
    scaleCoords = 1
    if(projInfo$proj_type == "flat") {
        if(projInfo$unitx != "km" | projInfo$unity != "km")
            stop("readTITANTracks: flat projection requires coordinates in km.")

        ## Convert coordinates from km to m.
        tracks[, xCoord := xCoord * 1e3]
        tracks[, yCoord := yCoord * 1e3]
        scaleCoords = 1e3
        
        inProj = CRS(paste("+proj=aeqd +units=m +lat_0=", projInfo$origin_lat,
                           " +lon_0=", projInfo$origin_lon, sep=""))
    } else if(projInfo$proj_type == "latlong") {
        if(projInfo$unitx != "deg" | projInfo$unity != "deg")
            stop("readTITANTracks: latlong projection requires coordinates in degrees.")
        
        inProj = projLatLon()
    } else {
        stop("readTITANTracks: unknown projection type.")
    }

    ## Reproject centre coordinates.
    tracks = reprojectDataTable(dt=tracks, coordNames=c("xCoord", "yCoord"),
                                oldProj=inProj, newProj=outputProj)
    
    ## Set track ID to a string type.
    tracks[, trackID := as.character(trackID)]

    ## Track outlines are stored in stormRuns (x,y,n triplets in grid coordinates). To convert them
    ## to raster/polygons/coordinates, the original projection information is required. Here it is
    ## returned along with the tracks.
    tracks$trackProj = outputProj@projargs
    tracks$runProj = inProj@projargs
    tracks$runMinX = projInfo$minx
    tracks$runMinY = projInfo$miny
    tracks$runDx = projInfo$dx
    tracks$runDy = projInfo$dy
    tracks$runUnitX = projInfo$unitx
    tracks$runUnitY = projInfo$unity

    return(tracks)
}

titanTrackRuns = function(tracks, extraCols=NULL,
                          cols=c("trackProj", "runProj", "runMinX", "runMinY",
                                 "runDx", "runDy", "runUnitX", "runUnitY")) {
    ## Parse and return x,y,n triplets for runs for each track in a dataset.
    ## x and y are grid coordinates, n is the number of grid points in the x
    ## direction that are part of the storm from a given point.
    ##
    ## Args:
    ##   tracks: Track information.
    ##   extraCols: Any extra columns to return; should be invariant within each track.
    ##   cols: Run information cols that should always be included.
    ##
    ## Returns: run information in long format.

    stopifnot(!any(extraCols %in% cols))
    stopifnot(c("stormRuns", "timestamp", "trackID", "stormID",
                cols, extraCols) %in% names(tracks))
    
    runCoords = tracks[, list(triplet=unlist(strsplit(stormRuns, split=" "))),
                       by=c("timestamp", "trackID", "stormID", cols, extraCols)]

    ## Extract x, y, and length of run n from the stored triplets.
    runCoords[, y := as.numeric(str_match(triplet, "[0-9]+,([0-9]+),[0-9]+")[,2])]
    runCoords[, x := as.numeric(str_match(triplet, "([0-9]+),[0-9]+,[0-9]+")[,2])]
    runCoords[, n := as.numeric(str_match(triplet, "[0-9]+,[0-9]+,([0-9]+)")[,2])]
    stopifnot(runCoords[, identical(triplet, paste(x,y,n, sep=","))])
    runCoords[, triplet := NULL]
    
    return(runCoords)
}

checkTITANConsistency = function(tracks, tRes=300) {
    ## Check the simple internal consistency of a TITAN track database.
    ##
    ## Args:
    ##   tracks: The database to check.
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
    
    checkBasicConsistency(tracks, tRes=tRes)

    ## stormDuration.
    t(tracks[, list(a=as.numeric(difftime(max(timestamp), min(timestamp), units="secs"))+tRes,
                    b=unique(stormDuration)), by=stormID], "stormDuration")

    ## numTracks.
    t(tracks[, list(a=unique(numTracks), b=length(unique(trackID))), by=stormID], "numTracks")
    
    ## nParents/parents.
    stopifnot(tracks[nParents == 0, all(is.na(parents))])
    t(tracks[nParents > 0, list(a=unlist(lapply(str_split(parents, pattern=" "), length)),
                                b=nParents)], "nParents/parents")

    ## nChildren/children.
    stopifnot(tracks[nChildren == 0, all(is.na(children))])
    t(tracks[nChildren > 0, list(a=unlist(lapply(str_split(children, pattern=" "), length)),
                                b=nChildren)], "nChildren/children")
}

relativeIntensity = function(dat, intensityCol="maxZ", timeRes=300) {
    ## Calculate relative intensities for each 5% of the time covered by each track.
    ## 
    ## Args:
    ##   dat: The data, must contain trackID, timestamp, and 'intensityCol'.
    ##   intensityCol: The column that denotes intensity. Must be all positive.
    ##   timeRes: Time resolution for the data.
    ## 
    ## Returns: distributions of proportions of intensity, for each 5% proportion
    ## of track time.
    
    ## Check required names and that intensities are positive.
    stopifnot(c("trackID", "timestamp", intensityCol) %in% names(dat))
    stopifnot(!("intensity" %in% names(dat)))

    ## Don't change the original data.
    dat = copy(dat)
    setnames(dat, intensityCol, "intensity")
    stopifnot(dat[, all(intensity > 0)])

    ## Order by time within each track.
    setkey(dat, trackID, timestamp)

    ## Work out intensity percentages.
    dat[, intensityRange := max(intensity) - min(intensity), by=trackID]
    dat[, minIntensity := min(intensity), by=trackID]
    dat[, intensityPerc := (intensity - minIntensity) / intensityRange]

    if(any(is.na(dat[, intensityPerc])))
        warning("Some cells had an intensity range of zero.")
    
    ## Sample intensity percentages for each 5% of the track time.
    sampleIntPercs = function(d) {
        ## Percentages to sample.
        percs = seq(0, 1, by=0.05)
        minTime = d[, as.numeric(min(timestamp))]
        maxTime = d[, as.numeric(max(timestamp))]
        timeRange = maxTime - minTime
        
        ## Times from which we can sample.
        sampleTimes = minTime + timeRange * percs
        
        ## Sample the indexes that correspond to each time percentage.
        sampleIdx = nn2(data=d[, as.numeric(timestamp)], query=sampleTimes, k=1)$nn.idx
        return(data.table(d[sampleIdx, list(intensityPerc, perc=percs)]))
    }

    intPercs = dat[intensityRange != 0, sampleIntPercs(.SD), by=trackID]

    relDat = intPercs[, list(q10=quantile(intensityPerc, probs=0.10),
                             q25=quantile(intensityPerc, probs=0.25),
                             mean=mean(intensityPerc),
                             med=median(intensityPerc),
                             q75=quantile(intensityPerc, probs=0.75),
                             q90=quantile(intensityPerc, probs=0.90)), by=perc]
    
    return(relDat)
}

TITANParameters = function(paramFile) {
    ## Read a TITAN parameter file and mine out important details.
    ##
    ## Args:
    ##   paramFile: The parameter file to read.
    ##
    ## Returns: list with important parameter details.

    con = file(paramFile, "r")
    params = NULL
    
    ## Read the parameters line by line.
    i = 1
    while(TRUE) {
        line = readLines(con, n=1)
        if(length(line) == 0)
            break

        ## Remove lines without an equals sign.
        if(!str_detect(line, "="))
            next
        
        ## Remove comments.
        if(str_detect(line, "^//"))
            next

        ## Remove opening brace statements.
        if(str_detect(line, "\\{"))
            next

        ## Remove leading spaces.
        line = str_replace(line, "^ *", "")

        ## Separate line into key/value pairs.
        matches = unlist(str_match_all(line, "([^ ]*) *= *([^ |;|,]*)[;|,]?"))
        paramName = matches[2]

        if(!is.null(params)) {
            if(paramName %in% params[, param]) {
                paramName = paste(paramName, "_", i)
                i = i + 1
            }
        }
        
        params = rbind(params, data.table(param=paramName, value=matches[3]))
    }

    stopifnot(!any(is.na(params$value)))
    stopifnot(!any(is.na(params$param)))
    close(con)

    ## Description of thresholding used ("single" or "dual").
    params = rbind(params, data.table(param="thresholding_used", value="single"))
    if(params[param == "use_dual_threshold", value] == TRUE)
        params[param == "thresholding_used", value := "dual"]

    return(params)
}

getTITANCells = function(dat) {
    ## Retrieve TITAN cells from run information; return as (large) data.table.
    ##
    ## Args:
    ##  dat: TITAN track data including stormRuns.
    ## 
    ## Returns: cell data.table.
    
    cells = titanTrackRuns(dat)

    ## Ensure each x,y coordinate for each track/time combination is unique.
    stopifnot(!any(duplicated(cells, by=c("timestamp", "trackID", "x","y","n"))))
    
    ## Repeat each x,y coordinate n times.
    idx = rep(1:nrow(cells), times=cells$n)
    cells = cells[idx,]

    ## Increase values of x by one for each repeat.
    cells[, add := seq(0,n-1), by=c("timestamp", "trackID", "x","y","n")]
    cells[, x := x + add]
    cells[, add := NULL]    

    ## Convert x and y from grid numbers to coordinates.
    cells[, x := runMinX + runDx * x]
    cells[, y := runMinY + runDy * y]
    
    ## Get projection information.
    proj = unique(cells$runProj)
    if(length(proj) != 1)
        stop("titanCells: Only a single run projection (runProj) can be specified!")

    ## Convert km to m if the grid coordinates are in km.
    cells[runUnitX == "km", x := x * 1e3]
    cells[runUnitY == "km", y := y * 1e3]
    
    return(list(cells=cells, proj=proj))
}

TITANSettingsMatch = function(set1, set2, ignoreVarName=TRUE, ignoreThresholds=TRUE) {
    ## Check whether TITAN settings match and warn if not.
    ##
    ## Args:
    ##   set1, set2: settings read by TITANParameters().
    ##   ignoreVarName: If TRUE, do not expect the name (of tracked variable) to
    ##               match (default: TRUE).
    ##   ignoreThresholds: If TRUE, do not expect thresholds to match (default: TRUE).
    ##
    ## Returns: void.
    
    params = set1[, unique(param)]

    if(ignoreVarName) {
        params = params[which(params != "name")]
    }
    
    if(ignoreThresholds) {
        params = params[which(params != "low_dbz_threshold")]
        params = params[which(params != "high_dbz_threshold")]
        params = params[which(params != "dbz_threshold")]
    }

    stopifnot(length(params) > 0)
    res = all.equal(set1[param %in% params], set2[param %in% params])
    if(!(identical(res, TRUE))) {
        warning("Differences between two TITAN settings objects:")
        warning(res)
        mismatches = NULL
        for(p in params)
            if(!identical(all.equal(set1[param == p], set2[param == p]), TRUE))
                mismatches = paste(mismatches, "\n", p, sep="")
        warning(mismatches)
    }
}

allTITANParameters = function(dirs, names, paramFile="params/Titan.opt") {
    ## Read multiple TITAN parameter files and check no differences other than
    ## thresholds exist.
    ##
    ## Args:
    ##   dirs: Directories in which to find parameters.
    ##   names: Names of each set, one per directory.
    ##
    ## Returns: Parameters by paramSet.

    stopifnot(length(dirs) == length(names))
    params = NULL
    for(i in seq(1, length(dirs))) {
        res = TITANParameters(paramFile=paste(dirs[i], paramFile, sep="/"))
        res[, paramSet := names[i]]
        params = rbind(params, res)
    }

    ## Find non-threshold differences and warn about them. Name is the name of the
    ## variable that is tracked and is allowed to differ.
    allowedDiffs = c("name", "low_dbz_threshold", "dbz_threshold", "min_storm_size",
                     "min_area_each_part")
    diffCols = unique(params[, list(param, value)])[duplicated(param), unique(param)]
    diffCols = diffCols[which(!(diffCols %in% allowedDiffs))]

    if(length(diffCols) != 0) {
        warning("Differences between TITAN parameter files, for parameters:")
        mismatches = ""
        for(p in diffCols)
            mismatches = paste(mismatches, "\n", p)
        warning(mismatches)
    }

    return(params)
}

TITANThresholdsTable = function(params, ...) {
    ## Print a table of all TITAN thresholds used.
    ##
    ## Args:
    ##   params: The parameters returned by allTITANParameters().
    ##   ...: Options to printTable().
    ##
    ## Returns: void.
    
    cap = paste("The threshold values used in each application of TITAN. Other thresholds were",
                "left at default values. These thresholds are for the basic detection threshold",
                "($Z$ threshold, the \\texttt{low\\_dbz\\_threshold} parameter), the",
                "dual-thresholding sub-region threshold (Sub-region $Z$ threshold,",
                "\\texttt{dual\\_threshold}'s \\texttt{dbz\\_threshold} parameter), the",
                "minimum allowed storm volume (Min. volume, the \\texttt{min\\_storm\\_size}",
                "parameter), and the minimum area for sub-parts (Min. sub-area,",
                "\\texttt{dual\\_threshold}'s \\texttt{min\\_area\\_each\\_part} parameter).")
    
    params = unique(params[param %in% c("low_dbz_threshold", "dbz_threshold",
                                        "min_storm_size", "min_area_each_part")])

    params[param == "low_dbz_threshold", param := "$Z$ threshold [dBZ]"]
    params[param == "dbz_threshold", param := "Sub-region $Z$ threshold [dBZ]"]
    params[param == "min_storm_size", param := "Min. volume [km$^3$]"]
    params[param == "min_area_each_part", param := "Min. sub-area [km$^2$]"]
    
    tab = tabular(Heading()*Factor(paramSet)~Heading()*Factor(param, texify=FALSE)*
                      Heading()*identity*Heading()*value, data=params)
    printTable(tab, cap=cap, lab="tab:TITANThresholds", ...)
}

readTITANSets = function(titanDirs, titanNames, titanFile, timeRes=300, minDuration=30,
                         subsetFunc=subsetToROI, copyAllRegions=TRUE, titanSetName="WRF+TITAN", ...) {
    ## Read in TITAN data, ready for comparisons to be made. All TITAN data is read.
    ##
    ## Args:
    ##   titanDirs: TITAN directories to read from.
    ##   titanNames: A name for each TITAN dataset.
    ##   titanFile: The TITAN data files to read.
    ##   timeRes: Time resolution to expect [s], (default: 300 s).
    ##   minDuration: The minimum duration to keep [min] (default: 30 minutes).
    ##   subsetFunc: A function that spatially subsets tracks.
    ##   copyAllRegions: Copy data for "all" regions? (Default: TRUE).
    ##   titanSetName: The name for the TITAN set of data (default: WRF+TITAN).
    ##   ...: Optional argumentes to addHailInfo() or readTITANforComparisons().
    ##   
    ## Returns: list containing titan data, the percentages of tracks removed for being too short,
    ## and percentages in the final dataset of tracks affected by detected splits. Also contains
    ## "simple", a simplified dataset with common names for common variables, to ease comparisons.
    
    titan = NULL
    ## Loop through simulation sets to load.
    for(i in seq(1, length(titanDirs))) {
        res = readTITANforComparisons(titanFile=paste(titanDirs[i], titanFile, sep="/"),
                                      timeRes=timeRes, subsetFunc=subsetFunc, ...)
        res$titan[, set := paste(titanSetName, " (", titanNames[i], ")", sep="")]
        res$titan[, scheme := titanNames[i]]
        titan = rbind(titan, res$titan)
        
        if(i == 1) {
            minTime = res$minTime
            maxTime = res$maxTime
        } else {
            minTime = min(minTime, res$minTime)
            maxTime = max(maxTime, res$maxTime)
        }
    }

    ## Determine number of tracks split. Note this before minimum duration is applied.
    splitChangesPerc = round(titan[split == TRUE, length(unique(trackID))] /
                             titan[, length(unique(trackID))] * 100, 2)   
    
    ## Subset to minimum duration.
    shortTracksPerc = round(titan[, mean(duration < minDuration)*100], 1)
    titan = titan[duration >= minDuration]
    titan = titan[, stormDuration := NULL]
    titan = titan[, numTracks := NULL]

    if(copyAllRegions) {
        ## Make a copy of all tracks for "All" regions.
        titan_all = copy(titan)
        titan_all[, region := "All"]
        titan = rbind(titan, titan_all)
    }

    ## Add hail information.
    titan = addHailInfo(dat=titan, simDirs=titanDirs, simNames=titanNames, ...)

    ## Simple version for easier comparisons.
    simple = titan[, list(set=set, scheme, trackID, xCoord, yCoord, timestamp,
                          area=projArea, speed, direction=dir, meanZ, maxZ, duration,
                          day, hour, region,
                          propSevereHail25=NPixHailgt25mm/nPix,
                          propSevereHail40=NPixHailgt40mm/nPix)]
    
    return(list(shortTracksPerc=shortTracksPerc, splitChangesPerc=splitChangesPerc,
                minTime=minTime, maxTime=maxTime, titan=titan, simple=simple,
                minDuration=minDuration))
}

readTITANforComparisons = function(titanFile, subsetFunc=NULL,
                                   proj=projSwiss(), timeRes=300, ...) {
    ## Read TITAN tracks and set them up for comparisons.
    ##
    ## Args:
    ##   titanFile: The TITAN CSV file to read.
    ##   subsetFunc: Optional function to subset data (default: no subsetting).
    ##   proj: The projection to return.
    ##   timeRes: The expected temporal resolution [s].
    ##   ...: Optional extra arguments to distanceFromNearestRadar().
    ##
    ## Returns: list with titan, minLon, maxLon, minTime, and maxTime.

    ## Read tracks.
    titan = readTITANTracks(titanFile, timeRes=timeRes) 
       
    ## Reproject to Swiss coordinates.
    titan = reprojectDataTable(dt=titan, coordNames=c("xCoord", "yCoord"),
                               oldProj=projLatLon(), newProj=proj)
    
    ## Add radar distances.
    titan = distanceFromNearestRadar(dat=titan, coordCols=c("xCoord", "yCoord"), ...)
        
    ## Assign regions.
    titan[, region := coordinateRegion(xCoord, yCoord, proj=proj)]

    ## Set up days and hours.
    titan[, day := as.Date(timestamp)]
    titan[, hour := as.numeric(strftime(timestamp, tz="UTC", format="%H"))]

    ## Subset?
    if(!is.null(subsetFunc))
        titan = subsetFunc(titan)

    ## Fix split tracks.
    titan = fixSplitTracks(titan, expectedDiff=timeRes)

    ## Check tracks for consistency.
    checkTITANConsistency(titan, tRes=timeRes)       
        
    ## Convert durations from seconds to minutes.
    titan[, duration := duration / 60]
    titan[, stormDuration := stormDuration / 60]
    
    ## Temporal coverage.
    minTime = titan[, min(timestamp)]  
    maxTime = titan[, max(timestamp)]
    
    return(list(titan=titan, minTime=minTime, maxTime=maxTime))
}

addHailInfo = function(dat, simDirs, simNames, hailFile="tracks/track_hail_data.rds") {
    ## Read hail information collected offline using collect_hail_properties.R, and
    ## add it to a track information data.table.
    ##
    ## Args:
    ##   dat: Track data.
    ##   simDirs: Directories for each microphysical scheme.
    ##   simNames: Names of each scheme, should match "scheme" in dat.
    ##   hailFile: The relative path of the hail file for each scheme to read.
    ##
    ## Returns: The data.table with hail information added.
    
    ## 'dat' should contain 'scheme' equal to simNames.
    stopifnot("scheme" %in% names(dat))
    stopifnot(all(dat$scheme %in% simNames))
    
    ## Get hail for each microphysical scheme.
    hail = data.table()
    for(i in seq(1, length(simDirs))) {
        hailfile = paste(simDirs[i], hailFile, sep="/")
        if(file.exists(hailfile)) {
            hailinfo = readRDS(file=hailfile)
            hailinfo[, scheme := simNames[i]]
            hail = rbind(hail, hailinfo)
        }
    }    

    if(nrow(hail) == 0) {
        dat[, NPixHailgt40mm := NA]
        dat[, NPixHailgt25mm := NA]
        dat[, nPix := NA]
        return(dat)
    }
    
    setkey(dat, scheme, timestamp, trackID)
    setkey(hail, scheme, timestamp, trackID)
            
    res = hail[dat]
    stopifnot(nrow(res) == nrow(dat))
    stopifnot(identical(res$timestamp, dat$timestamp))
    stopifnot(identical(res$scheme, dat$scheme))
    stopifnot(identical(res$trackID, dat$trackID))
    return(res)
}
