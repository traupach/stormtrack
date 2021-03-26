## WRF_functions.R
## WRF model helper functions.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(stringr)
require(data.table)

openWRFFile = function(time, dir, dateFormat="%Y-%m-%d_%H:%M:%S") {
  ## Open a connection to a WRF file by time.
  ##
  ## Args:
  ##   time: The time to search for (UTC).
  ##   dir: Directory containing WRF files.
  ##   dateFormat: Format of date in WRF filenames.
  ## 
  ## Returns: netCDF object pointing to a WRF file for the time specified.
  
  pattern = strftime(time, tz="UTC", format=dateFormat)
  file = list.files(dir, pattern=paste(".*", pattern, ".*", sep=""), 
                    full.names=TRUE)
  if(length(file) != 1) {
    stop(paste("Error finding file for time", time))
  }
  
  nc = nc_open(file)
  return(nc)
}

readWRF = function(time, dir, vars, varNames=vars, vertField="ZNU", 
                   vertLevel=NULL, outputProj=projLatLon(), ...) {
  ## Read a WRF netCDF file and return variables.
  ## 
  ## Args:
  ##   time: The time to read.
  ##   dir: NetCDF file directory.
  ##   vars: Variables to read from WRF.
  ##   varNames: Names to assign to each variable (default: names(vars))).
  ##   vertField: Name of the vertical field in the WRF file.
  ##   outputProj: CRS object for output projection (default: lat/long).
  ##   vertLevel: If specified, subset to certain vertical indices 
  ##              (default: NULL).
  ##   ...: Optional extra arguments to openWRFFile()
  ##
  ## Returns: data.table with coordinates and variables.
  
  nc = openWRFFile(time=time, dir=dir, ...)
  if(any(c("x", "y", "z") %in% names(vars))) {
    stop("readWRF: requested variable x, y, or z is reserved for coordinates.")
  }
  
  ## Read coordinates.
  lats = getNCField(nc, "XLAT", "lat")
  longs = getNCField(nc, "XLONG", "lon")
  stopifnot(identical(key(lats), key(longs)))
  res = lats[longs]
  
  for(i in 1:length(vars)) {
    field = getNCField(nc, vars[i], varNames[i])
    setkeyv(res, intersect(key(field), key(res)))
    res = field[res]
  }
  
  ## Read vertical levels.
  if("z" %in% names(res)) {
    vert = getNCField(nc, vertField, "vert", dimNames="z")
    setkey(res, z)
    res = vert[res]
  }
  
  nc_close(nc)
  
  if(!is.null(vertLevel)) {
    res = res[z == vertLevel]
  }
  
  res = reprojectDataTable(res, coordNames=c("lon", "lat"),
                           oldProj=projLatLon(), 
                           newProj=outputProj,
                           rename=TRUE)
  
  return(res) 
}

WRFMetadata = function(WRFDir, WRFPattern="wrfout*", expectedRes=300) {
    ## Get WRF metadata.
    ##
    ## Args:
    ##   WRFDirInner: WRF output directory for the inner domain.
    ##   WRFDirOuter: WRF output directory for the outer domain.
    ##   WRFPatternInner: WRF file pattern for inner domain (default: "wrfout*").
    ##   WRFPatternInner: WRF file pattern for outer domain (default: "wrfout*").
    ##   expectedRes: The expected WRF temporal resolution [s] (default: 300 s).
    ##
    ## Returns: a data.table with metadata.

    ## Analyse simulated period and check for missing time steps.
    WRFFiles = list.files(WRFDir, pattern=WRFPattern, full.name=TRUE)
    WRFTimes = data.table(file=basename(WRFFiles)) 
    WRFTimes[, timeString := str_extract(file, paste("[0-9]{4}-[0-9]{2}-[0-9]{2}",
                                                     "_[0-9]{2}:[0-9]{2}:[0-9]{2}", sep=""))]
    WRFTimes[, time := as.POSIXct(timeString, format="%Y-%m-%d_%H:%M:%S", tz="UTC")]

    firstTime = WRFTimes[, min(time)]
    lastTime = WRFTimes[, max(time)]

    expectedTimes = seq(firstTime, lastTime, by=expectedRes)
    missingTimes = expectedTimes[which(!(expectedTimes %in% WRFTimes[, time]))]
    if(length(missingTimes) != 0) {
        missingTimes = paste(strftime(missingTimes, tz="UTC"), collapse=", ")
        warning(paste("WRF dataset has missing times:", missingTimes, collapse=" "))
    } else {
        missingTimes = ""
    }
    
    ## Open one file to get metadata.
    exampleFile = WRFFiles[1]
    nc = nc_open(exampleFile)

    ## Version of the WRF model.
    version = str_match(ncatt_get(nc=nc, varid=0, attname="TITLE")$value,
                        pattern="WRF.*V(.*) MODEL")[2]

    ## Planetary boundary layer physics scheme.
    boundary = WRFBoundaryLayer(ncatt_get(nc=nc, varid=0, attname="BL_PBL_PHYSICS")$value)
    
    ## Microphysics scheme used (one-word description).
    microphysics = WRFMicrophysics(ncatt_get(nc=nc, varid=0, attname="MP_PHYSICS")$value)

    ## Short-wave radiation.      
    shortwave = WRFShortwave(ncatt_get(nc=nc, varid=0, attname="RA_SW_PHYSICS")$value)

    ## Long-wave radiation.      
    longwave = WRFLongwave(ncatt_get(nc=nc, varid=0, attname="RA_LW_PHYSICS")$value)
    
    ## Velocity damping (lessens updraft formation).
    velocityDamping = "on"
    if(!as.logical(ncatt_get(nc=nc, varid=0, attname="W_DAMPING")$value))
        velocityDamping = "off"

    ## Horizontal resolution.
    res = WRFResolution(nc=nc)

    ## Vertical levels.
    numLevels = length(ncvar_get(nc=nc, varid="ZNU"))

    ## Land surface scheme.
    surface = WRFLandSurface(ncatt_get(nc=nc, varid=0, attname="SF_SURFACE_PHYSICS")$value)

    ## Surface layer physics scheme.
    surfaceLayer = WRFSurfaceLayer(ncatt_get(nc=nc, varid=0, attname="SF_SFCLAY_PHYSICS")$value)

    ## Was cumulus parameterisation used? If not, set explicitConvection to TRUE.
    explicitConvection = (ncatt_get(nc=nc, varid=0, attname="CU_PHYSICS")$value == 0)

    ## The grid properties; if pole_lat, pole_lon and stand_lon are nonzero the grid is regional
    ## rotated.
    poleLat = ncatt_get(nc=nc, varid=0, attname="POLE_LAT")$value
    poleLon = ncatt_get(nc=nc, varid=0, attname="POLE_LON")$value
    standLat = ncatt_get(nc=nc, varid=0, attname="STAND_LAT")$value
    if(poleLat != 90 | poleLon != 0)
        gridType = "regional rotated"
    else
        gridType = "non-rotated"

    ## Test if HAILCAST was used.
    if(!is.null(nc$var[["HAILCAST_DIAM_MAX"]]))
        hailcast = TRUE
    else
        hailcast = FALSE
    
    properties = data.table(firstTime=firstTime,
                            lastTime=lastTime,
                            missingTimes=missingTimes,
                            exampleFile=exampleFile,
                            version=version,
                            microphysicsName=microphysics$name,
                            microphysicsRef=microphysics$ref,
                            velocityDamping=velocityDamping,
                            xAvRes=res$x,
                            yAvRes=res$y,
                            levels=numLevels,
                            landSurfaceName=surface$name,
                            landSurfaceRef=surface$ref,
                            boundaryLayerName=boundary$name,
                            boundaryLayerRef=boundary$ref,
                            surfaceLayerName=surfaceLayer$name,
                            surfaceLayerRef=surfaceLayer$ref,
                            shortwaveName=shortwave$name,
                            shortwaveRef=shortwave$ref,
                            longwaveName=longwave$name,
                            longwaveRef=longwave$ref,
                            expectedRes=expectedRes,
                            explicitConvection=explicitConvection,
                            poleLat=poleLat,
                            poleLon=poleLon,
                            standLat=standLat,
                            gridType=gridType,
                            hailcast=hailcast)

    nc_close(nc)
    return(properties)
}

WRFBoundaryLayer = function(sid) {
    ## Look up a WRF boundary layer physics scheme ID and return its description and reference.
    ##
    ## Args:
    ##   sid: The WRF boundary layer scheme ID number (BL_PBL_PHYSICS).
    ##
    ## Returns: The description and reference for the boundary layer physics scheme.
    
    lookup = rbind(
        data.table(id=1,  name="Yonsei University", ref="Hong_MWR_2006"))
    
    if(!(sid %in% lookup[, id])) {
        stop(paste("Land surface scheme information for ID", sid, "not found."))
    }

    return(lookup[id == sid])
}

WRFMicrophysics = function(sid) {
    ## Look up a WRF microphysics ID and return its description and reference.
    ##
    ## Args:
    ##   sid: The WRF microphysics model ID number (MP_PHYSICS).
    ##
    ## Returns: The description and reference for the microphysics scheme.
    
    lookup = rbind(
        data.table(id=8,  name="Thompson", ref="Thompson_MWR_2008"),
        data.table(id=10, name="Morrison", ref="Morrison_MWR_2009"),
        data.table(id=50, name="Predicted Particle Property (P3)", ref="Morrison_JAS_2015"),
        data.table(id=51, name="Predicted Particle Property (P3)", ref="Morrison_JAS_2015"),
        data.table(id=52, name="Predicted Particle Property (P3)", ref="Morrison_JAS_2015"))

    if(!(sid %in% lookup[, id])) {
        stop(paste("Microphysics information for ID", sid, "not found."))
    }

    return(lookup[id == sid])
}

WRFShortwave = function(sid) {
    ## Look up a WRF shortwave radiation model ID and return its description and reference.
    ##
    ## Args:
    ##   sid: The WRF shortwave radiation model ID number (RA_SW_PHYSICS).
    ##
    ## Returns: The description and reference for the scheme.
    
    lookup = rbind(
        data.table(id=1,  name="Dudhia", ref="Dudhia_JAS_1989"))

    if(!(sid %in% lookup[, id])) {
        stop(paste("Microphysics information for ID", sid, "not found."))
    }

    return(lookup[id == sid])
}

WRFLongwave = function(sid) {
    ## Look up a WRF longwave radiation model ID and return its description and reference.
    ##
    ## Args:
    ##   sid: The WRF longwave radiation model ID number (RA_LW_PHYSICS).
    ##
    ## Returns: The description and reference for the scheme.
    
    lookup = rbind(
        data.table(id=1,  name="RRTM", ref="Mlawer_JGRA_1997"))

    if(!(sid %in% lookup[, id])) {
        stop(paste("Microphysics information for ID", sid, "not found."))
    }

    return(lookup[id == sid])
}

WRFSurfaceLayer = function(slid) {
    ## Look up a WRF microphysics ID and return its one-word description.
    ##
    ## Args:
    ##   slid: The WRF surface layer ID number (SF_SFCLAY_PHYSICS).
    ##
    ## Returns: The one-word description for the microphysics scheme.
    
    surfacelayer = rbind(
        data.table(id=1,  name="Revised MM5 Monin-Obukhov", ref="Jimenez_MWR_2012"))

    if(!(slid %in% surfacelayer[, id])) {
        stop(paste("Surface layer information for ID", slid, "not found."))
    }

    return(surfacelayer[id == slid])
}
    
WRFLandSurface = function(lsid) {
    ## Look up a WRF land surface physics model ID and return its one-word description.
    ##
    ## Args:
    ##   lsid: The WRF land surface model ID number (SF_SURFACE_PHYSICS).
    ##
    ## Returns: The one-word description for the land surface physics model.

    ## Ref from here: https://ral.ucar.edu/solutions/products/unified-noah-lsm.
    landsurface = rbind(
        data.table(id=2,  name="Noah", ref="Chen_MWR_2001"))
    
    if(!(lsid %in% landsurface[, id])) {
        stop(paste("Land surface model information for ID", lsid, "not found."))
    }

    return(landsurface[id == lsid])
}

WRFResolution = function(nc, fromProj=projLatLon(), toProj=projSwiss()) {
    ## Return WRF x and y average horizontal resolution.
    ##
    ## Args:
    ##   nc:  The WRF netCDF file, already open.
    ##   fromProj: Resolution of XLAT and XLONG in file (default: WGS84).   
    ##   toProj: Projection in which to find the resolution (default: Swisscoords).
    ##
    ## Returns: list with x and y average resolutions.
    
    ## Get latitudes and longitude values.
    lats = t(ncvar_get(nc, varid="XLAT"))
    lons = t(ncvar_get(nc, varid="XLONG"))
    rows = row(lats)
    cols = col(lats)
    lats = data.table(row=as.vector(rows), col=as.vector(cols), lat=as.vector(lats))
    lons = data.table(row=as.vector(rows), col=as.vector(cols), lon=as.vector(lons))

    ## Compile coordinates.
    setkey(lats, row, col)
    setkey(lons, row, col)
    coords = lats[lons]
    coords[, x := lon]
    coords[, y := lat]
    
    ## Transform from lat-long to proj in m.
    coordinates(coords) = ~x+y
    proj4string(coords) = fromProj
    coords = data.table(data.frame(spTransform(coords, toProj)))

    xres = round(coords[, diff(x), by=row][, mean(V1)], 0)
    yres = round(coords[, diff(y), by=col][, mean(V1)], 0)

    return(list(x=xres, y=yres))
}

WRFTerrainPlot = function(file, dir="nc", label="fig:domain",
                          script="~/git/stormtrack/code/ncl/plot_WRF_terrain_with_radars.ncl", 
                          caption=paste("Terrain height for points covered by the WRF",
                                        "simulation domain. Black lines show national",
                                        "borders and coastlines. The locations of the",
                                        "five Meteoswiss radars used by TRT are indicated",
                                        "with blue circled points, and the solid blue line shows",
                                        "the approximate radar domain and therefore the",
                                        "coverage of the TRT results. The dashed blue line",
                                        "shows the study domain."),
                          figname="terrain", ...) {
    ## Create and write latex for a WRF terrain plot.
    ## 
    ## Args:
    ##   file: A WRF file to get height data from.
    ##   dir: Directory in which to put the netCDF and plot files.
    ##   label: Label for latex output.
    ##   script: The NCL script to run.
    ##   caption: Caption for latex output.
    ##   width: Image width in latex output.
    ##   figname: Output figure name.
    ##   ...: Extra arguments to runNCL().
    ## 
    ## Returns: void.

    ## Produce the terrain plot.
    runNCL(args=paste("wrfFile=\'\"", file, "\"\' ",
                      "outName=\'\"", figname, "\"\' ",
                      script, sep=""), dir=dir,
           label=label, caption=caption, figname=figname, ...)
}

WRFSubdomainPlot = function(file, dir="nc", label="fig:subdomains",
                         script="~/git/stormtrack/code/ncl/plot_subdomains.ncl", 
                         caption="Subdomains used in this study (blue lines).",
                         figname="subdomains", ...) {
    ## Create and write latex for a WRF terrain plot.
    ## 
    ## Args:
    ##   file: A WRF file to get height data from.
    ##   dir: Directory in which to put the netCDF and plot files.
    ##   label: Label for latex output.
    ##   script: The NCL script to run.
    ##   caption: Caption for latex output.
    ##   width: Image width in latex output.
    ##   figname: Output figure name.
    ##   ...: Extra arguments to runNCL().
    ## 
    ## Returns: void.

    ## Update the domain shapefile (in the default location).
    writeSwissZonesShapefile(dir=dir)
    
    ## Produce the terrain plot.
    runNCL(args=paste("wrfFile=\'\"", file, "\"\' ", script, sep=""), dir=dir,
           label=label, caption=caption, figname=figname, ...)
}

WRFInfoTable = function(WRFInfo, caption="Schemes used in the WRF model in this study.",
                        label="tab:WRFSchemes", cite=TRUE, tableEnv="table",
                        captionAbove=TRUE, booktabs=TRUE) {
    ## Print a LaTeX table with information about WRF metadata (which schemes were used etc).
    ##
    ## Args:
    ##   WRFInfo: object returned by WRFMetadata().
    ##   caption, label: LaTeX caption and label.
    ##   cite: Include references? (default: TRUE).
    ##   tableEnv: Table environment to use (default: "table").
    ##   captionAbove: Put the caption above the table? (default: TRUE).
    ##   booktabs: Use booktabs separators (rules)? (default: TRUE).
    ##
    ## Returns: void.

    stopifnot(identical(WRFInfo$explicitConvection, TRUE))

    writeCaption = function() {
        cat(paste("\\caption{", caption, "}\n", sep=""))
        cat(paste("\\label{", label, "}\n", sep=""))
    }
    
    cat(paste("\\begin{", tableEnv, "}[t]\n", sep=""))
    if(captionAbove)
        writeCaption()

    cat("\\centering\n")
    cat("\\begin{tabular}{ll}")
    if(booktabs)
        cat("\\toprule\n")
    
    cat("Configuration option & Scheme used \\\\ \n")

    if(booktabs)
        cat("\\midrule\n")
    else
        cat("\\hline\n")
    
    cat(paste("Boundary layer scheme & ", WRFInfo$boundaryLayerName, sep=""))
    if(cite)
        cat(paste(" \\citep{", WRFInfo$boundaryLayerRef, "}", sep=""))
    cat("\\\\ \nCumulus parameterisation & None (explicit convection) \\\\ \n")
    cat(paste("Shortwave radiation scheme & ", WRFInfo$shortwaveName, sep=""))
    if(cite)
        cat(paste(" \\citep{", WRFInfo$shortwaveRef, "}", sep=""))
    cat(paste("\\\\ \nLongwave radiation scheme & ", WRFInfo$longwaveName, sep=""))
    if(cite)
        cat(paste(" \\citep{", WRFInfo$longwaveRef, "}", sep=""))
    cat(paste("\\\\ \nLand surface scheme & ", WRFInfo$landSurfaceName, sep=""))
    if(cite)
        cat(paste(" \\citep[][]{", WRFInfo$landSurfaceRef, "}", sep=""))
    cat(paste("\\\\ \nSurface layer model & ", WRFInfo$surfaceLayerName, sep=""))
    if(cite)
        cat(paste(" \\citep{", WRFInfo$surfaceLayerRef, "}", sep=""))
    cat("\\\\ \nHail model & HAILCAST ")
    cat(" \\citep{Adams-Selin_MWR_2016}")
    cat("\\\\\n")
    if(booktabs)
        cat("\\bottomrule\n")

    cat("\\end{tabular}\n")

    if(!captionAbove)
        writeCaption()
    cat(paste("\\end{", tableEnv, "}\n", sep=""))
}

WRFValuesForRasters = function(time, dir, areas, vars=c("RAINC"), fun=mean) {

    ## Args:
    ##   time: The time for which to search.
    ##   dir: WRF files directory.
    ##   areas: A set of rasters.
    ##   vars: Variables to return.
    ##   fun: Aggregation function to be run over all values per raster area (default: mean).
    
    wrf = openWRFFile(time=time, dir=dir)
    numy = ncatt_get(nc=wrf, varid=0, attname="SOUTH-NORTH_GRID_DIMENSION")$value
    numx = ncatt_get(nc=wrf, varid=0, attname="WEST-EAST_GRID_DIMENSION")$value
    
    vals = readWRF(time=time, dir=dir, vars=vars, outputProj=projLatLon())
    vals = vals[, c("coordX", "coordY", vars), with=FALSE]
    coordinates(vals) = ~coordX+coordY
    proj4string(vals) = projLatLon()
    
    valsraster = raster(xmn=min(vals$coordX), xmx=max(vals$coordX),
                        ymn=min(vals$coordY), ymx=max(vals$coordY),
                        nrows=numy, ncols=numx, crs=projLatLon())

    valsraster = rasterize(vals, valsraster, )
    
    plot(valsraster)
    
    dateFormat="%Y-%m-%d_%H:%M:%S"
    pattern = strftime(time, tz="UTC", format=dateFormat)
    file = list.files(dir, pattern=paste(".*", pattern, ".*", sep=""), 
                      full.names=TRUE)
    if(length(file) != 1) {
        stop(paste("Error finding file for time", time))
    }

}

WRFSettingsMatch = function(set1, set2, withoutRes=TRUE, withoutHailcast=TRUE) {
    ## Return whether two sets of results from WRFInfo() are the same and produce a warning if not.
    ##
    ## Args:
    ##  set1, set2: The two sets to compare.
    ##  withoutRes: Don't compare resolution if TRUE (default: TRUE).
    ##  withoutHailcast: Don't expect hailcast to match (default: TRUE).
    ##
    ## Returns: void.

    cols = names(set1)
    cols = cols[which(cols != "exampleFile")]

    if(withoutRes) {
        cols = cols[which(cols != "xAvRes")]
        cols = cols[which(cols != "yAvRes")]
    }

    if(withoutHailcast)
        cols = cols[which(cols != "hailcast")]
    
    res = all.equal(set1[, cols, with=FALSE],
                    set2[, cols, with=FALSE])
    if(!(identical(res, TRUE))) {
        warning("Differences between two WRF settings objects:")
        warning(res)
    }
}

WRFRegriddedInfo = function(dir, dp=4) {
    ## Get information on the grid used by a regridded WRF file.
    ##
    ## Args:
    ##   dir: The directory in which to look for files (the first file is used).
    ##   dp: Number of decimal places to round resolution to (default: 4).
    ##
    ## Returns: data.table of information.
    
    file = list.files(dir, full.name=TRUE)[1]
    nc = nc_open(file)

    alts = ncvar_get(nc=nc, varid="interp_levels")
    lats = ncvar_get(nc=nc, varid="lat")
    lons = ncvar_get(nc=nc, varid="lon")

    resX = unique(round(diff(lons), dp)) ## deg. 
    resY = unique(round(diff(lats), dp)) ## deg.
    resZ = unique(diff(alts)) ## km.

    stopifnot(length(resX) == 1)
    stopifnot(length(resY) == 1)
    stopifnot(length(resZ) == 1)
    
    nc_close(nc)

    res = data.table(
        dX=resX,
        dY=resY,
        dZ=resZ,
        minX=min(lons),
        maxX=max(lons),
        minY=min(lats),
        maxY=max(lats),
        minZ=min(alts),
        maxZ=max(alts))

    return(res)
}

allWRFMetadata = function(dirs, names, WRFDir="wrf/", WRFNestDir="wrf_nest/",
                          wrfPattern="wrfout*", nestPattern="wrfout_d01*", expectedRes=300) {
    ## Get metadata for multiple sets of WRF outputs; concatenate microphysics model
    ## differences but expect no other differences between data sets.
    ##
    ## Args:
    ##   dirs, names: WRF base directories and a name for each.
    ##   WRFDir: sub-directory for WRF output files (inner domain).
    ##   WRFNestDir: sub-directory for WRF outer domain outputs.
    ##   nestPattern: Pattern to match outer domain files.
    ##
    ## Returns: inner and outer domain metadata for each WRF output set. Raises an warnings
    ##          if settings other than microphysics schemes differ between the sets.

    stopifnot(length(dirs) == length(names))
    WRFInfo = NULL
    WRFNestInfo = NULL
    
    for(i in seq(1, length(dirs))) {
        info = WRFMetadata(WRFDir=paste(dirs[i], WRFDir, sep="/"),
                           WRFPattern=wrfPattern, expectedRes=expectedRes)
        nestInfo = WRFMetadata(WRFDir=paste(dirs[i], WRFNestDir, sep="/"),
                               WRFPattern=nestPattern, expectedRes=expectedRes)

        ## Raise warnings if the inner and outer domain do not match.
        WRFSettingsMatch(set1=info, set2=nestInfo)

        info[, simname := names[i]]
        nestInfo[, simname := names[i]]
        WRFInfo = rbind(WRFInfo, info)
        WRFNestInfo = rbind(WRFNestInfo, nestInfo)
    }

    ## Microphysics should be the same from inner to outer.
    if(!all.equal(WRFInfo[, microphysicsName, by=simname],
                 WRFNestInfo[, microphysicsName, by=simname]))
        warning("Microhysics differs between inner and outer WRF domains.")
    
    ## Find which columns contain more than one value.
    f = function(x) { return(length(unique(x))) }
    repcols_inner = melt(WRFInfo[, lapply(.SD, f)], measure.vars=names(WRFInfo))
    repcols_outer = melt(WRFNestInfo[, lapply(.SD, f)], measure.vars=names(WRFNestInfo))
    
    repcols = rbind(data.table(domain="inner", repcols_inner),
                    data.table(domain="outer", repcols_outer))
    repcols = repcols[value > 1]
    
    ## Expect differences in certain columns.
    repcols = repcols[!(variable %in% c("missingTimes", "exampleFile", "simname",
                                        "microphysicsName", "microphysicsRef"))]
    repcols = repcols[, unique(variable)]
    if(length(repcols) != 0)
        warning(paste("WRF metadata differed between simulations for:",
                      paste(repcols, collapse=", ")))

    return(list(inner=WRFInfo, outer=WRFNestInfo))
}

quoteMicrophysicSchemes = function(info) {
    ## Quote all microphysics schemes in a WRFInfo object, with citations.
    ##
    ## Args:
    ##   info: A WRFInfo object.
    ##
    ## Returns: string with "scheme (cite) and scheme (cite)" etc.
    
    str = ""
    for(i in seq(1, nrow(info))) {
        str = paste(str, "the ", info[i, microphysicsName], " scheme ",
                    " \\citep{", info[i, microphysicsRef], "}", sep="")
        if(i < nrow(info)-1)
            str = paste(str, ", ", sep="")
        if(i == nrow(info)-1)
            str = paste(str, "and ")
    }

    return(str)
}

allWRFRegriddedInfo = function(dirs, subdir="wrf_regridded/", dp=4) {
    ## Get regridded WRF file information for multiple simulation sets.
    ##
    ## Args:
    ##   dirs: Simulation base directories.
    ##   names: Simulation names.
    ##   subdir: Regridded WRF file sub-directory name.
    ##   dp: Number of decimal places to round to.
    ##
    ## Returns: information on regridded data.
    
    info = NULL
    for(dir in dirs) {
        info = rbind(info,
                     data.table(WRFRegriddedInfo(paste(dir, subdir, sep="/"), dp=dp)))
    }

    ## info should only have one row, because all regridded data should have the
    ## same properties.
    stopifnot(nrow(unique(info)) == 1)
    return(unique(info))
}

getWRFHailProperties = function(wrfDir, cells, cellProj) {
    ## Collect hail properties for cell locations, from WRF files.
    ##
    ## Args:
    ##   wrfDir: Directory in which to look for WRF files.
    ##   cells: Cell information, with at least trackID, timestamp, x, y.
    ##   cellProj: The projection for cell coordinates.
    ##
    ## Returns: hail information as a data.table.

    stopifnot(c("x", "y", "timestamp", "trackID") %in% names(cells))
    
    ## cl = makeCluster(detectCores(), outfile="")
    ## registerDoParallel(cl)   
    ## setkey(cells, timestamp)

    allProps = list()

    ## Make a blank raster the same size and resolution as the cells.
    cellPoints = unique(cells[, list(x, y)])
    coordinates(cellPoints) = ~x+y
    proj4string(cellPoints) = cellProj
    gridded(cellPoints) = TRUE
    blankRaster = raster(cellPoints)
    
    for(time in cells[, unique(timestamp)]) {
        propsForTime = hailPropsForTime(cells=cells[timestamp == time],
                                        wrfDir=wrfDir, cellProj=cellProj,
                                        blankRaster=blankRaster)
        allProps = c(allProps, list(propsForTime))
    }

    hail = rbindlist(allProps)
    return(hail)
}

hailPropsForTime = function(cells, wrfDir, cellProj, blankRaster) {
    ## Collect hail properties for cell locations, from a single WRF files (for one time).
    ##
    ## Args:
    ##   cells: Cell information, with at least trackID, timestamp, x, y; for one time only.
    ##   wrfDir: Directory in which to look for WRF files.
    ##   cellProj: The projection for cell coordinates.
    ##
    ## Returns: hail information as a data.table.
        
    time = cells[, unique(timestamp)]
    stopifnot(length(time) == 1)
    
    print(paste("Getting hail information for time:", time))
        
    ## Get the WRF file for this timestep, use it to get hail sizes.
    filePattern = strftime(format="*%y-%m-%d_%H:%M:%S*", tz="UTC", time)
    wrfFile = list.files(wrfDir, pattern=filePattern, full.name=TRUE)
    wrf = nc_open(filename=wrfFile)    
    
    hailsizes = data.table(
        timestamp=as.numeric(as.POSIXct(ncvar_get(nc=wrf, varid="Times"), tz="UTC",
                                        format="%Y-%m-%d_%H:%M:%S")),
        long=as.numeric(ncvar_get(nc=wrf, varid="XLONG")),
        lat=as.numeric(ncvar_get(nc=wrf, varid="XLAT")))
    
    if(is.null(wrf$var[["HAILCAST_DIAM_MAX"]])) {
        warning(paste("No HAILCAST found in ", wrfFile))
        hailsizes[, maxHailSize := NA]
    } else {
        hailsizes[, maxHailSize :=
                        as.numeric(ncvar_get(nc=wrf, varid="HAILCAST_DIAM_MAX"))]
    }
    nc_close(wrf)

    ## Make the times from the NC file a proper time string; check it matches "time".
    hailsizes[, timestamp := as.POSIXct(timestamp, tz="UTC", origin="1970-1-1")]
    stopifnot(identical(hailsizes[, unique(timestamp)], time))
        
    ## Make a spatial representation of the points for which we have hail info.
    hailsizes_sp = copy(hailsizes)
    coordinates(hailsizes_sp) = ~long+lat
    proj4string(hailsizes_sp) = projLatLon()
    
    ## Subset the cells to those for this timestep.
    tscells = cells[timestamp == time, list(x, y, trackID)]

    ## Make a lookup table of unique IDs for this timestep to trackID values.
    cellProps = tscells[, list(trackID=unique(trackID))]
    cellProps[, cellID := seq(1, nrow(cellProps))]
    setkey(cellProps, trackID)

    ## Assign unique cell IDs to all cells; this is so cellID is numeric.
    setkey(tscells, trackID)
    tscells[, cellID := cellProps[tscells, cellID]]
    tscells = tscells[, list(x, y, cellID)]
    
    ## Make the cell locations a spatial object with x, y, cellID.
    coordinates(tscells) = ~x+y
    proj4string(tscells) = cellProj
    cellsRaster = rasterize(tscells, blankRaster)[["cellID"]]
    
    ## Assign the cellID for every hail information point, and subset to those
    ## points inside cells.
    hailsizes[, cellID := extract(cellsRaster, hailsizes_sp)]
    hailsizes = hailsizes[!is.na(cellID)]
    
    ## Use the lookup table to assign trackIDs to each hail information point.
    setkey(hailsizes, cellID)
    setkey(cellProps, cellID)
    hailsizes[, trackID := cellProps[hailsizes, trackID]]
    hailsizes[, cellID := NULL]
    
    ## Make statistics to compare to TRT.
    hailProp = hailsizes[, list(maxSize=max(maxHailSize),
                                NPixHailgt25mm=length(which(maxHailSize > 25)),
                                NPixHailgt30mm=length(which(maxHailSize > 30)),
                                NPixHailgt35mm=length(which(maxHailSize > 35)),
                                NPixHailgt40mm=length(which(maxHailSize > 40)),
                                NPixHailgt45mm=length(which(maxHailSize > 45)),
                                NPixHailgt50mm=length(which(maxHailSize > 50)),
                                NPixHailgt55mm=length(which(maxHailSize > 55)),
                                NPixHailgt60mm=length(which(maxHailSize > 60)),
                                nPixNA=length(which(is.na(maxHailSize))),
                                nPix=length(maxHailSize)), by=c("timestamp", "trackID")]

    stopifnot(hailProp[, !any(duplicated(trackID))])
    return(hailProp)
}
