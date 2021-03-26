## basic_functions.R
## Basic helper functions in R.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(shape)
require(scales)
require(english)
require(ggplot2)

label_alpha = function(x) {
  ## Labeller for facet plots that adds (a), (b) etc to facet labels.
  x[,1] = paste('(', letters[1:length(x[,1])], ') ', x[,1], sep="")
  return(x) 
}

label_alpha_parsed = function(x) {
  ## Version of label_alpha with strings parsed as expressions.
  x[,1] = paste('(', letters[1:length(x[,1])], ')~', x[,1], sep="")
  return(label_parsed(x))
}

inRange = function(x, vec) {
  ## Return whether a number is within the range of a value set.
  ## 
  ## Args:
  ##   x: The value to test.
  ##   vec: Vector of values to test against.
  ## 
  ## Return: TRUE if x is within the range of vec (inclusive), 
  ##         FALSE otherwise.
  
  return(x <= max(vec) & x >= min(vec))
}

roundToNearest = function(x, n) {
  ## Round numbers to the nearest "n" (e.g. nearest 100).
  ##  
  ## Args:
  ##   x: numbers to round.
  ##   n: precision of rounding (n=100 for nearest 100).
  ## 
  ## Returns: rounded numbers.
  
  return(round(x/n, 0)*n)
}

meanDb = function(x, na.rm=FALSE) {
    ## Find the linear mean of values in dB.
    ## 
    ## Args:
    ##   x: Values in dB.
    ##   na.rm: Remove NA values before mean?
    ## 
    ## Returns: the linear mean, converted back to dB.
  
  vals = 10^(x/10)
  return(10*log10(mean(vals, na.rm=na.rm)))
}

meanAngle = function(x, w=NULL, na.rm=FALSE) {
  ## Calculate the mean of circular angles.
  ## 
  ## Args:
  ##   x: Angles over which to take the mean.
  ##   w: Weights to use per angle.
  ## 
  ## Returns: The mean angle.
  
  ## Convert to radians.
  rad = pi/180 * x
  
  ## If no weights specified, do not weight.
  if(is.null(w)) 
    w = rep(1, length(x))
  stopifnot(length(w) == length(x))
  
  ## Find mean cosines and sines of each angle.
  meanSin = stats::weighted.mean(sin(rad), w=w, na.rm=na.rm)
  meanCos = stats::weighted.mean(cos(rad), w=w, na.rm=na.rm)
  
  res = atan(meanSin/meanCos) / (pi/180)
  
  if(meanCos < 0)
    res = res + 180
  if(meanSin < 0 & meanCos > 0)
    res = res + 360
  
  return(res)
}

angleNames = function(angles, min=0, max=360, includeSub=TRUE) {
    ## Assign compass point names to angles. 
    ##
    ## Args:
    ##    angles: The angles to process.
    ##    min, max: Minimum and maximum values in the angles.
    ##    includeSub: Include NNE, ESE, etc? (Default: TRUE).
    ## 
    ## Angles are wrapped cyclically, so for example if max is 180, then angles from 168.75 to
    ## 180 degrees will be counted in the FIRST class (0 to 11.5 degrees). 
    ## 
    ## Returns: data.table with angles and the name of each one's compass point.
    
    res = data.table(angle=angles)

    if(includeSub)
        binNames = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                     "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
    else
        binNames = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    
    binwidth = 360/length(binNames)
    
    angleclasses = seq(0, 360, by=binwidth) - binwidth/2
    
    stopifnot(all(angles >= min))
    stopifnot(all(angles <= max))
    stopifnot(length(binNames) == length(angleclasses)-1)
    
    classmax = max(angleclasses[which(angleclasses < max)])
    res[, modifiedAngle := angle]
    res[angles > classmax, modifiedAngle := angle - max]
    res[, angleClass := cut(modifiedAngle, breaks=angleclasses, labels=FALSE)]
    res[, angleName := factor(binNames[angleClass], levels=binNames)]
    
    return(res[, list(angle, angleName)])
}

ellipsePoints = function(x, y, minor, major, rotation, nPoints=50) {
  ## Calculate points on an ellipse.
  ## 
  ## Args:
  ##   x: Centre point x coordinate.
  ##   y: Centre point y coordinate.
  ##   minor: Semi-minor axis length.
  ##   major: Semi-major axis length.
  ##   rotation: Counter-clockwise rotation in degrees. 
  ##   nPoints: Number of points to include in the ellipse (default: 50).
  ## 
  ## Returns: data.table with x, y coordinate values for the ellipse.

  ## Following this derivation of a parametric rotated ellipse:
  ## https://math.stackexchange.com/questions/941490/
  ## 
  ## An ellipse at (0,0), with major axis aligned with the X axis.
  ## 
  ## x(rads) = major * cos(rads)
  ## y(rads) = minor * sin(rads)
  ## 
  ## Which can be written
  ##  
  ## | x |   | major 0     |   | cos(rads) |   | major*cos(rads) |
  ## | y | = | 0     minor | * | sin(rads) | = | minor*sin(rads) |
  ## 
  ## Multiplying by the rotation matrix with angle theta,
  ##
  ## | x |   | cos(theta) -sin(theta) |   | major*cos(rads) |
  ## | y | = | sin(theta)  cos(theta) | * | minor*sin(rads) |
  ##
  ## Which works out as
  ## 
  ## x = major*cos(rads)*cos(theta) - minor*sin(rads)*sin(theta)
  ## y = major*cos(rads)*sin(theta) + minor*sin(rads)*cos(theta)
  ##
  ## In this case the angle is for counter-clockwise rotation from the 
  ## major (X) axis. So 90 - theta gives the angle to rotate clockwise
  ## from the Y axis (north).
  
  ## Calculate angles.
  theta = (90 - rotation) * pi/180
  rads = seq(0, 2*pi, length.out=nPoints) 
  
  ## Calculate coordinates.
  ellipseCoords = data.table(xCoord=major*cos(rads)*cos(theta) - 
                               minor*sin(rads)*sin(theta) + x,
                             yCoord=major*cos(rads)*sin(theta) +
                               minor*sin(rads)*cos(theta) + y)

  return(ellipseCoords)
}

arrowOffsets = function(x, y, direction, scales, lengths=c(0.05, 0.5)) {
  ## Determine the coordinates of arrow start and end points.
  ## 
  ## Args:
  ##  x, y: Start coordinates.
  ##  direction: Directions for each arrow in degrees clockwise from north.
  ##  scales: Scaling factor for each arrow length.
  ##  lengths: The range of output arrow lengths in coordinate units.
  ## 
  ## Returns: data.table with xStart, yStart, xEnd, yEnd.
  
  lengths = rescale(scales, lengths)
  rads = direction * pi/180
  
  res = data.table(xStart=x, yStart=y, 
                   xEnd=x+lengths*sin(rads),
                   yEnd=y+lengths*cos(rads))
    
  return(res)
}

dtSummary = function(dat, col) {
    ## Summarise data into a data.table.
    ## 
    ## Args:
    ##   dat: The data.table to summarise.
    ##   col: The column to summarise.
    ##
    ## Returns: the summary information as a data.table.

    dat = dat[[col]]
    return(data.table(min=min(dat), q25=quantile(dat, probs=0.25), mean=mean(dat),
                      median=quantile(dat, probs=0.5), q75=quantile(dat, probs=0.75),
                      max=max(dat)))
}

summaryTable = function(dat, col, byCol="set", byColName="Dataset",
                        cap=paste("Summary for ", col, ".", sep="")) {
    ## Print a summary table for data by a divisor.
    ##
    ## Args:
    ##   dat: The data.table to use.
    ##   col: Name of the column to summarise.
    ##   byCol: Column to divide by.
    ##   byColName: The name to give the dividor in the results.
    ##
    ## Returns: void.

    sum = dat[, dtSummary(.SD, col), by=byCol, .SDcols=col]
    names(sum) = c(byColName, "Min", "Q25", "Mean", "Median", "Q75", "Max")
    print(xtable(sum, label=paste("tab:", col, sep=""), caption=cap), include.rownames=FALSE)
}

secondsToMins = function(secs) {
    ## Convert seconds to minutes and return the number of minutes in English words.
    ##
    ## Args:
    ##  secs: Seconds to convert (numeric).
    ##
    ## Returns: English words for the number of minutes.

    mins = secs / 60
    return(as.character(as.english(mins)))
}

compNumbers = function(test, than, what=NULL) {
    ## Test a quantity against a reference and return a comparison word and amount of difference in
    ## English.
    ##
    ## Args:
    ##   test: The quantity to test.
    ##   than: The reference amount.
    ##
    ## Returns: "more" if test > than, "fewer" if test < than, and "the same number of" if test ==
    ## than. More and fewer give a relative difference as a percent.

    diff = test - than
    relDiff = abs(round(diff / than * 100, 0))
    
    if(test > than)
        return(paste(relDiff, "\\% more", what, sep=""))
    if(test < than)
        return(paste(relDiff, "\\% fewer", what, sep=""))
    if(test == than)
        return(paste("the same number of", what))
}

printTable = function(tab, cap, lab, captionAbove=TRUE, size=NULL, useBooktabs=TRUE, tabEnv="table", centre=TRUE) {
    ## Print out a table made using tabular(), with caption and reference label.
    ##
    ## Args:
    ##   tab: The table, formulated by tabular().
    ##   cap: Caption to use.
    ##   lab: Label to use.
    ##   captionAbove: Put the caption above the table? (default: TRUE).
    ##   size: Latex font size to use (default: NULL, don't specify).
    ##   useBooktabs: Use booktabs (see ?latex)?
    ##   centre: Centre the table?
    ## 
    ## Returns: void.

    printCap = function() {
        cat(paste("\\caption{", cap, "}\n", sep=""))
        cat(paste("\\label{", lab, "}\n", sep=""))
    }

    tabs = table_options()
    if(useBooktabs == TRUE)
        booktabs()
    
    cat(paste("\\begin{", tabEnv, "}[t]\n", sep=""))
    if(centre)
        cat("\\centering")
    if(!is.null(size))
        cat(paste("\\", size, "\n", sep=""))
    if(captionAbove)
        printCap()
    print(toLatex(tab))
    if(!captionAbove)
        printCap()
    cat(paste("\\end{", tabEnv, "}\n", sep=""))

    table_options(tabs)
}

moreOrLess = function(test, than, what=NULL) {
    ## Test whether 'test' is larger, equal, or smaller than 'than' and return a word that describes
    ## the relationship.
    
    if(test > than)
        return(paste("more", what))
    if(test < than)
        return(paste("less", what))
    if(test == than)
        return(paste("the same number of", what))
}

setComparison = function(ref, res, col, key, groupBy, missing=NA) {
    ## Compare two sets of results and produce comparison statistics.  Note that reference values
    ## equal to zero will not be counted in relative statistics but will be counted in the other
    ## statistics.
    ##
    ## Args:
    ##   res: result data.
    ##   ref: reference ("truth") data.
    ##   col: the data column to compare.
    ##   key: the column by which to key/order the data.
    ##   groupBy: a column to group by.
    ##   missing: Value to set missing values to (kept missing by default).
    ## 
    ## Returns: the statistics, and a table to print. 

    ref = copy(ref)
    res = copy(res)
    setkeyv(ref, c(key, groupBy))
    setkeyv(res, c(key, groupBy))

    stopifnot(!("ref" %in% names(ref)))
    stopifnot(!("res" %in% names(res)))
    setnames(ref, col, "ref")
    setnames(res, col, "res")

    stopifnot(!any(duplicated(res, by=c(key, groupBy))))
    stopifnot(!any(duplicated(ref, by=c(key, groupBy))))
    
    ref = ref[, c(key, "ref", groupBy), with=FALSE]
    res = res[, c(key, "res", groupBy), with=FALSE]
    
    comp = merge(res, ref, all=TRUE)
    comp[is.na(res), res := missing]
    comp[is.na(ref), ref := missing]
    
    comp[, diff := res - ref]
    comp[, relDiff := diff / ref * 100]
    
    ## Relative differences only defined for non-zero reference values.
    comp[ref == 0, relDiff := NA]
    
    stats = comp[, list(bias=mean(diff),
                        RMSE=sqrt(mean(diff^2)),
                        meanRB=mean(relDiff, na.rm=TRUE),
                        medRB=median(relDiff, na.rm=TRUE),
                        RBIQR=IQR(relDiff, na.rm=TRUE),
                        r2=cor(ref, res)^2), by=groupBy]
    
    setkeyv(stats, groupBy)
    return(list(stats=stats, raw=comp))
}

writeNumber = function(x, capitalise=FALSE) {
    ## Write a number in words if less than 10, or leave as-is if not.
    require(english)
    require(Hmisc)

    words = as.character(english(x, UK=TRUE))
    if(capitalise)
        words = capitalize(words)

    res = rep(NA, length(x))
    res[x >= 10] = as.character(x[x >= 10])
    res[x < 10] = words[x < 10]
    stopifnot(!any(is.na(res)))
    return(res)
}
