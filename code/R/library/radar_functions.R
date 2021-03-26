## radar_functions.R
## Functions for dealing with (primarily MCH) radar data in R.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

require(stringr)
require(data.table)
require(ncdf4)

freqToWavelength = function(freq, sol=299792458) {
    ## Convert frequency in Hz to wavelength in cm.
    ## 
    ## Args:
    ##   freq: Frequency in Hz [s-1].
    ##   sol: Speed of light [m s-1].
    ##
    ## Returns: Wavelength [cm].

    wlength = sol / freq    ## [m]
    wlength = wlength * 100 ## [cm]
    return(wlength)
}

radarBand = function(freq) {
    ## Return IEEE standard radar band name, from frequency.
    ##
    ## Args:
    ##  freq: Frequency in Hz [s-1].
    ##
    ## Returns: associated band name.

    ## Convert to GHz.
    freq = freq / 1e9
    
    ## Return band name.
    if(freq >= 2 & freq < 4)
        return("S-band")
    if(freq >= 4 & freq < 8)
        return("C-band")
    if(freq >= 8 & freq < 12)
        return("X-band")
    if(freq >= 12 & freq < 18)
        return("Ku-band")
    if(freq >= 18 & freq < 27)
        return("K-band")
    if(freq >= 27 & freq < 40)
        return("Ka-band")

    stop("radarBand: Frequency is of unknown band.")
}

radarInfoTable = function(info, lab="tab:radarInfo",
                          cap=paste("Details of the MeteoSwiss radars used, including",
                                    "their longitude (Lon) and latitude (Lat) coordinates,",
                                    "altitude above sea level (Alt), and radar wavelength",
                                    "($\\lambda$)."), booktabs=TRUE, captionAbove=TRUE, ...) {
    ## Print radar information to a latex table.
    ##
    ## Args:
    ##   info: perRadarInfo from radarMetadata().
    ##   cap, lab: Caption and label for the xtable.
    ##   booktabs: Use booktabs? (default: TRUE).
    ##   captionAbove: Put the caption above the table? (default: TRUE).
    ##   ...: Extra arguments for print.xtable().
    ## 
    ## Returns: void.
    
    info = copy(info)

    info = info[, list(radarName, radarLon, radarLat, radarAltitude, wavelength)]
    names(info) = c("Radar name", "Lon [$^{\\circ}$]", "Lat [$^{\\circ}$]", "Alt [m]",
                    "$\\lambda$ [cm]")

    capPlacement = "bottom"
    if(captionAbove == TRUE)
        capPlacement = "top"
    
    print(xtable(info, caption=cap, label=lab, digits=c(0,0,5,5,0,2)), booktabs=booktabs,
          sanitize.text.function=identity, include.rownames=FALSE,
          caption.placement=capPlacement, table.placement="t", ...)
}
