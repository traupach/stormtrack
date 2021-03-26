## proj_definitions.R
## Definitions of map projections.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

## Note that with GDAL 3 and PROJ >= 6, these proj4strings generate
## warnings about dropped keys (especially datums). According to the
## document at
## https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/ it
## is safe to ignore these warnings which are due to coding changes in
## the rgdal and sp libraries. Here I follow the guidelines to simply
## use EPSG codes.

require(rgdal)
require(sp)

projLatLon = function() {
    ## WGS84 latitude/longitude projection.
    ## +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
    return(CRS(SRS_string="EPSG:4326"))
}

projSwiss = function() {
    ## Swiss CH1903+/LV95 (note, not CH1903/LV03 as used in CombiPrecip).
    ## +proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000
    ## +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs
    return(CRS(SRS_string="EPSG:2056"))
}

projCH1903 = function() {
    ## Swiss CH1903/LV03 projection, as used in CombiPrecip (note, projection superceded by CH1903+,
    ## projSwiss()).
    ## +proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000
    ## +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs
    return(CRS(SRS_string="EPSG:21781"))
}
