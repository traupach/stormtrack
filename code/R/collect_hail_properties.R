## collect_hail_properties.R
##
## Collect hail properties for each TITAN track, using WRF data, and save them
## as an .Rdata file.
## 
## Usage: run using Rscript from a simulation base directory.
##
## Tim Raupach 2019.

rm(list=ls())
wd = getwd()
setwd("~/git/stormtrack/code/R/library/")
for(file in list.files(pattern="*.R"))
    source(file)
setwd(wd) 
rm(list=c("wd", "file"))

stopifnot(dir.exists("wrf"))
stopifnot(dir.exists("tracks"))

print(paste("Getting hail properties in dir:", getwd()))

## Find track file, process all tracks in the file.
trackFile = list.files("tracks", pattern="*.csv", full.name=TRUE)
stopifnot(length(trackFile) == 1)

print(paste("Opening track file", trackFile))
tracks = readTITANTracks(trackFile=trackFile)

print("Calculating cell outline information...")
cells = getTITANCells(dat=tracks)
rm(list=c("tracks"))

## Collect hail properties.
print("Getting hail properties from WRF input files...")
hailProps = getWRFHailProperties(wrfDir="wrf", cells=cells$cells, cellProj=cells$proj)

## Write it out.
print("Writing results to tracks/track_hail_data.rds")
saveRDS(object=hailProps, file="tracks/track_hail_data.rds")
