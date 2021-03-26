#!/bin/bash

## Conduct a complete analysis of WRF outputs, using TITAN for storm tracking.
## Before running this script, run prep_WRF_for_TITAN.sh to regrid WRF files to MDV format,
## and put a valid TITAN parameter file in "params/Titan.opt".
## Tim Raupach, 2019.

## Settings ----------------------------------------------------------------------------------------
TITAN_BIN="$HOME/software/lrose/bin/"       ## Location of unmodified TITAN binaries.
MOD_TITAN="$HOME/git/stormtrack/utils/"     ## Parent location for modified TITAN programs.
## -------------------------------------------------------------------------------------------------

## Check that mdv and log directories already exist.
if [ ! -d mdv ]; then
   echo "ERROR: A directory called 'mdv' is required.";
   exit;
fi;

## Make a directory for log files.
if [ ! -d log ]; then
    echo "ERROR: A directory called 'log' is required.";
    exit;
fi;

if [ ! -e params/Titan.opt ]; then
    echo "ERROR: No TITAN parameter file was found in params/Titan.opt.";
    exit;
fi;

## 1. Run TITAN on the MDV files to track storms.
if [ ! -d storms ]; then
    ## Determine start and end times. 
    starttime=`ls mdv | egrep [0-9]{8} | head -n 1`;
    starttime="${starttime:0:4} ${starttime:4:2} ${starttime:6:2} 0 0 0";
    endtime=`ls mdv | egrep [0-9]{8} | tail -n 1`;
    endtime="${endtime:0:4} ${endtime:4:2} ${endtime:6:2} 23 59 59";
    
    dstr=`date -u +"%Y-%m-%d_%H:%M:%S_UTC"`;
    logfile="log/Titan_$dstr.log";
    
    echo "Running TITAN to track storms [logged in $logfile]...";
    echo "Tracking from $starttime to $endtime".
    stdbuf -oL $TITAN_BIN/Titan -params params/Titan.opt -start "$starttime" \
	   -end "$endtime" &> $logfile;
else
    echo "Not overwriting TITAN storm tracking output in 'storms'; skipping this step.";
fi;

## 2. Process storm tracks to get CSV files.
if [ ! -d tracks ]; then
    dstr=`date -u +"%Y-%m-%d_%H:%M:%S_UTC"`;
    logfile="log/stormStats_$dstr.log";
    compLogFile="log/stormStats_compile_$dstr.log";

    echo "Compiling stormStats...";
    bash -c "cd $MOD_TITAN/stormStats; source set_build_env.sh; make install" > $compLogFile 2> $compLogFile;
    
    echo "Processing TITAN output to CSV track info [logged in $logfile]...";
    stdbuf -oL $TITAN_BIN/stormStats -params params/stormStats.opt \
	   -f storms/*.sd5 &> $logfile;
    gzip tracks/*.csv;
else
    echo "Not overwriting track CSV files in 'tracks'; skipping this step.";
fi;

## Check for errors and warnings in the log files. Print the warnings and errors if they exist.
numwarnings=`grep -ri warning log/* | wc -l`;
numerrors=`grep -ri error log/* | wc -l`;
echo -n "Log files contain $numwarnings warnings";
if [ $numwarnings -eq 0 ]; then
    echo ".";
else
    echo ":"
    grep -ri warning log/* | sed 's/^/\t/';
fi;
   
echo -n "Log files contain $numerrors errors";
if [ $numerrors -eq 0 ]; then
    echo ".";
else
    echo ":"
    grep -ri error log/* | sed 's/^/\t/';
fi;
