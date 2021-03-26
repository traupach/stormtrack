#!/bin/bash

## Process WRF data to be ready to run TITAN storm tracking.
## Tim Raupach, 2019.

## Settings ----------------------------------------------------------------------------------------
GIT_DIR="$HOME/git/stormtrack/"       ## Location of cloned stormtrack git repository.
TITAN_BIN="$HOME/software/lrose/bin/" ## Location of compiled TITAN binaries.
## -------------------------------------------------------------------------------------------------

## Check for the "wrf" directory.
if [ ! -d wrf ]; then
   echo "ERROR: A directory called 'wrf' is required.";
   exit;
fi;

## Make a directory for log files.
if [ ! -d log ]; then
    mkdir log;
fi;

## Make a directory for parameters and copy defaults for Nc2Mdv and stormStats.
if [ ! -d params ]; then
    mkdir params;
    cp $GIT_DIR/params/Nc2Mdv.opt params;
    cp $GIT_DIR/params/stormStats.opt params;
fi;

## Regrid WRF files into a regular grid. 
if [ -d wrf_regridded ]; then
    echo "WARNING: 'wrf_regridded' directory exists; resuming from last missing file."
fi;
dstr=`date -u +"%Y-%m-%d_%H:%M:%S_UTC"`;
logfile="log/regrid_WRF_to_regular_$dstr.log";
echo "Regridding WRF files to regular grid [logged in $logfile]...";
stdbuf -oL ncl $GIT_DIR/code/ncl/regrid_WRF_to_regular.ncl &> $logfile;

## Convert regridded NetCDF files to MDV. 
if [ ! -d mdv ]; then
    dstr=`date -u +"%Y-%m-%d_%H:%M:%S_UTC"`;
    logfile="log/Nc2Mdv_$dstr.log";
    compLogFile="log/Nc2Mdv_compile_$dstr.log"

    ## Compile and install Nc2MDV in the LROSE directory.
    ## This is to ensure the binary that is run is always the latest version.
    echo "Compiling Nc2MDV..."
    bash -c "cd $GIT_DIR/utils/Nc2Mdv; source set_build_env.sh; make clean; make install" > $compLogFile 2> $compLogFile;
    
    echo "Converting regridded NC files to MDV [logged in $logfile]...";
    stdbuf -oL $TITAN_BIN/Nc2Mdv -params params/Nc2Mdv.opt -f \
	   wrf_regridded/wrfout*regridded.nc &> $logfile;
else
    echo "WARNING: Not overwriting MDV converter output in 'mdv'; skipping this step.";
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
