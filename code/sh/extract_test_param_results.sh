#!/bin/bash

## For each storm directory in the current directory, run stormStats.
## Tim Raupach 2019.

## Compile stormStats.
dir=`pwd`;
cd $HOME/git/stormtrack/utils/stormStats;
source set_build_env.sh;
make clean;
make;
cd $dir;

for stormDir in `ls | grep storms_`; do
    echo $stormDir;
    cd $stormDir;
    $HOME/git/stormtrack/utils/stormStats/stormStats -f *.sh5;
    cd ..;
done
