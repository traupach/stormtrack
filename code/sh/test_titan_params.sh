#!/bin/bash

## Run TITAN using all parameter files in a directory.
## Tim Raupach 2019.

paramDir="$HOME/titan/test_params/"
paramFiles=`ls $paramDir`

if [ ! -e logs ]; then
    mkdir logs;
fi;

for param in $paramFiles; do
    outdir=`echo $param | cut -d_ -f3,4,5 | cut -d. -f1` 

    if [ ! -e storms_$outdir ]; then
	echo $param
	~/software/lrose/bin/Titan -params $paramDir/$param -start "2018 5 29 0 0 0" -end "2018 5 31 0 0 0" > logs/$param.log 2> logs/$param.log
    else
	echo Not overwriting $param;
    fi;
done
