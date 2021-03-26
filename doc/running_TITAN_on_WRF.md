# Running TITAN on WRF data

These instructions assume that the stormtrack project is cloned into `$HOME/git`.

## Setup

Set the following environment variables:

```
export NCL_LIB_PATH=$HOME/git/stormtrack/code/ncl/library/
```

Note that NCL scripts expect NCL libraries installed in `/usr/lib/ncarg`.

## Regrid WRF data

Make a fresh directory and put all WRF files to process in a subdirectory called `wrf`. Change to the new directory.

```
$HOME/git/stormtrack/code/sh/prep_WRF_for_TITAN.sh
```

This script regrids WRF output to a rectilinear grid in `wrf_regridded`, then converts those files to MDV format in `mdv`.

## Set up TITAN parameters

Copy a TITAN parameter file from `$HOME/git/stormtrack/params/` to `params/Titan.opt`. Edit if required. Then run:

```
$HOME/git/stormtrack/code/sh/run_TITAN.sh
```

This script will

1. Run TITAN to create binary storm files in the output directory 'storms'.
2. Run stormStats to create CSV output of storm statistics in 'tracks'.

