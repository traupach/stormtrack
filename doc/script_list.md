# List of scripts

## `sh`

- `prep_WRF_for_TITAN.sh`: Prepare WRF data for use with TITAN by regridding and converting to MDV.
- `test_titan_params.sh`: Run TITAN using a range of parameter files to test different options.
- `extract_test_param_results.sh`: Extract storm data from the results of `test_titan_params.sh`. 

## `ncl`

- `regrid_WRF_to_regular.ncl`: regrid WRF fields to rectilinear, regularly spaced fields in `.nc` files.

## `R`

- `generate_titan_param_files.R`: generate parameter files for permutation of TITAN threshold options.
