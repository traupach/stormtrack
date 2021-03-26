# Running TITAN with permutated parameters

To determine the best thresholding parameters to use, it may be useful to run TITAN over a range of permutations of parameter values.

## Generate parameter files

The TITAN parameter base file ([test_param_base.txt](../params/test_param_base.txt)) sets common parameters to use in all test cases.

Edit the `R` script [generate_titan_param_files.R](../code/R/generate_titan_param_files.R) to choose the range of values over which to permute. The default values are:

```
low_thresh = seq(34, 42, by=1)         ## Values for low_dbz_threshold.
dual_by = 1                            ## Increment for dbz_threshold in dual_threshold.
dual_min = 4                           ## Try from low_dbz_threshold + dual_min, for dbz_threshold.
dual_max = 12                          ## Try up to low_dbz_threshold + dual_max, for dbz_threshold.
min_size = c(25, 50, 75)               ## Values for min_storm_size (km3).
```

Run the R script to generate TITAN parameter files for each threshold permutation.

```
Rscript $HOME/git/stormtrack/code/R/generate_titan_param_files.R
```

## Run TITAN

Edit and run the script [test_titan_params.sh](../code/sh/test_titan_params.sh) to run TITAN using each parameter. The date range over which to run is specified in this script. Run the script in the directory in which you want output.

```
cd param_test
$HOME/git/stormtrack/code/sh/test_titan_params.sh
```

## Extract storm properties

Edit and run the script [extract_test_param_results.sh](../code/sh/extract_test_param_results.sh) to run `stormStats` on each output storm file.

```
$HOME/git/stormtrack/code/sh/extract_test_param_results.sh
```

