# Collecting hail properties

The R script [collect_hail_properties.R](../code/R/collect_hail_properties.R) can be used to collect hail properties for each track returned by TITAN.

It reads a TITAN tracks file, then for each track it opens the relevant WRF file and finds values of the variable `HAILCAST_DIAM_MAX` inside each tracked cell; it then calculates statistics on the number of pixels within the cell that are above a certain diameter in mm.

To run, change to the base directory in which other scripts were run, then run

```
Rscript ~/git/stormtrack/code/R/collect_hail_properties.R
```

The script runs using parallel processing but will still take some time to extract the hail properties.
