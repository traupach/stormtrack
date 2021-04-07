# TITAN tracking of storms in WRF output
Code and documentation for tracking simulated thunderstorms in WRF output.

Tim Raupach <timothy.h.raupach@gmail.com>  

This code was written by me (Tim Raupach) while I was a postdoc in the Institute of Geography and Oeschger Centre for Climate Change Research, University of Bern, Bern, Switzerland.

Affiliation:
- Until 31.12.2019: Institute of Geography and Oeschger Centre for Climate Change Research, University of Bern, Bern, Switzerland.
- Current affiliation: Centre for Climate Change Research, UNSW Sydney, Sydney, Australia.

Documentation - setup:

- [How to compile TITAN](doc/compiling_TITAN.md).
- [How to compile script dependencies](doc/compiling_dependencies.md).
- [Required R packages for R code](doc/required_R_packages.md).

Documentation - running:

- [How to run TITAN on permutations of threshold values](doc/test_params.md).
- [How to track storms in WRF output using TITAN](doc/running_TITAN_on_WRF.md).
- [How to collect hail properties from WRF/TITAN output](doc/collecting_hail_properties.md).

Structure:

- [WRF_config](WRF_config) -- WRF namelist.input file and information.
- [code](code) -- scripts, split into ncl, R, and sh.
- [data](data) -- support data for storm tracking (subdomain definitions).
- [doc](doc) -- documentation.
- [threshold_optimisation](threshold_optimisation) -- reports showing TITAN threshold optimisation.
- [manuscript](manuscript) -- R markdown code for generation of the manuscript describing this work.

LROSE utilites:

- Modified versions of LROSE utilities that are used in this project are available under a [separate repository](https://github.com/traupach/modified_LROSE_utils). Parameter files for the LROSE utilities, including optimisted parameter files per microphysics scheme, are available under that repository.
