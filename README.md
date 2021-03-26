# TITAN tracking of storms in WRF output
Code and documentation for tracking simulated thunderstorms in WRF output.

Tim Raupach <timothy.h.raupach@gmail.com>  
Institute of Geography and Oeschger Centre for Climate Change Research, University of Bern, Bern, Switzerland.

Documentation - setup:

- [How to compile TITAN](doc/compiling_TITAN.md).
- [How to compile script dependencies](doc/compiling_dependencies.md).
- [Required R packages for R code](doc/required_R_packages.md).

Documentation - running:

- [How to run TITAN on permutations of threshold values](doc/test_params.md).
- [How to track storms in WRF output using TITAN](doc/running_TITAN_on_WRF.md).
- [How to collect hail properties from WRF/TITAN output](doc/collecting_hail_properties.md).

Structure:

- [code](code) -- scripts, split into ncl, R, and sh.
- [doc](doc) -- documentation.
- [params](params) -- parameter files for LROSE utilities.
- [utils](utils) -- versions of LROSE code modified to work with the WRF storm tracking workflow.
- [WRF_config](WRF_config) -- WRF namelist.input files used for each microphysical scheme.
- [data](data) -- support data for storm tracking, including [subdomain definitions](data/domains).
