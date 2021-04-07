# TITAN tracking of storms in WRF output
Code and documentation for tracking simulated thunderstorms in WRF output.

Tim Raupach <timothy.h.raupach@gmail.com>  

Affiliation:
- Until 31.12.2019: Institute of Geography and Oeschger Centre for Climate Change Research, University of Bern, Bern, Switzerland.
- Current affiliation: Centre for Climate Change Research, UNSW Sydney, Sydney, Australia.

Licenses:
 - This repository is covered by the MIT [license](LICENSE), **with the exception of the modified LROSE code under the [utils](utils/) section which is covered by the LROSE BSD license (see `LICENSE.txt` in each subdirectory)**. 
 - This code was written/modified by me (Tim Raupach) while I was a postdoc in the Institute of Geography and Oeschger Centre for Climate Change Research, University of Bern, Bern, Switzerland.

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
- [params](params) -- parameter files for LROSE utilities.
- [threshold_optimisation](threshold_optimisation) -- reports showing TITAN threshold optimisation.
- [manuscript](manuscript) -- R markdown code for generation of the manuscript describing this work.
- [utils](utils) -- **modified versions** of LROSE code, for the WRF storm tracking workflow, with their own licenses.

