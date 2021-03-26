# Nc2Mdv 

`Nc2Mdv` converts fields from WRF NetCDF files to MDV format.

`Nc2Mdv` is a modified version of `NcGeneric2Mdv` by Mike Dixon. `NcGeneric2Mdv` is part of [lrose-core](https://github.com/NCAR/lrose-core). As per the terms of the LROSE BSD license, this modified version is redistributed under the same [license](LICENSE.txt).

## Compilation

Requires lrose-core tools and libraries to be installed. To compile, check paths in `set_build_env.sh`, then run:

```
source set_build_env.sh
make clean
make
```
