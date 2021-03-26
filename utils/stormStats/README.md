# stormStats

`stormStats` reads TITAN storm files and outputs the storm information in ascii csv format.

`stormStats` is a modified version of `Storms2Xml2`, which is part of [lrose-core](https://github.com/NCAR/lrose-core). As per the terms of the LROSE BSD license, this modified version is redistributed under the same [license](LICENSE.txt).

## Compilation

Requires lrose-core tools and libraries to be installed. To compile, check paths in `set_build_env.sh`, then run:

```
source set_build_env.sh
make clean
make
```
