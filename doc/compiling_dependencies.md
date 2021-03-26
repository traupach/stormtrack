# Compiling dependencies

The stormtrack scripts rely on dependencies. Here are instructions on how to compile them on a Debian system, assuming git code is to be downloaded to `$HOME/git` and software is installed in `$HOME/software`.

## ESMF 

[ESMF](https://www.earthsystemcog.org/projects/esmf/) is used for regridding WRF to a regular grid. To install, download the code (in this case, version 8.0.0):

```
cd $HOME/git
git clone -b ESMF_8_0_0 --depth 1 https://git.code.sf.net/p/esmf/esmf
```

Set environment variables:

```
export ESMF_DIR=$HOME/git/esmf/
export ESMF_INSTALL_PREFIX=$HOME/software/esmf/
export ESMF_NETCDF="nc-config"
```

Compile and install:

```
cd $HOME/git/esmf
make -j 8
make install
```

Adjust paths in ``.bashrc`:

```
export PATH=$PATH:$HOME/software/esmf/bin/binO/Linux.gfortran.64.mpiuni.default/
```
