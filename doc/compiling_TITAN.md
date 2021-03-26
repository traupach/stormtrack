# Compiling TITAN

TITAN is storm tracking software that is part of NCAR's [lrose](https://github.com/NCAR/lrose-core) project.  

These compilation steps are based on the lrose [linux installation guide (accessed 2019-11-06)](https://github.com/NCAR/lrose-core/blob/master/docs/download/clone_src_and_build.linux.md) and the guide to [building for the NCAR environment (accessed 2019-11-06)](https://github.com/NCAR/lrose-core/blob/master/docs/build/NCAR_build_environment.linux.md). They record the steps that produced a working version of TITAN on a system running Ubuntu 18.04.3 LTS.  

The following steps assume that the installation path will be `$HOME/software/lrose` and that git repositories will be in `$HOME/git`.

## Debian/Ubuntu set-up

Set up required packages and link `qmake`:

```
sudo apt-get install -y \
  git gcc g++ gfortran cmake rsync mlocate \
  automake make libtool pkg-config python \
  libcurl3-dev curl \
  libfl-dev libbz2-dev libx11-dev libpng-dev \
  libfftw3-dev libexpat1-dev \
  qtbase5-dev qtdeclarative5-dev \
  libgeographic-dev libeigen3-dev libzip-dev \
  libnetcdf-dev netcdf-bin libhdf5-dev hdf5-tools

cd /usr/bin
sudo /bin/rm -f qmake qmake-qt5
sudo ln -s /usr/lib/x86_64-linux-gnu/qt5/bin/qmake qmake
sudo ln -s /usr/lib/x86_64-linux-gnu/qt5/bin/qmake qmake-qt5
```

## Compile and install `lrose-netcdf`

TITAN requires lrose-netcdf to be installed. Download the package from git:

```
cd $HOME/git 
git clone https://github.com/NCAR/lrose-netcdf
cd $HOME/git/lrose-netcdf
./build_and_install_netcdf -x $HOME/software/lrose/
```

## Compile TITAN

Download the LROSE package:

```
cd $HOME/git
git clone https://github.com/NCAR/lrose-core
```

The newly installed local version of hdf5 should be used, but (at the time of writing) the installation files include `if` statements that cause system versions to be used on Debian installs. To fix this, in `$HOME/git/lrose-core/codebase/make_bin/createMakefile.am.lib.py`, remove the following lines:

```
if (isDebianBased):
        fo.write("# NOTE: add in Debian location of HDF5\n")
        fo.write("AM_CFLAGS += -I/usr/include/hdf5/serial\n")
```

In `$HOME/git/lrose-core/codebase/make_bin/createMakefile.am.app.lrose-core.py`, remove the same block as above, as well as the following lines:

```
if (isDebianBased):
        fo.write("# NOTE: add in Debian location of HDF5\n")
        fo.write("AM_LDFLAGS += -L/usr/lib/x86_64-linux-gnu/hdf5/serial\n")
```

In `$HOME/git/lrose-core/codebase/make_bin/createMakefile.am.app.lrose-core.py`, move `physics` to be after `rapformats` in the variable `linkOrder`.  

Create makefiles:

```
cd $HOME/git/lrose-core/codebase
./make_bin/installPackageMakefiles.py --debug --package titan --codedir .
```

Run autoconf:

```
cd $HOME/git/lrose-core/codebase
cp ../build/Makefile.top Makefile
cp ../build/autoconf/configure.base.shared configure.base.shared
./make_bin/createConfigure.am.py --dir . --baseName configure.base.shared --shared --pkg titan --debug
```

Set environment variables:

```
export LROSE_INSTALL_DIR=$HOME/software/lrose/
export LROSE_CORE_DIR=$HOME/git/lrose-core/
export LDFLAGS="-L$HOME/software/lrose/lib -Wl,--enable-new-dtags,-rpath,$HOME/software/lrose/lib"
export CXXFLAGS=" -std=c++11 "
```

Set the build environment:

```
cd $LROSE_CORE_DIR
source build/set_build_env.sh
```

Configure:

```
cd $LROSE_CORE_DIR/codebase/
./configure --prefix=$HOME/software/lrose \
   --with-netcdf=$HOME/software/lrose \
   --with-hdf5=$HOME/software/lrose
```

Build and install libraries:

```
cd $HOME/git/lrose-core/codebase/libs/
make -j 8
make install
```

Build certain required legacy libraries that aren't build by default:

```
cd $LROSE_CORE_DIR/codebase/libs/legacy/symprod
make -j 8
make install

cd $LROSE_CORE_DIR/codebase/libs/legacy/mdv
make -j 8
make install

cd $LROSE_CORE_DIR/codebase/libs/rdi
make -j 8
make install
```

Build required binaries:

```
cd $LROSE_CORE_DIR/codebase/apps/titan
make -j 8
make install

cd $LROSE_CORE_DIR/codebase/apps/ingest
make -j 8
make install
```

Build and install `tdrp`:

```
cd $LROSE_CORE_DIR/codebase/apps/tdrp
make -j 8
make install
```
