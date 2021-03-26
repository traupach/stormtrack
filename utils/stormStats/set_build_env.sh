# Set environment variables for the RAP Makefile system
#
# for sh and bash
#
# Before sourcing, you need to set:
#
#   $LROSE_CORE_DIR
#   $LROSE_INSTALL_DIR
#
# to the top dir for installation.
#
# MODIFIED from lrose-core BY T. RAUPACH, UNIBE, 2018-2019; see LICENSE.txt.

export LROSE_CORE_DIR=$HOME/git/lrose-core/
export LROSE_INSTALL_DIR=$HOME/software/lrose/

export HOST_OS=LINUX_LROSE
uname -a | grep x86_64
if [ "$?" = 1 ]
then
    export HOST_OS=LINUX
fi

export RAP_MAKE_INC_DIR=$LROSE_CORE_DIR/codebase/make_include
export RAP_MAKE_BIN_DIR=$LROSE_CORE_DIR/codebase/make_bin

export RAP_INC_DIR=$LROSE_INSTALL_DIR/include
export RAP_LIB_DIR=$LROSE_INSTALL_DIR/lib
export RAP_BIN_DIR=$LROSE_INSTALL_DIR/bin
export RAP_MAN_DIR=$LROSE_INSTALL_DIR/man
export RAP_DOC_DIR=$LROSE_INSTALL_DIR/doc

export RAP_SHARED_INC_DIR=$LROSE_INSTALL_DIR/include
export RAP_SHARED_LIB_DIR=$LROSE_INSTALL_DIR/lib
export RAP_SHARED_BIN_DIR=$LROSE_INSTALL_DIR/bin
export RAP_SHARED_MAN_DIR=$LROSE_INSTALL_DIR/man
export RAP_SHARED_DOC_DIR=$LROSE_INSTALL_DIR/doc

export RAP_INST_LIB_DIR=$LROSE_INSTALL_DIR/lib
export RAP_INST_BIN_DIR=$LROSE_INSTALL_DIR/bin

export PATH=$RAP_BIN_DIR:$PATH
export LD_LIBRARY_PATH=$RAP_LIB_DIR:$LD_LIBRARY_PATH

