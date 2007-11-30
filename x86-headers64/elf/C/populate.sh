#!/bin/sh
CFLAGS="-m64";export CFLAGS
rm -rf usr
# if the libelf in question comes from RedHat's elfutils,
# it doesn't seem possible to use other interfaces (gelf)
# without risking GPL contagion.  
h-to-ffi.sh /usr/include/libelf.h


