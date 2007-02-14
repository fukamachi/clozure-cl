#!/bin/sh
CFLAGS="-m64";export CFLAGS
rm -rf usr
h-to-ffi.sh /usr/include/GL/glx.h
h-to-ffi.sh /usr/include/GL/glu.h
h-to-ffi.sh /usr/include/GL/glut.h
