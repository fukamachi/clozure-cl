#!/bin/sh
rm -rf usr
CFLAGS="-m64 -I/usr/X11R6/include";export CFLAGS
h-to-ffi.sh /usr/X11R6/include/GL/glx.h
h-to-ffi.sh /usr/X11R6/include/GL/glu.h
h-to-ffi.sh /usr/X11R6/include/GL/glut.h
