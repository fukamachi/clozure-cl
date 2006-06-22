#!/bin/sh
CFLAGS="-m64"; export CFLAGS
h-to-ffi.sh `pkg-config --cflags libgnomeui-2.0` /usr/include/libgnomeui-2.0/gnome.h
