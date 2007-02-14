#!/bin/sh
rm -rf usr
CFLAGS="-m64"; export CFLAGS
h-to-ffi.sh `pkg-config --cflags libgnomeui-2.0` /usr/local/include/libgnomeui-2.0/gnome.h
