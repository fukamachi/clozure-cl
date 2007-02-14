#!/bin/sh
rm -rf usr
h-to-ffi.sh `pkg-config --cflags gtk+-2.0` -m64 /usr/local/include/gtk-2.0/gtk/gtk.h
