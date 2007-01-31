#!/bin/sh
h-to-ffi.sh `pkg-config --cflags gtk+-2.0` -m64 /usr/include/gtk-2.0/gtk/gtk.h
