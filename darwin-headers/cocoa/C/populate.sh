#!/bin/sh
CFLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk"; export CFLAGS
h-to-ffi.sh /Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/PreferencePanes.framework/Headers/PreferencePanes.h
