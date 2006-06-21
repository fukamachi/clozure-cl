#!/bin/sh
CFLAGS="-Wno-multichar -isysroot /Developer/SDKs/MacOSX10.4u.sdk" ; export CFLAGS
/bin/rm -rf Developer System
h-to-ffi.sh /Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/Carbon.framework/Headers/Carbon.h

