#!/bin/sh
SDK=/Developer/SDKs/MacOSX10.4u.sdk
if [ $# -eq 1 ]
then
SDK=$1
fi
CFLAGS="-Wno-multichar -isysroot ${SDK}" ; export CFLAGS
/bin/rm -rf Developer System
h-to-ffi.sh ${SDK}/System/Library/Frameworks/Carbon.framework/Headers/Carbon.h

