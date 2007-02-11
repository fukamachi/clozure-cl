#!/bin/sh
SDK=/Developer/SDKs/MacOSX10.4u.sdk
if [ $# -eq 1 ]
then
SDK=$1
fi
rm -rf System Developer usr
CFLAGS="-isysroot ${SDK}"; export CFLAGS
h-to-ffi.sh ${SDK}/System/Library/Frameworks/WebKit.framework/Headers/WebKit.h
