#!/bin/sh
SDK=/Developer/SDKs/MacOSX10.5.sdk
if [ $# -eq 1 ]
then
SDK=$1
fi
rm -rf System Developer usr
CFLAGS="-m64 -fobjc-abi-version=2 -isysroot ${SDK} -mmacosx-version-min=10.5"; export CFLAGS
h-to-ffi.sh ${SDK}/System/Library/Frameworks/WebKit.framework/Headers/WebKit.h
