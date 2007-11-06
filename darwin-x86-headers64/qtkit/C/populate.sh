#!/bin/sh
# For now, things earlier than the 10.5 sdk are pointless
rm -rf System Developer usr
SDK=/Developer/SDKs/MacOSX10.5.sdk
CFLAGS="-m64 -fobjc-abi-version=2 -isysroot ${SDK} -mmacosx-version-min=10.5"; export CFLAGS
h-to-ffi.sh ${SDK}/System/Library/Frameworks/QTKit.framework/Headers/QTKit.h
