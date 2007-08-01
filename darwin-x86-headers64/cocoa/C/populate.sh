#!/bin/sh
# For now, things earlier than the 10.5 sdk are pointless
rm -rf System Developer usr
SDK=/Developer/SDKs/MacOSX10.5.sdk
CFLAGS="-m64 -fobjc-abi-version=2 -isysroot ${SDK}"; export CFLAGS
h-to-ffi.sh ${SDK}/usr/include/objc/objc-runtime.h
h-to-ffi.sh ${SDK}/usr/include/objc/objc-exception.h
h-to-ffi.sh ${SDK}/usr/include/objc/Object.h
h-to-ffi.sh ${SDK}/usr/include/objc/Protocol.h
h-to-ffi.sh ${SDK}/System/Library/Frameworks/Cocoa.framework/Headers/Cocoa.h
