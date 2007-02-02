#!/bin/sh
rm -rf System Developer
SDK=/Developer/SDKs/MacOSX10.4u.sdk
CFLAGS="-isysroot ${SDK}"; export CFLAGS
h-to-ffi.sh ${SDK}/System/Library/Frameworks/AddressBook.framework/Headers/AddressBook.h
