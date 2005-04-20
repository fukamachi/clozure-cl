#!/bin/sh
# This assumes a fairly standard/fairly recent GNUstep installation,
# with objc headers installed in the GNUstep hierarchy (i.e., in
# /usr/GNUstep/System/Library/Headers/objc
#
# I -think- that NXConstStr.h is effectively obsolete ...
#h-to-ffi.sh -x objective-c -I/usr/GNUstep/System/Library/Headers/ /usr/GNUstep/System/Library/Headers/objc/NXConstStr.h
h-to-ffi.sh -x objective-c -I/usr/GNUstep/System/Library/Headers/ /usr/GNUstep/System/Library/Headers/objc/objc.h
h-to-ffi.sh -x objective-c -I/usr/GNUstep/System/Library/Headers/ /usr/GNUstep/System/Library/Headers/Foundation/Foundation.h
h-to-ffi.sh -x objective-c -I/usr/GNUstep/System/Library/Headers/ /usr/GNUstep/System/Library/Headers/AppKit/AppKit.h
