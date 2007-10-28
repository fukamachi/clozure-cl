#!/bin/sh
SDK=/Developer/SDKs/MacOSX10.5.sdk
if [ $# -eq 1 ]
then
SDK=$1
fi 
CFLAGS="-m64 -isysroot ${SDK}"; export CFLAGS
rm -rf System Developer
h-to-ffi.sh ${SDK}/System/Library/Frameworks/OpenGL.framework/Headers/OpenGL.h
h-to-ffi.sh ${SDK}/System/Library/Frameworks/GLUT.framework/Headers/glut.h
h-to-ffi.sh ${SDK}/System/Library/Frameworks/AGL.framework/Headers/agl.h
