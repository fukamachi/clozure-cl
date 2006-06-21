#!/bin/sh
CFLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk"; export CFLAGS
h-to-ffi.sh /Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/OpenGL.framework/Headers/OpenGL.h
h-to-ffi.sh /Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/GLUT.framework/Headers/glut.h
h-to-ffi.sh /Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/AGL.framework/Headers/agl.h
