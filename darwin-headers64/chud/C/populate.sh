#!/bin/sh
# This is for CHUD 4.4.x.  No two versions of CHUD install in the
# same place, or contain the same set of headers
# A lot of the CHUD headers are missing terminating newlines;
# I'm not sure how to suppress warnings about that (or why those newlines
# are missing.)
h-to-ffi.sh -m64 -Wno-endif-labels /System/Library/PrivateFrameworks/CHUD.framework/Headers/CHUDCore.h
