/*
   Copyright (C) 1994-2001 Digitool, Inc
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#ifndef __lisptypes__
#define __lisptypes__

typedef unsigned LispObj;

typedef struct ucontext ExceptionInformation, ExceptionInformationPowerPC;

typedef char *BytePtr;
typedef int OSStatus, OSErr;
#define noErr ((OSErr) 0)
typedef int Boolean;
typedef void *LogicalAddress;
typedef char *StringPtr;
typedef char *Ptr;
typedef unsigned int UInt32;
typedef union {
  unsigned short halfword;
  struct {
    unsigned short offset:14;
    unsigned short hasnode:1;
    unsigned short modified:1;
  } bits;
} pageentry;


#define true 1
#define false 0

#ifdef DARWIN
typedef void (*__sighandler_t)(int);
#endif
#endif /*__lisptypes__ */
