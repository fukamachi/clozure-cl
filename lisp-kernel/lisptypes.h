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

#include <sys/types.h>
#ifdef PPC64
typedef u_int64_t LispObj;
typedef u_int64_t natural;
typedef int64_t signed_natural;
#ifdef FOREIGN_POINTER_32BIT
typedef u_int32_t unsigned_of_pointer_size;
#else
typedef u_int64_t unsigned_of_pointer_size;
#endif
#else
typedef u_int32_t LispObj;
typedef u_int32_t natural;
typedef int32_t signed_natural;
typedef u_int32_t unsigned_of_pointer_size;
#endif

typedef struct ucontext ExceptionInformation, ExceptionInformationPowerPC;

typedef int OSStatus, OSErr;
#define noErr ((OSErr) 0)
typedef int Boolean;
typedef void *LogicalAddress;
typedef char *Ptr, *BytePtr, *StringPtr;
typedef unsigned int UInt32;

typedef union {
  unsigned short halfword;
  struct {
#ifdef PPC64
    unsigned short offset:13;
    unsigned short pad:1;
#else
    unsigned short offset:14;
#endif
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
