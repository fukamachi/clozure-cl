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
#define WORD_SIZE 32
#ifdef PPC64
#undef WORD_SIZE
#define WORD_SIZE 64
#endif
#ifdef X8664
#undef WORD_SIZE
#define WORD_SIZE 64
#endif

#if WORD_SIZE == 64
typedef u_int64_t LispObj;
typedef u_int64_t natural;
typedef int64_t signed_natural;
typedef u_int64_t unsigned_of_pointer_size;
#else
typedef u_int32_t LispObj;
typedef u_int32_t natural;
typedef int32_t signed_natural;
typedef u_int32_t unsigned_of_pointer_size;
#endif


#if defined(DARWIN)
#if WORD_SIZE == 64
typedef struct ucontext64 ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext64
#else
typedef struct ucontext ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext
#endif
#else  /* #ifdef DARWIN */
#ifdef LINUX
typedef struct ucontext ExceptionInformation;
#endif
#ifdef FREEBSD
typedef struct __ucontext ExceptionInformation;
#endif
#define UC_MCONTEXT(UC) UC->uc_mcontext
#endif /* #ifdef DARWIN */



typedef int OSStatus, OSErr;
#define noErr ((OSErr) 0)
typedef int Boolean;
typedef void *LogicalAddress;
typedef char *Ptr, *BytePtr, *StringPtr;
typedef unsigned int UInt32;



#define true 1
#define false 0

#ifdef DARWIN
typedef void (*__sighandler_t)(int);
#endif
#endif /*__lisptypes__ */
