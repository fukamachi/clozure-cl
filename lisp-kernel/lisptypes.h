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


#ifdef SOLARIS
/* Solaris doesn't laugh and play like the other children */
typedef int64_t s64_t;
typedef uint64_t u64_t;
typedef int32_t s32_t;
typedef uint32_t u32_t;
typedef int16_t s16_t;
typedef uint16_t u16_t;
typedef int8_t s8_t;
typedef uint8_t u8_t;
#else
typedef int64_t s64_t;
typedef u_int64_t u64_t;
typedef int32_t s32_t;
typedef u_int32_t u32_t;
typedef int16_t s16_t;
typedef u_int16_t u16_t;
typedef int8_t s8_t;
typedef u_int8_t u8_t;
#endif

#if WORD_SIZE == 64
typedef u64_t LispObj;
typedef u64_t natural;
typedef s64_t signed_natural;
typedef u64_t unsigned_of_pointer_size;
#else
typedef u32_t LispObj;
typedef u32_t natural;
typedef s32_t signed_natural;
typedef u32_t unsigned_of_pointer_size;
#endif


#if defined(DARWIN)
#if WORD_SIZE == 64
typedef struct ucontext64 ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext64
#ifdef X8664
/* Broken <i386/ucontext.h> in xcode 2.4 */
#include <sys/ucontext.h>
#ifndef _MCONTEXT64_T /* A guess at what'll be defined when this is fixed */
struct mcontext64 {
	x86_exception_state64_t	es;
	x86_thread_state64_t 	ss;	
	x86_float_state64_t	fs;
};
#endif
#endif
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
#ifdef SOLARIS
typedef struct ucontext ExceptionInformation;
#endif
#define UC_MCONTEXT(UC) UC->uc_mcontext
#endif /* #ifdef DARWIN */

#ifdef CHAR_SIZE_32
typedef uint32_t lisp_char_code;
#else
typedef unsigned char lisp_char_code;
#endif

typedef int OSStatus, OSErr;
#define noErr ((OSErr) 0)
typedef int Boolean;
typedef void *LogicalAddress;
typedef char *Ptr, *BytePtr, *StringPtr;
typedef unsigned int UInt32;



#define true 1
#define false 0

#endif /*__lisptypes__ */
