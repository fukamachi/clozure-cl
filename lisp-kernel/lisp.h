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

#ifndef __lisp__
#define __lisp__

#include "lisptypes.h"

#include "constants.h"
#include "macros.h"



static inline unsigned long
_align_to_power_of_2(unsigned long n, unsigned power)
{
  unsigned align = (1<<power) -1;

  return (n+align) & ~align;
}

#define align_to_power_of_2(n,p) _align_to_power_of_2(((unsigned long)(n)),p)

static inline unsigned long
_truncate_to_power_of_2(unsigned long n, unsigned power)
{
  return n & ~((1<<power) -1);
}

#define truncate_to_power_of_2(n,p) _truncate_to_power_of_2((unsigned long)(n),p)

LispObj start_lisp(TCR*, LispObj);

#include "kernel-globals.h"
#endif
