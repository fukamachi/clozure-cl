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


#include "lisp.h"
#include "bits.h"



/* This should be a lot faster than calling set_bit N times */

void
set_n_bits(bitvector bits, unsigned first, unsigned n)
{
  if (n) {
    unsigned
      lastbit = (first+n)-1,
      leftbit = first & 0x1f,
      leftmask = 0xffffffff >> leftbit,
      rightmask = 0xffffffff << (31 - (lastbit & 0x1f)),
      *wstart = ((unsigned *) bits) + (first>>5),
      *wend = ((unsigned *) bits) + (lastbit>>5);

    if (wstart == wend) {
      *wstart |= (leftmask & rightmask);
    } else {
      *wstart++ |= leftmask;
      n -= (32 - leftbit);
      
      while (n >= 32) {
        *wstart++ = 0xffffffff;
        n-=32;
      }
      
      if (n) {
        *wstart |= rightmask;
      }
    }
  }
}
  




bitvector 
new_bitvector(unsigned nbits)
{
  return (bitvector) zalloc((sizeof(unsigned)*(nbits+31)>>5));
}

/* Note that this zeros fullwords */
void
zero_bits(bitvector bits, unsigned nbits)
{
  memset(bits, 0, (sizeof(unsigned)*((nbits+31)>>5)));
}

void
ior_bits(bitvector dest, bitvector src, unsigned nbits)
{
  while (nbits > 0) {
    *dest++ |= *src++;
    nbits -= 32;
  }
}
