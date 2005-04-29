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

/*
  12/26/95  gb  lots o macros.
*/


#ifndef __bits_h__
#define __bits_h__ 1

#include <string.h>

typedef natural *bitvector;


static inline int
set_bit(bitvector bits,natural bitnum)
{
  natural
    windex = bitnum>>5, 
    old = bits[windex],
    new = old | (0x80000000 >> (bitnum & 0x1f));
  if (new == old) {
    return 1;			/* Was set */
  } else {
    bits[windex] = new;
    return 0;			/* Was clear */
  }
}

static inline int 
atomic_set_bit(bitvector bits ,natural bitnum)
{
  extern int atomic_ior(bitvector, natural);
  natural
    windex = bitnum>>5,
    mask = (0x80000000 >> (bitnum & 0x1f));

  return atomic_ior(bits + windex, mask);
}

void set_n_bits(bitvector,natural,natural);

static inline int
clr_bit(bitvector bits, natural bitnum)
{
  unsigned 
    windex = bitnum>>5, 
    old = bits[windex],
    new = old & ~(0x80000000 >> (bitnum & 0x1f));
  if (new == old) {
    return 0;	/* Was clear */
  } else {
    bits[windex] = new;
    return 1;	/* Was set */
  }
}


static inline unsigned
ref_bit(bitvector bits,natural bitnum)
{
  return ((bits[bitnum>>5] & (0x80000000 >> (bitnum & 0x1f))) != 0);
}

bitvector new_bitvector(natural);
void zero_bits(bitvector, natural);
void ior_bits(bitvector,bitvector,natural);

#define BIT0_MASK 0x80000000U 
#define bits_word_index(bitnum) (((natural)(bitnum)) >> 5)
#define bits_bit_index(bitnum) (((natural)(bitnum)) & 0x1f)
#define bits_word_ptr(bits,bitnum) \
  ((natural*) (((natural*) bits) + ((natural) (bits_word_index(bitnum)))))
#define bits_word_mask(bitnum) ((BIT0_MASK) >> bits_bit_index(bitnum))
#define bits_indexed_word(bitv,indexw) ((((natural*)(bitv))[indexw]))
#define bits_word(bitv,bitnum) bits_indexed_word(bits,bits_word_index(bitnum))

/* Evaluates some arguments twice */

#define set_bits_vars(BITVvar,BITNUMvar,BITPvar,BITWvar,MASKvar) \
{ BITPvar = bits_word_ptr(BITVvar,BITNUMvar); BITWvar = *BITPvar; MASKvar = bits_word_mask(BITNUMvar); }

#define set_bitidx_vars(BITVvar,BITNUMvar,BITPvar,BITWvar,BITIDXvar) \
{ BITPvar = bits_word_ptr(BITVvar,BITNUMvar); BITIDXvar = bits_bit_index(BITNUMvar); \
    BITWvar = (*BITPvar << BITIDXvar) >> BITIDXvar; }

#ifdef __GNUC__
static __inline__ unsigned
count_leading_zeros(natural w) __attribute__((always_inline));

static __inline__ unsigned
count_leading_zeros(natural w)
{
  unsigned lz;
#ifdef PPC64
  __asm__  ("cntlzd %0,%1" : "=r" (lz) : "r" (w));
#else
  __asm__  ("cntlzw %0,%1" : "=r" (lz) : "r" (w));
#endif
  return lz;
}
#else
unsigned
count_leading_zeros(natural);
#endif




                                        
#endif /* __bits_h__ */
