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

/* Totally different content than 'macros.s' */



#ifndef __macros__
#define __macros__

#define ptr_to_lispobj(p) ((LispObj)((unsigned_of_pointer_size)(p)))
#define ptr_from_lispobj(o) ((LispObj*)((unsigned_of_pointer_size)(o)))
#define lisp_reg_p(reg)  ((reg) >= fn)

#define fulltag_of(o)  ((o) & fulltagmask)
#define tag_of(o) ((o) & tagmask)
#define untag(o) ((o) & ~fulltagmask)

#define deref(o,n) (*((LispObj*) (ptr_from_lispobj(untag((LispObj)o)))+(n)))
#define header_of(o) deref(o,0)

#define header_subtag(h) ((h) & subtagmask)
#define header_element_count(h) ((h) >> num_subtag_bits)
#define make_header(subtag,element_count) ((subtag)|((element_count)<<num_subtag_bits))

#define unbox_fixnum(x) ((int)(((int)(x))>>fixnum_shift))
#define box_fixnum(x) ((x)<<fixnum_shift)

#define car(x) (((cons *)(untag(x)))->car)
#define cdr(x) (((cons *)(untag(x)))->cdr)

/* "sym" is an untagged pointer to a symbol */
#define BOUNDP(sym)  ((((lispsymbol *)(sym))->vcell) != undefined)

/* Likewise. */
#define FBOUNDP(sym) ((((lispsymbol *)(sym))->fcell) != nrs_UDF.vcell)

#ifdef PPC64
#define nodeheader_tag_p(tag) (((tag) & lowtag_mask) == lowtag_nodeheader)
#else
#define nodeheader_tag_p(tag) (tag == fulltag_nodeheader)
#endif

#ifdef PPC64
#define immheader_tag_p(tag) (((tag) & lowtag_mask) == lowtag_immheader)
#else
#define immheader_tag_p(tag) (tag == fulltag_immheader)
#endif

/* lfuns */
#define lfun_bits(f) (deref(f,header_element_count(header_of(f))))
#define named_function_p(f) (!(lfun_bits(f)&(1<<(29+fixnum_shift))))
#define named_function_name(f) (deref(f,-1+header_element_count(header_of(f))))

#endif
