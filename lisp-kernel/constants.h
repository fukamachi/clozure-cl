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

#ifndef __constants__
#define __constants__ 1

/*  Register usage: */
#define rzero 0
#define sp 1
#define rcontext 2
#define imm0 3
#define imm1 4
#define imm2 5
#define imm3 6
#define imm4 7
#define imm5 8
#define allocptr 9
#define allocbase 10
#define nargs 11
#define tsp 12
#define vsp 13
#define loc_pc 14		/*  code vector locative */
#define new_vsp 15		
#define fn 16
#define temp3 17
#define temp2 18
#define temp1 19
#define temp0 20	
#define arg_x 21
#define arg_y 22
#define arg_z 23
#define save7 24
#define save6 25
#define save5 26
#define save4 27
#define save3 28
#define save2 29
#define save1 30
#define save0 31

#define vfp save0	/*  frame pointer if needed (stack consing). */
#define fname temp3
#define nfn temp2
#define next_method_context temp1
#define closure_data temp0

/* 
  These were previously global variables.  There are lots of implicit
  assumptions about the size of a heap segment, so they might as well
  be constants.
*/

#define heap_segment_size 0x00010000
#define log2_heap_segment_size 16

#define BA_MASK ((unsigned) ((-1<<26) | (1<<1)))
#define BA_VAL  ((unsigned) ((18<<26) | (1<<1)))

#define TCR_FLAG_BIT_FOREIGN fixnumshift
#define TCR_FLAG_BIT_AWAITING_PRESET (fixnumshift+1)
#define TCR_FLAG_BIT_ALT_SUSPEND (fixnumshift+2)

#define TCR_STATE_FOREIGN (1)
#define TCR_STATE_LISP    (0)
#define TCR_STATE_EXCEPTION_WAIT (2)
#define TCR_STATE_EXCEPTION_RETURN (4)

#ifdef PPC64
#include "constants64.h"
#else
#include "constants32.h"
#endif

#define dnode_size (node_size*2)
#define dnode_shift node_shift+1

#endif
