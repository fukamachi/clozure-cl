/*
   Copyright (C) 2005 Clozure Associates
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

		
	include(lisp.s)
	_beginfile
	
        .align 2
define([_spentry],[ifdef([__func_name],[_endfn],[])
	_exportfn(_SP$1)
	.line  __line__
])

             
define([_endsubp],[
	_endfn(_SP$1)
# __line__
])

/* %arg_z <- %arg_y + %arg_z.  Do the fixnum case - including overflow -
  inline.  Call out otherwise. */
_spentry(builtin_plus)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 2f)
	__(addq %arg_y,%arg_z)
	__(jo 1f)
	__(jmp *%ra0)
	/* Overflowed by one bit.  Make a bignum with 2 (32-bit) digits. */
1:	__(unbox_fixnum(%arg_z,%imm0))
	__(xorq overflow_mask(%rip),%imm0)
	__(rol $32,%imm0)
	
2:
_endsubp
overflow_mask:	
	.quad 0xe000000000000000
		
