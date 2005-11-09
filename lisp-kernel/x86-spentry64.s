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

define([jump_builtin],[
	ref_nrs_value(%fname,builtin_functions)
	set_nargs($2)
	vrefr(%fname,%fname,$1)
	jump_fname()
])

/* %arg_z has verflowed by one bit.  Make a bignum with 2 (32-bit) digits. */
_startfn(C(fix_onebit_overflow))
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed([],aligned_bignum_size(2)))
	__(unbox_fixnum(%imm0,%arg_z))
	__(mov %temp0,%arg_z)
	__(xorq overflow_mask(%rip),%imm0)
	__(movq %imm0,misc_data_offset(%arg_z))
	__(jmp *%ra0)	
overflow_mask: 	.quad 0xe000000000000000
_endfn

/* %imm1:%imm0 constitute a signed integer, almost certainly a bignum.
   Make a lisp integer out of those 128 bits .. */
_startfn(C(makes128))
/* We're likely to have to make a bignum out of the integer in %imm1 and
   %imm0. We'll need to use %imm0 and %imm1 to cons the bignum, and
   will need to do some arithmetic (determining significant bigits)
   on %imm0 and %imm1 in order to know how large that bignum needs to be.
   Cache %imm0 and %imm1 in %xmm0 and %xmm1. */
   
	__(movd %imm0,%xmm0)
	__(movd %imm1,%xmm1)
	
/* If %imm1 is just a sign extension of %imm0, make a 64-bit signed integer. */
	
	__(sarq $63,%imm0) 
	__(cmpq %imm0,%imm1)
	__(movd %xmm0,%imm0)
	__(je _SPmakes64)
	
/* Otherwise, if the high 32 bits of %imm1 are a sign-extension of the
   low 32 bits of %imm1, make a 32-digit bignum.  If the upper 32 bits
   of %imm1 are significant, make a 4 digit bignum */
	__(movq %imm1,%imm0)
	__(shlq $32,%imm0)
	__(sarq $32,%imm0)
	__(cmpq %imm0,%imm1)
	__(jz 3f)
	__(mov $four_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(4)))
	__(movq %xmm0,misc_data_offset(%arg_z))
	__(movq %xmm1,misc_data_offset+8(%arg_z))
	__(jmp *%ra0)
3:	__(mov $three_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(3)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movd %mm1,misc_data_offset+8(%arg_z))
	__(jmp *%ra0)
_endfn

/* Make a lisp integer (fixnum or two-digit bignum) from the signed
   64-bit value in %imm0.  Shift left 3 bits - a bit at a time, via 
   addition - and check for overlow after each add, since the overflow
   bit isn't cumulative on x86.
*/
_spentry(makes64)
	__(movq %imm0,%imm1)
	__(addq %imm1,%imm1)
	__(jo 1f)
	__(addq %imm1,%imm1)
	__(jo 1f)
	__(addq %imm1,%imm1)
	__(jo 1f)
	__(movq %imm1,%arg_z)
	__(jmp *%ra0)
1:	__(movq %imm0,%imm1)
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(2)))
	__(movq %imm1,misc_data_offset(%arg_z))
	__(jmp *%ra0)
_endsubp(makes64)	
				
/* %arg_z <- %arg_y + %arg_z.  Do the fixnum case - including overflow -
  inline.  Call out otherwise. */
_spentry(builtin_plus)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(addq %arg_y,%arg_z)
	__(jo C(fix_one_bit_overflow))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_plus,2))
_endsubp(builtin_plus)
	

/* %arg_z <- %arg_z - %arg_y.  Do the fixnum case - including overflow -
  inline.  Call out otherwise. */
_spentry(builtin_minus)			
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(subq %arg_y,%arg_z)
	__(jo C(fix_one_bit_overflow))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_minus,2))
_endsubp(builtin_minus)

/* %arg_z <- %arg_z * %arg_y.  Do the fixnum case - including overflow -
  inline.  Call out otherwise. */
_spentry(builtin_times)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 2f)
	__(unbox_fixnum(%imm0,%arg_z))
	/* 128-bit fixnum result in %imm1:%imm0. Overflow set if %imm1
	   is significant */
	__(imul %arg_y)
	__(jo 1f)
	__(mov %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(unbox_fixnum(%imm0,%arg_z))
	__(unbox_fixnum(%imm1,%arg_y))
	__(imul %imm1)
	__(jmp C(makes128))
2:	__(jump_builtin(_builtin_times,2))
_endsubp(builtin_times)

_spentry(builtin_div)
	__(jump_builtin(_builtin_div,2))

/* %arg_z <- (= %arg_y %arg_z).	*/
_spentry(builtin_eq)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(xor %imm0,%imm0)
	__(cmp %arg_z,%arg_y)
	__(setne %imm0_b)
	__(dec %imm0)
	__(and $t_offset,%imm0)
	__(lea nil_value(%imm0),%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_eq,2))
_endsubp(builtin_eq)
	
/* %arg_z <- (/= %arg_y %arg_z).	*/
_spentry(builtin_ne)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovne %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_ne,2))
_endsubp(builtin_ne)
	
/* %arg_z <- (> %arg_y %arg_z).	*/
_spentry(builtin_gt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovl %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_gt,2))
_endsubp(builtin_gt)

/* %arg_z <- (>= %arg_y %arg_z).	*/
_spentry(builtin_ge)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovge %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_ge,2))
_endsubp(builtin_ge)
	
/* %arg_z <- (< %arg_y %arg_z).	*/
_spentry(builtin_lt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovl %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_lt,2))
_endsubp(builtin_lt)

/* %arg_z <- (<= %arg_y %arg_z). */
_spentry(builtin_le)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovle %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_le,2))
_endsubp(builtin_le)

_spentry(builtin_eql)
_endsubp(builtin_eql)

_spentry(builtin_length)
_endsubp(builtin_length)

_spentry(builtin_seqtype)
_endsubp(builtin_seqtype)

_spentry(builtin_assq)
_endsubp(builtin_assq)	

_spentry(builtin_memq)
_endsubp(builtin_memq)

_spentry(builtin_logbitp)
_endsubp(builtin_logbitp)
			
