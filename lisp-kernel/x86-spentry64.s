/*
   Copyright (C) 2005-2006 Clozure Associates and contributors
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
	ref_nrs_value(builtin_functions,%fname)
	set_nargs($2)
	vrefr(%fname,%fname,$1)
	jump_fname()
])

_spentry(bad_funcall)	
	.globl C(bad_funcall)	
__(tra(C(bad_funcall)))
	__(uuo_error_not_callable)
_endsubp(bad_funcall)
	
/* %arg_z has overflowed by one bit.  Make a bignum with 2 (32-bit) digits. */
_spentry(fix_overflow)
C(fix_one_bit_overflow):	
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed([],aligned_bignum_size(2)))
	__(unbox_fixnum(%arg_z,%imm0))
	__(movq $0xe000000000000000,%imm1)
	__(mov %temp0,%arg_z)
	__(xorq %imm1,%imm0)
	__(movq %imm0,misc_data_offset(%arg_z))
	__(jmp *%ra0)	
_endsubp(fix_overflow)

	
/* Make a lisp integer (fixnum or two-digit bignum) from the signed
   64-bit value in %imm0.  Shift left 3 bits - a bit at a time, via 
   addition - and check for overlow after each add, since the overflow
   bit isn't cumulative on x86.
*/
_spentry(makes64)
	__(movq %imm0,%imm1)
	__(shlq $fixnumshift,%imm1)
	__(movq %imm1,%arg_z)
	__(sarq $fixnumshift,%imm1)
	__(cmpq %imm1,%imm0)
	__(jz,pt 0f)
0:	__(jmp *%ra0)
1:	__(movd %imm0,%mm0)
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(2)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(jmp *%ra0)
_endsubp(makes64)	
				

/* %imm1:%imm0 constitute a signed integer, almost certainly a bignum.
   Make a lisp integer out of those 128 bits .. */
_startfn(C(makes128))
/* We're likely to have to make a bignum out of the integer in %imm1 and
   %imm0. We'll need to use %imm0 and %imm1 to cons the bignum, and
   will need to do some arithmetic (determining significant bigits)
   on %imm0 and %imm1 in order to know how large that bignum needs to be.
   Cache %imm0 and %imm1 in %mm0 and %mm1. */
   
	__(movd %imm0,%mm0)
	__(movd %imm1,%mm1)
	
/* If %imm1 is just a sign extension of %imm0, make a 64-bit signed integer. */
	
	__(sarq $63,%imm0) 
	__(cmpq %imm0,%imm1)
	__(movd %mm0,%imm0)
	__(je _SPmakes64)
	
/* Otherwise, if the high 32 bits of %imm1 are a sign-extension of the
   low 32 bits of %imm1, make a 3-digit bignum.  If the upper 32 bits
   of %imm1 are significant, make a 4 digit bignum */
	__(movq %imm1,%imm0)
	__(shlq $32,%imm0)
	__(sarq $32,%imm0)
	__(cmpq %imm0,%imm1)
	__(jz 3f)
	__(mov $four_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(4)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movq %mm1,misc_data_offset+8(%arg_z))
	__(jmp *%ra0)
3:	__(mov $three_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(3)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movd %mm1,misc_data_offset+8(%arg_z))
	__(jmp *%ra0)
_endfn

/* %imm1:%imm0 constitute an unsigned integer, almost certainly a bignum.
   Make a lisp integer out of those 128 bits .. */
_startfn(C(makeu128))
/* We're likely to have to make a bignum out of the integer in %imm1 and
   %imm0. We'll need to use %imm0 and %imm1 to cons the bignum, and
   will need to do some arithmetic (determining significant bigits)
   on %imm0 and %imm1 in order to know how large that bignum needs to be.
   Cache %imm0 and %imm1 in %mm0 and %mm1. */

/* If the high word is 0, make an unsigned-byte 64 ... */	
	__(testq %imm1,%imm1)
	__(jz _SPmakeu64)
	
	__(movd %imm0,%mm0)
	__(movd %imm1,%mm1)

	__(js 5f)		/* Sign bit set in %imm1. Need 5 digits */
	__(bsrq %imm1,%imm0)
	__(rcmpb(%imm0_b,$31))
	__(jae 4f)		/* Some high bits in %imm1.  Need 4 digits */
	__(testl %imm1_l,%imm1_l)
	__(movd %mm0,%imm0)
	__(jz _SPmakeu64)
	/* Need 3 digits */
	__(movq $three_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(3)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movd %mm1,misc_data_offset+8(%arg_z))
	__(jmp *%ra0)
4:	__(movq $four_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(4)))
	__(jmp 6f)
5:	__(movq $five_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(5)))
6:	__(movq %mm0,misc_data_offset(%arg_z))
	__(movq %mm0,misc_data_offset+8(%arg_z))
	__(jmpq *%ra0)
_endfn

_spentry(misc_ref)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_y_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(je,pt 0f)
	__(uuo_error_reg_not_tag(Rarg_y,tag_misc))
0:	__(testb $fixnummask,%arg_z_b)
	__(je,pt 1f)
	__(uuo_error_reg_not_fixnum(Rarg_z))
1:	__(movq %arg_z,%imm0)
	__(shlq $num_subtag_bits-fixnumshift,%imm0)
	__(movb $-1,%imm0_b)
	__(cmpq misc_header_offset(%arg_y),%imm0)
	__(jb 2f)
	/* This is a vector bounds trap, which is hard to encode */
	.byte 0xcd,0xc8,0xf7
2:	__(movb misc_subtag_offset(%arg_y),%imm1_b)
_endsubp(misc_ref)
	
/* %imm1.b = subtag, %arg_y = uvector, %arg_z = index.
   Bounds/type-checking done in caller */	
_startfn(C(misc_ref_common))
	__(extract_fulltag(%imm1,%imm0))
	__(cmpb $ivector_class_64_bit,%imm0_b)
	__(je local_label(misc_ref_64))
	__(cmpb $ivector_class_32_bit,%imm0_b)
	__(je local_label(misc_ref_32))
	__(cmpb $ivector_class_other_bit,%imm0_b)
	__(je local_label(misc_ref_other))
	/* Node vector.  Functions are funny: the first  N words
	   are treated as (UNSIGNED-BYTE 64), where N is the low
	   32 bits of the first word. */
	__(cmpb $subtag_function,%imm1_b)
	__(jne local_label(misc_ref_node))
	__(movl misc_data_offset(%arg_y),%imm0_l)
	__(shl $fixnumshift,%imm0)
	__(rcmpq(%arg_z,%imm0))
	__(jb local_label(misc_ref_u64))
local_label(misc_ref_node):
	__(movq misc_data_offset(%arg_y,%arg_z),%arg_z)
	__(jmp *%ra0)
local_label(misc_ref_u64):
	__(movq misc_data_offset(%arg_y,%arg_z),%imm0)
	__(jmp _SPmakeu64)
local_label(misc_ref_double_float_vector):
	__(movsd misc_data_offset(%arg_y,%arg_z),%fp1)
	__(movq $double_float_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,double_float.size))
	__(movsd %fp1,double_float.value(%arg_z))
	__(jmp *%ra0)
local_label(misc_ref_64):
	__(cmpb $subtag_double_float_vector,%imm1_b)
	__(je local_label(misc_ref_double_float_vector))
	__(cmpb $subtag_s64_vector,%imm0_b)
	__(jne local_label(misc_ref_u64))
local_label(misc_ref_s64):	
	__(movq misc_data_offset(%arg_y,%arg_z),%imm0)
	__(jmp _SPmakes64)
local_label(misc_ref_u32):
	__(movl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(box_fixnum(%imm0,%arg_z))
	__(jmp *%ra0)
local_label(misc_ref_s32):
	__(movslq misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(jmp *%ra0)
local_label(misc_ref_32):
	__(movq %arg_z,%imm0)
	__(shr $1,%imm0)
	__(cmpb $subtag_s32_vector,%imm1_b)
	__(je local_label(misc_ref_s32))
	__(cmpb $subtag_single_float_vector,%imm1_b)
	__(jne local_label(misc_ref_u32))
local_label(misc_ref_single_float_vector):
	__(movsd misc_data_offset(%arg_y,%imm0),%fp1)
	__(movd %fp1,%imm0_l)
	__(shl $32,%imm0)
	__(lea subtag_single_float(%imm0),%arg_z)
	__(jmp *%ra0)
local_label(misc_ref_other):
	__(cmpb $subtag_u16_vector,%imm1_b)
	__(jle local_label(misc_ref_16))
	__(cmpb $subtag_bit_vector,%imm1_b)
	__(jz local_label(misc_ref_bit_vector))
	/* 8-bit case:	string, u8, s8 */
	__(movq %arg_z,%imm0)
	__(shr $3,%imm0)
	__(cmpb $subtag_s8_vector,%imm1_b)
	__(je local_label(misc_ref_s8))
	__(jl local_label(misc_ref_string))
local_label(misc_ref_u8):
	__(movzbl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(box_fixnum(%imm0,%arg_z))
	__(jmp *%ra0)
local_label(misc_ref_s8):	
	__(movsbq misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(jmp *%ra0)
local_label(misc_ref_string):
	__(movzbl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(shlq $charcode_shift,%imm0)
	__(leaq subtag_character(%imm0),%arg_z)
	__(jmp *%ra0)
local_label(misc_ref_16):
	__(movq %arg_z,%imm0)
	__(shrq $2,%imm0)
	__(cmpb $subtag_s16_vector,%imm1_b)
	__(je local_label(misc_ref_s16))
local_label(misc_ref_u16):	
	__(movzwl misc_data_offset(%arg_y,%imm0),%imm0_l)
	__(box_fixnum(%imm0,%arg_z))
	__(jmp *%ra0)
local_label(misc_ref_s16):	
	__(movswq misc_data_offset(%arg_y,%imm0),%imm0)
	__(box_fixnum(%imm0,%arg_z))
	__(jmp *%ra0)
local_label(misc_ref_bit_vector):
	__(unbox_fixnum(%arg_z,%imm0))
	__(movl $63,%imm1_l)
	__(andb %imm0_b,%imm1_b)
	__(shrq $6,%imm0)
	__(btq %imm1,misc_data_offset(%arg_y,%imm0,8))
	__(setc %imm0_b)
	__(andl $fixnum_one,%imm0_l)
	__(movq %imm0,%arg_z)
	__(jmp *%ra0)			
_endfn(C(misc_ref_common))

/* like misc_ref, only the boxed subtag is in arg_x. 
*/					
_spentry(subtag_misc_ref)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_y_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(je,pt 0f)
	__(uuo_error_reg_not_tag(Rarg_y,tag_misc))
0:	__(testb $fixnummask,%arg_z_b)
	__(je,pt 1f)
	__(uuo_error_reg_not_fixnum(Rarg_z))
1:	__(movq %arg_z,%imm0)
	__(shlq $num_subtag_bits-fixnumshift,%imm0)
	__(movb $255,%imm0_b)
	__(cmpq misc_header_offset(%arg_y),%imm0)
	__(jb 2f)
	/* This is a vector bounds trap, which is hard to encode */
	.byte 0xcd,0xc8,0xf7
2:	__(unbox_fixnum(%arg_x,%imm1))
	__(jmp C(misc_ref_common))
_endsubp(subtag_misc_ref)

_spentry(subtag_misc_set)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_x_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(je,pt 0f)
	__(uuo_error_reg_not_tag(Rarg_x,tag_misc))
0:	__(testb $fixnummask,%arg_y_b)
	__(je,pt 1f)
	__(uuo_error_reg_not_fixnum(Rarg_y))
1:	__(movq %arg_y,%imm0)
	__(shlq $num_subtag_bits-fixnumshift,%imm0)
	__(movb $255,%imm0_b)
	__(cmpq misc_header_offset(%arg_x),%imm0)
	__(jb 2f)
	/* This is a vector bounds trap, which is hard to encode */
	.byte 0x49,0xcd,0xc8,0xf8
2:	__(unbox_fixnum(%temp0,%imm1))
	__(jmp C(misc_set_common))
_endsubp(subtag_misc_set)

_spentry(misc_set)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_x_b,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(je,pt 0f)
	__(uuo_error_reg_not_tag(Rarg_x,tag_misc))
0:	__(testb $fixnummask,%arg_y_b)
	__(je,pt 1f)
	__(uuo_error_reg_not_fixnum(Rarg_y))
1:	__(movq %arg_y,%imm0)
	__(shlq $num_subtag_bits-fixnumshift,%imm0)
	__(movb $255,%imm0_b)
	__(cmpq misc_header_offset(%arg_x),%imm0)
	__(jb 2f)
	/* This is a vector bounds trap, which is hard to encode */
	.byte 0x49,0xcd,0xc8,0xf8
2:	__(movb misc_subtag_offset(%arg_x),%imm1_b)
	/*	__(jmp C(misc_set_common)) */
_endsubp(misc_set)
		
_startfn(C(misc_set_common))
	__(extract_fulltag(%imm1,%imm0))
	__(cmpb $ivector_class_64_bit,%imm0_b)
	__(je local_label(misc_set_64))
	__(cmpb $ivector_class_32_bit,%imm0_b)
	__(je local_label(misc_set_32))
	__(cmpb $ivector_class_other_bit,%imm0_b)
	__(je local_label(misc_set_other))
	/* Node vector.  Functions are funny: the first  N words
	   are treated as (UNSIGNED-BYTE 64), where N is the low
	   32 bits of the first word. */
	__(cmpb $subtag_function,%imm1_b)
	__(jne _SPgvset)
	__(movl misc_data_offset(%arg_x),%imm0_l)
	__(shl $fixnumshift,%imm0)
	__(rcmpq(%arg_y,%imm0))
	__(jae _SPgvset)
local_label(misc_set_u64):
	__(movq $~(target_most_positive_fixnum << fixnumshift),%imm0)
	__(testq %arg_z,%imm0)
	__(movq %arg_z,%imm0)
	__(jne 1f)
	__(sarq $fixnumshift,%imm0)
	__(jmp 9f)
1:	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(cmovew misc_subtag_offset(%arg_z),%imm0_w)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $three_digit_bignum_header,%imm0)
	__(je 3f)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(jne local_label(misc_set_bad))
	__(movq misc_data_offset(%arg_z),%imm0)
	__(testq %imm0,%imm0)
	__(js local_label(misc_set_bad))
	__(jmp 9f)
3:	__(movq misc_data_offset(%arg_z),%imm0)
	__(cmpl $0,misc_data_offset+8(%arg_z))
	__(jne local_label(misc_set_bad))
9:	__(movq %imm0,misc_data_offset(%arg_x,%arg_y))
	__(jmp *%ra0)	
local_label(misc_set_s64):
	__(movq %arg_z,%imm0)
	__(sarq $fixnumshift,%imm0)
	__(testb $fixnummask,%arg_z_b)
	__(je 9f)
1:	__(movb %arg_z_b,%imm0_b)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(cmovew misc_subtag_offset(%arg_z),%imm0_w)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(movq misc_data_offset(%arg_z),%imm0)
	__(jne local_label(misc_set_bad))
9:	__(movq %imm0,misc_data_offset(%arg_x,%arg_y))
	__(jmp *%ra0)	
local_label(misc_set_bad):
	__(movq %arg_z,%arg_y)
	__(movq %arg_x,%arg_z)
	__(movq $XNOTELT,%arg_x)
	__(set_nargs(3))
	__(jmp _SPksignalerr)
local_label(misc_set_dfloat_vector):	
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_misc,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movb misc_subtag_offset(%arg_z),%imm0_b)
	__(cmpb $subtag_double_float,%imm0_b)
	__(jne local_label(misc_set_bad))
	__(movq double_float.value(%arg_z),%imm0)
	__(movq %imm0,misc_dfloat_offset(%arg_x,%arg_y))
	__(jmp *%ra0)
local_label(misc_set_64):	
	__(cmpb $subtag_double_float_vector,%imm1_b)
	__(je local_label(misc_set_dfloat_vector))
	__(cmpb $subtag_s64_vector,%imm1_b)
	__(je local_label(misc_set_s64))
	__(jmp local_label(misc_set_u64))
local_label(misc_set_s32):	
	__(movq %arg_z,%imm0)
	__(shlq $64-(32+fixnumshift),%imm0)
	__(shrq $64-(32+fixnumshift),%imm0)
	__(cmpq %imm0,%arg_z)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(shr $fixnumshift,%imm0)
	__(movl %imm0_l,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
local_label(misc_set_single_float):	
	__(cmpb $tag_single_float,%arg_z_b)
	__(movq %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(shr $32,%imm0)
	__(movl %imm0_l,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
local_label(misc_set_32):
	__(movq %arg_y,%imm1)
	__(shrq %imm1)
	__(cmpb $subtag_s32_vector,%imm0_b)
	__(je local_label(misc_set_s32))
	__(cmpb $subtag_single_float_vector,%imm0_b)
	__(je local_label(misc_set_single_float))
local_label(misc_set_u32):	
	__(movq $~(0xffffffff<<fixnumshift),%imm0)
	__(testq %imm0,%arg_z)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_z,%imm0))
	__(movl %imm0_l,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
local_label(misc_set_other):
	__(cmpb $subtag_bit_vector,%imm0_b)
	__(jne local_label(misc_set_other_not_bit_vector))
	__(testq $~fixnumone,%arg_z)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_y,%imm1))
	__(movb $63,%imm0_b)
	__(andb %imm1_b,%imm0_b)
	__(shrq $6,%imm1)
	__(testb %arg_z_b,%arg_z_b)
	__(je local_label(misc_set_clr_bit))
local_label(misc_set_set_bit):	
	__(btsq %imm0,misc_data_offset(%arg_x,%imm1,8))
	__(jmp *%ra0)
local_label(misc_set_clr_bit):	
	__(btcq %imm0,misc_data_offset(%arg_x,%imm1,8))
	__(jmp *%ra0)
local_label(misc_set_other_not_bit_vector):
	__(cmpb $subtag_simple_base_string,%imm0_b)
	__(jb local_label(misc_set_16))
	__(je local_label(misc_set_char))
	__(cmpb $subtag_s8_vector,%imm0_b)
	__(je local_label(misc_set_s8))
local_label(misc_set_u8):	
	__(testq $~(0xff<<fixnumshift),%arg_z)
	__(jne local_label(misc_set_bad))
	__(movq %arg_y,%imm1)
	__(shrq $3,%imm1)
	__(unbox_fixnum(%arg_z,%imm0))
	__(movb %imm0_b,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
local_label(misc_set_s8):
	__(movq %arg_z,%imm0)
	__(shlq $64-(8+fixnumshift),%imm0)	
	__(sarq $64-(8+fixnumshift),%imm0)
	__(cmpq %arg_z,%temp0)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(shrq $fixnumshift,%imm0)
	__(movq %arg_y,%imm1)
	__(shrq $3,%imm1)
	__(movb %imm0_b,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
local_label(misc_set_char):
	__(cmpb $subtag_character,%arg_z_b)
	__(movq %arg_z,%imm0)
	__(jne local_label(misc_set_bad))
	__(shrq $charcode_shift,%imm0)
	__(movq %arg_y,%imm1)
	__(shrq $3,%imm1)
	__(movb %imm0_b,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
local_label(misc_set_16):
	__(movq %arg_y,%imm1)
	__(shrq $2,%imm1)
	__(cmpb $subtag_u16_vector,%imm0_b)
	__(je local_label(misc_set_u16))
local_label(misc_set_s16):	
	__(shlq $64-(16+fixnumshift),%imm0)	
	__(sarq $64-(16+fixnumshift),%imm0)
	__(cmpq %arg_z,%temp0)
	__(jne local_label(misc_set_bad))
	__(testb $fixnummask,%arg_z_b)
	__(jne local_label(misc_set_bad))
	__(shrq $fixnumshift,%imm0)
	__(movw %imm0_w,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
local_label(misc_set_u16):
	__(testq $(0xffff<<fixnumshift),%arg_z)
	__(jne local_label(misc_set_bad))
	__(unbox_fixnum(%arg_z,%imm0))
	__(movw %imm0_w,misc_data_offset(%arg_x,%imm1))
	__(jmp *%ra0)
_endfn(C(misc_set_common))
	
/* ret1valn returns "1 multiple value" when a called function does not */
/* return multiple values.  Its presence on the stack (as a return address) */
/* identifies the stack frame to code which returns multiple values. */

_exportfn(C(ret1valn))
	__(leaveq)
	__(pop %ra0)
	__(push %arg_z)
	__(set_nargs(1))
	__(jmpq *%ra0)
_endfn
	

_spentry(nvalret)
	.globl C(nvalret)			
C(nvalret):	
	__(ref_global(ret1val_addr,%temp1))
	__(cmpq lisp_frame.savera0(%rbp),%temp1)
	__(je 1f)
	__(testw %nargs,%nargs)
	__(movzwl %nargs,%nargs_l)
	__(movl $nil_value,%arg_z_l)
	__(cmovneq -node_size(%rsp,%nargs_q),%arg_z)
	__(leaveq)
	__(popq %ra0)
	__(jmp *%ra0)
	
/* actually need to return values ; always need to copy */
1:	__(leaq 8(%rbp),%imm1)
	__(movq (%imm1),%ra0)
	__(movq 0(%rbp),%rbp)
	__(leaq (%rsp,%nargs_q),%temp0)
	__(xorl %imm0_l,%imm0_l)
	__(jmp 3f)
2:	__(movq -node_size(%temp0),%temp1)
	__(subq $node_size,%temp1)
	__(addq $node_size,%imm0)
	__(movq %temp1,-node_size(%imm1))
	__(subq $node_size,%imm1)
3:	__(cmpw %imm0_w,%nargs)
	__(jne 2b)
	__(movq %imm1,%rsp)
	__(jmp *%ra0)	
_endsubp(nvalret)
	
_spentry(jmpsym)
	__(jump_fname())
_endsubp(jmpsym)

_spentry(jmpnfn)
	__(movq %temp0,%fn)
	__(jmp *%fn)
_endsubp(jmpnfn)

_spentry(funcall)
	__(do_funcall())
_endsubp(funcall)

_spentry(mkcatch1v)
	__(Make_Catch(0))
	__(jmp *%ra0)
_endsubp(mkcatch1v)

_spentry(mkunwind)
	__(movq $undefined,%arg_z)
	__(Make_Catch(fixnumone))
	__(jmp *%ra0)
_endsubp(mkunwind)

_spentry(mkcatchmv)
	__(Make_Catch(fixnumone))
	__(jmp *%ra0)
_endsubp(mkcatchmv)

_spentry(throw)
	__(movq %rcontext:tcr.catch_top,%imm1)
	__(xorl %imm0_l,%imm0_l)
	__(movzwl %nargs,%nargs_l)
	__(movq (%rsp,%nargs_q),%temp0)	/* temp0 = tag */
	__(jmp local_label(_throw_test))
local_label(_throw_loop):
	__(cmpq %temp0,catch_frame.catch_tag(%imm1))
	__(je local_label(_throw_found))
	__(movq catch_frame.link(%imm1),%imm1)
	__(addq $fixnum_one,%imm0)
local_label(_throw_test):
	__(testq %imm1,%imm1)
	__(jne local_label(_throw_loop))
	__(uuo_error_reg_not_tag(Rtemp0,subtag_catch_frame))
	__(jmp _SPthrow)
local_label(_throw_found):	
	__(testb $fulltagmask,catch_frame.mvflag(%imm1))
	__(jne local_label(_throw_multiple))
	__(testw %nargs,%nargs)
	__(movl $nil_value,%arg_z_l)
	__(je local_label(_throw_one_value))
	__(movq -node_size(%rsp,%nargs_q),%arg_z)
	__(add %nargs_q,%rsp)
local_label(_throw_one_value):
	__(lea local_label(_threw_one_value)(%rip),%ra0)
	__(jmp _SPnthrow1value)
__(tra(local_label(_threw_one_value)))
	__(movq %rcontext:tcr.catch_top,%temp0)
	__(movq catch_frame.db_link(%temp0),%imm1)
	__(cmpq %imm0,%imm1)
	__(jz local_label(_threw_one_value_dont_unbind))
	__(push %ra0)
	__(lea local_label(_threw_one_value_back_from_unbind)(%rip),%ra0)
	__(jmp _SPunbind_to)
__(tra(local_label(_threw_one_value_back_from_unbind)))
	__(pop %ra0)
local_label(_threw_one_value_dont_unbind):
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame.rsp(%temp0),%rsp)
	__(movq catch_frame.foreign_sp(%temp0),%imm0)
	__(movq catch_frame.xframe(%temp0),%imm1)
	__(movq %imm0,%rcontext:tcr.foreign_sp)
	__(movq %imm1,%rcontext:tcr.xframe)
	__(movq catch_frame.link(%temp0),%imm1)
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save2(%temp0),%save2)
	__(movq catch_frame._save3(%temp0),%save3)
	__(movq %imm1,%rcontext:tcr.catch_top)
	__(movq catch_frame.pc(%temp0),%ra0)
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
	__(movq (%imm1),%tsp)
	__(movq %tsp,%next_tsp)
	__(jmp *%ra0)
local_label(_throw_multiple):
	__(lea local_label(_threw_multiple)(%rip),%ra0)
	__(jmp _SPnthrowvalues)
__(tra(local_label(_threw_multiple)))
	__(movq %rcontext:tcr.catch_top,%temp0)
	__(movq catch_frame.db_link(%temp0),%imm0)
			

		
_endsubp(throw)

/* This takes N multiple values atop the vstack. */
_spentry(nthrowvalues)
	__(movb $1,%rcontext:tcr.unwinding)
	__(movzwl %nargs,%nargs_l)
local_label(_nthrowv_nextframe):
	__(subq $fixnumone,%imm0)
	__(js local_label(_nthrowv_done))
	__(movd %imm0,%mm1)
	__(movq %rcontext:tcr.catch_top,%temp0)
	__(movq catch_frame.link(%temp0),%imm1)
	__(movq catch_frame.db_link(%temp0),%imm0)
	__(movq %imm1,%rcontext:tcr.catch_top)
	__(cmpq %imm0,%rcontext:tcr.db_link)
	__(jz local_label(_nthrowv_dont_unbind))
	__(push %ra0)
	__(leaq local_label(_nthrowv_back_from_unbind)(%rip),%ra0)
	__(jmp _SPunbind_to)
__(tra(local_label(_nthrowv_back_from_unbind)))

	__(pop %ra0)
local_label(_nthrowv_dont_unbind):
	__(cmpb $unbound_marker,catch_frame.catch_tag(%temp0))
	__(je local_label(_nthrowv_do_unwind))
/* A catch frame.  If the last one, restore context from there. */
	__(movd %mm1,%imm0)
	__(testq %imm0,%imm0)	/* last catch frame ? */
	__(jz local_label(_nthrowv_skip))
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,%rcontext:tcr.xframe)
	__(leaq (%rsp,%nargs_q),%save1)
	__(movq catch_frame.rsp(%temp0),%save2)
	__(movq %nargs_q,%save0)
	__(jmp local_label(_nthrowv_push_test))
local_label(_nthrowv_push_loop):
	__(subq $node_size,%save1)
	__(subq $node_size,%save2)
	__(movq (%save1),%temp1)
	__(movq %temp1,(%save2))
local_label(_nthrowv_push_test):
	__(subq $node_size,%save0)
	__(jns local_label(_nthrowv_push_loop))
	__(movq %save2,%rsp)
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame._save3(%temp0),%save3)
	__(movq catch_frame._save2(%temp0),%save2)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame.foreign_sp(%temp0),%imm1)
	__(movq %imm1,%rcontext:tcr.foreign_sp)
local_label(_nthrowv_skip):	
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
	__(movd %imm1,%tsp)
	__(movd %imm1,%next_tsp)
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrowv_nextframe))
local_label(_nthrowv_do_unwind):	
/* This is harder.  Call the cleanup code with the multiple values and 
    nargs, the throw count, and the caller's return address in a temp
    stack frame. */
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,%rcontext:tcr.xframe)
	__(leaq (%rsp,%nargs_q),%save1)
	__(push catch_frame._save0(%temp0))
	__(push catch_frame._save1(%temp0))
	__(push catch_frame._save2(%temp0))
	__(push catch_frame._save3(%temp0))
	__(push catch_frame.pc(%temp0))
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame.rsp(%temp0),%temp1)
	__(movq catch_frame.foreign_sp(%temp0),%imm0)
	__(movq %imm0,%rcontext:tcr.foreign_sp)
	/* Discard the catch frame, so we can build a temp frame */
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
	__(movd %imm1,%tsp)
	__(movd %imm1,%next_tsp)
	/* tsp overhead, nargs, throw count, ra0 */
	__(dnode_align(%nargs_q,(tsp_frame.fixed_overhead+(3*node_size)),%imm0))
	__(TSP_Alloc_Var(%imm0,%imm1))
	__(movq %nargs_q,%temp0)
	__(movq %nargs_q,(%imm1))
	__(movq %ra0,node_size(%imm1))
	__(movq %mm1,node_size*2(%imm1))
	__(leaq node_size*3(%imm1),%imm1)
	__(jmp local_label(_nthrowv_tpushtest))
local_label(_nthrowv_tpushloop):
	__(movq -node_size(%save0),%temp0)
	__(subq $node_size,%save0)
	__(movq %temp0,(%imm1))
	__(addq $node_size,%imm1)
local_label(_nthrowv_tpushtest):
	__(subw $node_size,%nargs)
	__(jns local_label(_nthrowv_tpushloop))
	__(pop %xfn)
	__(pop %save3)
	__(pop %save2)
	__(pop %save1)
	__(pop %save0)
	__(movq %temp1,%rsp)
/* Ready to call cleanup code: set up tra, jmp to %xfn */
	__(leaq local_label(_nthrowv_called_cleanup)(%rip),%ra0)
	__(movb $0,%rcontext:tcr.unwinding)
	__(jmp *%xfn)
__(tra(local_label(_nthrowv_called_cleanup)))

	__(movb $1,%rcontext:tcr.unwinding)
	__(movd %tsp,%imm1)
	__(movq tsp_frame.data_offset+(0*node_size)(%imm1),%nargs_q)
	__(movq tsp_frame.data_offset+(1*node_size)(%imm1),%ra0)
	__(movq tsp_frame.data_offset+(2*node_size)(%imm1),%mm1)
	__(movq %nargs_q,%imm0)
	__(leaq node_size*3(%imm1),%imm1)
	__(jmp local_label(_nthrowv_tpoptest))
local_label(_nthrowv_tpoploop):	
	__(push (%imm1))
	__(addq $node_size,%imm1)
local_label(_nthrowv_tpoptest):	
	__(subq $node_size,%imm0)
	__(jns local_label(_nthrowv_tpoploop))
	__(movd %tsp,%imm1)
	__(movq (%imm1),%imm1)
	__(movd %imm1,%tsp)
	__(movd %imm1,%next_tsp)
	__(movd %mm1,%temp0)
	__(jmp local_label(_nthrowv_nextframe))
local_label(_nthrowv_done):
	__(movb $0,%rcontext:tcr.unwinding)
	__(movq %rcontext:tcr.tlb_pointer,%imm0)
	__(cmpq $0,INTERRUPT_LEVEL_BINDING_INDEX(%imm1))
	__(js local_label(_nthrowv_return))
	__(cmpq $0,%rcontext:tcr.interrupt_pending)
	__(je local_label(_nthrowv_return))
	__(interrupt_now())
local_label(_nthrowv_return):	
	__(jmp *%ra0)	
_endsubp(nthrowvalues)

/* This is a (slight) optimization.  When running an unwind-protect,
   save the single value and the throw count in the tstack frame.
   Note that this takes a single value in arg_z. */
_spentry(nthrow1value)
	__(movb $1,%rcontext:tcr.unwinding)
	__(movzwl %nargs,%nargs_l)
local_label(_nthrow1v_nextframe):
	__(subq $fixnumone,%imm0)
	__(js local_label(_nthrow1v_done))
	__(movd %imm0,%mm1)
	__(movq %rcontext:tcr.catch_top,%temp0)
	__(movq catch_frame.link(%temp0),%imm1)
	__(movq catch_frame.db_link(%temp0),%imm0)
	__(movq %imm1,%rcontext:tcr.catch_top)
	__(cmpq %imm0,%rcontext:tcr.db_link)
	__(jz local_label(_nthrow1v_dont_unbind))
	__(push %ra0)
	__(leaq local_label(_nthrow1v_back_from_unbind)(%rip),%ra0)
	__(jmp _SPunbind_to)
__(tra(local_label(_nthrow1v_back_from_unbind)))

	__(pop %ra0)
local_label(_nthrow1v_dont_unbind):
	__(cmpb $unbound_marker,catch_frame.catch_tag(%temp0))
	__(je local_label(_nthrow1v_do_unwind))
/* A catch frame.  If the last one, restore context from there. */
	__(movd %mm1,%imm0)
	__(testq %imm0,%imm0)	/* last catch frame ? */
	__(jz local_label(_nthrow1v_skip))
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,%rcontext:tcr.xframe)
	__(leaq (%rsp,%nargs_q),%save1)
	__(movq catch_frame.rsp(%temp0),%save2)
	__(movq %nargs_q,%save0)
	__(jmp local_label(_nthrow1v_push_test))
local_label(_nthrow1v_push_loop):
	__(subq $node_size,%save1)
	__(subq $node_size,%save2)
	__(movq (%save1),%temp1)
	__(movq %temp1,(%save2))
local_label(_nthrow1v_push_test):
	__(subq $node_size,%save0)
	__(jns local_label(_nthrow1v_push_loop))
	__(movq %save2,%rsp)
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame._save3(%temp0),%save3)
	__(movq catch_frame._save2(%temp0),%save2)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame.foreign_sp(%temp0),%imm1)
	__(movq %imm1,%rcontext:tcr.foreign_sp)
local_label(_nthrow1v_skip):	
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
	__(movd %imm1,%tsp)
	__(movd %imm1,%next_tsp)
	__(movd %mm1,%imm0)
	__(jmp local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_do_unwind):
/* This is harder, but not as hard (not as much BLTing) as the
   multiple-value case. */
	__(movq catch_frame.xframe(%temp0),%save0)
	__(movq %save0,%rcontext:tcr.xframe)
	__(movq catch_frame._save0(%temp0),%save0)
	__(movq catch_frame._save1(%temp0),%save1)
	__(movq catch_frame._save2(%temp0),%save2)
	__(movq catch_frame._save3(%temp0),%save3)
	__(movq catch_frame.pc(%temp0),%xfn)
	__(movq catch_frame.rbp(%temp0),%rbp)
	__(movq catch_frame.rsp(%temp0),%rsp)
	__(movq catch_frame.foreign_sp(%temp0),%imm0)
	__(movq %imm0,%rcontext:tcr.foreign_sp)
	/* Discard the catch frame, so we can build a temp frame */
	__(lea -(tsp_frame.fixed_overhead+fulltag_misc)(%temp0),%imm1)
	__(movd %imm1,%tsp)
	__(movd %imm1,%next_tsp)
	/* tsp overhead, throw count, ra0, arg_z */
	__(dnode_align(%nargs_q,(tsp_frame.fixed_overhead+(3*node_size)),%imm0))
	__(TSP_Alloc_Fixed((2*node_size),%imm1))
	__(addq $tsp_frame.fixed_overhead,%imm1)
	__(movq %ra0,(%imm1))
	__(movq %mm1,node_size*1(%imm1))
	__(movq %arg_z,node_size*2(%imm1))
/* Ready to call cleanup code: set up tra, jmp to %xfn */
	__(leaq local_label(_nthrow1v_called_cleanup)(%rip),%ra0)
	__(movb $0,%rcontext:tcr.unwinding)
	__(jmp *%xfn)
__(tra(local_label(_nthrow1v_called_cleanup)))

	__(movb $1,%rcontext:tcr.unwinding)
	__(movd %tsp,%imm1)
	__(movq tsp_frame.data_offset+(0*node_size)(%imm1),%ra0)
	__(movq tsp_frame.data_offset+(1*node_size)(%imm1),%mm1)
	__(movq tsp_frame.data_offset+(2+node_size)(%imm1),%arg_z)
	__(movd %tsp,%imm1)
	__(movq (%imm1),%imm1)
	__(movd %imm1,%tsp)
	__(movd %imm1,%next_tsp)
	__(movd %mm1,%temp0)
	__(jmp local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_done):
	__(movb $0,%rcontext:tcr.unwinding)
	__(movq %rcontext:tcr.tlb_pointer,%imm0)
	__(cmpq $0,INTERRUPT_LEVEL_BINDING_INDEX(%imm1))
	__(js local_label(_nthrow1v_return))
	__(cmpq $0,%rcontext:tcr.interrupt_pending)
	__(je local_label(_nthrow1v_return))
	__(interrupt_now())
local_label(_nthrow1v_return):	
	__(jmp *%ra0)	
_endsubp(nthrow1value)

/* This never affects the symbol's vcell */
/* Non-null symbol in arg_y, new value in arg_z */        
	
_spentry(bind)
	__(movq symbol.binding_index(%arg_y),%temp0)
	__(cmpq %rcontext:tcr.tlb_limit,%temp0)
	__(jb,pt 0f)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq %rcontext:tcr.tlb_pointer,%temp1)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push %rcontext:tcr.db_link)
	__(movq %arg_z,(%temp1,%temp0))
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
9:	
	__(movq %arg_y,%arg_z)
	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)	
_endsubp(bind)

/* arg_z = symbol: bind it to its current value */        
_spentry(bind_self)
	__(movq symbol.binding_index(%arg_z),%temp0)
	__(cmpq %rcontext:tcr.tlb_limit,%temp0)
	__(jb,pt 0f)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq %rcontext:tcr.tlb_pointer,%temp1)
	__(cmpb $no_thread_local_binding_marker,(%temp0,%temp1))
	__(jz 2f)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push %rcontext:tcr.db_link)
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
2:	__(movq symbol.vcell(%arg_z),%arg_y)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push %rcontext:tcr.db_link)
	__(movq %arg_y,(%temp1,%temp0))
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
9:	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
_endsubp(bind_self)

_spentry(bind_nil)
	__(movq symbol.binding_index(%arg_z),%temp0)
	__(cmpq %rcontext:tcr.tlb_limit,%temp0)
	__(jb,pt 0f)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq %rcontext:tcr.tlb_pointer,%temp1)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push %rcontext:tcr.db_link)
	__(movq $nil_value,(%temp0,%temp1))
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
9:	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
_endsubp(bind_nil)

_spentry(bind_self_boundp_check)
	__(movq symbol.binding_index(%arg_z),%temp0)
	__(cmpq %rcontext:tcr.tlb_limit,%temp0)
	__(jb,pt 0f)
	__(tlb_too_small())
0:	__(testq %temp0,%temp0)
	__(jz 9f)
	__(movq %rcontext:tcr.tlb_pointer,%temp1)
	__(cmpb $no_thread_local_binding_marker,(%temp1,%temp0))
	__(je 2f)
	__(cmpb $unbound_marker,(%temp1,%temp0))
	__(je 8f)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push %rcontext:tcr.db_link)
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
2:	__(movq symbol.vcell(%arg_z),%arg_y)
	__(cmpb $unbound_marker,%arg_y_b)
	__(jz 8f)
	__(push (%temp1,%temp0))
	__(push %temp0)
	__(push %rcontext:tcr.db_link)
	__(movq %arg_y,(%temp1,%temp0))
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
8:	__(uuo_error_reg_unbound(Rarg_z))
	
9:	__(movq $XSYMNOBIND,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
_endsubp(bind_self_boundp_check)

_spentry(conslist)
	__(movl $nil_value,%arg_z_l)
	__(testw %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subw $node_size,%nargs)
2:	__(jnz 1b)
	__(jmp *%ra0)		
_endsubp(conslist)

/* do list*: last arg in arg_z, all others pushed, nargs set to #args pushed.*/
/* Cons, one cons cell at at time.  Maybe optimize this later. */
_spentry(conslist_star)
	__(testw %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subw $node_size,%nargs)
2:	__(jnz 1b)
	__(jmp *%ra0)		
_endsubp(conslist_star)

/* We always have to create a tsp frame (even if nargs is 0), so the compiler 
   doesn't get confused. */
_spentry(stkconslist)
	__(movzwl %nargs,%nargs_l)
	__(movq %nargs_q,%imm1)
	__(addq %imm1,%imm1)
	__(movl $nil_value,%arg_z_l)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead,%imm1))
	__(TSP_Alloc_Var(%imm1,%imm0))
	__(testw %nargs,%nargs)
	__(addq $fulltag_cons,%imm0)
	__(jmp 2f)
1:	__(pop %temp0)
	__(_rplaca(%imm0,%temp0))
	__(_rplacd(%imm0,%arg_z))
	__(subw $node_size,%nargs)
	__(movq %imm0,%arg_z)
	__(addq $cons.size,%imm0)
2:	__(jne 1b)
	__(jmp *%ra0)
_endsubp(stkconslist)

/* do list*: last arg in arg_z, all others vpushed, 
	nargs set to #args vpushed. */
_spentry(stkconslist_star)
	__(movzwl %nargs,%nargs_l)
	__(movq %nargs_q,%imm1)
	__(addq %imm1,%imm1)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead,%imm1))
	__(TSP_Alloc_Var(%imm1,%imm0))
	__(testw %nargs,%nargs)
	__(addq $fulltag_cons,%imm0)
	__(jmp 2f)
1:	__(pop %temp0)
	__(_rplaca(%imm0,%temp0))
	__(_rplacd(%imm0,%arg_z))
	__(subw $node_size,%nargs)
	__(movq %imm0,%arg_z)
	__(addq $cons.size,%imm0)
2:	__(jne 1b)
	__(jmp *%ra0)
_endsubp(stkconslist_star)

/* Make a stack-consed simple-vector out of the NARGS objects 
	on top of the vstack; return it in arg_z. */
_spentry(mkstackv)
	__(movzwl %nargs,%nargs_l)
	__(dnode_align(%nargs_q,tsp_frame.fixed_overhead+node_size,%imm1))
	__(TSP_Alloc_Var(%imm1,%temp0))
	__(movl %nargs_l,%imm0_l)
	__(shlq $(num_subtag_bits-fixnumshift),%imm0)
	__(movb $subtag_simple_vector,%imm0_b)
	__(movq %imm0,(%temp0))
	__(leaq misc_data_offset(%temp0),%arg_z)
	__(testw %nargs,%nargs)
	__(jmp 2f)
1:	__(pop %temp0)
	__(movq %temp0,-node_size(%imm1))
	__(subw $node_size,%nargs)
	__(leaq -node_size(%imm1),%imm1)
2:	__(jne 1b)
	__(jmp *%ra0)	
_endsubp(mkstackv)

	
        .globl C(egc_write_barrier_start)
C(egc_write_barrier_start):
/*
   The function pc_luser_xp() - which is used to ensure that suspended threads
   are suspended in a GC-safe way - has to treat these subprims (which implement
   the EGC write-barrier) specially.  Specifically, a store that might introduce
   an intergenerational reference (a young pointer stored in an old object) has
   to "memoize" that reference by setting a bit in the global "refbits" bitmap.
   This has to happen atomically, and has to happen atomically wrt GC.

   Note that updating a word in a bitmap is itself not atomic, unless we use
   interlocked loads and stores.
*/

/*
  For RPLACA and RPLACD, things are fairly simple: regardless of where we are
  in the function, we can do the store (even if it's already been done) and
  calculate whether or not we need to set the bit out-of-line.  (Actually
  setting the bit needs to be done atomically, unless we're sure that other
  threads are suspended.)
  We can unconditionally set the suspended thread's RIP to its RA0.
*/
	
_spentry(rplaca)
        .globl C(egc_rplaca)
C(egc_rplaca):
	__(_rplaca(%arg_y,%arg_z))
	__(jmp *%ra0)
_endsubp(rplaca)

_spentry(rplacd)
        .globl C(egc_rplacd)
C(egc_rplacd):          
	__(_rplacd(%arg_y,%arg_z))
	__(jmp *%ra0)
_endsubp(rplacd)

/*
  Storing into a gvector can be handled the same way as storing into a CONS.
*/

_spentry(gvset)
        .globl C(egc_gvset)
C(egc_gvset):
	__(movq %arg_z,misc_data_offset(%arg_x,%arg_y))
	__(jmp *%ra0)
_endsubp(gvset)

/* This is a special case of storing into a gvector: if we need to memoize the store,
   record the address of the hash-table vector in the refmap, as well.
*/        

_spentry(set_hash_key)
        .globl C(egc_set_hash_key)
C(egc_set_hash_key):  
	__(movq %arg_z,misc_data_offset(%arg_x,%arg_y))
	__(jmp *%ra0)
_endsubp(set_hash_key)

/*
  This is a little trickier: the first instruction clears the EQ bit in CR0; the only
  way that it can get set is if the conditional store succeeds.  So:
  a) if we're interrupted on the first instruction, or if we're interrupted on a subsequent
     instruction but CR0[EQ] is clear, the condtional store hasn't succeeded yet.  We don't
     have to adjust the PC in this case; when the thread's resumed, the conditional store
     will be (re-)attempted and will eventually either succeed or fail.
  b) if the CR0[EQ] bit is set (on some instruction other than the first), the handler can
     decide if/how to handle memoization.  The handler should set the PC to the LR, and
     set arg_z to T.
*/

_spentry(store_node_conditional)
        .globl C(egc_store_node_conditional)
C(egc_store_node_conditional):
	__(pop %temp0)
	__(unbox_fixnum(%temp0,%imm1))
0:	__(movq (%arg_x,%imm1),%temp0)
	__(cmpq %arg_y,%temp0)
	__(movq %temp0,%imm0)
	__(jne 3f)
	__(lock cmpxchgq %arg_y,(%arg_x,%imm1))
	__(jne 0f)
       .globl C(egc_write_barrier_end)
C(egc_write_barrier_end):
	__(movl $t_value,%arg_z_l)
	__(jmp *%ra0)
3:	__(movl $nil_value,%arg_z_l)
	__(jmp *%ra0)

_endsubp(store_node_conditional)
				
_spentry(setqsym)
	__(btq $sym_vbit_const,symbol.flags(%arg_y))
	__(je _SPspecset)
	__(movq %arg_y,%arg_z)
	__(movq $XCONST,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
_endsubp(setqsym)

_spentry(progvsave)
	
_endsubp(progvsave)

/* Allocate node objects on the temp stack, immediate objects on the foreign
   stack. (The caller has to know which stack to discard a frame from.)
   %arg_y = boxed element-count, %arg_z = boxed subtype
*/	
_spentry(stack_misc_alloc)
	__(movq $(((1<<56)-1)<<fixnumshift),%temp0)
	__(testq %temp0,%arg_y)
	__(jne local_label(stack_misc_alloc_not_u56))
	__(unbox_fixnum(%arg_y,%imm0))
	__(movq %arg_y,%temp0)
	__(shl $num_subtag_bits-fixnumshift,%temp0)
	__(orq %temp0,%imm0)		/* %imm0 now = header */
	__(movb $fulltagmask,%imm1_b)
	__(andb %imm0_b,%imm1_b)
	__(cmpb $fulltag_nodeheader_0,%imm1_b)
	__(je local_label(stack_misc_alloc_node))
	__(cmpb $fulltag_nodeheader_0,%imm1_b)
	__(je local_label(stack_misc_alloc_node))
	__(cmpb $ivector_class_64_bit,%imm1_b)
	__(jz local_label(stack_misc_alloc_64))
	__(cmpb $ivector_class_32_bit,%imm1_b)
	__(jz local_label(stack_misc_alloc_32))
	__(unbox_fixnum(%arg_y,%imm1))
	/* ivector_class_other_bit: 16, 8, or 1 ... */
	__(cmpb $subtag_bit_vector,%imm0_b)
	__(jne local_label(stack_misc_alloc_8))
	__(addq $7,%imm1)
	__(shrq $3,%imm1)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_8):	
	__(cmpb $subtag_simple_base_string,%imm0_b)
	__(jb local_label(stack_misc_alloc_16))
	__(unbox_fixnum(%arg_y,%imm1))
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_16):	
	__(unbox_fixnum(%arg_y,%imm1))
	__(shlq %imm1)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_32):
	/* 32-bit ivector */
	__(unbox_fixnum(%arg_y,%imm1))
	__(shlq $2,%imm1)
	__(jmp local_label(stack_misc_alloc_alloc_ivector))
local_label(stack_misc_alloc_64):
	/* 64-bit ivector */	
	__(movq %arg_y,%imm1)
local_label(stack_misc_alloc_alloc_ivector):	
	__(dnode_align(%imm1,tsp_frame.fixed_overhead+node_size,%imm1))
	__(cmpq $tstack_alloc_limit,%imm1)
	__(ja local_label(stack_misc_alloc_heap_alloc_ivector))
	__(movd %foreign_sp,%temp0)
	__(movq %temp0,%temp1)
	__(sub %imm1,%temp0)
0:	__(movapd %fp1,-dnode_size(%temp1))
	__(subq $dnode_size,%temp1)
	__(cmpq %temp1,%temp0)
	__(jnz 0b)	
	__(movq %foreign_sp,(%temp0))
	__(movd %temp0,%foreign_sp)
	__(movq %imm0,tsp_frame.fixed_overhead(%temp0))
	__(leaq tsp_frame.fixed_overhead+fulltag_misc,%arg_z)
	__(jmp *%ra0)
local_label(stack_misc_alloc_heap_alloc_ivector):
	__(movd %foreign_sp,%imm0)
	__(subq $dnode_size,%imm0)
	__(movq %foreign_sp,(%imm0))
	__(movd %imm0,%foreign_sp)
	__(jmp _SPmisc_alloc)	
local_label(stack_misc_alloc_node):
	__(movq %arg_y,%imm1)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead+node_size,%imm1))
	__(cmpq $tstack_alloc_limit,%imm1)
	__(ja local_label(stack_misc_alloc_heap_alloc_gvector))
	__(TSP_Alloc_Var(%imm1,%temp0))
	__(movq %imm0,(%temp0))
	__(leaq fulltag_misc(%temp0),%arg_z)
	__(jmp *%ra0)
local_label(stack_misc_alloc_heap_alloc_gvector):	
	__(TSP_Alloc_Fixed(0,%imm0))
	__(jmp _SPmisc_alloc)	
		
local_label(stack_misc_alloc_not_u56):				
	__(uuo_error_reg_not_type(Rarg_y,error_object_not_unsigned_byte_56))	
_endsubp(stack_misc_alloc)

/* subtype (boxed, of course) is pushed, followed by nargs bytes worth of 
   initial-contents.  Note that this can be used to cons any type of initialized 
   node-header'ed misc object (symbols, closures, ...) as well as vector-like 
   objects. 
   Note that we're guaranteed to win (or force GC, or run out of memory) 
   because nargs < 32K. */	
_spentry(gvector)
	__(movzwl %nargs,%nargs_l)
	__(movq (%rsp,%nargs_q),%imm0)	/* boxed subtype */
	__(sarq $fixnumshift,%imm0)
	__(movq %nargs_q,%imm1)
	__(shlq $num_subtag_bits-word_shift,%imm1)
	__(orq %imm1,%imm0)
	__(dnode_align(%nargs_q,node_size,%imm1))
	__(Misc_Alloc(%arg_z))
	__(movq %nargs_q,%imm1)
	__(jmp 2f)
1:	__(movq %temp0,misc_data_offset(%arg_z,%imm1))
2:	__(subq $node_size,%imm1)
	__(pop %temp0)	/* Note the intentional fencepost:
			   discard the subtype as well.*/
	__(jge 1b)
	__(jmp *%ra0)
_endsubp(gvector)

_spentry(mvpass)
	__(int $3)
_endsubp(mvpass)

_spentry(fitvals)
	__(int $3)
_endsubp(fitvals)

/* N pushed %nargs values. */
_spentry(nthvalue)
	__(movzwl %nargs,%nargs_l)
	__(leaq (%rsp,%nargs_q),%imm0)
	__(movq (%imm0),%imm1)	
	__(movl $nil_value,%arg_z_l)
	__(cmpq %imm1,%nargs_q)
	__(jb 1f)
	__(neg %imm1)
	__(movq -node_size(%imm0,%imm1),%arg_z)
1:	__(leaq node_size(%imm0),%rsp)
	__(jmp *%ra0)	
_endsubp(nthvalue)

_spentry(values)
_endsubp(values)

_spentry(default_optional_args)
_endsubp(default_optional_args)

_spentry(opt_supplied_p)
_endsubp(opt_supplied_p)

_spentry(lexpr_entry)
_endsubp(lexpr_entry)
	
_spentry(heap_rest_arg)
	__(push_argregs())
	__(movzwl %nargs,%nargs_l)
	__(movl %nargs_l,%imm1_l)
	__(testl %imm1_l,%imm1_l)
	__(movl $nil_value,%arg_z_l)
	__(jmp 2f)
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm1_l)
2:	__(jg 1b)
	__(push %arg_z)
	__(jmp *%ra0)		
_endsubp(heap_rest_arg)

/* %imm0 contains the number of fixed args ; make an &rest arg out of the others */
_spentry(req_heap_rest_arg)
	__(push_argregs())
	__(movzwl %nargs,%nargs_l)
	__(movl %nargs_l,%imm1_l)
	__(subl %imm0_l,%imm1_l)
	__(movl $nil_value,%arg_z_l)
	__(jmp 2f)
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm1_l)
2:	__(jg 1b)
	__(push %arg_z)
	__(jmp *%ra0)		
_endsubp(req_heap_rest_arg)

/* %imm0 bytes of stuff has already been pushed	; make an &rest out of any others */
_spentry(heap_cons_rest_arg)
	__(movzwl %nargs,%nargs_l)
	__(movl %nargs_l,%imm1_l)
	__(subl %imm0_l,%imm1_l)
	__(movl $nil_value,%arg_z_l)
	__(jmp 2f)
	.p2align 4
1:	__(pop %temp1)
	__(Cons(%temp1,%arg_z,%arg_z))
	__(subl $node_size,%imm1_l)
2:	__(jg 1b)
	__(push %arg_z)
	__(jmp *%ra0)		
_endsubp(heap_cons_rest_arg)

_spentry(simple_keywords)
	__(xorl %imm0_l,%imm0_l)
	__(push_argregs())
	__(jmp _SPkeyword_bind)
_endsubp(simple_keywords)

_spentry(keyword_args)
	__(push_argregs())
	__(jmp _SPkeyword_bind)
_endsubp(keyword_args)

/* There are %nargs words of arguments on the stack; %imm0 contains the number
   of non-keyword args pushed.  It's possible that we never actually got
   any keyword args, which would make things much simpler. 

   On entry, temp1 contains a fixnum with bits indicating whether 
   &allow-other-keys and/or &rest was present in the lambda list.
   Once we get here, we can use the arg registers.
*/
define([keyword_flags_aok_bit],[fixnumshift])
define([keyword_flags_unknown_keys_bit],[fixnumshift+1])
define([keyword_flags_rest_bit],[fixnumshift+2])
	
_spentry(keyword_bind)
	__(movzwl %nargs,%imm1_l)
	__(subq %imm0,%imm1)
	__(jbe local_label(no_keyword_values))
	__(btq $word_shift,%imm1)
	__(jnc 2f)
	__(movl $nil_value,%arg_z_l)
	__(movq %imm1,%nargs_q)
	__(testw %nargs,%nargs)
	__(jmp 1f)
0:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subw $node_size,%nargs)
1:	__(jnz 0b)
	__(movl $XBADKEYS,%arg_y_l)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
	/* Now that we're sure that we have an even number of keywords and values
	  (in %imm1), copy all pairs to the temp stack */
	__(lea tsp_frame.fixed_overhead(%imm1),%arg_z)
	__(TSP_Alloc_Var(%arg_z,%imm0))
2:	__(subq $node_size,%arg_z)
	__(pop (%arg_z))
	__(cmpq %arg_z,%imm0)
	__(jne 2b)
	/* Get the keyword vector into arg_x, and its length into arg_y.
	   Push arg_y pairs of NILs. */
	__(movl function_data_offset(%fn),%imm0_l)
	__(movq function_data_offset(%fn,%imm0,node_size),%arg_x)
	__(vector_length(%arg_x,%arg_y))
	__(movq %arg_y,%imm0)
	__(jmp 4f)
3:	__(push $nil_value)
	__(push $nil_value)
4:	__(subq $fixnumone,%arg_y)
	__(jge 3b)
	/* Push the %saveN registers, so that we can use them in this loop */
	__(push %save3)
	__(push %save2)
	__(push %save1)
	__(push %save0)
	__(leaq 4*node_size(%rsp,%imm0,2),%save0)
	/* %save0 points to the 0th value/supplied-p pair */
	__(leaq (%arg_z,%imm1),%save1)
	/* %save1 is the end of the provided keyword/value pairs (the old %tsp). */
	__(movq %imm0,%save2)
	/* %save2 is the length of the keyword vector */

5:	__(movq (%arg_z),%save3)	/* %save3 is current keyword */
	__(xorl %imm0_l,%imm0_l)
6:	__(cmpq misc_data_offset(%arg_x,%imm0),%save3)
	__(jne 7f)
	/* Got a match; have we already seen this keyword ? */
	__(cmpb $fulltag_nil,node_size(%save0,%imm0,2))
	__(jne 9f)	/* already seen keyword, ignore this value */
	__(movq node_size(%arg_z),%save3)
	__(movq %save3,(%save0,%imm0,2))
	__(movl $t_value,node_size(%save0,%imm0,2))
	__(jmp 9f)
7:	__(addq $node_size,%imm0)	
	__(cmpq %imm0,%save2)
	__(jne 6b)
	/* Didn't match anything in the keyword vector. Is the keyword
	   :allow-other-keys ? */
	__(cmpq $nrs.kallowotherkeys,%save3)
	__(jne 8f)
	__(cmpb $fulltag_nil,node_size(%arg_z))
	__(jne 9f)
	__(btsq $keyword_flags_aok_bit,%temp1)
	__(jmp 9f)
8:	__(btsq $keyword_flags_unknown_keys_bit,%temp1)
9:	__(addq $dnode_size,%arg_z)
	__(cmpq %arg_z,%save1)
	__(jne 5b)
	__(pop %save0)
	__(pop %save1)
	__(pop %save2)
	__(pop %save3)
	/* If the function takes an &rest arg, or if we got an unrecognized
	   keyword and don't allow that, copy the incoming keyword/value
	   pairs from the temp stack back to the value stack */
	__(btq $keyword_flags_rest_bit,%temp1)
	__(jc 1f)
	__(btq $keyword_flags_unknown_keys_bit,%temp1)
	__(jnc 0f)
	__(btq $keyword_flags_aok_bit,%temp1)
	__(jnc 1f)
	/* pop the temp frame */
0:	__(discard_temp_frame(%imm1))
	__(jmp *%ra0)
	/* Copy the keyword/value pairs from the tsp back to sp, either because
	   the function takes an &rest arg or because we need to signal an
	   "unknown keywords" error */
1:	__(movd %tsp,%arg_z)
	__(mov (%arg_z),%arg_y)
	__(jnp 3f)
2:	__(push (%arg_z))
	__(push node_size(%arg_z))
3:	__(addq $dnode_size,%arg_z)
	__(cmpq %arg_z,%arg_y)
	__(jne 3b)
	__(discard_temp_frame(%imm0))
	__(btq $keyword_flags_unknown_keys_bit,%temp1)
	__(jnc 0b)
	__(btq $keyword_flags_aok_bit,%temp1)
	__(jc 0b)
	/* Signal an "unknown keywords" error */
	__(movq %imm1,%nargs_q)
	__(testw %nargs,%nargs)
	__(jmp 5f)
4:	__(pop %arg_y)
	__(Cons(%arg_y,%arg_z,%arg_z))
	__(subw $node_size,%nargs)
5:	__(jnz 4b)
	__(movl $XBADKEYS,%arg_y_l)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
	
/* No keyword values were provided.  Access the keyword vector (which is the 0th
   constant in %fn), determine its length N, and push N	pairs of NILs.  N could
   be 0 ... */
local_label(no_keyword_values):		
	__(movl function_data_offset(%fn),%imm0_l)
	__(movq function_data_offset(%fn,%imm0,node_size),%arg_x)
	__(movl $nil_value,%arg_z_l)
	__(vector_length(%arg_x,%arg_y))
	__(jmp 1f)
0:	__(push %arg_z)
	__(push %arg_z)
1:	__(subq $fixnumone,%arg_y)
	__(jge 0b)
	__(jmp *%ra0)		
_endsubp(keyword_bind)



_spentry(ksignalerr)
	__(movq $nrs.errdisp,%fname)
	__(jump_fname)	
_endsubp(ksignalerr)

_spentry(stack_rest_arg)
	__(xorl %imm0_l,%imm0_l)
	__(push_argregs())
	__(jmp _SPstack_cons_rest_arg)
_endsubp(stack_rest_arg)

_spentry(req_stack_rest_arg)
	__(push_argregs())
	__(jmp _SPstack_cons_rest_arg)
_endsubp(req_stack_rest_arg)

_spentry(stack_cons_rest_arg)
	__(movzwl %nargs,%nargs_l)
	__(movl %nargs_l,%imm1_l)
	__(subl %imm0_l,%imm1_l)
	__(movl $nil_value,%arg_z_l)
	__(je 2f)	/* empty list ; make an empty TSP frame */
	__(cmpq $(tstack_alloc_limit-dnode_size)/2,%imm1)
	__(ja 3f)	/* make empty frame, then heap-cons */
	__(addq %imm1,%imm1)
	__(dnode_align(%imm1,tsp_frame.fixed_overhead,%imm0))
	__(TSP_Alloc_Var(%imm0,%temp1))
	__(addq $fulltag_cons,%temp1)
1:	__(pop %arg_x)
	__(_rplacd(%temp1,%arg_z))
	__(_rplaca(%temp1,%arg_x))
	__(movq %temp1,%arg_z)
	__(addq $cons.size,%temp1)
	__(subq $dnode_size,%imm1)
	__(jne 1b)
	__(push %arg_z)
	__(jmp *%ra0)
	
/* Length 0, make empty frame */	
2:
	__(TSP_Alloc_Fixed(0,%temp1))
	__(push %arg_z)
	__(jmp *%ra0)
	
/* Too big to stack-cons, but make an empty frame before heap-consing */
3:		
	__(TSP_Alloc_Fixed(0,%temp1))
	__(jmp _SPheap_cons_rest_arg)
_endsubp(stack_cons_rest_arg)


/* Prepend all but the first three (2 words of code, inner fn) and last two */
/* (function name, lfbits) elements of %fn to the "arglist". */
_spentry(call_closure)
        __(subq $fulltag_function-fulltag_misc,%fn)
        __(header_length(%fn,%imm0))
       	__(movzwl %nargs,%nargs_l)
        __(subq $5<<fixnumshift,%imm0)  /* imm0 = inherited arg count */
        __(cmpw $nargregs<<fixnumshift,%nargs)
        __(jna,pt local_label(no_insert))
	
	/* Some arguments have already been pushed.  Push imm0's worth */
	/* of NILs, copy those arguments that have already been vpushed from */
	/* the old TOS to the new, then insert all of the inerited args */
	/* and go to the function. */
        __(movq %imm0,%imm1)
local_label(push_nil_loop):     
        __(push $nil_value)
        __(sub $fixnumone,%imm1)
        __(jne local_label(push_nil_loop))
        /* Need to use arg regs as temporaries here.  */
        __(movq %rsp,%temp1)
        __(push %arg_z)
        __(push %arg_y)
        __(push %arg_x)
        __(lea (%rsp,%imm0),%arg_x)
        __(lea -nargregs<<fixnumshift(%nargs_q),%arg_y)
local_label(copy_already_loop): 
        __(movq (%arg_x),%arg_z)
        __(addq $fixnumone,%arg_x)
        __(movq %arg_z,(%temp1))
        __(addq $fixnumone,%temp1)
        __(subq $fixnumone,%arg_y)
        __(jne local_label(copy_already_loop))

        __(movl $3<<fixnumshift,%imm1_l) /* skip code, new fn */
local_label(insert_loop):               
        __(movq misc_data_offset(%fn,%imm1),%arg_z)
        __(addq $node_size,%imm1)
        __(addw $fixnum_one,%nargs)
        __(subq $node_size,%arg_x)
        __(movq %arg_z,(%arg_x))
        __(subq $fixnum_one,%imm0)
        __(jne local_label(insert_loop))

        /* Recover the argument registers, pushed earlier */
        __(pop %arg_x)
        __(pop %arg_y)
        __(pop %arg_z)
        __(jmp local_label(go))

        /* Here if nothing was pushed by the caller.  If we're
           going to push anything, we have to reserve a stack
           frame first. (We'll need to push something if the
           sum of %nargs and %imm0 is greater than nargregs) */
local_label(no_insert): 
        __(lea (%nargs_q,%imm0),%imm1)
        __(cmpq $nargregs<<fixnumshift,%imm1)
        __(jna local_label(no_insert_no_frame))
        /* Reserve space for a stack frame */
        __(push $0)
        __(push $0)
local_label(no_insert_no_frame):        
	/* nargregs or fewer args were already vpushed. */
	/* if exactly nargregs, vpush remaining inherited vars. */
        __(cmpw $nargregs<<fixnumshift,%nargs)
        __(movl $3<<fixnumshift,%imm1_l) /* skip code, new fn */
        __(leaq 3<<fixnumshift(%imm0),%temp1)
        __(jnz local_label(set_regs))
local_label(vpush_remaining):  
        __(push misc_data_offset(%fn,%imm1))
        __(addq $node_size,%imm1)
        __(addw $fixnumone,%nargs)
        __(subq $node_size,%imm0)
        __(jnz local_label(vpush_remaining))
        __(jmp local_label(go))
local_label(set_regs):
	/* if nargs was > 1 (and we know that it was < 3), it must have */
	/* been 2.  Set arg_x, then vpush the remaining args. */
        __(cmpw $fixnumone,%nargs)
        __(jle local_label(set_y_z))
local_label(set_arg_x): 
        __(subq $node_size,%temp1)
        __(movq misc_data_offset(%fn,%temp1),%arg_x)
        __(addw $fixnumone,%nargs)
        __(subq $fixnumone,%imm0)
        __(jne local_label(vpush_remaining))
        __(jmp local_label(go))
	/* Maybe set arg_y or arg_z, preceding args */
local_label(set_y_z):
        __(jne local_label(set_arg_z))
	/* Set arg_y, maybe arg_x, preceding args */
local_label(set_arg_y): 
        __(subq $node_size,%temp1)
        __(movq misc_data_offset(%fn,%temp0),%arg_y)
        __(addw $fixnumone,%nargs)
        __(subq $fixnum_one,%imm0)
        __(jnz local_label(set_arg_x))
        __(jmp local_label(go))
local_label(set_arg_z): 
        __(subq $node_size,%temp1)
        __(movq misc_data_offset(%fn,%temp0),%arg_z)
        __(addw $fixnumone,%nargs)
        __(subq $fixnum_one,%imm0)
        __(jne local_label(set_arg_y))
        
local_label(go):        
        __(movq misc_data_offset+(2*node_size)(%fn),%fn)
        __(jmp *%fn)                
_endsubp(call_closure)

_spentry(getxlong)
_endsubp(getxlong)

/* Have to be a little careful here: the caller may or may not have pushed
   an empty frame, and we may or may not have needed one.  We can't easily
   tell whether or not a frame will be needed (if the caller didn't reserve
   a frame, whether or not we need one depends on the length of the list
   in arg_z.  So, if the caller didn't push a frame, we do so ; once everything's
   been spread, we discard the reserved frame (regardless of who pushed it)
   if all args fit in registers. */
_spentry(spreadargz)
	__(testw %nargs,%nargs)
	__(jne 0f)
	__(push $0)
	__(push $0)
0:	__(movq %arg_z,%arg_y)	/* save in case of error */
	__(xorl %imm0_l,%imm0_l)
1:	__(compare_reg_to_nil(%arg_z))
	__(je 2f)
	__(extract_fulltag(%arg_z,%imm1))
	__(cmpb $fulltag_cons,%imm1_b)
	__(jne 9f)
	__(_car(%arg_x,%arg_z))
	__(_cdr(%arg_z,%arg_z))
	__(addl $node_size,%imm0_l)
	__(compare_reg_to_nil(%arg_z))
	__(push %arg_x)
	__(jne 1b)
2:	__(addw %imm0_w,%nargs)
	__(jne 4f)
3:	__(addq 2*node_size,%rsp)
	__(jmp *%ra0)
4:	__(cmpw $1*node_size,%nargs)
	__(pop %arg_z)
	__(je 3b)
	__(cmpw $2*node_size,%nargs)
	__(pop %arg_y)
	__(je 3b)
	__(cmpw $3*node_size,%nargs)
	__(pop %arg_x)
	__(je 3b)
	__(jmp *%ra0)
/* Discard everything that's been pushed already, complain */
9:	__(lea 2*node_size(%rsp,%imm0),%rsp)
	__(movq %arg_y,%arg_z)	/* recover original */
	__(movq $XNOSPREAD,%arg_y)
	__(set_nargs(2))
	__(jmp _SPksignalerr)
_endsubp(spreadargz)

/* Caller built it's own frame when it was entered.  If all outgoing args
   are in registers, we can discard that frame; otherwise, we copy outgoing
   relative to it and restore %rbp/%ra0 */
_spentry(tfuncallgen)
	__(cmpw $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(movzwl %nargs,%nargs_l)
	__(lea nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address
	   is on the stack */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq 0(%rbp),%rbp)
	__(do_funcall())
/* All args in regs; exactly the same as the tfuncallvsp case */
9:		
	__(leave)
	__(pop %ra0)
	__(do_funcall())
_endsubp(tfuncallgen)

/* Some args were pushed; move them down in the frame */
_spentry(tfuncallslide)
	__(movzwl %nargs,%nargs_l)
	__(lea nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address
	   is on the stack */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq 0(%rbp),%rbp)
	__(do_funcall())	
_endsubp(tfuncallslide)

/* No args were pushed; recover saved context & do funcall */	
_spentry(tfuncallvsp)
	__(leave)
	__(pop %ra0)
	__(do_funcall())
_endsubp(tfuncallvsp)

_spentry(tcallsymgen)
	__(cmpw $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(movzwl %nargs,%nargs_l)
	__(lea nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address
	   is on the stack */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq 0(%rbp),%rbp)
	__(jump_fname())
/* All args in regs; exactly the same as the tcallsymvsp case */
9:		
	__(leave)
	__(pop %ra0)
	__(jump_fname())
_endsubp(tcallsymgen)

_spentry(tcallsymslide)
	__(movzwl %nargs,%nargs_l)
	__(lea nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address
	   is on the stack */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(lea (%rbp,%imm1),%rsp)
	__(movq 8(%rbp),%ra0)
	__(movq 0(%rbp),%rbp)
	__(jump_fname())
_endsubp(tcallsymslide)

_spentry(tcallsymvsp)
	__(leave)
	__(pop %ra0)
	__(jump_fname())
_endsubp(tcallsymvsp)

_spentry(tcallnfngen)
	__(cmpw $nargregs*node_size,%nargs)
	__(jbe 9f)
	__(movzwl %nargs,%nargs_l)
	__(lea nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address
	   is on the stack */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(movq %temp1,%fn)
	__(lea (%rbp,%imm1),%rsp)
	__(movq lisp_frame.savera0(%rbp),%ra0)
	__(movq lisp_frame.backlink(%rbp),%rbp)
	__(jmp *%fn)
/* All args in regs; exactly the same as the tcallnfnvsp case */
9:		
	__(movq %temp1,%fn)
	__(leave)
	__(pop %ra0)
	__(jmp *%fn)
_endsubp(tcallnfngen)

_spentry(tcallnfnslide)
	__(movzwl %nargs,%nargs_l)
	__(lea nargregs*node_size(%rsp,%nargs_q),%imm0)
	__(xorl %imm1_l,%imm1_l)
	/* We can use %ra0 as a temporary here, since the real return address
	   is on the stack */
0:	__(movq -node_size(%imm0),%ra0)
	__(movq %ra0,-node_size(%rbp,%imm1))
	__(subq $node_size,%imm0)
	__(subq $node_size,%imm1)
	__(cmpq %imm0,%rsp)
	__(jne 0b)
	__(movq %temp1,%fn)
	__(lea (%rbp,%imm1),%rsp)
	__(movq lisp_frame.savera0(%rbp),%ra0)
	__(movq lisp_frame.backlink(%rbp),%rbp)
	__(jmp *%fn)
_endsubp(tcallnfnslide)

_spentry(tcallnfnvsp)
	__(movq %temp1,%fn)
	__(leave)
	__(pop %ra0)
	__(jmp *%fn)
_endsubp(tcallnfnvsp)


/* Make a "raw" area on the foreign stack, stack-cons a macptr to point to it, 
   and return the macptr.  Size (in bytes, boxed) is in arg_z on entry; macptr
   in arg_z on exit. */
_spentry(makestackblock)
	__(unbox_fixnum(%arg_z,%imm0))
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+macptr.size,%imm0))
	__(cmpq $tstack_alloc_limit,%imm0)
	__(jae 1f)
	__(movd %foreign_sp,%arg_z)
	__(subq %imm0,%arg_z)
	__(movq %foreign_sp,(%arg_z))
	__(movd %arg_z,%foreign_sp)
	__(lea macptr.size+tsp_frame.fixed_overhead(%arg_z),%imm0)
	__(movq $macptr_header,tsp_frame.fixed_overhead(%arg_z))
	__(addq fulltag_misc+tsp_frame.fixed_overhead,%arg_z)
	__(movq %imm0,macptr.address(%arg_z))
	__(movss %fp0,macptr.domain(%arg_z))
	__(movss %fp0,macptr.type(%arg_z))
	__(jmp *%ra0)
1:	__(movd %foreign_sp,%imm0)
	__(subq $dnode_size,%imm0)
	__(movq %foreign_sp,(%imm0))
	__(movd %imm0,%foreign_sp)
	__(set_nargs(1))
	__(movq $nrs.new_gcable_ptr,%fname)
	__(jump_fname())
_endsubp(makestackblock)

_spentry(makestackblock0)
	__(unbox_fixnum(%arg_z,%imm0))
	__(dnode_align(%imm0,tsp_frame.fixed_overhead+macptr.size,%imm0))
	__(cmpq $tstack_alloc_limit,%imm0)
	__(jae 9f)
	__(movd %foreign_sp,%arg_z)
	__(movq %arg_z,%imm1)
	__(subq %imm0,%arg_z)
	__(movq %foreign_sp,(%arg_z))
	__(movd %arg_z,%foreign_sp)
	__(lea macptr.size+tsp_frame.fixed_overhead(%arg_z),%imm0)
	__(movq $macptr_header,tsp_frame.fixed_overhead(%arg_z))
	__(addq fulltag_misc+tsp_frame.fixed_overhead,%arg_z)
	__(movq %imm0,macptr.address(%arg_z))
	__(movss %fp0,macptr.domain(%arg_z))
	__(movss %fp0,macptr.type(%arg_z))
	__(jmp 2f)
1:	__(movapd %fp0,(%imm0))
	__(addq $dnode_size,%imm0)
2:	__(cmpq %imm0,%imm1)
	__(jne 1b)		
	__(jmp *%ra0)
9:	__(movd %foreign_sp,%imm0)
	__(subq $dnode_size,%imm0)
	__(movq %foreign_sp,(%imm0))
	__(movd %imm0,%foreign_sp)
	__(set_nargs(1))
	__(movq $nrs.new_gcable_ptr,%fname)
	__(jump_fname())
_endsubp(makestackblock0)

_spentry(makestacklist)
_endsubp(makestacklist)

_spentry(stkgvector)
_endsubp(stkgvector)

_spentry(misc_alloc)
	__(movq $(((1<<56)-1)<<fixnumshift),%temp0)
	__(testq %temp0,%arg_y)
	__(jne local_label(misc_alloc_not_u56))
	__(unbox_fixnum(%arg_y,%imm0))
	__(movq %arg_y,%temp0)
	__(shl $num_subtag_bits-fixnumshift,%temp0)
	__(orq %temp0,%imm0)		/* %imm0 now = header */
	__(movb $fulltagmask,%imm1_b)
	__(andb %imm0_b,%imm1_b)
	__(cmpb $fulltag_nodeheader_0,%imm1_b)
	__(je local_label(misc_alloc_64))
	__(cmpb $fulltag_nodeheader_0,%imm1_b)
	__(je local_label(misc_alloc_64))
	__(cmpb $ivector_class_64_bit,%imm1_b)
	__(jz local_label(misc_alloc_64))
	__(cmpb $ivector_class_32_bit,%imm1_b)
	__(jz local_label(misc_alloc_32))
	__(unbox_fixnum(%arg_y,%imm1))
	/* ivector_class_other_bit: 16, 8, or 1 ... */
	__(cmpb $subtag_bit_vector,%imm0_b)
	__(jne local_label(misc_alloc_8))
	__(addq $7,%imm1)
	__(shrq $3,%imm1)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_8):	
	__(cmpb $subtag_simple_base_string,%imm0_b)
	__(jb local_label(misc_alloc_16))
	__(unbox_fixnum(%arg_y,%imm1))
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_16):	
	__(unbox_fixnum(%arg_y,%imm1))
	__(shlq %imm1)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_32):
	/* 32-bit ivector */
	__(unbox_fixnum(%arg_y,%imm1))
	__(shlq $2,%imm1)
	__(jmp local_label(misc_alloc_alloc_vector))
local_label(misc_alloc_64):
	/* 64-bit ivector or gvector */	
	__(movq %arg_y,%imm1)
local_label(misc_alloc_alloc_vector):	
	__(dnode_align(%imm1,tsp_frame.fixed_overhead+node_size,%imm1))
	__(Misc_Alloc(%arg_z))
	__(jmp *%ra0)
local_label(misc_alloc_not_u56):
	__(uuo_error_reg_not_type(Rarg_y,error_object_not_unsigned_byte_56))
_endsubp(misc_alloc)



_spentry(macro_bind)
_endsubp(macro_bind)

_spentry(destructuring_bind)
_endsubp(destructuring_bind)

_spentry(destructuring_bind_inner)
_endsubp(destructuring_bind_inner)

_spentry(recover_values)
_endsubp(recover_values)

_spentry(vpopargregs)
_endsubp(vpopargregs)

/* If arg_z is an integer, return in imm0 something whose sign
   is the same as arg_z's.  If not an integer, error. */
_spentry(integer_sign)
	__(testb $tagmask,%arg_z_b)
	__(movq %arg_z,%imm0)
	__(je 8f)
	__(extract_typecode(%arg_z,%imm0))
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(getvheader(%arg_z,%imm0))
	__(shr $num_subtag_bits,%imm0)
	__(movl misc_data_offset-4(%arg_z,%imm0),%imm0_l)
	__(setae %imm0_b)
	__(andl $1,%imm0_l)
8:	__(jmp *%ra0)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_integer))
_endsubp(integer_sign)


_spentry(reset)
_endsubp(reset)

_spentry(mvslide)
_endsubp(mvslide)

_spentry(save_values)
_endsubp(save_values)

_spentry(add_values)
_endsubp(add_values)


_spentry(misc_alloc_init)
	__(push %ra0)
	__(push %rbp)
	__(movq %rsp,%rbp)
	__(push %arg_z)
	__(movq %arg_y,%arg_z)
	__(movq %arg_x,%arg_y)
	__(lea local_label(misc_alloc_init_back)(%rip),%ra0)
	__(jmp _SPmisc_alloc)
__(tra(local_label(misc_alloc_init_back)))
	__(pop %arg_y)
	__(leave)
	__(pop %ra0)
	__(movq $nrs.init_misc,%fname)
	__(set_nargs(2))
	__(jump_fname())	
_endsubp(misc_alloc_init)

_spentry(stack_misc_alloc_init)
	__(push %ra0)
	__(push %rbp)
	__(movq %rsp,%rbp)
	__(push %arg_z)
	__(movq %arg_y,%arg_z)
	__(movq %arg_x,%arg_y)
	__(lea local_label(stack_misc_alloc_init_back)(%rip),%ra0)
	__(jmp _SPstack_misc_alloc)
__(tra(local_label(stack_misc_alloc_init_back)))
	__(pop %arg_y)
	__(leave)
	__(pop %ra0)
	__(movq $nrs.init_misc,%fname)
	__(set_nargs(2))
	__(jump_fname())	
_endsubp(stack_misc_alloc_init)



	.globl C(popj)
_spentry(popj)
C(popj):
	__(leave)
	__(pop %ra0)
	__(jmp *%ra0)
_endsubp(popj)



_spentry(getu64)
	__(movq $~(target_most_positive_fixnum << fixnumshift),%imm0)
	__(testq %arg_z,%imm0)
	__(movq %arg_z,%imm0)
	__(jne 1f)
	__(sarq $fixnumshift,%imm0)
	__(jmp *%ra0)
1:	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(cmovew misc_subtag_offset(%arg_z),%imm0_w)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $three_digit_bignum_header,%imm0)
	__(je 3f)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(jne 9f)
	__(movq misc_data_offset(%arg_z),%imm0)
	__(testq %imm0,%imm0)
	__(js 9f)
	__(jmp *%ra0)
3:	__(movq misc_data_offset(%arg_z),%imm0)
	__(cmpl $0,misc_data_offset+8(%arg_z))
	__(jne 9f)
	__(jmp *%ra0)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_u64))
_endsubp(getu64)

_spentry(gets64)
	__(movq %arg_z,%imm0)
	__(sarq $fixnumshift,%imm0)
	__(testb $fixnummask,%arg_z_b)
	__(je 8f)
1:	__(movb %arg_z_b,%imm0_b)
	__(andb $tagmask,%imm0_b)
	__(cmpb $tag_misc,%imm0_b)
	__(cmovew misc_subtag_offset(%arg_z),%imm0_w)
	__(cmpb $subtag_bignum,%imm0_b)
	__(jne 9f)
	__(movq misc_header_offset(%arg_z),%imm0)
	__(cmpq $two_digit_bignum_header,%imm0)
	__(movq misc_data_offset(%arg_z),%imm0)
	__(jne 9f)
8:	__(jmp *%ra0)
9:	__(uuo_error_reg_not_type(Rarg_z,error_object_not_u64))
_endsubp(gets64)

_spentry(makeu64)
	__(movq %imm0,%imm1)
	__(shlq $fixnumshift+1,%imm1)
	__(movq %imm1,%arg_z)	/* Tagged as a fixnum, 2x  */
	__(shrq $fixnumshift+1,%imm1)
	__(shrq %arg_z)
	__(cmpq %imm0,%imm1)
	__(je 9f)
	__(testq %imm0,%imm0)
	__(movd %imm0,%mm0)
	__(js 3f)
	/* Make a 2-digit bignum. */
	__(movl $two_digit_bignum_header,%imm0_l)
	__(movl $aligned_bignum_size(2),%imm1_l)
	__(Misc_Alloc(%arg_z))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(jmp *%ra0)
3:	__(movl $three_digit_bignum_header,%imm0_l)
	__(movl $aligned_bignum_size(3),%imm1_l)
	__(Misc_Alloc(%arg_z))
	__(movq %mm0,misc_data_offset(%arg_z))
9:	__(jmp *%ra0)
_endsubp(makeu64)

/* on entry: arg_z = symbol.  On exit, arg_z = value (possibly
	unbound_marker), arg_y = symbol */
_spentry(specref)
	__(movq symbol.binding_index(%arg_z),%imm0)
	__(cmp %rcontext:tcr.tlb_limit,%imm0)
	__(movq %rcontext:tcr.tlb_limit,%imm1)
	__(movq %arg_z,%arg_z)
	__(jae 7f)
	__(movq (%imm1,%imm0),%arg_z)
	__(cmpb $no_thread_local_binding_marker,%arg_z_b)
	__(jne 8f)
7:	__(movq symbol.vcell(%arg_y),%arg_z)
8:	__(jmp *%ra0)		
_endsubp(specref)

_spentry(specset)
_endsubp(specset)

_spentry(specrefcheck)
	__(movq symbol.binding_index(%arg_z),%imm0)
	__(cmp %rcontext:tcr.tlb_limit,%imm0)
	__(movq %rcontext:tcr.tlb_limit,%imm1)
	__(movq %arg_z,%arg_z)
	__(jae 7f)
	__(movq (%imm1,%imm0),%arg_z)
	__(cmpb $no_thread_local_binding_marker,%arg_z_b)
	__(jne 8f)
7:	__(movq symbol.vcell(%arg_y),%arg_z)
8:	__(cmpb $unbound_marker,%arg_z_b)
	__(jne,pt 9f)
	__(uuo_error_reg_not_tag(Rarg_z,unbound_marker))
9:	__(jmp *%ra0)		
_endsubp(specrefcheck)

_spentry(restoreintlevel)
_endsubp(restoreintlevel)

_spentry(makes32)
	__(int $3)
_endsubp(makes32)

_spentry(makeu32)
	__(int $3)
_endsubp(makeu32)

_spentry(gets32)
	__(int $3)
_endsubp(gets32)

_spentry(getu32)
	__(int $3)
_endsubp(getu32)


_spentry(mvpasssym)
_endsubp(mvpasssym)


_spentry(unbind)
	__(movq %rcontext:tcr.db_link,%imm1)
	__(movq %rcontext:tcr.tlb_pointer,%arg_x)
	__(movq binding.sym(%imm1),%temp1)
	__(movq binding.val(%imm1),%arg_y)
	__(movq binding.link(%imm1),%imm1)
	__(movq %arg_y,(%arg_x,%temp1))
	__(movq %imm1,%rcontext:tcr.db_link)
	__(jmp *%ra0)	
_endsubp(unbind)

_spentry(unbind_n)
	__(movq %rcontext:tcr.db_link,%imm1)
	__(movq %rcontext:tcr.tlb_pointer,%arg_x)
1:		
	__(movq binding.sym(%imm1),%temp1)
	__(movq binding.val(%imm1),%arg_y)
	__(movq binding.link(%imm1),%imm1)
	__(movq %arg_y,(%arg_x,%temp1))
	__(subq $1,%imm0)
	__(jne 1b)
	__(movq %imm1,%rcontext:tcr.db_link)
	__(jmp *%ra0)	
_endsubp(unbind_n)

_spentry(unbind_to)
	__(movq %rcontext:tcr.db_link,%imm1)
	__(movq %rcontext:tcr.tlb_pointer,%arg_x)
1:		
	__(movq binding.sym(%imm1),%temp1)
	__(movq binding.val(%imm1),%arg_y)
	__(movq binding.link(%imm1),%imm1)
	__(movq %arg_y,(%arg_x,%temp1))
	__(cmpq %imm1,%imm0)
	__(jne 1b)
	__(movq %imm1,%rcontext:tcr.db_link)
	__(jmp *%ra0)	
_endsubp(unbind_to)


/* Bind CCL::*INTERRUPT-LEVEL* to 0.  If its value had been negative, check 
   for pending interrupts after doing so. */
	
_spentry(bind_interrupt_level_0)
	__(movq %rcontext:tcr.tlb_pointer,%temp1)
	__(cmpq $0,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push %rcontext:tcr.db_link)
	__(movq $0,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(movq %rsp,%rcontext:tcr.db_link)
	__(js,pn 1f)
0:	__(jmp *%ra0)
	/* Interrupt level was negative; interrupt may be pending */
1:	__(cmpq $0,%rcontext:tcr.interrupt_pending)
	__(movq $0,%rcontext:tcr.interrupt_pending)
	__(je 0b)
	__(interrupt_now())
	__(jmp *%ra0)
_endsubp(bind_interrupt_level_0)
	

/* Bind CCL::*INTERRUPT-LEVEL* to the fixnum -1.  (This has the effect
   of disabling interrupts.) */

_spentry(bind_interrupt_level_m1)
	__(movq %rcontext:tcr.tlb_pointer,%temp1)
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push %rcontext:tcr.db_link)
	__(movq $-1<<fixnumshift,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
_endsubp(bind_interrupt_level_m1)

/* Bind CCL::*INTERRUPT-LEVEL* to the value in arg_z.  If that value's 0,
   do what _SPbind_interrupt_level_0 does */
_spentry(bind_interrupt_level)
	__(testq %arg_z,%arg_z)
	__(movq %rcontext:tcr.tlb_pointer,%temp1)
	__(jz _SPbind_interrupt_level_0)
	__(push INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(push $INTERRUPT_LEVEL_BINDING_INDEX)
	__(push %rcontext:tcr.db_link)
	__(movq %arg_z,INTERRUPT_LEVEL_BINDING_INDEX(%temp1))
	__(movq %rsp,%rcontext:tcr.db_link)
	__(jmp *%ra0)
_endsubp(bind_interrupt_level)

/* Unbind CCL::*INTERRUPT-LEVEL*.  If the value changes from negative to
   non-negative, check for pending interrupts.  */
	
_spentry(unbind_interrupt_level)
	__(movq %rcontext:tcr.db_link,%imm1)
	__(movq %rcontext:tcr.tlb_pointer,%arg_x)
	__(movq INTERRUPT_LEVEL_BINDING_INDEX(%arg_x),%imm0)
	__(testq %imm0,%imm0)
	__(movq binding.val(%imm1),%temp0)
	__(movq binding.link(%imm1),%imm1)
	__(movq %temp0,INTERRUPT_LEVEL_BINDING_INDEX(%arg_x))
 	__(movq %imm1,%rcontext:tcr.db_link)
	__(js,pn 1f)
0:	__(jmp *%ra0)
1:	__(testq %temp0,%temp0)
	__(js 0b)
	__(cmpq $0,%rcontext:tcr.interrupt_pending)
	__(movq $0,%rcontext:tcr.interrupt_pending)
	__(je 0b)
	__(interrupt_now())
	__(jmp *%ra0)	
_endsubp(unbind_interrupt_level)

	
_spentry(progvrestore)
_endsubp(progvrestore)
	

/* %arg_z <- %arg_y + %arg_z.  Do the fixnum case - including overflow -
  inline.  Call out otherwise. */
_spentry(builtin_plus)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(addq %arg_y,%arg_z)
	__(jo,pn C(fix_one_bit_overflow))
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
	__(jo,pn C(fix_one_bit_overflow))
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
	__(unbox_fixnum(%arg_z,%imm0))
	/* 128-bit fixnum result in %imm1:%imm0. Overflow set if %imm1
	   is significant */
	__(imul %arg_y)
	__(jo 1f)
	__(mov %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(unbox_fixnum(%arg_z,%imm0))
	__(unbox_fixnum(%arg_y,%imm1))
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
	__(rcmpq(%arg_z,%arg_y))
	__(condition_to_boolean(e,%imm0,%arg_z))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_eq,2))
_endsubp(builtin_eq)
	
/* %arg_z <- (/= %arg_y %arg_z).	*/
_spentry(builtin_ne)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_z,%arg_y))
	__(condition_to_boolean(ne,%imm0,%arg_z))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_ne,2))
_endsubp(builtin_ne)
	
/* %arg_z <- (> %arg_y %arg_z).	*/
_spentry(builtin_gt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(g,%imm0,%arg_z))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_gt,2))
_endsubp(builtin_gt)

/* %arg_z <- (>= %arg_y %arg_z).	*/
_spentry(builtin_ge)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(ge,%imm0,%arg_z))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_ge,2))
_endsubp(builtin_ge)
	
/* %arg_z <- (< %arg_y %arg_z).	*/
_spentry(builtin_lt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(l,%imm0,%arg_z))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_lt,2))
_endsubp(builtin_lt)

/* %arg_z <- (<= %arg_y %arg_z). */
_spentry(builtin_le)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(rcmpq(%arg_y,%arg_z))
	__(condition_to_boolean(le,%imm0,%arg_z))
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_le,2))
_endsubp(builtin_le)

_spentry(builtin_eql)
	__(cmpq %arg_y,%arg_z)
	__(je 1f)
	/* Not EQ.  Could only possibly be EQL if both are fulltag-misc
	   and both have the same subtag */
	__(extract_lisptag(%arg_y,%imm0))
	__(extract_lisptag(%arg_z,%imm1))
	__(cmpb $fulltag_misc,%imm0_b)
	__(jne 2f)
	__(cmpb %imm0_b,%imm1_b)
	__(jne 2f)
	__(extract_subtag(%arg_y,%imm0_b))
	__(extract_subtag(%arg_z,%imm1_b))
	__(cmpb %imm0_b,%imm1_b)
	__(jne 2f)
	__(jump_builtin(_builtin_eql,2))
1:	__(movl $t_value,%arg_z_l)
	__(jmp *%ra0)
2:	__(movl $nil_value,%arg_z_l)
	__(jmp *%ra0)	
_endsubp(builtin_eql)

_spentry(builtin_length)
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jz 2f)
	__(cmpb $tag_misc,%imm0_b)
	__(jnz 8f)
	__(extract_subtag(%arg_z,%imm0_b))
	__(rcmpb(%imm0_b,$min_vector_subtag))
	__(jb 8f)
	__(je 1f)
	/* (simple-array * (*)) */
	__(movq %arg_z,%arg_y)
	__(vector_length(%arg_y,%arg_z))
	__(jmp *%ra0)
1:	/* vector header */
	__(movq vectorH.logsize(%arg_z),%arg_z)
	__(jmp *%ra0)
2:	/* list.  Maybe null, maybe dotted or circular. */
	__(movq $-fixnumone,%temp2)
	__(movq %arg_z,%temp0)	/* fast pointer */
	__(movq %arg_z,%temp1)  /* slow pointer */
3:	__(extract_lisptag(%temp0,%imm0))	
	__(cmpb $fulltag_nil,%temp0_b)
	__(addq $fixnumone,%temp2)
	__(je 9f)
	__(cmpb $tag_list,%imm0_b)
	__(je 8f)
	__(extract_lisptag(%temp1,%imm1))
	__(testb $fixnumone,%temp2_b)
	__(_cdr(%temp0,%temp0))
	__(je 3b)
	__(cmpb $tag_list,%imm1_b)
	__(jne 8f)
	__(_cdr(%temp1,%temp1))
	__(cmpq %temp0,%temp1)
	__(jne 3b)
8:	
	__(jump_builtin(_builtin_length,1))
9:	
	__(movq %temp2,%arg_z)
	__(jmp *%ra0)		
_endsubp(builtin_length)

	
_spentry(builtin_seqtype)
	__(extract_lisptag(%arg_z,%imm0))
	__(cmpb $tag_list,%imm0_b)
	__(jz 1f)
	__(cmpb $tag_misc,%imm0_b)
	__(cmovew misc_subtag_offset(%arg_z),%imm0_w)
	__(jne 2f)
	__(rcmpb(%imm0_b,$min_vector_subtag))
	__(jb 2f)
	__(movl $nil_value,%arg_z_l)
	__(jmp *%ra0)
1:	__(movl $t_value,%arg_z_l)
	__(jmp *%ra0)
2:	
	__(jump_builtin(_builtin_seqtype,1))
_endsubp(builtin_seqtype)

_spentry(builtin_assq)
	__(cmpb $fulltag_nil,%arg_z_b)
	__(jz 5f)
1:	__(movb $tagmask,%imm0_b)
	__(andb %arg_z_b,%imm0_b)
	__(cmpb $tag_list,%imm0_b)
	__(jz,pt 2f)
	__(uuo_error_reg_not_list(Rarg_z))
2:	__(_car(%arg_z,%arg_x))
	__(_cdr(%arg_z,%arg_z))
	__(cmpb $fulltag_nil,%arg_x_b)
	__(jz 4f)
	__(movb $tagmask,%imm0_b)
	__(andb %arg_x_b,%imm0_b)
	__(cmpb $tag_list,%imm0_b)
	__(jz,pt 3f)
	__(uuo_error_reg_not_list(Rarg_x))
3:	__(_car(%arg_x,%temp0))
	__(cmpq %temp0,%arg_y)
	__(jnz 4f)
	__(movq %arg_x,%arg_z)
	__(jmp *%ra0)
4:	__(cmpb $fulltag_nil,%arg_z_b)
5:	__(jnz 1b)
	__(jmp *%ra0)			
_endsubp(builtin_assq)	

_spentry(builtin_memq)
	__(cmpb $fulltag_nil,%arg_z_b)
	__(jmp 3f)
1:	__(movb $tagmask,%imm0_b)
	__(andb %arg_z_b,%imm0_b)
	__(cmpb $tag_list,%imm0_b)
	__(jz,pt 2f)
	__(uuo_error_reg_not_list(Rarg_z))
2:	__(_car(%arg_z,%arg_x))
	__(_cdr(%arg_z,%temp0))
	__(cmpq %arg_x,%arg_y)
	__(jz 4f)
	__(cmpb $fulltag_nil,%temp0_b)
	__(movq %temp0,%arg_z)
3:	__(jnz 1b)
4:	__(jmp *%ra0)				
_endsubp(builtin_memq)

        __ifdef([X8664])
logbitp_max_bit = 61
        __else
logbitp_max_bit = 30
        __endif
	
_spentry(builtin_logbitp)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jnz 1f)
	__(cmpq $logbitp_max_bit<<fixnumshift,%arg_y)
	__(ja 1f)
	__(unbox_fixnum(%arg_y,%imm0))
	__(addb $fixnumshift,%imm0_b)
	__(bt %imm0,%arg_z)
	__(condition_to_boolean(b,%imm0,%arg_z))
/*	
	__(setb %imm0_b)
	__(andb $t_offset,%imm0_b)
	__(lea nil_value(%imm0),%arg_z)
*/	
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_logbitp,2))
_endsubp(builtin_logbitp)

_spentry(builtin_logior)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(orq %arg_y,%arg_z)
	__(jmp *%ra0)
1:	
	__(jump_builtin(_builtin_logior,2))
		
_endsubp(builtin_logior)

_spentry(builtin_logand)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(andq %arg_y,%arg_z)
	__(jmp *%ra0)
1:		
	__(jump_builtin(_builtin_logand,2))
_endsubp(builtin_logand)

_spentry(builtin_negate)
	__(testb $fixnummask,%arg_z_b)
	__(jne 1f)
	__(negq %arg_z)
	__(jo,pn C(fix_one_bit_overflow))
	__(jmp *%ra0)
1:		
	__(jump_builtin(_builtin_negate,1))	
_endsubp(builtin_negate)

_spentry(builtin_logxor)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(xorq %arg_y,%arg_z)
	__(jmp *%ra0)
1:		
	__(jump_builtin(_builtin_logand,2))
_endsubp(builtin_logxor)


_spentry(builtin_aset1)
	__(extract_typecode(%arg_x,%imm0))
	__(box_fixnum(%imm0,%temp0))
	__(cmpb $min_vector_subtag,%imm0_b)
	__(jae _SPsubtag_misc_set)
	__(jump_builtin(_builtin_aset1,3))
_endsubp(builtin_aset1)

/* We have to be a little careful here	%cl has to be used for
   the (unboxed) shift count in all variable-length shifts, and
   %temp2 = %rcx.  Zero all but the low 8 (or 6) bits of %rcx,
   so that the shift count doesn't confuse the GC.
*/
_spentry(builtin_ash)
	__(movb %arg_y_b,%imm0_b)
	__(orb %arg_z_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 9f)
	__(unbox_fixnum(%arg_y,%imm1))
	__(unbox_fixnum(%arg_z,%imm0))
	/* Z flag set if zero ASH shift count */
	__(jnz 1f)
	__(movq %arg_y,%arg_z)	/* shift by 0 */
	__(jmp *%ra0)
1:	__(jns 3f)
	__(rcmpq(%imm0,$-63))
	__(jg 2f)
	__(sar $63,%imm1)
	__(box_fixnum(%imm1,%arg_z))
	__(jmp *%ra0)
2:	/* Right-shift by small fixnum */
	__(negb %imm0_b)
	__(movzbl %imm0_b,%ecx)
	__(sar %cl,%imm1)
	__(xorl %ecx,%ecx)
	__(box_fixnum(%imm1,%arg_z))
	__(jmp *%ra0)
3:    /* Left shift by fixnum. We cant shift by more than 63 bits, though
	shifting by 64 is actually easy. */
	__(rcmpq(%imm0,$64))
	__(jg 9f)
	__(jne 4f)
	/* left-shift by 64-bits exactly */
	__(xorl %imm0_l,%imm0_l)
	__(jmp C(makes128))
4:	/* left-shift by 1..63 bits.  Safe to move shift count to %rcx/%cl */
	__(movzbl %imm0_b,%ecx)	 /* zero-extending mov */
	__(movq %imm1,%imm0)
	__(xorq %imm1,%imm1)
	__(testq %imm0,%imm0)
	__(js 5f)
	__(shld %cl,%imm0,%imm1)
	__(shl %cl,%imm0)
	__(xorb %cl,%cl)
	__(jmp C(makeu128))
5:	__(subq $1,%imm1)
	__(shld %cl,%imm0,%imm1)
	__(shl %cl,%imm0)
	__(xorb %cl,%cl)
	__(jmp C(makes128))
9:	
	__(jump_builtin(_builtin_ash,2))
_endsubp(builtin_ash)

_spentry(builtin_aref1)
	__(extract_typecode(%arg_y,%imm0))
	__(cmpb $min_vector_subtag,%imm0_b)
	__(box_fixnum(%imm0,%arg_x))
	__(jae _SPsubtag_misc_ref)
	__(jump_builtin(_builtin_aref1,2))
_endsubp(builtin_aref1)
	

_spentry(spread_lexprz)
_endsubp(spread_lexprz)
	
/* NYI, but should be :	*/
	


/* FFI stuff, needs to be renamed and some flavor(s) implemented */		
_spentry(poweropen_callbackX)
_endsubp(poweropen_callbackX)
	
_spentry(poweropen_ffcall)
_endsubp(poweropen_ffcall)
	
_spentry(poweropen_ffcallX)
_endsubp(poweropen_ffcallX)

_spentry(poweropen_callback)
_endsubp(poweropen_callback)
	
_spentry(poweropen_syscall)
_endsubp(poweropen_syscall)

_spentry(eabi_ff_call)
_endsubp(eabi_ff_call)

_spentry(eabi_callback)
_endsubp(eabi_callback)

_spentry(eabi_syscall)
_endsubp(eabi_syscall)

/* Unused, and often not used on PPC either */	
_spentry(callbuiltin)
	__(int $3)
_endsubp(callbuiltin)

_spentry(callbuiltin0)
	__(int $3)
_endsubp(callbuiltin0)

_spentry(callbuiltin1)
	__(int $3)
_endsubp(callbuiltin1)

_spentry(callbuiltin2)
	__(int $3)
_endsubp(callbuiltin2)

_spentry(callbuiltin3)
	__(int $3)
_endsubp(callbuiltin3)
	
_spentry(restorefullcontext)
	__(int $3)
_endsubp(restorefullcontext)

_spentry(savecontextvsp)
	__(int $3)
_endsubp(savecontextvsp)

_spentry(savecontext0)
	__(int $3)
_endsubp(savecontext0)

_spentry(restorecontext)
	__(int $3)
_endsubp(restorecontext)

_spentry(stkconsyz)
	__(int $3)
_endsubp(stkconsyz)

_spentry(stkvcell0)
	__(int $3)
_endsubp(stkvcell0)

_spentry(stkvcellvsp)
	__(int $3)
_endsubp(stkvcellvsp)

_spentry(breakpoint)
_endsubp(breakpoint)
	
_spentry(unused_0)
_endsubp(unused_0)

_spentry(unused_1)
_endsubp(unused_1)
		
_spentry(unused_2)
_endsubp(unused_2)

_spentry(unused_3)
_endsubp(unused_3)

_spentry(unused_4)
_endsubp(unused_4)

_spentry(unused_5)
_endsubp(unused_5)

_spentry(unused_6)
_endsubp(unused_6)
	
