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

/* Register usage.  This is certainly a little short of
   immediate registers; we can maybe use the low bits
   of mmx or xmm registers to hold immediate values and
   do some unboxed arithmetic. */

define([%imm0],[%rax]) 
	define([%imm0d],[%eax])
	define([%imm0w],[%ax])
	define([%imm0b],[%al])
define([%temp0,[%rbx])
	define([%temp0d],[%ebx])
	define([%temp0w],[%bx])
	define([%temp0b],[%bl])
define([%temp1],[%rcx])
	define([%temp1d],[%ecx])
	define([%temp1w],[%cx])
	define([%temp1b],[%cl])
define([%temp2],[%rdx])
	define([%temp2d],[%edx])
	define([%temp2w],[%dx])
	define([%temp2b],[%dl])
define([%fn],[%rsi])
	define([%fnd],[%esi])
	define([%fnw],[%si])
	define([%fnb],[%sil])
define([%nfn],[%rdi])
	define([%nfnd],[%edi])
	define([%nfnw],[%di])
	define([%nfnb],[%dil])
define([%arg_z],[%r8])
	define([%arg_zd],[%r8d])
	define([%arg_zw],[%r8w])
	define([%arg_zb],[%r8b])
define([%arg_y],[%r9])
	define([%arg_yd],[%r9d])
	define([%arg_yw],[%r9w])
	define([%arg_yb],[%r9b])
define([%arg_x],[%r10])
	define([%arg_xd],[%r10d])
	define([%arg_xw],[%r10w])
	define([%arg_xb],[%r10b])	
define([%save3],[%r11])		
	define([%save3d],[%r11d])
	define([%save3w],[%r11w])
	define([%save3b],[%r11b])	
define([%save2],[%r12])
	define([%save2d],[%r12d])
	define([%save2w],[%r12w])
	define([%save2b],[%r12b])	
define([%imm1],[%r13])		/* some addressing restrictions */
	define([%imm1d],[%r13d])
	define([%imm1w],[%r13w])
	define([%imm1b],[%r13b])		
define([%save1],[%r14])
	define([%save1d],[%r14d])
	define([%save1w],[%r14w])
	define([%save1b],[%r14b])	
define([%save0],[%r15])
	define([%save0d],[%r15d])
	define([%save0w],[%r15w])
	define([%save0b],[%r15b])	

/* Define indices of the GPRs in the mcontext component of a ucontext */
ifdef([LINUX],[
define([arg_z],[0])
define([arg_y],[1])
define([arg_x],[2])
define([save3],[3])
define([save2],[4])
define([imm1],[5])
define([save1],[6])
define([save0],[7])
define([nfn],[8])
define([fn],[9])
define([rbp],[10])
define([temp0],[11])
define([temp2],[12])
define([imm0],[13])
define([temp1],[14])
define([sp],[15])
define([ip],[16])
define([flags],[17])
])				

nbits_in_word = 64
nbits_in_byte = 8
ntagbits = 4
nlisptagbits = 3
nfixnumtagbits = 3
nlowtagbits = 2        
num_subtag_bits = 8
fixnumshift = 3
fixnum_shift = 3
fulltagmask = 15
tagmask = 7
fixnummask = 7
ncharcodebits = 8
charcode_shift = 8
word_shift = 3
node_size = 8
dnode_size = 16
dnode_align_bits = 4
dnode_shift = dnode_align_bits        
bitmap_shift = 6
        
fixnumone = (1<<fixnumshift)
fixnum_one = fixnumone
fixnum1 = fixnumone


lowtagmask = ((1<<nlowtagbits)-1)
lowtag_mask = lowtagmask

lowtag_primary = 0
lowtag_imm = 1
lowtag_immheader = 2
lowtag_nodeheader = 3

tag_fixnum = 0

fulltag_even_fixnum = 0
fulltag_imm_0 = 1
fulltag_immheader_0 = 2
fulltag_nodeheader_0 = 3
fulltag_cons = 4
fulltag_tra_0 = 5		/* tagged return address */
fulltag_immheader_1 = 6
fulltag_nodeheader_1 = 7
fulltag_odd_fixnum = 8
fulltag_imm_2 = 9
fulltag_immheader_2 = 10
fulltag_nodeheader_2 = 11
fulltag_misc = 12
fulltag_tra_1 = 13
fulltag_immheader_3 = 14
fulltag_nodeheader_3 = 15

cl_array_subtag_mask = 0x80
define([define_cl_array_subtag],[
define_subtag($1,(cl_array_subtag_mask|$2),$3)
])

define_cl_array_subtag(arrayH,fulltag_nodeheader_1,0)
define_cl_array_subtag(vectorH,fulltag_nodeheader_2,0)
define_cl_array_subtag(simple_vector,fulltag_nodeheader_3,0)
min_vector_subtag = subtag_vectorH
min_array_subtag = subtag_arrayH
        
	
ivector_class_64_bit = fulltag_immheader_3
ivector_class_32_bit = fulltag_immheader_2
ivector_class_other_bit = fulltag_immheader_1
ivector_class_8_bit = fulltag_immheader_0

define_cl_array_subtag(s64_vector,ivector_class_64_bit,1)
define_cl_array_subtag(u64_vector,ivector_class_64_bit,2)
define_cl_array_subtag(double_float_vector,ivector_class_64_bit,4)
define_cl_array_subtag(s32_vector,ivector_class_32_bit,1)
define_cl_array_subtag(u32_vector,ivector_class_32_bit,2)
define_cl_array_subtag(single_float_vector,ivector_class_32_bit,3)
define_cl_array_subtag(s16_vector,ivector_class_other_bit,1)
define_cl_array_subtag(u16_vector,ivector_class_other_bit,2)
define_cl_array_subtag(bit_vector,ivector_class_other_bit,7)
define_cl_array_subtag(s8_vector,ivector_class_8_bit,1)
define_cl_array_subtag(u8_vector,ivector_class_8_bit,2)
define_cl_array_subtag(simple_base_string,ivector_class_8_bit,5)

/* There's some room for expansion in non-array ivector space. */
define_subtag(macptr,ivector_class_64_bit,1)
define_subtag(dead_macptr,ivector_class_64_bit,2)
define_subtag(code_vector,ivector_class_32_bit,0)
define_subtag(xcode_vector,ivector_class_32_bit,1)
define_subtag(bignum,ivector_class_32_bit,2)
define_subtag(double_float,ivector_class_32_bit,3)

					
