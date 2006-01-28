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

	
	include(lisp.s)
	_beginfile
	
define([jump_builtin],[
	ref_nrs_value(fname,builtin_functions)
	set_nargs($2)
	vrefr(fname,fname,$1)
	jump_fname()
])

_spentry(builtin_aref1)
	__(extract_typecode(imm0,arg_y))
	__(cmpri(cr0,imm0,min_vector_subtag))
	__(box_fixnum(arg_x,imm0))
	__(bgt cr0,_SPsubtag_misc_ref)
	__(jump_builtin(_builtin_aref1,2))

_spentry(callbuiltin)
	__(ref_nrs_value(fname,builtin_functions))
	__(la imm0,misc_data_offset(imm0))
	__(ldrx(fname,fname,imm0))
	__(jump_fname())

/* the value of the nilreg-relative symbol %builtin-functions% should be */
/* a vector of symbols.  Call the symbol indexed by imm0 (boxed) and */
/* return a single value. */

_spentry(callbuiltin0)
	__(set_nargs(0))
	__(ref_nrs_value(fname,builtin_functions))
	__(la imm0,misc_data_offset(imm0))
	__(ldrx(fname,fname,imm0))
	__(jump_fname())

_spentry(callbuiltin1)
	__(ref_nrs_value(fname,builtin_functions))
	__(set_nargs(1))
	__(la imm0,misc_data_offset(imm0))
	__(ldrx(fname,fname,imm0))
	__(jump_fname())

_spentry(callbuiltin2)
	__(set_nargs(2))
	__(ref_nrs_value(fname,builtin_functions))
	__(la imm0,misc_data_offset(imm0))
	__(ldrx(fname,fname,imm0))
	__(jump_fname())


_spentry(callbuiltin3)
	__(set_nargs(3))
	__(ref_nrs_value(fname,builtin_functions))
	__(la imm0,misc_data_offset(imm0))
	__(ldrx(fname,fname,imm0))
	__(jump_fname())

_spentry(builtin_length)
        __(cmpri(cr1,arg_z,nil_value))
	__(extract_typecode(imm0,arg_z))
	__(cmpri(cr0,imm0,min_vector_subtag))
        __(beq cr1,1f)
        __ifdef([PPC64])
         __(cmpdi cr2,imm0,fulltag_cons)
        __else
	 __(cmpwi cr2,imm0,tag_list)
        __endif
	__(beq- cr0,2f)
	__(blt- cr0,3f)
	/* (simple-array * (*)) */
	__(vector_length(arg_z,arg_z,imm0))
	__(blr)
1:      __(li arg_z,0)
        __(blr)
2:
	__(ldr(arg_z,vectorH.logsize(arg_z)))
	__(blr)        
3:	__(bne cr2,8f)
	__(li temp2,-1<<fixnum_shift)
	__(mr temp0,arg_z)	/* fast pointer */
	__(mr temp1,arg_z)	/* slow pointer */
        __ifdef([PPC64])
4:       __(extract_fulltag(imm0,temp0))
         __(cmpdi cr7,temp0,nil_value)
         __(cmpdi cr1,imm0,fulltag_cons)
         __(addi temp2,temp2,fixnum_one)
         __(beq cr7,9f)
         __(andi. imm0,temp2,1<<fixnum_shift)
         __(bne cr1,8f)
         __(extract_fulltag(imm1,temp1))
         __(_cdr(temp0,temp0))
         __(cmpdi cr1,imm1,fulltag_cons)
	 __(beq cr0,4b)
	 __(bne cr1,8f)
	 __(_cdr(temp1,temp1))
	 __(cmpd cr0,temp0,temp1)
	 __(bne cr0,4b)
        __else
4:	 __(extract_lisptag(imm0,temp0))
	 __(cmpri(cr7,temp0,nil_value))
	 __(cmpri(cr1,imm0,tag_list))
	 __(addi temp2,temp2,fixnum_one)
	 __(beq cr7,9f)
	 __(andi. imm0,temp2,1<<fixnum_shift)
	 __(bne cr1,8f)
	 __(extract_lisptag(imm1,temp1))	
	 __(_cdr(temp0,temp0))
	 __(cmpri(cr1,imm1,tag_list))
	 __(beq cr0,4b)
	 __(bne cr1,8f)
	 __(_cdr(temp1,temp1))
	 __(cmpr(cr0,temp0,temp1))
	 __(bne cr0,4b)
        __endif
8:	
	__(jump_builtin(_builtin_length,1))
9:	
	__(mr arg_z,temp2)
	__(blr)
        
_spentry(builtin_seqtype)
        __ifdef([PPC64])
         __(cmpdi cr2,arg_z,nil_value)
         __(extract_typecode(imm0,arg_z))
         __(beq cr2,1f)
	 __(cmpri(cr0,imm0,fulltag_cons))
        __else
	 __(extract_typecode(imm0,arg_z))
 	 __(cmpri(cr0,imm0,tag_list))
        __endif
	__(cmpri(cr1,imm0,min_vector_subtag))
	__(beq cr0,1f)
	__(blt- cr1,2f)
	__(li arg_z,nil_value)
	__(blr)
1:	__(li arg_z,t_value)
	__(blr)
2:
	__(jump_builtin(_builtin_seqtype,1))
        
_spentry(builtin_assq)
	__(cmpri(arg_z,nil_value))
	__(beqlr)
1:	__(trap_unless_list(arg_z,imm0))
	__(_car(arg_x,arg_z))
	__(_cdr(arg_z,arg_z))
	__(cmpri(cr2,arg_x,nil_value))
	__(cmpri(cr1,arg_z,nil_value))
	__(beq cr2,2f)
	__(trap_unless_list(arg_x,imm0))
	__(_car(temp0,arg_x))
	__(cmpr(temp0,arg_y))
	__(bne cr0,2f)
	__(mr arg_z,arg_x)
	__(blr)
2:	__(bne cr1,1b)
	__(blr)

_spentry(builtin_memq)
	__(cmpri(cr1,arg_z,nil_value))
	__(b 2f)
1:	__(trap_unless_list(arg_z,imm0))
	__(_car(arg_x,arg_z))
	__(_cdr(temp0,arg_z))
	__(cmpr(arg_x,arg_y))
	__(cmpri(cr1,temp0,nil_value))
	__(beqlr)
	__(mr arg_z,temp0)
2:	__(bne cr1,1b)
	__(blr)

        __ifdef([PPC64])
logbitp_max_bit = 61
        __else
logbitp_max_bit = 30
        __endif
        
_spentry(builtin_logbitp)
	/* Call out unless both fixnums,0 <=  arg_y < logbitp_max_bit */
        __(cmplri(cr2,arg_y,logbitp_max_bit<<fixnum_shift))
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(unbox_fixnum(imm0,arg_y))
	__(subfic imm0,imm0,logbitp_max_bit)
        __ifdef([PPC64])
         __(rldcl imm0,arg_z,imm0,63)
         __(mulli imm0,imm0,t_offset)
        __else
  	 __(rlwnm imm0,arg_z,imm0,31,31)
	 __(rlwimi imm0,imm0,4,27,27)
        __endif
	__(bnl cr2,1f)
	__(bne cr0,1f)
        __(bne cr1,1f)
	__(addi arg_z,imm0,nil_value)
	__(blr)
1:
	__(jump_builtin(_builtin_logbitp,2))

_spentry(builtin_logior)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(or arg_z,arg_y,arg_z)
	__(blr)
1:
	__(jump_builtin(_builtin_logior,2))

_spentry(builtin_logand)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(and arg_z,arg_y,arg_z)
	__(blr)
1:
	__(jump_builtin(_builtin_logand,2))
	
_spentry(builtin_ash)
        __ifdef([PPC64])
	 __(cmpdi cr1,arg_z,0)
         __(extract_lisptag(imm0,arg_y))
         __(extract_lisptag(imm1,arg_z))
         __(cmpdi cr0,imm0,tag_fixnum)
         __(cmpdi cr3,imm1,tag_fixnum)
	 __(cmpdi cr2,arg_z,-(63<<3))	/* !! 3 =  fixnumshift */
	 __(bne- cr0,9f)
         __(bne- cr3,9f)
	 __(bne cr1,0f)
	 __(mr arg_z,arg_y)	/* (ash n 0) => n */
	 __(blr)
0:		
	 __(unbox_fixnum(imm1,arg_y))
	 __(unbox_fixnum(imm0,arg_z))
	 __(bgt cr1,2f)
	 /* (ash n -count) => fixnum */
	 __(neg imm2,imm0)
	 __(bgt cr2,1f)
	 __(li imm2,63)
1:	
	 __(srad imm0,imm1,imm2)
	 __(box_fixnum(arg_z,imm0))
	 __(blr)
	 /* Integer-length of arg_y/imm1 to imm2 */
2:		
	 __(cntlzd. imm2,imm1)
	 __(bne 3f)		/* cr0[eq] set if negative */
	 __(not imm2,imm1)
	 __(cntlzd imm2,imm2)
3:
	 __(subfic imm2,imm2,64)
	 __(add imm2,imm2,imm0)	 /* imm2 <- integer-length(imm1) + count */
	 __(cmpdi cr1,imm2,63-fixnumshift)
	 __(cmpdi cr2,imm0,64)
	 __(sld imm2,imm1,imm0)
	 __(bgt cr1,6f)
	 __(box_fixnum(arg_z,imm2))
	 __(blr)	
6:
	 __(bgt cr2,9f)
	 __(bne cr2,7f)
	 /* Shift left by 64 bits exactly */
	 __(mr imm0,imm1)
	 __(li imm1,0)
	 __(beq _SPmakes128)
	 __(b _SPmakeu128)
7:
	 /* Shift left by fewer than 64 bits, result not a fixnum */
	 __(subfic imm0,imm0,64)
	 __(beq 8f)
	 __(srd imm0,imm1,imm0)
	 __(mr imm1,imm2)
	 __(b _SPmakeu128)
8:	
	 __(srad imm0,imm1,imm0)
	 __(mr imm1,imm2)
	 __(b _SPmakes128)
        __else
	 __(cmpri(cr1,arg_z,0))
         __(extract_lisptag(imm0,arg_y))
         __(extract_lisptag(imm1,arg_z))
         __(cmpri(cr0,imm0,tag_fixnum))
         __(cmpri(cr3,imm1,tag_fixnum))
	 __(cmpri(cr2,arg_z,-(29<<2)))	/* !! 2 =  fixnumshift */
	 __(bne- cr0,9f)
         __(bne- cr3,9f)
	 __(bne cr1,0f)
	 __(mr arg_z,arg_y)	/* (ash n 0) => n */
	 __(blr)
0:		
	 __(unbox_fixnum(imm1,arg_y))
	 __(unbox_fixnum(imm0,arg_z))
	 __(bgt cr1,2f)
	 /* (ash n -count) => fixnum */
	 __(neg imm2,imm0)
	 __(bgt cr2,1f)
	 __(li imm2,31)
1:	
	 __(sraw imm0,imm1,imm2)
	 __(box_fixnum(arg_z,imm0))
	 __(blr)
	 /* Integer-length of arg_y/imm1 to imm2 */
2:		
	 __(cntlzw. imm2,imm1)
	 __(bne 3f)		/* cr0[eq] set if negative */
	 __(not imm2,imm1)
	 __(cntlzw imm2,imm2)
3:
	 __(subfic imm2,imm2,32)
	 __(add imm2,imm2,imm0)	 /* imm2 <- integer-length(imm1) + count */
	 __(cmpri(cr1,imm2,31-fixnumshift))
	 __(cmpri(cr2,imm0,32))
	 __(slw imm2,imm1,imm0)
	 __(bgt cr1,6f)
	 __(box_fixnum(arg_z,imm2))
	 __(blr)	
6:
	 __(bgt cr2,9f)
	 __(bne cr2,7f)
	 /* Shift left by 32 bits exactly */
	 __(mr imm0,imm1)
	 __(li imm1,0)
	 __(beq _SPmakes64)
	 __(b _SPmakeu64)
7:
	 /* Shift left by fewer than 32 bits, result not a fixnum */
	 __(subfic imm0,imm0,32)
	 __(beq 8f)
	 __(srw imm0,imm1,imm0)
	 __(mr imm1,imm2)
	 __(b _SPmakeu64)
8:	
	 __(sraw imm0,imm1,imm0)
	 __(mr imm1,imm2)
	 __(b _SPmakes64)
        __endif
9:		
	__(jump_builtin(_builtin_ash,2))

_spentry(builtin_negate)
	__(extract_lisptag_(imm0,arg_z))
	__(bne- cr0,1f)
	__(nego. arg_z,arg_z)
	__(bnslr+)
	__(mtxer rzero)
	__(unbox_fixnum(imm1,arg_z))
        __ifdef([PPC64])
	 __(li imm0,two_digit_bignum_header)
         __(rotldi imm1,imm1,32)
	 __(xoris imm1,imm1,0xe000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,aligned_bignum_size(2)))
	 __(str(imm1,misc_data_offset(arg_z)))
        __else
	 __(li imm0,one_digit_bignum_header)
	 __(xoris imm1,imm1,0xc000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,aligned_bignum_size(1)))
	 __(str(imm1,misc_data_offset(arg_z)))
        __endif
	__(blr)
1:
	__(jump_builtin(_builtin_negate,1))

_spentry(builtin_logxor)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(xor arg_z,arg_y,arg_z)
	__(blr)
1:
	__(jump_builtin(_builtin_logxor,2))



        
_spentry(builtin_aset1)
	__(extract_typecode(imm0,arg_x))
	__(cmpri(cr0,imm0,min_vector_subtag))
	__(box_fixnum(temp0,imm0))
	__(bgt cr0,1f)
	__(jump_builtin(_builtin_aset1,3))
1:
	__(b _SPsubtag_misc_set)

_spentry(builtin_plus)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(addo. arg_z,arg_y,arg_z)
	__(bnslr+)
	__(mtxer rzero)
	__(unbox_fixnum(imm1,arg_z))
        __ifdef([PPC64])
	 __(li imm0,two_digit_bignum_header)
         __(rotldi imm1,imm1,32)
	 __(xoris imm1,imm1,0xe000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,aligned_bignum_size(2)))
	 __(str(imm1,misc_data_offset(arg_z)))
        __else
	 __(li imm0,one_digit_bignum_header)
	 __(xoris imm1,imm1,0xc000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,aligned_bignum_size(1)))
	 __(str(imm1,misc_data_offset(arg_z)))
        __endif
	__(blr)
1:
	__(jump_builtin(_builtin_plus,2))
_spentry(builtin_minus)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(subo. arg_z,arg_y,arg_z)
	__(bnslr+)
	__(mtxer rzero)
	__(unbox_fixnum(imm1,arg_z))
        __ifdef([PPC64])
	 __(li imm0,two_digit_bignum_header)
         __(rotldi imm1,imm1,32)
	 __(xoris imm1,imm1,0xe000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,aligned_bignum_size(2)))
	 __(str(imm1,misc_data_offset(arg_z)))
        __else
	 __(li imm0,one_digit_bignum_header)
	 __(xoris imm1,imm1,0xc000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,aligned_bignum_size(1)))
	 __(str(imm1,misc_data_offset(arg_z)))
        __endif
	__(blr)
1:
	__(jump_builtin(_builtin_minus,2))
_spentry(builtin_times)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(unbox_fixnum(imm2,arg_y))
	__(bne cr0,1f)
        __(bne cr1,1f)
        __ifdef([PPC64])
         __(mulldo. imm3,arg_z,imm2)
         __(bso 2f)
         __(mr arg_z,imm3)
         __(blr)
	 /* Args are fixnums; result can't be */
2:	 __(mtxer rzero)
	 __(unbox_fixnum(imm3,arg_z))
	 __(mulld imm1,imm3,imm2) /* imm1 = low  64 bits */
	 __(mulhd imm0,imm3,imm2) /* imm0 = high 64 bits */
	 __(b _SPmakes128)
        __else
	 __(mullwo. imm3,arg_z,imm2)
	 __(bso 2f)		/*  SO set if result would overflow a fixnum */
	 __(mr arg_z,imm3)
	 __(blr)
	 /* Args are fixnums; result can't be */
2:	 __(mtxer rzero)
	 __(unbox_fixnum(imm3,arg_z))
	 __(mullw imm1,imm3,imm2) /* imm1 = low  32 bits */
	 __(mulhw imm0,imm3,imm2) /* imm0 = high 32 bits */
	 __(b _SPmakes64)
        __endif

1:	__(jump_builtin(_builtin_times,2))

_spentry(builtin_div)
	__(jump_builtin(_builtin_div,2))

_spentry(builtin_eq)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(cmpr(cr2,arg_y,arg_z))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(li arg_z,nil_value)
	__(bnelr cr2)
	__(li arg_z,t_value)
	__(blr)
1:
	__(jump_builtin(_builtin_eq,2))

_spentry(builtin_ne)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(cmpr(cr2,arg_y,arg_z))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(li arg_z,nil_value)
	__(beqlr cr2)
	__(li arg_z,t_value)
	__(blr)
1:
	__(jump_builtin(_builtin_ne,2))

_spentry(builtin_gt)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(cmpr(cr2,arg_y,arg_z))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(li arg_z,nil_value)
	__(bnglr cr2)
	__(li arg_z,t_value)
	__(blr)
1:
	__(jump_builtin(_builtin_gt,2))

_spentry(builtin_ge)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(cmpr(cr2,arg_y,arg_z))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(li arg_z,nil_value)
	__(bltlr cr2)
	__(li arg_z,t_value)
	__(blr)
1:
	__(jump_builtin(_builtin_ge,2))

_spentry(builtin_lt)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(cmpr(cr2,arg_y,arg_z))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(li arg_z,nil_value)
	__(bnllr cr2)
	__(li arg_z,t_value)
	__(blr)
1:
	__(jump_builtin(_builtin_lt,2))

_spentry(builtin_le)
        __(extract_lisptag(imm0,arg_y))
        __(extract_lisptag(imm1,arg_z))
        __(cmpri(cr0,imm0,tag_fixnum))
        __(cmpri(cr1,imm1,tag_fixnum))
	__(cmpr(cr2,arg_y,arg_z))
	__(bne- cr0,1f)
        __(bne- cr1,1f)
	__(li arg_z,nil_value)
	__(bgtlr cr2)
	__(li arg_z,t_value)
	__(blr)
1:
	__(jump_builtin(_builtin_le,2))


_spentry(builtin_eql)
        __(cmpr(cr1,arg_y,arg_z))
        __(extract_fulltag(imm2,arg_y))
        __(extract_fulltag(imm3,arg_z))
        __(beq cr1,1f)
        __(cmpri(cr1,imm2,fulltag_misc))
        __(cmpri(cr0,imm3,fulltag_misc))
        __(bne cr1,2f)
        __(extract_subtag(imm0,arg_y))
        __(bne cr0,2f)
        __(extract_subtag(imm1,arg_z))
        __(cmpr(cr0,imm0,imm1))
        __(bne cr0,2f)
	__(jump_builtin(_builtin_eql,2))
1:	__(li arg_z,t_value)
	__(blr)
2:	__(li arg_z,nil_value)
	__(blr)
        
			
