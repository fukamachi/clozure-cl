/*   Copyright (C) 1994-2001 Digitool, Inc */
/*   This file is part of OpenMCL. */
 
/*   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with OpenMCL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with OpenMCL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence. */
 
/*   OpenMCL is referenced in the preamble as the "LIBRARY." */
 
/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */



	

error_reg_errnum = 0		/* "real" (typically negative) error number is in RB */
error_udf = 1
error_udf_call = 2
error_throw_tag_missing = 3
error_alloc_failed = 4
error_stack_overflow = 5
error_excised_function_call = 6
error_too_many_values = 7
error_cant_call = 17
        
error_type_error = 64

define([def_type_error],[
error_object_not_$1 = (error_type_error+$2)])

	def_type_error(array,0)
	def_type_error(bignum,1)
	def_type_error(fixnum,2)
	def_type_error(character,3)
	def_type_error(integer,4)
	def_type_error(list,5)
	def_type_error(number,6)
	def_type_error(sequence,7)
	def_type_error(simple_string,8)
	def_type_error(simple_vector,9)
	def_type_error(string,10)
	def_type_error(symbol,11)
	def_type_error(macptr,12)
	def_type_error(real,13)
	def_type_error(cons,14)
	def_type_error(unsigned_byte,15)
	def_type_error(radix,16)
	def_type_error(float,17)
	def_type_error(rational,18)
	def_type_error(ratio,19)
	def_type_error(short_float,20)
	def_type_error(double_float,21)
	def_type_error(complex,22)
	def_type_error(vector,23)
	def_type_error(simple_base_string,24)
	def_type_error(function,25)
	def_type_error(unsigned_byte_16,26)
	def_type_error(unsigned_byte_8,27)
	def_type_error(unsigned_byte_32,28)
	def_type_error(signed_byte_32,29)
	def_type_error(signed_byte_16,30)
	def_type_error(signed_byte_8,31)	
	def_type_error(base_character,32)
	def_type_error(bit,33)
	def_type_error(unsigned_byte_24,34)
	def_type_error(u64,35)
	def_type_error(s64,36)
        def_type_error(unsigned_byte_56,37)
	
/* These are the "old" error constants that %ERR-DISP understands */

define([deferr],[
$1 = $2<<fixnumshift])


	deferr(XVUNBND,1)
	deferr(XNOCDR,2)
	deferr(XTMINPS,3)
	deferr(XNEINPS,4)
	deferr(XWRNGINP,5)
	deferr(XBADVEC,6)
	deferr(XSETBADVEC,7)
	deferr(XCOERCE,8)
	deferr(XWRONGSYS,9)
	deferr(XNOMEM,10)
	deferr(XOPENIMAGE,11)
	deferr(XNOTFUN,13)
	deferr(XNOCTAG,33)
	deferr(XNOFPU,36)
	deferr(XBADTOK,49)
	deferr(XFLOVFL,64)
	deferr(XDIVZRO,66)
	deferr(XFLDZRO,66)
	deferr(XMEMFULL,76)
	deferr(XARRLIMIT,77)
	deferr(XSTKOVER,75)
	deferr(XFLEXC,98)
	deferr(XMFULL,-41)

	deferr(XARROOB,112)
	deferr(XCONST,115)
	deferr(XNOSPREAD,120)
	deferr(XFASLVERS,121)
	deferr(XNOTFASL,122)
	deferr(XUDFCALL,123)
	deferr(XWRONGIMAGE,124)

	deferr(XNOPKG,130)
	deferr(XBADFASL,132)
	deferr(XSYMACC,135)
	deferr(XEXPRTC,136)
	deferr(XNDIMS,148)
	deferr(XNARGS,150)
	deferr(XBADKEYS,153)
	deferr(XWRONGTYPE,157)
	deferr(XBADSTRUCT,158)
	deferr(XSTRUCTBOUNDS,159)
	deferr(XCALLNOTLAMBDA,160)
	deferr(XTEMPFLT,161)
	deferr(XCALLTOOMANY,167)
	deferr(XCALLTOOFEW,168)
	deferr(XCALLNOMATCH,169)
	deferr(XIMPROPERLIST,170)
	deferr(XNOFILLPTR,171)
	deferr(XMALADJUST,172)
	deferr(XACCESSNTH,173)
	deferr(XNOTELT,174)
	deferr(XSGEXHAUSTED,175)
	deferr(XSGNARGS,176)
	deferr(XTOOMANYVALUES,177)
        deferr(XSYMNOBIND,178)

error_FPU_exception_double = 1024
error_FPU_exception_short = 1025
error_memory_full = 2048
