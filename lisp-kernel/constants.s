/*
   Copyright (C) 2004 Clozure Associates
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

/* Register usage: */


define([rzero],[r0])	
define([sp],[r1])
define([rcontext],[r2])
define([imm0],[r3])
define([imm1],[r4])
define([imm2],[r5])
define([imm3],[r6])
define([imm4],[r7])
define([imm5],[r8])
define([allocptr],[r9])
define([allocbase],[r10])
define([nargs],[r11])
define([tsp],[r12])      /* temp-consing stack. */
define([vsp],[r13])
define([loc_pc],[r14]) 	 /* code vector locative */
define([fn],[r15])
define([temp4],[r16])
define([temp3],[r17])
define([temp2],[r18])
define([temp1],[r19])
define([temp0],[r20])
define([arg_x],[r21])
define([arg_y],[r22])
define([arg_z],[r23])
define([save7],[r24])
define([save6],[r25])
define([save5],[r26])
define([save4],[r27])
define([save3],[r28])
define([save2],[r29])
define([save1],[r30])
define([save0],[r31])

define([fname],[temp3])
define([nfn],[temp2])
define([next_method_context],[temp1])
define([first_nvr],[save7])
define([nargregs],[3])
	
r0 = 0
r1 = 1
r2 = 2
r3 = 3
r4 = 4
r5 = 5
r6 = 6
r7 = 7
r8 = 8
r9 = 9
r10 = 10
r11 = 11
r12 = 12
r13 = 13
r14 = 14
r15 = 15
r16 = 16
r17 = 17
r18 = 18
r19 = 19
r20 = 20
r21 = 21
r22 = 22
r23 = 23
r24 = 24
r25 = 25
r26 = 26
r27 = 27
r28 = 28
r29 = 29
r30 = 30
r31 = 31

/* Lisp code keeps 0.0 in fp_zero */
define([fp_zero],[f31])   /* a non-volatile reg as far as FFI is concerned. */
define([fp_s32conv],[f30])   /* for s32->fp conversion */
	
/* registers, as used in destrucuring-bind/macro-bind */

define([whole_reg],[temp1])
define([arg_reg],[temp3])
define([keyvect_reg],[temp2])
define([mask_req_start],[24])
define([mask_req_width],[8])
define([mask_opt_start],[16])
define([mask_opt_width],[8])
define([mask_key_start],[8])
define([mask_key_width],[8])
define([mask_initopt],[7])
define([mask_keyp],[6]) /*  note that keyp can be true even when 0 keys. */
define([mask_aok],[5])
define([mask_restp],[4])

ifdef([DARWIN],[
	define([STACK_ALIGN],16)
	define([STACK_ALIGN_MASK],15)
],[
	define([STACK_ALIGN],8)
	define([STACK_ALIGN_MASK],7)
])

define([TCR_STATE_FOREIGN],1)
define([TCR_STATE_LISP],0)
define([TCR_STATE_EXCEPTION_WAIT],2)
define([TCR_STATE_EXCEPTION_RETURN],4)
	
ifdef([PPC64],[
        include(constants64.s)
],[
        include(constants32.s)
])             