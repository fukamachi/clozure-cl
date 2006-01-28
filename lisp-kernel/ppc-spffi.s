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
        .align 2

/* Foreign-function calls, syscalls, callbacks */

/*
  A PowerOpen ff-call.  arg_z is either a fixnum (word-aligned entrypoint)
  or a macptr (whose address had better be word-aligned as well.)  A
  PowerOpen stack frame is on top of the stack; 4 additional words (to
  be used a a lisp frame) sit under the C frame.

  Since we probably can't deal with FP exceptions in foreign code, we
  disable them in the FPSCR, then check on return to see if any previously
  enabled FP exceptions occurred.

  As it turns out, we can share a lot of code with the eabi version of
  ff-call.  Some things that happen up to the point of call differ between
  the ABIs, but everything that happens after is the same.
*/
        
_spentry(poweropen_ffcall)
	__(mflr loc_pc)
	__(vpush_saveregs())		/* Now we can use save0-save7 to point to stacks */
	__(mr save0,rcontext)	/* or address globals. */
	__(extract_typecode(imm0,arg_z))
	__(cmpri(cr7,imm0,subtag_macptr))
	__(ldr(save1,0(sp)))	/* bottom of reserved lisp frame */
	__(la save2,-lisp_frame.size(save1))	/* top of lisp frame*/
        __(zero_doublewords save2,0,lisp_frame.size)
	__(str(save1,lisp_frame.backlink(save2)))
	__(str(save2,c_frame.backlink(sp)))
	__(str(fn,lisp_frame.savefn(save2)))
	__(str(loc_pc,lisp_frame.savelr(save2)))
	__(str(vsp,lisp_frame.savevsp(save2)))
       	__(bne cr7,1f)
	__(ldr(arg_z,macptr.address(arg_z)))
1:
	__(ldr(save3,tcr.cs_area(rcontext)))
	__(str(save2,area.active(save3)))
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(str(rzero,tcr.ffi_exception(rcontext)))
	__(mffs f0)
	__(stfd f0,tcr.lisp_fpscr(rcontext))	/* remember lisp's fpscr */
	__(mtfsf 0xff,fp_zero)	/* zero foreign fpscr */
	__(li r4,TCR_STATE_FOREIGN)
	__(str(r4,tcr.valence(rcontext)))
        __ifdef([rTOC])
         __(ld rTOC,8(arg_z))
         __(ld arg_z,0(arg_z))
        __else
	 __(li rcontext,0)
        __endif
	__(mtctr arg_z)
	__(ldr(r3,c_frame.param0(sp)))
	__(ldr(r4,c_frame.param1(sp)))
	__(ldr(r5,c_frame.param2(sp)))
	__(ldr(r6,c_frame.param3(sp)))
	__(ldr(r7,c_frame.param4(sp)))
	__(ldr(r8,c_frame.param5(sp)))
	__(ldr(r9,c_frame.param6(sp)))
	__(ldr(r10,c_frame.param7(sp)))
	/* Darwin is allegedly very picky about what register points
	   to the function on entry. */
	__(mr r12,arg_z)
	__(bctrl)
	__(b FF_call_return_common)

_spentry(poweropen_callbackX)        
	/* Save C argument registers */
	__(str(r3,c_frame.param0(sp)))
	__(str(r4,c_frame.param1(sp)))
	__(str(r5,c_frame.param2(sp)))
	__(str(r6,c_frame.param3(sp)))
	__(str(r7,c_frame.param4(sp)))
	__(str(r8,c_frame.param5(sp)))
	__(str(r9,c_frame.param6(sp)))
	__(str(r10,c_frame.param7(sp)))
	__(mflr imm3)
	__(str(imm3,c_frame.savelr(sp)))
	__(mfcr imm0)
	__(str(imm0,c_frame.crsave(sp)))

	/* Save the non-volatile registers on the sp stack */
	/* This is a non-standard stack frame, but noone will ever see it, */
        /* so it doesn't matter. It will look like more of the stack frame pushed below. */
	__(stru(sp,-(stack_align(c_reg_save.size))(sp)))
        __(str(r13,c_reg_save.save_gprs+(0*node_size)(sp)))
        __(str(r14,c_reg_save.save_gprs+(1*node_size)(sp)))
        __(str(r15,c_reg_save.save_gprs+(2*node_size)(sp)))
        __(str(r16,c_reg_save.save_gprs+(3*node_size)(sp)))
        __(str(r17,c_reg_save.save_gprs+(4*node_size)(sp)))
        __(str(r18,c_reg_save.save_gprs+(5*node_size)(sp)))
        __(str(r19,c_reg_save.save_gprs+(6*node_size)(sp)))
        __(str(r20,c_reg_save.save_gprs+(7*node_size)(sp)))
        __(str(r21,c_reg_save.save_gprs+(8*node_size)(sp)))
        __(str(r22,c_reg_save.save_gprs+(9*node_size)(sp)))
        __(str(r23,c_reg_save.save_gprs+(10*node_size)(sp)))
        __(str(r24,c_reg_save.save_gprs+(11*node_size)(sp)))
        __(str(r25,c_reg_save.save_gprs+(12*node_size)(sp)))
        __(str(r26,c_reg_save.save_gprs+(13*node_size)(sp)))
        __(str(r27,c_reg_save.save_gprs+(14*node_size)(sp)))
        __(str(r28,c_reg_save.save_gprs+(15*node_size)(sp)))
        __(str(r29,c_reg_save.save_gprs+(16*node_size)(sp)))
        __(str(r30,c_reg_save.save_gprs+(17*node_size)(sp)))
        __(str(r31,c_reg_save.save_gprs+(18*node_size)(sp)))
	__(check_stack_alignment(r0))
	__(mffs f0)
	__(stfd f0,c_reg_save.save_fp_zero(sp))
	__(ldr(r31,c_reg_save.save_fp_zero+4(sp)))	/* recover FPSCR image */
	__(str(r31,c_reg_save.save_fpscr(sp)))
	__(lwi(r30,0x43300000))
	__(lwi(r31,0x80000000))
	__(stw r30,c_reg_save.save_fp_zero(sp))
	__(stw r31,c_reg_save.save_fp_zero+4(sp))
	__(stfd fp_s32conv,c_reg_save.save_fps32conv(sp))
	__(lfd fp_s32conv,c_reg_save.save_fp_zero(sp))
	__(stfd fp_zero,c_reg_save.save_fp_zero(sp))
	__(lfs fp_zero,lisp_globals.short_float_zero(0))	/* ensure that fp_zero contains 0.0 */

/* Restore rest of Lisp context. */
/* Could spread out the memory references here to gain a little speed */

	__(li loc_pc,0)
	__(li fn,0)                     /* subprim, not a lisp function */
	__(li temp3,0)
	__(li temp2,0)
	__(li temp1,0)
	__(li temp0,0)
	__(li arg_x,0)
	__(box_fixnum(arg_y,r11))	/* callback-index */
	__(la arg_z,stack_align(c_reg_save.size)+c_frame.param0(sp))	/* parameters (tagged as a fixnum) */

	/* Recover lisp thread context. Have to call C code to do so. */
	__(ref_global(r12,get_tcr))
	__(mtctr r12)
        __(li r3,1)
	__(stru(sp,-(stack_align(c_frame.minsiz))(sp)))
	__(bctrl)
	__(la rcontext,TCR_BIAS(r3))
	/* re-establish lisp exception handling */
	__(ref_global(r12,lisp_return_hook))
	__(mtctr r12)
	__(bctrl)
	__(la sp,(stack_align(c_frame.minsiz))(sp))

	__(ldr(vsp,tcr.save_vsp(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))		
	__(li rzero,0)
	__(mtxer rzero) /* lisp wants the overflow bit clear */
        __(mtctr rzero)
	__(li imm0,TCR_STATE_LISP)
	__(li save0,0)
	__(li save1,0)
	__(li save2,0)
	__(li save3,0)
	__(li save4,0)
	__(li save5,0)
	__(li save6,0)
	__(li save7,0)
	__(lfd f0,tcr.lisp_fpscr(rcontext))
	__(mtfsf 0xff,f0)
	__(li allocptr,0)
	__(li allocbase,0)
	__(str(imm0,tcr.valence(rcontext)))
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
	
	/* load nargs and callback to the lisp */
	__(set_nargs(2))
	__(ldr(imm2,tcr.cs_area(rcontext)))
	__(ldr(imm4,area.active(imm2)))
	__(stru(imm4,-lisp_frame.size(sp)))
	__(str(imm3,lisp_frame.savelr(sp)))
	__(str(vsp,lisp_frame.savevsp(sp)))	/* for stack overflow code */
	__(li fname,nrs.callbacks)	/* %pascal-functions% */
	__(call_fname)
	__(ldr(imm2,lisp_frame.backlink(sp)))
	__(ldr(imm3,tcr.cs_area(rcontext)))
	__(str(imm2,area.active(imm3)))
	__(discard_lisp_frame())
	/* save_vsp will be restored from ff_call's stack frame, but */
	/* I included it here for consistency. */
	/* save_tsp is set below after we exit Lisp context. */
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))

	__(li imm1,TCR_STATE_FOREIGN)
	__(str(imm1,tcr.valence(rcontext)))
	__(mr r3,rcontext)
	__(ldr(r4,tcr.foreign_exception_status(rcontext)))
	__(cmpri(r4,0))
	/* Restore the non-volatile registers & fpscr */
	__(lfd fp_zero,c_reg_save.save_fp_zero(sp))
	__(ldr(r31,c_reg_save.save_fpscr(sp)))
	__(str(r31,c_reg_save.save_fp_zero+4(sp)))
	__(lfd f0,c_reg_save.save_fp_zero(sp))
	__(mtfsf 0xff,f0)
	__(ldr(r13,c_reg_save.save_gprs+(0*node_size)(sp)))
	__(ldr(r14,c_reg_save.save_gprs+(1*node_size)(sp)))
	__(ldr(r15,c_reg_save.save_gprs+(2*node_size)(sp)))
	__(ldr(r16,c_reg_save.save_gprs+(3*node_size)(sp)))
	__(ldr(r17,c_reg_save.save_gprs+(4*node_size)(sp)))
	__(ldr(r18,c_reg_save.save_gprs+(5*node_size)(sp)))
	__(ldr(r19,c_reg_save.save_gprs+(6*node_size)(sp)))
	__(ldr(r20,c_reg_save.save_gprs+(7*node_size)(sp)))
	__(ldr(r21,c_reg_save.save_gprs+(8*node_size)(sp)))
	__(ldr(r22,c_reg_save.save_gprs+(9*node_size)(sp)))
	__(ldr(r23,c_reg_save.save_gprs+(10*node_size)(sp)))
	__(ldr(r24,c_reg_save.save_gprs+(11*node_size)(sp)))
	__(ldr(r25,c_reg_save.save_gprs+(12*node_size)(sp)))
	__(ldr(r26,c_reg_save.save_gprs+(13*node_size)(sp)))
	__(ldr(r27,c_reg_save.save_gprs+(14*node_size)(sp)))
	__(ldr(r28,c_reg_save.save_gprs+(15*node_size)(sp)))
	__(ldr(r29,c_reg_save.save_gprs+(16*node_size)(sp)))
	__(ldr(r30,c_reg_save.save_gprs+(17*node_size)(sp)))
	__(ldr(r31,c_reg_save.save_gprs+(18*node_size)(sp)))
	__(lfd fp_s32conv,c_reg_save.save_fps32conv(sp))
	__(beq 9f)
	__(ref_global(r12,lisp_exit_hook))
	__(mtctr r12)
	__(bctrl)
9:
	__(ldr(sp,0(sp)))
	__(ldr(r3,c_frame.param0(sp)))
	__(ldr(r4,c_frame.param1(sp)))
        __(lfd f1,c_frame.param2(sp))
	__(ldr(r5,c_frame.savelr(sp)))
	__(mtlr r5)
	__(ldr(r5,c_frame.crsave(sp)))
	__(mtcr r5)
	__(blr)

/* On entry, R11->callback-index */
/* Restore lisp context, then funcall #'%pascal-functions% with */
/* two args: callback-index, args-ptr (a macptr pointing to the args on the stack) */
_spentry(poweropen_callback)
        __ifdef([rTOC])
         __(mr r11,rTOC)
        __endif
	/* Save C argument registers */
	__(str(r3,c_frame.param0(sp)))
	__(str(r4,c_frame.param1(sp)))
	__(str(r5,c_frame.param2(sp)))
	__(str(r6,c_frame.param3(sp)))
	__(str(r7,c_frame.param4(sp)))
	__(str(r8,c_frame.param5(sp)))
	__(str(r9,c_frame.param6(sp)))
	__(str(r10,c_frame.param7(sp)))
	__(mflr imm3)
	__(str(imm3,c_frame.savelr(sp)))
	__(mfcr imm0)
	__(str(imm0,c_frame.crsave(sp)))

	/* Save the non-volatile registers on the sp stack */
	/* This is a non-standard stack frame, but noone will ever see it, */
        /* so it doesn't matter. It will look like more of the stack frame pushed below. */
	__(stru(sp,-(stack_align(c_reg_save.size))(sp)))
        __(str(r13,c_reg_save.save_gprs+(0*node_size)(sp)))
        __(str(r14,c_reg_save.save_gprs+(1*node_size)(sp)))
        __(str(r15,c_reg_save.save_gprs+(2*node_size)(sp)))
        __(str(r16,c_reg_save.save_gprs+(3*node_size)(sp)))
        __(str(r17,c_reg_save.save_gprs+(4*node_size)(sp)))
        __(str(r18,c_reg_save.save_gprs+(5*node_size)(sp)))
        __(str(r19,c_reg_save.save_gprs+(6*node_size)(sp)))
        __(str(r20,c_reg_save.save_gprs+(7*node_size)(sp)))
        __(str(r21,c_reg_save.save_gprs+(8*node_size)(sp)))
        __(str(r22,c_reg_save.save_gprs+(9*node_size)(sp)))
        __(str(r23,c_reg_save.save_gprs+(10*node_size)(sp)))
        __(str(r24,c_reg_save.save_gprs+(11*node_size)(sp)))
        __(str(r25,c_reg_save.save_gprs+(12*node_size)(sp)))
        __(str(r26,c_reg_save.save_gprs+(13*node_size)(sp)))
        __(str(r27,c_reg_save.save_gprs+(14*node_size)(sp)))
        __(str(r28,c_reg_save.save_gprs+(15*node_size)(sp)))
        __(str(r29,c_reg_save.save_gprs+(16*node_size)(sp)))
        __(str(r30,c_reg_save.save_gprs+(17*node_size)(sp)))
        __(str(r31,c_reg_save.save_gprs+(18*node_size)(sp)))
	__(check_stack_alignment(r0))
	__(mffs f0)
	__(stfd f0,c_reg_save.save_fp_zero(sp))
	__(lwz r31,c_reg_save.save_fp_zero+4(sp))	/* recover FPSCR image */
	__(stw r31,c_reg_save.save_fpscr(sp))
	__(lwi(r30,0x43300000))
	__(lwi(r31,0x80000000))
	__(stw r30,c_reg_save.save_fp_zero(sp))
	__(stw r31,c_reg_save.save_fp_zero+4(sp))
	__(stfd fp_s32conv,c_reg_save.save_fps32conv(sp))
	__(lfd fp_s32conv,c_reg_save.save_fp_zero(sp))
	__(stfd fp_zero,c_reg_save.save_fp_zero(sp))
	__(lfs fp_zero,lisp_globals.short_float_zero(0))	/* ensure that fp_zero contains 0.0 */

/* Restore rest of Lisp context. */
/* Could spread out the memory references here to gain a little speed */

	__(li loc_pc,0)
	__(li fn,0)                     /* subprim, not a lisp function */
	__(li temp3,0)
	__(li temp2,0)
	__(li temp1,0)
	__(li temp0,0)
	__(li arg_x,0)
	__(box_fixnum(arg_y,r11))	/* callback-index */
	__(la arg_z,stack_align(c_reg_save.size)+c_frame.param0(sp))	/* parameters (tagged as a fixnum) */

	/* Recover lisp thread context. Have to call C code to do so. */
	__(ref_global(r12,get_tcr))
        __ifdef([rTOC])
         __(ld rTOC,8(r12))
         __(ld r12,0(r12))
        __endif
	__(mtctr r12)
        __(li r3,1)
	__(stru(sp,-(stack_align(c_frame.minsiz))(sp)))
	__(bctrl)
	__(la rcontext,TCR_BIAS(r3))
	__(la sp,(stack_align(c_frame.minsiz))(sp))

	__(ldr(vsp,tcr.save_vsp(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))		
	__(li rzero,0)
	__(li imm0,TCR_STATE_LISP)
	__(mtxer rzero) /* lisp wants the overflow bit being clear */
        __(mtctr rzero)
	__(li save0,0)
	__(li save1,0)
	__(li save2,0)
	__(li save3,0)
	__(li save4,0)
	__(li save5,0)
	__(li save6,0)
	__(li save7,0)
	__(lfd f0,tcr.lisp_fpscr(rcontext))
	__(mtfsf 0xff,f0)
	__(li allocbase,0)
	__(li allocptr,0)	
	__(str(imm0,tcr.valence(rcontext)))
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))

	/* load nargs and callback to the lisp */
	__(set_nargs(2))
	__(ldr(imm2,tcr.cs_area(rcontext)))
	__(ldr(imm4,area.active(imm2)))
	__(stru(imm4,-lisp_frame.size(sp)))
	__(str(imm3,lisp_frame.savelr(sp)))
	__(str(vsp,lisp_frame.savevsp(sp)))	/* for stack overflow code */
	__(li fname,nrs.callbacks)	/* %pascal-functions% */
	__(call_fname)
	__(ldr(imm2,lisp_frame.backlink(sp)))
	__(ldr(imm3,tcr.cs_area(rcontext)))
	__(str(imm2,area.active(imm3)))
	__(discard_lisp_frame())
	/* save_vsp will be restored from ff_call's stack frame, but */
	/* I included it here for consistency. */
	/* save_tsp is set below after we exit Lisp context. */
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	/* Exit lisp context */
	__(li imm1,TCR_STATE_FOREIGN)
	__(str(imm1,tcr.valence(rcontext)))
	/* Restore the non-volatile registers & fpscr */
	__(lfd fp_zero,c_reg_save.save_fp_zero(sp))
	__(lwz r31,c_reg_save.save_fpscr(sp))
	__(stw r31,c_reg_save.save_fp_zero+4(sp))
	__(lfd f0,c_reg_save.save_fp_zero(sp))
	__(mtfsf 0xff,f0)
	__(ldr(r13,c_reg_save.save_gprs+(0*node_size)(sp)))
	__(ldr(r14,c_reg_save.save_gprs+(1*node_size)(sp)))
	__(ldr(r15,c_reg_save.save_gprs+(2*node_size)(sp)))
	__(ldr(r16,c_reg_save.save_gprs+(3*node_size)(sp)))
	__(ldr(r17,c_reg_save.save_gprs+(4*node_size)(sp)))
	__(ldr(r18,c_reg_save.save_gprs+(5*node_size)(sp)))
	__(ldr(r19,c_reg_save.save_gprs+(6*node_size)(sp)))
	__(ldr(r20,c_reg_save.save_gprs+(7*node_size)(sp)))
	__(ldr(r21,c_reg_save.save_gprs+(8*node_size)(sp)))
	__(ldr(r22,c_reg_save.save_gprs+(9*node_size)(sp)))
	__(ldr(r23,c_reg_save.save_gprs+(10*node_size)(sp)))
	__(ldr(r24,c_reg_save.save_gprs+(11*node_size)(sp)))
	__(ldr(r25,c_reg_save.save_gprs+(12*node_size)(sp)))
	__(ldr(r26,c_reg_save.save_gprs+(13*node_size)(sp)))
	__(ldr(r27,c_reg_save.save_gprs+(14*node_size)(sp)))
	__(ldr(r28,c_reg_save.save_gprs+(15*node_size)(sp)))
	__(ldr(r29,c_reg_save.save_gprs+(16*node_size)(sp)))
	__(ldr(r30,c_reg_save.save_gprs+(17*node_size)(sp)))
	__(ldr(r31,c_reg_save.save_gprs+(18*node_size)(sp)))
	__(lfd fp_s32conv,c_reg_save.save_fps32conv(sp))
	__(ldr(sp,0(sp)))
	__(ldr(r3,c_frame.param0(sp)))
	__(ldr(r4,c_frame.param1(sp)))
        __(lfd f1,c_frame.param2(sp))
	__(ldr(r5,c_frame.savelr(sp)))
	__(mtlr r5)
	__(ldr(r5,c_frame.crsave(sp)))
	__(mtcr r5)
	__(blr)
        
/* almost exactly as above, but "swap exception handling info"
   on exit and return */
_spentry(poweropen_ffcallX)
	__(mflr loc_pc)
	__(vpush_saveregs())		/* Now we can use save0-save7 to point to stacks */
	__(mr save0,rcontext)	/* or address globals. */
	__(extract_typecode(imm0,arg_z))
	__(cmpri(cr7,imm0,subtag_macptr))
	__(ldr(save1,c_frame.backlink(sp)))	/* bottom of reserved lisp frame */
	__(la save2,-lisp_frame.size(save1))	/* top of lisp frame*/
        __(zero_doublewords save2,0,lisp_frame.size)
	__(str(save1,lisp_frame.backlink(save2)))
	__(str(save2,c_frame.backlink(sp)))
	__(str(fn,lisp_frame.savefn(save2)))
	__(str(loc_pc,lisp_frame.savelr(save2)))
	__(str(vsp,lisp_frame.savevsp(save2)))
	__(bne cr7,1f)
	__(ldr(arg_z,macptr.address(arg_z)))
1:
	__(ldr(save3,tcr.cs_area(rcontext)))
	__(str(save2,area.active(save3)))
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(str(rzero,tcr.ffi_exception(rcontext)))
	__(mffs f0)
	__(stfd f0,tcr.lisp_fpscr(rcontext))	/* remember lisp's fpscr */
	__(mtfsf 0xff,fp_zero)	/* zero foreign fpscr */
	__(ldr(r3,tcr.foreign_exception_status(rcontext)))
	__(cmpri(r3,0))
	__(ref_global(r12,lisp_exit_hook))
	__(mtctr r12)
	__(beq+ 1f)
	__(stru(sp,-(stack_align(c_frame.minsiz))(sp)))
	__(bctrl)
	__(la sp,(stack_align(c_frame.minsiz))(sp))
1:	
	__(li rcontext,0)
	__(mtctr arg_z)
	__(ldr(r3,c_frame.param0(sp)))
	__(ldr(r4,c_frame.param1(sp)))
	__(ldr(r5,c_frame.param2(sp)))
	__(ldr(r6,c_frame.param3(sp)))
	__(ldr(r7,c_frame.param4(sp)))
	__(ldr(r8,c_frame.param5(sp)))
	__(ldr(r9,c_frame.param6(sp)))
	__(ldr(r10,c_frame.param7(sp)))
	/* Darwin is allegedly very picky about what register points
	   to the function on entry. */
	__(mr r12,arg_z)
	__(bctrl)
	__(ref_global(r12,lisp_return_hook))
	__(mtctr r12)
	__(str(r3,c_frame.param0(sp)))
	__(str(r4,c_frame.param1(sp)))
	__(stfd f1,c_frame.param2(sp))
	__(stru(sp,-(stack_align(c_frame.minsiz))(sp)))
	__(mr r3,save0)
	__(bctrl)
	__(la sp,(stack_align(c_frame.minsiz))(sp))
	__(ldr(r3,c_frame.param0(sp)))
	__(ldr(r4,c_frame.param1(sp)))
	__(lfd f1,c_frame.param2(sp))
	__(b FF_call_return_common)	
                        
/* 
	This gets called with R11 holding the unboxed callback index.
*/
_spentry(eabi_callback)
	/* First, we extend the C frame so that it has room for
	incoming arg regs. */
	__(ldr(r0,eabi_c_frame.backlink(sp)))
	__(stru(r0,eabi_c_frame.param0-varargs_eabi_c_frame.incoming_stack_args(sp)))
	__(mflr r0)
	__(str(r0,varargs_eabi_c_frame.savelr(sp)))
	__(str(r3,varargs_eabi_c_frame.gp_save+(0*4)(sp)))
	__(str(r4,varargs_eabi_c_frame.gp_save+(1*4)(sp)))
	__(str(r5,varargs_eabi_c_frame.gp_save+(2*4)(sp)))
	__(str(r6,varargs_eabi_c_frame.gp_save+(3*4)(sp)))
	__(str(r7,varargs_eabi_c_frame.gp_save+(4*4)(sp)))
	__(str(r8,varargs_eabi_c_frame.gp_save+(5*4)(sp)))
	__(str(r9,varargs_eabi_c_frame.gp_save+(6*4)(sp)))
	__(str(r10,varargs_eabi_c_frame.gp_save+(7*4)(sp)))
	/* Could check the appropriate CR bit and skip saving FP regs here */
	__(stfd f1,varargs_eabi_c_frame.fp_save+(0*8)(sp))
	__(stfd f2,varargs_eabi_c_frame.fp_save+(1*8)(sp))
	__(stfd f3,varargs_eabi_c_frame.fp_save+(2*8)(sp))
	__(stfd f4,varargs_eabi_c_frame.fp_save+(3*8)(sp))
	__(stfd f5,varargs_eabi_c_frame.fp_save+(4*8)(sp))
	__(stfd f6,varargs_eabi_c_frame.fp_save+(5*8)(sp))
	__(stfd f7,varargs_eabi_c_frame.fp_save+(6*8)(sp))
	__(stfd f8,varargs_eabi_c_frame.fp_save+(7*8)(sp))
	__(la r0,varargs_eabi_c_frame.incoming_stack_args(sp))
	__(str(r0,varargs_eabi_c_frame.overflow_arg_area(sp)))
	__(la r0,varargs_eabi_c_frame.regsave(sp))
	__(str(r0,varargs_eabi_c_frame.reg_save_area(sp)))
	__(li r0,0)
	__(str(r0,varargs_eabi_c_frame.flags(sp)))

	/* Save the non-volatile registers on the sp stack */
	/* This is a non-standard stack frame, but noone will ever see it, */
        /* so it doesn't matter. It will look like more of the stack frame pushed below. */
	__(stru(sp,-(c_reg_save.size)(sp)))
        __(str(r13,c_reg_save.save_gprs+(0*node_size)(sp)))
        __(str(r14,c_reg_save.save_gprs+(1*node_size)(sp)))
        __(str(r15,c_reg_save.save_gprs+(2*node_size)(sp)))
        __(str(r16,c_reg_save.save_gprs+(3*node_size)(sp)))
        __(str(r17,c_reg_save.save_gprs+(4*node_size)(sp)))
        __(str(r18,c_reg_save.save_gprs+(5*node_size)(sp)))
        __(str(r19,c_reg_save.save_gprs+(6*node_size)(sp)))
        __(str(r20,c_reg_save.save_gprs+(7*node_size)(sp)))
        __(str(r21,c_reg_save.save_gprs+(8*node_size)(sp)))
        __(str(r22,c_reg_save.save_gprs+(9*node_size)(sp)))
        __(str(r23,c_reg_save.save_gprs+(10*node_size)(sp)))
        __(str(r24,c_reg_save.save_gprs+(11*node_size)(sp)))
        __(str(r25,c_reg_save.save_gprs+(12*node_size)(sp)))
        __(str(r26,c_reg_save.save_gprs+(13*node_size)(sp)))
        __(str(r27,c_reg_save.save_gprs+(14*node_size)(sp)))
        __(str(r28,c_reg_save.save_gprs+(15*node_size)(sp)))
        __(str(r29,c_reg_save.save_gprs+(16*node_size)(sp)))
        __(str(r30,c_reg_save.save_gprs+(17*node_size)(sp)))
        __(str(r31,c_reg_save.save_gprs+(18*node_size)(sp)))
	__(mffs f0)
	__(stfd f0,c_reg_save.save_fp_zero(sp))
	__(ldr(r31,c_reg_save.save_fp_zero+4(sp)))	/* recover FPSCR image */
	__(str(r31,c_reg_save.save_fpscr(sp)))
	__(lwi(r30,0x43300000))
	__(lwi(r31,0x80000000))
	__(str(r30,c_reg_save.save_fp_zero(sp)))
	__(str(r31,c_reg_save.save_fp_zero+4(sp)))
	__(stfd fp_s32conv,c_reg_save.save_fps32conv(sp))
	__(lfd fp_s32conv,c_reg_save.save_fp_zero(sp))
	__(stfd fp_zero,c_reg_save.save_fp_zero(sp))
	__(lfs fp_zero,lisp_globals.short_float_zero(0))	/* ensure that fp_zero contains 0.0 */

	
/* Restore rest of Lisp context. */
/* Could spread out the memory references here to gain a little speed */
	__(li loc_pc,0)
	__(li fn,0)                     /* subprim, not a lisp function */
	__(li temp3,0)
	__(li temp2,0)
	__(li temp1,0)
	__(li temp0,0)
	__(li arg_x,0)
	__(box_fixnum(arg_y,r11))	/* callback-index */
	__(la arg_z,c_reg_save.size+varargs_eabi_c_frame.gp_save(sp))	/* parameters (tagged as a fixnum) */

	/* Recover lisp thread context. Have to call C code to do so. */
	__(ref_global(r12,get_tcr))
	__(mtctr r12)
        __(li r3,1)
	__(stru(sp,-(stack_align(eabi_c_frame.minsiz))(sp)))
	__(bctrl)
	__(la sp,(stack_align(eabi_c_frame.minsiz))(sp))
	__(la rcontext,TCR_BIAS(r3))
	__(li allocptr,0)
	__(li allocbase,0)
	__(ldr(vsp,tcr.save_vsp(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))		
	__(li rzero,0)
	__(mtxer rzero) /* lisp wants the overflow bit clear */
	__(li imm0,TCR_STATE_LISP)
	__(li save0,0)
	__(li save1,0)
	__(li save2,0)
	__(li save3,0)
	__(li save4,0)
	__(li save5,0)
	__(li save6,0)
	__(li save7,0)
        __(mtctr rzero)
	__(str(imm0,tcr.valence(rcontext)))
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
	__(lfd f0,tcr.lisp_fpscr(rcontext))
	__(mtfsf 0xff,f0)

	/* load nargs and callback to the lisp */
	__(set_nargs(2))
	__(ldr(imm2,tcr.cs_area(rcontext)))
	__(ldr(imm4,area.active(imm2)))
	__(stru(imm4,-lisp_frame.size(sp)))
	__(str(imm3,lisp_frame.savelr(sp)))
	__(str(vsp,lisp_frame.savevsp(sp)))	/* for stack overflow code */
	__(li fname,nrs.callbacks)	/* %pascal-functions% */
	__(call_fname)
	__(ldr(imm2,lisp_frame.backlink(sp)))
	__(ldr(imm3,tcr.cs_area(rcontext)))
	__(str(imm2,area.active(imm3)))
	__(discard_lisp_frame())
	/* save_vsp will be restored from ff_call's stack frame, but */
	/* I included it here for consistency. */
	/* save_tsp is set below after we exit Lisp context. */
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	/* Exit lisp context */
	/* This is not necessary yet, but will be once we can be interrupted */
	__(li imm1,TCR_STATE_FOREIGN)
	__(str(imm1,tcr.valence(rcontext)))
	/* Restore the non-volatile registers & fpscr */
	__(lfd fp_zero,c_reg_save.save_fp_zero(sp))
	__(ldr(r31,c_reg_save.save_fpscr(sp)))
	__(str(r31,c_reg_save.save_fp_zero+4(sp)))
	__(lfd f0,c_reg_save.save_fp_zero(sp))
	__(mtfsf 0xff,f0)
	__(ldr(r13,c_reg_save.save_gprs+(0*node_size)(sp)))
	__(ldr(r14,c_reg_save.save_gprs+(1*node_size)(sp)))
	__(ldr(r15,c_reg_save.save_gprs+(2*node_size)(sp)))
	__(ldr(r16,c_reg_save.save_gprs+(3*node_size)(sp)))
	__(ldr(r17,c_reg_save.save_gprs+(4*node_size)(sp)))
	__(ldr(r18,c_reg_save.save_gprs+(5*node_size)(sp)))
	__(ldr(r19,c_reg_save.save_gprs+(6*node_size)(sp)))
	__(ldr(r20,c_reg_save.save_gprs+(7*node_size)(sp)))
	__(ldr(r21,c_reg_save.save_gprs+(8*node_size)(sp)))
	__(ldr(r22,c_reg_save.save_gprs+(9*node_size)(sp)))
	__(ldr(r23,c_reg_save.save_gprs+(10*node_size)(sp)))
	__(ldr(r24,c_reg_save.save_gprs+(11*node_size)(sp)))
	__(ldr(r25,c_reg_save.save_gprs+(12*node_size)(sp)))
	__(ldr(r26,c_reg_save.save_gprs+(13*node_size)(sp)))
	__(ldr(r27,c_reg_save.save_gprs+(14*node_size)(sp)))
	__(ldr(r28,c_reg_save.save_gprs+(15*node_size)(sp)))
	__(ldr(r29,c_reg_save.save_gprs+(16*node_size)(sp)))
	__(ldr(r30,c_reg_save.save_gprs+(17*node_size)(sp)))
	__(ldr(r31,c_reg_save.save_gprs+(18*node_size)(sp)))
	__(lfd fp_s32conv,c_reg_save.save_fps32conv(sp))
	__(ldr(sp,0(sp)))

	__(ldr(r3,varargs_eabi_c_frame.gp_save+(0*4)(sp)))
	__(ldr(r4,varargs_eabi_c_frame.gp_save+(1*4)(sp)))
	__(lfd f1,varargs_eabi_c_frame.gp_save+(2*4)(sp))
	__(ldr(r5,varargs_eabi_c_frame.savelr(sp)))
	__(str(r5,varargs_eabi_c_frame.old_savelr(sp)))
	__(mtlr r5)
	__(ldr(r5,varargs_eabi_c_frame.backlink(sp)))
	__(str(r5,varargs_eabi_c_frame.old_backlink(sp)))
	__(la sp,varargs_eabi_c_frame.old_backlink(sp))
	__(blr)

	
/*
	Do a linux system call:	 the system call index is (boxed)
	in arg_z, and other arguments are in an eabi_c_frame on
	the C stack.  As is the case with an eabi_ff_call, there's
	a lisp frame reserved underneath the eabi_c_frame.

	This is a little simpler than eabi_ff_call, because we
	can assume that there are no synchronous callbacks to
	lisp (that might cause a GC.)  It's also simpler for the
	caller, since we return error status atomically.

	A system call can clobber any or all of r9-r12, so we need
	to save and restore allocptr, allocbase, and tsp.
	*/
_spentry(eabi_syscall)
/*
	We're entered with an eabi_c_frame on the C stack.  There's a
	lisp_frame reserved underneath it; we'll link it in in a minute.
	Load the outgoing GPR arguments from eabi_c_frame.param[0-7],
	then shrink the eabi_c_frame.
*/
	__(mflr loc_pc)
        __(vpush_saveregs())
	__(str(sp,eabi_c_frame.savelr(sp)))
	__(li arg_x,nil_value)
	__(mr temp0,rcontext)
	__(ldr(temp1,c_frame.backlink(sp)))	/* bottom of reserved lisp frame */
	__(la temp2,-lisp_frame.size(temp1))	/* top of lisp frame */
        __(zero_doublewords temp2,0,lisp_frame.size)
	__(str(temp1,lisp_frame.backlink(temp2)))
	__(str(temp2,c_frame.backlink(sp)))
	__(str(fn,lisp_frame.savefn(temp2)))
	__(str(loc_pc,lisp_frame.savelr(temp2)))
	__(str(vsp,lisp_frame.savevsp(temp2)))
	__(ldr(temp3,tcr.cs_area(rcontext)))
	__(str(temp2,area.active(temp3)))
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(str(rzero,tcr.ffi_exception(rcontext)))
	__(li imm1,TCR_STATE_FOREIGN)
	__(str(imm1,tcr.valence(rcontext)))
	__(ldr(r13,lisp_globals.saveR13(0)))
	__(ldr(r3,eabi_c_frame.param0(sp)))
	__(ldr(r4,eabi_c_frame.param1(sp)))
	__(ldr(r5,eabi_c_frame.param2(sp)))
	__(ldr(r6,eabi_c_frame.param3(sp)))
	__(ldr(r7,eabi_c_frame.param4(sp)))
	__(ldr(r8,eabi_c_frame.param5(sp)))
	__(ldr(r9,eabi_c_frame.param6(sp)))
	__(ldr(r10,eabi_c_frame.param7(sp)))
	__(la temp1,eabi_c_frame.minsiz-eabi_c_frame.param0(sp))
	__(str(rzero,eabi_c_frame.savelr(temp1)))
	__(str(temp2,eabi_c_frame.backlink(temp1)))
	__(mr sp,temp1)
	__(unbox_fixnum(r0,arg_z))
	__(sc)
	__(nop)
	/* C should have preserved temp0 (= rcontext) for us. */
	__(ldr(sp,0(sp)))
	__(mr imm2,temp0)
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(li rzero,0)
	__(mr loc_pc,rzero)
	__(mr fn,rzero)
	__(li arg_x,nil_value)
	__(li arg_y,nil_value)
	__(li arg_z,nil_value)
	__(li temp0,nil_value)
	__(li temp1,nil_value)
	__(li temp2,nil_value)
	__(li temp3,nil_value)
	__(li fn,nil_value)
        
	__(li imm3,TCR_STATE_LISP)
	__(mr rcontext,imm2)
        __(li save0,0)
        __(li save1,0)
        __(li save2,0)
        __(li save3,0)
        __(li save4,0)
        __(li save5,0)
        __(li save6,0)
        __(li save7,0)        
	__(str(imm3,tcr.valence(rcontext)))
	__(vpop_saveregs)
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame())
	__(bns 1f)
	__(neg r3,r3)
1:      
	__(check_pending_interrupt([cr1]))                
	__(mtxer rzero)
	__(blr)

/*
  Do a system call in Darwin.  The stack is set up much as it would be
  for a PowerOpen ABI ff-call:	register parameters are in the stack
  frame, and there are 4 extra words at the bottom of the frame that
  we can carve a lisp frame out of.

  System call return conventions are a little funky in Darwin: if "@sc"
  is the address of the "sc" instruction, errors return to @sc+4 and
  non-error cases return to @sc+8.  Error values are returned as
  positive values in r3; this is true even if the system call returns
  a doubleword (64-bit) result.  Since r3 would ordinarily contain
  the high half of a doubleword result, this has to be special-cased.

  The caller should set the c_frame.crsave field of the stack frame
  to 0 if the result is to be interpreted as anything but a doubleword
  and to non-zero otherwise.  (This only matters on an error return.)
*/
        
_spentry(poweropen_syscall)
	__(mflr loc_pc)
	__(vpush_saveregs())
	__(ldr(imm1,0(sp)))
	__(la imm2,-lisp_frame.size(imm1))
        __(zero_doublewords imm2,0,lisp_frame.size)
	__(str(imm1,lisp_frame.backlink(imm2)))
	__(str(imm2,c_frame.backlink(sp)))
	__(str(fn,lisp_frame.savefn(imm2)))
	__(str(loc_pc,lisp_frame.savelr(imm2)))
	__(str(vsp,lisp_frame.savevsp(imm2)))
	__(ldr(imm3,tcr.cs_area(rcontext)))
	__(str(imm2,area.active(imm3)))
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(str(rzero,tcr.ffi_exception(rcontext)))
	__(mr save0,rcontext)
	__(li r3,TCR_STATE_FOREIGN)
	__(str(r3,tcr.valence(rcontext)))
	__(li rcontext,0)
	__(ldr(r3,c_frame.param0(sp)))
	__(ldr(r4,c_frame.param1(sp)))
	__(ldr(r5,c_frame.param2(sp)))
	__(ldr(r6,c_frame.param3(sp)))
	__(ldr(r7,c_frame.param4(sp)))
	__(ldr(r8,c_frame.param5(sp)))
	__(ldr(r9,c_frame.param6(sp)))
	__(ldr(r10,c_frame.param7(sp)))
	__(unbox_fixnum(r0,arg_z))
	__(sc)
        __ifdef([LINUX])
         __(bns+ 9f)
        __else
	 __(b 1f)
	 __(b 9f)
        __endif
1:
        __ifdef([PPC64])
         __(neg r3,r3)
        __else
	 __(ldr(imm2,c_frame.crsave(sp)))
	 __(cmpri(cr0,imm2,0))
	 __(bne cr0,2f)
	 /* 32-bit result */
	 __(neg r3,r3)
	 __(b 9f)
2:
	 /* 64-bit result */
	 __(neg r4,r3)
	 __(li r3,-1)
        __endif
9:
	__(mr imm2,save0)	/* recover context */
	__(ldr(sp,c_frame.backlink(sp)))
	__(li imm4,TCR_STATE_LISP)
	__(li rzero,0)
	__(li loc_pc,0)
	__(li arg_x,nil_value)
	__(li arg_y,nil_value)
	__(li arg_z,nil_value)
	__(li temp0,nil_value)
	__(li temp1,nil_value)
	__(li temp2,nil_value)
	__(li temp3,nil_value)
	__(li fn,nil_value)
	__(mr rcontext,imm2)
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))
        __(li save0,0)
        __(li save1,0)
        __(li save2,0)
        __(li save3,0)
        __(li save4,0)
        __(li save5,0)
        __(li save6,0)
        __(li save7,0)        
	__(str(imm4,tcr.valence(rcontext)))
	__(vpop_saveregs)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame)
        __(mtxer rzero)
	__(check_pending_interrupt([cr1]))
	__(blr)
                
/*
	We're entered with an eabi_c_frame on the C stack.  There's a
	lisp_frame reserved underneath it; we'll link it in in a minute.
	Load the outgoing GPR arguments from eabi_c_frame.param[0-7],
	then shrink the eabi_c_frame.
*/
	
_spentry(eabi_ff_call)
	__(mflr loc_pc)
	__(str(sp,eabi_c_frame.savelr(sp)))
	__(vpush_saveregs())		/* Now we can use save0-save7 to point to stacks */
	__(mr save0,rcontext)	/* or address globals. */
	__(extract_typecode(imm0,arg_z))
	__(cmpri(imm0,subtag_macptr))
	__(ldr(save1,0(sp)))	/* bottom of reserved lisp frame */
	__(la save2,-lisp_frame.size(save1))	/* top of lisp frame*/
        __(zero_doublewords save2,0,lisp_frame.size)
	__(str(save1,lisp_frame.backlink(save2)))
	__(str(save2,c_frame.backlink(sp)))
	__(str(fn,lisp_frame.savefn(save2)))
	__(str(loc_pc,lisp_frame.savelr(save2)))
	__(str(vsp,lisp_frame.savevsp(save2)))
	__(bne 1f)
	__(ldr(arg_z,macptr.address(arg_z)))
1:
	__(ldr(save3,tcr.cs_area(rcontext)))
	__(str(save2,area.active(save3)))
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
	__(mtctr arg_z)
	__(str(rzero,tcr.ffi_exception(rcontext)))
	__(mffs f0)
	__(stfd f0,tcr.lisp_fpscr(rcontext))	/* remember lisp's fpscr */
	__(mtfsf 0xff,fp_zero)	/* zero foreign fpscr */
	__(li imm1,TCR_STATE_FOREIGN)
	__(str(imm1,tcr.valence(rcontext)))
	__(ldr(r2,tcr.native_thread_info(rcontext)))
	__(ldr(r13,lisp_globals.saveR13(0)))
	__(ldr(r3,eabi_c_frame.param0(sp)))
	__(ldr(r4,eabi_c_frame.param1(sp)))
	__(ldr(r5,eabi_c_frame.param2(sp)))
	__(ldr(r6,eabi_c_frame.param3(sp)))
	__(ldr(r7,eabi_c_frame.param4(sp)))
	__(ldr(r8,eabi_c_frame.param5(sp)))
	__(ldr(r9,eabi_c_frame.param6(sp)))
	__(ldr(r10,eabi_c_frame.param7(sp)))
	__(la save1,eabi_c_frame.minsiz-eabi_c_frame.param0(sp))
	__(str(rzero,eabi_c_frame.savelr(save1)))
	__(str(save2,eabi_c_frame.backlink(save1)))
	__(mr sp,save1)
	/* If we're calling a varargs C function, it'll want to
	know whether or not we've passed any args in FP regs.
	Better to say that we did (and force callee to save FP
	arg regs on entry) than to say that we didn't and get
	garbage results */
	__(crset 6)
	__(bctrl)
        _endsubp(eabi_ff_call)
	
        _startfn(FF_call_return_common)
	/* C should have preserved save0 (= rcontext) for us. */
	__(ldr(sp,0(sp)))
	__(mr imm2,save0)
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(li rzero,0)
	__(mr loc_pc,rzero)
	__(li arg_x,nil_value)
	__(li arg_y,nil_value)
	__(li arg_z,nil_value)
	__(li temp0,nil_value)
	__(li temp1,nil_value)
	__(li temp2,nil_value)
	__(li temp3,nil_value)
	__(li fn,nil_value)
	__(mr rcontext,imm2)
	__(li imm2,TCR_STATE_LISP)
	__(ldr(tsp,tcr.save_tsp(rcontext)))
        __(li save0,0)
        __(li save1,0)
        __(li save2,0)
        __(li save3,0)
        __(li save4,0)
        __(li save5,0)
        __(li save6,0)
        __(li save7,0)        
	__(str(imm2,tcr.valence(rcontext)))	
	__(vpop_saveregs())
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(mffs f0)
	__(stfd f0,8(sp))
	__(lwz imm3,12(sp))	/* imm3 = FPSCR after call */
        __(clrrwi imm2,imm3,8)
	__(discard_lisp_frame())
	__(str(imm2,tcr.ffi_exception(rcontext)))
	__(lfd f0,tcr.lisp_fpscr(rcontext))
	__(mtfsf 0xff,f0)
	__(check_pending_interrupt([cr1]))
        __(mtxer rzero)
        __(mtctr rzero)
	__(blr)
