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

	.globl _SPmkcatch1v
	.globl _SPnthrow1value


/* This is called from a c-style context and calls a lisp function. */
/* This does the moral equivalent of */
/*   (loop 
       (catch %toplevel-catch% 
         (let* ((fn (symbol-value *toplevel-function*)))
           (if fn (funcall fn) (return nil))))))) 
*/

_exportfn(toplevel_loop)
	__(mflr imm0)
ifdef([DARWIN],[
	__(str(imm0,c_frame.savelr(sp)))
],[
	__(str(imm0,eabi_c_frame.savelr(sp)))
])
	__(b local_label(test))
local_label(loop):
	__(ref_nrs_value(arg_z,toplcatch))
	__(bl _SPmkcatch1v)
	__(b local_label(test))			/* cleanup address, not really a branch */

	__(set_nargs(0))
	__(bl _SPfuncall)
	__(li arg_z,nil_value)
	__(li imm0,fixnum_one)
	__(bl _SPnthrow1value)
local_label(test):
	__(ldr(temp0,0(vsp)))
	__(cmpri(cr0,temp0,nil_value))
	__(bne cr0,local_label(loop))
local_label(back_to_c):
ifdef([DARWIN],[
	__(ldr(imm0,c_frame.savelr(sp)))
],[
	__(ldr(imm0,eabi_c_frame.savelr(sp)))
])
	__(mtlr imm0)
	__(blr)
	_endfn


/* This sucker gets called with R3 pointing to the current TCR. */
/* r4 is 0 if we want to start the whole thing rolling, */
/* non-zero if we want to reset the current process */
/* by throwing to toplevel */

	.globl _SPreset
_exportfn(C(start_lisp))
	__(mflr r0)
	__(mr r5,r2)
	__(mr rcontext,r3)
ifdef([DARWIN],[
	__(str(r0,c_frame.savelr(sp)))
	__(stru(sp,-(stack_align(c_frame.minsiz+(32*4)))(sp)))
	__(stmw r13,c_frame.minsiz(sp)) /* don't worry about the stmw. */
	__(stfd fp_s32conv,c_frame.minsiz+(22*4)(sp))
],[
	__(str(r0,eabi_c_frame.savelr(sp)))
	__(stru(sp,-(eabi_c_frame.minsiz+(32*4))(sp)))
	__(stmw r13,eabi_c_frame.minsiz(sp)) /* don't worry about the stmw. */
	__(stfd fp_s32conv,eabi_c_frame.minsiz+(22*4)(sp))
])
	__(lwi(r30,0x43300000))
	__(lwi(r31,0x80000000))
ifdef([DARWIN],[
	__(str(r30,c_frame.minsiz+(20*4)(sp)))
	__(str(r31,c_frame.minsiz+(20*4)+4(sp)))
	__(lfd fp_s32conv,c_frame.minsiz+(20*4)(sp))
	__(stfd fp_zero,c_frame.minsiz+(20*4)(sp))
],[
	__(str(r30,eabi_c_frame.minsiz+(20*4)(sp)))
	__(str(r31,eabi_c_frame.minsiz+(20*4)+4(sp)))
	__(lfd fp_s32conv,eabi_c_frame.minsiz+(20*4)(sp))
	__(stfd fp_zero,eabi_c_frame.minsiz+(20*4)(sp))
])
	__(lfs fp_zero,lisp_globals.short_float_zero(0))
	__(lfd f0,tcr.lisp_fpscr(rcontext))
        __(mtfsf 0xff,f0)
	__(li rzero,0)
	__(mr save0,rzero)
	__(mr save1,rzero)
	__(mr save2,rzero)
	__(mr save3,rzero)
	__(mr save4,rzero)
	__(mr save5,rzero)
	__(mr save6,rzero)
	__(mr save7,rzero)
	__(mr arg_z,rzero)
	__(mr arg_y,rzero)
	__(mr arg_x,rzero)
	__(mr temp0,rzero)
	__(mr temp1,rzero)
	__(mr temp2,rzero)
	__(mr temp3,rzero)
	__(li loc_pc,0)
	__(li fn,0)
        __(li old_fn,0)
	__(cmpri(cr0,r4,0))
	__(mtxer rzero)  /* start lisp with the overflow bit clear */
	__(ldr(vsp,tcr.save_vsp(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
        __(li imm0,TCR_STATE_LISP)
        __(str(imm0,tcr.valence(rcontext)))
	__(bne cr0,1f)
	__(bl toplevel_loop)
	__(b 2f)
1:
	__(bl _SPreset)
2:
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
        __(li imm0,TCR_STATE_FOREIGN)
        __(str(imm0,tcr.valence(rcontext)))
ifdef([DARWIN],[
	__(lmw r13,c_frame.minsiz(sp))
],[
	__(lmw r13,eabi_c_frame.minsiz(sp))
])
	__(li r3,nil_value)
ifdef([DARWIN],[
	__(lfd fp_zero,c_frame.minsiz+(20*4)(sp))
	__(lfd fp_s32conv,c_frame.minsiz+(22*4)(sp))
	__(ldr(r0,((stack_align(c_frame.minsiz+(32*4)))+c_frame.savelr)(sp)))
],[
	__(lfd fp_zero,eabi_c_frame.minsiz+(20*4)(sp))
	__(lfd fp_s32conv,eabi_c_frame.minsiz+(22*4)(sp))
	__(ldr(r0,(eabi_c_frame.minsiz+(32*4)+eabi_c_frame.savelr)(sp)))
])
	__(mtlr r0)
	__(ldr(sp,0(sp)))
	__(ldr(r2,tcr.native_thread_info(rcontext)))
	__(blr)

_exportfn(_SPsp_end)
	nop
	_endfile

