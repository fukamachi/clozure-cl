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
	
local_label(start):	
define([_spentry],[ifdef([__func_name],[_endfn],[])
	_exportfn(_SP$1)
	.line  __line__
])

             
define([_endsubp],[
	_endfn(_SP$1)
# __line__
])


                	
               
define([jump_builtin],[
	ref_nrs_value(fname,builtin_functions)
	set_nargs($2)
	vrefr(fname,fname,$1)
	jump_fname()
])
	
_spentry(jmpsym)
	__(jump_fname())
        
_spentry(jmpnfn)
	__(jump_nfn())
        
	/* Call temp0 if it's either a symbol or function */
_spentry(funcall)
	__(do_funcall())
	
/* Subprims for catch, throw, unwind_protect. */

/* Push a catch frame on the temp stack (and some of it on the cstack, as well.) */
/* The PC in question is 4 bytes past the caller's return address. ALWAYS. */
/* The catch tag is in arg_z, the multiple-value flags is in imm2. */
/* Bash some of the imm registers and loc_pc. */

_spentry(mkcatch1v)
	__(li imm2,0)
	__(mkcatch())

_spentry(mkunwind)
	__(lwi(arg_z,unbound_marker))
	__(li imm2,fixnum_one)
	__(mkcatch())
	
_spentry(mkcatchmv)
	__(li imm2,fixnum_one)
	__(mkcatch())

/* Caller has pushed tag and 0 or more values; nargs = nvalues. */
/* Otherwise, process unwind-protects and throw to indicated catch frame. */
	
_spentry(throw)
	__(ldr(imm1,tcr.catch_top(rcontext)))
	__(li imm0,0) /* count intervening catch/unwind-protect frames. */
	__(cmpri(cr0,imm1,0))
	__(ldrx(temp0,vsp,nargs))
	__(beq- cr0,local_label(_throw_tag_not_found))
local_label(_throw_loop):
	__(ldr(temp1,catch_frame.catch_tag(imm1)))
	__(cmpr(cr0,temp0,temp1))
	__(mr imm2,imm1)
	__(ldr(imm1,catch_frame.link(imm1)))
	__(cmpri(cr1,imm1,0))
	__(beq cr0,local_label(_throw_found))
	__(addi imm0,imm0,fixnum_one)
	__(beq- cr1,local_label(_throw_tag_not_found))
	__(b local_label(_throw_loop))
/* imm2: (tstack-consed) target catch frame, imm0: count of intervening frames.
  If target isn't a multiple-value receiver, discard extra values 
  (less hair, maybe.) */
local_label(_throw_found):
	__(ldr(imm1,catch_frame.mvflag(imm2)))
	__(cmpri(cr0,imm1,0))
	__(cmpri(cr1,nargs,0))
	__(li fn,0)
	__(add imm1,vsp,nargs)
	__(la imm1,-node_size(imm1))
	__(bne cr0,local_label(_throw_all_values))
	__(set_nargs(1))
	__(beq cr1,local_label(_throw_default_1_val))
	__(mr vsp,imm1)
	__(b local_label(_throw_all_values))
local_label(_throw_default_1_val):
	__(li imm4,nil_value)
	__(vpush(imm4))
local_label(_throw_all_values):
	__(bl _SPnthrowvalues)
	__(ldr(imm3,tcr.catch_top(rcontext)))
	__(ldr(imm1,tcr.db_link(rcontext)))
	__(ldr(imm0,catch_frame.db_link(imm3)))
	__(ldr(imm4,catch_frame.mvflag(imm3)))
	__(cmpr(cr0,imm0,imm1))
	__(cmpri(cr1,imm4,0))
	__(la tsp,-((tsp_frame.fixed_overhead+fulltag_misc))(imm3))
	__(beq cr0,local_label(_throw_dont_unbind))
        __(bl _SPsvar_unbind_to)
local_label(_throw_dont_unbind):
	__(add imm0,vsp,nargs)
	__(cmpri(cr0,nargs,0))
	__(ldr(imm1,catch_frame.csp(imm3)))
	__(ldr(imm1,lisp_frame.savevsp(imm1)))
	__(bne cr1,local_label(_throw_multiple))
/* Catcher expects single value in arg_z */
	__(ldr(arg_z,-node_size(imm0)))
	__(b local_label(_throw_pushed_values))
local_label(_throw_multiple):
	__(beq cr0,local_label(_throw_pushed_values))
	__(mr imm2,nargs)
local_label(_throw_mvloop):
	__(subi imm2,imm2,fixnum_one)
	__(cmpri(imm2,0))
	__(ldru(temp0,-node_size(imm0)))
	__(push(temp0,imm1))
	__(bgt local_label(_throw_mvloop))
local_label(_throw_pushed_values):
	__(mr vsp,imm1)
	__(ldr(sp,catch_frame.csp(imm3)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(discard_lisp_frame())
	__(mtlr loc_pc)
        __(restore_catch_nvrs(imm3))
	__(ldr(imm1,catch_frame.xframe(imm3)))
	__(str(imm1,tcr.xframe(rcontext)))
	__(ldr(imm3,catch_frame.link(imm3)))
	__(str(imm3,tcr.catch_top(rcontext)))
	__(unlink(tsp))
	__(blr)
local_label(_throw_tag_not_found):
	__(uuo_interr(error_throw_tag_missing,temp0))
	__(strux(temp0,vsp,nargs))
	__(b _SPthrow)


/* This takes N multiple values atop the vstack. */
_spentry(nthrowvalues)
	__(mr imm4,imm0)
local_label(_nthrowv_nextframe):
	__(subi imm4,imm4,fixnum_one)
	__(cmpri(cr1,imm4,0))
	__(ldr(temp0,tcr.catch_top(rcontext)))
	__(ldr(imm1,tcr.db_link(rcontext)))
	__(bltlr cr1)
	__(ldr(imm0,catch_frame.db_link(temp0)))
	__(ldr(imm3,catch_frame.link(temp0)))
	__(cmpr(cr0,imm0,imm1))
	__(str(imm3,tcr.catch_top(rcontext)))
	__(ldr(temp1,catch_frame.catch_tag(temp0)))
	__(cmpri(cr7,temp1,unbound_marker))		/* unwind-protect ? */
	__(ldr(sp,catch_frame.csp(temp0)))
	__(beq cr0,local_label(_nthrowv_dont_unbind))
	__(mflr loc_pc)
        __(bl _SPsvar_unbind_to)
	__(mtlr loc_pc)
local_label(_nthrowv_dont_unbind):
	__(beq cr7,local_label(_nthrowv_do_unwind))
/* A catch frame.  If the last one, restore context from there. */
	__(bne cr1,local_label(_nthrowv_skip))
	__(ldr(first_nvr,catch_frame.xframe(temp0)))
	__(str(first_nvr,tcr.xframe(rcontext)))
	__(ldr(imm0,lisp_frame.savevsp(sp)))
	__(str(rzero,lisp_frame.savevsp(sp)))	/* marker for stack overflow code */
	__(add imm1,vsp,nargs)
	__(mr imm2,nargs)
	__(b local_label(_nthrowv_push_test))
local_label(_nthrowv_push_loop):
	__(ldru(temp1,-node_size(imm1)))
	__(push(temp1,imm0))
local_label(_nthrowv_push_test):
	__(cmpri(imm2,0))
	__(subi imm2,imm2,fixnum_one)
	__(bne local_label(_nthrowv_push_loop))
	__(mr vsp,imm0)
        __(restore_catch_nvrs(temp0))

local_label(_nthrowv_skip):
	__(la tsp,-(tsp_frame.fixed_overhead+fulltag_misc)(temp0))
	__(unlink(tsp))
	__(discard_lisp_frame())
	__(b local_label(_nthrowv_nextframe))
local_label(_nthrowv_do_unwind):
/* This is harder.  Call the cleanup code with the multiple values (and */
/* nargs, which is a fixnum.)  Remember the throw count (also a fixnum) */
/* as well. */
/* Save our caller's LR and FN in the csp frame created by the unwind- */
/* protect.  (Clever, eh ?) */
	__(ldr(first_nvr,catch_frame.xframe(temp0)))
	__(str(first_nvr,tcr.xframe(rcontext)))
        __(restore_catch_nvrs(temp0))
	__(la tsp,-(tsp_frame.fixed_overhead+fulltag_misc)(temp0))
	__(unlink(tsp))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(nfn,lisp_frame.savefn(sp)))
	__(mtctr loc_pc)	/* cleanup code address. */
	__(str(fn,lisp_frame.savefn(sp)))
	__(mflr loc_pc)
	__(mr fn,nfn)
	__(str(loc_pc,lisp_frame.savelr(sp)))
	__(dnode_align(imm0,nargs,tsp_frame.fixed_overhead+(2*node_size)))	/* tsp overhead, nargs, throw count */
	__(TSP_Alloc_Var_Boxed_nz(imm0,imm1))
	__(mr imm2,nargs)
	__(add imm1,nargs,vsp)
	__(la imm0,tsp_frame.data_offset(tsp))
	__(str(nargs,0(imm0)))
	__(b local_label(_nthrowv_tpushtest))
local_label(_nthrowv_tpushloop):
	__(ldru(temp0,-node_size(imm1)))
	__(stru(temp0,node_size(imm0)))
	__(subi imm2,imm2,fixnum_one)
local_label(_nthrowv_tpushtest):
	__(cmpri(imm2,0))
	__(bne local_label(_nthrowv_tpushloop))
	__(stru(imm4,node_size(imm0)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(bctrl)
	__(la imm0,tsp_frame.data_offset(tsp))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(discard_lisp_frame())
	__(mtlr loc_pc)
	__(ldr(nargs,0(imm0)))
	__(mr imm2,nargs)
	__(b local_label(_nthrowv_tpoptest))
local_label(_nthrowv_tpoploop):
	__(ldru(temp0,node_size(imm0)))
	__(vpush(temp0))
	__(subi imm2,imm2,fixnum_one)
local_label(_nthrowv_tpoptest):
	__(cmpri(imm2,0))
	__(bne local_label(_nthrowv_tpoploop))
	__(ldr(imm4,node_size(imm0)))
	__(unlink(tsp))
	__(b local_label(_nthrowv_nextframe))


/* This is a (slight) optimization.  When running an unwind-protect, */
/* save the single value and the throw count in the tstack frame. */
/* Note that this takes a single value in arg_z. */
_spentry(nthrow1value)
	__(mr imm4,imm0)
local_label(_nthrow1v_nextframe):
	__(subi imm4,imm4,fixnum_one)
	__(cmpri(cr1,imm4,0))
	__(ldr(temp0,tcr.catch_top(rcontext)))
	__(ldr(imm1,tcr.db_link(rcontext)))
	__(set_nargs(1))
	__(bltlr cr1)
	__(ldr(imm3,catch_frame.link(temp0)))
	__(ldr(imm0,catch_frame.db_link(temp0)))
	__(cmpr(cr0,imm0,imm1))
	__(str(imm3,tcr.catch_top(rcontext)))
	__(ldr(temp1,catch_frame.catch_tag(temp0)))
	__(cmpri(cr7,temp1,unbound_marker))		/* unwind-protect ? */
	__(ldr(sp,catch_frame.csp(temp0)))
	__(beq cr0,local_label(_nthrow1v_dont_unbind))
	 __(mflr loc_pc)
         __(bl _SPsvar_unbind_to)
	 __(mtlr loc_pc)
local_label(_nthrow1v_dont_unbind):
	__(beq cr7,local_label(_nthrow1v_do_unwind))
/* A catch frame.  If the last one, restore context from there. */
	__(bne cr1,local_label(_nthrow1v_skip))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(ldr(first_nvr,catch_frame.xframe(temp0)))
	__(str(first_nvr,tcr.xframe(rcontext)))
        __(restore_catch_nvrs(temp0))
local_label(_nthrow1v_skip):
	__(la tsp,-(tsp_frame.fixed_overhead+fulltag_misc)(temp0))
	__(unlink(tsp))
	__(discard_lisp_frame())
	__(b local_label(_nthrow1v_nextframe))
local_label(_nthrow1v_do_unwind):
/* This is harder, but not as hard (not as much BLTing) as the */
/* multiple-value case. */

/* Save our caller's LR and FN in the csp frame created by the unwind- */
/* protect.  (Clever, eh ?) */

	__(ldr(first_nvr,catch_frame.xframe(temp0)))
	__(str(first_nvr,tcr.xframe(rcontext)))
        __(restore_catch_nvrs(temp0))
	__(la tsp,-(tsp_frame.fixed_overhead+fulltag_misc)(temp0))
	__(unlink(tsp))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(nfn,lisp_frame.savefn(sp)))
	__(mtctr loc_pc)		/* cleanup code address. */
	__(str(fn,lisp_frame.savefn(sp)))
	__(mflr loc_pc)
	__(mr fn,nfn)
	__(str(loc_pc,lisp_frame.savelr(sp)))
	__(TSP_Alloc_Fixed_Boxed(2*node_size)) /* tsp overhead, value, throw count */
	__(str(arg_z,tsp_frame.data_offset(tsp)))
	__(str(imm4,tsp_frame.data_offset+node_size(tsp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(bctrl)
	__(ldr(arg_z,tsp_frame.data_offset(tsp)))
	__(ldr(imm4,tsp_frame.data_offset+node_size(tsp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(discard_lisp_frame())
	__(mtlr loc_pc)
	__(unlink(tsp))
	__(b local_label(_nthrow1v_nextframe))



_spentry(req_stack_restv_arg)
       
_spentry(stack_cons_restv_arg)

_spentry(stack_restv_arg)
       

_spentry(vspreadargz)
        __(nop)

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
  threads are suspended.
  We can unconditionally set the suspended thread's PC to its LR.
*/
_spentry(rplaca)
        .globl C(egc_rplaca)
C(egc_rplaca):          
        __(cmplr(cr2,arg_z,arg_y))
        __(_rplaca(arg_y,arg_z))
        __(blelr cr2)
        __(ref_global(imm2,heap_start))
        __(sub imm0,arg_y,imm2)
        __(load_highbit(imm3))
        __(srri(imm0,imm0,dnode_shift))       
        __(ref_global(imm1,oldspace_dnode_count))
        __(extract_bit_shift_count(imm4,imm0))
        __(cmplr(imm0,imm1))
        __(srr(imm3,imm3,imm4))
        __(srri(imm0,imm0,bitmap_shift))       
        __(ref_global(imm2,refbits))
        __(bgelr)
        __(slri(imm0,imm0,word_shift))
1:      __(lrarx(imm1,imm2,imm0))
        __(or imm1,imm1,imm3)
        __(strcx(imm1,imm2,imm0))
        __(bne- 1b)
        __(isync)
        __(blr)

_spentry(rplacd)
        .globl C(egc_rplacd)
C(egc_rplacd):          
        __(cmplr(cr2,arg_z,arg_y))
	__(_rplacd(arg_y,arg_z))
        __(blelr cr2)
        __(ref_global(imm2,heap_start))
        __(sub imm0,arg_y,imm2)
        __(load_highbit(imm3))
        __(srri(imm0,imm0,dnode_shift))       
        __(ref_global(imm1,oldspace_dnode_count))
        __(extract_bit_shift_count(imm4,imm0))
        __(cmplr(imm0,imm1))
        __(srr(imm3,imm3,imm4))
        __(srri(imm0,imm0,bitmap_shift))       
        __(ref_global(imm2,refbits))
        __(bgelr)
        __(slri(imm0,imm0,word_shift))
1:      __(lrarx(imm1,imm2,imm0))
        __(or imm1,imm1,imm3)
        __(strcx(imm1,imm2,imm0))
        __(bne- 1b)
        __(isync)
        __(blr)

/*
  Storing into a gvector can be handled the same way as storing into a CONS.
*/

_spentry(gvset)
        .globl C(egc_gvset)
C(egc_gvset):
        __(cmplr(cr2,arg_z,arg_x))
        __(la imm0,misc_data_offset(arg_y))
        __(strx(arg_z,arg_x,imm0))
        __(blelr cr2)
        __(add imm0,imm0,arg_x)
        __(ref_global(imm2,heap_start))
        __(load_highbit(imm3))
        __(ref_global(imm1,oldspace_dnode_count))
        __(sub imm0,imm0,imm2)
        __(srri(imm0,imm0,dnode_shift))       
        __(cmplr(imm0,imm1))
        __(extract_bit_shift_count(imm4,imm0))
        __(srri(imm0,imm0,bitmap_shift))       
        __(srr(imm3,imm3,imm4))
        __(ref_global(imm2,refbits))
        __(bgelr)
        __(slri(imm0,imm0,word_shift))
1:      __(lrarx(imm1,imm2,imm0))
        __(or imm1,imm1,imm3)
        __(strcx(imm1,imm2,imm0))
        __(bne- 1b)
        __(isync)
        __(blr)

/* This is a special case of storing into a gvector: if we need to memoize the store,
   record the address of the hash-table vector in the refmap, as well.
*/        
_spentry(set_hash_key)
        .globl C(egc_set_hash_key)
C(egc_set_hash_key):  
        __(cmplr(cr2,arg_z,arg_x))
        __(la imm0,misc_data_offset(arg_y))
        __(strx(arg_z,arg_x,imm0))
        __(blelr cr2)
        __(add imm0,imm0,arg_x)
        __(ref_global(imm2,heap_start))
        __(load_highbit(imm3))
        __(ref_global(imm1,oldspace_dnode_count))
        __(sub imm0,imm0,imm2)
        __(srri(imm0,imm0,dnode_shift))       
        __(cmplr(imm0,imm1))
        __(extract_bit_shift_count(imm4,imm0))
        __(srri(imm0,imm0,bitmap_shift))       
        __(srr(imm3,imm3,imm4))
        __(ref_global(imm2,refbits))
        __(bgelr)
        __(slri(imm0,imm0,word_shift))
1:      __(lrarx(imm1,imm2,imm0))
        __(or imm1,imm1,imm3)
        __(strcx(imm1,imm2,imm0))
        __(bne- 1b)
        __(isync)
        __(ref_global(imm1,heap_start))
        __(sub imm0,arg_x,imm1)
        __(srri(imm0,imm0,dnode_shift))
        __(load_highbit(imm3))
        __(extract_bit_shift_count(imm4,imm0))
        __(srri(imm0,imm0,bitmap_shift))
        __(srr(imm3,imm3,imm4))
        __(slri(imm0,imm0,word_shift))
2:      __(lrarx(imm1,imm2,imm0))
        __(or imm1,imm1,imm3)
        __(strcx(imm1,imm2,imm0))
        __(bne- 2b)
        __(isync)
        __(blr)
        
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
        __(crclr 2)              /* 2 = cr0_EQ */
        __(cmplr(cr2,arg_z,arg_x))
        __(vpop(temp0))
        __(unbox_fixnum(imm4,temp0))
1:      __(lrarx(temp1,arg_x,imm4))
        __(cmpr(cr1,temp1,arg_y))
        __(bne cr1,3f)
        __(strcx(arg_z,arg_x,imm4))
        __(bne 1b)
        __(isync)
        __(add imm0,imm4,arg_x)
        __(ref_global(imm2,heap_start))
        __(ref_global(imm1,oldspace_dnode_count))
        __(sub imm0,imm0,imm2)
        __(load_highbit(imm3))
        __(srri(imm0,imm0,dnode_shift))       
        __(cmplr(imm0,imm1))
        __(extract_bit_shift_count(imm2,imm0))
        __(srri(imm0,imm0,bitmap_shift))       
        __(srr(imm3,imm3,imm2))
        __(ref_global(imm2,refbits))
        __(bge 5f)
        __(slri(imm0,imm0,word_shift))
2:      __(lrarx(imm1,imm2,imm0))
        __(or imm1,imm1,imm3)
        __(strcx( imm1,imm2,imm0))
        __(bne- 2b)
        __(isync)
        __(b 5f)
3:      __(li imm0,RESERVATION_DISCHARGE)
        __(strcx(rzero,0,imm0))
/*        __(b 4f) */

       .globl C(egc_write_barrier_end)
C(egc_write_barrier_end):
4:      __(li arg_z,nil_value)
        __(blr)
5:      __(li arg_z,t_value)
        __(blr)
        
_spentry(conslist)
	__(li arg_z,nil_value)
	__(cmpri(nargs,0))
	__(b 2f)	
1:
	__(ldr(temp0,0(vsp)))
	__(cmpri(nargs,fixnum_one))
	__(la vsp,node_size(vsp))
	__(Cons(arg_z,temp0,arg_z))
	__(subi nargs,nargs,fixnum_one)
2:
	__(bne 1b)
	__(blr)
	
/* do list*: last arg in arg_z, all others vpushed, nargs set to #args vpushed. */
/* Cons, one cons cell at at time.  Maybe optimize this later. */
_spentry(conslist_star)
	__(cmpri(nargs,0))
	__(b 2f)	
1:
	__(ldr(temp0,0(vsp)))
	__(cmpri(nargs,fixnum_one))
	__(la vsp,node_size(vsp))
	__(Cons(arg_z,temp0,arg_z))
	__(subi nargs,nargs,fixnum_one)
2:
	__(bne 1b)
	__(blr)

/* We always have to create a tsp frame (even if nargs is 0), so the compiler 
   doesn't get confused. */
_spentry(stkconslist)
	__(li arg_z,nil_value)
	__(cmpri(cr1,nargs,0))
	__(add imm1,nargs,nargs)
	__(addi imm1,imm1,tsp_frame.fixed_overhead)
	__(TSP_Alloc_Var_Boxed(imm1,imm2))
	__(la imm1,tsp_frame.data_offset+fulltag_cons(tsp))
	__(b 2f)
1:	__(ldr(temp0,0(vsp)))
	__(cmpri(cr1,nargs,fixnum_one))
	__(la vsp,node_size(vsp))
	__(_rplaca(imm1,temp0))
	__(_rplacd(imm1,arg_z))
	__(mr arg_z,imm1)
	__(la imm1,cons.size(imm1))
	__(la nargs,-fixnum_one(nargs))
2:
	__(bne cr1,1b)
	__(blr)

/* do list*: last arg in arg_z, all others vpushed, 
	nargs set to #args vpushed. */
_spentry(stkconslist_star)
	__(cmpri(cr1,nargs,0))
	__(add imm1,nargs,nargs)
	__(addi imm1,imm1,tsp_frame.fixed_overhead)
	__(TSP_Alloc_Var_Boxed(imm1,imm2))
	__(la imm1,tsp_frame.data_offset+fulltag_cons(tsp))
	__(b 2f)
1:	__(ldr(temp0,0(vsp)))
	__(cmpri(cr1,nargs,fixnum_one))
	__(la vsp,node_size(vsp))
	__(_rplaca(imm1,temp0))
	__(_rplacd(imm1,arg_z))
	__(mr arg_z,imm1)
	__(la imm1,cons.size(imm1))
	__(la nargs,-fixnum_one(nargs))
2:
	__(bne cr1,1b)
	__(blr)


/* Make a stack-consed simple-vector out of the NARGS objects 
	on top of the vstack; return it in arg_z. */
_spentry(mkstackv)
	__(cmpri(cr1,nargs,0))
	__(dnode_align(imm1,nargs,tsp_frame.fixed_overhead+node_size))
	__(TSP_Alloc_Var_Boxed_nz(imm1,imm2))
	__(slwi imm0,nargs,num_subtag_bits-fixnumshift)
	__(ori imm0,imm0,subtag_simple_vector)
	__(str(imm0,tsp_frame.data_offset(tsp)))
	__(la arg_z,tsp_frame.data_offset+fulltag_misc(tsp))
	__(beq- cr1,2f)
	__(la imm0,misc_data_offset(arg_z))
	__(add imm1,imm0,nargs)
1:
	__(la nargs,-node_size(nargs))
	__(cmpri(cr1,nargs,0))
	__(ldr(temp1,0(vsp)))
	__(la vsp,node_size(vsp))
	__(stru(temp1,-node_size(imm1)))
	__(bne cr1,1b)
2:
	__(blr)

	
        

/* Is it worth trying to avoid (postpone) consing here ? */
_spentry(newblocktag)
        __(li imm1,lisp_globals.block_tag_counter)
1:              
        __(lrarx(imm0,0,imm1))
	__(addi imm0,imm0,1<<num_subtag_bits)
	__(cmpri(imm0,0))
	__(ori arg_z,imm0,subtag_block_tag)
	__(beq- local_label(cons_nil_nil))
        __(strcx(imm0,0,imm1))
        __(bne- 1b)
        __(isync)
	__(blr)
	
_spentry(newgotag)
        __(li imm1,lisp_globals.go_tag_counter)
1:      __(lrarx(imm0,0,imm1))
	__(addi imm0,imm0,1<<num_subtag_bits)
	__(cmpri(imm0,0))
	__(ori arg_z,imm0,subtag_go_tag)
	__(beq- local_label(cons_nil_nil))
        __(strcx(imm0,0,imm1))
        __(bne- 1b)
        __(isync)
	__(blr)
local_label(cons_nil_nil):
        __(li imm2,RESERVATION_DISCHARGE)
        __(strcx(imm2,0,imm2))
	__(li imm0,nil_value)
	__(Cons(arg_z,imm0,imm0))
	__(blr)

	
/* Allocate a miscobj on the temp stack.  (Push a frame on the tsp and 
   heap-cons the object if there's no room on the tstack.) */
_spentry(stack_misc_alloc)
        __ifdef([PPC64])
         __(rldicr. imm2,arg_y,64-fixnumshift,7)
         __(unbox_fixnum(imm0,arg_z))
         __(clrldi imm2,imm0,64-nlowtagbits)
         __(extract_fulltag(imm1,imm0))
         __(bne cr0,9f)
         __(cmpdi cr2,imm2,lowtag_nodeheader)
         __(cmpdi cr4,imm1,ivector_class_8_bit)
         __(cmpdi cr1,imm1,ivector_class_64_bit)
         __(cmpdi cr3,imm1,ivector_class_32_bit)
         __(cmpdi cr5,imm1,ivector_class_other_bit)
         __(sldi imm1,arg_y,num_subtag_bits-fixnumshift)
         __(mr imm2,arg_y)
         __(beq cr2,3f)
         __(cmpdi cr2,imm0,subtag_bit_vector)
         __(beq cr1,3f)
         __(beq cr3,1f)
         __(beq cr4,2f)
         __(beq cr2,0f)
         /* 2 bytes per element */
         __(srdi imm2,imm2,2)
         __(b 3f)
0:       /* bit-vector case */
         __(addi imm2,imm2,7<<fixnumshift)
         __(srdi imm2,imm2,3+fixnumshift)
         __(b 3f)        
         /* 4 bytes per element */
1:       __(srdi imm2,imm2,1)
         __(b 3f)
2:       /* 1 byte per element */
         __(srdi imm2,imm2,3)
3:       /* 8 bytes per element */
         __(or imm0,imm1,imm0)   /* imm0 = header, imm2 = byte count */
         __(dnode_align(imm3,imm2,tsp_frame.fixed_overhead+node_size))
	 __(cmpldi cr0,imm3,tstack_alloc_limit) /* more than limit ? */
	 __(bgt- cr0,4f)
	 __(TSP_Alloc_Var_Boxed_nz(imm3,imm4))
        /* Slap the header on the vector, then return. */
	 __(str(imm0,tsp_frame.data_offset(tsp)))
	 __(la arg_z,tsp_frame.data_offset+fulltag_misc(tsp))
	__(blr)
        /* Too large to safely fit on tstack.  Heap-cons the vector, but make 
           sure that there's an empty tsp frame to keep the compiler happy. */
	
4:       __(TSP_Alloc_Fixed_Unboxed(0))
	 __(b _SPmisc_alloc)
        __else
	 __(rlwinm. imm2,arg_y,32-fixnumshift,0,(8+fixnumshift)-1)
	 __(unbox_fixnum(imm0,arg_z))
	 __(extract_fulltag(imm1,imm0))
	 __(bne- cr0,9f)
	 __(cmpri(cr0,imm1,fulltag_nodeheader))
	 __(mr imm3,imm0)
	 __(cmplri(cr1,imm0,max_32_bit_ivector_subtag))
	 __(rlwimi imm0,arg_y,num_subtag_bits-fixnum_shift,0,31-num_subtag_bits) /* imm0 now = header */
	 __(mr imm2,arg_y)
	 __(beq cr0,1f)	/* do probe if node object 
			   (fixnum element count = byte count). */
	 __(cmplri(cr0,imm3,max_16_bit_ivector_subtag))
	 __(bng cr1,1f) /* do probe if 32-bit imm object */
	 __(cmplri(cr1,imm3,max_8_bit_ivector_subtag))
	 __(srwi imm2,imm2,1)
	 __(bgt cr0,3f)
	 __(bgt cr1,1f)
	 __(srwi imm2,imm2,1)
/* imm2 now = byte count.  Add 4 for header, 7 to align, then 
	clear low three bits. */
1:
         __(dnode_align(imm3,imm2,tsp_frame.fixed_overhead+node_size))
	 __(cmplri(cr0,imm3,tstack_alloc_limit)) /* more than limit ? */
	 __(bgt- cr0,0f)
	 __(TSP_Alloc_Var_Boxed_nz(imm3,imm4))

/* Slap the header on the vector, then return. */
	 __(str(imm0,tsp_frame.data_offset(tsp)))
	 __(la arg_z,tsp_frame.data_offset+fulltag_misc(tsp))
	 __(blr)
9: 



/* Too large to safely fit on tstack.  Heap-cons the vector, but make 
   sure that there's an empty tsp frame to keep the compiler happy. */
0:
	 __(TSP_Alloc_Fixed_Unboxed(0))
	 __(b _SPmisc_alloc)
3:
	 __(cmplri(imm3,subtag_double_float_vector))
	 __(slwi imm2,arg_y,1)
	 __(beq 1b)
	 __(addi imm2,arg_y,7<<fixnumshift)
	 __(srwi imm2,imm2,fixnumshift+3)
	 __(b 1b)
        __endif
        
/* subtype (boxed, of course) is vpushed, followed by nargs bytes worth of */
/* initial-contents.  Note that this can be used to cons any type of initialized */
/* node-header'ed misc object (symbols, closures, ...) as well as vector-like */
/* objects. */
/* Note that we're guaranteed to win (or force GC, or run out of memory) */
/* because nargs < 32K. */
_spentry(gvector)
	__(ldrx(arg_z,vsp,nargs))
	__(unbox_fixnum(imm0,arg_z))
        __ifdef([PPC64])
         __(sldi imm1,nargs,num_subtag_bits-fixnum_shift)
         __(or imm0,imm0,imm1)
        __else
	 __(rlwimi imm0,nargs,num_subtag_bits-fixnum_shift,0,31-num_subtag_bits)
        __endif
        __(dnode_align(imm1,nargs,node_size))
	__(Misc_Alloc(arg_z,imm0,imm1))
	__(mr imm1,nargs)
	__(la imm2,misc_data_offset(imm1))
	__(b 2f)
1:
	__(strx(temp0,arg_z,imm2))
2:
	__(subi imm1,imm1,node_size)
	__(cmpri(cr0,imm1,0))
	__(subi imm2,imm2,node_size)
	__(vpop(temp0))         /* Note the intentional fencepost:
				      discard the subtype as well. */
	__(bge cr0,1b)
	__(blr)
	
	
/* funcall temp0, returning multiple values if it does. */
_spentry(mvpass)
	__(cmpri(cr0,nargs,node_size*nargregs))
	__(mflr loc_pc)
	__(mr imm0,vsp)
	__(ble+ cr0,1f)
	 __(subi imm0,imm0,node_size*nargregs)
	 __(add imm0,imm0,nargs)
1:
	__(build_lisp_frame(fn,loc_pc,imm0))
	__(ref_global(loc_pc,ret1val_addr))
	__(li fn,0)
	__(mtlr loc_pc)
	__(do_funcall())
	
/* ret1valn returns "1 multiple value" when a called function does not */
/* return multiple values.  Its presence on the stack (as a return address) */
/* identifies the stack frame to code which returns multiple values. */

_exportfn(C(ret1valn))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame())
	__(vpush(arg_z))
	__(set_nargs(1))
	__(blr)
	
_spentry(fitvals)
	__(subf. imm0,nargs,imm0)
	__(li imm1,nil_value)
	__(bge 2f)
	__(sub vsp,vsp,imm0)
	__(blr)
1:
	__(subic. imm0,imm0,node_size)
	__(vpush(imm1))
	__(addi nargs,nargs,node_size)
2:
	__(bne 1b)
	__(blr)


_spentry(nthvalue)
	__(add imm0,vsp,nargs)
	__(ldr(imm1,0(imm0)))
	__(cmplr(imm1,nargs))	/*  do unsigned compare:	 if (n < 0) => nil. */
	__(li arg_z,nil_value)
	__(neg imm1,imm1)
	__(subi imm1,imm1,node_size)
	__(bge 1f)
	__(ldrx(arg_z,imm0,imm1))
1:	
	__(la vsp,node_size(imm0))
	__(blr)
        

	/* Come here to return multiple values when */
	/* the caller's context isn't saved in a lisp_frame. */
	/* lr, fn valid; temp0 = entry vsp */

_spentry(values)
	__(mflr loc_pc)
local_label(return_values):  
	__(ref_global(imm0,ret1val_addr))
	__(li arg_z,nil_value)
	/* max tsp frame is 4K. 8+8 is overhead for save_values_to_tsp below */
	/* and @do_unwind in nthrowvalues in "sp_catch.s". */
	__(cmpri(cr2,nargs,4096-(dnode_size+dnode_size)))
	__(cmpr(cr1,imm0,loc_pc))
	__(cmpri(cr0,nargs,fixnum_one))
	__(bge cr2,2f)
	__(beq+ cr1,3f)
	__(mtlr loc_pc)
	__(add imm0,nargs,vsp)
	__(blt- cr0,1f)
	__(ldr(arg_z,-node_size(imm0)))
1:
	__(mr vsp,temp0)
	__(blr)

2:
	__(uuo_interr(error_too_many_values,nargs))
	__(b 2b)

/* Return multiple values to real caller. */
3:
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(add imm1,nargs,vsp)
	__(ldr(imm0,lisp_frame.savevsp(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(cmpr(cr0,imm1,imm0)) /* a fairly common case */
	__(mtlr loc_pc)
	__(cmpri(cr1,nargs,fixnum_one)) /* sadly, a very common case */
	__(discard_lisp_frame())
	__(beqlr cr0) /* already in the right place */
	__(bne cr1,4f)
	 __(ldr(arg_z,0(vsp)))
	 __(mr vsp,imm0)
	 __(vpush(arg_z))
	 __(blr)
4:
	__(blt cr1,6f)
	__(li imm2,fixnum_one)
5:
	__(cmpr(cr0,imm2,nargs))
	__(addi imm2,imm2,fixnum_one)
	__(ldru(arg_z,-node_size(imm1)))
	__(push(arg_z,imm0))
	__(bne cr0,5b)
6:
	__(mr vsp,imm0)
	__(blr)

	.globl C(nvalret)
	
	/* Come here with saved context on top of stack. */
_spentry(nvalret)
C(nvalret):	
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(temp0,lisp_frame.savevsp(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame())
        __(b local_label(return_values))
        	
/* Provide default (NIL) values for &optional arguments; imm0 is 
   the (fixnum) upper limit on the total of required and &optional 
   arguments.  nargs is preserved, all arguments wind up on the 
   vstack. */
_spentry(default_optional_args)
	__(cmplr( cr7,nargs,imm0))
	__(li imm5,nil_value)
	__(vpush_argregs())
	__(mr imm1,nargs)
	__(bgelr cr7)
1:	
	__(addi imm1,imm1,fixnum_one)
	__(cmpr(cr0,imm1,imm0))
	__(vpush(imm5))
	__(bne cr0,1b)
	__(blr)
	
/* Indicate whether &optional arguments were actually supplied.  nargs 
   contains the actual arg count (minus the number of required args); 
   imm0 contains the number of &optional args in the lambda list. 
   Note that nargs may be > imm0 if &rest/&key is involved. */
_spentry(opt_supplied_p)
	__(li imm1,0)
1:
	/* (vpush (< imm1 nargs)) */
        __ifdef([PPC64])
	 __(xor imm2,imm1,nargs)
	 __(sradi imm2,imm2,63)
	 __(or imm2,imm2,imm1)
	 __(addi imm1,imm1,fixnumone)
	 __(cmpr(cr0,imm1,imm0))
	 __(subf imm2,nargs,imm2)
	 __(srdi imm2,imm2,63)
         __(mulli imm2,imm2,t_offset)
	 __(addi imm2,imm2,nil_value)
	 __(vpush(imm2))
	 __(bne cr0,1b)
	 __(blr)
        __else
	 __(xor imm2,imm1,nargs)
	 __(srawi imm2,imm2,31)
	 __(or imm2,imm2,imm1)
	 __(addi imm1,imm1,fixnumone)
	 __(cmpr(cr0,imm1,imm0))
	 __(subf imm2,nargs,imm2)
	 __(srwi imm2,imm2,31)
	 __(insrwi imm2,imm2,1,27)
	 __(addi imm2,imm2,nil_value)
	 __(vpush(imm2))
	 __(bne cr0,1b)
	 __(blr)
        __endif
	


/* If nargs is <= imm0, vpush a nil.  Otherwise, cons a list of length 
   (- nargs imm0) and vpush it. 
   Use this entry point to heap-cons a simple &rest arg. */
_spentry(heap_rest_arg)
	__(li imm0,0)
	__(vpush_argregs())
 	__(sub imm1,nargs,imm0)
	__(cmpri(imm1,0))
	__(li arg_z,nil_value)
	__(b 2f)
1:
	__(ldr(temp0,0(vsp)))
	__(cmpri(imm1,fixnum_one))
	__(la vsp,node_size(vsp))
	__(Cons(arg_z,temp0,arg_z))
	__(subi imm1,imm1,fixnum_one)
2:
	__(bgt 1b)
	__(vpush(arg_z))
	__(blr)

	
/* And this entry point when the argument registers haven't yet been 
   vpushed (as is typically the case when required/&rest but no &optional/&key.) */
_spentry(req_heap_rest_arg)
	__(vpush_argregs())
 	__(sub imm1,nargs,imm0)
	__(cmpri(imm1,0))
	__(li arg_z,nil_value)
	__(b 2f)
1:
	__(ldr(temp0,0(vsp)))
	__(cmpri(imm1,fixnum_one))
	__(la vsp,node_size(vsp))
	__(Cons(arg_z,temp0,arg_z))
	__(subi imm1,imm1,fixnum_one)
2:
	__(bgt 1b)
	__(vpush(arg_z))
	__(blr)


_spentry(heap_cons_rest_arg)
 	__(sub imm1,nargs,imm0)
	__(cmpri(imm1,0))
	__(li arg_z,nil_value)
	__(b 2f)
1:
	__(ldr(temp0,0(vsp)))
	__(cmpri(imm1,fixnum_one))
	__(la vsp,node_size(vsp))
	__(Cons(arg_z,temp0,arg_z))
	__(subi imm1,imm1,fixnum_one)
2:
	__(bgt 1b)
	__(vpush(arg_z))
	__(blr)

	
_spentry(simple_keywords)
	__(li imm0,0)
        __(vpush_argregs())
        __(b _SPkeyword_bind)
                
_spentry(keyword_args)
	__(vpush_argregs())
        __(b _SPkeyword_bind)

/* Treat the last (- nargs imm0) values on the vstack as keyword/value 
   pairs.  There'll be imm3 keyword arguments.  Imm2 contains flags 
   that indicate whether &allow-other-keys was specified and whether 
   or not to leave the keyword/value pairs on the vstack for an &rest 
   argument.  Temp3 contains a vector of keyword specifiers which we 
   must (in general) match. 
   If the number of arguments is greater than imm0, the difference must 
   be even. 
   Note that the caller hasn't yet saved its caller's context and that 
   the temp registers used to pass closure_data (temp0) and next_method_context 
   (temp1) may still have "live" values in them, as does nfn (temp2). */

define([keyword_flags],[imm2])
define([keyword_vector],[temp3])
define([keyword_count],[imm3])



define([varptr],[save0])
define([valptr],[save1])
define([limit],[save2])

_spentry(keyword_bind)
	/* Before we can really do anything, we have to */
	/* save the caller's context.  To do so, we need to know */
	/* how many args have actually been pushed.  Ordinarily, that'd */
	/* be "nargs", but we may have pushed more args than we received */
	/* if we had to default any &optionals. */
	/* So, the number of args pushed so far is the larger of nargs */
	/* and the (canonical) total of required/&optional args received. */
	__(cmpr(cr0,nargs,imm0))
	__(add arg_z,vsp,nargs)
	__(bge+ cr0,1f)
	__(add arg_z,vsp,imm0)
1:
	__(build_lisp_frame(fn,loc_pc,arg_z))
	__(mr fn,nfn)
	/* If there are key/value pairs to consider, we slide them down */
	/* the vstack to make room for the value/supplied-p pairs. */
	/* The first step in that operation involves pushing imm3 pairs */
	/* of NILs. */
	/* If there aren't any such pairs, the first step is the last */
	/* step. */
	__(cmpri(cr0,imm3,0))
	__(li arg_z,0)
	__(sub imm1,nargs,imm0)
	__(mr imm4,vsp)	/* in case odd keywords error */
	__(cmpri(cr1,imm1,0))
	__(b 3f)
2:
	__(addi arg_z,arg_z,fixnum_one)
	__(cmplr(cr0,arg_z,imm3))
	__(li imm5,nil_value)
	__(vpush(imm5))
	__(vpush(imm5))
3:
	__(bne cr0,2b)
	__(andi. arg_z,imm1,fixnum_one)
	__(blelr cr1)	/* no keyword/value pairs to consider. */
	__(bne cr0,odd_keywords)
	/* We have key/value pairs.  Move them to the top of the vstack, */
	/* then set the value/supplied-p vars to NIL. */
	/* Have to use some save regs to do this. */
	__(vpush(limit))
	__(vpush(valptr))
	__(vpush(varptr))
	/* recompute ptr to user args in case stack overflowed */
	__(add imm4,vsp,imm3)
	__(add imm4,imm4,imm3)
	__(addi imm4,imm4,3*node_size)
	/* error if odd number of keyword/value args */
	__(mr varptr,imm4)
	__(la limit,3*node_size(vsp))
	__(mr valptr,limit)
	__(mr arg_z,imm1)
4:
	__(li imm4,nil_value)
	__(subi arg_z,arg_z,2<<fixnumshift)
	__(cmplri(cr0,arg_z,0))
	__(ldr(arg_x,node_size*0(varptr)))
	__(ldr(arg_y,node_size*1(varptr)))
	__(str(imm4,node_size*0(varptr)))
	__(str(imm4,node_size*1(varptr)))
	__(la varptr,node_size*2(varptr))
	__(str(arg_x,node_size*0(valptr)))
	__(str(arg_y,node_size*1(valptr)))
	__(la valptr,node_size*2(valptr))
	__(bne cr0,4b)


/* Now, iterate through each supplied keyword/value pair.  If 
   it's :allow-other-keys and the corresponding value is non-nil, 
   note that other keys will be allowed. 
   Find its position in the function's keywords vector.  If that's 
   nil, note that an unknown keyword was encountered. 
   Otherwise, if the keyword arg hasn't already had a value supplied, 
   supply it. 
   When done, complain if any unknown keywords were found and that 
   situation was unexpected. */
	__(mr imm4,valptr)
5:
        __(cmpri(cr0,keyword_flags,16<<fixnumshift)) /* seen :a-o-k yet ? */
	__(ldru(arg_z,-node_size(valptr)))
	__(ldru(arg_y,-node_size(valptr)))
	__(cmpri(cr1,arg_y,nil_value))
	__(li arg_x,nrs.kallowotherkeys)
        /* cr6_eq <- (eq current-keyword :allow-other-keys) */
	__(cmpr(cr6,arg_x,arg_z))
	__(cmpr(cr7,valptr,limit))
	__(bne cr6,6f)
        __(bge cr0,6f) /* Already seen :allow-other-keys */
        __(ori keyword_flags,keyword_flags,16<<fixnumshift)
	__(beq cr1,6f)
	__(ori keyword_flags,keyword_flags,fixnum_one)
6:
	__(cmpri(cr1,imm3,0))
	__(li imm1,misc_data_offset)
	__(li imm0,0)
	__(b 8f)
7:
	__(addi imm0,imm0,fixnum_one)
	__(cmpr(cr1,imm0,imm3))
	__(ldrx(arg_x,keyword_vector,imm1))
	__(cmpr(cr0,arg_x,arg_z))
	__(addi imm1,imm1,fixnum_one)
	__(bne cr0,8f)
	__(add imm0,imm0,imm0)
	__(sub imm0,varptr,imm0)
	__(ldr(arg_x,0(imm0)))
	__(cmpri(cr0,arg_x,nil_value))
	__(li arg_z,t_value)
	__(bne cr0,9f)
	__(str(arg_y,node_size(imm0)))
	__(str(arg_z,0(imm0)))
	__(b 9f)
8:
	__(bne cr1,7b)
	/* Unknown keyword. If it was :allow-other-keys, cr6_eq will still
           be set. */
        __(beq cr6,9f)
	__(ori keyword_flags,keyword_flags,2<<fixnumshift)
9:
	__(bne cr7,5b)
	__(vpop(varptr))
	__(vpop(valptr))
	__(vpop(limit))
	/* All keyword/value pairs have been processed. */
	/* If we saw an unknown keyword and didn't expect to, error. */
	/* Unless bit 2 is set in the fixnum in keyword_flags, discard the */
	/* keyword/value pairs from the vstack. */
	__(andi. imm0,keyword_flags,(fixnum_one)|(2<<fixnumshift))
	__(cmpri(cr0,imm0,2<<fixnumshift))
	__(beq- cr0,badkeys)
	__(andi. imm2,keyword_flags,4<<fixnumshift)
	__(bnelr cr0)
	__(mr vsp,imm4)
	__(blr)

/* Signal an error.  We saved context on entry, so this thing doesn't 
   have to. 
   The "unknown keywords" error could be continuable (ignore them.) 
   It might be hard to then cons an &rest arg. 
   In the general case, it's hard to recover the set of args that were 
   actually supplied to us ... */
/* For now, just cons a list out of the keyword/value pairs
   that were actually provided, and signal an "invalid keywords"
   error with that list as an operand. */
odd_keywords:
	__(mr vsp,imm4)
	__(mr nargs,imm1)
	__(b 1f)
badkeys:
	__(sub nargs,imm4,vsp)
1:
	__(bl _SPconslist)
	__(li arg_y,XBADKEYS)
	__(set_nargs(2))
	__(b _SPksignalerr)

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
        
_spentry(ffcall)
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
	__(b FF_call_return_common)

_spentry(ffcalladdress)
        __(b _SPbreakpoint)
	
/* Signal an error synchronously, via %ERR-DISP. */
/* If %ERR-DISP isn't fbound, it'd be nice to print a message */
/* on the C runtime stderr. */

_spentry(ksignalerr)
	__(li fname,nrs.errdisp)
	__(jump_fname)
        
/* As in the heap-consed cases, only stack-cons the &rest arg */
_spentry(stack_rest_arg)
	__(li imm0,0)
	__(vpush_argregs())
        __(b _SPstack_cons_rest_arg)

	
_spentry(req_stack_rest_arg)
	__(vpush_argregs())
        __(b _SPstack_cons_rest_arg)
	
_spentry(stack_cons_rest_arg)
	__(sub imm1,nargs,imm0)
	__(cmpri(cr0,imm1,0))
	__(cmpri(cr1,imm1,(4096-dnode_size)/2))
	__(li arg_z,nil_value)
	__(ble cr0,2f)		/* always temp-push something. */
	__(bge cr1,3f)
	__(add imm1,imm1,imm1)
	__(dnode_align(imm2,imm1,tsp_frame.fixed_overhead))
	__(TSP_Alloc_Var_Boxed(imm2,imm3))
	__(la imm0,tsp_frame.data_offset+fulltag_cons(tsp))
1:
	__(cmpri(cr0,imm1,cons.size))	/* last time through ? */
	__(subi imm1,imm1,cons.size)
	__(vpop(arg_x))
	__(_rplacd(imm0,arg_z))
	__(_rplaca(imm0,arg_x))
	__(mr arg_z,imm0)
	__(la imm0,cons.size(imm0))
	__(bne cr0,1b)
	__(vpush(arg_z))
	__(blr)
2:
	__(TSP_Alloc_Fixed_Unboxed(0))
	__(vpush(arg_z))
	__(blr)
3:
	__(TSP_Alloc_Fixed_Unboxed(0))
	__(b _SPheap_cons_rest_arg)


_spentry(callbackX)        
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
	__(li imm0,TCR_STATE_FOREIGN)
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
	
/* Prepend all but the first two (closure code, fn) and last two */
/* (function name, lfbits) elements of nfn to the "arglist". */
/* Doing things this way (the same way that 68K MCL does) lets */
/* functions which take "inherited arguments" work consistently */
/* even in cases where no closure object is created. */
_spentry(call_closure)        
	__(cmpri(cr0,nargs,nargregs<<fixnumshift))
	__(cmpri(cr1,nargs,fixnum_one))
	__(vector_length(imm0,nfn,imm0))
	__(subi imm0,imm0,4<<fixnumshift) /* imm0 = inherited arg count */
	__(li imm1,misc_data_offset+(2<<fixnumshift)) /* point to 1st arg */
	__(li imm4,nil_value)
	__(ble+ cr0,local_label(no_insert))
	/* Some arguments have already been vpushed.  Vpush imm0's worth */
	/* of NILs, copy those arguments that have already been vpushed from */
	/* the old TOS to the new, then insert all of the inerited args */
	/* and go to the function. */
	__(li imm2,0)
local_label(push_nil_loop):
	__(addi imm2,imm2,fixnum_one)
	__(cmpr(cr2,imm2,imm0))
	__(vpush(imm4))
	__(bne cr2,local_label(push_nil_loop))

	__(mr imm3,vsp)
	__(add imm4,vsp,imm0)
	__(subi imm2,nargs,nargregs<<fixnumshift)
local_label(copy_already_loop):
	__(cmpri(cr2,imm2,fixnum_one))
	__(subi imm2,imm2,fixnum_one)
	__(ldr(fname,0(imm4)))
	__(addi imm4,imm4,fixnum_one)
	__(str(fname,0(imm3)))
	__(addi imm3,imm3,fixnum_one)
	__(bne cr2,local_label(copy_already_loop))

local_label(insert_loop):
	__(cmpri(cr2,imm0,fixnum_one))
	__(ldrx(fname,nfn,imm1))
	__(addi imm1,imm1,fixnum_one)
	__(addi nargs,nargs,fixnum_one)
	__(subi imm0,imm0,fixnum_one)
	__(push(fname,imm4))
	__(bne cr2,local_label(insert_loop))
	__(b local_label(go))
local_label(no_insert):
	/* nargregs or fewer args were already vpushed. */
	/* if exactly nargregs, vpush remaining inherited vars. */
	__(add imm2,imm1,imm0)
	__(bne cr0,local_label(set_regs))
local_label(vpush_remaining):
	__(cmpri(cr2,imm0,fixnum_one))
	__(ldrx(fname,nfn,imm1))
	__(addi imm1,imm1,fixnum_one)
	__(vpush(fname))
	__(subi imm0,imm0,fixnum_one)
	__(addi nargs,nargs,fixnum_one)
	__(bne cr2,local_label(vpush_remaining))
	__(b local_label(go))
local_label(set_regs):
	/* if nargs was > 1 (and we know that it was < 3), it must have */
	/* been 2.  Set arg_x, then vpush the remaining args. */
	__(ble cr1,local_label(set_y_z))
local_label(set_arg_x):
	__(subi imm0,imm0,fixnum_one)
	__(cmpri(cr0,imm0,0))
	__(subi imm2,imm2,fixnum_one)
	__(ldrx(arg_x,nfn,imm2))
	__(addi nargs,nargs,fixnum_one)
	__(bne cr0,local_label(vpush_remaining))
	__(b local_label(go))
	/* Maybe set arg_y or arg_z, preceding args */
local_label(set_y_z):
	__(bne cr1,local_label(set_arg_z))
	/* Set arg_y, maybe arg_x, preceding args */
local_label(set_arg_y):
	__(subi imm0,imm0,fixnum_one)
	__(cmpri(cr0,imm0,0))
	__(subi imm2,imm2,fixnum_one)
	__(ldrx(arg_y,nfn,imm2))
	__(addi nargs,nargs,fixnum_one)
	__(bne cr0,local_label(set_arg_x))
	__(b local_label(go))
local_label(set_arg_z):
	__(subi imm0,imm0,fixnum_one)
	__(cmpri(cr0,imm0,0))
	__(subi imm2,imm2,fixnum_one)
	__(ldrx(arg_z,nfn,imm2))
	__(addi nargs,nargs,fixnum_one)
	__(bne cr0,local_label(set_arg_y))

local_label(go):
	__(vrefr(nfn,nfn,1))
	__(ldr(loc_pc,_function.codevector(nfn)))
	__(mtctr loc_pc)
	__(bctr)
        
/* This  treats anything that's either
   #+ppc32 (signed-byte 32), (unsigned-byte 32)
   #+ppc64 (signed-byte 64), (unsigned-byte 64)
   as if it denoted a "natural-sized" value. 
   Argument in arg_z, result in imm0.  May use temp0. */
_spentry(getxlong)
        __ifdef([PPC64])
        __else
        __(extract_typecode(imm0,arg_z))
	__(cmpri(cr0,imm0,tag_fixnum))
	__(cmpri(cr1,imm0,subtag_bignum))
	__(unbox_fixnum(imm0,arg_z))
	__(beqlr cr0)
	__(mr temp0,arg_z)
	__(bne- cr1,local_label(error))
	__(getvheader(imm0,temp0))
	__(cmpri(cr1,imm0,one_digit_bignum_header))
	__(cmpri(cr7,imm0,two_digit_bignum_header))
	__(beq cr1,local_label(big1))
        __(beq cr7,local_label(big2))
local_label(error):
	__(uuo_interr(error_object_not_integer,arg_z)) /* not quite right but what 68K MCL said */



local_label(big2):
	__(vrefr(imm0,temp0,1)) /* sign digit must be 0 */
	__(cmpri(imm0,0))
	__(bne local_label(error))
local_label(big1):
	__(vrefr(imm0,temp0,0))
	__(blr)


        __endif
                
/* Everything up to the last arg has been vpushed, nargs is set to 
   the (boxed) count of things already pushed. 
   On exit, arg_x, arg_y, arg_z, and nargs are set as per a normal 
   function call (this may require vpopping a few things.) 
   ppc2-invoke-fn assumes that temp1 is preserved here. */
_spentry(spreadargz)
        __ifdef([PPC64])
	 __(extract_fulltag(imm1,arg_z))
	 __(cmpri(cr1,imm1,fulltag_cons))
        __else
	 __(extract_lisptag(imm1,arg_z))
	 __(cmpri(cr1,imm1,tag_list))
        __endif
	__(cmpri(cr0,arg_z,nil_value))
	__(li imm0,0)
	__(mr arg_y,arg_z)		/*  save in case of error */
	__(beq cr0,2f)
1:
	__(bne- cr1,3f)
	__(_car(arg_x,arg_z))
	__(_cdr(arg_z,arg_z))
	__(cmpri(cr0,arg_z,nil_value))
        __ifdef([PPC64])
	 __(extract_fulltag(imm1,arg_z))
	 __(cmpri(cr1,imm1,fulltag_cons))
        __else
	 __(extract_lisptag(imm1,arg_z))
	 __(cmpri(cr1,imm1,tag_list))
        __endif
	__(vpush(arg_x))
	__(addi imm0,imm0,fixnum_one)
	__(bne cr0,1b)
2:
	__(add. nargs,nargs,imm0)
	__(cmpri(cr2,nargs,2<<fixnumshift))
	__(beqlr- cr0)
	__(vpop(arg_z))
	__(bltlr cr2)
	__(vpop(arg_y))
	__(beqlr cr2)
	__(vpop(arg_x))
	__(blr)
/*  Discard whatever's been vpushed already, complain. */
3:	
	__(add vsp,vsp,imm0)
	__(mr arg_z,arg_y)		/* recover original arg_z */
	__(li arg_y,XNOSPREAD)
	__(set_nargs(2))
	__(b _SPksignalerr)
        
/* Tail-recursively funcall temp0. */
	/* Pretty much the same as the tcallsym* cases above. */
_spentry(tfuncallgen)
	__(cmpri(cr0,nargs,nargregs<<fixnumshift))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(mtlr loc_pc)
	__(ble cr0,2f)
	__(ldr(imm0,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	/* can use nfn (= temp2) as a temporary */
	__(subi imm1,nargs,nargregs<<fixnumshift)
	__(add imm1,imm1,vsp)
1:
	__(ldru(temp2,-node_size(imm1)))
	__(cmpr(cr0,imm1,vsp))
	__(push(temp2,imm0))
	__(bne cr0,1b)
	__(mr vsp,imm0)
	__(do_funcall())
2:
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	__(do_funcall())


	/* Some args were vpushed.  Slide them down to the base of */
	/* the current frame, then do funcall. */
_spentry(tfuncallslide)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(imm0,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	/* can use nfn (= temp2) as a temporary */
	__(subi imm1,nargs,nargregs<<fixnumshift)
	__(add imm1,imm1,vsp)
	__(mtlr loc_pc)
1:
	__(ldru(temp2,-node_size(imm1)))
	__(cmpr(cr0,imm1,vsp))
	__(push(temp2,imm0))
	__(bne cr0,1b)
	__(mr vsp,imm0)
	__(do_funcall())

	/* No args were vpushed; recover saved context & do funcall */
_spentry(tfuncallvsp)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(mtlr loc_pc)
	__(discard_lisp_frame())
	__(do_funcall())
        
/* Tail-recursively call the (known symbol) in fname. */
/* In the general case, we don't know if any args were */
/* vpushed or not.  If so, we have to "slide" them down */
/* to the base of the frame.  If not, we can just restore */
/* vsp, lr, fn from the saved lisp frame on the control stack. */
_spentry(tcallsymgen)
	__(cmpri(cr0,nargs,nargregs<<fixnumshift))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(mtlr loc_pc)
	__(ble cr0,2f)

	__(ldr(imm0,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	/* can use nfn (= temp2) as a temporary */
	__(subi imm1,nargs,nargregs<<fixnumshift)
	__(add imm1,imm1,vsp)
1:
	__(ldru(temp2,-node_size(imm1)))
	__(cmpr(cr0,imm1,vsp))
	__(push(temp2,imm0))
	__(bne cr0,1b)
	__(mr vsp,imm0)
	__(jump_fname)
	
2:		
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	__(jump_fname)
	
	
/* Some args were vpushed.  Slide them down to the base of */
/* the current frame, then do funcall. */
_spentry(tcallsymslide)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(imm0,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	__(mtlr loc_pc)
	/* can use nfn (= temp2) as a temporary */
	__(subi imm1,nargs,nargregs<<fixnumshift)
	__(add imm1,imm1,vsp)
1:
	__(ldru(temp2,-node_size(imm1)))
	__(cmpr(cr0,imm1,vsp))
	__(push(temp2,imm0))
	__(bne cr0,1b)
	__(mr vsp,imm0)
	__(jump_fname)

/* No args were vpushed; recover saved context & call symbol */
_spentry(tcallsymvsp)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	__(mtlr loc_pc)
	__(jump_fname)
	
/* Tail-recursively call the function in nfn. */
	/* Pretty much the same as the tcallsym* cases above. */
_spentry(tcallnfngen)
	__(cmpri(cr0,nargs,nargregs<<fixnumshift))
	__(ble cr0,_SPtcallnfnvsp)
        __(b _SPtcallnfnslide)

/* Some args were vpushed.  Slide them down to the base of */
/* the current frame, then do funcall. */
_spentry(tcallnfnslide)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(imm0,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	__(mtlr loc_pc)
	/* Since we have a known function, can use fname as a temporary. */
	__(subi imm1,nargs,nargregs<<fixnumshift)
	__(add imm1,imm1,vsp)
1:
	__(ldru(fname,-node_size(imm1)))
	__(cmpr(cr0,imm1,vsp))
	__(push(fname,imm0))
	__(bne cr0,1b)
	__(mr vsp,imm0)
       	__(jump_nfn())
        
_spentry(tcallnfnvsp)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	__(mtlr loc_pc)
       	__(jump_nfn())
	
_spentry(misc_ref)
	__(trap_unless_fulltag_equal(arg_y,fulltag_misc,imm0))
	__(trap_unless_lisptag_equal(arg_z,tag_fixnum,imm0))
	__(vector_length(imm0,arg_y,imm1))
	__(trlge(arg_z,imm0))
	__(extract_lowbyte(imm1,imm1))	/* imm1 = subtag */
	
/* Reference index arg_z of a misc-tagged object (arg_y). 
   Note that this conses in some cases.  Return a properly-tagged 
   lisp object in arg_z.  Do type and bounds-checking. 
*/
local_label(misc_ref_common):   
        __ifdef([PPC64])
	 __(extract_fulltag(imm2,imm1))
         __(extract_lowtag(imm3,imm1))
         __(cmpdi cr1,imm2,ivector_class_64_bit)
         __(cmpdi cr0,imm3,lowtag_nodeheader)
         __(cmpdi cr2,imm2,ivector_class_8_bit)
         __(cmpdi cr3,imm2,ivector_class_32_bit)
         __(la imm0,misc_data_offset(arg_z))
         __(bne cr0,local_label(ref_imm))
         /* A node vector */
         __(ldx arg_z,arg_y,imm0)
         __(blr)
local_label(ref_imm):
         __(cmpdi cr0,imm1,subtag_double_float_vector)
         __(bne cr1,local_label(ref_not_64))
         __(cmpdi cr1,imm1,subtag_s64_vector)
         __(ldx imm0,arg_y,imm0)
         __(beq cr0,local_label(ref_dfloat))
         __(beq cr1,_SPmakes64)
         __(b _SPmakeu64)
local_label(ref_dfloat):        
         __(li imm1,double_float_header)
	 __(Misc_Alloc_Fixed(arg_z,imm1,double_float.size))
         __(std imm0,double_float.value(arg_z))
         __(blr)
local_label(ref_not_64):
         __(cmpdi cr0,imm1,subtag_s32_vector)
         __(bne cr3,local_label(ref_not_32))
         __(srdi imm0,arg_z,1)
         __(cmpdi cr3,imm1,subtag_single_float_vector)
         __(la imm0,misc_data_offset(imm0))
         __(lwzx imm0,arg_y,imm0)
         __(beq cr3,2f)
         __(bne cr0,1f)
         __(extsw imm0,imm0)
1:       __(box_fixnum(arg_z,imm0))
         __(blr)
2:       __(rldicr arg_z,imm0,32,31)
         __(ori arg_z,arg_z,subtag_single_float)
         __(blr)
local_label(ref_not_32):
         __(cmpdi cr0,imm1,subtag_simple_base_string)
         __(bne cr2,local_label(ref_not_8))
         __(srdi imm0,arg_z,3)
         __(cmpdi cr2,imm1,subtag_s8_vector)
         __(la imm0,misc_data_offset(imm0))
         __(lbzx imm0,arg_y,imm0)
         __(beq cr0,2f)
         __(bne cr2,1f)
         __(extsb imm0,imm0)
1:       __(box_fixnum(arg_z,imm0))
         __(blr)
2:       __(sldi imm0,imm0,charcode_shift)
         __(ori arg_z,imm0,subtag_character)
         __(blr)
/* either s16, u16, or bit vector */
local_label(ref_not_8):
         __(cmpdi cr0,imm1,subtag_bit_vector)
         __(cmpdi cr1,imm1,subtag_s16_vector)
         __(srdi imm0,arg_z,2)
         __(la imm0,misc_data_offset(imm0))
         __(beq cr0,2f)
         __(bne cr1,1f)
         __(lhzx imm0,arg_y,imm0)
         __(box_fixnum(arg_z,imm0))
         __(blr)
1:       __(lhax imm0,arg_y,imm0)
         __(box_fixnum(arg_z,imm0))
         __(blr)
2:                      
	 __(extrwi imm1,arg_z,5,32-(fixnumshift+5))	/* imm1 = bitnum */
	 __(la imm1,1+fixnumshift(imm1))
         __(srdi imm0,arg_z,5+fixnumshift)
         __(sldi imm0,imm0,2)
	 __(la imm0,misc_data_offset(imm0))
	 __(ldrx(imm0,arg_y,imm0))
	 __(rlwnm arg_z,imm0,imm1,31-fixnumshift,31-fixnumshift)
	 __(blr)
        __else
	 __(extract_fulltag(imm2,imm1))
	 __(cmpri(cr0,imm2,fulltag_nodeheader))
	 __(cmpri(cr1,imm1,max_32_bit_ivector_subtag))
	 __(cmpri(cr2,imm1,max_8_bit_ivector_subtag))
	 __(addi imm0,arg_z,misc_data_offset)
	 __(bne cr0,local_label(ref_imm))
	 /* A node vector. */
	 __(ldrx(arg_z,arg_y,imm0))
	 __(blr)
local_label(ref_imm):
	 __(bgt cr1,local_label(ref_not32))
	 __(cmpri(cr1,imm1,subtag_single_float_vector))
	 __(cmpri(cr0,imm1,subtag_s32_vector))
	 __(ldrx(imm0,arg_y,imm0))
	 __(beq cr1,local_label(ref_sfloat))
	 __(beq cr0,local_label(ref_signed))
local_label(ref_unsigned):
	 __(cmpri(cr1,imm0,0))
	 __(srawi. imm1,imm0,31-nfixnumtagbits)
	 __(box_fixnum(arg_z,imm0))
	 __(beqlr+ cr0)
	 __(li imm1,one_digit_bignum_header)
	 __(blt cr1,local_label(two_digit))
	 __(Misc_Alloc_Fixed(arg_z,imm1,8))
	 __(str(imm0,misc_data_offset(arg_z)))
	 __(blr)
local_label(two_digit):
	 __(li imm1,two_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm1,16))
	 __(str(imm0,misc_data_offset(arg_z)))
	 __(blr)	
local_label(ref_signed):
	 __(addo imm1,imm0,imm0)
	 __(addo. arg_z,imm1,imm1)
	 __(bnslr)
	 __(mtxer rzero)
	 __(li imm1,one_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm1,8))
	 __(str(imm0,misc_data_offset(arg_z)))
	 __(blr)
local_label(ref_sfloat):
	 __(li imm1,single_float_header)
	 __(Misc_Alloc_Fixed(arg_z,imm1,single_float.size))
	 __(str(imm0,single_float.value(arg_z)))
	 __(blr)
local_label(ref_not32):	
	 __(cmpri(cr1,imm1,max_16_bit_ivector_subtag))
	 __(bgt cr2,local_label(ref_not8))
	 /* 8-bit objects are either u8, s8, or base_strings. */
	 /* cr2_eq is set if base_string (= max_8_bit_ivector_subtag) */
	 __(cmpri(cr1,imm1,subtag_s8_vector))
	 __(srwi imm0,arg_z,2)
	 __(la imm0,misc_data_offset(imm0))
	 __(lbzx imm0,arg_y,imm0)
	 __(beq cr2,local_label(ref_char))
	 __(bne cr1,local_label(ref_box))
	 __(extsb imm0,imm0)
local_label(ref_box):	
	 __(box_fixnum(arg_z,imm0))
	 __(blr)
local_label(ref_char):	
	 __(slwi arg_z,imm0,charcode_shift)
	 __(ori arg_z,arg_z,subtag_character)
	 __(blr)
local_label(ref_not8):
	 __(cmpri(cr2,imm1,subtag_bit_vector))
	 __(bgt cr1,local_label(ref_not16))
	 /* 16-bit objects are either u16, s16, or general_strings. */
	 /* cr1_eq is set if s16_vector (= max_16_bit_ivector_subtag) */
	 __(cmpri(cr0,imm1,subtag_simple_general_string))
	 __(srwi imm0,arg_z,1)
	 __(la imm0,misc_data_offset(imm0))
	 __(lhzx imm0,arg_y,imm0)
	 __(beq cr0,local_label(ref_char))
	 __(bne cr1,local_label(ref_box))
	 __(extsh imm0,imm0)
	 __(b local_label(ref_box))
local_label(ref_not16):
	 __(bne cr2,local_label(ref_dfloat))
	 __(extrwi imm1,arg_z,5,32-(fixnumshift+5))	/* imm1 = bitnum */
	 __(la imm1,1+fixnumshift(imm1))
	 __(rlwinm imm0,arg_z,32-5,5,31-fixnumshift)
	 __(la imm0,misc_data_offset(imm0))
	 __(ldrx(imm0,arg_y,imm0))
	 __(rlwnm arg_z,imm0,imm1,31-fixnumshift,31-fixnumshift)
	 __(blr)
local_label(ref_dfloat):
	 __(slwi imm0,arg_z,1)
	 __(la imm0,misc_dfloat_offset(imm0))
	 __(la imm1,4(imm0))
	 __(ldrx(imm0,arg_y,imm0))
	 __(ldrx(imm1,arg_y,imm1))
	 __(li imm2,double_float_header)
	 __(Misc_Alloc_Fixed(arg_z,imm2,double_float.size))
	 __(str(imm0,double_float.value(arg_z)))
	 __(str(imm1,double_float.value+4(arg_z)))
	 __(blr)
        __endif
        
/* like misc_ref, only the boxed subtag is in arg_x. 
*/
_spentry(subtag_misc_ref)
	__(trap_unless_fulltag_equal(arg_y,fulltag_misc,imm0))
        __(trap_unless_lisptag_equal(arg_z,tag_fixnum,imm0))
	__(vector_length(imm0,arg_y,imm1))
	__(trlge(arg_z,imm0))
	__(unbox_fixnum(imm1,arg_x))
        __(b local_label(misc_ref_common))

_spentry(builtin_aref1)
	__(extract_typecode(imm0,arg_y))
	__(cmpri(cr0,imm0,min_vector_subtag))
	__(box_fixnum(arg_x,imm0))
	__(bgt cr0,_SPsubtag_misc_ref)
	__(jump_builtin(_builtin_aref1,2))
        	
	
/* Make a cons cell on the vstack.  Always push 3 words, 'cause we're  
   not sure how the vstack will be aligned. */
_spentry(stkconsyz)
	__(li imm0,nil_value)
	__(vpush(imm0))
	__(vpush(imm0))
	__(vpush(imm0))
	__(andi. imm0,vsp,1<<word_shift) /* (oddp vsp ?) */
	__(beq cr0,1f)
	__(str(arg_y,node_size*2(vsp))) /* car */
	__(str(arg_z,node_size(vsp))) /* cdr */
	__(la arg_z,fulltag_cons+node_size(vsp))
	__(blr)
1:
	__(str(arg_y,node_size(vsp))) /* car, again */
	__(str(arg_z,0(vsp)))
	__(la arg_z,fulltag_cons(vsp))
	__(blr)

/* Make a stack-consed value cell.  Much like the case of
   stack-allocating a cons cell.  Imm0 points to the closed-over value
   (already vpushed).  Replace that locative with the vcell. */
_spentry(stkvcell0)
	__(sub imm1,imm0,vsp) /* imm1 = delta from vsp to value cell loc */
	__(li arg_z,nil_value)
	__(vpush(arg_z))
	__(vpush(arg_z))
	__(vpush(arg_z))
	__(addi imm1,imm1,node_size*3)
	__(add imm0,vsp,imm1) /* in case stack overflowed */
	__(andi. imm1,vsp,1<<word_shift) /* (oddp vsp) ? */
	__(li imm1,value_cell_header)
	__(ldr(arg_z,0(imm0)))
	__(beq cr0,1f)
	__(str(arg_z,node_size*2(vsp)))
	__(str(imm1,node_size(vsp)))
	__(la arg_z,fulltag_misc+node_size(vsp))
	__(str(arg_z,0(imm0)))
	__(blr)
1:
	__(str(arg_z,node_size(vsp)))
	__(str(imm1,0(vsp)))
	__(la arg_z,fulltag_misc(vsp))
	__(str(arg_z,0(imm0)))
	__(blr)

        
_spentry(stkvcellvsp)      
	__(li arg_z,nil_value)
	__(vpush(arg_z))
	__(vpush(arg_z))
	__(vpush(arg_z))
	__(li imm1,node_size*3)
	__(add imm0,vsp,imm1) /* in case stack overflowed */
	__(andi. imm1,vsp,1<<word_shift) /* (oddp vsp) ? */
	__(li imm1,value_cell_header)
	__(ldr(arg_z,0(imm0)))
	__(beq cr0,1f)
	__(str(arg_z,node_size*2(vsp)))
	__(str(imm1,node_size(vsp)))
	__(la arg_z,fulltag_misc+node_size(vsp))
	__(str(arg_z,0(imm0)))
	__(blr)
1:
	__(str(arg_z,node_size(vsp)))
	__(str(imm1,0(vsp)))
	__(la arg_z,fulltag_misc(vsp))
	__(str(arg_z,0(imm0)))
	__(blr)

/* Make a "raw" area on the temp stack, stack-cons a macptr to point to it, 
   and return the macptr.  Size (in bytes, boxed) is in arg_z on entry; macptr
   in arg_z on exit. */
_spentry(makestackblock)
	__(unbox_fixnum(imm0,arg_z))
        __(dnode_align(imm0,imm0,tsp_frame.fixed_overhead+macptr.size))
	__(cmplri(cr0,imm0,tstack_alloc_limit))
	__(bge cr0,1f)
	__(TSP_Alloc_Var_Unboxed(imm0))
	__(li imm0,macptr_header)
	__(la imm1,tsp_frame.data_offset+macptr.size(tsp))
	__(str(imm0,tsp_frame.data_offset(tsp)))
	__(la arg_z,tsp_frame.data_offset+fulltag_misc(tsp))
	__(str(imm1,macptr.address(arg_z)))
        __ifdef([PPC64])
         __(std rzero,macptr.domain(arg_z))
         __(std rzero,macptr.type(arg_z))
        __else
	 __(stfd fp_zero,macptr.domain(arg_z))
        __endif
	__(blr)

/* Too big. Heap cons a gcable macptr */
1:
	__(TSP_Alloc_Fixed_Unboxed(0))
	__(set_nargs(1))
	__(li fname,nrs.new_gcable_ptr)
	__(jump_fname())

/* As above, only set the block's contents to 0. */
_spentry(makestackblock0)
	__(unbox_fixnum(imm0,arg_z))
        __(dnode_align(imm0,imm0,tsp_frame.fixed_overhead+macptr.size))
	__(cmplri(cr0,imm0,tstack_alloc_limit))
	__(bge cr0,3f)
	__(TSP_Alloc_Var_Unboxed(imm0))
	__(Zero_TSP_Frame(imm0,imm1))
	__(li imm0,macptr_header)
	__(la imm1,tsp_frame.data_offset+macptr.size(tsp))
	__(str(imm0,tsp_frame.data_offset(tsp)))
	__(la arg_z,tsp_frame.data_offset+fulltag_misc(tsp))
	__(str(imm1,macptr.address(arg_z))) /* makestackblock0 expects the address to be in imm1 */
	__(stfd fp_zero,macptr.domain(arg_z))
	__(blr)

/* Too big. Heap cons a gcable macptr */
3:
	__(TSP_Alloc_Fixed_Unboxed(0)) /* "raw" block to make the compiler happy */

	__(mr arg_y,arg_z) /* save block size */
	__(li arg_z,t_value) /* clear-p arg to %new-gcable-ptr */
	__(set_nargs(2))
	__(li fname,nrs.new_gcable_ptr)
	__(jump_fname())

/* Make a list of length arg_y (boxed), initial-element arg_z (boxed) on 
   the tstack.  Return the list in arg_z. */
_spentry(makestacklist)
	__(add imm0,arg_y,arg_y)
	__(cmplri(cr1,imm0,((tstack_alloc_limit+1)-cons.size)))
	__(addi imm0,imm0,tsp_frame.fixed_overhead)
	__(bge cr1,3f)
	__(TSP_Alloc_Var_Boxed(imm0,imm1))
	__(mr imm1,arg_y)
	__(cmpri(cr1,imm1,0))
	__(mr arg_y,arg_z)
	__(li arg_z,nil_value)
	__(ldr(imm2,tsp_frame.backlink(tsp)))
	__(la imm2,-tsp_frame.fixed_overhead+fulltag_cons(imm2))
	__(b 2f)
1:
	__(subi imm1,imm1,fixnum1)
	__(cmpri(cr1,imm1,0))
	__(_rplacd(imm2,arg_z))
	__(_rplaca(imm2,arg_y))
	__(mr arg_z,imm2)
	__(subi imm2,imm2,cons.size)
2:
	__(bne cr1,1b)
	__(blr)

3:
	__(cmpri(cr1,arg_y,0))
	__(TSP_Alloc_Fixed_Boxed(0))  /* make the compiler happy */
	__(mr imm1,arg_y) /* count */
	__(mr arg_y,arg_z) /* initial value */
	__(li arg_z,nil_value) /* result */
	__(b 5f)
4:
	__(subi imm1,imm1,fixnum1)
	__(cmpri(cr1,imm1,0))
	__(Cons(arg_z,arg_y,arg_z))
5:
	__(bne cr1,4b)
	__(blr)

/* subtype (boxed) vpushed before initial values. (Had better be a 
	node header subtag.) Nargs set to count of things vpushed. */

_spentry(stkgvector)
	__(la imm0,-fixnum_one(nargs))
	__(cmpri(cr1,imm0,0))
	__(add imm1,vsp,nargs)
	__(ldru(temp0,-node_size(imm1)))
	__(slri(imm2,imm0,num_subtag_bits-fixnumshift))
        __ifdef([PPC64])
         __(unbox_fixnum(imm3,temp0))
         __(or imm2,imm3,imm2)
        __else
	 __(rlwimi imm2,temp0,32-fixnumshift,32-num_subtag_bits,31)
        __endif
        __(dnode_align(imm0,imm0,node_size+tsp_frame.fixed_overhead))
	__(TSP_Alloc_Var_Boxed_nz(imm0,imm3))
	__(str(imm2,tsp_frame.data_offset(tsp)))
	__(la arg_z,tsp_frame.data_offset+fulltag_misc(tsp))
	__(la imm3,misc_header_offset(arg_z))
	__(li imm0,fixnum1)
	__(b 2f)
1:
	__(addi imm0,imm0,fixnum1)
	__(cmpr(cr1,imm0,nargs))
	__(ldru(temp0,-node_size(imm1)))
	__(stru(temp0,node_size(imm3)))
2:
	__(bne cr1,1b)
	__(add vsp,vsp,nargs)
	__(blr)

/* Allocate a "fulltag_misc" object.  On entry, arg_y contains the element */
/* count (boxed) and  arg_z contains the subtag (boxed).  Both of these  */
/* parameters must be "reasonable" (the  subtag must be valid, the element */
/* count must be of type (unsigned-byte 24)/(unsigned-byte 56).  */
/* On exit, arg_z contains the (properly tagged) misc object; it'll have a */
/* proper header on it and its contents will be 0.   imm0 contains  */
/* the object's header (fulltag = fulltag_immheader or fulltag_nodeheader.) */
/* This is intended for things like "make-array" and "%make-bignum" and the  */
/* like.  Things that involve creating small objects of known size can usually */
/* do so inline with less hair. */

/* If this has to go out-of-line (to GC or whatever), it should do so via a  */
/* trap (or should otherwise ensure that both the LR and CTR are preserved  */
/* where the GC can find them.) */


_spentry(misc_alloc)
        __ifdef([PPC64])
         __(rldicr. imm2,arg_y,64-fixnumshift,7)
         __(unbox_fixnum(imm0,arg_z))
         __(sldi imm2,arg_y,num_subtag_bits-fixnumshift)
         __(clrldi imm1,imm0,64-nlowtagbits)
         __(or imm0,imm2,imm0)
         __(extract_fulltag(imm2,imm0))
         __(cmpdi cr1,imm1,lowtag_nodeheader)
         __(cmpdi cr2,imm2,ivector_class_64_bit)
         __(bne- cr0,9f)
         __(cmpdi cr3,imm2,ivector_class_32_bit)
         __(cmpdi cr4,imm2,ivector_class_8_bit)
         __(mr imm2,arg_y)
         __(cmpdi cr5,imm1,subtag_bit_vector)
         __(beq cr1,1f)
         __(beq cr2,1f)
         __(srdi imm2,imm2,1)
         __(beq cr3,1f)
         __(beq cr5,2f)
         __(srdi imm2,imm2,1)
         __(bne cr4,1f)
         __(srdi imm2,imm2,1)
/* imm2 now = byte count.  Add 8 for header, 15 to align, then clear low four bits. */
1:
         __(dnode_align(imm2,imm2,node_size))

	 __(Misc_Alloc(arg_z,imm0,imm2))
	 __(blr)
2:      /* bit-vector case */
         __(addi imm2,arg_y,7<<fixnumshift)
         __(srdi imm2,imm2,3+fixnumshift)
         __(b 1b)
9:                      
	 __(uuo_interr(error_object_not_unsigned_byte_56,arg_y))
        __else
	 __(extract_unsigned_byte_bits_(imm2,arg_y,24))
	 __(unbox_fixnum(imm0,arg_z))
	 __(extract_fulltag(imm1,imm0))
	 __(bne- cr0,9f)
	 __(cmpri(cr0,imm1,fulltag_nodeheader))
	 __(mr imm3,imm0)
	 __(cmplri(cr1,imm0,max_32_bit_ivector_subtag))
	 __(rlwimi imm0,arg_y,num_subtag_bits-fixnum_shift,0,31-num_subtag_bits	/* imm0 now = header */)
	 __(mr imm2,arg_y)
	 __(beq cr0,1f)	/* do probe if node object (fixnum element count = byte count). */
	 __(cmplri(cr0,imm3,max_16_bit_ivector_subtag))
	 __(bng cr1,1f)	/* do probe if 32-bit imm object */
	 __(cmplri(cr1,imm3,max_8_bit_ivector_subtag))
	 __(srwi imm2,imm2,1)
	 __(bgt cr0,2f)
	 __(bgt cr1,1f)
	 __(srwi imm2,imm2,1)
/* imm2 now = byte count.  Add 4 for header, 7 to align, then clear low three bits. */
1:
         __(dnode_align(imm2,imm2,node_size))

	 __(Misc_Alloc(arg_z,imm0,imm2))
	 __(blr)
2:
	 __(cmplri(imm3,subtag_double_float_vector))
	 __(slwi imm2,arg_y,1)
	 __(beq 1b)
	 __(addi imm2,arg_y,7<<fixnumshift)
	 __(srwi imm2,imm2,fixnumshift+3)
	 __(b 1b)
9:
	 __(uuo_interr(error_object_not_unsigned_byte_24,arg_y))
        __endif
        
/* almost exactly as above, but "swap exception handling info"
   on exit and return */
_spentry(ffcallX)
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
        


/* Destructuring-bind, macro-bind. 
   */
/* OK to use arg_x, arg_y for whatever (tagged) purpose; 
   likewise immX regs. 
   arg_z preserved, nothing else in particular defined on exit. 
   nargs contains req count (0-255) in PPC bits mask_req_start/mask_req_width, 
                  opt count (0-255) in PPC bits mask_opt_start/mask_opt_width, 
                  key count (0-255) in PPC bits mask_key_start/mask_key_width, 
                  opt-supplied-p flag in PPC bit mask_initopt, 
                  keyp flag in PPC bit mask_keyp, 
                  &allow-other-keys flag in PPC bit mask_aok, 
   		   &rest flag in PPC bit mask_restp. 
   When mask_keyp bit is set, keyvect contains vector of keyword symbols, 
	length key count. */

_spentry(macro_bind)
        __ifdef([PPC64])
        __else
	__(mr whole_reg,arg_reg)
	__(extract_lisptag(imm0,arg_reg))
	__(cmpri(cr0,imm0,tag_list))
	__(bne- cr0,1f)
	__(_cdr(arg_reg,arg_reg))
	__(b destbind1)
1:
	__(li arg_y,XCALLNOMATCH)
	__(mr arg_z,whole_reg)
	__(set_nargs(2))
	__(b _SPksignalerr)
        __endif

_spentry(destructuring_bind)
	__(mr whole_reg,arg_reg)
        __(b destbind1)
	
_spentry(destructuring_bind_inner)
	__(mr whole_reg,arg_z)
destbind1:
        __ifdef([PPC64])
        __else
	/* Extract required arg count. */
	 /* A bug in gas: can't handle shift count of "32" (= 0 */
	ifelse(eval(mask_req_width+mask_req_start),eval(32),[
	__(clrlwi. imm0,nargs,mask_req_start)
	],[
	__(extrwi. imm0,nargs,mask_req_width,mask_req_start)
	])
	__(extrwi imm1,nargs,mask_opt_width,mask_opt_start)
	__(rlwinm imm2,nargs,0,mask_initopt,mask_initopt)
	__(rlwinm imm4,nargs,0,mask_keyp,mask_keyp)
	__(cmpri(cr4,imm4,0))
	__(rlwinm imm4,nargs,0,mask_restp,mask_restp)
	__(cmpri(cr5,imm4,0))
	__(cmpri(cr1,imm1,0))
	__(cmpri(cr2,imm2,0))
	/* Save entry vsp in case of error. */
	__(mr imm4,vsp)
	__(beq cr0,2f)
1:
	__(cmpri(cr7,arg_reg,nil_value))
	__(extract_lisptag(imm3,arg_reg))
	__(cmpri(cr3,imm3,tag_list))
	__(subi imm0,imm0,1)
	__(cmpri(cr0,imm0,0))
	__(beq cr7,toofew)
	__(bne cr3,badlist)
	__(ldr(arg_x,cons.car(arg_reg)))
	__(ldr(arg_reg,cons.cdr(arg_reg)))
	__(vpush(arg_x))
	__(bne cr0,1b)
2:
	__(beq cr1,rest_keys)
	__(bne cr2,opt_supp)
	/* 'simple' &optionals:	 no supplied-p, default to nil. */
simple_opt_loop:
	__(cmpri(cr0,arg_reg,nil_value))
	__(extract_lisptag(imm3,arg_reg))
	__(cmpri(cr3,imm3,tag_list))
	__(subi imm1,imm1,1)
	__(cmpri(cr1,imm1,0))
	__(li imm5,nil_value)
	__(beq cr0,default_simple_opt)
	__(bne cr3,badlist)
	__(ldr(arg_x,cons.car(arg_reg)))
	__(ldr(arg_reg,cons.cdr(arg_reg)))
	__(vpush(arg_x))
	__(bne cr1,simple_opt_loop)
	__(b rest_keys)
default_simple_opt_loop:
	__(subi imm1,imm1,1)
	__(cmpri(cr1,imm1,0))
default_simple_opt:
	__(vpush(imm5))
	__(bne cr1,default_simple_opt_loop)
	__(b rest_keys)
	/* Provide supplied-p vars for the &optionals. */
opt_supp:
	__(li arg_y,t_value)
opt_supp_loop:
	__(cmpri(cr0,arg_reg,nil_value))
	__(extract_lisptag(imm3,arg_reg))
	__(cmpri(cr3,imm3,tag_list))
	__(subi imm1,imm1,1)
	__(cmpri(cr1,imm1,0))
	__(beq cr0,default_hard_opt)
	__(bne cr3,badlist)
	__(ldr(arg_x,cons.car(arg_reg)))
	__(ldr(arg_reg,cons.cdr(arg_reg)))
	__(vpush(arg_x))
	__(vpush(arg_y))
	__(bne cr1,opt_supp_loop)
	__(b rest_keys)
default_hard_opt_loop:
	__(subi imm1,imm1,1)
	__(cmpri(cr1,imm1,0))
default_hard_opt:
	__(vpush(imm5))
	__(vpush(imm5))
	__(bne cr1,default_hard_opt_loop)
rest_keys:
	__(cmpri(cr0,arg_reg,nil_value))
	__(bne cr5,have_rest)
	__(bne cr4,have_keys)
	__(bne cr0,toomany)
	__(blr)
have_rest:
	__(vpush(arg_reg))
	__(beqlr cr4)
have_keys:
	/* Ensure that arg_reg contains a proper,even-length list. */
	/* Insist that its length is <= 512 (as a cheap circularity check.) */
	__(li imm0,256)
	__(mr arg_x,arg_reg)
count_keys_loop:
	__(extract_lisptag(imm3,arg_x))
	__(cmpri(cr3,imm3,tag_list))
	__(cmpri(cr0,arg_x,nil_value))
	__(subi imm0,imm0,1)
	__(cmpri(cr4,imm0,0))
	__(bne cr3,badlist)
	__(beq cr0,counted_keys)
	__(ldr(arg_x,cons.cdr(arg_x)))
	__(extract_lisptag(imm3,arg_x))
	__(cmpri(cr3,imm3,tag_list))
	__(blt cr4,toomany)
	__(cmpri(cr0,arg_x,nil_value))
	__(bne cr3,badlist)
	__(beq cr0,db_badkeys)
	__(ldr(arg_x,cons.cdr(arg_x)))
	__(b count_keys_loop)
counted_keys:
	/* 
	  We've got a proper, even-length list of key/value pairs in
	arg_reg. For each keyword var in the lambda-list, push a pair
	of NILs on the vstack. */
	__(extrwi. imm0,nargs,mask_key_width,mask_key_start )
	__(mr imm2,imm0) 	/* save number of keys */
	__(li imm5,nil_value)
	__(b push_pair_test)
push_pair_loop:
	__(cmpri(cr0,imm0,1))
	__(subi imm0,imm0,1)
	__(vpush(imm5))
	__(vpush(imm5))
push_pair_test:
	__(bne cr0,push_pair_loop)
	__(slwi imm2,imm2,3)		/* pairs -> bytes */
	__(add imm2,vsp,imm2)		/* imm2 points below pairs */
	__(li imm0,0)			/* count unknown keywords so far */
	__(extrwi imm1,nargs,1,mask_aok) /* unknown keywords allowed */
	__(extrwi nargs,nargs,mask_key_width,mask_key_start)
	/* Now, for each keyword/value pair in the list */
	/*  a) if the keyword is found in the keyword vector, set the */
	/*     corresponding entry on the vstack to the value and the */
	/*     associated supplied-p var to T. */
	/*  b) Regardless of whether or not the keyword is found, */
	/*     if the keyword is :ALLOW-OTHER-KEYS and the value is non-nil, */
	/*     set imm1 to a non-zero value to indicate that unknown keywords */
	/*     are acceptable. */
	/*  c) If the keyword is not found (and isn't :ALLOW-OTHER-KEYS), increment */
	/*     the count of unknown keywords in imm0. */
	/* At the end of the list, signal an error if any unknown keywords were seen */
	/* but not allowed.  Otherwise, return. */

match_keys_loop:
	__(cmpri(cr0,arg_reg,nil_value))
	__(li imm0,0)
	__(li imm3,misc_data_offset)
	__(beq cr0,matched_keys)
	__(ldr(arg_x,cons.car(arg_reg)))
	__(li arg_y,nrs.kallowotherkeys)
	__(cmpr(cr3,arg_x,arg_y))	/* :ALLOW-OTHER-KEYS ? */
	__(ldr(arg_reg,cons.cdr(arg_reg)))
	__(ldr(arg_y,cons.car(arg_reg)))
	__(cmpri(cr0,arg_y,nil_value))
	__(cmpr(cr4,imm0,nargs))
	__(ldr(arg_reg,cons.cdr(arg_reg)))
	__(bne cr3,match_test)
	__(beq cr0,match_test)
	__(ori imm1,imm1,1)
	__(b match_test)
match_loop:
	__(ldrx(temp0,keyvect_reg,imm3))
	__(cmpr(cr0,arg_x,temp0))
	__(addi imm0,imm0,1)
	__(cmpr(cr4,imm0,nargs))
	__(addi imm3,imm3,4)
	__(bne cr0,match_test)
	/* Got a hit.  Unless this keyword's been seen already, set it. */
	__(slwi imm0,imm0,3)
	__(subf imm0,imm0,imm2)
	__(ldr(temp0,0(imm0)))
	__(cmpri(cr0,temp0,nil_value))
	__(li temp0,t_value)
	__(bne cr0,match_keys_loop)	/* already saw this */
	__(str(arg_y,node_size*1(imm0)))
	__(str(temp0,node_size*0(imm0)))
	__(b match_keys_loop)
match_test:
	__(bne cr4,match_loop)
	__(oris imm1,imm1,0x8000)
	__(b match_keys_loop)
matched_keys:
	__(cmpri(cr1,imm1,0))
	__(add imm1,imm1,imm1)
	__(cmpri(cr0,imm1,0))
	__(bgelr cr1)
	__(bnelr cr0)
	/* Some unrecognized keywords.  Complain generically about */
	/* invalid keywords. */
db_badkeys:
	__(li arg_y,XBADKEYS)
	__(b destructure_error)
toomany:
	__(li arg_y,XCALLTOOMANY)
	__(b destructure_error)
toofew:
	__(li arg_y,XCALLTOOFEW)
	__(b destructure_error)
badlist:
	__(li arg_y,XCALLNOMATCH)
	/* b destructure_error */
destructure_error:
	__(mr vsp,imm4)		/* undo everything done to the stack */
	__(mr arg_z,whole_reg)
	__(set_nargs(2))
	__(b _SPksignalerr)
        __endif
        
/* vpush the values in the value set atop the vsp, incrementing nargs. */
/* Discard the tsp frame; leave values atop the vsp. */

_spentry(recover_values)

/* First, walk the segments reversing the pointer to previous segment pointers */
/* Can tell the end because that previous segment pointer is the prev tsp pointer */
	__(ldr(imm0,tsp_frame.backlink(tsp))) /* previous tsp */
	__(mr imm1,tsp) /* current segment */
	__(mr imm2,tsp) /* last segment */
local_label(walkloop):
	__(ldr(imm3,tsp_frame.fixed_overhead+node_size(imm1))) /* next segment */
	__(cmpr(cr0,imm0,imm3)) /* last segment? */
	__(str(imm2,tsp_frame.fixed_overhead+node_size(imm1))) /* reverse pointer */
	__(mr imm2,imm1) /* last segment <- current segment */
	__(mr imm1,imm3) /* current segment <- next segment */
	__(bne cr0,local_label(walkloop))

/* the final segment ptr is now in imm2 */
/* walk backwards, pushing values on VSP and incrementing NARGS */
local_label(pushloop):
	__(ldr(imm0,tsp_frame.data_offset(imm2))) /* nargs in segment */
	__(cmpri(cr0,imm0,0))
	__(cmpr(cr1,imm2,tsp))
	__(la imm3,tsp_frame.data_offset+(2*node_size)(imm2))
	__(add imm3,imm3,imm0)
	__(add nargs,nargs,imm0)
	__(b 2f)
1:
	__(ldru(arg_z,-node_size(imm3)))
	__(cmpri(cr0,imm0,fixnum_one))
	__(subi imm0,imm0,fixnum_one)
	__(vpush(arg_z))
2:
	__(bne cr0,1b)
	__(ldr(imm2,tsp_frame.data_offset+node_size(imm2))) /* previous segment */
	__(bne cr1,local_label(pushloop))
	__(unlink(tsp))
	__(blr)

	
/* Go out of line to do this.  Sheesh. */

_spentry(vpopargregs)
	__(cmpri(cr0,nargs,0))
	__(cmpri(cr1,nargs,2<<fixnumshift))
	__(beqlr cr0)
	__(beq cr1,local_label(yz))
	__(blt cr1,local_label(z))
	__(ldr(arg_z,node_size*0(vsp)))
	__(ldr(arg_y,node_size*1(vsp)))
	__(ldr(arg_x,node_size*2(vsp)))
	__(la vsp,node_size*3(vsp))
	__(blr)
local_label(yz):
	__(ldr(arg_z,node_size*0(vsp)))
	__(ldr(arg_y,node_size*1(vsp)))
	__(la vsp,node_size*2(vsp))
	__(blr)
local_label(z):
	__(ldr(arg_z,node_size*0(vsp)))
	__(la vsp,node_size*1(vsp))
	__(blr)

/* If arg_z is an integer, return in imm0 something whose sign */
/* is the same as arg_z's.  If not an integer, error. */
_spentry(integer_sign)
	__(extract_typecode(imm0,arg_z))
	__(cmpri(cr1,imm0,tag_fixnum))
	__(cmpri(cr0,imm0,subtag_bignum))
	__(mr imm0,arg_z)
	__(beqlr+ cr1)
	__(bne- cr0,1f)
	__(getvheader(imm0,arg_z))
	__(header_length(imm0,imm0)) /* boxed length = scaled size */
	__(addi imm0,imm0,misc_data_offset-4) /* bias, less 1 element */
	__(lwzx imm0,arg_z,imm0)
	__(cmpri(cr0,imm0,0))
	__(li imm0,1)
	__(bgelr cr0)
	__(li imm0,-1)
	__(blr)
1:
	__(uuo_interr(error_object_not_integer,arg_z))

/* like misc_set, only pass the (boxed) subtag in temp0 */
_spentry(subtag_misc_set)
	__(trap_unless_fulltag_equal(arg_x,fulltag_misc,imm0))
	__(trap_unless_lisptag_equal(arg_y,tag_fixnum,imm0))
	__(vector_length(imm0,arg_x,imm1))
	__(trlge(arg_y,imm0))
	__(unbox_fixnum(imm1,temp0))
local_label(misc_set_common):
        __ifdef([PPC64])
         __(extract_typecode(imm0,arg_z))
         __(extract_lowtag(imm3,imm1))
         __(extract_fulltag(imm2,imm1))
         __(cmpdi cr0,imm3,lowtag_nodeheader)
         __(cmpdi cr1,imm2,ivector_class_64_bit)
         __(cmpdi cr2,imm2,ivector_class_32_bit)
         __(cmpdi cr3,imm2,ivector_class_8_bit)
         __(cmpdi cr7,imm0,tag_fixnum)
         __(la imm4,misc_data_offset(arg_y))
         __(beq cr0,_SPgvset)
         __(cmpdi cr0,imm1,subtag_bit_vector)
         __(beq cr1,local_label(set64))
         __(beq cr2,local_label(set32)
         __(beq cr3,local_label(set8))
/* Bit-vector, s16, or u16 */
         __(cmpdi cr1,imm1,subtag_s16_vector)
         __(lis imm3,0x8000)
         __(bne cr0,local_label(setu16))
         __(extract_unsigned_byte_bits_(imm0,arg_z,1))
         __(beq cr1,local_label(sets16))
	 __(extrwi imm1,arg_z,5,32-(fixnumshift+5))	/* imm1 = bitnum */
	 __(la imm1,1+fixnumshift(imm1))
         __(srdi imm0,arg_z,5+fixnumshift)
	 __(srw imm3,imm3,imm1)
         __(bne local_label(misc_set_bad))
         __(cmpdi cr0,arg_z,0)
         __(sldi imm0,imm0,2)
	 __(la imm0,misc_data_offset(imm0))
	 __(ldx imm2,arg_x,imm0)
         __(beq 1f)
         __(or imm2,imm3,imm2)
         __(stdx imm2,arg_x,imm0)
         __(blr)
1:       __(andc imm2,imm3,imm2)
         __(stdx imm2,arg_y,imm0)
         __(blr)
local_label(sets16):
         __(sldi imm0,arg_z,64-(16-fixnumshift))
         __(sradi imm0,imm0,64-(16-fixnumshift))
         __(cmpd imm0,arg_z)
         __(unbox_fixnum(imm0,arg_z))
         __(srdi imm1,arg_y,2)
         __(la imm1,misc_data_offset(imm1))
         __(bne local_label(misc_set_bad))
         __(bne cr7,local_label(misc_set_bad))
         __(sthx imm0,arg_x,imm1)
         __(blr)
local_label(setu16):
         __(extract_unsigned_byte_bits_(imm0,arg_z,16))
         __(unbox_fixnum(imm0,arg_z))
         __(srdi imm1,arg_y,2)                
         __(la imm1,misc_data_offset(imm1))
         __(bne local_label(misc_set_bad))
         __(sthx imm0,arg_x,imm1)
         __(blr)
local_label(set32):     
         __(srdi imm4,arg_y,1)
         __(cmpdi cr1,imm1,subtag_s32_vector)
         __(cmpdi cr2,imm1,subtag_single_float_vector)
         __(cmpdi cr3,imm0,subtag_single_float)
         __(la imm4,misc_data_offset(imm4))
         __(beq cr1,local_label(sets32))
         __(beq cr2,local_label(setsfloat))
         __(extract_unsigned_byte_bits_(imm0,arg_z,32))
         __(unbox_fixnum(imm0,arg_z))
         __(bne local_label(misc_set_bad))
         __(stwx imm0,arg_x,imm4)
         __(blr)
local_label(setsfloat):         
         __(bne cr3,local_label(misc_set_bad))
         __(srdi imm0,arg_z,32)
         __(stwx imm0,arg_x,imm4)
         __(blr)
local_label(sets32):
         __(unbox_fixnum(imm0,arg_z))
         __(sldi imm1,imm0,32)
         __(sradi imm1,imm1,32)
         __(bne cr7,local_label(misc_set_bad))
         __(cmpd imm1,imm0)
         __(bne local_label(misc_set_bad))
         __(srdi imm0,arg_z,32)
         __(stwx imm0,arg_x,imm4)
         __(blr)
local_label(set8):      
         __(extract_lowbyte(imm0,arg_z))                
         __(srdi imm4,arg_y,3)
         __(cmpdi cr1,imm1,subtag_s8_vector)
         __(cmpdi cr2,imm1,subtag_u8_vector)
         __(cmpdi cr0,imm0,subtag_character)
         __(la imm4,misc_data_offset(imm4))
         __(beq cr1,local_label(sets8))
         __(beq cr2,local_label(setu8))
         __(bne cr0,local_label(misc_set_bad))
         __(srwi imm0,arg_z,charcode_shift)
         __(stbx imm0,arg_x,imm4)
         __(blr)
local_label(sets8):     
         __(unbox_fixnum(imm0,arg_z))
         __(sldi imm1,imm0,56))
         __(sradi imm1,imm1,56)
         __(bne cr7,local_label(misc_set_bad))
         __(cmpd imm1,imm0)
         __(bne local_label(misc_set_bad))
         __(stbx imm0,arg_x,imm4)
         __(blr)
local_label(setu8):     
         __(extract_unsigned_byte_bits_(imm0,arg_z,8))
         __(unbox_fixnum(imm0,arg_z))
         __(bne local_label(misc_set_bad))
         __(stbx imm0,arg_x,imm4)
         __(blr)
local_label(set64):     
         __(cmpdi cr1,imm1,subtag_s64_vector)
         __(cmpdi cr2,imm1,subtag_double_float_vector)                         
         __(cmpdi cr5,imm0,subtag_double_float)
         __(cmpdi cr6,imm0,subtag_bignum)
         __(beq cr1,local_label(sets64))
         __(beq cr2,local_label(setdfloat))
         __(cmpdi cr0,arg_z,0)
         __(bne cr7,local_label(setu64_maybe_bignum))
         __(unbox_fixnum(imm0,arg_z))
         __(blt cr0,local_label(misc_set_bad))
         __(stdx imm0,arg_x,imm4)
         __(blr)
local_label(setu64_maybe_bignum):
         __(bne cr6,local_label(misc_set_bad))
         __(getvheader(imm1,arg_z))
         __(ld imm0,misc_data_offset(arg_z))
         __(lwz imm3,misc_data_offset+8(arg_z))
         __(rotldi imm0,imm0,32)
         __(cmpdi cr2,imm1,two_digit_bignum_header)
         __(cmpdi cr3,imm1,three_digit_bignum_header)
         __(cmpdi cr0,imm0,0)
         __(cmpdi cr4,imm0,0)
         __(beq cr2,1f)
         __(bne cr3,local_label(misc_set_bad))
         __(bne cr4,local_label(misc_set_bad))
         __(stdx imm0,arg_x,imm4)
         __(blr)
1:       __(blt cr0,local_label(misc_set_bad))
         __(stdx imm0,arg_x,imm4)
         __(blr)
local_label(setdfloat): 
         __(bne cr5,local_label(misc_set_bad))
         __(ld imm0,misc_dfloat_offset(arg_z))
         __(stdx imm0,arg_x,imm4)
         __(blr)
local_label(sets64):    
         __(unbox_fixnum(imm0,arg_z))
         __(bne cr7,local_label(sets64_maybe_bignum))
         __(stdx imm0,arg_x,imm4)
         __(blr)
local_label(sets64_maybe_bignum):       
         __(bne cr6,local_label(misc_set_bad))
         __(getvheader(imm1,arg_z))
         __(ld imm0,misc_data_offset(arg_z))
         __(cmpdi cr1,imm1,two_digit_bignum_header)
         __(rotldi imm0,imm0,32)
         __(bne cr1,local_label(misc_set_bad))
         __(stdx imm0,arg_x,imm4)
         __(blr)
local_label(misc_set_bad):
	 __(mr arg_y,arg_z)
	 __(mr arg_z,arg_x)
	 __(li arg_x,XNOTELT)
	 __(set_nargs(3))
	 __(b _SPksignalerr)
        __else
	__(extract_fulltag(imm2,imm1))
	__(cmpri(cr0,imm2,fulltag_nodeheader))
	__(cmpri(cr1,imm1,max_32_bit_ivector_subtag))
	__(cmpri(cr2,imm1,max_8_bit_ivector_subtag))
	__(addi imm0,arg_y,misc_data_offset)
        __(beq cr0,_SPgvset)
local_label(set_imm):
	__(extract_lisptag(imm2,arg_z))
	__(cmpri(cr7,imm2,tag_misc))
	__(cmpri(cr6,imm2,tag_imm))
	__(cmpri(cr5,imm2,tag_fixnum))
	__(bgt cr1,local_label(set_not32))
	__(cmpri(cr1,imm1,subtag_single_float_vector))
	__(cmpri(cr0,imm1,subtag_s32_vector))
	__(beq cr1,local_label(set_sfloat))
	__(beq cr0,local_label(set_signed))
	/* Either a non-negative fixnum, a one-digit bignum, or a two-digit */
	/* bignum whose sign-digit is 0 is ok. */
	__(srawi. imm1,arg_z,fixnum_shift)
	__(bne cr5,local_label(set_not_fixnum_u32))
	__(blt- cr0,local_label(set_bad))
local_label(set_set32):
	__(strx(imm1,arg_x,imm0))
	__(blr)
local_label(set_not_fixnum_u32):
	__(bne cr7,local_label(set_bad))
	__(extract_header(imm2,arg_z))
	__(cmpri(cr0,imm2,one_digit_bignum_header))
	__(cmpri(cr1,imm2,two_digit_bignum_header))
	__(vrefr(imm1,arg_z,0))
	__(cmpri(cr2,imm1,0))
	__(bne cr0,local_label(set_not_1_digit))
	__(bge cr2,local_label(set_set32))
	__(b local_label(set_bad))
local_label(set_not_1_digit):
	__(bne- cr1,local_label(set_bad))
	__(vrefr(imm2,arg_z,1))
	__(cmpri(cr0,imm2,0))
	__(bne- cr1,local_label(set_bad))
	__(beq cr0,local_label(set_set32))
local_label(set_bad):
	/* arg_z does not match the array-element-type of arg_x. */
	__(mr arg_y,arg_z)
	__(mr arg_z,arg_x)
	__(li arg_x,XNOTELT)
	__(set_nargs(3))
	__(b _SPksignalerr)
local_label(set_signed):
	__(unbox_fixnum(imm1,arg_z))
	__(beq cr5,local_label(set_set32))
	__(bne cr7,local_label(set_bad))
	__(extract_header(imm2,arg_z))
	__(cmpri(cr0,imm2,one_digit_bignum_header))
	__(vrefr(imm1,arg_z,0))
	__(bne- cr0,local_label(set_bad))
	__(strx(imm1,arg_x,imm0))
	__(blr)
local_label(set_sfloat):
	__(bne- cr7,local_label(set_bad))
	__(extract_header(imm2,arg_z))
	__(cmpri(cr0,imm2,single_float_header))
	__(bne- cr0,local_label(set_bad))
	__(ldr(imm1,single_float.value(arg_z)))
	__(strx(imm1,arg_x,imm0))
	__(blr)
	
local_label(set_not32):
	__(cmpri(cr1,imm1,max_16_bit_ivector_subtag))
	__(bgt cr2,local_label(set_not8))
	/* 8-bit objects are either u8, s8, or base_strings. */
	/* cr2_eq is set if base_string (= max_8_bit_ivector_subtag) */
	__(cmpri(cr1,imm1,subtag_s8_vector))
	__(extract_lisptag(imm2,arg_z))
	__(srwi imm0,arg_y,2)
	__(la imm0,misc_data_offset(imm0))
	__(extract_unsigned_byte_bits_(imm1,arg_z,8))
	__(beq cr2,local_label(set_char8))
	__(beq cr1,local_label(set_signed8))
	__(unbox_fixnum(imm1,arg_z))
	__(bne- cr0,local_label(set_bad))
	__(stbx imm1,arg_x,imm0)
	__(blr)
local_label(set_char8):
	__(extract_lowbyte(imm2,arg_z))
	__(cmpri(cr2,imm2,subtag_character))
	__(srwi imm1,arg_z,charcode_shift)
	__(bne- cr2,local_label(set_bad))
	__(stbx imm1,arg_x,imm0)
	__(blr)
local_label(set_signed8):
	__(unbox_fixnum(imm1,arg_z))
	__(extsb imm2,imm1)
	__(cmpr(cr0,imm2,imm1))
	__(bne- cr5,local_label(set_bad))
	__(bne- cr0,local_label(set_bad))
	__(stbx imm1,arg_x,imm0)
	__(blr)
local_label(set_not8):
	__(cmpri(cr2,imm1,subtag_bit_vector))
	__(bgt cr1,local_label(set_not16))
/* 16-bit objects are either u16, s16, or general_strings. */
/* cr1_eq is set if s16_vector (= max_16_bit_ivector_subtag) */
	__(cmpri(cr0,imm1,subtag_simple_general_string))
	__(srwi imm0,arg_y,1)
	__(la imm0,misc_data_offset(imm0))
	__(beq cr1,local_label(set_s16))
	__(beq cr0,local_label(set_char16))
	__(extract_unsigned_byte_bits_(imm1,arg_z,16))
	__(unbox_fixnum(imm1,arg_z))
	__(bne- cr0,local_label(set_bad))
	__(sthx imm1,arg_x,imm0)
	__(blr)
local_label(set_s16):
	__(unbox_fixnum(imm1,arg_z))
	__(extsh imm2,imm1)
	__(cmpr(cr0,imm2,imm1))
	__(bne- cr5,local_label(set_bad))
	__(bne- cr0,local_label(set_bad))
	__(sthx imm1,arg_x,imm0)
	__(blr)
local_label(set_char16):
	__(extract_lowbyte(imm2,arg_z))
	__(cmpri(cr0,imm2,subtag_character))
	__(srwi imm1,arg_z,charcode_shift)
	__(bne- cr0,local_label(set_bad))
	__(sthx imm1,arg_x,imm0)
	__(blr)
local_label(set_not16):	
	__(bne cr2,local_label(set_dfloat))
	/* Bit vector case. */
	__(cmplri(cr2,arg_z,fixnumone))   /* nothing not a (boxed) bit  */
	__(extrwi imm1,arg_y,5,32-(fixnumshift+5))	/* imm1 = bitnum */
	__(extlwi imm2,arg_z,1,31-fixnumshift)
	__(srw imm2,imm2,imm1)
	__(lis imm3,0x8000)
	__(rlwinm imm0,arg_y,32-5,5,31-fixnumshift)
	__(la imm0,misc_data_offset(imm0))
	__(srw imm3,imm3,imm1)
	__(bgt- cr2,local_label(set_bad))
	__(ldrx(imm1,arg_x,imm0))
	__(andc imm1,imm1,imm3)
	__(or imm1,imm1,imm2)
	__(strx(imm1,arg_x,imm0))
	__(blr)

local_label(set_dfloat):
	__(bne- cr7,local_label(set_bad))		/* not tag_misc */
	__(extract_header(imm2,arg_z))
	__(cmpri(cr0,imm2,double_float_header))
	__(slwi imm0,arg_y,1)  /* imm0 gets index * 2 */
	__(la imm0,misc_dfloat_offset(imm0)) /* + offset */
	__(bne- cr0,local_label(set_bad))
	__(ldr(imm1,double_float.value(arg_z))) /* get value parts */
	__(ldr(imm2,double_float.value+4(arg_z)))
	__(strx(imm1,arg_x,imm0))
	__(la imm0,4(imm0))
	__(strx(imm2,arg_x,imm0))
	__(blr)
        __endif

/* misc_set (vector index newval).  Pretty damned similar to 
   misc_ref, as one might imagine. 
*/
_spentry(misc_set)
	__(trap_unless_fulltag_equal(arg_x,fulltag_misc,imm0))
	__(trap_unless_lisptag_equal(arg_y,tag_fixnum,imm0))
	__(vector_length(imm0,arg_x,imm1))
	__(trlge(arg_y,imm0))
	__(extract_lowbyte(imm1,imm1))
        __(b local_label(misc_set_common))
        
/* "spread" the lexpr in arg_z. 
   ppc2-invoke-fn assumes that temp1 is preserved here. */
_spentry(spread_lexprz)
	__(ldr(imm0,0(arg_z)))
	__(cmpri(cr3,imm0,3<<fixnumshift))
	__(cmpri(cr4,imm0,2<<fixnumshift))
	__(add imm1,arg_z,imm0)
	__(cmpri(cr0,imm0,0))
	__(add nargs,nargs,imm0)
	__(cmpri(cr1,nargs,0))
	__(cmpri(cr2,nargs,2<<fixnumshift))
	__(la imm1,node_size(imm1))
	__(bge cr3,9f)
	__(beq cr4,2f)
	__(bne cr0,1f)
	/* lexpr count was 0; vpop the arg regs that */
	/* were vpushed by the caller */
	__(beqlr cr1)
	__(vpop(arg_z))
	__(bltlr cr2)
	__(vpop(arg_y))
	__(beqlr cr2)
	__(vpop(arg_x))
	__(blr)

	/* vpush args from the lexpr until we have only */
	/* three left, then assign them to arg_x, arg_y, */
	/* and arg_z. */
8:
	__(cmpri(cr3,imm0,4<<fixnumshift))
	__(subi imm0,imm0,fixnumone)
	__(ldru(arg_z,-node_size(imm1)))
	__(vpush(arg_z))
9:
	__(bne cr3,8b)
	__(ldr(arg_x,-node_size*1(imm1)))
	__(ldr(arg_y,-node_size*2(imm1)))
	__(ldr(arg_z,-node_size*3(imm1)))
	__(blr)

	/* lexpr count is two: set arg_y, arg_z from the */
	/* lexpr, maybe vpop arg_x */
2:	
	__(ldr(arg_y,-node_size*1(imm1)))
	__(ldr(arg_z,-node_size*2(imm1)))
	__(beqlr cr2)		/* return if (new) nargs = 2 */
	__(vpop(arg_x))
	__(blr)

	/* lexpr count is one: set arg_z from the lexpr, */
	/* maybe vpop arg_y, arg_x */
1:	
	__(ldr(arg_z,-node_size(imm1)))
	__(bltlr cr2)		/* return if (new) nargs < 2 */
	__(vpop(arg_y))
	__(beqlr cr2)		/* return if (new) nargs = 2 */
	__(vpop(arg_x))
	__(blr)
        
		
_spentry(reset)
	.globl _SPthrow
	__(nop)
	__(ref_nrs_value(temp0,toplcatch))
	__(li temp1,XSTKOVER)
	__(vpush(temp0))
	__(vpush(temp1))
	__(set_nargs(1))
	__(b _SPthrow)

	
/* "slide" nargs worth of values up the vstack.  IMM0 contains */
/* the difference between the current VSP and the target. */
_spentry(mvslide)
	__(cmpri(cr0,nargs,0))
	__(mr imm3,nargs)
	__(add imm2,vsp,nargs)
	__(add imm2,imm2,imm0)
	__(add imm0,vsp,nargs)
	__(beq 2f)
1:
	__(cmpri(cr0,imm3,1<<fixnumshift))
	__(subi imm3,imm3,1<<fixnumshift)
	__(ldru(temp0,-node_size(imm0)))
	__(stru(temp0,-node_size(imm2)))
	__(bne cr0,1b)
2:
	__(mr vsp,imm2)
	__(blr)

/* Build a new TSP area to hold nargs worth of multiple-values. */
/* Pop the multiple values off of the vstack. */
/* The new TSP frame will look like this: */
/* 
+--------+-------+-------+---------+--------+--------+--------+======+----------+
| ptr to | zero  | nargs | ptr to  | valn-1 | valn-2 | val-0  | ???? | prev TSP | 
|  prev  |       |       |  prev   |        |        |        | fill |          | 
| TSP    |       |       | segment |        |        |        |      |          |
+--------+-------+-------+---------+--------+--------+--------+------+----------+ 
 */
/* e.g., the first multiple value goes in the last cell in the frame, the */
/* count of values goes in the first word, and the word after the value count */
/* is 0 if the number of values is even (for alignment). */
/* Subsequent calls to .SPadd_values preserve this alignment. */
/* .SPrecover_values is therefore pretty simple. */

_spentry(save_values)
	__(mr imm1,tsp)

/* common exit: nargs = values in this set, imm1 = ptr to tsp before call to save_values */
local_label(save_values_to_tsp):
	__(mr imm2,tsp)
	__(dnode_align(imm0,nargs,tsp_frame.fixed_overhead+(2*node_size))) /* count, link */
	__(TSP_Alloc_Var_Boxed_nz(imm0,imm3))
	__(str(imm1,tsp_frame.backlink(tsp))) /* keep one tsp "frame" as far as rest of lisp is concerned */
	__(str(nargs,tsp_frame.data_offset(tsp)))
	__(str(imm2,tsp_frame.data_offset+node_size(tsp))) /* previous tsp */
	__(la imm3,tsp_frame.data_offset+node_size*2(tsp))
	__(add imm3,imm3,nargs)
	__(add imm0,vsp,nargs)
	__(cmpr(cr0,imm0,vsp))
	__(b 2f)
1:
	__(ldru(arg_z,-node_size(imm0)))
	__(cmpr(cr0,imm0,vsp))
	__(stru(arg_z,-node_size(imm3)))
2:
	__(bne cr0,1b)
	__(add vsp,vsp,nargs) /*  discard values */
	__(blr)
	

/* Add the multiple values that are on top of the vstack to the set */
/* saved in the top tsp frame, popping them off of the vstack in the */
/* process.  It is an error (a bad one) if the TSP contains something */
/* other than a previously saved set of multiple-values. */
/* Since adding to the TSP may cause a new TSP segment to be allocated, */
/* each add_values call adds another linked element to the list of */
/* values. This makes recover_values harder. */

_spentry(add_values)
	__(cmpri(cr0,nargs,0))
	__(ldr(imm1,0(tsp)))
	__(bne cr0,local_label(save_values_to_tsp))
	__(blr)
        
/* On entry, R11->callback-index */
/* Restore lisp context, then funcall #'%pascal-functions% with */
/* two args: callback-index, args-ptr (a macptr pointing to the args on the stack) */
_spentry(callback)
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
	__(la arg_z,stack_align(c_reg_save.size)+c_frame.param0(sp))	/* parameters (tagged as a fixnum) */

	/* Recover lisp thread context. Have to call C code to do so. */
	__(ref_global(r12,get_tcr))
	__(mtctr r12)
        __(li r3,1)
	__(stru(sp,-(stack_align(c_frame.minsiz))(sp)))
	__(bctrl)
	__(la rcontext,TCR_BIAS(r3))
	__(la sp,(stack_align(c_frame.minsiz))(sp))

	__(ldr(vsp,tcr.save_vsp(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))		
	__(li rzero,0)
	__(mtxer rzero) /* lisp wants the overflow bit being clear */
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
	__(ldr(r3,c_frame.param0(sp)))
	__(ldr(r4,c_frame.param1(sp)))
        __(lfd f1,c_frame.param2(sp))
	__(ldr(r5,c_frame.savelr(sp)))
	__(mtlr r5)
	__(ldr(r5,c_frame.crsave(sp)))
	__(mtcr r5)
	__(blr)
        
/* Like misc_alloc (a LOT like it, since it does most of the work), but takes */
/* an initial-value arg in arg_z, element_count in arg_x, subtag in arg_y. */
/* Calls out to %init-misc, which does the rest of the work. */

_spentry(misc_alloc_init)
	__(mflr loc_pc)
	__(build_lisp_frame(fn,loc_pc,vsp))
	__(li fn,0)
	__(mr temp0,arg_z)		/* initval */
	__(mr arg_z,arg_y)		/* subtag */
	__(mr arg_y,arg_x)		/* element-count */
	__(bl _SPmisc_alloc)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp))) 
	__(discard_lisp_frame())
	__(li fname,nrs.init_misc)
	__(set_nargs(2))
	__(mr arg_y,temp0)
	__(jump_fname())

/* As in stack_misc_alloc above, only with a non-default initial-value. */

_spentry(stack_misc_alloc_init)
	__(mflr loc_pc)
	__(build_lisp_frame(fn,loc_pc,vsp))
	__(li fn,0)
	__(mr temp0,arg_z) /* initval */
	__(mr arg_z,arg_y) /* subtag */
	__(mr arg_y,arg_x) /* element-count */
	__(bl _SPstack_misc_alloc)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(discard_lisp_frame())
	__(li fname,nrs.init_misc)
	__(set_nargs(2))
	__(mr arg_y,temp0)
	__(jump_fname())

	
/*
   Restore the special bindings from the top of the tstack, 
   leaving the tstack frame allocated. 
   Note that there might be 0 saved bindings, in which case 
   do nothing. 
   Note also that this is -only- called from an unwind-protect 
   cleanup form, and that .SPnthrowXXX is keeping one or more 
   values in a frame on top of the tstack. 
*/
	
_spentry(progvrestore)

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
	

_spentry(popj)
	.globl C(popj)
C(popj):
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame())
	__(blr)

_spentry(restorefullcontext)
	__(mflr loc_pc)
	__(mtctr loc_pc)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(mtlr loc_pc)
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame())
	__(bctr)

_spentry(savecontextvsp)
	__(ldr(imm0,tcr.cs_limit(rcontext)))
	__(build_lisp_frame(fn,loc_pc,vsp))
	__(mr fn,nfn)
	__(trllt(sp,imm0))
	__(blr)

_spentry(savecontext0)
	__(add imm0,vsp,imm0)
	__(build_lisp_frame(fn,loc_pc,imm0))
	__(ldr(imm0,tcr.cs_limit(rcontext)))
	__(mr fn,nfn)
	__(trllt(sp,imm0))
	__(blr)


/* Like .SPrestorefullcontext, only the saved return address */
/* winds up in loc-pc instead of getting thrashed around ... */
_spentry(restorecontext)
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(ldr(vsp,lisp_frame.savevsp(sp)))
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame())
	__(blr)

        
/* Nargs is valid; all arg regs, lexpr-count pushed by caller. */
/* imm0 = vsp to restore. */
/* Return all values returned by caller to its caller, hiding */
/* the variable-length arglist. */
/* If we can detect that the caller's caller didn't expect */
/* multiple values, then things are even simpler. */
_spentry(lexpr_entry)
	__(ref_global(imm1,ret1val_addr))
	__(cmpr(cr0,imm1,loc_pc))
	__(build_lisp_frame(fn,loc_pc,imm0))
	__(bne cr0,1f)
	__(ref_global(imm0,lexpr_return))
	__(build_lisp_frame(rzero,imm0,vsp))
	__(mr loc_pc,imm1)
	__(ldr(imm0,tcr.cs_limit(rcontext)))
	__(trllt(sp,imm0))
	__(li fn,0)
	__(blr)

/* The single-value case just needs to return to something that'll pop */
/* the variable-length frame off of the vstack. */
1:
	__(ref_global(loc_pc,lexpr_return1v))
	__(ldr(imm0,tcr.cs_limit(rcontext)))
	__(trllt(sp,imm0))
	__(li fn,0)
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
        
_spentry(darwin_syscall)
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
	__(b 1f)
	__(b 9f)
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
	__(vpop_saveregs)
	__(mr rcontext,imm2)
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))
	__(str(imm4,tcr.valence(rcontext)))
	__(ldr(loc_pc,lisp_frame.savelr(sp)))
	__(mtlr loc_pc)
	__(ldr(fn,lisp_frame.savefn(sp)))
	__(discard_lisp_frame)
        __(mtxer rzero)
	__(check_pending_interrupt([cr1]))
	__(blr)
        
        
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
	 __(Misc_Alloc_Fixed(arg_z,imm0,16))
	 __(str(imm1,misc_data_offset(arg_z)))
        __else
	 __(li imm0,one_digit_bignum_header)
	 __(xoris imm1,imm1,0xc000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,8))
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
	 __(Misc_Alloc_Fixed(arg_z,imm0,16))
	 __(str(imm1,misc_data_offset(arg_z)))
        __else
	 __(li imm0,one_digit_bignum_header)
	 __(xoris imm1,imm1,0xc000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,8))
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
	 __(mulld imm1,imm3,imm2) /* imm1 = low  32 bits */
	 __(mulhd imm0,imm3,imm2) /* imm0 = high 32 bits */
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
        __ifdef([PPC64])
         __(cmpd cr1,arg_y,arg_z)
         __(extract_typecode(imm0,arg_y))
         __(extract_typecode(imm1,arg_z))
         __(cmpd cr0,imm0,imm1)
         __(extract_fulltag(imm2,arg_y))
         __(beq cr1,1f)
         __(cmpdi cr1,imm2,fulltag_misc)
         __(bne cr0,2f)
         __(bne cr1,2f)
        __else
	 __(cmpr(cr0,arg_y,arg_z))
	 __(extract_lisptag(imm0,arg_y))
	 __(extract_lisptag(imm1,arg_z))
	 __(cmpr(cr1,imm0,imm1))
	 __(beq cr0,1f)
	 __(cmpri(cr0,imm0,tag_misc))
	 __(bne cr1,2f)
	 __(bne cr0,2f)
        __endif
	__(jump_builtin(_builtin_eql,2))
1:	__(li arg_z,t_value)
	__(blr)
2:	__(li arg_z,nil_value)
	__(blr)
        
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
        __else
4:	__(extract_lisptag(imm0,temp0))
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
	__(cmpri(cr1,imm1,min_vector_subtag))
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
	 __(Misc_Alloc_Fixed(arg_z,imm0,16))
	 __(str(imm1,misc_data_offset(arg_z)))
        __else
	 __(li imm0,one_digit_bignum_header)
	 __(xoris imm1,imm1,0xc000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,8))
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

/* Enter the debugger */
_spentry(breakpoint)
	__(li r3,0)
	__(tw 28,sp,sp)	/* 28 = lt|gt|eq (assembler bug for the latter) */
	__(blr)		/* if handler didn't */

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
	__(vpop_saveregs())
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
	__(str(imm2,tcr.valence(rcontext)))	
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
	__(blr)
        
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
	__(ldr(r2,tcr.native_thread_info(rcontext)))
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
_spentry(syscall)
/*
	We're entered with an eabi_c_frame on the C stack.  There's a
	lisp_frame reserved underneath it; we'll link it in in a minute.
	Load the outgoing GPR arguments from eabi_c_frame.param[0-7],
	then shrink the eabi_c_frame.
*/
	__(mflr loc_pc)
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
	__(stw imm3,tcr.valence(rcontext))
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
        
/* arg_z should be of type (UNSIGNED-BYTE 64); 
   On PPC32, return high 32 bits in imm0, low 32 bits in imm1
   On PPC64, return unboxed value in imm0 */

_spentry(getu64)
        __ifdef([PPC64])
        __(extract_typecode(imm0,arg_z))
        __(cmpdi cr0,imm0,tag_fixnum)
        __(cmpdi cr2,arg_z,0)
        __(cmpdi cr1,imm0,subtag_bignum)
        __(bne cr0,1f)
        __(unbox_fixnum(imm0,arg_z))
        __(bgelr cr2)
0:             
	__(uuo_interr(error_object_not_u64,arg_z))
        
1:      __(bne cr1,0b)
        __(getvheader(imm1,arg_z))
        __(ld imm0,misc_data_offset(arg_z))
        __(cmpdi cr2,imm1,two_digit_bignum_header)
        __(rotldi imm0,imm0,32)
        __(cmpdi cr1,imm1,three_digit_bignum_header)
        __(cmpdi cr0,imm0,0)
        __(beq cr2,2f)
        __(lwz imm1,misc_data_offset+8(arg_z))
        __(bne cr1,0b)
        __(cmpwi imm1,0)
        __(bne 0b)
        __(blr)
2:      __(blt 0b)
        __(blr)        
        __else
	__(extract_typecode(imm0,arg_z))
	__(cmpri(cr0,imm0,tag_fixnum))
	__(cmpri(cr1,arg_z,0))
	__(cmpri(cr2,imm0,subtag_bignum))
	__(unbox_fixnum(imm1,arg_z))
	__(bne cr0,8f)
	__(bgelr cr1)
9:
	__(uuo_interr(error_object_not_u64,arg_z))
8:
	__(bne- cr2,9b)
	__(getvheader(imm2,arg_z))
	__(cmpri(cr2,imm2,two_digit_bignum_header))
	__(vrefr(imm1,arg_z,0))
	__(cmpri(cr1,imm1,0))
	__(li imm0,0)
	__(bge cr2,2f)
	__(blt- cr1,9b)
	__(blr)
2:
	__(cmpri(cr0,imm2,three_digit_bignum_header))
	__(vrefr(imm0,arg_z,1))
	__(cmpri(cr1,imm0,0))
	__(bne cr2,3f)
	__(blt- cr1,9b)
	__(blr)
3:
	__(vrefr(imm2,arg_z,2))
	__(cmpri(cr1,imm2,0))
	__(bne- cr0,9b)
	__(bne- cr1,9b)
	__(blr)
        __endif
        
/* arg_z should be of type (SIGNED-BYTE 64); 
        PPC32:   return high 32 bits  in imm0, low 32 bits in imm1 
        PPC64:   return unboxed value in imm0 */

_spentry(gets64)
        __ifdef([PPC64])
	 __(extract_typecode(imm1,arg_z))
         __(unbox_fixnum(imm0,arg_z))
	 __(cmpri(cr0,imm1,tag_fixnum))
	 __(cmpri(cr2,imm1,subtag_bignum))
         __(beqlr cr0)
         __(bne cr2,9f)
         __(ld imm1,misc_header_offset(arg_z))
         __(ld imm0,misc_data_offset(arg_z))
         __(cmpdi imm1,two_digit_bignum_header)
         __(rotldi imm0,imm0,32)
         __(beqlr)
        __else
	 __(extract_typecode(imm0,arg_z))
	 __(cmpri(cr0,imm0,tag_fixnum))
	 __(cmpri(cr2,imm0,subtag_bignum))
	 __(unbox_fixnum(imm1,arg_z))
	 __(srawi imm0,imm1,31)
	 __(beqlr cr0)
	 __(bne cr2,9f)
	 __(getvheader(imm2,arg_z))
	 __(cmpri(cr2,imm2,two_digit_bignum_header))
	 __(vrefr(imm1,arg_z,0))
	 __(srawi imm0,imm1,31)
	 __(bltlr cr2)
	 __(vrefr(imm0,arg_z,1))
	 __(beqlr cr2)
        __endif
9:
	__(uuo_interr(error_object_not_s64,arg_z))

/*
  Construct a lisp integer out of the 64-bit unsigned value in
  imm0 (high 32 bits) and imm1 (low 32 bits). */
_spentry(makeu64)
        __ifdef([PPC64])
	 __(clrrdi. imm1,imm0,64-nfixnumtagbits)
	 __(cmpri(cr1,imm0,0))
	 __(box_fixnum(arg_z,imm0))
	 __(beqlr cr0) /* A fixnum */
         __(rotldi imm1,imm0,32)
	 __(li imm2,two_digit_bignum_header)
	 __(blt cr1,2f)
	 __(Misc_Alloc_Fixed(arg_z,imm2,node_size*2))
	 __(str(imm1,misc_data_offset(arg_z)))
	 __(blr)
2:
	 __(li imm2,two_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm2,node_size*4))
	 __(str(imm1,misc_data_offset(arg_z)))
	 __(blr)
        __else        
 	 __(cmpri(cr1,imm0,0))
	 __(rlwinm. imm2,imm1,0,0,fixnum_shift)
	 __(li imm2,three_digit_bignum_header)
	 __(box_fixnum(arg_z,imm1))
	 __(blt cr1,3f)
	 __(bne cr1,2f)
	 __(beqlr cr0) /* A fixnum */
	 __(blt cr0,2f)
	 __(li imm2,one_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm2,8))
	 __(str(imm1,misc_data_offset(arg_z)))
	 __(blr)
2:
	 __(li imm2,two_digit_bignum_header)
3:
	 __(Misc_Alloc_Fixed(arg_z,imm2,16))
	 __(str(imm1,misc_data_offset(arg_z)))
	 __(str(imm0,misc_data_offset+4(arg_z)))
	 __(blr)
        __endif


/*
  Construct a lisp integer out of the 64-bit signed value in
  imm0 (high 32 bits) and imm1 (low 32 bits). */
_spentry(makes64)
        __ifdef([PPC64])
	 __(addo imm1,imm0,imm0)
         __(addo imm1,imm1,imm1)
	 __(addo. arg_z,imm1,imm1)
	 __(bnslr+)
	 __(mtxer rzero)
	 __(li imm1,two_digit_bignum_header)
         __(rotldi imm2,imm0,32)
	 __(Misc_Alloc_Fixed(arg_z,imm1,node_size*2))
	 __(str(imm1,misc_data_offset(arg_z)))
         __(blr)
        __else
	 __(srawi imm2,imm1,31)
	 __(cmpr(cr1,imm2,imm0))
	 __(addo imm2,imm1,imm1)
	 __(addo. arg_z,imm2,imm2)
	 __(bne cr1,2f) /* High word is significant */
	 __(li imm2,one_digit_bignum_header)
	 __(bnslr cr0) /* No overflow:	 fixnum */
	 __(mtxer rzero)
	 __(Misc_Alloc_Fixed(arg_z,imm2,8))
	 __(str(imm1,misc_data_offset(arg_z)))
	 __(blr)
2:
	 __(mtxer rzero)
	 __(li imm2,two_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm2,16))
	 __(str(imm1,misc_data_offset(arg_z)))
	 __(str(imm0,misc_data_offset+4(arg_z)))
	 __(blr)
        __endif

/* imm0:imm1 constitute an unsigned integer, almost certainly a bignum.
   Make a lisp integer out of those 128 bits .. */
_spentry(makeu128)
        __ifdef([PPC64])
         __(cmpdi imm0,0)
         __(cmpdi cr1,imm1,0)
         __(srdi imm3,imm0,32)
         __(srawi imm4,imm0,31)
         __(cmpdi cr3,imm3,0)
         __(cmpdi cr4,imm4,0)
         __(li imm2,five_digit_bignum_header)
         __(blt cr1,0f)
         __(beq 3f)
0:              
         __(bgt 1f)
         /* All 128 bits are significant, and the most significant
            bit is set.  Allocate a 5-digit bignum (with a zero
            sign digit */
         __(Misc_Alloc_Fixed(arg_z,imm2,48))
         __(rotldi imm0,imm0,32)
         __(rotldi imm1,imm1,32)
         __(std imm1,misc_data_offset(arg_z))
         __(std imm0,misc_data_offset+8(arg_z))
         __(blr)
1:      /* If the high word of imm0 is a zero-extension of the low
           word, we only need 3 digits ; otherwise, we need 4. */
         __(li imm2,three_digit_bignum_header)
         __(rotldi imm1,imm1,32)
         __(bne cr3,2f) /* high word of imm0 is non-zero */
         __(bne cr4,2f) /* sign bit is on in low word of imm0 */
         __(Misc_Alloc_Fixed(arg_z,imm2,32))
         __(std imm1,misc_data_offset(arg_z))
         __(stw imm0,misc_data_offset+8(arg_z))
         __(blr)
2:       __(li imm2,four_digit_bignum_header)
         __(rotldi imm0,imm0,32)
         __(Misc_Alloc_Fixed(arg_z,imm2,48))
         __(std imm1,misc_data_offset(arg_z))
         __(std imm0,misc_data_offset+8(arg_z))
         __(blr)
3:       __(mr imm0,imm1)
         __(b _SPmakeu64)              
        __else
         __(twgei r0,r0)
        __endif

/* imm0:imm1 constitute a signed integer, almost certainly a bignum.
   Make a lisp integer out of those 128 bits .. */
_spentry(makes128)
        __ifdef([PPC64])
        /* Is imm0 just a sign-extension of imm1 ? */
         __(srdi imm2,imm1,64)
        /* Is the high word of imm0 just a sign-extension of the low word ? */
         __(extsw imm3,imm0)
         __(cmpdi imm2,imm0)
         __(cmpdi cr1,imm3,imm0)
         __(beq 2f)
         __(rotldi imm0,imm0,32)
         __(rotldi imm1,imm1,32)
         __(beq cr1,1f)
         __(li imm2,four_digit_bignum_header)
         __(Misc_Alloc_Fixed(arg_z,imm2,48))
         __(std imm1,misc_data_offset(arg_z))
         __(std imm0,misc_data_offset+8(arg_z))
         __(blr)
1:       __(li imm2,three_digit_bignum_header)
         __(Misc_Alloc_Fixed(arg_z,imm2,32))
         __(std imm1,misc_data_offset(arg_z))
         __(stw imm0,misc_data_offset+8(arg_z))
         __(blr)
2:       __(mr imm0,imm1)
         __(b _SPmakes64)        
        __else
         __(twgei r0,r0)
        __endif        
                        
_spentry(heap_restv_arg)

_spentry(req_heap_restv_arg)

_spentry(heap_cons_restv_arg)

	/* Restore current thread's interrupt level to arg_z,
	   noting whether the tcr's interrupt_pending flag was set. */
_spentry(restoreintlevel)
	__(cmpri(cr1,arg_z,0))
	__(ldr(imm0,tcr.interrupt_pending(rcontext)))
	__(cmpri(cr0,imm0,0))
	__(bne cr1,1f)
	__(beq cr0,1f)
	__(str(rzero,tcr.interrupt_pending(rcontext)))
	__(li nargs,fixnum_one)
	__(trgti(nargs,0))
	__(blr)
1:	
	__(str(arg_z,tcr.interrupt_level(rcontext)))
	__(blr)

/*
  Construct a lisp integer out of the 32-bit signed value in imm0
 */
_spentry(makes32)
        __ifdef([PPC64])
         __(box_fixnum(arg_z,imm0))
        __else
	 __(addo imm1,imm0,imm0)
	 __(addo. arg_z,imm1,imm1)
	 __(bnslr+)
	 __(mtxer rzero)
	 __(li imm1,one_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm1,node_size*2))
	 __(str(imm0,misc_data_offset(arg_z)))
        __endif
	 __(blr)

/*
  Construct a lisp integer out of the 32-bit unsigned value in imm0
 */
_spentry(makeu32)
        __ifdef([PPC64])
         __(box_fixnum(arg_z,imm0))
         __(blr)
        __else
	 __(clrrwi. imm1,imm0,31-nfixnumtagbits)
	 __(cmpri(cr1,imm0,0))
	 __(box_fixnum(arg_z,imm0))
	 __(beqlr cr0) /* A fixnum */
	 __(blt cr1,2f)
	 __(li imm2,one_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm2,8))
	 __(str(imm0,misc_data_offset(arg_z)))
	 __(blr)
2:
	 __(li imm2,two_digit_bignum_header)
	 __(Misc_Alloc_Fixed(arg_z,imm2,16))
	 __(str(imm0,misc_data_offset(arg_z)))
	 __(blr)
        __endif

/* 
  arg_z should be of type (SIGNED-BYTE 32); return unboxed result in imm0
*/
_spentry(gets32)
        __ifdef([PPC64])
         __(sldi imm1,arg_z,32-fixnumshift)
         __(extract_lisptag_(imm0,arg_z))
         __(sradi imm1,imm1,32-fixnumshift)
         __(box_fixnum(imm0,arg_z))
         __(cmpd cr1,imm1,arg_z)
         __(bne cr0,9f)
         __(beqlr cr1)
         __(b 9f)
        __else
	 __(extract_typecode(imm1,arg_z))
	 __(cmpri(cr0,imm1,tag_fixnum))
	 __(cmpri(cr2,imm1,subtag_bignum))
	 __(unbox_fixnum(imm0,arg_z))
	 __(beqlr+ cr0)
	 __(bne cr2,9f)
	 __(getvheader(imm1,arg_z))
	 __(cmpri(cr1,imm1,one_digit_bignum_header))
	 __(vrefr(imm0,arg_z,0))
	 __(beqlr+ cr1)
        __endif
9:
	__(uuo_interr(error_object_not_signed_byte_32,arg_z))

/* 
  arg_z should be of type (UNSIGNED-BYTE 32); return unboxed result in imm0
*/

_spentry(getu32)
	__(extract_typecode(imm1,arg_z))
	__(cmpri(cr0,imm1,tag_fixnum))
	__(cmpri(cr1,arg_z,0))
	__(cmpri(cr2,imm1,subtag_bignum))
	__(unbox_fixnum(imm0,arg_z))
	__(bne cr0,8f)
	__(bgelr cr1)
8:
	__(bne- cr2,9f)
	__(getvheader(imm2,arg_z))
	__(cmpri(cr2,imm2,two_digit_bignum_header))
	__(vrefr(imm0,arg_z,0))
	__(cmpri(cr0,imm0,0))
	__(bgt cr2,9f)
	__(beq cr2,2f)
	__(blt cr0,9f)
	__(blr)
2:
	__(vrefr(imm1,arg_z,1))
	__(cmpri(cr0,imm1,0))
	__(beqlr+ cr0)

9:
	__(uuo_interr(error_object_not_unsigned_byte_32,arg_z))

/*
  arg_z has overflowed (by one bit) as the result of an addition or subtraction.
  Make a bignum out of it.
*/
_spentry(fix_overflow)
	__(mtxer rzero)
	__(unbox_fixnum(imm1,arg_z))
        __ifdef([PPC64])
	 __(li imm0,two_digit_bignum_header)
         __(rotldi imm1,imm1,32)
	 __(xoris imm1,imm1,0xe000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,16))
	 __(str(imm1,misc_data_offset(arg_z)))
        __else
	 __(li imm0,one_digit_bignum_header)
	 __(xoris imm1,imm1,0xc000)
	 __(Misc_Alloc_Fixed(arg_z,imm0,8))
	 __(str(imm1,misc_data_offset(arg_z)))
        __endif
	__(blr)
		


/*
        As per mvpass above, but in this case fname is known to be a
        symbol.
*/
_spentry(mvpasssym)
	__(cmpri(cr0,nargs,node_size*nargregs))
	__(mflr loc_pc)
	__(mr imm0,vsp)
	__(ble+ cr0,1f)
	 __(subi imm0,imm0,node_size*nargregs)
	 __(add imm0,imm0,nargs)
1:            
	__(build_lisp_frame(fn,loc_pc,imm0))
	__(ref_global(loc_pc,ret1val_addr))
	__(li fn,0)
	__(mtlr loc_pc)
	__(jump_fname())

        
/* on entry:  temp0 = svar.  On exit, arg_z = value (possibly unbound_marker),
        arg_y = symbol, imm3 = svar.index */
_spentry(svar_specref)
        __(ldr(imm3,svar.idx(temp0)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldr(arg_y,svar.symbol(temp0)))
        __(cmpr(imm3,imm0))
        __(bge 1f)
        __(ldrx(arg_z,imm2,imm3))
        __(cmpri(arg_z,no_thread_local_binding_marker))
        __(bnelr)
1:     	__(ldr(arg_z,symbol.vcell(arg_y)))
        __(blr)

_spentry(svar_specrefcheck)
        __(ldr(imm3,svar.idx(temp0)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldr(arg_y,svar.symbol(temp0)))
        __(cmpr(imm3,imm0))
        __(bge 1f)
        __(ldrx(arg_z,imm2,imm3))
        __(cmpri(arg_z,no_thread_local_binding_marker))
        __(bne 2f)
1:     	__(ldr(arg_z,symbol.vcell(arg_y)))
2:      __(treqi(arg_z,unbound_marker))
        __(blr)

/* This never affects the symbol's vcell */
_spentry(svar_bind)
0:              
        __(ldr(imm3,svar.idx(temp0)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(beq 9f)
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(arg_z,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:
        __(ldr(arg_z,svar.symbol(temp0)))
        __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)

_spentry(svar_bind_self)
0:              
        __(ldr(imm3,svar.idx(temp0)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(cmpri(cr1,temp1,no_thread_local_binding_marker))
        __(ldr(arg_y,svar.symbol(temp0)))
        __(beq 9f)
        __(mr arg_z,temp1)
        __(bne cr1,1f)
        __(ldr(arg_z,symbol.vcell(arg_y)))
1:              
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(arg_z,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:      __(ldr(arg_z,svar.symbol(temp0)))
        __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)

_spentry(svar_bind_nil)
0:              
        __(ldr(imm3,svar.idx(temp0)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(beq- 9f)
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(li imm0,nil_value)
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(imm0,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:      __(ldr(arg_z,svar.symbol(temp0)))
        __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)
                	
_spentry(svar_bind_self_boundp_check)
        __(ldr(imm3,svar.idx(temp0)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(beq 9f)              /* no real tlb index */
        __(cmpri(temp1,no_thread_local_binding_marker))
        __(mr arg_z,temp1)
        __(bne 1f)
        __(ldr(arg_y,svar.symbol(temp0)))
        __(ldr(arg_z,symbol.vcell(arg_y)))
1:      __(treqi(arg_z,unbound_marker))       
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(arg_z,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:      __(ldr(arg_z,svar.symbol(temp0)))
        __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)

_spentry(svar_unbind)
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))   
        __(ldr(imm3,binding.sym(imm1)))
        __(ldr(temp1,binding.val(imm1)))
        __(ldr(imm1,binding.link(imm1)))
        __(strx(temp1,imm2,imm3))
        __(str(imm1,tcr.db_link(rcontext)))
        __(blr)

_spentry(svar_unbind_n)
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))   
1:      __(subi imm0,imm0,1)
        __(ldr(imm3,binding.sym(imm1)))
        __(ldr(temp1,binding.val(imm1)))
        __(cmpri(imm0,0))
        __(ldr(imm1,binding.link(imm1)))
        __(strx(temp1,imm2,imm3))
        __(bne 1b)
        __(str(imm1,tcr.db_link(rcontext)))
        __(blr)

 /*
   Clobbers imm1,imm2,imm5,arg_x, arg_y
*/
_spentry(svar_unbind_to)
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
1:      __(ldr(imm5,binding.sym(imm1)))
        __(ldr(arg_y,binding.val(imm1)))
        __(ldr(imm1,binding.link(imm1)))
        __(cmpr(imm0,imm1))
        __(strx(arg_y,imm2,imm5))
        __(bne 1b)
        __(str(imm1,tcr.db_link(rcontext)))
        __(blr)
	
/* temp0 = svar for special symbol, arg_z = new value. */        
_spentry(svar_specset)
        __(ldr(imm3,svar.idx(temp0)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldr(arg_y,svar.symbol(temp0)))
        __(cmpr(imm3,imm0))
        __(bge 1f)
        __(ldrx(temp1,imm2,imm3))
        __(cmpri(temp1,no_thread_local_binding_marker))
        __(beq 1f)
        __(strx(arg_z,imm2,imm3))
        __(blr)
1:     	__(mr arg_x,arg_y)
        __(li arg_y,symbol.vcell-misc_data_offset)
        __(b _SPgvset)


_spentry(svar_setqsym)
        __(ldr(arg_y,svar.symbol(temp0)))
	__(ldr(imm0,symbol.flags(arg_y)))
	__(andi. imm0,imm0,sym_vbit_const_mask)
	__(beq _SPsvar_specset)
	__(mr arg_z,arg_y)
	__(lwi(arg_y,XCONST))
	__(set_nargs(2))
	__(b _SPksignalerr)

_spentry(svar_progvsave)
	/* Error if arg_z isn't a proper list.  That's unlikely,
	   but it's better to check now than to crash later.
	*/
	__(cmpri(arg_z,nil_value))
	__(mr arg_x,arg_z)	/* fast */
	__(mr temp1,arg_z)	/* slow */
	__(beq 9f)		/* Null list is proper */
0:	
	__(trap_unless_list(arg_x,imm0))
	__(_cdr(temp2,arg_x))	/* (null (cdr fast)) ? */
	__(cmpri(cr3,temp2,nil_value))
	__(trap_unless_list(temp2,imm0,cr0))
	__(_cdr(arg_x,temp2))
	__(beq cr3,9f)
	__(_cdr(temp1,temp1))
	__(cmpr(arg_x,temp1))
	__(bne 0b)
	__(lwi(arg_y,XIMPROPERLIST))
	__(set_nargs(2))
	__(b _SPksignalerr)
9:	/* Whew */	
	
        /* Next, determine the length of arg_y.  We */
        /* know that it's a proper list. */
	__(li imm0,-node_size)
	__(mr arg_x,arg_y)
1:
	__(cmpri(cr0,arg_x,nil_value))
	__(la imm0,node_size(imm0))
	__(_cdr(arg_x,arg_x))
	__(bne 1b)
	/* imm0 is now (boxed) triplet count. */
	/* Determine word count, add 1 (to align), and make room. */
	/* if count is 0, make an empty tsp frame and exit */
	__(cmpri(cr0,imm0,0))
	__(add imm1,imm0,imm0)
	__(add imm1,imm1,imm0)
        __(dnode_align(imm1,imm1,node_size))
	__(bne+ cr0,2f)
	 __(TSP_Alloc_Fixed_Boxed(8))
	 __(blr)
2:
	__(la imm1,tsp_frame.fixed_overhead(imm1))	/* tsp header */
	__(TSP_Alloc_Var_Boxed_nz(imm1,imm2))
	__(str(imm0,tsp_frame.data_offset(tsp)))
	__(ldr(imm2,tsp_frame.backlink(tsp)))
	__(mr arg_x,arg_y)
	__(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(imm3,tcr.tlb_limit(rcontext)))
3:
        __(cmpri(cr1,arg_z,nil_value))
	__(_car(temp0,arg_x))
        __(ldr(imm0,svar.idx(temp0)))
	__(_cdr(arg_x,arg_x))
        __(trlle(imm3,imm0))
        __(ldr(imm4,tcr.tlb_pointer(rcontext))) /* Need to reload after trap */
        __(ldrx(temp3,imm4,imm0))
	__(cmpri(cr0,arg_x,nil_value))
        __(li temp2,unbound_marker)
        __(beq cr1,4f)
	__(_car(temp2,arg_z))
	__(_cdr(arg_z,arg_z))
4:      __(push(temp3,imm2))
	__(push(imm0,imm2))
	__(push(imm1,imm2))
        __(strx(temp2,imm4,imm0))
	__(mr imm1,imm2)
	__(bne cr0,3b)
	__(str(imm2,tcr.db_link(rcontext)))
	__(blr)
                
_spentry(svar_progvrestore)
	__(ldr(imm0,tsp_frame.backlink(tsp)))	/* ignore .SPnthrowXXX values frame */
	__(ldr(imm0,tsp_frame.data_offset(imm0)))
	__(cmpri(cr0,imm0,0))
	__(unbox_fixnum(imm0,imm0))
	__(bne+ cr0,_SPsvar_unbind_n)
	__(blr)
                        
/*  EOF, basically */
        .globl _SPsp_end
        b _SPsp_end
	_endfile
