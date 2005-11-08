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


define([ref_global],[
	mov lisp_globals.$1,$2
])

define([set_global],[
	mov $1,lisp_globals.$2
])
						
define([unbox_fixnum],[
	mov $1,$2
	sar [$]fixnumshift,$2
])

define([save_node_regs],[
	push %arg_z
	push %arg_y
	push %arg_x
	push %temp0
	push %temp1
	push %temp2
	push %save0
	push %save1
	push %save2
	push %save3
	push %nfn
	push %fn
])

/* This needs to be done before we transition back to the lisp stack
   from the foreign stack. */
		
define([zero_node_regs],[
	xor %fn,%fn
	mov %fn,%nfn
	mov %fn,%save3
	mov %fn,%save2
	mov %fn,%save1
	mov %fn,%save0
	mov %fn,%temp2
	mov %fn,%temp1
	mov %fn,%temp0
	mov %fn,%arg_x
	mov %fn,%arg_y
	mov %fn,arg_z
])	
define([restore_node_regs],[
	pop %fn
	pop %nfn
	pop %save3
	pop %save2
	pop %save1
	pop %save0
	pop %temp2
	pop %temp1
	pop %temp0
	pop %arg_x
	pop %arg_y
	pop %arg_z
])	

/* Consing can get interrupted (either by PROCESS-INTERRUPT or by GC
   activity in some other thread; if it's interrupted, the interrupting
   process needs to be able to determine what's going on well enough
   to be able to either back out of the attempt or finish the job.
   That requires that we use easily recogninized instruction sequences
   and follow certain conventions when consing (either in the kernel
   or in compiled code.)  (One of those conventions involves using
   %allocptr = %temp0 as a freepointer; when consing, %temp0 can't
   contain a live value.)
   Making a CONS cell is a little simpler than making a uvector.
*/	
define([Cons],[
	new_macro_labels()
	sub $cons.size+fulltag_cons,%rcontext:tcr.save_allocptr
	mov %rcontext:tcr.save_allocptr,%allocptr
	cmp %allocptr,%rcontext:tcr.save_allocbase
	jgt macro_label(no_trap)
	uuo2_alloc_cons()
macro_label(no_trap):	
	mov $3,cons.cdr(%allocptr)
	mov $2,cons.car(%allocptr)
	mov %allocptr,$1
	andq $~fulltagmask,%rcontext:tcr.save_allocptr
])

/* The header has to be in %imm0, and the physical size in bytes has
   to be in %imm1. We bash %imm1. */

define([Misc_Alloc],[
	new_macro_labels()
	leaq -fulltag_misc(%imm1),%imm1
	sub $imm1,%rcontext:tcr.save_allocptr
	mov %rcontext:tcr.save_allocptr,%allocptr
	cmp %allocptr,%rcontext:tcr.save_allocbase
	jgt macro_label(no_trap)
	uuo2_alloc_variable()
macro_label(no_trap):	
	movq %imm0,misc_header_offset(%allocptr)
	movq %allocptr,$1
	andq $~fulltagmask,%rcontext:tcr.save_allocptr
])
	
define([Misc_Alloc_Fixed],[
	movq $2,%imm1
	Misc_Alloc($1)
])					
