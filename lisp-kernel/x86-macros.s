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

/* to be consistent with PPC:
   - if an operation involves a register and memory, the
	register parameter precedes the memory parameter,
	regardless of the direction of transfer.
   - if an operation involves two registers, the destination
	precedes the source
*/
define([ref_global],[
	mov lisp_globals.$2,$1
])

define([set_global],[
	mov $1,lisp_globals.$2
])

define([ref_nrs_value],[
	mov nrs.$2+symbol.vcell,$1
])
	
define([set_nrs_value],[
	mov $1,nrs.$2+symbol.vcell
])
							
define([unbox_fixnum],[
	mov $2,$1
	sar [$]fixnumshift,$1
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
/* The instructions where tcr.save_allocptr is tagged are difficult
   to interrupt; the interrupting code has to recognize and possibly
   emulate the instructions in between */
	sub $cons.size+fulltag_cons,%rcontext:tcr.save_allocptr
	mov %rcontext:tcr.save_allocptr,%allocptr
	cmp %allocptr,%rcontext:tcr.save_allocbase
	jg macro_label(no_trap)
	uuo_alloc()
macro_label(no_trap):	
	andb $~fulltagmask,%rcontext:tcr.save_allocptr
/* Easy to interrupt now that tcr.save_allocptr isn't tagged as a cons */ 
	mov $3,cons.cdr(%allocptr)
	mov $2,cons.car(%allocptr)
	ifelse($1,[],[],[
	 mov %allocptr,$1
	])
])

/* The header has to be in %imm0, and the physical size in bytes has
   to be in %imm1. We bash %imm1. */

define([Misc_Alloc],[
	new_macro_labels()
	subq $fulltag_misc,%imm1
/* Here Be Monsters: we have to treat some/all of this instruction 
   sequence atomically, as soon as tcr.save_allocptr becomes tagged.
*/                
	subq %imm1,%rcontext:tcr.save_allocptr
	movq %rcontext:tcr.save_allocptr,%allocptr
	cmpq %allocptr,%rcontext:tcr.save_allocbase
	jg macro_label(no_trap)
	uuo_alloc()
macro_label(no_trap):	
	movq %imm0,misc_header_offset(%allocptr)
	andb $~fulltagmask,%rcontext:tcr.save_allocptr
/* Now that tcr.save_allocptr is untagged, it's easier to be interrupted */
	ifelse($1,[],[],[
	 mov %allocptr,$1
	])
])
	
define([Misc_Alloc_Fixed],[
	movq $2,%imm1
	Misc_Alloc($1)
])					

define([vrefr],[
	mov misc_data_offset+($3<<word_shift)($2),$1
])	

define([jump_nfn],[
	jmp *%nfn
])
			
define([jump_fname],[
	mov symbol.fcell(%fname),%nfn
	jump_nfn()
])	
	
define([set_nargs],[
	movw $1<<fixnumshift,%nargs
])

/* $1 = ndigits.  Assumes 4-byte digits */        
define([aligned_bignum_size],[((~(dnode_size-1)&(node_size+(dnode_size-1)+(4*$1))))])
	

define([_car],[
	mov $1,cons.car($2)
])	

define([tra],[
	.p2align 3
	ifelse($2,[],[
	.long $1-$2
	],[
	.long 0
	])
$1:
])
				
define([do_funcall],[
	new_macro_labels()
	lea macro_label(bad)(%rip),%nfn
	movb %temp0_b,%temp0_b
	andb $fulltagmask,%temp0_b
	cmpb $fulltag_symbol,%temp0_b
	/* %fname == %temp0 */
	cmoveq symbol.fcell(%fname),%nfn
	cmovgq %temp0,%nfn
	jmp *nfn
	tra(macro_label(bad))
])
