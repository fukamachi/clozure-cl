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

/* %arg_z has verflowed by one bit.  Make a bignum with 2 (32-bit) digits. */
_startfn(C(fix_one_bit_overflow))
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed([],aligned_bignum_size(2)))
	__(unbox_fixnum(%imm0,%arg_z))
	__(mov %temp0,%arg_z)
	__(xorq overflow_mask(%rip),%imm0)
	__(movq %imm0,misc_data_offset(%arg_z))
	__(jmp *%ra0)	
overflow_mask: 	.quad 0xe000000000000000
_endfn

/* %imm1:%imm0 constitute a signed integer, almost certainly a bignum.
   Make a lisp integer out of those 128 bits .. */
_startfn(C(makes128))
/* We're likely to have to make a bignum out of the integer in %imm1 and
   %imm0. We'll need to use %imm0 and %imm1 to cons the bignum, and
   will need to do some arithmetic (determining significant bigits)
   on %imm0 and %imm1 in order to know how large that bignum needs to be.
   Cache %imm0 and %imm1 in %xmm0 and %xmm1. */
   
	__(movd %imm0,%xmm0)
	__(movd %imm1,%xmm1)
	
/* If %imm1 is just a sign extension of %imm0, make a 64-bit signed integer. */
	
	__(sarq $63,%imm0) 
	__(cmpq %imm0,%imm1)
	__(movd %xmm0,%imm0)
	__(je _SPmakes64)
	
/* Otherwise, if the high 32 bits of %imm1 are a sign-extension of the
   low 32 bits of %imm1, make a 32-digit bignum.  If the upper 32 bits
   of %imm1 are significant, make a 4 digit bignum */
	__(movq %imm1,%imm0)
	__(shlq $32,%imm0)
	__(sarq $32,%imm0)
	__(cmpq %imm0,%imm1)
	__(jz 3f)
	__(mov $four_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(4)))
	__(movq %xmm0,misc_data_offset(%arg_z))
	__(movq %xmm1,misc_data_offset+8(%arg_z))
	__(jmp *%ra0)
3:	__(mov $three_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(3)))
	__(movq %mm0,misc_data_offset(%arg_z))
	__(movd %mm1,misc_data_offset+8(%arg_z))
	__(jmp *%ra0)
_endfn

/* Make a lisp integer (fixnum or two-digit bignum) from the signed
   64-bit value in %imm0.  Shift left 3 bits - a bit at a time, via 
   addition - and check for overlow after each add, since the overflow
   bit isn't cumulative on x86.
*/
_spentry(makes64)
	__(movq %imm0,%imm1)
	__(addq %imm1,%imm1)
	__(jo 1f)
	__(addq %imm1,%imm1)
	__(jo 1f)
	__(addq %imm1,%imm1)
	__(jo 1f)
	__(movq %imm1,%arg_z)
	__(jmp *%ra0)
1:	__(movq %imm0,%imm1)
	__(movq $two_digit_bignum_header,%imm0)
	__(Misc_Alloc_Fixed(%arg_z,aligned_bignum_size(2)))
	__(movq %imm1,misc_data_offset(%arg_z))
	__(jmp *%ra0)
_endsubp(makes64)	
				
/* %arg_z <- %arg_y + %arg_z.  Do the fixnum case - including overflow -
  inline.  Call out otherwise. */
_spentry(builtin_plus)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(addq %arg_y,%arg_z)
	__(jo C(fix_one_bit_overflow))
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
	__(jo C(fix_one_bit_overflow))
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
	__(unbox_fixnum(%imm0,%arg_z))
	/* 128-bit fixnum result in %imm1:%imm0. Overflow set if %imm1
	   is significant */
	__(imul %arg_y)
	__(jo 1f)
	__(mov %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(unbox_fixnum(%imm0,%arg_z))
	__(unbox_fixnum(%imm1,%arg_y))
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
	__(cmp %arg_z,%arg_y)
	__(mov $t_value,%arg_z)
	__(mov $nil_value,%imm0)
	__(cmovne %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_eq,2))
_endsubp(builtin_eq)
	
/* %arg_z <- (/= %arg_y %arg_z).	*/
_spentry(builtin_ne)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovne %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_ne,2))
_endsubp(builtin_ne)
	
/* %arg_z <- (> %arg_y %arg_z).	*/
_spentry(builtin_gt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovl %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_gt,2))
_endsubp(builtin_gt)

/* %arg_z <- (>= %arg_y %arg_z).	*/
_spentry(builtin_ge)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovge %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_ge,2))
_endsubp(builtin_ge)
	
/* %arg_z <- (< %arg_y %arg_z).	*/
_spentry(builtin_lt)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovl %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_lt,2))
_endsubp(builtin_lt)

/* %arg_z <- (<= %arg_y %arg_z). */
_spentry(builtin_le)
	__(movb %arg_z_b,%imm0_b)
	__(orb %arg_y_b,%imm0_b)
	__(testb $fixnummask,%imm0_b)
	__(jne 1f)
	__(cmp %arg_z,%arg_y)
	__(mov $nil_value+t_offset,%imm0)
	__(mov $nil_value,%arg_z)
	__(cmovle %imm0,%arg_z)
	__(jmp *%ra0)
1:	__(jump_builtin(_builtin_le,2))
_endsubp(builtin_le)

_spentry(builtin_eql)
_endsubp(builtin_eql)

_spentry(builtin_length)
_endsubp(builtin_length)

_spentry(builtin_seqtype)
_endsubp(builtin_seqtype)

_spentry(builtin_assq)
_endsubp(builtin_assq)	

_spentry(builtin_memq)
_endsubp(builtin_memq)

_spentry(builtin_logbitp)
_endsubp(builtin_logbitp)

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
	
	.globl C(popj)
C(popj):

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
_endsubp(jmpsym)

_spentry(jmpnfn)
_endsubp(jmpnfn)

_spentry(funcall)
_endsubp(funcall)

_spentry(mkcatch1v)
_endsubp(mkcatch1v)

_spentry(mkunwind)
_endsubp(mkunwind)

_spentry(mkcatchmv)
_endsubp(mkcatchmv)

_spentry(throw)
_endsubp(throw)

_spentry(nthrowvalues)
_endsubp(nthrowvalues)

_spentry(nthrow1value)
_endsubp(nthrow1value)

_spentry(bind)
_endsubp(bind)

_spentry(bind_self)
_endsubp(bind_self)

_spentry(bind_nil)
_endsubp(bind_nil)

_spentry(bind_self_boundp_check)
_endsubp(bind_self_boundp_check)

_spentry(rplaca)
_endsubp(rplaca)

_spentry(rplacd)
_endsubp(rplacd)

_spentry(conslist)
_endsubp(conslist)

_spentry(conslist_star)
_endsubp(conslist_star)

_spentry(stkconslist)
_endsubp(stkconslist)

_spentry(stkconslist_star)
_endsubp(stkconslist_star)

_spentry(mkstackv)
_endsubp(mkstackv)

_spentry(subtag_misc_ref)
_endsubp(subtag_misc_ref)

_spentry(setqsym)
_endsubp(setqsym)

_spentry(progvsave)
_endsubp(progvsave)

_spentry(stack_misc_alloc)
_endsubp(stack_misc_alloc)

_spentry(gvector)
_endsubp(gvector)

_spentry(mvpass)
_endsubp(mvpass)

_spentry(fitvals)
_endsubp(fitvals)

_spentry(nthvalue)
_endsubp(nthvalue)

_spentry(values)
_endsubp(values)

_spentry(default_optional_args)
_endsubp(default_optional_args)

_spentry(opt_supplied_p)
_endsubp(opt_supplied_p)

_spentry(heap_rest_arg)
_endsubp(heap_rest_arg)

_spentry(req_heap_rest_arg)
_endsubp(req_heap_rest_arg)

_spentry(heap_cons_rest_arg)
_endsubp(heap_cons_rest_arg)

_spentry(simple_keywords)
_endsubp(simple_keywords)

_spentry(keyword_args)
_endsubp(keyword_args)

_spentry(keyword_bind)
_endsubp(keyword_bind)

_spentry(poweropen_ffcall)
_endsubp(poweropen_ffcall)

_spentry(unused_0)
_endsubp(unused_0)

_spentry(ksignalerr)
_endsubp(ksignalerr)

_spentry(stack_rest_arg)
_endsubp(stack_rest_arg)

_spentry(req_stack_rest_arg)
_endsubp(req_stack_rest_arg)

_spentry(stack_cons_rest_arg)
_endsubp(stack_cons_rest_arg)

_spentry(poweropen_callbackX)
_endsubp(poweropen_callbackX)

/* Prepend all but the first three (2 words of code, inner fn) and last two */
/* (function name, lfbits) elements of %fn to the "arglist". */
_spentry(call_closure)
        __(subq $subtag_function-subtag_misc,%fn)
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
        __(movq %rsp,%temp0)
        __(push %arg_z)
        __(push %arg_y)
        __(push %arg_x)
        __(lea (%rsp,%imm0),%arg_x)
        __(lea -nargregs<<fixnumshift(%nargs_q),%arg_y)
local_label(copy_already_loop): 
        __(movq (%arg_x),%arg_z)
        __(addq $fixnumone,%arg_x)
        __(movq %arg_z,(%temp0))
        __(addq $fixnumone,%temp0)
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
           sum of %nargs and %imm0 is greater than nargregs */
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
        __(leaq $3<<fixnumshift(%imm0),%temp0)
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
        __(subq $node_size,%temp0)
        __(movq misc_data_offset(%fn,%temp0),%arg_x)
        __(addw $fixnumone,%nargs)
        __(subq $fixnumone,%imm0)
        __(jne local_label(vpush_remaining))
        __(jmp local_label(go))
	/* Maybe set arg_y or arg_z, preceding args */
local_label(set_y_z):
        __(jne local_label(set_arg_z))
	/* Set arg_y, maybe arg_x, preceding args */
local_label(set_arg_y): 
        __(subq $node_size,%temp0)
        __(movq misc_data_offset(%fn,%temp0),%arg_y)
        __(addw $fixnumone,%nargs)
        __(subq $fixnum_one,%imm0)
        __(jnz local_label(set_arg_x))
        __(jmp local_label(go))
local_label(set_arg_z): 
        __(subq $node_size,%temp0)
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

_spentry(spreadargz)
_endsubp(spreadargz)

_spentry(tfuncallgen)
_endsubp(tfuncallgen)

_spentry(tfuncallslide)
_endsubp(tfuncallslide)

_spentry(tfuncallvsp)
_endsubp(tfuncallvsp)

_spentry(tcallsymgen)
_endsubp(tcallsymgen)

_spentry(tcallsymslide)
_endsubp(tcallsymslide)

_spentry(tcallsymvsp)
_endsubp(tcallsymvsp)

_spentry(tcallnfngen)
_endsubp(tcallnfngen)

_spentry(tcallnfnslide)
_endsubp(tcallnfnslide)

_spentry(tcallnfnvsp)
_endsubp(tcallnfnvsp)

_spentry(misc_ref)
_endsubp(misc_ref)

_spentry(misc_set)
_endsubp(misc_set)

_spentry(stkconsyz)
_endsubp(stkconsyz)

_spentry(stkvcell0)
_endsubp(stkvcell0)

_spentry(stkvcellvsp)
_endsubp(stkvcellvsp)

_spentry(makestackblock)
_endsubp(makestackblock)

_spentry(makestackblock0)
_endsubp(makestackblock0)

_spentry(makestacklist)
_endsubp(makestacklist)

_spentry(stkgvector)
_endsubp(stkgvector)

_spentry(misc_alloc)
_endsubp(misc_alloc)

_spentry(poweropen_ffcallX)
_endsubp(poweropen_ffcallX)

_spentry(gvset)
_endsubp(gvset)

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

_spentry(integer_sign)
_endsubp(integer_sign)

_spentry(subtag_misc_set)
_endsubp(subtag_misc_set)

_spentry(spread_lexprz)
_endsubp(spread_lexprz)

_spentry(store_node_conditional)
_endsubp(store_node_conditional)

_spentry(reset)
_endsubp(reset)

_spentry(mvslide)
_endsubp(mvslide)

_spentry(save_values)
_endsubp(save_values)

_spentry(add_values)
_endsubp(add_values)

_spentry(poweropen_callback)
_endsubp(poweropen_callback)

_spentry(misc_alloc_init)
_endsubp(misc_alloc_init)

_spentry(stack_misc_alloc_init)
_endsubp(stack_misc_alloc_init)

_spentry(set_hash_key)
_endsubp(set_hash_key)

_spentry(unused_1)
_endsubp(unused_1)

_spentry(callbuiltin)
_endsubp(callbuiltin)

_spentry(callbuiltin0)
_endsubp(callbuiltin0)

_spentry(callbuiltin1)
_endsubp(callbuiltin1)

_spentry(callbuiltin2)
_endsubp(callbuiltin2)

_spentry(callbuiltin3)
_endsubp(callbuiltin3)

_spentry(popj)
_endsubp(popj)

_spentry(restorefullcontext)
_endsubp(restorefullcontext)

_spentry(savecontextvsp)
_endsubp(savecontextvsp)

_spentry(savecontext0)
_endsubp(savecontext0)

_spentry(restorecontext)
_endsubp(restorecontext)

_spentry(lexpr_entry)
_endsubp(lexpr_entry)

_spentry(poweropen_syscall)
_endsubp(poweropen_syscall)

_spentry(builtin_logior)
_endsubp(builtin_logior)

_spentry(builtin_logand)
_endsubp(builtin_logand)

_spentry(builtin_ash)
_endsubp(builtin_ash)

_spentry(builtin_negate)
_endsubp(builtin_negate)

_spentry(builtin_logxor)
_endsubp(builtin_logxor)

_spentry(builtin_aref1)
_endsubp(builtin_aref1)

_spentry(builtin_aset1)
_endsubp(builtin_aset1)

_spentry(breakpoint)
_endsubp(breakpoint)

_spentry(eabi_ff_call)
_endsubp(eabi_ff_call)

_spentry(eabi_callback)
_endsubp(eabi_callback)

_spentry(eabi_syscall)
_endsubp(eabi_syscall)

_spentry(getu64)
_endsubp(getu64)

_spentry(gets64)
_endsubp(gets64)

_spentry(makeu64)
_endsubp(makeu64)

_spentry(specref)
_endsubp(specref)

_spentry(specset)
_endsubp(specset)

_spentry(specrefcheck)
_endsubp(specrefcheck)

_spentry(restoreintlevel)
_endsubp(restoreintlevel)

_spentry(makes32)
_endsubp(makes32)

_spentry(makeu32)
_endsubp(makeu32)

_spentry(gets32)
_endsubp(gets32)

_spentry(getu32)
_endsubp(getu32)

_spentry(fix_overflow)
_endsubp(fix_overflow)

_spentry(mvpasssym)
_endsubp(mvpasssym)

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

_spentry(unbind_interrupt_level)
_endsubp(unbind_interrupt_level)

_spentry(unbind)
_endsubp(unbind)

_spentry(unbind_n)
_endsubp(unbind_n)

_spentry(unbind_to)
_endsubp(unbind_to)

_spentry(bind_interrupt_level_m1)
_endsubp(bind_interrupt_level_m1)

_spentry(bind_interrupt_level)
_endsubp(bind_interrupt_level)

_spentry(bind_interrupt_level_0)
_endsubp(bind_interrupt_level_0)

_spentry(progvrestore)
_endsubp(progvrestore)
	

