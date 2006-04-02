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

/* Flush %rsi cache lines, starting at address in %rdi.  Each line is
   assumed to be %rdx bytes wide. */
_exportfn(C(flush_cache_lines))
	__(cmpq $0,%rsi)
	__(jmp 2f)
1:	__(clflush (%rdi))
	__(addq %rdx,%rdi)
	__(subq $1,%rsi)
2:	__(jg 1b)	
	__(ret)
_endfn

_exportfn(C(current_stack_pointer))
	__(movq %rsp,%rax)
	__(ret)
_endfn

_exportfn(C(touch_page))
        __(movq %rdi,(%rdi))
        __(movq $0,(%rdi))
        __(movl $1,%eax)
        .globl C(touch_page_end)
C(touch_page_end):	
        __(ret)
                        
_exportfn(C(count_leading_zeros))
	__(bsrq %rdi,%rax)
	__(xorq $63,%rax)
	__(ret)
_endfn

_exportfn(C(noop))
	__(retq)
_endfn

/* Do we need to set FP control modes in the x87 FPU ? Or just SSE2 ? */
_exportfn(C(set_fpscr))
_endfn
	
_exportfn(C(save_fp_context))
_endfn

_exportfn(C(restore_fp_context))
_endfn


/*
  Atomically store new value (%rdx) in *%rdi, if old value == %rsi.
  Return actual old value.
*/
_exportfn(C(store_conditional))
	__(mov %rsi,%rax)
	__(lock cmpxchgq %rdx,(%rdi))
	__(cmovne %rdx,%rax)
	__(ret)	
_endfn

/*
	Atomically store new_value(%rsi) in *%rdi ;  return previous contents
	of *%rdi.
*/
_exportfn(C(atomic_swap))
	__(lock xchg %rsi,(%rdi))
	__(mov %rsi,%rax)
	__(ret)
_endfn

/*
        Logior the value in *%rdi with the value in %rsi (presumably a
	bitmask with exactly 1 bit set.)  Return non-zero if any of
	the bits in that bitmask were already set.
*/        
_exportfn(C(atomic_ior))
0:	__(movq (%rdi),	%rax)
	__(movq %rax,%rcx)
	__(orq %rsi,%rcx)
	__(lock cmpxchg %rcx,(%rdi))
        __(jnz 0b)
	__(andq %rsi,%rax)
	__(ret)
_endfn
        
        



/* int cpuid (int code, int *pebx, int *pecx, int *pedx) 
	          %rdi,     %rsi,      %rdx,      %rcx    	    
*/
_exportfn(C(cpuid))
	__(pushq %rdx)		/* pecx */
	__(pushq %rcx)		/* pedx */
	__(pushq %rbx)		/* %rbx is non-volatile */
	__(movq %rdi,%rax)
	__(cpuid)
	__(movl %ebx,(%rsi))
	__(popq %rbx)
	__(popq %rsi)		 /* recover pedx */
	__(movl %edx,(%rsi))
	__(popq %rsi)		/* recover pecx */
	__(movl %ecx,(%rsi))
	__(ret)
_endfn

/* switch_to_foreign_stack(new_sp, func, arg_0, arg_1, arg_2, arg_3) 
   Not fully general, but should get us off of the signal stack */	
_exportfn(C(switch_to_foreign_stack))
	__(movq %rdi,%rsp)
	__(movq %rsi,%rax)
	__(movq %rdx,%rdi)
	__(movq %rcx,%rsi)
	__(movq %r8,%rdx)
	__(movq %r9,%rcx)
	__(jmp *%rax)
_endfn
		
_exportfn(C(get_vector_registers))
_endfn

_exportfn(C(put_vector_registers))
_endfn				
			
	_endfile
