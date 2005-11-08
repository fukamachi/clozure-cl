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


ifdef([X8664],[
	include(x86-constants64.s)
],[
	include(x86-constants32.s)
])						

num_lisp_globals = 48		 /* MUST UPDATE THIS !!! */
	
	_struct(lisp_globals,lisp_globals_limit-(num_lisp_globals*node_size))
	 _node(initial_tcr)	        /* initial thread tcr */
	 _node(image_name)	        /* --image-name argument */
	 _node(BADfpscr_save_high)	        /* high word of FP reg used to save FPSCR */
	 _node(BADfpscr_save)              /* saved FPSCR */
	 _node(batch_flag)	        /* -b */
	 _node(host_platform)	        /* for runtime platform-specific stuff */
	 _node(argv)			/* address of argv[0] */
	 _node(errno)		        /* ADDRESS of errno */
	 _node(tenured_area) 		/* the tenured_area */
	 _node(oldest_ephemeral) 	/* dword address of oldest ephemeral object or 0 */
	 _node(lisp_exit_hook)		/* install foreign exception_handling */
	 _node(lisp_return_hook)	/* install lisp exception_handling */
	 _node(double_float_one) 	/* high half of 1.0d0 */
	 _node(short_float_zero) 	/* low half of 1.0d0 */
	 _node(doh_head) 		/* creole objects header */
	 _node(metering_info) 		/* address of lisp_metering global */
	 _node(in_gc) 			/* non-zero when GC active */
	 _node(lexpr_return1v) 		/* simpler when &lexpr called for single value. */
	 _node(lexpr_return) 		/* magic &lexpr return code. */
	 _node(all_areas) 		/* doubly-linked list of all memory areas */
	 _node(BAD_cs_overflow_limit) 	/* limit for control-stack overflow check */
	 _node(BAD_current_ts) 		/* current temp-stack area */
	 _node(BAD_current_vs) 		/* current value-stack area */
	 _node(statically_linked)	/* non-zero if -static */
	 _node(heap_end)                /* end of lisp heap */
	 _node(heap_start)              /* start of lisp heap */
	 _node(gcable_pointers)         /* linked-list of weak macptrs. */
	 _node(gc_num)                  /* fixnum: GC call count. */
	 _node(fwdnum)                  /* fixnum: GC "forwarder" call count. */
	 _node(altivec_present)         /* non-zero when AltiVec available */
	 _node(oldspace_dnode_count) 	/* dynamic dnodes older than g0 start */
	 _node(refbits) 		/* EGC refbits */
	 _node(gc_inhibit_count)
	 _node(intflag) 		/* sigint pending */
	 _node(BAD_block_tag_counter) 	/* counter for (immediate) block tag */
	 _node(BAD_go_tag_counter) 		
	 _node(exception_lock)
	 _node(area_lock)
	 _node(tcr_key) 		/* tsd key for per-thread tcr */
	 _node(ret1val_addr) 		/* address of "dynamic" subprims magic values return addr */
	 _node(subprims_base) 		/* address of dynamic subprims jump table */
	 _node(saveR13)			/* probably don]t really need this */
	 _node(saveTOC)                 /* where the 68K emulator stores the  emulated regs */
	 _node(tcr_lock)		/* this thread]s exception frame chain */
	 _node(kernel_imports) 		/* some things we need imported for us */
	 _node(interrupt_signal)	/* signal used by PROCESS-INTERRUPT */
	 _node(tcr_count) 		/* tcr_id for next tcr */
	 _node(get_tcr) 		/* address of get_tcr() */	
	_ends
	
	
uuo2_alloc_cons_function = 0
uuo2_alloc_variable_function = 1
		
