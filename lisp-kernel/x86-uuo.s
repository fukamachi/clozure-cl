dnl
dnl   Copyright (C) 2005 Clozure Associates
dnl   This file is part of OpenMCL.  
dnl
dnl   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
dnl   License , known as the LLGPL and distributed with OpenMCL as the
dnl   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
dnl   which is distributed with OpenMCL as the file "LGPL".  Where these
dnl   conflict, the preamble takes precedence.  

dnl   OpenMCL is referenced in the preamble as the "LIBRARY."

dnl   The LLGPL is also available online at
dnl   http://opensource.franz.com/preamble.html


define([uuo_error_too_few_args],[
        int [$]0xc0
])

define([uuo_error_too_many_args],[
        int [$]0xc1
])

define([uuo_error_wrong_number_of_args],[
        int [$]0xc2
])

define([uuo_error_stack_overflow],[
        int [$]0xc3
])

define([uuo_error_gc_trap],[
        int [$]0xc4
])                        
        
                                        
dnl If we're allocating a CONS, the tcr's save_allocptr slot will be
dnl tagged as a cons.  Otherwise, it'll be tagged as fulltag_misc,
dnl and we have to look at the immediate registers to determine what's
dnl being allocated.
define([uuo_alloc],[
	int [$]0xc5
])
				
define([uuo_error_not_callable],[
        int [$]0xc6
])


define([xuuo],[
	ud2a
	.byte $1
])
	
define([tlb_too_small],[
	xuuo(1)
])

define([interrupt_now],[
	xuuo(2)
])		

define([uuo_error_reg_not_fixnum],[
	int [$]0xf0|$1
])	
	
define([uuo_error_reg_not_list],[
	int [$]0xe0|$1
])

define([uuo_error_reg_not_tag],[
	int [$]0xd0|$1
	.byte $2
])			

define([uuo_error_reg_not_type],[
	int [$]0xb0|$1
	.byte $2
])

define([uuo_error_reg_not_fixnum],[
	int [$]0xf0|$1
])	
		
define([uuo_error_reg_unbound],[
	int [$]0x90|$1
])	

define([uuo_error_vector_bounds],[
	int [$]0xc8
	.byte ($1<<4)|($2)
])	

