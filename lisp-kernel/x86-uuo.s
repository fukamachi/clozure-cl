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

/* ud2b instructions can be followed by a single byte, which
   requests some functionality from the kernel uuo handler.
   ud2a instructions are used to signal errors;  they're
   generally going to be followed by a variable number of
   bytes which contain additional information about the
   error context.
*/
		
define([uuo2],[
	ud2b ; .byte $1
])

/* If we're allocating a CONS, the tcr's save_allocptr slot will be
   tagged as a cons.  Otherwise, it'll be tagged as fulltag_misc,
   and we have to look at the immediate registers to determine what's
   being allocated. */
define([uuo_alloc],[
	int [$]0xc4
])
				
