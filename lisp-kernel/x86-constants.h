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

#ifndef __x86_constants__
#define __x86_constants__ 1

#define TCR_FLAG_BIT_FOREIGN fixnumshift
#define TCR_FLAG_BIT_AWAITING_PRESET (fixnumshift+1)
#define TCR_FLAG_BIT_ALT_SUSPEND (fixnumshift+2)
#define TCR_FLAG_BIT_PROPAGATE_EXCEPTION (fixnumshift+3)
#define TCR_FLAG_BIT_SUSPEND_ACK_PENDING (fixnumshift+4)
#define TCR_FLAG_BIT_PENDING_EXCEPTION (fixnumshift+5)

#define TCR_STATE_FOREIGN (1)
#define TCR_STATE_LISP    (0)
#define TCR_STATE_EXCEPTION_WAIT (2)
#define TCR_STATE_EXCEPTION_RETURN (4)

#ifdef X8664
#include "x86-constants64.h"
#else
#include "x86-constants32.h"
#endif

#define dnode_size (node_size*2)
#define dnode_shift node_shift+1

#define INTERRUPT_LEVEL_BINDING_INDEX (1)

#endif /* __x86_constants__ */

