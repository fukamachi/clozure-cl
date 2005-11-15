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

#include "lisp.h"
#include "lisp-exceptions.h"
#include "lisp_globals.h"
#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>
#ifdef LINUX
#include <strings.h>
#include <sys/mman.h>
#include <fpu_control.h>
#include <linux/prctl.h>
#endif

int
page_size = 8192;

void
install_signal_handler(int signo, __sighandler_t handler)
{
  struct sigaction sa;
  
  sa.sa_sigaction = (void *)handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = 
    SA_RESTART
    | SA_SIGINFO;

  sigaction(signo, &sa, NULL);
}

void
enable_fp_exceptions()
{
}

void
exception_init()
{
}

void
adjust_exception_pc(ExceptionInformation *xp, int delta)
{
  xpPC(xp) += delta;
}

void
restore_soft_stack_limit(unsigned stkreg)
{
}

/* Maybe this'll work someday.  We may have to do something to
   make the thread look like it's not handling an exception */
void
reset_lisp_process(ExceptionInformation *xp)
{
}
