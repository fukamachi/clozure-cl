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

#include "lisp-exceptions.h"
#include "lisp_globals.h"
#include "area.h"
#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>

#include <sys/socket.h>
#include <sys/stat.h>

int
running_detached()
{
  struct stat dev_null_stat, fd_zero_stat;

  if (stat("/dev/null", &dev_null_stat) == 0) {
    if (fstat(0, &fd_zero_stat) == 0) {
      return (dev_null_stat.st_ino == fd_zero_stat.st_ino);
    }
  }
  return 0;
}


int
readc()
{
  int c;
  while (1) {
    c = getchar();
    switch(c) {
    case '\n':
      continue;
    case EOF:
      if (ferror(stdin)) {
	if (errno == EINTR) {
	  continue;
	}
      }
      /* fall through */
    default:
      return c;
    }
  }
}

void
show_lisp_register(ExceptionInformationPowerPC *xp, char *label, int r)
{
  extern char *print_lisp_object(LispObj);

  LispObj val = xpGPR(xp, r);

  fprintf(stderr, "r%02d (%s) = %s\n", r, label, print_lisp_object(val));
}


void
debug_lisp_registers(ExceptionInformationPowerPC *xp)
{
  int xpcontext = (int) xpGPR(xp, rcontext);

  fprintf(stderr, "rcontext = 0x%08X ", xpcontext);
  if (!active_tcr_p((TCR *)xpcontext)) {
    fprintf(stderr, "(INVALID)\n");
  } else {
    fprintf(stderr, "\nnargs = %d\n", xpGPR(xp, nargs) >> 2);
    show_lisp_register(xp, "fn", fn);
    show_lisp_register(xp, "arg_z", arg_z);
    show_lisp_register(xp, "arg_y", arg_y);
    show_lisp_register(xp, "arg_x", arg_x);
    show_lisp_register(xp, "temp0", temp0);
    show_lisp_register(xp, "temp1/next_method_context", temp1);
    show_lisp_register(xp, "temp2/nfn", temp2);
    show_lisp_register(xp, "temp3/fname", temp3);
    show_lisp_register(xp, "temp4", temp4);
    show_lisp_register(xp, "save0", save0);
    show_lisp_register(xp, "save1", save1);
    show_lisp_register(xp, "save2", save2);
    show_lisp_register(xp, "save3", save3);
    show_lisp_register(xp, "save4", save4);
    show_lisp_register(xp, "save5", save5);
    show_lisp_register(xp, "save6", save6);
    show_lisp_register(xp, "save7", save7);
  }
}
    
void
debug_show_registers(ExceptionInformationPowerPC *xp)
{
  int a, b, c, d, i;

  for (a = 0, b = 8, c = 16, d = 24; a < 8; a++, b++, c++, d++) {
    fprintf(stderr,"r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X\n",
	    a, xpGPR(xp, a),
	    b, xpGPR(xp, b),
	    c, xpGPR(xp, c),
	    d, xpGPR(xp, d));
  }
  fprintf(stderr, "\n PC = 0x%08X   LR = 0x%08X  CTR = 0x%08X  CCR = 0x%08X\n",
	  xpPC(xp), xpLR(xp), xpCTR(xp), xpCCR(xp));
  fprintf(stderr, "XER = 0x%08X  MSR = 0x%08X  DAR = 0x%08X  DSISR = 0x%08X\n",
	  xpXER(xp), xpMSR(xp), xpDAR(xp), xpDSISR(xp));
}

void
debug_show_fpu(ExceptionInformationPowerPC *xp)
{
  double *dp, d;
  int *np, n, i;

  dp = xpFPRvector(xp);
  np = (int *) dp;
  
  for (i = 0; i < 32; i++) {
    fprintf(stderr, "f%02d : 0x%08X%08X (%f)\n", i,  *np++, *np++, *dp++);
  }
  fprintf(stderr, "FPSCR = %08X\n", xpFPSCR(xp));

}

void
debug_backtrace(ExceptionInformationPowerPC *xp)
{
  extern LispObj current_stack_pointer();
  extern void plbt_sp(LispObj);
  extern void plbt(ExceptionInformationPowerPC *);

  if (xp) {
    plbt(xp);
  } else {
    plbt_sp(current_stack_pointer());
  }
}

extern pid_t main_thread_pid;

void
lisp_Debugger(ExceptionInformationPowerPC *xp, char *message)
{
  suspend_other_threads();
  fprintf(stderr, "%s\n", message);
  if (lisp_global(BATCH_FLAG)) {
    terminate_lisp();
  }
  while (1) {
    fprintf(stderr,"\n[Process ID = %d]\n", main_thread_pid);
    fprintf(stderr,"\n(R)egisters, (L)isp registers (F)loating-point, (B)acktrace (K)ill, e(X)it ?");
    switch(readc()) {
    case 'R': case 'r':
      if (xp) {
	debug_show_registers(xp);
      }
      break;
    case 'L': case 'l':
      if (xp) {
	debug_lisp_registers(xp);
      }
      break;
    case 'F': case 'f':
      if (xp) {
	debug_show_fpu(xp);
      }
      break;
    case 'B': case 'b':
      debug_backtrace(xp);
      break;
    case 'K': case 'k': case EOF:
      terminate_lisp();
    case 'X': case 'x':
      resume_other_threads();
      return;
    }
  }
}

