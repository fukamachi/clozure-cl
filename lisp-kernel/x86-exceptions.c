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
#include "Threads.h"
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
page_size = 4096;

int
log2_page_size = 12;




int
gc_from_xp(ExceptionInformation *xp, signed_natural param)
{
  Bug(xp, "GC ?  Not yet ...");

}

void
update_bytes_allocated(TCR* tcr, void *cur_allocptr)
{
  BytePtr 
    last = (BytePtr) tcr->last_allocptr, 
    current = (BytePtr) cur_allocptr;
  if (last && (cur_allocptr != ((void *)VOID_ALLOCPTR))) {
    tcr->bytes_allocated += last-current;
  }
  tcr->last_allocptr = 0;
}


/*
  This doesn't GC; it returns true if it made enough room, false
  otherwise.
  If "extend" is true, it can try to extend the dynamic area to
  satisfy the request.
*/

Boolean
new_heap_segment(ExceptionInformation *xp, natural need, Boolean extend, TCR *tcr)
{
  area *a;
  natural newlimit, oldlimit;
  natural log2_allocation_quantum = tcr->log2_allocation_quantum;

  a  = active_dynamic_area;
  oldlimit = (natural) a->active;
  newlimit = (align_to_power_of_2(oldlimit, log2_allocation_quantum) +
	      align_to_power_of_2(need, log2_allocation_quantum));
  if (newlimit > (natural) (a->high)) {
    if (extend) {
      if (! resize_dynamic_heap(a->active, (newlimit-oldlimit)+lisp_heap_gc_threshold)) {
        return false;
      }
    } else {
      return false;
    }
  }
  a->active = (BytePtr) newlimit;
  tcr->last_allocptr = (void *)newlimit;
  tcr->save_allocptr = (void *)newlimit;
  xpGPR(xp,Iallocptr) = (LispObj) newlimit;
  tcr->save_allocbase = (void *) oldlimit;

  while (HeapHighWaterMark < (BytePtr)newlimit) {
    zero_page(HeapHighWaterMark);
    HeapHighWaterMark+=page_size;
  }
  return true;
}

Boolean
allocate_object(ExceptionInformation *xp,
                natural bytes_needed, 
                signed_natural disp_from_allocptr,
		TCR *tcr)
{
  area *a = active_dynamic_area;

  /* Maybe do an EGC */
  if (a->older && lisp_global(OLDEST_EPHEMERAL)) {
    if (((a->active)-(a->low)) >= a->threshold) {
      gc_from_xp(xp, 0L);
    }
  }

  /* Life is pretty simple if we can simply grab a segment
     without extending the heap.
  */
  if (new_heap_segment(xp, bytes_needed, false, tcr)) {
    xpGPR(xp, Iallocptr) -= disp_from_allocptr;
    tcr->save_allocptr = (void *) (xpGPR(xp, Iallocptr));
    return true;
  }
  
  /* It doesn't make sense to try a full GC if the object
     we're trying to allocate is larger than everything
     allocated so far.
  */
  if ((lisp_global(HEAP_END)-lisp_global(HEAP_START)) > bytes_needed) {
    untenure_from_area(tenured_area); /* force a full GC */
    gc_from_xp(xp, 0L);
  }
  
  /* Try again, growing the heap if necessary */
  if (new_heap_segment(xp, bytes_needed, true, tcr)) {
    xpGPR(xp, Iallocptr) -= disp_from_allocptr;
    tcr->save_allocptr = (void *) (xpGPR(xp, Iallocptr));
    return true;
  }
  
  return false;
}

Boolean
handle_alloc_trap(ExceptionInformation *xp, TCR *tcr)
{
  natural cur_allocptr, bytes_needed;
  unsigned allocptr_tag;
  signed_natural disp;
  
  cur_allocptr = xpGPR(xp,Iallocptr);
  allocptr_tag = fulltag_of(cur_allocptr);
  if (allocptr_tag == fulltag_misc) {
    disp = xpGPR(xp,Iimm1);
  } else {
    disp = dnode_size-fulltag_cons;
  }
  bytes_needed = disp+allocptr_tag;

  update_bytes_allocated(tcr,((BytePtr)(cur_allocptr-disp)));
  if (allocate_object(xp, bytes_needed, disp, tcr)) {
    return true;
  }

  return false;
}

Boolean
handle_exception(int signum, siginfo_t *info, ExceptionInformation  *context, TCR *tcr)
{
  pc program_counter = (pc)xpPC(context);

  if (signum == SIGSEGV) {
    if ((info->si_addr) == 0) {
      /* Something mapped to SIGSEGV that has nothing to do with
	 a memory fault */
      if (*program_counter == 0xcd) {	/* an int instruction */
	program_counter++;
	if (*program_counter == 0xc5) {
	  if (handle_alloc_trap(context, tcr)) {
	    xpPC(context) = (natural) (program_counter+1);
	    return true;
	  }
	}
	if (*program_counter == 0xca) {
	  lisp_Debugger(context, info, debug_entry_dbg, "Lisp Breakpoint");
	    xpPC(context) = (natural) (program_counter+1);
	    return true;
	}
      }
    }
  }
  return false;
}

void
signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  xframe_list xframe_link;
  int old_valence;
  TCR* tcr = get_tcr(false);

  if (! handle_exception(signum, info, context, tcr)) {
    char msg[512];
    snprintf(msg, sizeof(msg), "Unhandled exception %d at 0x%lx, context->regs at #x%lx", signum, xpPC(context), (natural)xpGPRvector(context));
    if (lisp_Debugger(context, info, signum, msg)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }
}

void
interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
#ifdef NOTYET
  if (tcr) {
    if (TCR_INTERRUPT_LEVEL(tcr) < 0) {
      tcr->interrupt_pending = 1 << fixnumshift;
    } else {
      LispObj cmain = nrs_CMAIN.vcell;

      if ((fulltag_of(cmain) == fulltag_misc) &&
	  (header_subtag(header_of(cmain)) == subtag_macptr)) {
	/* 
	   This thread can (allegedly) take an interrupt now.
	   It's tricky to do that if we're executing
	   foreign code (especially Linuxthreads code, much
	   of which isn't reentrant.)
           If we're unwinding the stack, we also want to defer
           the interrupt.
	*/
	if ((tcr->valence != TCR_STATE_LISP) ||
            (tcr->unwinding != 0)) {
	  TCR_INTERRUPT_LEVEL(tcr) = (1 << fixnumshift);
	} else {
	  xframe_list xframe_link;
	  int old_valence;
	  
	  pc_luser_xp(context, NULL);
	  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
	  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
	  PMCL_exception_handler(signum, context, tcr, info);
	  unlock_exception_lock_in_handler(tcr);
	  exit_signal_handler(tcr, old_valence);
	}
      }
    }
  }
#endif
}

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
install_pmcl_exception_handlers()
{
  
  install_signal_handler(SIGILL, (__sighandler_t)signal_handler);
  
  install_signal_handler(SIGBUS,  (__sighandler_t)signal_handler);
  install_signal_handler(SIGSEGV, (__sighandler_t)signal_handler);
  install_signal_handler(SIGFPE, (__sighandler_t)signal_handler);

  
  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT,
			 (__sighandler_t)interrupt_handler);
  signal(SIGPIPE, SIG_IGN);
}

void
enable_fp_exceptions()
{
}

void
exception_init()
{
  install_pmcl_exception_handlers();
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
