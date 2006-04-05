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


void
update_bytes_allocated(TCR* tcr, void *cur_allocptr)
{
  BytePtr 
    last = (BytePtr) tcr->last_allocptr, 
    current = (BytePtr) cur_allocptr;
  if (last && (tcr->save_allocbase != ((void *)VOID_ALLOCPTR))) {
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
handle_gc_trap(ExceptionInformation *xp, TCR *tcr)
{
  LispObj 
    selector = xpGPR(xp,Iimm0), 
    arg = xpGPR(xp,Iimm1);
  area *a = active_dynamic_area;
  Boolean egc_was_enabled = (a->older != NULL);

  switch (selector) {
  case GC_TRAP_FUNCTION_EGC_CONTROL:
    egc_control(arg != 0, a->active);
    xpGPR(xp,Iarg_z) = lisp_nil + (egc_was_enabled ? t_offset : 0);
    break;

  case GC_TRAP_FUNCTION_CONFIGURE_EGC:
    a->threshold = unbox_fixnum(xpGPR(xp, Iarg_x));
    g1_area->threshold = unbox_fixnum(xpGPR(xp, Iarg_y));
    g2_area->threshold = unbox_fixnum(xpGPR(xp, Iarg_z));
    xpGPR(xp,Iarg_z) = lisp_nil+t_offset;
    break;

  case GC_TRAP_FUNCTION_SET_LISP_HEAP_THRESHOLD:
    if (((signed_natural) arg) > 0) {
      lisp_heap_gc_threshold = 
        align_to_power_of_2((arg-1) +
                            (heap_segment_size - 1),
                            log2_heap_segment_size);
    }
    /* fall through */
  case GC_TRAP_FUNCTION_GET_LISP_HEAP_THRESHOLD:
    xpGPR(xp, Iimm0) = lisp_heap_gc_threshold;
    break;

  case GC_TRAP_FUNCTION_USE_LISP_HEAP_THRESHOLD:
    /*  Try to put the current threshold in effect.  This may
        need to disable/reenable the EGC. */
    untenure_from_area(tenured_area);
    resize_dynamic_heap(a->active,lisp_heap_gc_threshold);
    if (egc_was_enabled) {
      if ((a->high - a->active) >= a->threshold) {
        tenure_to_area(tenured_area);
      }
    }
    xpGPR(xp, Iimm0) = lisp_heap_gc_threshold;
    break;

  default:
    update_bytes_allocated(tcr, (void *) ptr_from_lispobj(xpGPR(xp, Iallocptr)));

    if (selector == GC_TRAP_FUNCTION_IMMEDIATE_GC) {
      gc_from_xp(xp, 0L);
      break;
    }
    
    if (egc_was_enabled) {
      egc_control(false, (BytePtr) a->active);
    }
    gc_from_xp(xp, 0L);
    if (selector & GC_TRAP_FUNCTION_PURIFY) {
      purify_from_xp(xp, 0L);
      gc_from_xp(xp, 0L);
    }
    if (selector & GC_TRAP_FUNCTION_SAVE_APPLICATION) {
      OSErr err;
      extern OSErr save_application(unsigned);
      area *vsarea = tcr->vs_area;
	
      nrs_TOPLFUNC.vcell = *((LispObj *)(vsarea->high)-1);
      err = save_application(arg);
      if (err == noErr) {
	exit(0);
      }
      fatal_oserr(": save_application", err);
    }
    if (selector == GC_TRAP_FUNCTION_SET_HONS_AREA_SIZE) {
      LispObj aligned_arg = align_to_power_of_2(arg, log2_nbits_in_word);
      signed_natural 
	delta_dnodes = ((signed_natural) aligned_arg) - 
	((signed_natural) tenured_area->static_dnodes);
      change_hons_area_size_from_xp(xp, delta_dnodes*dnode_size);
      xpGPR(xp, Iimm0) = tenured_area->static_dnodes;
    }
    if (egc_was_enabled) {
      egc_control(true, NULL);
    }
    break;
  }
  return true;
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

int
callback_to_lisp (TCR * tcr, LispObj callback_macptr, ExceptionInformation *xp,
                  natural arg1, natural arg2, natural arg3, natural arg4, natural arg5)
{
  sigset_t mask;
  natural  callback_ptr, i;
  int delta;



  /* Put the active stack pointers where .SPcallback expects them */
  tcr->save_vsp = (LispObj *) xpGPR(xp, Isp);
  tcr->save_tsp = (LispObj *) xpMMXreg(xp, Itsp);



  /* Call back.
     Lisp will handle trampolining through some code that
     will push lr/fn & pc/nfn stack frames for backtrace.
  */
  callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
  UNLOCK(lisp_global(EXCEPTION_LOCK), tcr);
  delta = ((int (*)())callback_ptr) (xp, arg1, arg2, arg3, arg4, arg5);
  LOCK(lisp_global(EXCEPTION_LOCK), tcr);
  return delta;
}

void
callback_for_interrupt(TCR *tcr, ExceptionInformation *xp)
{
  callback_to_lisp(tcr, nrs_CMAIN.vcell,xp, 0, 0, 0, 0, 0);
}


Boolean
handle_fault(TCR *tcr, ExceptionInformation *xp, siginfo_t *info)
{
  return false;
}

Boolean
handle_exception(int signum, siginfo_t *info, ExceptionInformation  *context, TCR *tcr)
{
  pc program_counter = (pc)xpPC(context);

  switch (signum) {
  case SIGSEGV:
    if ((info->si_addr) == 0) {
      /* Something mapped to SIGSEGV that has nothing to do with
	 a memory fault */
      if (*program_counter == INTN_OPCODE) {
	program_counter++;
	switch (*program_counter) {
	case UUO_ALLOC_TRAP:
	  if (handle_alloc_trap(context, tcr)) {
	    xpPC(context) += 2;	/* we might have GCed. */
	    return true;
	  }
	  break;
	case UUO_GC_TRAP:
	  if (handle_gc_trap(context, tcr)) {
	    xpPC(context) += 2;
	    return true;
	  }
	  break;
	  
	case UUO_DEBUG_TRAP:
	  lisp_Debugger(context, info, debug_entry_dbg, "Lisp Breakpoint");
	  xpPC(context) = (natural) (program_counter+1);
	  return true;

	}
      } else if ((program_counter[0] == XUUO_OPCODE_0) &&
		 (program_counter[1] == XUUO_OPCODE_1)) {
	switch (program_counter[2]) {
	case XUUO_TLB_TOO_SMALL:
	  return false;
	  break;
	  
	case XUUO_INTERRUPT_NOW:
	  callback_for_interrupt(tcr,context);
	  xpPC(xp)+=3;
	  return true;

	default:
	  return false;
	}
      } else {
	return false;
      }

    } else {
      return handle_fault(tcr, context, info);
    }
    break;

  case SIGNAL_FOR_PROCESS_INTERRUPT:
    callback_for_interrupt(tcr, context);
    return true;
    break;
    
    
  }
  return false;
}


/* 
   Current thread has all signals masked.  Before unmasking them,
   make it appear that the current thread has been suspended.
   (This is to handle the case where another thread is trying
   to GC before this thread is able to seize the exception lock.)
*/
int
prepare_to_wait_for_exception_lock(TCR *tcr, ExceptionInformation *context)
{
  int old_valence = tcr->valence;

  tcr->pending_exception_context = context;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;

  ALLOW_EXCEPTIONS(context);
  return old_valence;
}  

void
wait_for_exception_lock_in_handler(TCR *tcr, 
				   ExceptionInformation *context,
				   xframe_list *xf)
{

  LOCK(lisp_global(EXCEPTION_LOCK), tcr);
#if 0
  fprintf(stderr, "0x%x has exception lock\n", tcr);
#endif
  xf->curr = context;
  xf->prev = tcr->xframe;
  tcr->xframe =  xf;
  tcr->pending_exception_context = NULL;
  tcr->valence = TCR_STATE_FOREIGN; 
}

void
unlock_exception_lock_in_handler(TCR *tcr)
{
  tcr->pending_exception_context = tcr->xframe->curr;
  tcr->xframe = tcr->xframe->prev;
  tcr->valence = TCR_STATE_EXCEPTION_RETURN;
  UNLOCK(lisp_global(EXCEPTION_LOCK),tcr);
#if 0
  fprintf(stderr, "0x%x released exception lock\n", tcr);
#endif
}

/* 
   If an interrupt is pending on exception exit, try to ensure
   that the thread sees it as soon as it's able to run.
*/
void
raise_pending_interrupt(TCR *tcr)
{
  if (TCR_INTERRUPT_LEVEL(tcr) > 0) {
    pthread_kill((pthread_t)ptr_from_lispobj(tcr->osid), SIGNAL_FOR_PROCESS_INTERRUPT);
  }
}

void
exit_signal_handler(TCR *tcr, int old_valence)
{
  sigset_t mask;
  sigfillset(&mask);
  
  pthread_sigmask(SIG_SETMASK,&mask, NULL);
  tcr->valence = old_valence;
  tcr->pending_exception_context = NULL;
}

void
signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  xframe_list xframe_link;
  int old_valence;
  TCR* tcr = get_tcr(false);

  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
  wait_for_exception_lock_in_handler(tcr,context, &xframe_link);


  if (! handle_exception(signum, info, context, tcr)) {
    char msg[512];
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask,SIGINT);
    pthread_sigmask(SIG_BLOCK, &mask, NULL);

    snprintf(msg, sizeof(msg), "Unhandled exception %d at 0x%lx, context->regs at #x%lx", signum, xpPC(context), (natural)xpGPRvector(context));
    
    if (lisp_Debugger(context, info, signum, msg)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }
  unlock_exception_lock_in_handler(tcr);
  exit_signal_handler(tcr, old_valence);
  /* raise_pending_interrupt(tcr); */
}

LispObj *
copy_fpregs(ExceptionInformation *xp, LispObj *current, fpregset_t *destptr)
{
  fpregset_t src = xp->uc_mcontext.fpregs, dest;
  
  if (src) {
    dest = ((fpregset_t)current)-1;
    *dest = *src;
    *destptr = dest;
    current = (LispObj *) dest;
  }
  return current;
}

LispObj *
copy_siginfo(siginfo_t *info, LispObj *current)
{
  siginfo_t *dest = ((siginfo_t *)current) - 1;
  *dest = *info;
  return (LispObj *)dest;
}

LispObj *
copy_ucontext(ExceptionInformation *context, LispObj *current, fpregset_t fp)
{
  ExceptionInformation *dest = ((ExceptionInformation *)current)-1;

  *dest = *context;
  /* Fix it up a little; where's the signal mask allocated, if indeed
     it is "allocated" ? */
  dest->uc_mcontext.fpregs = fp;
  dest->uc_stack.ss_sp = 0;
  dest->uc_stack.ss_size = 0;
  dest->uc_stack.ss_flags = 0;
  return (LispObj *)dest;
}

LispObj *
find_foreign_rsp(ExceptionInformation *xp, area *foreign_area)
{
  LispObj rsp = xpGPR(xp, Isp);
  if (((BytePtr)rsp < foreign_area->low) ||
      ((BytePtr)rsp > foreign_area->high)) {
    rsp = xpMMXreg(xp, Iforeign_sp);
  }
  return (LispObj *) ((rsp-128 & ~!5));
}

void
altstack_signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  TCR* tcr = get_tcr(true);
  LispObj *foreign_rsp = find_foreign_rsp(context, tcr->cs_area);
  fpregset_t fpregs = NULL;
  siginfo_t *info_copy = NULL;
  ExceptionInformation *xp = NULL;

  if (foreign_rsp) {
    foreign_rsp = copy_fpregs(context, foreign_rsp, &fpregs);
    foreign_rsp = copy_siginfo(info, foreign_rsp);
    info_copy = (siginfo_t *)foreign_rsp;
    foreign_rsp = copy_ucontext(context, foreign_rsp, fpregs);
    xp = (ExceptionInformation *)foreign_rsp;
    *--foreign_rsp = (LispObj)__builtin_return_address(0);
    switch_to_foreign_stack(foreign_rsp,signal_handler,signum,info_copy,xp);
  }
}

void
interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  if (tcr) {
    if (TCR_INTERRUPT_LEVEL(tcr) < 0) {
      tcr->interrupt_pending = (1L << (nbits_in_word - 1L));
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
	  tcr->interrupt_pending = (1L << (nbits_in_word - 1L));
	} else {
	  xframe_list xframe_link;
	  int old_valence;
	  
	  pc_luser_xp(context, NULL);
	  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
	  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
	  handle_exception(signum, info, context, tcr);
	  unlock_exception_lock_in_handler(tcr);
	  exit_signal_handler(tcr, old_valence);
	}
      }
    }
  }
}

void
altstack_interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  LispObj *foreign_rsp = find_foreign_rsp(context, tcr->cs_area);
  fpregset_t fpregs = NULL;

  if (foreign_rsp) {
    foreign_rsp = copy_fpregs(context, foreign_rsp, &fpregs);
    foreign_rsp = copy_siginfo(info, foreign_rsp);
    foreign_rsp = copy_ucontext(context, foreign_rsp, fpregs);
    *--foreign_rsp = (LispObj)__builtin_return_address(0);
    switch_to_foreign_stack(foreign_rsp,interrupt_handler,signum,info,context);
  }
}


void
install_signal_handler(int signo, __sighandler_t handler)
{
  struct sigaction sa;
  
  sa.sa_sigaction = (void *)handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = 
    SA_RESTART
    | SA_ONSTACK
    | SA_SIGINFO;

  sigaction(signo, &sa, NULL);
}


void
install_pmcl_exception_handlers()
{
  
  install_signal_handler(SIGILL, (__sighandler_t)altstack_signal_handler);
  
  install_signal_handler(SIGBUS,  (__sighandler_t)altstack_signal_handler);
  install_signal_handler(SIGSEGV, (__sighandler_t)altstack_signal_handler);
  install_signal_handler(SIGFPE, (__sighandler_t)altstack_signal_handler);

  
  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT,
			 (__sighandler_t)altstack_interrupt_handler);
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

void
setup_sigaltstack(area *a)
{
  stack_t stack;
  stack.ss_sp = a->low;
  a->low += 8192;
  stack.ss_size = 8192;
  stack.ss_flags = 0;

  sigaltstack(&stack, NULL);
}

void
pc_luser_xp(ExceptionInformation *xp, TCR *tcr)
{
  if (fulltag_of((LispObj)(tcr->save_allocptr)) != 0) {
    /* Not handled yet */
    Bug(NULL, "Other thread suspended during memory allocation");
  }
}

void
normalize_tcr(ExceptionInformation *xp, TCR *tcr, Boolean is_other_tcr)
{
  void *cur_allocptr = (void *)(tcr->save_allocptr);
  LispObj lisprsp, lisptsp;
  area *a;

  if (xp) {
    if (is_other_tcr) {
      pc_luser_xp(xp, tcr);
    }
    tcr->save_allocptr = tcr->save_allocbase = (void *)VOID_ALLOCPTR;
    a = tcr->vs_area;
    lisprsp = xpGPR(xp, Isp);
    if (((BytePtr)lisprsp >= a->low) &&
	((BytePtr)lisprsp < a->high)) {
      a->active = (BytePtr)lisprsp;
    } else {
      a->active = (BytePtr) tcr->save_vsp;
    }
    a = tcr->ts_area;
    a->active = (BytePtr) xpMMXreg(xp, Itsp);
  } else {
    /* In ff-call; get area active pointers from tcr */
    tcr->vs_area->active = (BytePtr) tcr->save_vsp;
    tcr->ts_area->active = (BytePtr) tcr->save_tsp;
  }
  if (cur_allocptr) {
    update_bytes_allocated(tcr, cur_allocptr);
  }
}


/* Suspend and "normalize" other tcrs, then call a gc-like function
   in that context.  Resume the other tcrs, then return what the
   function returned */

int
gc_like_from_xp(ExceptionInformation *xp, 
                int(*fun)(TCR *, signed_natural), 
                signed_natural param)
{
  TCR *tcr = get_tcr(false), *other_tcr;
  ExceptionInformation* other_xp;
  int result;
  signed_natural inhibit;

  suspend_other_threads(true);
  inhibit = (signed_natural)(lisp_global(GC_INHIBIT_COUNT));
  if (inhibit != 0) {
    if (inhibit > 0) {
      lisp_global(GC_INHIBIT_COUNT) = (LispObj)(-inhibit);
    }
    resume_other_threads(true);
    return 0;
  }

  xpGPR(xp, Iallocptr) = VOID_ALLOCPTR;

  normalize_tcr(xp, tcr, false);


  for (other_tcr = tcr->next; other_tcr != tcr; other_tcr = other_tcr->next) {
    if (other_tcr->pending_exception_context) {
      other_tcr->gc_context = other_tcr->pending_exception_context;
    } else if (other_tcr->valence == TCR_STATE_LISP) {
      other_tcr->gc_context = other_tcr->suspend_context;
    } else {
      /* no pending exception, didn't suspend in lisp state:
	 must have executed a synchronous ff-call. 
      */
      other_tcr->gc_context = NULL;
    }
    normalize_tcr(other_tcr->gc_context, other_tcr, true);
  }
    


  result = fun(tcr, param);

  other_tcr = tcr;
  do {
    other_tcr->gc_context = NULL;
    other_tcr = other_tcr->next;
  } while (other_tcr != tcr);

  resume_other_threads(true);

  return result;

}

int
change_hons_area_size_from_xp(ExceptionInformation *xp, signed_natural delta_in_bytes)
{
  return gc_like_from_xp(xp, change_hons_area_size, delta_in_bytes);
}

int
purify_from_xp(ExceptionInformation *xp, signed_natural param)
{
  return gc_like_from_xp(xp, purify, param);
}

int
impurify_from_xp(ExceptionInformation *xp, signed_natural param)
{
  return gc_like_from_xp(xp, impurify, param);
}

/* Returns #bytes freed by invoking GC */

int
gc_from_tcr(TCR *tcr, signed_natural param)
{
  area *a;
  BytePtr oldfree, newfree;
  BytePtr oldend, newend;

#if 0
  fprintf(stderr, "Start GC  in 0x%lx\n", tcr);
#endif
  a = active_dynamic_area;
  oldend = a->high;
  oldfree = a->active;
  gc(tcr, param);
  newfree = a->active;
  newend = a->high;
#if 0
  fprintf(stderr, "End GC  in 0x%lx\n", tcr);
#endif
  return ((oldfree-newfree)+(newend-oldend));
}

int
gc_from_xp(ExceptionInformation *xp, signed_natural param)
{
  int status = gc_like_from_xp(xp, gc_from_tcr, param);

  freeGCptrs();
  return status;
}
