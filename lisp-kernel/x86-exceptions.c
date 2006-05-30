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



//  This doesn't GC; it returns true if it made enough room, false
//  otherwise.
//  If "extend" is true, it can try to extend the dynamic area to
//  satisfy the request.


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
    update_bytes_allocated(tcr, (void *) tcr->save_allocptr);

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


void
push_on_lisp_stack(ExceptionInformation *xp, LispObj value)
{
  LispObj *vsp = (LispObj *)xpGPR(xp,Isp);
  *--vsp = value;
  xpGPR(xp,Isp) = (LispObj)vsp;
}


/* Hard to know if or whether this is necessary in general.  For now,
   do it when we get a "wrong number of arguments" trap.
*/
void
finish_function_entry(ExceptionInformation *xp)
{
  natural nargs = (xpGPR(xp,Inargs)&0xffff)>> fixnumshift;
  signed_natural disp = nargs-3;
  LispObj *vsp =  (LispObj *) xpGPR(xp,Isp);
   
  
  if (disp > 0) {               /* implies that nargs > 3 */
    vsp[disp] = xpGPR(xp,Irbp);
    vsp[disp+1] = xpGPR(xp,Ira0);
    xpGPR(xp,Irbp) = (LispObj)(vsp+disp);
    push_on_lisp_stack(xp,xpGPR(xp,Iarg_x));
    push_on_lisp_stack(xp,xpGPR(xp,Iarg_y));
    push_on_lisp_stack(xp,xpGPR(xp,Iarg_z));
  } else {
    push_on_lisp_stack(xp,xpGPR(xp,Ira0));
    push_on_lisp_stack(xp,xpGPR(xp,Irbp));
    xpGPR(xp,Irbp) = xpGPR(xp,Isp);
    if (nargs == 3) {
      push_on_lisp_stack(xp,xpGPR(xp,Iarg_x));
    }
    if (nargs >= 2) {
      push_on_lisp_stack(xp,xpGPR(xp,Iarg_y));
    }
    if (nargs >= 1) {
      push_on_lisp_stack(xp,xpGPR(xp,Iarg_z));
    }
  }
}

Boolean
object_contains_pc(LispObj container, LispObj addr)
{
  if (fulltag_of(container) >= fulltag_misc) {
    natural elements = header_element_count(header_of(container));
    if ((addr >= container) &&
        (addr < ((LispObj)&(deref(container,1+elements))))) {
      return true;
    }
  }
  return false;
}

LispObj
create_exception_callback_frame(ExceptionInformation *xp)
{
  LispObj containing_uvector = 0, 
    relative_pc, 
    nominal_function = lisp_nil, 
    f, tra, tra_f = 0, abs_pc;

  f = xpGPR(xp,Ifn);
  tra = xpGPR(xp,Ira0);
  if (tag_of(tra) == tag_tra) {
    tra_f = tra - ((int *)tra)[-1];
    if (fulltag_of(tra_f) != fulltag_function) {
      tra_f = 0;
    }
  }

  abs_pc = (LispObj)xpPC(xp);

  if (fulltag_of(f) == fulltag_function) {
    nominal_function = f;
  } else {
    if (tra_f) {
      nominal_function = tra_f;
    }
  }
  
  f = xpGPR(xp,Ifn);
  if (object_contains_pc(f, abs_pc)) {
    containing_uvector = untag(f)+fulltag_misc;
  } else {
    f = xpGPR(xp,Ixfn);
    if (object_contains_pc(f, abs_pc)) {
      containing_uvector = untag(f)+fulltag_misc;
    } else {
      if (tra_f) {
        f = tra_f;
        if (object_contains_pc(f, abs_pc)) {
          containing_uvector = untag(f)+fulltag_misc;
          relative_pc = (abs_pc - f) << fixnumshift;
        }
      }
    }
  }
  if (containing_uvector) {
    relative_pc = (abs_pc - (LispObj)&(deref(containing_uvector,1))) << fixnumshift;
  } else {
    containing_uvector = lisp_nil;
    relative_pc = abs_pc << fixnumshift;
  }
  
  push_on_lisp_stack(xp,tra);
  push_on_lisp_stack(xp,(LispObj)xp);
  push_on_lisp_stack(xp,containing_uvector); 
  push_on_lisp_stack(xp,relative_pc);
  push_on_lisp_stack(xp,nominal_function);
  push_on_lisp_stack(xp,0);
  push_on_lisp_stack(xp,xpGPR(xp,Irbp));
  xpGPR(xp,Irbp) = xpGPR(xp,Isp);
  return xpGPR(xp,Isp);
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
  tcr->save_rbp = (LispObj *) xpGPR(xp, Irbp);


  /* Call back.  The caller of this function may have modified stack/frame
     pointers (and at least should have called prepare_for_callback()).
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
  LispObj save_rbp = xpGPR(xp,Irbp),
    save_vsp = xpGPR(xp,Isp),
    xcf = create_exception_callback_frame(xp);
  
  callback_to_lisp(tcr, nrs_CMAIN.vcell,xp, xcf, 0, 0, 0, 0);
  xpGPR(xp,Irbp) = save_rbp;
  xpGPR(xp,Isp) = save_vsp;
}

Boolean
handle_error(TCR *tcr, ExceptionInformation *xp)
{
  pc program_counter = (pc)xpPC(xp);
  unsigned char op0 = program_counter[0], op1 = program_counter[1];
  LispObj rpc = (LispObj) program_counter, errdisp = nrs_ERRDISP.vcell,
    save_rbp = xpGPR(xp,Irbp), save_vsp = xpGPR(xp,Isp), xcf;
  int skip;

  if ((fulltag_of(errdisp) == fulltag_misc) &&
      (header_subtag(header_of(errdisp)) == subtag_macptr)) {

    if ((op0 == 0xcd) && (op1 >= 0xc0) && (op1 <= 0xc2)) {
      finish_function_entry(xp);
    }
    xcf = create_exception_callback_frame(xp);
    skip = callback_to_lisp(tcr, errdisp, xp, xcf, 0, 0, 0, 0);
    xpGPR(xp,Irbp) = save_rbp;
    xpGPR(xp,Isp) = save_vsp;
    xpPC(xp) += skip;
    return true;
  } else {
    return false;
  }
}


protection_handler
* protection_handlers[] = {
  do_spurious_wp_fault,
  do_soft_stack_overflow,
  do_soft_stack_overflow,
  do_soft_stack_overflow,
  do_hard_stack_overflow,    
  do_hard_stack_overflow,
  do_hard_stack_overflow,
};


/* Maybe this'll work someday.  We may have to do something to
   make the thread look like it's not handling an exception */
void
reset_lisp_process(ExceptionInformation *xp)
{
}

Boolean
do_hard_stack_overflow(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{
  reset_lisp_process(xp);
  return false;
}


Boolean
do_spurious_wp_fault(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{

  return false;
}

Boolean
do_soft_stack_overflow(ExceptionInformation *xp, protected_area_ptr prot_area, BytePtr addr)
{
  /* Trying to write into a guard page on the vstack or tstack.
     Allocate a new stack segment, emulate stwu and stwux for the TSP, and
     signal an error_stack_overflow condition.
      */
  lisp_protection_kind which = prot_area->why;
  Boolean on_TSP = (which == kTSPsoftguard);
  LispObj save_rbp = xpGPR(xp,Irbp), 
    save_vsp = xpGPR(xp,Isp), 
    xcf,
    cmain = nrs_CMAIN.vcell;
  area *a;
  protected_area_ptr soft;
  TCR *tcr = get_tcr(false);
  int skip;

  if ((fulltag_of(cmain) == fulltag_misc) &&
      (header_subtag(header_of(cmain)) == subtag_macptr)) {
    if (on_TSP) {
      a = tcr->ts_area;
    } else {
      a = tcr->vs_area;
    }
    soft = a->softprot;
    unprotect_area(soft);
    xcf = create_exception_callback_frame(xp);
    skip = callback_to_lisp(tcr, nrs_CMAIN.vcell, xp, xcf, SIGSEGV, on_TSP, 0, 0);
    xpGPR(xp,Irbp) = save_rbp;
    xpGPR(xp,Isp) = save_vsp;
    xpPC(xp) += skip;
    return true;
  }
  return false;
}

Boolean
handle_fault(TCR *tcr, ExceptionInformation *xp, siginfo_t *info)
{
  BytePtr addr = (BytePtr) info->si_addr;
  protected_area *a = find_protected_area(addr);
  protection_handler *handler;

  if (a) {
    handler = protection_handlers[a->why];
    return handler(xp, a, addr);
  }
  return false;
}

Boolean
handle_floating_point_exception(TCR *tcr, ExceptionInformation *xp, siginfo_t *info)
{
  int code = info->si_code, rfn = 0, skip;
  pc program_counter = (pc)xpPC(xp);
  LispObj rpc = (LispObj) program_counter, xcf, cmain = nrs_CMAIN.vcell,
    save_rbp = xpGPR(xp,Irbp), save_vsp = xpGPR(xp,Isp);

  if ((fulltag_of(cmain) == fulltag_misc) &&
      (header_subtag(header_of(cmain)) == subtag_macptr)) {
    xcf = create_exception_callback_frame(xp);
    skip = callback_to_lisp(tcr, cmain, xp, xcf, SIGFPE, code, 0, 0);
    xpPC(xp) += skip;
    xpGPR(xp,Irbp) = save_rbp;
    xpGPR(xp,Isp) = save_vsp;
    return true;
  } else {
    return false;
  }
}

Boolean
extend_tcr_tlb(TCR *tcr, ExceptionInformation *xp)
{
  LispObj index, old_limit = tcr->tlb_limit, new_limit, new_bytes;
  LispObj *old_tlb = tcr->tlb_pointer, *new_tlb, *work, *tos;

  tos = (LispObj*)(xpGPR(xp,Isp));
  index = *tos++;
  (xpGPR(xp,Isp))=(LispObj)tos;
  
  new_limit = align_to_power_of_2(index+1,12);
  new_bytes = new_limit-old_limit;
  new_tlb = realloc(old_tlb, new_limit);

  if (new_tlb == NULL) {
    return false;
  }
  work = (LispObj *) ((BytePtr)new_tlb+old_limit);

  while (new_bytes) {
    *work++ = no_thread_local_binding_marker;
    new_bytes -= sizeof(LispObj);
  }
  tcr->tlb_pointer = new_tlb;
  tcr->tlb_limit = new_limit;
  return true;
}


Boolean
handle_exception(int signum, siginfo_t *info, ExceptionInformation  *context, TCR *tcr)
{
  pc program_counter = (pc)xpPC(context);

  switch (signum) {
  case SIGSEGV:
    if (((info->si_code) &0x7f) == 0) {
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
	  xpPC(context) = (natural) (program_counter+1);
	  lisp_Debugger(context, info, debug_entry_dbg, "Lisp Breakpoint");
	  return true;
          
        default:
          return handle_error(tcr, context);
	}
      } else {
	return false;
      }

    } else {
      return handle_fault(tcr, context, info);
    }
    break;

  case SIGNAL_FOR_PROCESS_INTERRUPT:
    tcr->interrupt_pending = 0;
    callback_for_interrupt(tcr, context);
    return true;
    break;


  case SIGILL:
    if ((program_counter[0] == XUUO_OPCODE_0) &&
	(program_counter[1] == XUUO_OPCODE_1)) {
      switch (program_counter[2]) {
      case XUUO_TLB_TOO_SMALL:
        if (extend_tcr_tlb(tcr,context)) {
          xpPC(context)+=3;
          return true;
        }
	break;
	
      case XUUO_INTERRUPT_NOW:
	callback_for_interrupt(tcr,context);
	xpPC(context)+=3;
	return true;
	
      default:
	return false;
      }
    } else {
      return false;
    }
    break;
    
  case SIGFPE:
    return handle_floating_point_exception(tcr, context, info);

  default:
    return false;
  }
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
    pthread_kill((pthread_t)(tcr->osid), SIGNAL_FOR_PROCESS_INTERRUPT);
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

    snprintf(msg, sizeof(msg), "Unhandled exception %d at 0x%lx, context->regs at #x%lx", signum, xpPC(context), (natural)xpGPRvector(context));
    
    if (lisp_Debugger(context, info, signum, msg)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }
  unlock_exception_lock_in_handler(tcr);
  exit_signal_handler(tcr, old_valence);
  /* raise_pending_interrupt(tcr); */
}

#ifdef LINUX
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
#endif

LispObj *
copy_siginfo(siginfo_t *info, LispObj *current)
{
  siginfo_t *dest = ((siginfo_t *)current) - 1;
  *dest = *info;
  return (LispObj *)dest;
}

#ifdef LINUX
typedef fpregset_t copy_ucontext_last_arg_t;
#else
typedef void * copy_ucontext_last_arg_t;
#endif

LispObj *
copy_ucontext(ExceptionInformation *context, LispObj *current, copy_ucontext_last_arg_t fp)
{
  ExceptionInformation *dest = ((ExceptionInformation *)current)-1;
  dest = (ExceptionInformation *) (((LispObj)dest) & ~15);

  *dest = *context;
  /* Fix it up a little; where's the signal mask allocated, if indeed
     it is "allocated" ? */
#ifdef LINUX
  dest->uc_mcontext.fpregs = fp;
#endif
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
#ifdef LINUX
  if (xp->uc_mcontext.fpregs == NULL) {
    Bug(xp, "no FP regs in context\n");
    exit(1);
  }
#endif
    rsp = xpMMXreg(xp, Iforeign_sp);
  }
  return (LispObj *) ((rsp-128 & ~15));
}

void
altstack_signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  TCR* tcr = get_tcr(true);
  LispObj *foreign_rsp = find_foreign_rsp(context, tcr->cs_area);
#ifdef LINUX
  fpregset_t fpregs = NULL;
#else
  void *fpregs = NULL;
#endif
  siginfo_t *info_copy = NULL;
  ExceptionInformation *xp = NULL;

  if (foreign_rsp) {
#ifdef LINUX
    foreign_rsp = copy_fpregs(context, foreign_rsp, &fpregs);
#endif
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
          signed_natural alloc_displacement = 0;
	  
	  pc_luser_xp(context, tcr, &alloc_displacement);
	  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
	  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
	  handle_exception(signum, info, context, tcr);
          if (alloc_displacement) {
            tcr->save_allocptr -= alloc_displacement;
          }
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
#ifdef LINUX
  fpregset_t fpregs = NULL;
#else
  void *fpregs = NULL;
#endif

  if (foreign_rsp) {
#ifdef LINUX
    foreign_rsp = copy_fpregs(context, foreign_rsp, &fpregs);
#endif
    foreign_rsp = copy_siginfo(info, foreign_rsp);
    foreign_rsp = copy_ucontext(context, foreign_rsp, fpregs);
    *--foreign_rsp = (LispObj)__builtin_return_address(0);
    switch_to_foreign_stack(foreign_rsp,interrupt_handler,signum,info,context);
  }
}


void
install_signal_handler(int signo, void * handler)
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
  
  install_signal_handler(SIGILL, altstack_signal_handler);
  
  install_signal_handler(SIGBUS, altstack_signal_handler);
  install_signal_handler(SIGSEGV,altstack_signal_handler);
  install_signal_handler(SIGFPE, altstack_signal_handler);

  
  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT,
			 altstack_interrupt_handler);
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

/*
  Lower (move toward 0) the "end" of the soft protected area associated
  with a by a page, if we can.
*/

void

adjust_soft_protection_limit(area *a)
{
  char *proposed_new_soft_limit = a->softlimit - 4096;
  protected_area_ptr p = a->softprot;
  
  if (proposed_new_soft_limit >= (p->start+16384)) {
    p->end = proposed_new_soft_limit;
    p->protsize = p->end-p->start;
    a->softlimit = proposed_new_soft_limit;
  }
  protect_area(p);
}

void
restore_soft_stack_limit(unsigned restore_tsp)
{
  TCR *tcr = get_tcr(false);
  area *a;
 
  if (restore_tsp) {
    a = tcr->ts_area;
  } else {
    a = tcr->vs_area;
  }
  adjust_soft_protection_limit(a);
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

extern opcode egc_write_barrier_start, egc_write_barrier_end,
  egc_store_node_conditional_success_test,egc_store_node_conditional,
  egc_set_hash_key, egc_gvset, egc_rplacd;

/* We use (extremely) rigidly defined instruction sequences for consing,
   mostly so that 'pc_luser_xp()' knows what to do if a thread is interrupted
   while consing.

   Note that we can usually identify which of these instructions is about
   to be executed by a stopped thread without comparing all of the bytes
   to those at the stopped program counter, but we generally need to
   know the sizes of each of these instructions.
*/

opcode load_allocptr_reg_from_tcr_save_allocptr_instruction[] =
  {0x65,0x48,0x8b,0x1c,0x25,0xd8,0x00,0x00,0x00};
opcode compare_allocptr_reg_to_tcr_save_allocbase_instruction[] =
  {0x65,0x48,0x3b,0x1c,0x25,0xe0,0x00,0x00,0x00};
opcode branch_around_alloc_trap_instruction[] =
  {0x7f,0x02};
opcode alloc_trap_instruction[] =
  {0xcd,0xc5};
opcode clear_tcr_save_allocptr_tag_instruction[] =
  {0x65,0x80,0x24,0x25,0xd8,0x00,0x00,0x00,0xf0};
opcode set_allocptr_header_instruction[] =
  {0x48,0x89,0x43,0xf3};


alloc_instruction_id
recognize_alloc_instruction(pc program_counter)
{
  switch(program_counter[0]) {
  case 0xcd: return ID_alloc_trap_instruction;
  case 0x7f: return ID_branch_around_alloc_trap_instruction;
  case 0x48: return ID_set_allocptr_header_instruction;
  case 0x65: 
    switch(program_counter[1]) {
    case 0x80: return ID_clear_tcr_save_allocptr_tag_instruction;
    case 0x48:
      switch(program_counter[2]) {
      case 0x3b: return ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction;
      case 0x8b: return ID_load_allocptr_reg_from_tcr_save_allocptr_instruction;
      }
    }
  }
  return ID_unrecognized_alloc_instruction;
}
      
  
void
pc_luser_xp(ExceptionInformation *xp, TCR *tcr, signed_natural *interrupt_displacement)
{
  pc program_counter = (pc)xpPC(xp);
  int allocptr_tag = fulltag_of((LispObj)(tcr->save_allocptr));

  if (allocptr_tag != 0) {
    alloc_instruction_id state = recognize_alloc_instruction(program_counter);
    signed_natural 
      disp = (allocptr_tag == fulltag_cons) ?
      sizeof(cons) - fulltag_cons :
      xpGPR(xp,Iimm1);
    LispObj new_vector;

    if ((state == ID_unrecognized_alloc_instruction) ||
        ((state == ID_set_allocptr_header_instruction) &&
         (allocptr_tag == fulltag_cons))) {
      Bug(NULL, "Can't determine state of thread 0x%lx, interrupted during memory allocation", tcr);
    }
    switch(state) {
    case ID_set_allocptr_header_instruction:
      /* We were consing a vector and we won.  Set the header of the new vector
         (in the allocptr register) to the header in %rax and skip over this
         instruction, then fall into the next case. */
      new_vector = xpGPR(xp,Iallocptr);
      deref(new_vector,0) = xpGPR(xp,Iimm0);

      xpPC(xp) += sizeof(set_allocptr_header_instruction);
      /* Fall thru */
    case ID_clear_tcr_save_allocptr_tag_instruction:
      tcr->save_allocptr = (void *)(((LispObj)tcr->save_allocptr) & ~fulltagmask);
      xpPC(xp) += sizeof(clear_tcr_save_allocptr_tag_instruction);
      break;
    case ID_alloc_trap_instruction:
      /* If we're looking at another thread, we're pretty much committed to
         taking the trap.  We don't want the allocptr register to be pointing
         into the heap, so make it point to (- VOID_ALLOCPTR disp), where 'disp'
         was determined above. 
      */
      if (interrupt_displacement == NULL) {
        xpGPR(xp,Iallocptr) = VOID_ALLOCPTR - disp;
      } else {
        /* Back out, and tell the caller how to resume the allocation attempt */
        *interrupt_displacement = disp;
        xpPC(xp) -= (sizeof(branch_around_alloc_trap_instruction)+
                     sizeof(compare_allocptr_reg_to_tcr_save_allocbase_instruction) +
                     sizeof(load_allocptr_reg_from_tcr_save_allocptr_instruction));
      }
      break;
    case ID_branch_around_alloc_trap_instruction:
      /* If we'd take the branch - which is a 'jg" - around the alloc trap,
         we might as well finish the allocation.  Otherwise, back out of the
         attempt. */
      {
        int flags = (int)xpGPR(xp,Iflags);
        
        if ((!(flags & (1 << X86_ZERO_FLAG_BIT))) &&
            ((flags & (1 << X86_SIGN_FLAG_BIT)) ==
             (flags & (1 << X86_CARRY_FLAG_BIT)))) {
          /* The branch (jg) would have been taken.  Emulate taking it. */
          xpPC(xp) += (sizeof(branch_around_alloc_trap_instruction)+
                       sizeof(alloc_trap_instruction));
          if (allocptr_tag == fulltag_misc) {
            /* Slap the header on the new uvector */
            new_vector = xpGPR(xp,Iallocptr);
            deref(new_vector,0) = xpGPR(xp,Iimm0);
            xpPC(xp) += sizeof(set_allocptr_header_instruction);
          }
          tcr->save_allocptr = (void *)(((LispObj)tcr->save_allocptr) & ~fulltagmask);
          xpPC(xp) += sizeof(clear_tcr_save_allocptr_tag_instruction);
        } else {
          /* Back up */
          xpPC(xp) -= (sizeof(compare_allocptr_reg_to_tcr_save_allocbase_instruction) +
                       sizeof(load_allocptr_reg_from_tcr_save_allocptr_instruction));
          xpGPR(xp,Iallocptr) = VOID_ALLOCPTR;
          tcr->save_allocptr = (void *)(VOID_ALLOCPTR-disp);
        }
      }
      break;
    case ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction:
      xpGPR(xp,Iallocptr) = VOID_ALLOCPTR;
      xpPC(xp) -= sizeof(load_allocptr_reg_from_tcr_save_allocptr_instruction);
      /* Fall through */
    case ID_load_allocptr_reg_from_tcr_save_allocptr_instruction:
      tcr->save_allocptr = (void *)(VOID_ALLOCPTR-disp);
      break;
    }
    return;
  }
  if ((program_counter >= &egc_write_barrier_start) &&
      (program_counter < &egc_write_barrier_end)) {
    LispObj *ea = 0, val, root;
    bitvector refbits = (bitvector)(lisp_global(REFBITS));
    Boolean need_store = true, need_check_memo = true, need_memoize_root = false;

    if (program_counter >= &egc_store_node_conditional) {
      if ((program_counter < &egc_store_node_conditional_success_test) ||
          ((program_counter == &egc_store_node_conditional_success_test) &&
           !(xpGPR(xp, Iflags) & (1 << X86_ZERO_FLAG_BIT)))) {
        /* Back up the PC, try again */
        xpPC(xp) = (LispObj) &egc_store_node_conditional;
        return;
      }
      /* The conditional store succeeded.  Set the refbit, return to ra0 */
      val = xpGPR(xp,Iarg_z);
      ea = (LispObj*)(xpGPR(xp,Iarg_x) + (unbox_fixnum((signed_natural)
                                                       xpGPR(xp,Itemp0))));
      xpGPR(xp,Iarg_z) = t_value;
      need_store = false;
    } else if (program_counter >= &egc_set_hash_key) {
      root = xpGPR(xp,Iarg_x);
      ea = (LispObj *) (root+xpGPR(xp,Iarg_y)+misc_data_offset);
      val = xpGPR(xp,Iarg_z);
      need_memoize_root = true;
    } else if (program_counter >= &egc_gvset) {
      ea = (LispObj *) (xpGPR(xp,Iarg_x)+xpGPR(xp,Iarg_y)+misc_data_offset);
      val = xpGPR(xp,Iarg_z);
    } else if (program_counter >= &egc_rplacd) {
      ea = (LispObj *) untag(xpGPR(xp,Iarg_y));
      val = xpGPR(xp,Iarg_z);
    } else {                      /* egc_rplaca */
      ea =  ((LispObj *) untag(xpGPR(xp,Iarg_y)))+1;
      val = xpGPR(xp,Iarg_z);
    }
    if (need_store) {
      *ea = val;
    }
    if (need_check_memo) {
      natural  bitnumber = area_dnode(ea, lisp_global(HEAP_START));
      if ((bitnumber < lisp_global(OLDSPACE_DNODE_COUNT)) &&
          ((LispObj)ea < val)) {
        atomic_set_bit(refbits, bitnumber);
        if (need_memoize_root) {
          bitnumber = area_dnode(root, lisp_global(HEAP_START));
          atomic_set_bit(refbits, bitnumber);
        }
      }
    }
    xpPC(xp) = xpGPR(xp,Ira0);
    return;
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
      pc_luser_xp(xp, tcr, NULL);
    }
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
  tcr->save_allocbase = (void *)VOID_ALLOCPTR;
  if (fulltag_of((LispObj)(tcr->save_allocptr)) == 0) {
    tcr->save_allocptr = (void *)VOID_ALLOCPTR;
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

  /* This is generally necessary if the current thread invoked the GC
     via an alloc trap, and harmless if the GC was invoked via a GC
     trap.  (It's necessary in the first case because the "allocptr"
     register - %rbx - may be pointing into the middle of something
     below tcr->save_allocbase, and we wouldn't want the GC to see
     that bogus pointer.) */
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
