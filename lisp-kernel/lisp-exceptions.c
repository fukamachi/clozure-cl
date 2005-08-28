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

#ifdef DARWIN
#include <sys/mman.h>
#define _FPU_RESERVED 0xffffff00
#ifndef SA_NODEFER
#define SA_NODEFER 0
#endif

/* a distinguished UUO at a distinguished address */
extern void pseudo_sigreturn(ExceptionInformationPowerPC *);
#endif


#include "Threads.h"

#define MSR_FE0_MASK (((unsigned)0x80000000)>>20)
#define MSR_FE1_MASK (((unsigned)0x80000000)>>23)
#define MSR_FE0_FE1_MASK (MSR_FE0_MASK|MSR_FE1_MASK)
extern void enable_fp_exceptions(void);
extern void disable_fp_exceptions(void);

#ifdef LINUX
/* Some relatively recent kernels support this interface.
   If this prctl isn't supported, assume that we're always
   running with excptions enabled and "precise". 
*/
#ifndef PR_SET_FPEXC
#define PR_SET_FPEXC 12
#endif
#ifndef PR_FP_EXC_DISABLED
#define PR_FP_EXC_DISABLED 0
#endif
#ifndef PR_FP_EXC_PRECISE
#define PR_FP_EXC_PRECISE 3
#endif

void
enable_fp_exceptions()
{
  prctl(PR_SET_FPEXC, PR_FP_EXC_PRECISE);
}

void
disable_fp_exceptions()
{
  prctl(PR_SET_FPEXC, PR_FP_EXC_DISABLED);
}

#endif

/*
  Handle exceptions.

*/

extern LispObj lisp_nil;

extern natural lisp_heap_gc_threshold;
extern Boolean grow_dynamic_area(natural);


#ifdef PPC64
#define codevec_hdr_p(value) ((value) == (('C'<<24)|('O'<<16)|('D'<<8)|'E'))
#else
/* top 6 bits will be zero, subtag will be subtag_code_vector */
#define CV_HDR_MASK     (OP_MASK | subtagmask)
#define CV_HDR_VALUE    subtag_code_vector
#define codevec_hdr_p(value)	(((value) & CV_HDR_MASK) == CV_HDR_VALUE)
#endif


void
allocation_failure(Boolean pointerp, natural size)
{
  char buf[64];
  sprintf(buf, "Can't allocate %s of size %d bytes.", pointerp ? "pointer" : "handle", size);
  Fatal(":   Kernel memory allocation failure.  ", buf);
}

void
fatal_oserr(StringPtr param, OSErr err)
{
  char buf[64];
  sprintf(buf," - operating system error %d.", err);
  Fatal(param, buf);
}


Ptr
allocate(natural size)
{
  return (Ptr) malloc(size);
}

void
deallocate(Ptr p)
{
  free((void *)p);
}

Ptr
zalloc(natural size)
{
  Ptr p = allocate(size);
  if (p != NULL) {
    memset(p, 0, size);
  }
  return p;
}

int
ProtectMemory(LogicalAddress addr, int nbytes)
{
  int status = mprotect(addr, nbytes, PROT_READ | PROT_EXEC);
  
  if (status) {
    status = errno;
    Bug(NULL, "couldn't protect %d bytes at %x, errno = %d", nbytes, addr, status);
  }
  return status;
}

int
UnProtectMemory(LogicalAddress addr, int nbytes)
{
  return mprotect(addr, nbytes, PROT_READ|PROT_WRITE|PROT_EXEC);
}

void
unprotect_area(protected_area_ptr p)
{
  BytePtr start = p->start;
  natural nprot = p->nprot;
  
  if (nprot) {
    UnProtectMemory(start, nprot);
    p->nprot = 0;
  }
}



int
page_size = 4096;

extern void
zero_cache_lines(BytePtr, size_t, size_t);

void
zero_page(BytePtr start)
{
  zero_cache_lines(start, (page_size/cache_block_size), cache_block_size);
}



/*
  If the PC is pointing to an allocation trap, the previous instruction
  must have decremented allocptr.  Return the non-zero amount by which
  allocptr was decremented.
*/
int
allocptr_displacement(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter, prev_instr = *(program_counter-1);

  if (instr == ALLOC_TRAP_INSTRUCTION) {
    if (match_instr(prev_instr, 
                    XO_MASK | RT_MASK | RB_MASK,
                    XO(major_opcode_X31,minor_opcode_SUBF, 0, 0) |
                    RT(allocptr) |
                    RB(allocptr))) {
      return ((int) xpGPR(xp, RA_field(prev_instr)));
    }
    if (match_instr(prev_instr,
                    OP_MASK | RT_MASK | RA_MASK,
                    OP(major_opcode_ADDI) | 
                    RT(allocptr) |
                    RA(allocptr))) {
      return (int) -((short) prev_instr);
    }
    Bug(xp, "Can't determine allocation displacement");
  }
  return 0;
}


/*
  A cons cell's been successfully allocated, but the allocptr's
  still tagged (as fulltag_cons, of course.)  Emulate any instructions
  that might follow the allocation (stores to the car or cdr, an
  assignment to the "result" gpr) that take place while the allocptr's
  tag is non-zero, advancing over each such instruction.  When we're
  done, the cons cell will be allocated and initialized, the result
  register will point to it, the allocptr will be untagged, and
  the PC will point past the instruction that clears the allocptr's
  tag.
*/
void
finish_allocating_cons(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr;
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  cons *c = (cons *)ptr_from_lispobj(untag(cur_allocptr));
  int target_reg;

  while (1) {
    instr = *program_counter++;

    if (instr == UNTAG_ALLOCPTR_INSTRUCTION) {
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    }
    
    switch (instr & STORE_CXR_ALLOCPTR_MASK) {
    case STORE_CAR_ALLOCPTR_INSTRUCTION:
      c->car = xpGPR(xp,RT_field(instr));
      break;
    case STORE_CDR_ALLOCPTR_INSTRUCTION:
      c->cdr = xpGPR(xp,RT_field(instr));
      break;
    default:
      /* Assume that this is an assignment: {rt/ra} <- allocptr.
         There are several equivalent instruction forms
         that might have that effect; just assign to target here.
      */
      if (major_opcode_p(instr,major_opcode_X31)) {
	target_reg = RA_field(instr);
      } else {
	target_reg = RT_field(instr);
      }
      xpGPR(xp,target_reg) = cur_allocptr;
      break;
    }
  }
}

/*
  We were interrupted in the process of allocating a uvector; we
  survived the allocation trap, and allocptr is tagged as fulltag_misc.
  Emulate any instructions which store a header into the uvector,
  assign the value of allocptr to some other register, and clear
  allocptr's tag.  Don't expect/allow any other instructions in
  this environment.
*/
void
finish_allocating_uvector(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr;
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  int target_reg;

  while (1) {
    instr = *program_counter++;
    if (instr == UNTAG_ALLOCPTR_INSTRUCTION) {
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    }
    if ((instr &  STORE_HEADER_ALLOCPTR_MASK) == 
        STORE_HEADER_ALLOCPTR_INSTRUCTION) {
      header_of(cur_allocptr) = xpGPR(xp, RT_field(instr));
    } else {
      /* assume that this is an assignment */

      if (major_opcode_p(instr,major_opcode_X31)) {
	target_reg = RA_field(instr);
      } else {
	target_reg = RT_field(instr);
      }
      xpGPR(xp,target_reg) = cur_allocptr;
    }
  }
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


  a  = active_dynamic_area;
  oldlimit = (natural) a->active;
  newlimit = (align_to_power_of_2(oldlimit, log2_heap_segment_size) +
	      align_to_power_of_2(need, log2_heap_segment_size));
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
  xpGPR(xp,allocptr) = (LispObj) newlimit;
  xpGPR(xp,allocbase) = (LispObj) oldlimit;

#ifdef PPC64x
  bzero((BytePtr)oldlimit,align_to_power_of_2(oldlimit, 12)-oldlimit);
  HeapHighWaterMark = (BytePtr)align_to_power_of_2(oldlimit, 12);
#endif
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
      gc_from_xp(xp);
    }
  }

  /* Life is pretty simple if we can simply grab a segment
     without extending the heap.
  */
  if (new_heap_segment(xp, bytes_needed, false, tcr)) {
    xpGPR(xp, allocptr) += disp_from_allocptr;
    return true;
  }
  
  /* It doesn't make sense to try a full GC if the object
     we're trying to allocate is larger than everything
     allocated so far.
  */
  if ((lisp_global(HEAP_END)-lisp_global(HEAP_START)) > bytes_needed) {
    untenure_from_area(tenured_area); /* force a full GC */
    gc_from_xp(xp);
  }
  
  /* Try again, growing the heap if necessary */
  if (new_heap_segment(xp, bytes_needed, true, tcr)) {
    xpGPR(xp, allocptr) += disp_from_allocptr;
    return true;
  }
  
  return false;
}

#ifndef XNOMEM
#define XNOMEM 10
#endif

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

OSStatus
handle_alloc_trap(ExceptionInformation *xp, TCR *tcr)
{
  pc program_counter;
  natural cur_allocptr, bytes_needed = 0;
  opcode prev_instr;
  signed_natural disp = 0;
  unsigned allocptr_tag;

  cur_allocptr = xpGPR(xp,allocptr);
  program_counter = xpPC(xp);
  prev_instr = *(program_counter-1);
  allocptr_tag = fulltag_of(cur_allocptr);

  switch (allocptr_tag) {
  case fulltag_cons:
    bytes_needed = sizeof(cons);
    disp = -sizeof(cons) + fulltag_cons;
    break;

  case fulltag_even_fixnum:
  case fulltag_odd_fixnum:
    break;

  case fulltag_misc:
    if (match_instr(prev_instr, 
                    XO_MASK | RT_MASK | RB_MASK,
                    XO(major_opcode_X31,minor_opcode_SUBF, 0, 0) |
                    RT(allocptr) |
                    RB(allocptr))) {
      disp = -((signed_natural) xpGPR(xp, RA_field(prev_instr)));
    } else if (match_instr(prev_instr,
                           OP_MASK | RT_MASK | RA_MASK,
                           OP(major_opcode_ADDI) | 
                           RT(allocptr) |
                           RA(allocptr))) {
      disp = (signed_natural) ((short) prev_instr);
    }
    if (disp) {
      bytes_needed = (-disp) + fulltag_misc;
      break;
    }
    /* else fall thru */
  default:
    return -1;
  }

  if (bytes_needed) {
    update_bytes_allocated(tcr,((BytePtr)(cur_allocptr-disp)));
    if (allocate_object(xp, bytes_needed, disp, tcr)) {
      adjust_exception_pc(xp,4);
      return 0;
    }
    /* Couldn't allocate the object.  If it's smaller than some arbitrary
       size (say 128K bytes), signal a "chronically out-of-memory" condition;
       else signal a "allocation request failed" condition.
    */
    xpGPR(xp,allocptr) = xpGPR(xp,allocbase) = VOID_ALLOCPTR;
    handle_error(xp, bytes_needed < (128<<10) ? XNOMEM : error_alloc_failed, 0, 0,  xpPC(xp));
    return -1;
  }
  return -1;
}

OSStatus
handle_gc_trap(ExceptionInformation *xp, TCR *tcr)
{
  LispObj 
    selector = xpGPR(xp,imm0), 
    arg = xpGPR(xp,imm1);
  area *a = active_dynamic_area;
  Boolean egc_was_enabled = (a->older != NULL);


  switch (selector) {
  case GC_TRAP_FUNCTION_EGC_CONTROL:
    egc_control(arg != 0, a->active);
    xpGPR(xp,arg_z) = lisp_nil + (egc_was_enabled ? t_offset : 0);
    break;

  case GC_TRAP_FUNCTION_CONFIGURE_EGC:
    a->threshold = unbox_fixnum(xpGPR(xp, arg_x));
    g1_area->threshold = unbox_fixnum(xpGPR(xp, arg_y));
    g2_area->threshold = unbox_fixnum(xpGPR(xp, arg_z));
    xpGPR(xp,arg_z) = lisp_nil+t_offset;
    break;

  case GC_TRAP_FUNCTION_SET_LISP_HEAP_THRESHOLD:
    if (((int) arg) > 0) {
      lisp_heap_gc_threshold = 
        align_to_power_of_2((arg-1) +
                            (heap_segment_size - 1),
                            log2_heap_segment_size);
    }
    /* fall through */
  case GC_TRAP_FUNCTION_GET_LISP_HEAP_THRESHOLD:
    xpGPR(xp, arg_z) = box_fixnum(lisp_heap_gc_threshold);
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
    xpGPR(xp, arg_z) = box_fixnum(lisp_heap_gc_threshold);
    break;

  default:
    update_bytes_allocated(tcr, (void *) ptr_from_lispobj(xpGPR(xp, allocptr)));
    if (egc_was_enabled) {
      egc_control(false, (BytePtr) a->active);
    }
    gc_from_xp(xp);
    if (selector > GC_TRAP_FUNCTION_GC) {
      if (selector & GC_TRAP_FUNCTION_IMPURIFY) {
        impurify_from_xp(xp);
        /*        nrs_GC_EVENT_STATUS_BITS.vcell |= gc_integrity_check_bit; */
        gc_from_xp(xp);
      }
      if (selector & GC_TRAP_FUNCTION_PURIFY) {
        purify_from_xp(xp);
        gc_from_xp(xp);
      }
      if (selector & GC_TRAP_FUNCTION_SAVE_APPLICATION) {
        OSErr err;
        extern OSErr save_application(unsigned, Boolean);
        TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
        area *vsarea = tcr->vs_area;
	
        nrs_TOPLFUNC.vcell = *((LispObj *)(vsarea->high)-1);
        err = save_application(arg, (selector == GC_TRAP_FLAG_COMPRESS));
        if (err == noErr) {
          exit(0);
        }
        fatal_oserr(": save_application", err);
      }
    }
    
    if (egc_was_enabled) {
      egc_control(true, NULL);
    }
    break;
    
  }

  adjust_exception_pc(xp,4);
  return 0;
}


protected_area_ptr
AllProtectedAreas = NULL;

/* 
  This does a linear search.  Areas aren't created all that often;
  if there get to be very many of them, some sort of tree search
  might be justified.
*/

protected_area_ptr
find_protected_area(BytePtr addr)
{
  protected_area* p;
  
  for(p = AllProtectedAreas; p; p=p->next) {
    if ((p->start <= addr) && (p->end > addr)) {
      return p;
    }
  }
  return NULL;
}

void
signal_stack_soft_overflow(ExceptionInformation *xp, unsigned reg)
{
  /* The cstack just overflowed.  Force the current thread's
     control stack to do so until all stacks are well under their overflow
     limits. 
  */

#if 0
  lisp_global(CS_OVERFLOW_LIMIT) = CS_OVERFLOW_FORCE_LIMIT; /* force unsigned traps to fail */
#endif
  handle_error(xp, error_stack_overflow, reg, 0,  xpPC(xp));
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
restore_soft_stack_limit(unsigned stkreg)
{
  area *a;
  TCR *tcr = get_tcr(true);

  switch (stkreg) {
  case sp:
    a = tcr->cs_area;
    if ((a->softlimit - 4096) > (a->hardlimit + 16384)) {
      a->softlimit -= 4096;
    }
    tcr->cs_limit = (LispObj)ptr_to_lispobj(a->softlimit);
    break;
  case vsp:
    a = tcr->vs_area;
    adjust_soft_protection_limit(a);
    break;
  case tsp:
    a = tcr->ts_area;
    adjust_soft_protection_limit(a);
  }
}

/* Maybe this'll work someday.  We may have to do something to
   make the thread look like it's not handling an exception */
void
reset_lisp_process(ExceptionInformation *xp)
{
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp,rcontext));
  catch_frame *last_catch = (catch_frame *) ptr_from_lispobj(untag(tcr->catch_top));

  tcr->save_allocptr = (void *) ptr_from_lispobj(xpGPR(xp, allocptr));
  tcr->save_allocbase = (void *) ptr_from_lispobj(xpGPR(xp, allocbase));

  tcr->save_vsp = (LispObj *) ptr_from_lispobj(((lisp_frame *)ptr_from_lispobj(last_catch->csp))->savevsp);
  tcr->save_tsp = (LispObj *) ptr_from_lispobj((LispObj) ptr_to_lispobj(last_catch)) - (2*node_size); /* account for TSP header */

  start_lisp(tcr, 1);
}
  

/* 
   Grow or shrink the dynamic area.  Or maybe not.
   Whether or not the end of (mapped space in) the heap changes,
   ensure that everything between the freeptr and the heap end
   is mapped and read/write.  (It'll incidentally be zeroed.)
*/
Boolean
resize_dynamic_heap(BytePtr newfree, 
		    natural free_space_size)
{
  natural protbytes, zerobytes;
  area *a = active_dynamic_area;
  BytePtr newlimit, protptr, zptr;
  
  /* 
     Zero the region between the new freepointer and the end of the
     containing segment.
  */
  zptr = (BytePtr) align_to_power_of_2(newfree,log2_heap_segment_size);
  zerobytes = zptr-newfree;
  HeapHighWaterMark = zptr;

  while (zerobytes >= page_size) {
    zptr -= page_size;
    zerobytes -= page_size;
    zero_page(zptr);
  }
  
  if (zerobytes) {
    bzero(newfree, zerobytes);
  }
  if (free_space_size) {
    BytePtr lowptr = a->active;
    newlimit = lowptr + align_to_power_of_2(newfree-lowptr+free_space_size,
					    log2_heap_segment_size);
    if (newlimit > a->high) {
      return grow_dynamic_area(newlimit-a->high);
    } else if ((HeapHighWaterMark + free_space_size) < a->high) {
      shrink_dynamic_area(a->high-newlimit);
      return true;
    }
  }
}


 
void
update_area_active (area **aptr, BytePtr value)
{
  area *a = *aptr;
  for (; a; a = a->older) {
    if ((a->low <= value) && (a->high >= value)) break;
  };
  if (a == NULL) Bug(NULL, "Can't find active area");
  a->active = value;
  *aptr = a;

  for (a = a->younger; a; a = a->younger) {
    a->active = a->high;
  }
}

void
normalize_tcr(ExceptionInformation *xp, TCR *tcr, Boolean is_other_tcr)
{
  void *cur_allocptr = NULL;
  LispObj freeptr = 0;

  if (xp) {
    if (is_other_tcr) {
      pc_luser_xp(xp, tcr);
      freeptr = xpGPR(xp, allocptr);
      if (fulltag_of(freeptr) == 0){
	cur_allocptr = (void *) ptr_from_lispobj(freeptr);
      }
    }
    update_area_active((area **)&tcr->cs_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, sp)));
    update_area_active((area **)&tcr->vs_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, vsp)));
    update_area_active((area **)&tcr->ts_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, tsp)));
  } else {
    /* In ff-call.  No need to update cs_area */
    cur_allocptr = (void *) (tcr->save_allocptr);
    tcr->save_allocptr = tcr->save_allocbase = (void *)VOID_ALLOCPTR;
    update_area_active((area **)&tcr->vs_area, (BytePtr) tcr->save_vsp);
    update_area_active((area **)&tcr->ts_area, (BytePtr) tcr->save_tsp);
  }
  if (cur_allocptr) {
    update_bytes_allocated(tcr, cur_allocptr);
    if (freeptr) {
      xpGPR(xp, allocptr) = VOID_ALLOCPTR;
      xpGPR(xp, allocbase) = VOID_ALLOCPTR;
    }
  }
}

/* Suspend and "normalize" other tcrs, then call a gc-like function
   in that context.  Resume the other tcrs, then return what the
   function returned */

int
gc_like_from_xp(ExceptionInformation *xp, int(*fun)(TCR *))
{
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext)), *other_tcr;
  ExceptionInformation* other_xp;
  int result;

  suspend_other_threads();
  if (lisp_global(GC_INHIBIT_COUNT) != 0) {
    resume_other_threads();
    return 0;
  }

  xpGPR(xp, allocptr) = VOID_ALLOCPTR;
  xpGPR(xp, allocbase) = VOID_ALLOCPTR;

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
    


  result = fun(tcr);

  resume_other_threads();

  return result;

}


/* Returns #bytes freed by invoking GC */

int
gc_from_tcr(TCR *tcr)
{
  area *a;
  BytePtr oldfree, newfree;
  BytePtr oldend, newend;

  a = active_dynamic_area;
  oldend = a->high;
  oldfree = a->active;
  gc(tcr);
  newfree = a->active;
  newend = a->high;
  return ((oldfree-newfree)+(newend-oldend));
}

int
gc_from_xp(ExceptionInformation *xp)
{
  int status = gc_like_from_xp(xp, gc_from_tcr);

  freeGCptrs();
  return status;
}

int
purify_from_xp(ExceptionInformation *xp)
{
  return gc_like_from_xp(xp, purify);
}

int
impurify_from_xp(ExceptionInformation *xp)
{
  return gc_like_from_xp(xp, impurify);
}


void
protect_area(protected_area_ptr p)
{
  BytePtr start = p->start;
  natural n = p->protsize;

  if (n && ! p->nprot) {
    ProtectMemory(start, n);
    p->nprot = n;
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


Boolean
is_write_fault(ExceptionInformation *xp, siginfo_t *info)
{
  /* use the siginfo if it's available.  Some versions of Linux
     don't propagate the DSISR and TRAP fields correctly from
     64- to 32-bit handlers.
  */
  if (info) {
    /* 
       To confuse matters still further, the value of SEGV_ACCERR
       varies quite a bit among LinuxPPC variants (the value defined
       in the header files varies, and the value actually set by
       the kernel also varies.  So far, we're only looking at the
       siginfo under Linux and Linux always seems to generate
       SIGSEGV, so check for SIGSEGV and check the low 16 bits
       of the si_code.
    */
    return ((info->si_signo == SIGSEGV) &&
	    ((info->si_code & 0xff) == (SEGV_ACCERR & 0xff)));
  }
  return(((xpDSISR(xp) & (1 << 25)) != 0) &&
	 (xpTRAP(xp) == 
#ifdef LINUX
0x0300
#endif
#ifdef DARWIN
0x0300/0x100
#endif
)
	 );
#if 0 
  /* Maybe worth keeping around; not sure if it's an exhaustive
     list of PPC instructions that could cause a WP fault */
  /* Some OSes lose track of the DSISR and DSR SPRs, or don't provide
     valid values of those SPRs in the context they provide to
     exception handlers.  Look at the opcode of the offending
     instruction & recognize 32-bit store operations */
  opcode instr = *(xpPC(xp));

  if (xp->regs->trap != 0x300) {
    return 0;
  }
  switch (instr >> 26) {
  case 47:			/* STMW */
  case 36:			/* STW */
  case 37:			/* STWU */
    return 1;
  case 31:
    switch ((instr >> 1) & 1023) {
    case 151:			/* STWX */
    case 183:			/* STWUX */
      return 1;
    default:
      return 0;
    }
  default:
    return 0;
  }
#endif
}

OSStatus
handle_protection_violation(ExceptionInformation *xp, siginfo_t *info)
{
  BytePtr addr;
  protected_area_ptr area;
  protection_handler *handler;
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));

  if (! is_write_fault(xp, info)) {
    return -1;
  }

  if (info) {
    addr = (BytePtr)(info->si_addr);
  } else {
    addr = (BytePtr) ((natural) (xpDAR(xp)));
  }
  area = find_protected_area(addr);

  if (area == NULL) {		/* Don't know why this fault happened. */
    return -1;
  }
  
  handler = protection_handlers[area->why];

  return handler(xp, area, addr);
}


protected_area_ptr
new_protected_area(BytePtr start, BytePtr end, lisp_protection_kind reason, natural protsize, Boolean now)
{
  protected_area_ptr p = (protected_area_ptr) allocate(sizeof(protected_area));
  
  if (p == NULL) return NULL;
  p->protsize = protsize;
  p->nprot = 0;
  p->start = start;
  p->end = end;
  p->why = reason;
  p->next = AllProtectedAreas;

  AllProtectedAreas = p;
  if (now) {
    protect_area(p);
  }
  
  return p;
}

/*
  Un-protect the first nbytes bytes in specified area.
  Note that this may cause the area to be empty.
*/
void
unprotect_area_prefix(protected_area_ptr area, size_t delta)
{
  unprotect_area(area);
  area->start += delta;
  if ((area->start + area->protsize) <= area->end) {
    protect_area(area);
  }
}


/*
  Extend the protected area, causing the preceding nbytes bytes
  to be included and protected.
*/
void
protect_area_prefix(protected_area_ptr area, size_t delta)
{
  unprotect_area(area);
  area->start -= delta;
  protect_area(area);
}



OSStatus
do_hard_stack_overflow(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(area,addr)
#endif
  reset_lisp_process(xp);
  return -1;
}

extern area*
allocate_vstack(natural useable);       /* This is in "pmcl-kernel.c" */

extern area*
allocate_tstack(natural useable);       /* This is in "pmcl-kernel.c" */

#ifdef EXTEND_VSTACK
Boolean
catch_frame_p(lisp_frame *spPtr)
{
  catch_frame* catch = (catch_frame *) untag(lisp_global(CATCH_TOP));

  for (; catch; catch = (catch_frame *) untag(catch->link)) {
    if (spPtr == ((lisp_frame *) catch->csp)) {
      return true;
    }
  }
  return false;
}
#endif

Boolean
unwind_protect_cleanup_frame_p(lisp_frame *spPtr)
{
  if ((spPtr->savevsp == (LispObj)NULL) ||  /* The frame to where the unwind-protect will return */
      (((spPtr->backlink)->savevsp) == (LispObj)NULL)) {  /* The frame that returns to the kernel  from the cleanup form */
    return true;
  } else {
    return false;
  }
}

Boolean
lexpr_entry_frame_p(lisp_frame *spPtr)
{
  LispObj savelr = spPtr->savelr;
  LispObj lexpr_return = (LispObj) lisp_global(LEXPR_RETURN);
  LispObj lexpr_return1v = (LispObj) lisp_global(LEXPR_RETURN1V);
  LispObj ret1valn = (LispObj) lisp_global(RET1VALN);

  return
    (savelr == lexpr_return1v) ||
    (savelr == lexpr_return) ||
    ((savelr == ret1valn) &&
     (((spPtr->backlink)->savelr) == lexpr_return));
}

Boolean
lisp_frame_p(lisp_frame *spPtr)
{
  LispObj savefn;
  /* We can't just look at the size of the stack frame under the EABI
     calling sequence, but that's the first thing to check. */
  if (((lisp_frame *) spPtr->backlink) != (spPtr+1)) {
    return false;
  }
  savefn = spPtr->savefn;
  return (savefn == 0) || (fulltag_of(savefn) == fulltag_misc);
  
}


int ffcall_overflow_count = 0;

/* Find a frame that is neither a catch frame nor one of the
   lexpr_entry frames We don't check for non-lisp frames here because
   we'll always stop before we get there due to a dummy lisp frame
   pushed by .SPcallback that masks out the foreign frames.  The one
   exception is that there is a non-lisp frame without a valid VSP
   while in the process of ppc-ff-call. We recognize that because its
   savelr is NIL.  If the saved VSP itself is 0 or the savevsp in the
   next frame is 0, then we're executing an unwind-protect cleanup
   form, and the top stack frame belongs to its (no longer extant)
   catch frame.  */

#ifdef EXTEND_VSTACK
lisp_frame *
find_non_catch_frame_from_xp (ExceptionInformation *xp)
{
  lisp_frame *spPtr = (lisp_frame *) xpGPR(xp, sp);
  if ((((natural) spPtr) + sizeof(lisp_frame)) != ((natural) (spPtr->backlink))) {
    ffcall_overflow_count++;          /* This is mostly so I can breakpoint here */
  }
  for (; !lisp_frame_p(spPtr)  || /* In the process of ppc-ff-call */
         unwind_protect_cleanup_frame_p(spPtr) ||
         catch_frame_p(spPtr) ||
         lexpr_entry_frame_p(spPtr) ; ) {
     spPtr = spPtr->backlink;
     };
  return spPtr;
}
#endif

#ifdef EXTEND_VSTACK
Boolean
db_link_chain_in_area_p (area *a)
{
  LispObj *db = (LispObj *) lisp_global(DB_LINK),
          *high = (LispObj *) a->high,
          *low = (LispObj *) a->low;
  for (; db; db = (LispObj *) *db) {
    if ((db >= low) && (db < high)) return true;
  };
  return false;
}
#endif




/* Note: CURRENT_VS (CURRENT_TS) is always either the area containing
  the current value of VSP (TSP) or an older area.  */

OSStatus
do_vsp_overflow (ExceptionInformation *xp, BytePtr addr)
{
  TCR* tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
  area *a = tcr->vs_area;
  protected_area_ptr vsp_soft = a->softprot;
  unprotect_area(vsp_soft);
  signal_stack_soft_overflow(xp,vsp);
  return 0;
}


OSStatus
do_tsp_overflow (ExceptionInformation *xp, BytePtr addr)
{
  TCR* tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
  area *a = tcr->ts_area;
  protected_area_ptr tsp_soft = a->softprot;
  unprotect_area(tsp_soft);
  signal_stack_soft_overflow(xp,tsp);
  return 0;
}

OSStatus
do_soft_stack_overflow(ExceptionInformation *xp, protected_area_ptr prot_area, BytePtr addr)
{
  /* Trying to write into a guard page on the vstack or tstack.
     Allocate a new stack segment, emulate stwu and stwux for the TSP, and
     signal an error_stack_overflow condition.
      */
  lisp_protection_kind which = prot_area->why;
  Boolean on_TSP = (which == kTSPsoftguard);

  if (on_TSP) {
    return do_tsp_overflow(xp, addr);
   } else {
    return do_vsp_overflow(xp, addr);
   }
}

OSStatus
do_spurious_wp_fault(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(xp,area,addr)
#endif
  return -1;
}

/*
  We have a couple of choices here.  We can simply unprotect the page
  and let the store happen on return, or we can try to emulate writes
  that we know will involve an intergenerational reference.  Both are
  correct as far as EGC constraints go, but the latter approach is
  probably more efficient.  (This only matters in the case where the
  GC runs after this exception handler returns but before the write
  actually happens.  If we didn't emulate node stores here, the EGC
  would scan the newly-writen page, find nothing interesting, and
  run to completion.  This thread will try the write again afer it
  resumes, the page'll be re-protected, and we'll have taken this
  fault twice.  The whole scenario shouldn't happen very often, but
  (having already taken a fault and committed to an mprotect syscall)
  we might as well emulate stores involving intergenerational references,
  since they're pretty easy to identify.

  Note that cases involving two or more threads writing to the same
  page (before either of them can run this handler) is benign: one
  invocation of the handler will just unprotect an unprotected page in
  that case.

  If there are GCs (or any other suspensions of the thread between
  the time that the write fault was detected and the time that the
  exception lock is obtained) none of this stuff happens.
*/

/*
  Return true (and emulate the instruction) iff:
  a) the fault was caused by an "stw rs,d(ra)" or "stwx rs,ra.rb"
     instruction.
  b) RS is a node register (>= fn)
  c) RS is tagged as a cons or vector
  d) RS is in some ephemeral generation.
  This is slightly conservative, since RS may be no younger than the
  EA being written to.
*/
Boolean
is_ephemeral_node_store(ExceptionInformation *xp, BytePtr ea)
{
  if (((ptr_to_lispobj(ea)) & 3) == 0) {
    opcode instr = *xpPC(xp);
    
    if (X_opcode_p(instr,major_opcode_X31,minor_opcode_STWX) ||
        major_opcode_p(instr, major_opcode_STW)) {
      LispObj 
        rs = RS_field(instr), 
        rsval = xpGPR(xp,rs),
        tag = fulltag_of(rsval);
      
      if (rs >= fn) {
        if ((tag == fulltag_misc) || (tag == fulltag_cons)) {
          if (((BytePtr)ptr_from_lispobj(rsval) > tenured_area->high) &&
              ((BytePtr)ptr_from_lispobj(rsval) < active_dynamic_area->high)) {
            *(LispObj *)ea = rsval;
            return true;
          }
        }
      }
    }
  }
  return false;
}

      





OSStatus
handle_sigfpe(ExceptionInformation *xp, TCR *tcr)
{
  (void) zero_fpscr(tcr);
  enable_fp_exceptions();


  tcr->lisp_fpscr.words.l =  xpFPSCR(xp) & ~_FPU_RESERVED;

  /* 'handle_fpux_binop' scans back from the specified PC until it finds an FPU
     operation; there's an FPU operation right at the PC, so tell it to start
     looking one word beyond */
  return handle_fpux_binop(xp, (pc)((natural)(xpPC(xp))+4));
}

    
int
altivec_present = 1;
		  


OSStatus
PMCL_exception_handler(int xnum, 
                       ExceptionInformation *xp, 
                       TCR *tcr, 
                       siginfo_t *info)
{
  unsigned oldMQ;
  OSStatus status = -1;
  pc program_counter;
  opcode instruction = 0;


  program_counter = xpPC(xp);
  
  if ((xnum == SIGILL) | (xnum == SIGTRAP)) {
    instruction = *program_counter;
  }

  if (instruction == ALLOC_TRAP_INSTRUCTION) {
    status = handle_alloc_trap(xp, tcr);
  } else if ((xnum == SIGSEGV) ||
	     (xnum == SIGBUS)) {
    if (tcr->suspend_total != 
        tcr->suspend_total_on_exception_entry) {
      /*
        This thread's been suspended since the exception occurred
        (possibly by the GC); that makes it hard to do anything
        meaningful about protection violations - there's no guarantee
        that the addess in question is still protected, the object
        being written to may have moved, etc.  Retry the instuction.
        If it faults again, hopefully it'll eventually do so in an
        environment where this handler runs without having been
        suspended.  Note that a pending write to tenured space hasn't
        happened yet.
      */
      status = 0;
    } else {
      status = handle_protection_violation(xp, info);
    }
  } else if (xnum == SIGFPE) {
    status = handle_sigfpe(xp, tcr);
  } else if ((xnum == SIGILL) || (xnum == SIGTRAP)) {
    if (instruction == GC_TRAP_INSTRUCTION) {
      status = handle_gc_trap(xp, tcr);
    } else if (IS_UUO(instruction)) {
      status = handle_uuo(xp, instruction, program_counter);
    } else if (is_conditional_trap(instruction)) {
      status = handle_trap(xp, instruction, program_counter);
    }
  } else if (xnum == SIGNAL_FOR_PROCESS_INTERRUPT) {
    callback_for_trap(nrs_CMAIN.vcell, xp, 0, TRI_instruction(TO_GT,nargs,0),0, 0);
    status = 0;
  }

  return status;
}

void
adjust_exception_pc(ExceptionInformation *xp, int delta)
{
  xpPC(xp) += (delta >> 2);
}


/* 
  This wants to scan backwards until "where" points to an instruction
   whose major opcode is either 63 (double-float) or 59 (single-float)
*/

OSStatus
handle_fpux_binop(ExceptionInformation *xp, pc where)
{
  OSStatus err;
  opcode *there = (opcode *) where, instr, errnum;
  int i = TRAP_LOOKUP_TRIES, delta = 0;
  
  while (i--) {
    instr = *--there;
    delta -= 4;
    if (codevec_hdr_p(instr)) {
      return -1;
    }
    if (major_opcode_p(instr, major_opcode_FPU_DOUBLE)) {
      errnum = error_FPU_exception_double;
      break;
    }

    if (major_opcode_p(instr, major_opcode_FPU_SINGLE)) {
      errnum = error_FPU_exception_short;
      break;
    }
  }
  
  err = handle_error(xp, errnum, rcontext, 0,  there);
  /* Yeah, we said "non-continuable".  In case we ever change that ... */
  
  adjust_exception_pc(xp, delta);
  xpFPSCR(xp)  &=  0x03fff;
  
  return err;

}

OSStatus
handle_uuo(ExceptionInformation *xp, opcode the_uuo, pc where) 
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(where)
#endif
  unsigned 
    minor = UUO_MINOR(the_uuo),
    rt = 0x1f & (the_uuo >> 21),
    ra = 0x1f & (the_uuo >> 16),
    rb = 0x1f & (the_uuo >> 11),
    errnum = 0x3ff & (the_uuo >> 16);

  OSStatus status = -1;

  int bump = 4;

  switch (minor) {

  case UUO_ZERO_FPSCR:
    status = 0;
    xpFPSCR(xp) = 0;
    break;


  case UUO_INTERR:
    status = handle_error(xp, errnum, rb, 0,  where);
    break;

  case UUO_INTCERR:
    status = handle_error(xp, errnum, rb, 1,  where);
    if (errnum == error_udf_call) {
      /* If lisp's returned from a continuable undefined-function call,
	 it's put a code vector in the xp's PC.  Don't advance the
	 PC ... */
      bump = 0;
    }
    break;

  case UUO_FPUX_BINOP:
    status = handle_fpux_binop(xp, where);
    bump = 0;
    break;

  default:
    status = -1;
    bump = 0;
  }
  
  if ((!status) && bump) {
    adjust_exception_pc(xp, bump);
  }
  return status;
}

natural
register_codevector_contains_pc (natural lisp_function, pc where)
{
  natural code_vector, size;

  if ((fulltag_of(lisp_function) == fulltag_misc) &&
      (header_subtag(header_of(lisp_function)) == subtag_function)) {
    code_vector = deref(lisp_function, 1);
    size = header_element_count(header_of(code_vector)) << 2;
    if ((untag(code_vector) < (natural)where) && 
	((natural)where < (code_vector + size)))
      return(code_vector);
  }

  return(0);
}

/* Callback to lisp to handle a trap. Need to translate the
   PC (where) into one of two forms of pairs:

   1. If PC is in fn or nfn's code vector, use the register number
      of fn or nfn and the index into that function's code vector.
   2. Otherwise use 0 and the pc itself
*/
void
callback_for_trap (LispObj callback_macptr, ExceptionInformation *xp, pc where,
                   natural arg1, natural arg2, natural arg3)
{
  natural code_vector = register_codevector_contains_pc(xpGPR(xp, fn), where);
  unsigned register_number = fn;
  natural index = (natural)where;

  if (code_vector == 0) {
    register_number = nfn;
    code_vector = register_codevector_contains_pc(xpGPR(xp, nfn), where);
  }
  if (code_vector == 0)
    register_number = 0;
  else
    index = ((natural)where - (code_vector + misc_data_offset)) >> 2;
  callback_to_lisp(callback_macptr, xp, register_number, index, arg1, arg2, arg3);
}

void
callback_to_lisp (LispObj callback_macptr, ExceptionInformation *xp,
                  natural arg1, natural arg2, natural arg3, natural arg4, natural arg5)
{
  sigset_t mask;
  natural  callback_ptr, i;
  area *a;

  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));

  /* Put the active stack pointer where .SPcallback expects it */
  a = tcr->cs_area;
  a->active = (BytePtr) ptr_from_lispobj(xpGPR(xp, sp));

  /* Copy globals from the exception frame to tcr */
  tcr->save_allocptr = (void *)ptr_from_lispobj(xpGPR(xp, allocptr));
  tcr->save_allocbase = (void *)ptr_from_lispobj(xpGPR(xp, allocbase));
  tcr->save_vsp = (LispObj*) ptr_from_lispobj(xpGPR(xp, vsp));
  tcr->save_tsp = (LispObj*) ptr_from_lispobj(xpGPR(xp, tsp));
  
  enable_fp_exceptions();


  /* Call back.
     Lisp will handle trampolining through some code that
     will push lr/fn & pc/nfn stack frames for backtrace.
  */
  callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
  UNLOCK(lisp_global(EXCEPTION_LOCK), tcr);
  ((void (*)())callback_ptr) (xp, arg1, arg2, arg3, arg4, arg5);
  LOCK(lisp_global(EXCEPTION_LOCK), tcr);



  /* Copy GC registers back into exception frame */
  xpGPR(xp, allocbase) = (LispObj) ptr_to_lispobj(tcr->save_allocbase);
  xpGPR(xp, allocptr) = (LispObj) ptr_to_lispobj(tcr->save_allocptr);
}

area *
allocate_no_stack (natural size)
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(size)
#endif

  return (area *) NULL;
}






/* callback to (symbol-value cmain) if it is a macptr, 
   otherwise report cause and function name to console.
   Returns noErr if exception handled OK */
OSStatus
handle_trap(ExceptionInformation *xp, opcode the_trap, pc where)
{
  unsigned  instr, err_arg1 = 0, err_arg2 = 0, err_arg3 = 0;
  int       ra, rs, fn_reg = 0;
  char *    error_msg = NULL;
  char      name[kNameBufLen];
  LispObj   cmain = nrs_CMAIN.vcell;
  Boolean   event_poll_p = false;
  int old_interrupt_level = 0;
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));

  /* If we got here, "the_trap" is either a TRI or a TR instruction.
     It's a TRI instruction iff its major opcode is major_opcode_TRI. */

  /* If it's a "trllt" instruction where RA == sp, it's a failed 
     control stack overflow check.  In that case:
     
     a) We're in "yellow zone" mode if the value of the
     lisp_global(CS_OVERFLOW_LIMIT) is CS_OVERFLOW_FORCE_LIMIT.  If
     we're not already in yellow zone mode, attempt to create a new
     thread and continue execution on its stack. If that fails, call
     signal_stack_soft_overflow to enter yellow zone mode and signal
     the condition to lisp.
     
     b) If we're already in "yellow zone" mode, then:
     
     1) if the SP is past the current control-stack area's hard
     overflow limit, signal a "hard" stack overflow error (e.g., throw
     to toplevel as quickly as possible. If we aren't in "yellow zone"
     mode, attempt to continue on another thread first.
     
     2) if SP is "well" (> 4K) below its soft overflow limit, set
     lisp_global(CS_OVERFLOW_LIMIT) to its "real" value.  We're out of
     "yellow zone mode" in this case.
     
     3) Otherwise, do nothing.  We'll continue to trap every time
     something gets pushed on the control stack, so we should try to
     detect and handle all of these cases fairly quickly.  Of course,
     the trap overhead is going to slow things down quite a bit.
     */

  if (X_opcode_p(the_trap,major_opcode_X31,minor_opcode_TR) &&
      (RA_field(the_trap) == sp) &&
      (TO_field(the_trap) == TO_LO)) {
    area 
      *CS_area = tcr->cs_area,
      *VS_area = tcr->vs_area;
      
    natural 
      current_SP = xpGPR(xp,sp),
      current_VSP = xpGPR(xp,vsp);

    if (current_SP  < (natural) (CS_area->hardlimit)) {
      /* If we're not in soft overflow mode yet, assume that the
         user has set the soft overflow size very small and try to
         continue on another thread before throwing to toplevel */
      if ((tcr->cs_limit == CS_OVERFLOW_FORCE_LIMIT)) {
        reset_lisp_process(xp);
      }
    } else {
      if (tcr->cs_limit == CS_OVERFLOW_FORCE_LIMIT) {
        /* If the control stack pointer is at least 4K away from its soft limit
	   and the value stack pointer is at least 4K away from its soft limit,
           stop trapping.  Else keep trapping. */
        if ((current_SP > (natural) ((CS_area->softlimit)+4096)) &&
	    (current_VSP > (natural) ((VS_area->softlimit)+4096))) {
	  protected_area_ptr vs_soft = VS_area->softprot;
	  if (vs_soft->nprot == 0) {
	    protect_area(vs_soft);
	  }
          tcr->cs_limit = ptr_to_lispobj(CS_area->softlimit);
        }
      } else {
	tcr->cs_limit = ptr_to_lispobj(CS_area->hardlimit);	  
	signal_stack_soft_overflow(xp, sp);
      }
    }
    
    adjust_exception_pc(xp, 4);
    return noErr;
  } else {
    if (the_trap == LISP_BREAK_INSTRUCTION) {
      char *message =  (char *) ptr_from_lispobj(xpGPR(xp,3));
      set_xpPC(xp, xpLR(xp));
      if (message == NULL) {
	message = "Lisp Breakpoint";
      }
      lisp_Debugger(xp, debug_entry_dbg, message);
      return noErr;
    }
    if (the_trap == QUIET_LISP_BREAK_INSTRUCTION) {
      adjust_exception_pc(xp,4);
      lisp_Debugger(xp, debug_entry_dbg, "Lisp Breakpoint");
      return noErr;
    }
    /*
      twlle ra,rb is used to detect tlb overflow, where RA = current
      limit and RB = index to use.
    */
    if ((X_opcode_p(the_trap, 31, minor_opcode_TR)) && 
        (TO_field(the_trap) == (TO_LO|TO_EQ))) {
      if (extend_tcr_tlb(tcr, xp, RA_field(the_trap), RB_field(the_trap))) {
        return noErr;
      }
      return -1;
    }

    if ((fulltag_of(cmain) == fulltag_misc) &&
        (header_subtag(header_of(cmain)) == subtag_macptr)) {
      if (the_trap == TRI_instruction(TO_GT,nargs,0)) {
        /* reset interrup_level, interrupt_pending */
        tcr->interrupt_level = 0;
        tcr->interrupt_pending = 0;
      }
      
      callback_for_trap(cmain, xp,  where, (natural) the_trap,  0, 0);
      adjust_exception_pc(xp, 4);
      return(noErr);
    }
    return -1;
  }
}


/* Look at up to TRAP_LOOKUP_TRIES instrs before trap instr for a pattern.
   Stop if subtag_code_vector is encountered. */
unsigned
scan_for_instr( unsigned target, unsigned mask, pc where )
{
  int i = TRAP_LOOKUP_TRIES;

  while( i-- ) {
    unsigned instr = *(--where);
    if ( codevec_hdr_p(instr) ) {
      return 0;
    } else if ( match_instr(instr, mask, target) ) {
      return instr;
    }
  }
  return 0;
}


void non_fatal_error( char *msg )
{
  fprintf( stderr, "Non-fatal error: %s.\n", msg );
  fflush( stderr );
}

/* The main opcode.  */

int 
is_conditional_trap(opcode instr)
{
  unsigned to = TO_field(instr);
  int is_tr = X_opcode_p(instr,major_opcode_X31,minor_opcode_TR);

#ifndef MACOS
  if ((instr == LISP_BREAK_INSTRUCTION) ||
      (instr == QUIET_LISP_BREAK_INSTRUCTION)) {
    return 1;
  }
#endif
  if (is_tr || major_opcode_p(instr,major_opcode_TRI)) {
    /* A "tw/td" or "twi/tdi" instruction.  To be unconditional, the
       EQ bit must be set in the TO mask and either the register
       operands (if "tw") are the same or either both of the signed or
       both of the unsigned inequality bits must be set. */
    if (! (to & TO_EQ)) {
      return 1;			/* Won't trap on EQ: conditional */
    }
    if (is_tr && (RA_field(instr) == RB_field(instr))) {
      return 0;			/* Will trap on EQ, same regs: unconditional */
    }
    if (((to & (TO_LO|TO_HI)) == (TO_LO|TO_HI)) || 
	((to & (TO_LT|TO_GT)) == (TO_LT|TO_GT))) {
      return 0;			/* Will trap on EQ and either (LT|GT) or (LO|HI) : unconditional */
    }
    return 1;			/* must be conditional */
  }
  return 0;			/* Not "tw/td" or "twi/tdi".  Let
                                   debugger have it */
}

OSStatus
handle_error(ExceptionInformation *xp, unsigned errnum, unsigned rb, unsigned continuable, pc where)
{
  LispObj   pname;
  LispObj   errdisp = nrs_ERRDISP.vcell;

  if ((fulltag_of(errdisp) == fulltag_misc) &&
      (header_subtag(header_of(errdisp)) == subtag_macptr)) {
    /* errdisp is a macptr, we can call back to lisp */
    callback_for_trap(errdisp, xp, where, errnum, rb, continuable);
    return(0);
    }

  return(-1);
}
	       

/* 
   Current thread has all signals masked.  Before unmasking them,
   make it appear that the current thread has been suspended.
   (This is to handle the case where another thread is trying
   to GC before this thread is able to sieze the exception lock.)
*/
int
prepare_to_wait_for_exception_lock(TCR *tcr, 
				   ExceptionInformationPowerPC *context)
{
  int old_valence = tcr->valence;

  tcr->pending_exception_context = context;
  tcr->suspend_total_on_exception_entry =
    tcr->suspend_total;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;

  ALLOW_EXCEPTIONS(context);
  return old_valence;
}  

void
wait_for_exception_lock_in_handler(TCR *tcr, 
				   ExceptionInformationPowerPC *context,
				   xframe_list *xf)
{

  LOCK(lisp_global(EXCEPTION_LOCK), tcr);
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
}

/* 
   If an interrupt is pending on exception exit, try to ensure
   that the thread sees it as soon as it's able to run.
*/
void
raise_pending_interrupt(TCR *tcr)
{
  if (tcr->interrupt_level > 0) {
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
signal_handler(int signum, siginfo_t *info, ExceptionInformationPowerPC  *context, TCR *tcr)
{
  xframe_list xframe_link;
  int old_valence;
#ifdef LINUX

  tcr = (TCR *) get_interrupt_tcr(false);
  
  /* The signal handler's entered with all signals (notably the
     thread_suspend signal) blocked.  Don't allow any other signals
     (notably the thread_suspend signal) to preempt us until we've
     set the TCR's xframe slot to include the current exception
     context.
  */

  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
#endif
  
  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
  if ((noErr != PMCL_exception_handler(signum, context, tcr, info))) {
    char msg[512];
    snprintf(msg, sizeof(msg), "Unhandled exception %d at 0x%lx, context->regs at #x%lx", signum, xpPC(context), (natural)xpGPRvector(context));
    if (lisp_Debugger(context, signum, msg)) {
      (tcr->flags |= TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }

  unlock_exception_lock_in_handler(tcr);

  /* This thread now looks like a thread that was suspended while
     executing lisp code.  If some other thread gets the exception
     lock and GCs, the context (this thread's suspend_context) will
     be updated.  (That's only of concern if it happens before we
     can return to the kernel/to the Mach exception handler).
  */
#ifdef LINUX
  exit_signal_handler(tcr, old_valence);
  raise_pending_interrupt(tcr);
#endif
}

/*
  If it looks like we're in the middle of an atomic operation, make
  it seem as if that operation is either complete or hasn't started
  yet.

  The cases handled include:

  a) storing into a newly-allocated lisp frame on the stack.
  b) marking a newly-allocated TSP frame as containing "raw" data.
  c) consing: the GC has its own ideas about how this should be
     handled, but other callers would be best advised to back
     up or move forward, according to whether we're in the middle
     of allocating a cons cell or allocating a uvector.
  d) a STMW to the vsp
  e) EGC write-barrier subprims.
*/

extern pc
  egc_write_barrier_start,
  egc_write_barrier_end, 
  egc_store_node_conditional, 
  egc_set_hash_key,
  egc_gvset,
  egc_rplaca,
  egc_rplacd;

void
pc_luser_xp(ExceptionInformationPowerPC *xp, TCR *tcr)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter;
  lisp_frame *frame = (lisp_frame *)ptr_from_lispobj(xpGPR(xp,sp));
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  int allocptr_tag = fulltag_of(cur_allocptr);
  
  if ((program_counter < egc_write_barrier_end) && 
      (program_counter >= egc_write_barrier_start)) {
    LispObj *ea = 0, val, root;
    bitvector refbits = (bitvector)(lisp_global(REFBITS));
    Boolean need_store = true, need_check_memo = true, need_memoize_root = false;

    if (program_counter >= egc_store_node_conditional) {
      if ((program_counter == egc_store_node_conditional) || ! (xpCCR(xp) & 0x20000000)) {
        /* The conditional store either hasn't been attempted yet, or
           has failed.  No need to adjust the PC, or do memoization. */
        return;
      }
      val = xpGPR(xp,arg_z);
      ea = (LispObj*)(xpGPR(xp,arg_x) + xpGPR(xp,imm4));
      xpGPR(xp,arg_z) = t_value;
      need_store = false;
    } else if (program_counter >= egc_set_hash_key) {
      root = xpGPR(xp,arg_x);
      ea = (LispObj *) (root+xpGPR(xp,arg_y)+fulltag_misc);
      need_memoize_root = true;
    } else if (program_counter >= egc_gvset) {
      ea = (LispObj *) (xpGPR(xp,arg_x)+xpGPR(xp,arg_y)+fulltag_misc);
      val = xpGPR(xp,arg_z);
    } else if (program_counter >= egc_rplacd) {
      ea = (LispObj *) untag(xpGPR(xp,arg_y));
      val = xpGPR(xp,arg_z);
    } else {                      /* egc_rplaca */
      ea =  ((LispObj *) untag(xpGPR(xp,arg_y)))+1;
      val = xpGPR(xp,arg_z);
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
    set_xpPC(xp, xpLR(xp));
    return;
  }


  if (instr == MARK_TSP_FRAME_INSTRUCTION) {
    LispObj tsp_val = xpGPR(xp,tsp);
    
    ((LispObj *)ptr_from_lispobj(tsp_val))[1] = tsp_val;
    adjust_exception_pc(xp, 4);
    return;
  }
  
  if (frame->backlink == (frame+1)) {
    if (
#ifdef PPC64
        (major_opcode_p(instr, major_opcode_DS_STORE64)) &&
        (DS_VARIANT_FIELD(instr) == DS_STORE64_VARIANT_STD) &&
#else
        (major_opcode_p(instr, major_opcode_STW)) && 
#endif
	(RA_field(instr) == sp) &&
	/* There are a few places in the runtime that store into
	   a previously-allocated frame atop the stack when
	   throwing values around.  We only care about the case
	   where the frame was newly allocated, in which case
	   there must have been a CREATE_LISP_FRAME_INSTRUCTION
	   a few instructions before the current program counter.
	   (The whole point here is that a newly allocated frame
	   might contain random values that we don't want the
	   GC to see; a previously allocated frame should already
	   be completely initialized.)
	*/
	((program_counter[-1] == CREATE_LISP_FRAME_INSTRUCTION) ||
	 (program_counter[-2] == CREATE_LISP_FRAME_INSTRUCTION) ||
	 (program_counter[-3] == CREATE_LISP_FRAME_INSTRUCTION)))  {
#ifdef PPC64
      int disp = DS_field(instr) << 2;
#else      
      int disp = D_field(instr);
#endif
      
      if (disp < (4*node_size)) {
	frame->savevsp = 0;
	if (disp < (3*node_size)) {
	  frame->savelr = 0;
	  if (disp == node_size) {
	    frame->savefn = 0;
	  }
	}
      }
      return;
    }
  }

  if (allocptr_tag != tag_fixnum) {
    int disp = allocptr_displacement(xp);

    if (disp) {
      /* We're "at" the alloc_trap.  If this is happening for the
         GC's benefit (the GC has just suspended the thread that
         owns xp), set allocbase to VOID_ALLOCPTR and pretend that
         allocptr was just decremented (by disp) from there; leave
         the PC pointing to the alloc_trap.  If this is happening
         in response to a PROCESS-INTERRUPT request, we can't have
         entered the trap handler yet: back out of the previous
         instruction (which subtracted "disp" from allocptr) and
         arrange to execute it again after the interrupt.
      */
      if (tcr) {
        update_bytes_allocated(tcr, (void *) ptr_from_lispobj(cur_allocptr + disp));
        xpGPR(xp, allocbase) = VOID_ALLOCPTR;
        xpGPR(xp, allocptr) = VOID_ALLOCPTR - disp;
      } else {
        xpGPR(xp, allocptr) = cur_allocptr + disp;
        xpPC(xp) -=1;
      }
    } else {
      /* If we're already past the alloc_trap, finish allocating
         the object. */
      if (allocptr_tag == fulltag_cons) {
        finish_allocating_cons(xp);
      } else {
        if (allocptr_tag == fulltag_misc) {
          finish_allocating_uvector(xp);
        } else {
          Bug(xp, "what's being allocated here ?");
        }
      }
    }
  }
#ifndef PC64
  if ((major_opcode_p(instr, 47)) && /* 47 = stmw */
      (RA_field(instr) == vsp)) {
    int r;
    LispObj *vspptr = ptr_from_lispobj(xpGPR(xp,vsp));
    
    for (r = RS_field(instr); r <= 31; r++) {
      *vspptr++ = xpGPR(xp,r);
    }
    adjust_exception_pc(xp, 4);
  }
#endif
}

void
interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  if (tcr) {
    if (tcr->interrupt_level < 0) {
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
	*/
	if (tcr->valence != TCR_STATE_LISP) {
	  tcr->interrupt_level = (1 << fixnumshift);
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
#ifdef DARWIN
  /* 
     No, I'm not proud of this.
     We have to use DarwinSigReturn to work around an OSX G5 bug.
     To make matters worse, Darwin loses track of the MSR[FE0,FE1]
     bits in a thread's context when it receives a signal via
     pthread_kill.  We can at least fix it in this case by returning
     to code which uses a UUO to set the MSR[FE0,FE1] bits, and
     make the handler for -that- UUO return to the real address that
     was interrupted via pthread_kill.

     I'm so ashamed.
  */
  { 
    
    lisp_frame *cur_frame = ((lisp_frame *)xpGPR(context,sp)),
      *new_frame = cur_frame-1;
    xpGPR(context,sp) = (LispObj)new_frame;
    new_frame->backlink = cur_frame;
    new_frame->savevsp=0;
    new_frame->savefn=0;
    new_frame->savelr = (LispObj)xpPC(context);
    xpPC(context) = (pc)enable_fp_exceptions;
    DarwinSigReturn(context);
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
    | SA_SIGINFO
#ifdef DARWIN
#ifdef PPC64
    | SA_64REGSET
#endif
#endif
    ;

  sigaction(signo, &sa, NULL);
}

void
install_pmcl_exception_handlers()
{
#ifndef DARWIN
  extern int no_sigtrap;
  install_signal_handler(SIGILL, (__sighandler_t)signal_handler);
  if (no_sigtrap != 1) {
    install_signal_handler(SIGTRAP, (__sighandler_t)signal_handler);
  }
  install_signal_handler(SIGBUS,  (__sighandler_t)signal_handler);
  install_signal_handler(SIGSEGV, (__sighandler_t)signal_handler);
  install_signal_handler(SIGFPE, (__sighandler_t)signal_handler);
#endif
  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT,
			 (__sighandler_t)interrupt_handler);
  signal(SIGPIPE, SIG_IGN);
}


void
unprotect_all_areas()
{
  protected_area_ptr p;

  for(p = AllProtectedAreas, AllProtectedAreas = NULL; p; p = p->next) {
    unprotect_area(p);
  }
}




void
exception_init()
{
  install_pmcl_exception_handlers();
}


void
Bug(ExceptionInformation *xp, const char *format, ...)
{
  va_list args;
  char s[512];
 
  va_start(args, format);
  vsnprintf(s, sizeof(s),format, args);
  va_end(args);
  lisp_Debugger(NULL, debug_entry_bug, s);

}

void
lisp_bug(char *string)
{
  Bug(NULL, "Bug in OpenMCL system code:\n%s", string);
}



#ifdef DARWIN

#define TCR_FROM_EXCEPTION_PORT(p) ((TCR *)((natural)p))
#define TCR_TO_EXCEPTION_PORT(tcr) ((mach_port_t)((natural)(tcr)))

lock_set_t mach_exception_lock_set;


#define LISP_EXCEPTIONS_HANDLED_MASK \
 (EXC_MASK_SOFTWARE | EXC_MASK_BAD_ACCESS | EXC_MASK_BAD_INSTRUCTION | EXC_MASK_ARITHMETIC)

/* (logcount LISP_EXCEPTIONS_HANDLED_MASK) */
#define NUM_LISP_EXCEPTIONS_HANDLED 4 

typedef struct {
  int foreign_exception_port_count;
  exception_mask_t         masks[NUM_LISP_EXCEPTIONS_HANDLED];
  mach_port_t              ports[NUM_LISP_EXCEPTIONS_HANDLED];
  exception_behavior_t behaviors[NUM_LISP_EXCEPTIONS_HANDLED];
  thread_state_flavor_t  flavors[NUM_LISP_EXCEPTIONS_HANDLED];
} MACH_foreign_exception_state;




/*
  Mach's exception mechanism works a little better than its signal
  mechanism (and, not incidentally, it gets along with GDB a lot
  better.

  Initially, we install an exception handler to handle each native
  thread's exceptions.  This process involves creating a distinguished
  thread which listens for kernel exception messages on a set of
  0 or more thread exception ports.  As threads are created, they're
  added to that port set; a thread's exception port is destroyed
  (and therefore removed from the port set) when the thread exits.

  A few exceptions can be handled directly in the handler thread;
  others require that we resume the user thread (and that the
  exception thread resumes listening for exceptions.)  The user
  thread might eventually want to return to the original context
  (possibly modified somewhat.)

  As it turns out, the simplest way to force the faulting user
  thread to handle its own exceptions is to do pretty much what
  signal() does: the exception handlng thread sets up a sigcontext
  on the user thread's stack and forces the user thread to resume
  execution as if a signal handler had been called with that
  context as an argument.  We can use a distinguished UUO at a
  distinguished address to do something like sigreturn(); that'll
  have the effect of resuming the user thread's execution in
  the (pseudo-) signal context.

  Since:
    a) we have miles of code in C and in Lisp that knows how to
    deal with Linux sigcontexts
    b) Linux sigcontexts contain a little more useful information
    (the DAR, DSISR, etc.) than their Darwin counterparts
    c) we have to create a sigcontext ourselves when calling out
    to the user thread: we aren't really generating a signal, just
    leveraging existing signal-handling code.

  we create a Linux sigcontext struct.

  Simple ?  Hopefully from the outside it is ...

  We want the process of passing a thread's own context to it to
  appear to be atomic: in particular, we don't want the GC to suspend
  a thread that's had an exception but has not yet had its user-level
  exception handler called, and we don't want the thread's exception
  context to be modified by a GC while the Mach handler thread is
  copying it around.  On Linux (and on Jaguar), we avoid this issue
  because (a) the kernel sets up the user-level signal handler and
  (b) the signal handler blocks signals (including the signal used
  by the GC to suspend threads) until tcr->xframe is set up.

  The GC and the Mach server thread therefore contend for the lock
  "mach_exception_lock_set[0]".  The Mach server thread holds the lock
  when copying exception information between the kernel and the
  user thread; the GC holds this lock during most of its execution
  (delaying exception processing until it can be done without
  GC interference.)

*/

#ifdef PPC64
#define	C_REDZONE_LEN		320
#define	C_STK_ALIGN             32
#else
#define	C_REDZONE_LEN		224
#define	C_STK_ALIGN		16
#endif
#define C_PARAMSAVE_LEN		64
#define	C_LINKAGE_LEN		48

#define TRUNC_DOWN(a,b,c)  (((((natural)a)-(b))/(c)) * (c))
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

void
fatal_mach_error(char *format, ...);

#define MACH_CHECK_ERROR(context,x) if (x != KERN_SUCCESS) {fatal_mach_error("Mach error while %s : %d", context, x);}


void
restore_mach_thread_state(mach_port_t thread, ExceptionInformation *lss)
{
  int i, j;
  kern_return_t kret;
#ifdef PPC64
  struct mcontext64 *mc = UC_MCONTEXT(lss);
#else
  struct mcontext * mc = UC_MCONTEXT(lss);
#endif

  /* Set the thread's FP state from the lss */
  kret = thread_set_state(thread,
                          PPC_FLOAT_STATE,
                          (thread_state_t)&(mc->fs),
                          PPC_FLOAT_STATE_COUNT);

  MACH_CHECK_ERROR("setting thread FP state", kret);

  /* The thread'll be as good as new ... */
#ifdef PPC64
  kret = thread_set_state(thread,
                          PPC_THREAD_STATE64,
                          (thread_state_t)&(mc->ss),
                          PPC_THREAD_STATE64_COUNT);
#else
  kret = thread_set_state(thread, 
                          MACHINE_THREAD_STATE,
                          (thread_state_t)&(mc->ss),
                          MACHINE_THREAD_STATE_COUNT);
#endif
  MACH_CHECK_ERROR("setting thread state", kret);
}  

/* This code runs in the exception handling thread, in response
   to an attempt to execute the UU0 at "pseudo_sigreturn" (e.g.,
   in response to a call to pseudo_sigreturn() from the specified
   user thread.
   Find that context (the user thread's R3 points to it), then
   use that context to set the user thread's state.  When this
   function's caller returns, the Mach kernel will resume the
   user thread.
*/

kern_return_t
do_pseudo_sigreturn(mach_port_t thread, TCR *tcr)
{
  ExceptionInformation *xp;

  lock_acquire(mach_exception_lock_set, 0);
  xp = tcr->pending_exception_context;
  tcr->pending_exception_context = NULL;
  tcr->valence = TCR_STATE_LISP;
  restore_mach_thread_state(thread, xp);
  raise_pending_interrupt(tcr);
  lock_release(mach_exception_lock_set, 0);
  return KERN_SUCCESS;

}  

ExceptionInformation *
create_thread_context_frame(mach_port_t thread, 
			    natural *new_stack_top)
{
#ifdef PPC64
  ppc_thread_state64_t ts;
#else
  ppc_thread_state_t ts;
#endif
  mach_msg_type_number_t thread_state_count;
  kern_return_t result;
  int i,j;
  ExceptionInformation *lss;
#ifdef PPC64
  struct mcontext64 *mc;
#else
  struct mcontext *mc;
#endif
  natural stackp, backlink;

#ifdef PPC64
  thread_state_count = PPC_THREAD_STATE64_COUNT;
  result = thread_get_state(thread,
                            PPC_THREAD_STATE64,
                            (thread_state_t)&ts,
                            &thread_state_count);
#else
  thread_state_count = MACHINE_THREAD_STATE_COUNT;
  result = thread_get_state(thread, 
                            PPC_THREAD_STATE,	/* GPRs, some SPRs  */
                            (thread_state_t)&ts,
                            &thread_state_count);
#endif
  
  if (result != KERN_SUCCESS) {
    get_tcr(true);
    Bug(NULL, "Exception thread can't obtain thread state, Mach result = %d", result);
  }
  stackp = ts.r1;
  backlink = stackp;
  stackp = TRUNC_DOWN(stackp, C_REDZONE_LEN, C_STK_ALIGN);
  stackp -= sizeof(*lss);
  lss = (ExceptionInformation *) ptr_from_lispobj(stackp);

  stackp = TRUNC_DOWN(stackp, sizeof(*mc), C_STK_ALIGN);
#ifdef PPC64
  mc = (struct mcontext64 *) ptr_from_lispobj(stackp);
#else
  mc = (struct mcontext *) ptr_from_lispobj(stackp);
#endif
  bcopy(&ts,&(mc->ss),sizeof(ts));

  thread_state_count = PPC_FLOAT_STATE_COUNT;
  thread_get_state(thread,
		   PPC_FLOAT_STATE,
		   (thread_state_t)&(mc->fs),
		   &thread_state_count);


#ifdef PPC64
  thread_state_count = PPC_EXCEPTION_STATE64_COUNT;
#else
  thread_state_count = PPC_EXCEPTION_STATE_COUNT;
#endif
  thread_get_state(thread,
#ifdef PPC64
                   PPC_EXCEPTION_STATE64,
#else
		   PPC_EXCEPTION_STATE,
#endif
		   (thread_state_t)&(mc->es),
		   &thread_state_count);


  UC_MCONTEXT(lss) = mc;
  stackp = TRUNC_DOWN(stackp, C_PARAMSAVE_LEN, C_STK_ALIGN);
  stackp -= C_LINKAGE_LEN;
  *(natural *)ptr_from_lispobj(stackp) = backlink;
  if (new_stack_top) {
    *new_stack_top = stackp;
  }
  return lss;
}

/*
  This code sets up the user thread so that it executes a "pseudo-signal
  handler" function when it resumes.  Create a linux sigcontext struct
  on the thread's stack and pass it as an argument to the pseudo-signal
  handler.

  Things are set up so that the handler "returns to" pseudo_sigreturn(),
  which will restore the thread's context.

  If the handler invokes code that throws (or otherwise never sigreturn()'s
  to the context), that's fine.

  Actually, check that: throw (and variants) may need to be careful and
  pop the tcr's xframe list until it's younger than any frame being
  entered.
*/

int
setup_signal_frame(mach_port_t thread,
		   void *handler_address,
		   int signum,
                   int code,
		   TCR *tcr)
{
#ifdef PPC64
  ppc_thread_state64_t ts;
#else
  ppc_thread_state_t ts;
#endif
  mach_msg_type_number_t thread_state_count;
  ExceptionInformation *lss;
  int i, j;
  kern_return_t result;
  natural stackp;

  lock_acquire(mach_exception_lock_set, 0);

  lss = create_thread_context_frame(thread, &stackp);
  lss->uc_onstack = 0;
  lss->uc_sigmask = (sigset_t) 0;
  tcr->pending_exception_context = lss;
  tcr->suspend_total_on_exception_entry =
    tcr->suspend_total;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;
  

  /* 
     It seems like we've created a  sigcontext on the thread's
     stack.  Set things up so that we call the handler (with appropriate
     args) when the thread's resumed.
  */

  ts.srr0 = (natural) handler_address;
  ts.srr1 = (int) xpMSR(lss) & ~MSR_FE0_FE1_MASK;
  ts.r1 = stackp;
  ts.r3 = signum;
  ts.r4 = (natural)lss;
  ts.r5 = (natural)tcr;
  ts.lr = (natural)pseudo_sigreturn;


#ifdef PPC64
  ts.r13 = xpGPR(lss,13);
  thread_set_state(thread,
                   PPC_THREAD_STATE64,
                   (thread_state_t)&ts,
                   PPC_THREAD_STATE64_COUNT);
#else
  thread_set_state(thread, 
		   MACHINE_THREAD_STATE,
		   (thread_state_t)&ts,
		   MACHINE_THREAD_STATE_COUNT);
#endif
  lock_release(mach_exception_lock_set, 0);
  return 0;
}


void
pseudo_signal_handler(int signum, ExceptionInformation *context, TCR *tcr)
{
  signal_handler(signum, NULL, context, tcr);
} 


int
thread_set_fp_exceptions_enabled(mach_port_t thread, Boolean enabled)
{
#ifdef PPC64
  ppc_thread_state64_t ts;
#else
  ppc_thread_state_t ts;
#endif
  mach_msg_type_number_t thread_state_count;

  lock_acquire(mach_exception_lock_set, 0);
#ifdef PPC64
  thread_state_count = PPC_THREAD_STATE64_COUNT;
#else
  thread_state_count = PPC_THREAD_STATE_COUNT;
#endif
  thread_get_state(thread, 
#ifdef PPC64
		   PPC_THREAD_STATE64,	/* GPRs, some SPRs  */
#else
		   PPC_THREAD_STATE,	/* GPRs, some SPRs  */
#endif
		   (thread_state_t)&ts,
		   &thread_state_count);
  if (enabled) {
    ts.srr1 |= MSR_FE0_FE1_MASK;
  } else {
    ts.srr1 &= ~MSR_FE0_FE1_MASK;
  }
  /* 
     Hack-o-rama warning (isn't it about time for such a warning?):
     pthread_kill() seems to want to lose the MSR's FE0/FE1 bits.
     Our handler for lisp's use of pthread_kill() pushes a phony
     lisp frame on the stack and force the context to resume at
     the UUO in enable_fp_exceptions(); the "saveLR" field of that
     lisp frame contains the -real- address that process_interrupt
     should have returned to, and the fact that it's in a lisp
     frame should convince the GC to notice that address if it
     runs in the tiny time window between returning from our
     interrupt handler and ... here.
     If the top frame on the stack is a lisp frame, discard it
     and set ts.srr0 to the saveLR field in that frame.  Otherwise,
     just adjust ts.srr0 to skip over the UUO.
  */
  {
    lisp_frame *tos = (lisp_frame *)ts.r1,
      *next_frame = tos->backlink;
    
    if (tos == (next_frame -1)) {
      ts.srr0 = tos->savelr;
      ts.r1 = (LispObj) next_frame;
    } else {
      ts.srr0 += 4;
    }
  }
  thread_set_state(thread, 
#ifdef PPC64
		   PPC_THREAD_STATE64,	/* GPRs, some SPRs  */
#else
		   PPC_THREAD_STATE,	/* GPRs, some SPRs  */
#endif
		   (thread_state_t)&ts,
#ifdef PPC64
                   PPC_THREAD_STATE64_COUNT
#else
		   PPC_THREAD_STATE_COUNT
#endif
                   );

  lock_release(mach_exception_lock_set, 0);
  return 0;
}

/*
  This function runs in the exception handling thread.  It's
  called (by this precise name) from the library function "exc_server()"
  when the thread's exception ports are set up.  (exc_server() is called
  via mach_msg_server(), which is a function that waits for and dispatches
  on exception messages from the Mach kernel.)

  This checks to see if the exception was caused by a pseudo_sigreturn()
  UUO; if so, it arranges for the thread to have its state restored
  from the specified context.

  Otherwise, it tries to map the exception to a signal number and
  arranges that the thread run a "pseudo signal handler" to handle
  the exception.

  Some exceptions could and should be handled here directly.
*/

kern_return_t
catch_exception_raise(mach_port_t exception_port,
		      mach_port_t thread,
		      mach_port_t task, 
		      exception_type_t exception,
		      exception_data_t code_vector,
		      mach_msg_type_number_t code_count)
{
  int signum = 0, code = *code_vector, code1;
  TCR *tcr = TCR_FROM_EXCEPTION_PORT(exception_port);


  if ((exception == EXC_BAD_INSTRUCTION) &&
      (code_vector[0] == EXC_PPC_UNIPL_INST) &&
      (((code1 = code_vector[1]) == (int)pseudo_sigreturn) ||
       (code1 == (int)enable_fp_exceptions) ||
       (code1 == (int)disable_fp_exceptions))) {
    if (code1 == (int)pseudo_sigreturn) {
      return do_pseudo_sigreturn(thread, tcr);
    }
    if (code1 == (int)enable_fp_exceptions) {
      return thread_set_fp_exceptions_enabled(thread, true);
    }
    return thread_set_fp_exceptions_enabled(thread, false);
  }
  if (tcr->flags & TCR_FLAG_BIT_PROPAGATE_EXCEPTION) {
    tcr->flags &= ~TCR_FLAG_BIT_PROPAGATE_EXCEPTION;
    return 17;
  }
  switch (exception) {
  case EXC_BAD_ACCESS:
    signum = SIGSEGV;
    break;

  case EXC_BAD_INSTRUCTION:
    signum = SIGILL;
    break;

  case EXC_SOFTWARE:
    if (code == EXC_PPC_TRAP) {
      signum = SIGTRAP;
    }
    break;

  case EXC_ARITHMETIC:
    signum = SIGFPE;
    break;

  default:
    break;
  }
  if (signum) {
    return setup_signal_frame(thread,
			      (void *)pseudo_signal_handler,
			      signum,
                              code,
			      tcr);
  }
  return 17;
}


/*
  The initial function for an exception-handling thread.
*/

void *
exception_handler_proc(void *arg)
{
  extern boolean_t exc_server();
  mach_port_t p = TCR_TO_EXCEPTION_PORT(arg);

  mach_msg_server(exc_server, 2048, p, 0);
  /* Should never return. */
  abort();
}


mach_port_t
mach_exception_port_set()
{
  static mach_port_t __exception_port_set = MACH_PORT_NULL;
  kern_return_t kret;  
  if (__exception_port_set == MACH_PORT_NULL) {
    lock_set_create(mach_task_self(), 
		    &mach_exception_lock_set,
		    1,
		    SYNC_POLICY_FIFO);

    kret = mach_port_allocate(mach_task_self(),
			      MACH_PORT_RIGHT_PORT_SET,
			      &__exception_port_set);
    MACH_CHECK_ERROR("allocating thread exception_ports",kret);
    create_system_thread(0,
                         NULL,
                         exception_handler_proc, 
                         (void *)((natural)__exception_port_set));
  }
  return __exception_port_set;
}

/*
  Setup a new thread to handle those exceptions specified by
  the mask "which".  This involves creating a special Mach
  message port, telling the Mach kernel to send exception
  messages for the calling thread to that port, and setting
  up a handler thread which listens for and responds to
  those messages.

*/

/*
  Establish the lisp thread's TCR as its exception port, and determine
  whether any other ports have been established by foreign code for
  exceptions that lisp cares about.

  If this happens at all, it should happen on return from foreign
  code and on entry to lisp code via a callback.

  This is a lot of trouble (and overhead) to support Java, or other
  embeddable systems that clobber their caller's thread exception ports.
  
*/
kern_return_t
tcr_establish_exception_port(TCR *tcr, mach_port_t thread)
{
  kern_return_t kret;
  MACH_foreign_exception_state *fxs = (MACH_foreign_exception_state *)tcr->native_thread_info;
  int i;
  unsigned n = NUM_LISP_EXCEPTIONS_HANDLED;
  mach_port_t lisp_port = TCR_TO_EXCEPTION_PORT(tcr), foreign_port;
  exception_mask_t mask = 0;

  kret = thread_swap_exception_ports(thread,
				     LISP_EXCEPTIONS_HANDLED_MASK,
				     lisp_port,
				     EXCEPTION_DEFAULT,
				     THREAD_STATE_NONE,
				     fxs->masks,
				     &n,
				     fxs->ports,
				     fxs->behaviors,
				     fxs->flavors);
  if (kret == KERN_SUCCESS) {
    fxs->foreign_exception_port_count = n;
    for (i = 0; i < n; i ++) {
      foreign_port = fxs->ports[i];

      if ((foreign_port != lisp_port) &&
	  (foreign_port != MACH_PORT_NULL)) {
	mask |= fxs->masks[i];
      }
    }
    tcr->foreign_exception_status = (int) mask;
  }
  return kret;
}

kern_return_t
tcr_establish_lisp_exception_port(TCR *tcr)
{
  return tcr_establish_exception_port(tcr, (mach_port_t)((natural)tcr->native_thread_id));
}

/*
  Do this when calling out to or returning from foreign code, if
  any conflicting foreign exception ports were established when we
  last entered lisp code.
*/
kern_return_t
restore_foreign_exception_ports(TCR *tcr)
{
  exception_mask_t m = (exception_mask_t) tcr->foreign_exception_status;
  
  if (m) {
    MACH_foreign_exception_state *fxs  = 
      (MACH_foreign_exception_state *) tcr->native_thread_info;
    int i, n = fxs->foreign_exception_port_count;
    exception_mask_t tm;

    for (i = 0; i < n; i++) {
      if ((tm = fxs->masks[i]) & m) {
	thread_set_exception_ports((mach_port_t)((natural)tcr->native_thread_id),
				   tm,
				   fxs->ports[i],
				   fxs->behaviors[i],
				   fxs->flavors[i]);
      }
    }
  }
}
				   

/*
  This assumes that a Mach port (to be used as the thread's exception port) whose
  "name" matches the TCR's 32-bit address has already been allocated.
*/

kern_return_t
setup_mach_exception_handling(TCR *tcr)
{
  mach_port_t 
    thread_exception_port = TCR_TO_EXCEPTION_PORT(tcr),
    target_thread = pthread_mach_thread_np((pthread_t)ptr_from_lispobj(tcr->osid)),
    task_self = mach_task_self();
  kern_return_t kret;

  kret = mach_port_insert_right(task_self,
				thread_exception_port,
				thread_exception_port,
				MACH_MSG_TYPE_MAKE_SEND);
  MACH_CHECK_ERROR("adding send right to exception_port",kret);

  kret = tcr_establish_exception_port(tcr, (mach_port_t)((natural) tcr->native_thread_id));
  if (kret == KERN_SUCCESS) {
    mach_port_t exception_port_set = mach_exception_port_set();

    kret = mach_port_move_member(task_self,
				 thread_exception_port,
				 exception_port_set);
  }
  return kret;
}

void
darwin_exception_init(TCR *tcr)
{
  void tcr_monitor_exception_handling(TCR*, Boolean);
  kern_return_t kret;
  MACH_foreign_exception_state *fxs = 
    calloc(1, sizeof(MACH_foreign_exception_state));
  
  tcr->native_thread_info = (void *) fxs;

  if ((kret = setup_mach_exception_handling(tcr))
      != KERN_SUCCESS) {
    fprintf(stderr, "Couldn't setup exception handler - error = %d\n", kret);
    terminate_lisp();
  }
  lisp_global(LISP_EXIT_HOOK) = (LispObj) restore_foreign_exception_ports;
  lisp_global(LISP_RETURN_HOOK) = (LispObj) tcr_establish_lisp_exception_port;
}

/*
  The tcr is the "name" of the corresponding thread's exception port.
  Destroying the port should remove it from all port sets of which it's
  a member (notably, the exception port set.)
*/
void
darwin_exception_cleanup(TCR *tcr)
{
  void *fxs = tcr->native_thread_info;

  if (fxs) {
    tcr->native_thread_info = NULL;
    free(fxs);
  }
  mach_port_deallocate(mach_task_self(),TCR_TO_EXCEPTION_PORT(tcr));
  mach_port_destroy(mach_task_self(),TCR_TO_EXCEPTION_PORT(tcr));
}


/*
  Only do this if pthread_kill indicated that the pthread isn't
  listening to signals anymore, as can happen as soon as pthread_exit()
  is called on Darwin.  The thread could still call out to lisp as it
  is exiting, so we need another way to suspend it in this case.
*/
Boolean
mach_suspend_tcr(TCR *tcr)
{
  mach_port_t mach_thread = (mach_port_t)((natural)( tcr->native_thread_id));
  kern_return_t status;
  ExceptionInformation *lss;
  Boolean result = false;
  
  lock_acquire(mach_exception_lock_set, 0);
  status = thread_suspend(mach_thread);
  if (status == KERN_SUCCESS) {
    thread_abort_safely(mach_thread);
    lss = create_thread_context_frame(mach_thread, NULL);
    lss->uc_onstack = 0;
    lss->uc_sigmask = (sigset_t) 0;
    tcr->suspend_context = lss;
    tcr->suspend_total++;
    result = true;
  }
  lock_release(mach_exception_lock_set, 0);
  return result;
}

void
mach_resume_tcr(TCR *tcr)
{
  ExceptionInformation *xp;
  mach_port_t mach_thread = (mach_port_t)((natural)(tcr->native_thread_id));
  
  lock_acquire(mach_exception_lock_set, 0);
  xp = tcr->suspend_context;
  tcr->suspend_context = NULL;
  restore_mach_thread_state(mach_thread, xp);
  thread_resume(mach_thread);
  lock_release(mach_exception_lock_set,0);
}

void
fatal_mach_error(char *format, ...)
{
  va_list args;
  char s[512];
 

  va_start(args, format);
  vsnprintf(s, sizeof(s),format, args);
  va_end(args);

  Fatal("Mach error", s);
}

#endif
