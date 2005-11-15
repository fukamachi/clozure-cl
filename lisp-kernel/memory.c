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
#include <fpu_control.h>
#include <linux/prctl.h>
#endif


#include <sys/mman.h>

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
  extern int page_size;
  natural protbytes, zerobytes;
  area *a = active_dynamic_area;
  BytePtr newlimit, protptr, zptr;
  int psize = page_size;
  /* 
     Zero the region between the new freepointer and the end of the
     containing segment.
  */
  zptr = (BytePtr) align_to_power_of_2(newfree,log2_heap_segment_size);
  zerobytes = zptr-newfree;
  HeapHighWaterMark = zptr;

  while (zerobytes >= psize) {
    zptr -= psize;
    zerobytes -= psize;
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
protect_area(protected_area_ptr p)
{
  BytePtr start = p->start;
  natural n = p->protsize;

  if (n && ! p->nprot) {
    ProtectMemory(start, n);
    p->nprot = n;
  }
}


void
zero_page(BytePtr start)
{
  extern int page_size;
#ifdef PPC
  extern void zero_cache_lines(BytePtr, size_t, size_t);
  zero_cache_lines(start, (page_size/cache_block_size), cache_block_size);
#else
  memset(start, 0, page_size);
#endif
}
