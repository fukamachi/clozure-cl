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



#ifdef LINUX
#include <sys/time.h>
#endif

#ifdef DARWIN
#include <sys/time.h>
#endif


#include "lisp.h"
#include "lisp_globals.h"
#include "bits.h"
#include "gc.h"
#include "area.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>



/* area management */


Boolean GCDebug = false;

area *
new_area(BytePtr lowaddr, BytePtr highaddr, area_code code)
{
  area *a = (area *) (zalloc(sizeof(area)));
  if (a) {
    unsigned ndwords = area_dword(highaddr, lowaddr);
    a->low = lowaddr;
    a->high = highaddr;
    a->active = (code == AREA_DYNAMIC) ? lowaddr : highaddr;
    a->code = code;
    a->ndwords = ndwords;
    /* Caller must allocate markbits when allocating heap ! */
    
  }
  return a;
}

static area *
add_area_before(area *new_area, area *before)
{
  area *before_before = before->pred;

  new_area->pred = before_before;
  new_area->succ = before;
  before_before->succ = new_area;
  before->pred = new_area;
  return new_area;
}

/*
  The active dynamic area comes first.
  Static areas follow dynamic areas.
  Stack areas follow static areas.
  Readonly areas come last.
*/

void
add_area(area *new_area)
{
  area *that = all_areas;
  int
    thiscode = (int)(new_area->code),
    thatcode;

  /* Cdr down the linked list */
  do {
    that = that->succ;
    thatcode = (int)(that->code);
  } while (thiscode < thatcode);
  add_area_before(new_area, that);
}

/*
  Search areas "forward" from the header's successor, until
  an area containing ADDR is found or an area with code < MINCODE
  is encountered.
  This walks the area list visiting heaps (dynamic, then static)
  first, then stacks.

*/
static area *
find_area_forward(BytePtr addr, area_code mincode)
{
  area *p, *header = all_areas;

  for (p = header->succ; p != header; p = p->succ) {
    area_code pcode = p->code;
    if (pcode < mincode) {
      return NULL;
    }
    if (pcode >= AREA_READONLY) {
      if ((addr >= p->low) &&
          (addr < p->active)) {
        return p;
      }
    } else {
      if ((addr >= p->active) &&
          (addr < p->high)) {
        return p;
      }
    }
  }
  return NULL;
}

static area *
find_area_backward(BytePtr addr, area_code maxcode)
{
  area *p, *header = all_areas;

  for (p = header->pred; p != header; p = p->pred) {
    area_code pcode = p->code;

    if (pcode > maxcode) {
      return NULL;
    }
    if (pcode >= AREA_READONLY) {
      if ((addr >= p->low) &&
          (addr < p->active)) {
        return p;
      }
    } else {
      if ((addr >= p->active) &&
          (addr < p->high)) {
        return p;
      }
    }
  }
  return NULL;
}

area *
area_containing(BytePtr addr)
{
  return find_area_forward(addr, AREA_VOID);
}

area *
heap_area_containing(BytePtr addr)
{
  return find_area_forward(addr, AREA_READONLY);
}

area *
stack_area_containing(BytePtr addr)
{
  return find_area_backward(addr, AREA_TSTACK);
}

/* Heap sanity checking. */

void
check_node(LispObj n)
{
  int tag = fulltag_of(n), header_tag;
  area *a;
  LispObj header;

  switch (tag) {
  case fulltag_even_fixnum:
  case fulltag_odd_fixnum:
  case fulltag_imm:
    return;

  case fulltag_nil:
    if (n != lisp_nil) {
      Bug(NULL,"Object tagged as nil, not nil : 0x%08x", n);
    }
    return;

  case fulltag_nodeheader:
  case fulltag_immheader:
    Bug(NULL, "Header not expected : 0x%08x", n);
    return;

  case fulltag_misc:
  case fulltag_cons:
    a = heap_area_containing((BytePtr)n);
    
    if (a == NULL) {
      /* Can't do as much sanity checking as we'd like to
         if object is a defunct stack-consed object.
         If a dangling reference to the heap, that's
         bad .. */
      a = active_dynamic_area;
      if ((n > ((LispObj)a->active)) &&
          (n < ((LispObj)a->high))) {
        Bug(NULL, "Node points to heap free space: 0x%08x", n);
      }
      return;
    }
    break;
  }
  /* Node points to heap area, so check header/lack thereof. */
  header = header_of(n);
  header_tag = fulltag_of(header);
  if (tag == fulltag_cons) {
    if ((header_tag == fulltag_nodeheader) ||
        (header_tag == fulltag_immheader)) {
      Bug(NULL, "Cons cell at 0x%08x has bogus header : 0x%08x", n, header);
    }
    return;
  }

  if ((header_tag != fulltag_nodeheader) &&
      (header_tag != fulltag_immheader)) {
    Bug(NULL,"Vector at 0x%08x has bogus header : 0x%08x", n, header);
  }
  return;
}

void
check_range(LispObj *start, LispObj *end)
{
  LispObj node, *current = start, *prev;
  int tag;
  unsigned elements;

  while (current < end) {
    prev = current;
    node = *current++;
    tag = fulltag_of(node);
    if (tag == fulltag_immheader) {
      current = (LispObj *)skip_over_ivector((unsigned)prev, node);
    } else if (tag == fulltag_nodeheader) {
      elements = header_element_count(node) | 1;
      while (elements--) {
        check_node(*current++);
      }
    } else {
      check_node(node);
      check_node(*current++);
    }
  }

  if (current != end) {
    Bug(NULL, "Overran end of memory range: start = 0x%08x, end = 0x%08x, prev = 0x%08x, current = 0x%08x",
        start, end, prev, current);
  }
}

void
check_all_areas()
{
  area *a = active_dynamic_area;
  area_code code = a->code;

  while (code != AREA_VOID) {
    switch (code) {
    case AREA_DYNAMIC:
    case AREA_STATIC:
    case AREA_STATICLIB:
      check_range((LispObj *)a->low, (LispObj *)a->active);
      break;

    case AREA_VSTACK:
      {
        LispObj* low = (LispObj *)a->active;
        LispObj* high = (LispObj *)a->high;
        
        if (((int)low) & 4) {
          check_node(*low++);
        }
        check_range(low, high);
      }
      break;

    case AREA_TSTACK:
      {
        LispObj *current, *next,
                *start = (LispObj *) a->active,
                *end = start,
                *limit = (LispObj *) a->high;
                 
        for (current = start;
             end != limit;
             current = next) {
          next = (LispObj *) *current;
          end = ((next >= start) && (next < limit)) ? next : limit;
          if (current[1] == 0) {
            check_range(current+2, end);
          }
        }
      }
      break;
    }
    a = a->succ;
    code = (a->code);
  }
}

/*
  Scan forward from the object boundary P until a node address >= to
   PAGE is found.
*/
LispObj *
first_node_on_page(LispObj *p, LispObj *page, pageentry *bucket, LispObj *limit)
{
  LispObj *q, *nextpage = (LispObj *) ((BytePtr)page + 4096), header;
  int tag;

  while (p < limit) {
    header = *p;
    tag = fulltag_of(header);

    if (p >= nextpage) {
      bucket->halfword = 0;
      return p;
    }
    
    if (tag == fulltag_immheader) {
      q = (LispObj *)skip_over_ivector((LispObj)p, header);
      if (q >= nextpage) {
	bucket->halfword = 0;
	return q;
      }
    } else if (tag == fulltag_nodeheader) {
      q = p + ((2 + header_element_count(header)) & ~1);
      if (p >= page) {
	bucket->bits.hasnode = 1;
	bucket->bits.offset = (p-page);
        if (q > nextpage) {
          return p;
        }
	return q;
      }
      if (q >= page) {
	bucket->bits.hasnode = 1;
	bucket->bits.offset = 0;
	if (q > nextpage) {
	  return p;
	}
	return q;
      }
    } else {
      q = p + 2;
      if (p >= page) {
	bucket->bits.hasnode = 1;
	bucket->bits.offset = (p-page);
	return q;
      }
    }
    p = q;
  }
  bucket->halfword = 0;
  return p;
}


void
make_page_node_map(LispObj *start, LispObj *end)
{
  LispObj *p, *page = (LispObj *)truncate_to_power_of_2(start,12);
  pageentry 
    *buckets = pagemap + (((LispObj)page - lisp_global(HEAP_START)) >> 12);

  if (start != page) {
    if (buckets->bits.hasnode) {
      /* We already know (from having built older generation's page_node_map)
         where the first node on this page is.  We're more interested in
         the next page(s) */
      buckets++;
      page += (4096/sizeof(LispObj *));
    }
  }
  for (p = start;
       p < end;
       page += (4096/sizeof(LispObj *)), buckets++) {
    p = first_node_on_page(p, page, buckets, end);
  }
}

void
update_refmap_for_range(LispObj *start, 
                        LispObj *end,
                        LispObj ephemeral_start,
                        unsigned long ephemeral_dwords)
{
  LispObj node, oldspacestart = lisp_global(HEAP_START);
  int tag;
  bitvector refbits = tenured_area->refbits;

  while (start < end) {
    node = *start;
    tag = fulltag_of(node);
    if (tag == fulltag_immheader) {	/* An ivector */
      start = (LispObj *)skip_over_ivector((LispObj)start, node);
    } else {
      if ((header_subtag(node) == subtag_hash_vector) ||
          /* Need to memoize location of hash vector headers, at
             least if we have to track key movement */
          (((tag == fulltag_cons) || (tag == fulltag_misc)) &&
           (area_dword(node, ephemeral_start) < ephemeral_dwords))) {
        /* Tagged pointer to (some) younger generation; update refmap */
        set_bit(refbits,area_dword(start, oldspacestart));
      } else {
        node = start[1];
        tag = fulltag_of(node);
        if (((tag == fulltag_cons) || (tag == fulltag_misc)) &&
            (area_dword(node, ephemeral_start) < ephemeral_dwords)) {
          set_bit(refbits,area_dword(start, oldspacestart));
        }
      }
      start += 2;
    }
  }
}
                        
void
update_refmap_for_page(pageentry *bucket,
		       LispObj *page,
		       LispObj ephemeral_start,
		       unsigned ephemeral_dwords)
{
  LispObj *start;
  if (bucket->bits.modified) {		/* Page was written to since last GC */
    if (bucket->bits.hasnode) {		/* Some nodes on this page */
      start = page + bucket->bits.offset;
      update_refmap_for_range(start,
                              (LispObj *) align_to_power_of_2((LispObj)start+1,12),
                              ephemeral_start,
                              ephemeral_dwords);
    }
  }
}


void
update_refmap_for_area(area *a, BytePtr curfree)
{
  if (a->ndwords) {
    LispObj 
      *start = (LispObj *) a->low,
      *limit = (LispObj *) a->active,
      *last_whole_page_end = (LispObj *) truncate_to_power_of_2(limit,12),
      *first_partial_page_start = (LispObj *) truncate_to_power_of_2(start,12);
    pageentry *p = pagemap + ((LispObj)start - lisp_global(HEAP_START) >> 12);
    unsigned younger_dwords = area_dword((LispObj)curfree,(LispObj)limit);
    
    if (last_whole_page_end == first_partial_page_start) {
      if (p->bits.modified && p->bits.hasnode) {
        update_refmap_for_range(start,limit,(LispObj)limit,younger_dwords);
      }
    } else {
      if (start != first_partial_page_start) {
        LispObj 
          *page_end = first_partial_page_start + (4096 / sizeof(LispObj *));
        if (p->bits.modified && p->bits.hasnode) {
          update_refmap_for_range(start,page_end,(LispObj)limit,younger_dwords);
        }
        start = page_end;
        p++;
      }
      for (; 
           start < last_whole_page_end;
           start += (4096 / sizeof(LispObj *)), p++) {
        update_refmap_for_page(p,start,(LispObj)limit,younger_dwords);
      }
      if (start < limit) {
        if (p->bits.modified && p->bits.hasnode) {
          update_refmap_for_range(start+p->bits.offset,limit,(LispObj)limit,younger_dwords);
        }
      }
    }
  }
}

void
update_area_refmaps(BytePtr curfree)
{
  unprotect_area(oldspace_protected_area);
  update_refmap_for_area(tenured_area,curfree);
  update_refmap_for_area(g2_area,curfree);
  update_refmap_for_area(g1_area,curfree);
}
      


/*
  Make everything "younger" than the start of the target area
  belong to that area; all younger areas will become empty, and
  the dynamic area will have to lose some of its markbits (they
  get zeroed and become part of the tenured area's refbits.)

  The active dynamic area must have been "normalized" (e.g., its
  active pointer must match the free pointer) before this is called.

  If the target area is 'tenured_area' (the oldest ephemeral generation),
  zero its refbits and update YOUNGEST_EPHEMERAL.

*/

void
tenure_to_area(area *target)
{
  area *a = active_dynamic_area, *child;
  BytePtr 
    curfree = a->active,
    target_low = target->low,
    tenured_low = tenured_area->low;
  unsigned 
    dynamic_dwords = area_dword(curfree, a->low),
    new_tenured_dwords = area_dword(curfree, tenured_area->low);
  bitvector 
    refbits = tenured_area->refbits,
    markbits = a->markbits,
    new_markbits;

  target->high = target->active = curfree;
  target->ndwords = area_dword(curfree, target_low);

  for (child = target->younger; child != a; child = child->younger) {
    child->high = child->low = child->active = curfree;
    child->ndwords = 0;
  }

  a->low = curfree;
  a->ndwords = area_dword(a->high, curfree);

  new_markbits = refbits + ((new_tenured_dwords + 31) >> 5);
  
  if (target == tenured_area) {
    zero_bits(refbits, new_tenured_dwords);
    lisp_global(OLDEST_EPHEMERAL) = (LispObj) curfree;
  } else {
    /* Need more (zeroed) refbits & fewer markbits */
    zero_bits(markbits, ((new_markbits-markbits)<<5));
  }
   
  a->markbits = new_markbits;
  make_page_node_map((LispObj *)target_low, (LispObj *)curfree);
  protect_oldspace(curfree);
}

/*
  Make everything younger than the oldest byte in 'from' belong to 
  the youngest generation.  If 'from' is 'tenured_area', this means
  that nothing's ephemeral any more (and OLDEST_EPHEMERAL can be set
  to 0 to indicate this.)
  
  Some tenured_area refbits become dynamic area markbits in the process;
  it's not necessary to zero them, since the GC will do that.
*/

void
untenure_from_area(area *from)
{
  if (lisp_global(OLDEST_EPHEMERAL) != 0) {
    area *a = active_dynamic_area, *child;
    BytePtr curlow = from->low;
    unsigned new_tenured_dwords = area_dword(curlow, tenured_area->low);
    
    for (child = from; child != a; child = child->younger) {
      child->low = child->active = child->high = curlow;
      child->ndwords = 0;
    }
    
    a->low = curlow;
    a->ndwords = area_dword(a->high, curlow);
    
    a->markbits = (tenured_area->refbits) + ((new_tenured_dwords+31)>>5);
    if (from == tenured_area) {
      /* Everything's in the dynamic area */
      lisp_global(OLDEST_EPHEMERAL) = 0;
    }
  }
}


Boolean
egc_control(Boolean activate, BytePtr curfree)
{
  area *a = active_dynamic_area;
  Boolean egc_is_active = (a->older != NULL);

  if (activate != egc_is_active) {
    if (curfree != NULL) {
      a->active = curfree;
    }
    if (activate) {
      LispObj *heap_start = (LispObj *)lisp_global(HEAP_START);

      a->older = g1_area;
      tenure_to_area(tenured_area);
      egc_is_active = true;
    } else {
      untenure_from_area(tenured_area);
      a->older = NULL;
      unprotect_area(oldspace_protected_area);
      egc_is_active = false;
    }
  }
  return egc_is_active;
}

/*
  Lisp ff-calls this; it needs to set the active area's active pointer
  correctly.
*/

Boolean
lisp_egc_control(Boolean activate)
{
  area *a = active_dynamic_area;
  return egc_control(activate, (BytePtr) a->active);
}


  
/* Splice the protected_area_ptr out of the list and dispose of it. */
void
delete_protected_area(protected_area_ptr p)
{
  BytePtr start = p->start;
  int nbytes = p->nprot;
  protected_area_ptr *prev = &AllProtectedAreas, q;

  if (nbytes) {
    UnProtectMemory((LogicalAddress)start, nbytes);
  }
  
  while ((q = *prev) != NULL) {
    if (p == q) {
      *prev = p->next;
      break;
    } else {
      prev = &(q->next);
    }
  }

  deallocate((Ptr)p);
}


/* 
  Unlink the area from all_areas.
  Unprotect and dispose of any hard/soft protected_areas.
  If the area has a handle, dispose of that as well.
  */

void
condemn_area(area *a)
{
  void free_stack(void *);
  area *prev = a->pred, *next = a->succ;
  Ptr h = a->h;
  protected_area_ptr p;

  prev->succ = next;
  next->pred = prev;

  p = a->softprot;
  if (p) delete_protected_area(p);

  p = a->hardprot;

  if (p) delete_protected_area(p);

  if (h) free_stack(h);
  deallocate((Ptr)a);
}


/*
  condemn an area and all the other areas that can be reached
  via the area.older & area.younger links.
  This is the function in the ppc::kernel-import-condemn-area slot,
  called by free-stack-area
  */
void
condemn_area_chain(area *a)
{
  area *older;
  for (; a->younger; a = a->younger) ;
  for (;a;) {
    older = a->older;
    condemn_area(a);
    a = older;
  }
}



bitvector GCmarkbits = NULL;
LispObj GCarealow;
unsigned GCndwords_in_area;
LispObj GCweakvll = (LispObj)NULL;
LispObj GCephemeral_low;
unsigned GCn_ephemeral_dwords;


/* Sooner or later, this probably wants to be in assembler */
/* Return false if n is definitely not an ephemeral node, true if
   it might be */
void
mark_root(LispObj n)
{
  int tag_n = fulltag_of(n);
  unsigned dword, bits, *bitsp, mask;

  if (!is_node_fulltag(tag_n)) {
    return;
  }

  dword = gc_area_dword(n);
  if (dword >= GCndwords_in_area) {
    return;
  }
  set_bits_vars(GCmarkbits,dword,bitsp,bits,mask);
  if (bits & mask) {
    return;
  }
  *bitsp = (bits | mask);

  if (tag_n == fulltag_cons) {
    cons *c = (cons *) untag(n);
    rmark(c->car);
    rmark(c->cdr);
    return;
  }
  {
    LispObj *base = (LispObj *) untag(n);
    unsigned
      header = *((unsigned *) base),
      subtag = header_subtag(header),
      element_count = header_element_count(header),
      total_size_in_bytes,      /* including 4-byte header */
      suffix_dwords;

    tag_n = fulltag_of(header);

    if ((tag_n == fulltag_nodeheader) ||
        (subtag <= max_32_bit_ivector_subtag)) {
      total_size_in_bytes = 4 + (element_count<<2);
    } else if (subtag <= max_8_bit_ivector_subtag) {
      total_size_in_bytes = 4 + element_count;
    } else if (subtag <= max_16_bit_ivector_subtag) {
      total_size_in_bytes = 4 + (element_count<<1);
    } else if (subtag == subtag_double_float_vector) {
      total_size_in_bytes = 8 + (element_count<<3);
    } else {
      total_size_in_bytes = 4 + ((element_count+7)>>3);
    }
    suffix_dwords = ((total_size_in_bytes+7)>>3) -1;

    if (suffix_dwords) {
      set_n_bits(GCmarkbits, dword+1, suffix_dwords);
    }

    if (tag_n == fulltag_nodeheader) {
      if (subtag == subtag_hash_vector) {
        ((hash_table_vector_header *) base)->cache_key = undefined;
        ((hash_table_vector_header *) base)->cache_value = lisp_nil;
        deref(base,1) = GCweakvll;
        GCweakvll = n;
        return;
      }

      if (subtag == subtag_pool) {
        deref(base, 1) = lisp_nil;
      }
      
      if (subtag == subtag_weak) {
        int weak_type = (unsigned) base[2];
        if (weak_type >> population_termination_bit) {
          element_count -= 2;
        } else {
          element_count -= 1;
        }
      }

      base += (1+element_count);
      while(element_count--) {
        rmark(*--base);
      }
      if (subtag == subtag_weak) {
        deref(base,1) = GCweakvll;
        GCweakvll = n;
      }
    }
  }
}


/* 
  This marks the node if it needs to; it returns true if the node
  is either a hash table vector header or a cons/misc-tagged pointer
  to ephemeral space.
  Note that it  might be a pointer to ephemeral space even if it's
  not pointing to the current generation.
*/

Boolean
mark_ephemeral_root(LispObj n)
{
  int tag_n = fulltag_of(n);
  unsigned eph_dword;

  if (tag_n == fulltag_nodeheader) {
    return (header_subtag(n) == subtag_hash_vector);
  }
 
  if ((tag_n == fulltag_cons) ||
      (tag_n == fulltag_misc)) {
    eph_dword = area_dword(n, GCephemeral_low);
    if (eph_dword < GCn_ephemeral_dwords) {
      mark_root(n);             /* May or may not mark it */
      return true;              /* but return true 'cause it's an ephemeral node */
    }
  }
  return false;                 /* Not a heap pointer or not ephemeral */
}
  

/*
  Some objects (saved LRs on the control stack, the LR, PC, and CTR
  in exception frames) may be tagged as fixnums but are really
  locatives into code_vectors.

  If "pc" is not tagged as a fixnum, mark it as a "normal" root.
  If "pc" doesn't point at an unmarked doubleword in the area
  being GCed, return.
  Else back up until the code_vector's header is found and mark
  all doublewords in the code_vector.
*/
void
mark_pc_root(LispObj pc)
{
  if (tag_of(pc) != tag_fixnum) {
    mark_root(pc);
  } else {
    unsigned dword = gc_area_dword(pc);
    if ((dword < GCndwords_in_area) &&
        !ref_bit(GCmarkbits,dword)) {
      LispObj
        *headerP,
        header;

      for(headerP = (LispObj*)(untag(pc));
          dword < GCndwords_in_area;
          headerP-=2, --dword) {
        header = *headerP;

        if ((header & code_header_mask) == subtag_code_vector) {
          set_n_bits(GCmarkbits, dword, (2+header_element_count(header))>>1);
          return;
        }
      }
      /*
        Expected to have found a header by now, but didn't.
        That's a bug.
        */
      Bug(NULL, "code_vector header not found!");
    }
  }
}


/*
  This wants to be in assembler even more than "mark_root" does.
  For now, it does link-inversion: hard as that is to express in C,
  reliable stack-overflow detection may be even harder ...
*/
void
rmark(LispObj n)
{
  int tag_n = fulltag_of(n);
  bitvector markbits = GCmarkbits;
  unsigned dword, bits, *bitsp, mask;

  if (!is_node_fulltag(tag_n)) {
    return;
  }

  dword = gc_area_dword(n);
  if (dword >= GCndwords_in_area) {
    return;
  }
  set_bits_vars(markbits,dword,bitsp,bits,mask);
  if (bits & mask) {
    return;
  }
  *bitsp = (bits | mask);
  {
    LispObj prev = undefined;
    LispObj this = n, next;
    /*
      This is an FSM.  The basic states are:
      (0) Just marked the cdr of a cons; mark the car next;
      (1) Just marked the car of a cons; back up.
      (2) Hit a gvector header.  Back up.
      (3) Marked a gvector element; mark the preceding one.
      (4) Backed all the way up to the object that got us here.
      
      This is all encoded in the fulltag of the "prev" pointer.
      */

    if (tag_n == fulltag_cons) goto MarkCons;
    goto MarkVector;

  ClimbCdr:
    prev = deref(this,0);
    deref(this,0) = next;

  Climb:
    next = this;
    this = prev;
    tag_n = fulltag_of(prev);
    switch(tag_n) {
    case fulltag_odd_fixnum:
    case fulltag_even_fixnum:
      goto ClimbVector;

    case fulltag_imm:
      return;

    case fulltag_cons:
      goto ClimbCdr;

    case fulltag_nil:
      goto ClimbCar;

      /* default: abort() */
    }

  DescendCons:
    prev = this;
    this = next;

  MarkCons:
    next = deref(this,1);
    this += 4;
    tag_n = fulltag_of(next);
    if (!is_node_fulltag(tag_n)) goto MarkCdr;
    dword = gc_area_dword(next);
    if (dword >= GCndwords_in_area) goto MarkCdr;
    set_bits_vars(markbits,dword,bitsp,bits,mask);
    if (bits & mask) goto MarkCdr;
    *bitsp = (bits | mask);
    deref(this,1) = prev;
    if (tag_n == fulltag_cons) goto DescendCons;
    goto DescendVector;

  ClimbCar:
    prev = deref(this,1);
    deref(this,1) = next;

  MarkCdr:
    next = deref(this, 0);
    this -= 4;
    tag_n = fulltag_of(next);
    if (!is_node_fulltag(tag_n)) goto Climb;
    dword = gc_area_dword(next);
    if (dword >= GCndwords_in_area) goto Climb;
    set_bits_vars(markbits,dword,bitsp,bits,mask);
    if (bits & mask) goto Climb;
    *bitsp = (bits | mask);
    deref(this, 0) = prev;
    if (tag_n == fulltag_cons) goto DescendCons;
    /* goto DescendVector; */

  DescendVector:
    prev = this;
    this = next;

  MarkVector:
    {
      LispObj *base = (LispObj *) untag(this);
      unsigned
        header = *((unsigned *) base),
      subtag = header_subtag(header),
      element_count = header_element_count(header),
      total_size_in_bytes,
      suffix_dwords;

      tag_n = fulltag_of(header);

      if ((tag_n == fulltag_nodeheader) ||
          (subtag <= max_32_bit_ivector_subtag)) {
        total_size_in_bytes = 4 + (element_count<<2);
      } else if (subtag <= max_8_bit_ivector_subtag) {
        total_size_in_bytes = 4 + element_count;
      } else if (subtag <= max_16_bit_ivector_subtag) {
        total_size_in_bytes = 4 + (element_count<<1);
      } else if (subtag == subtag_double_float_vector) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else {
        total_size_in_bytes = 4 + ((element_count+7)>>3);
      }
      suffix_dwords = ((total_size_in_bytes+7)>>3)-1;

      if (suffix_dwords) {
        set_n_bits(GCmarkbits, dword+1, suffix_dwords);
      }

      if (tag_n != fulltag_nodeheader) goto Climb;

      if (subtag == subtag_hash_vector) {
        /* Splice onto weakvll, then climb */
        ((hash_table_vector_header *) base)->cache_key = undefined;
        ((hash_table_vector_header *) base)->cache_value = lisp_nil;
        deref(base,1) = GCweakvll;
        GCweakvll = this;
        goto Climb;
      }

      if (subtag == subtag_pool) {
        deref(this, 1) = lisp_nil;
      }

      if (subtag == subtag_weak) {
        int weak_type = (unsigned) base[2];
        if (weak_type >> population_termination_bit)
          element_count -= 2;
        else
        element_count -= 1;
      }

      this = untag(this) + ((element_count+1) << 2);
      goto MarkVectorLoop;
    }

  ClimbVector:
    prev = *((LispObj *) this);
    *((LispObj *) this) = next;

  MarkVectorLoop:
    this -= 4;
    next = *((LispObj *) this);
    tag_n = fulltag_of(next);
    if (tag_n == fulltag_nodeheader) goto MarkVectorDone;
    if (!is_node_fulltag(tag_n)) goto MarkVectorLoop;
    dword = gc_area_dword(next);
    if (dword >= GCndwords_in_area) goto MarkVectorLoop;
    set_bits_vars(markbits,dword,bitsp,bits,mask);
    if (bits & mask) goto MarkVectorLoop;
    *bitsp = (bits | mask);
    *((LispObj *) this) = prev;
    if (tag_n == fulltag_cons) goto DescendCons;
    goto DescendVector;

  MarkVectorDone:
    /* "next" is vector header; "this" is fixnum-aligned.
       If  header subtag = subtag_weak_header, put it on weakvll */
    this += fulltag_misc;

    if (header_subtag(next) == subtag_weak) {
      deref(this, 1) = GCweakvll;
      GCweakvll = this;
    }
    goto Climb;
  }
}

unsigned
skip_over_ivector(unsigned start, LispObj header)
{
  unsigned
    element_count = header_element_count(header),
    subtag = header_subtag(header),
    nbytes;

  if (subtag <= max_32_bit_ivector_subtag) {
    nbytes = element_count << 2;
  } else if (subtag <= max_8_bit_ivector_subtag) {
    nbytes = element_count;
  } else if (subtag <= max_16_bit_ivector_subtag) {
    nbytes = element_count << 1;
  } else if (subtag == subtag_double_float_vector) {
    nbytes = 4 + (element_count << 3);
  } else {
    nbytes = (element_count+7) >> 3;
  }
  return start+(~7 & (nbytes + 4 + 7));
}


void
check_refmap_consistency(LispObj *start, LispObj *end, bitvector refbits)
{
  LispObj x1, *base = start;
  int tag;
  unsigned ref_dword, node_dword;
  Boolean intergen_ref;

  while (start < end) {
    x1 = *start;
    if ((tag = fulltag_of(x1)) == fulltag_immheader) {
      start = (LispObj *)skip_over_ivector((unsigned) start, x1);
    } else {
      intergen_ref = false;
      if ((tag == fulltag_misc) || (tag == fulltag_cons)) {        
        node_dword = gc_area_dword(x1);
        if (node_dword < GCndwords_in_area) {
          intergen_ref = true;
        }
      }
      if (intergen_ref == false) {        
        x1 = start[1];
        tag = fulltag_of(x1);
        if ((tag == fulltag_misc) || (tag == fulltag_cons)) {
          node_dword = gc_area_dword(x1);
          if (node_dword < GCndwords_in_area) {
            intergen_ref = true;
          }
        }
      }
      if (intergen_ref) {
        ref_dword = area_dword(start, base);
        if (!ref_bit(refbits, ref_dword)) {
          Bug(NULL, "Missing memoization in doubleword at 0x%08X", start);
          set_bit(refbits, ref_dword);
        }
      }
      start += 2;
    }
  }
}



void
mark_memoized_area(area *a, unsigned num_memo_dwords)
{
  bitvector refbits = a->refbits;
  LispObj *p = (LispObj *) a->low, x1, x2;
  unsigned inbits, outbits, bits, bitidx, *bitsp, nextbit, diff, memo_dword = 0;
  Boolean keep_x1, keep_x2;

  if (GCDebug) {
    check_refmap_consistency(p, p+(num_memo_dwords << 1), refbits);
  }

  /* The distinction between "inbits" and "outbits" is supposed to help us
     detect cases where "uninteresting" setfs have been memoized.  Storing
     NIL, fixnums, immediates (characters, etc.) or node pointers to static
     or readonly areas is definitely uninteresting, but other cases are
     more complicated (and some of these cases are hard to detect.)

     Some headers are "interesting", to the forwarder if not to us. 

     We -don't- give anything any weak treatment here.  Weak things have
     to be seen by a full gc, for some value of 'full'.
     */

  /*
    We need to ensure that there are no bits set at or beyond "num_memo_dwords"
    in the bitvector.  (This can happen as the EGC tenures/untenures things.)
    We find bits by grabbing a fullword at a time and doing a cntlzw instruction;
    and don't want to have to check for (< memo_dword num_memo_dwords) in the loop.
    */

  {
    unsigned 
      bits_in_last_word = (num_memo_dwords & 0x1f),
      index_of_last_word = (num_memo_dwords >> 5);

    if (bits_in_last_word != 0) {
      refbits[index_of_last_word] &= ~((1<<(32-bits_in_last_word))-1);
    }
  }
        
  set_bitidx_vars(refbits, 0, bitsp, bits, bitidx);
  inbits = outbits = bits;
  while (memo_dword < num_memo_dwords) {
    if (bits == 0) {
      int remain = 0x20 - bitidx;
      memo_dword += remain;
      p += (remain+remain);
      if (outbits != inbits) {
        *bitsp = outbits;
      }
      bits = *++bitsp;
      inbits = outbits = bits;
      bitidx = 0;
    } else {
      nextbit = count_leading_zeros(bits);
      if ((diff = (nextbit - bitidx)) != 0) {
        memo_dword += diff;
        bitidx = nextbit;
        p += (diff+diff);
      }
      x1 = *p++;
      x2 = *p++;
      bits &= ~(BIT0_MASK >> bitidx);
      keep_x1 = mark_ephemeral_root(x1);
      keep_x2 = mark_ephemeral_root(x2);
      if ((keep_x1 == false) && 
          (keep_x2 == false)) {
        outbits &= ~(BIT0_MASK >> bitidx);
      }
      memo_dword++;
      bitidx++;
    }
  }
  if (GCDebug) {
    p = (LispObj *) a->low;
    check_refmap_consistency(p, p+(num_memo_dwords << 1), refbits);
  }
}



void
mark_simple_area_range(LispObj *start, LispObj *end)
{
  LispObj x1, *base;
  int tag;

  while (start < end) {
    x1 = *start;
    if ((tag = fulltag_of(x1)) == fulltag_immheader) {
      start = (LispObj *)skip_over_ivector((unsigned) start, x1);
    } else if (tag != fulltag_nodeheader) {
      ++start;
      mark_root(x1);
      mark_root(*start++);
    } else {
      int subtag = header_subtag(x1);
      int element_count = header_element_count(x1);
      int size = (element_count+1 + 1) & ~1;

      if (subtag == subtag_hash_vector) {
        ((hash_table_vector_header *) start)->cache_key = undefined;
        ((hash_table_vector_header *) start)->cache_value = lisp_nil;
        start[1] = GCweakvll;
        GCweakvll = (LispObj) (((unsigned) start) + fulltag_misc);
      } else {

        if (subtag == subtag_pool) {
          start[1] = lisp_nil;
        }

        if (subtag == subtag_weak) {
          int weak_type = (unsigned) start[2];
          if (weak_type >> population_termination_bit)
            element_count -= 2;
          else
            element_count -= 1; 
          start[1] = GCweakvll;
          GCweakvll = (LispObj) (((unsigned) start) + fulltag_misc);    
        }

        base = start + element_count + 1;
        while(element_count--) {
          mark_root(*--base);
        }
      }
      start += size;
    }
  }
}

void
mark_dohs( void )
{
  doh_block_ptr doh_block = (doh_block_ptr) lisp_global(DOH_HEAD);
  while( doh_block ) {
    mark_simple_area_range( &doh_block->data[0], &doh_block->data[doh_block_slots] );
    doh_block = doh_block->link;
  }
}

/* Mark a tstack area */
void
mark_tstack_area(area *a)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) (a->active),
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = (LispObj *) *current;
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      mark_simple_area_range(current+2, end);
    }
  }
}

/*
  It's really important that headers never wind up in tagged registers.
  Those registers would (possibly) get pushed on the vstack and confuse
  the hell out of this routine.

  vstacks are just treated as a "simple area range", possibly with
  an extra word at the top (where the area's active pointer points.)
  */

void
mark_vstack_area(area *a)
{
  LispObj
    *start = (LispObj *) a->active,
    *end = (LispObj *) a->high;

  if ((((unsigned)end) - ((unsigned)start)) & 4) {
    /* Odd number of words.  Mark the first (can't be a header) */
    mark_root(*start);
    ++start;
  }
  mark_simple_area_range(start, end);
}

/*
  Mark lisp frames on the control stack.
  Ignore emulator frames (odd backpointer) and C frames (size != 4).
*/

void
mark_cstack_area(area *a)
{
  BytePtr
    current,
    next,
    limit = a->high,
    low = a->low;

  for (current = a->active; (current >= low) && (current < limit); current = next) {
    next = *((BytePtr *)current);
#if 0
    if (next < current) {
      Bug(NULL, "Child stack frame older than parent");
    }
#endif
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) &&
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      /* mark fn, then saved lr */
      mark_root(((lisp_frame *)current)->savefn);
      mark_pc_root(((lisp_frame *)current)->savelr);
    } else {
      /* Clear low 2 bits of "next", just in case */
      next = (BytePtr) (((unsigned)next) & ~3);
    }
  }
}


void
reapweakv(LispObj weakv)
{
  /*
    element 2 of the weak vector should be tagged as a cons: if it isn't, just mark it as a root.
    if it is, cdr through it until a "marked" cons is encountered.  If the car of any unmarked
    cons is marked, mark the cons which contains it; otherwise, splice the cons out of the list.
    N.B. : elements 0 and 1 are already marked (or are immediate, etc.)
    */
  LispObj *prev = ((LispObj *) untag(weakv))+(1+2), cell = *prev;
  LispObj termination_list = lisp_nil;
  int weak_type = (int) deref(weakv,2);
  Boolean alistp = ((weak_type & population_type_mask) == population_weak_alist),
          terminatablep = ((weak_type >> population_termination_bit) != 0);
  Boolean done = false;
  cons *rawcons;
  unsigned dword, car_dword;
  bitvector markbits = GCmarkbits;

  if (terminatablep) {
    termination_list = deref(weakv,1+3);
  }

  if (tag_of(cell) != tag_list) {
    mark_root(cell);
  } else if (alistp) {
    /* weak alist */
    while (! done) {
      dword = gc_area_dword(cell);
      if ((dword >= GCndwords_in_area) ||
          (ref_bit(markbits, dword))) {
        done = true;
      } else {
        /* Cons cell is unmarked. */
        LispObj alist_cell, thecar;
        unsigned cell_tag;

        rawcons = (cons *) untag(cell);
        alist_cell = rawcons->car;
        cell_tag = fulltag_of(alist_cell);

        if ((cell_tag == fulltag_cons) &&
            ((car_dword = gc_area_dword(alist_cell)) < GCndwords_in_area) &&
            (! ref_bit(markbits, car_dword)) &&
            (is_node_fulltag(fulltag_of(thecar = car(alist_cell)))) &&
            ((car_dword = gc_area_dword(thecar)) < GCndwords_in_area) &&
            (! ref_bit(markbits, car_dword))) {
          *prev = rawcons->cdr;
          if (terminatablep) {
            rawcons->cdr = termination_list;
            termination_list = cell;
          }
        } else {
          set_bit(markbits, dword);
          prev = (LispObj *)(&(rawcons->cdr));
          mark_root(alist_cell);
        }
        cell = *prev;
      }
    }
  } else {
    /* weak list */
    while (! done) {
      dword = gc_area_dword(cell);
      if ((dword >= GCndwords_in_area) ||
          (ref_bit(markbits, dword))) {
        done = true;
      } else {
        /* Cons cell is unmarked. */
        LispObj thecar;
        unsigned cartag;

        rawcons = (cons *) untag(cell);
        thecar = rawcons->car;
        cartag = fulltag_of(thecar);

        if (is_node_fulltag(cartag) &&
            ((car_dword = gc_area_dword(thecar)) < GCndwords_in_area) &&
            (! ref_bit(markbits, car_dword))) {
          *prev = rawcons->cdr;
          if (terminatablep) {
            rawcons->cdr = termination_list;
            termination_list = cell;
          }
        } else {
          set_bit(markbits, dword);
          prev = (LispObj *)(&(rawcons->cdr));
        }
        cell = *prev;
      }
    }
  }

  if (terminatablep) {
    deref(weakv,1+3) = termination_list;
    if (termination_list != lisp_nil) {
      deref(weakv,1) = GCweakvll;
      GCweakvll = weakv;
    }
  }
}

/* 
  Screw: doesn't deal with finalization.
  */

void
reaphashv(LispObj hashv)
{
  hash_table_vector_header *hashp = (hash_table_vector_header *) untag(hashv);
  unsigned 
    dword,
    npairs = (header_element_count(hashp->header) - 
              ((sizeof(hash_table_vector_header)/sizeof(LispObj)) -1)) >> 1;
  LispObj *pairp = (LispObj*) (hashp+1), weakelement;
  Boolean 
    weak_on_value = ((hashp->flags & nhash_weak_value_mask) != 0);
  bitvector markbits = GCmarkbits;
  int tag;

  while (npairs--) {
    if (weak_on_value) {
      weakelement = pairp[1];
    } else {
      weakelement = pairp[0];
    }
    tag = fulltag_of(weakelement);
    if (is_node_fulltag(tag)) {
      dword = gc_area_dword(weakelement);
      if ((dword < GCndwords_in_area) && 
          ! ref_bit(markbits, dword)) {
        pairp[0] = undefined;
        pairp[1] = lisp_nil;
        hashp->weak_deletions_count += (1<<fixnumshift);
      }
    }
    pairp += 2;
  }
}    
    


Boolean
mark_weak_hash_vector(hash_table_vector_header *hashp, unsigned elements)
{
  unsigned flags = hashp->flags, key_dword, val_dword;
  Boolean 
    marked_new = false, 
    key_marked,
    val_marked,
    weak_value = ((flags & nhash_weak_value_mask) != 0);
  int 
    skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1,
    key_tag,
    val_tag,
    i;
  LispObj 
    *pairp = (LispObj*) (hashp+1),
    key,
    val;

  /* Mark everything in the header */
  
  for (i = 2; i<= skip; i++) {
    mark_root(deref(hashp,i));
  }

  elements -= skip;

  for (i = 0; i<elements; i+=2, pairp+=2) {
    key = pairp[0];
    val = pairp[1];
    key_marked = val_marked = true;
    key_tag = fulltag_of(key);
    val_tag = fulltag_of(val);
    if (is_node_fulltag(key_tag)) {
      key_dword = gc_area_dword(key);
      if ((key_dword < GCndwords_in_area) &&
          ! ref_bit(GCmarkbits,key_dword)) {
        key_marked = false;
      }
    }
    if (is_node_fulltag(val_tag)) {
      val_dword = gc_area_dword(val);
      if ((val_dword < GCndwords_in_area) &&
          ! ref_bit(GCmarkbits,val_dword)) {
        val_marked = false;
      }
    }

    if (weak_value) {
      if (val_marked & !key_marked) {
        mark_root(key);
        marked_new = true;
      }
    } else {
      if (key_marked & !val_marked) {
        mark_root(val);
        marked_new = true;
      }
    }
  }
  return marked_new;
}


Boolean
mark_weak_alist(LispObj weak_alist, int weak_type)
{
  int elements = header_element_count(header_of(weak_alist));
  unsigned dword;
  int pair_tag;
  Boolean marked_new = false;
  LispObj alist, pair, key, value;
  bitvector markbits = GCmarkbits;

  if (weak_type >> population_termination_bit) {
    elements -= 1;
  }
  for(alist = deref(weak_alist, elements);
      (fulltag_of(alist) == fulltag_cons) &&
      ((dword = gc_area_dword(alist)) < GCndwords_in_area) &&
      (! ref_bit(markbits,dword));
      alist = cdr(alist)) {
    pair = car(alist);
    pair_tag = fulltag_of(pair);
    if ((is_node_fulltag(pair_tag)) &&
        ((dword = gc_area_dword(pair_tag)) < GCndwords_in_area) &&
        (! ref_bit(markbits,dword))) {
      if (pair_tag == fulltag_cons) {
        key = car(pair);
        if ((! is_node_fulltag(fulltag_of(key))) ||
            ((dword = gc_area_dword(key)) >= GCndwords_in_area) ||
            ref_bit(markbits,dword)) {
          /* key is marked, mark value if necessary */
          value = cdr(pair);
          if (is_node_fulltag(fulltag_of(value)) &&
              ((dword = gc_area_dword(value)) < GCndwords_in_area) &&
              (! ref_bit(markbits,dword))) {
            mark_root(value);
            marked_new = true;
          }
        }
      } else {
          mark_root(pair);
          marked_new = true;
      }
    }
  }
  return marked_new;
}
  
void
markhtabvs()
{
  LispObj this, header, pending;
  int subtag;
  bitvector markbits = GCmarkbits;
  hash_table_vector_header *hashp;
  Boolean marked_new;

  do {
    pending = (LispObj) NULL;
    marked_new = false;
    
    while (GCweakvll) {
      this = GCweakvll;
      GCweakvll = deref(this,1);
      
      header = header_of(this);
      subtag = header_subtag(header);
      
      if (subtag == subtag_weak) {
        int weak_type = deref(this,2);
        deref(this,1) = pending;
        pending = this;
        if ((weak_type & population_type_mask) == population_weak_alist) {
          if (mark_weak_alist(this, weak_type)) {
            marked_new = true;
          }
        }
      } else if (subtag == subtag_hash_vector) {
        int elements = header_element_count(header), i;

        hashp = (hash_table_vector_header *) untag(this);
        if (hashp->flags & nhash_weak_mask) {
          deref(this,1) = pending;
          pending = this;
          if (mark_weak_hash_vector(hashp, elements)) {
            marked_new = true;
          }
        } else {
          deref(this,1) = (LispObj)NULL;
          for (i = 2; i <= elements; i++) {
            mark_root(deref(this,i));
          }
        } 
      } else {
        Bug(NULL, "Strange object on weak vector linked list: 0x~08x\n", this);
      }
    }

    if (marked_new) {
      GCweakvll = pending;
    }
  } while (marked_new);

  /* Now, everything's marked that's going to be,  and "pending" is a list
     of populations and weak hash tables.  CDR down that list and free
     anything that isn't marked.
     */

  while (pending) {
    this = pending;
    pending = deref(this,1);
    deref(this,1) = (LispObj)NULL;

    subtag = header_subtag(header_of(this));
    if (subtag == subtag_weak) {
      reapweakv(this);
    } else {
      reaphashv(this);
    }
  }

  /* Finally, mark the termination lists in all terminatable weak vectors
     They are now linked together on GCweakvll.
     This is where to store  lisp_global(TERMINATION_LIST) if we decide to do that,
     but it will force terminatable popualations to hold on to each other
     (set TERMINATION_LIST before clearing GCweakvll, and don't clear deref(this,1)).
     */
  pending = GCweakvll;
  GCweakvll = (LispObj)NULL;
  while (pending) {
    this = pending;
    pending = deref(this,1);
    deref(this,1) = (LispObj)NULL;
    mark_root(deref(this,1+3));
  }
}

/* Mark the lisp objects in an exception frame */
void
mark_xp(ExceptionInformation *xp)
{
  unsigned long *regs = (unsigned long *) xpGPRvector(xp);
  int r;


  /* registers >= fn should be tagged and marked as roots.
     the PC, LR, loc_pc, and CTR should be treated as "pc_locatives".

     In general, marking a locative is more expensive than marking
     a node is, since it may be neccessary to back up and find the
     containing object's header.  Since exception frames contain
     many locatives, it'd be wise to mark them *after* marking the
     stacks, nilreg-relative globals, etc.
     */

  for (r = fn; r < 32; r++) {
    mark_root((regs[r]));
  }



  mark_pc_root((regs[loc_pc]));
  mark_pc_root((LispObj)xpPC(xp));
  mark_pc_root((LispObj)xpLR(xp));
  mark_pc_root((LispObj)xpCTR(xp));

}
void
mark_tcr_tlb(TCR *tcr)
{
  unsigned n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  mark_simple_area_range(start, end);
}

/*
  Mark things that're only reachable through some (suspended) TCR.
  (This basically means the tcr's gc_context and the exception
  frames on its xframe_list.)
*/

void
mark_tcr_xframes(TCR *tcr)
{
  xframe_list *xframes;
  ExceptionInformation *xp;

  xp = tcr->gc_context;
  if (xp) {
    mark_xp(xp);
  }
  
  for (xframes = (xframe_list *) tcr->xframe; 
       xframes; 
       xframes = xframes->prev) {
      mark_xp(xframes->curr);
  }
}
      
      
void
reap_gcable_ptrs()
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next, ptr;
  xmacptr_flag flag;
  unsigned dword;
  xmacptr *x;

  while((next = *prev) != (LispObj)NULL) {
    dword = gc_area_dword(next);
    x = (xmacptr *) untag(next);

    if ((dword >= GCndwords_in_area) ||
        (ref_bit(GCmarkbits,dword))) {
      prev = &(x->link);
    } else {
      *prev = x->link;
      flag = (xmacptr_flag)(x->flags);
      ptr = x->address;

      if (ptr) {
        switch (flag) {
        case xmacptr_flag_recursive_lock:
	  destroy_recursive_lock(ptr);
          break;

        case xmacptr_flag_ptr:
	  deallocate((char *)ptr);
          break;

        case xmacptr_flag_rwlock:
          break;

        case xmacptr_flag_semaphore:
	  destroy_semaphore((void**)&(x->address));
          break;

        default:
          /* (warn "unknown xmacptr_flag: ~s" flag) */
          /* Unknowd, and perhaps unknowdable. */
          /* Fall in: */
        case xmacptr_flag_none:
          break;
        }
      }
    }
  }
}



#if 1
const unsigned char _one_bits[256] = {
    0*8,1*8,1*8,2*8,1*8,2*8,2*8,3*8,1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,5*8,6*8,6*8,7*8,6*8,7*8,7*8,8*8
};

#define one_bits(x) _one_bits[x]

#else
#define one_bits(x) logcount16(x)

#endif

/* A "pagelet" contains 32 doublewords.  The relocation table contains
   a word for each pagelet which defines the lowest address to which
   dwords on that pagelet will be relocated.

   The relocation address of a given pagelet is the sum of the relocation
   address for the preceding pagelet and the number of bytes occupied by
   marked objects on the preceding pagelet.
*/

LispObj
calculate_relocation()
{
  LispObj *relocptr = GCrelocptr;
  LispObj current = GCarealow;
  bitvector markbits = GCmarkbits;
  unsigned char *bytep = (unsigned char *) markbits;
  unsigned npagelets = ((GCndwords_in_area+31)>>5);
  unsigned thesebits;
  LispObj first = 0;

  do {
    *relocptr++ = current;
    thesebits = *markbits++;
    if (thesebits == 0xffffffff) {
      current += 32*8;
      bytep += 4;
    } else {
      if (!first) {
        first = current;
        while (thesebits & 0x80000000) {
          first += 8;
          thesebits += thesebits;
        }
      }
      current += one_bits(*bytep++);
      current += one_bits(*bytep++);
      current += one_bits(*bytep++);
      current += one_bits(*bytep++);
    }
  } while(--npagelets);
  *relocptr++ = current;
  return first ? first : current;
}

LispObj
dword_forwarding_address(unsigned dword, int tag_n)
{
  unsigned pagelet, nbits;
  unsigned short near_bits;
  LispObj new;

  if (GCDebug) {
    if (! ref_bit(GCmarkbits, dword)) {
      Bug(NULL, "unmarked object being forwarded!\n");
    }
  }

  pagelet = dword >> 5;
  nbits = dword & 0x1f;
  near_bits = ((unsigned short *)GCmarkbits)[dword>>4];

  if (nbits < 16) {
    new = GCrelocptr[pagelet] + tag_n;;
    /* Increment "new" by the count of 1 bits which precede the dword */
    if (near_bits == 0xffff) {
      return (new + (nbits << 3));
    } else {
      near_bits &= (0xffff0000 >> nbits);
      if (nbits > 7) {
        new += one_bits(near_bits & 0xff);
      }
      return (new + (one_bits(near_bits >> 8))); 
    }
  } else {
    new = GCrelocptr[pagelet+1] + tag_n;
    nbits = 32-nbits;

    if (near_bits == 0xffff) {
      return (new - (nbits << 3));
    } else {
      near_bits &= (1<<nbits)-1;
      if (nbits > 7) {
        new -= one_bits(near_bits >> 8);
      }
      return (new -  one_bits(near_bits & 0xff));
    }
  }
}


LispObj
locative_forwarding_address(LispObj obj)
{
  int tag_n = fulltag_of(obj);
  unsigned dword;

  /* Locatives can be tagged as conses, "fulltag_misc"
     objects, or as fixnums.  Immediates, headers, and nil
     shouldn't be "forwarded".  Nil never will be, but it
     doesn't hurt to check ... */

  if ((1<<tag_n) & ((1<<fulltag_immheader) |
                    (1<<fulltag_nodeheader) |
                    (1<<fulltag_imm) |
                    (1<<fulltag_nil))) {
    return obj;
  }

  dword = gc_area_dword(obj);

  if ((dword >= GCndwords_in_area) ||
      (obj < GCfirstunmarked)) {
    return obj;
  }

  return dword_forwarding_address(dword, tag_n);
}

LispObj
node_forwarding_address(LispObj node)
{
  int tag_n;
  unsigned dword = gc_area_dword(node);

  if ((dword >= GCndwords_in_area) ||
      (node < GCfirstunmarked)) {
    return node;
  }

  tag_n = fulltag_of(node);
  if (!is_node_fulltag(tag_n)) {
    return node;
  }

  return dword_forwarding_address(dword, tag_n);
}

Boolean
update_noderef(LispObj *noderef)
{
  LispObj
    node = *noderef,
    new = node_forwarding_address(node);

  if (new != node) {
    *noderef = new;
    return true;
  }
  return false;
}

void
update_locref(LispObj *locref)
{
  LispObj
    obj = *locref,
    new = locative_forwarding_address(obj);

  if (new != obj) {
    *locref = new;
  }
}

void
forward_gcable_ptrs()
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((next = *prev) != (LispObj)NULL) {
    *prev = node_forwarding_address(next);
    prev = &(((xmacptr *)(untag(next)))->link);
  }
}

void
forward_range(LispObj *range_start, LispObj *range_end)
{
  LispObj *p = range_start, node, new;
  int tag_n, nwords;
  hash_table_vector_header *hashp;

  while (p < range_end) {
    node = *p;
    tag_n = fulltag_of(node);
    if (tag_n == fulltag_immheader) {
      p = (LispObj *) skip_over_ivector((unsigned) p, node);
    } else if (tag_n == fulltag_nodeheader) {
      nwords = header_element_count(node);
      nwords += (1- (nwords&1));
      if ((header_subtag(node) == subtag_hash_vector) &&
          ((((hash_table_vector_header *)p)->flags) & nhash_track_keys_mask)) {
        int skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1;
        hashp = (hash_table_vector_header *) p;
        p++;
        nwords -= skip;
        while(skip--) {
          update_noderef(p);
          p++;
        }
        /* "nwords" is odd at this point: there are (floor nwords 2)
           key/value pairs to look at, and then an extra word for
           alignment.  Process them two at a time, then bump "p"
           past the alignment word. */
        nwords >>= 1;
        while(nwords--) {
          if (update_noderef(p) && hashp) {
            hashp->flags |= nhash_key_moved_mask;
            hashp = NULL;
          }
          p++;
          update_noderef(p);
          p++;
        }
        *p++ = 0;
      } else {
        p++;
        while(nwords--) {
          update_noderef(p);
          p++;
        }
      }
    } else {
      new = node_forwarding_address(node);
      if (new != node) {
        *p = new;
      }
      p++;
      update_noderef(p);
      p++;
    }
  }
}


void
forward_memoized_area(area *a, unsigned num_memo_dwords)
{
  bitvector refbits = a->refbits;
  LispObj *p = (LispObj *) a->low, x1, x2, new;
  unsigned bits, bitidx, *bitsp, nextbit, diff, memo_dword = 0, hash_dword_limit = 0;
  int tag_x1;
  hash_table_vector_header *hashp = NULL;
  Boolean header_p;

  if (GCDebug) {
    check_refmap_consistency(p, p+(num_memo_dwords << 1), refbits);
  }

  /* This is pretty straightforward, but we have to note
     when we move a key in a hash table vector that wants
     us to tell it about that. */

  set_bitidx_vars(refbits, 0, bitsp, bits, bitidx);
  while (memo_dword < num_memo_dwords) {
    if (bits == 0) {
      int remain = 0x20 - bitidx;
      memo_dword += remain;
      p += (remain+remain);
      bits = *++bitsp;
      bitidx = 0;
    } else {
      nextbit = count_leading_zeros(bits);
      if ((diff = (nextbit - bitidx)) != 0) {
        memo_dword += diff;
        bitidx = nextbit;
        p += (diff+diff);
      }
      x1 = p[0];
      x2 = p[1];
      tag_x1 = fulltag_of(x1);
      bits &= ~(BIT0_MASK >> bitidx);
      header_p = (tag_x1 == fulltag_nodeheader);

      if (header_p &&
          (header_subtag(x1) == subtag_hash_vector)) {
        hashp = (hash_table_vector_header *) p;
        if (hashp->flags & nhash_track_keys_mask) {
          hash_dword_limit = memo_dword + ((header_element_count(x1)+2)>>1);
        } else {
          hashp = NULL;
        }
      }


      if (! header_p) {
        new = node_forwarding_address(x1);
        if (new != x1) {
          *p = new;
        }
      }
      p++;

      new = node_forwarding_address(x2);
      if (new != x2) {
        *p = new;
        if (memo_dword < hash_dword_limit) {
          hashp->flags |= nhash_key_moved_mask;
          hash_dword_limit = 0;
          hashp = NULL;
        }
      }
      p++;
      memo_dword++;
      bitidx++;

    }
  }
}

void
forward_dohs( void )
{
  doh_block_ptr doh_block = (doh_block_ptr) lisp_global(DOH_HEAD);
  while( doh_block ) {
    forward_range( &doh_block->data[0], &doh_block->data[doh_block_slots] );
    doh_block = doh_block->link;
  }
}


/* Forward a tstack area */
void
forward_tstack_area(area *a)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) a->active,
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = (LispObj *) *current;
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      forward_range(current+2, end);
    }
  }
}

/* Forward a vstack area */
void
forward_vstack_area(area *a)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  if (((unsigned)p) & 4) {
    update_noderef(p);
    p++;
  }
  forward_range(p, q);
}

void
forward_cstack_area(area *a)
{
  BytePtr
    current,
    next,
    limit = a->high,
    low = a->low;

  for (current = a->active; (current >= low) && (current < limit); current = next) {
    next = *((BytePtr *)current);
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) &&
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      update_noderef(&((lisp_frame *) current)->savefn);
      update_locref(&((lisp_frame *) current)->savelr);
    } else {
      /* Clear low 2 bits of "next", just in case */
      next = (BytePtr) (((unsigned)next) & ~3);
    }
  }
}

void
forward_xp(ExceptionInformation *xp)
{
  unsigned long *regs = (unsigned long *) xpGPRvector(xp);
  int r;

  /* registers >= fn should be tagged and forwarded as roots.
     the PC, LR, loc_pc, and CTR should be treated as "locatives".
     */

  for (r = fn; r < 32; r++) {
    update_noderef((LispObj*) (&(regs[r])));
  }

  update_locref((LispObj*) (&(regs[loc_pc])));

  update_locref((LispObj*) (&(xpPC(xp))));
  update_locref((LispObj*) (&(xpLR(xp))));
  update_locref((LispObj*) (&(xpCTR(xp))));
}

void
forward_tcr_tlb(TCR *tcr)
{
  unsigned n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  while (start < end) {
    update_noderef(start);
    start++;
  }
}

void
forward_tcr_xframes(TCR *tcr)
{
  xframe_list *xframes;
  ExceptionInformation *xp;

  xp = tcr->gc_context;
  if (xp) {
    forward_xp(xp);
  }
  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    forward_xp(xframes->curr);
  }
}


/*
  Compact the dynamic heap (from GCfirstunmarked through its end.)
  Return the doubleword address of the new freeptr.
  */

LispObj
compact_dynamic_heap()
{
  LispObj *src = (LispObj*) GCfirstunmarked, *dest = src, node, new;
  unsigned elements, dword = gc_area_dword(GCfirstunmarked), node_dwords = 0, imm_dwords = 0;
  unsigned bitidx, *bitsp, bits, nextbit, diff;
  int tag;
  bitvector markbits = GCmarkbits;
    /* keep track of whether or not we saw any
       code_vector headers, and only flush cache if so. */
  Boolean GCrelocated_code_vector = false;

  if (dword < GCndwords_in_area) {
    lisp_global(FWDNUM) += (1<<fixnum_shift);
  
    set_bitidx_vars(markbits,dword,bitsp,bits,bitidx);
    while (dword < GCndwords_in_area) {
      if (bits == 0) {
        int remain = 0x20 - bitidx;
        dword += remain;
        src += (remain+remain);
        bits = *++bitsp;
        bitidx = 0;
      } else {
        /* Have a non-zero markbits word; all bits more significant than
           "bitidx" are 0.  Count leading zeros in "bits" (there'll be
           at least "bitidx" of them.)  If there are more than "bitidx"
           leading zeros, bump "dword", "bitidx", and "src" by the difference. */
        nextbit = count_leading_zeros(bits);
        if ((diff = (nextbit - bitidx)) != 0) {
          dword += diff;
          bitidx = nextbit;
          src += (diff+diff);
        }

        if (GCDebug) {
          if (dest != (LispObj*)locative_forwarding_address((LispObj)src)) {
            Bug(NULL, "Out of synch in heap compaction.  Forwarding from 0x%08x to 0x%08x,\n expected to go to 0x%08x\n", 
                src, dest, locative_forwarding_address((LispObj)src));
          }
        }

        node = *src++;
        tag = fulltag_of(node);
        if (tag == fulltag_nodeheader) {
          elements = header_element_count(node);
          node_dwords = (elements+2)>>1;
          dword += node_dwords;
          if ((header_subtag(node) == subtag_hash_vector) &&
              (((hash_table_vector_header *) (src-1))->flags & nhash_track_keys_mask)) {
            hash_table_vector_header *hashp = (hash_table_vector_header *) dest;
            int skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1;
          
            *dest++ = node;
            elements -= skip;
            while(skip--) {
              *dest++ = node_forwarding_address(*src++);
            }
            /* There should be an even number of (key/value) pairs in elements;
               an extra alignment word follows. */
            elements >>= 1;
            while (elements--) {
              if (hashp) {
                node = *src++;
                new = node_forwarding_address(node);
                if (new != node) {
                  hashp->flags |= nhash_key_moved_mask;
                  hashp = NULL;
                  *dest++ = new;
                } else {
                  *dest++ = node;
                }
              } else {
                *dest++ = node_forwarding_address(*src++);
              }
              *dest++ = node_forwarding_address(*src++);
            }
            *dest++ = 0;
            src++;
          } else {
            *dest++ = node;
            *dest++ = node_forwarding_address(*src++);
            while(--node_dwords) {
              *dest++ = node_forwarding_address(*src++);
              *dest++ = node_forwarding_address(*src++);
            }
          }
          set_bitidx_vars(markbits,dword,bitsp,bits,bitidx);
        } else if (tag == fulltag_immheader) {
          *dest++ = node;
          *dest++ = *src++;
          elements = header_element_count(node);
          tag = header_subtag(node);
          if (tag <= max_32_bit_ivector_subtag) {
            if (tag == subtag_code_vector) {
              GCrelocated_code_vector = true;
            }
            imm_dwords = (((elements+1)+1)>>1);
          } else if (tag <= max_8_bit_ivector_subtag) {
            imm_dwords = (((elements+4)+7)>>3);
          } else if (tag <= max_16_bit_ivector_subtag) {
            imm_dwords = (((elements+2)+3)>>2);
          } else if (tag == subtag_bit_vector) {
            imm_dwords = (((elements+32)+63)>>6);
          } else {
            imm_dwords = elements+1;
          }
          dword += imm_dwords;
          while (--imm_dwords) {
            *dest++ = *src++;
            *dest++ = *src++;
          }
          set_bitidx_vars(markbits,dword,bitsp,bits,bitidx);
        } else {
          *dest++ = node_forwarding_address(node);
          *dest++ = node_forwarding_address(*src++);
          bits &= ~(BIT0_MASK >> bitidx);
          dword++;
          bitidx++;
        }
      }
  
    }

    {
      unsigned nbytes = (unsigned)dest - (unsigned)GCfirstunmarked;
      if ((nbytes != 0) && GCrelocated_code_vector) {
        xMakeDataExecutable((LogicalAddress)GCfirstunmarked, nbytes);
      }
    }
  }
  return (LispObj)dest;
}


Boolean
youngest_non_null_area_p (area *a)
{
  if (a->active == a->high) {
    return false;
  } else {
    for (a = a->younger; a; a = a->younger) {
      if (a->active != a->high) {
        return false;
      }
    }
  };
  return true;
}

Boolean just_purified_p = false;


/*
  All thread's stack areas have been "normalized", as
  has the dynamic heap.  (The "active" pointer in these areas
  matches the stack pointer/freeptr value at the time that
  the exception occurred.)
*/


#define get_time(when) gettimeofday(&when, NULL)

void 
gc(TCR *tcr)
{
  xframe_list *xframes = (tcr->xframe);
  struct timeval start, stop;
  area *a = active_dynamic_area, *to = NULL, *from = NULL, *note = NULL;
  unsigned timeidx = 1;
  xframe_list *x;
  special_binding *sb = (tcr->db_link);
  LispObj
    pkg,
    itabvec = 0;
  BytePtr oldfree = a->active;
  TCR *other_tcr;

  /* make_page_node_map((LispObj) a->low, (LispObj)a->active); */
  get_time(start);
  lisp_global(IN_GC) = (1<<fixnumshift);

  GCephemeral_low = lisp_global(OLDEST_EPHEMERAL);
  if (GCephemeral_low) {
    GCn_ephemeral_dwords=area_dword(oldfree, GCephemeral_low);
    update_area_refmaps(oldfree);
  } else {
    if (a->younger) {
      unprotect_area(oldspace_protected_area);
    }
    GCn_ephemeral_dwords = 0;
  }
  
  GCDebug = ((nrs_GC_EVENT_STATUS_BITS.vcell & gc_integrity_check_bit) != 0);

  if (just_purified_p) {
    just_purified_p = false;
  } else {
    if (GCDebug) {
      check_all_areas();
    }
  }

  if (GCephemeral_low) {
    if ((oldfree-g1_area->low) < g1_area->threshold) {
      to = g1_area;
      note = a;
      timeidx = 4;
    } else if ((oldfree-g2_area->low) < g2_area->threshold) {
      to = g2_area;
      from = g1_area;
      note = g1_area;
      timeidx = 3;
    } else {
      to = tenured_area;
      from = g2_area;
      note = g2_area;
      timeidx = 2;
    } 
  } else {
    note = tenured_area;
  }


  if (from) {
    untenure_from_area(from);
  }
      
  GCmarkbits = a->markbits;
  GCarealow = (LispObj) a->low,
  GCndwords_in_area = gc_area_dword(oldfree);

  zero_bits(GCmarkbits, GCndwords_in_area);
  GCweakvll = (LispObj)NULL;


  if (GCn_ephemeral_dwords == 0) {
    /* For GCTWA, mark the internal package hash table vector of
     *PACKAGE*, but don't mark its contents. */
    {
      LispObj
        itab;
      unsigned
        dword, ndwords;
      
      pkg = nrs_PACKAGE.vcell;
      if ((fulltag_of(pkg) == fulltag_misc) &&
          (header_subtag(header_of(pkg)) == subtag_package)) {
        itab = ((package *)(untag(pkg)))->itab;
        itabvec = car(itab);
        dword = gc_area_dword(itabvec);
        if (dword < GCndwords_in_area) {
          ndwords = (header_element_count(header_of(itabvec))+1) >> 1;
          set_n_bits(GCmarkbits, dword, ndwords);
        }
      }
    }
  }

  {
    area *next_area;
    area_code code;

    /* Could make a jump table instead of the typecase */

    for (next_area = a->succ; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
      switch (code) {
      case AREA_TSTACK:
        mark_tstack_area(next_area);
        break;

      case AREA_VSTACK:
        mark_vstack_area(next_area);
        break;

      case AREA_CSTACK:
        mark_cstack_area(next_area);
        break;

      case AREA_STATIC:
      case AREA_DYNAMIC:                  /* some heap that isn't "the" heap */
        /* In both of these cases, we -could- use the area's "markbits"
           bitvector as a reference map.  It's safe (but slower) to
           ignore that map and process the entire area.
           */
        if (next_area->younger == NULL) {
          mark_simple_area_range((LispObj *) next_area->low, (LispObj *) next_area->active);
        }
        break;

      default:
        break;
      }
    }
  }
  
  if (lisp_global(OLDEST_EPHEMERAL)) {
    mark_memoized_area(tenured_area, area_dword(a->low,tenured_area->low));
  }

  other_tcr = tcr;
  do {
    mark_tcr_xframes(other_tcr);
    mark_tcr_tlb(other_tcr);
    other_tcr = other_tcr->next;
  } while (other_tcr != tcr);

  mark_dohs();          /* Creole */



    /* Go back through *package*'s internal symbols, marking
       any that aren't worthless.
       */
    
  if (itabvec) {
    int
      i,
      n = header_element_count(header_of(itabvec));
    LispObj
      sym,
      *raw = 1+((LispObj *)(untag(itabvec)));

    for (i = 0; i < n; i++) {
      sym = *raw++;
      if (fulltag_of(sym) == fulltag_misc) {
        lispsymbol *rawsym = (lispsymbol *)(untag(sym));
        unsigned dword = gc_area_dword(sym);
          
        if ((dword < GCndwords_in_area) &&
            (!ref_bit(GCmarkbits,dword))) {
          /* Symbol is in GC area, not marked.
             Mark it if fboundp, boundp, or if
             it has a plist or another home package.
             */
            
          if (FBOUNDP(rawsym) ||
              BOUNDP(rawsym) ||
              (rawsym->flags != 0) || /* SPECIAL, etc. */
              ((rawsym->package_plist != pkg) &&
               (rawsym->package_plist != lisp_nil))) {
            mark_root(sym);
          }
        }
      }
    }
  }

  (void)markhtabvs();

  if (itabvec) {
    int
      i,
      n = header_element_count(header_of(itabvec));
    LispObj
      sym,
      *raw = 1+((LispObj *)(untag(itabvec)));

    for (i = 0; i < n; i++, raw++) {
      sym = *raw;
      if (fulltag_of(sym) == fulltag_misc) {
        lispsymbol *rawsym = (lispsymbol *)(untag(sym));
        unsigned dword = gc_area_dword(sym);

        if ((dword < GCndwords_in_area) &&
            (!ref_bit(GCmarkbits,dword))) {
          *raw = unbound;
        }
      }
    }
  }
  
  reap_gcable_ptrs();

  GCrelocptr = global_reloctab;
  GCfirstunmarked = calculate_relocation();

  forward_range((LispObj *) GCarealow, (LispObj *) GCfirstunmarked);

  other_tcr = tcr;
  do {
    forward_tcr_xframes(other_tcr);
    forward_tcr_tlb(other_tcr);
    other_tcr = other_tcr->next;
  } while (other_tcr != tcr);

  forward_dohs();               /* Creole */
  
  forward_gcable_ptrs();



  {
    area *next_area;
    area_code code;

    /* Could make a jump table instead of the typecase */

    for (next_area = a->succ; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
      switch (code) {
      case AREA_TSTACK:
        forward_tstack_area(next_area);
        break;

      case AREA_VSTACK:
        forward_vstack_area(next_area);
        break;

      case AREA_CSTACK:
        forward_cstack_area(next_area);
        break;

      case AREA_STATIC:
      case AREA_DYNAMIC:                  /* some heap that isn't "the" heap */
        if (next_area->younger == NULL) {
          forward_range((LispObj *) next_area->low, (LispObj *) next_area->active);
        }
        break;

      default:
        break;
      }
    }
  }
  
  if (GCephemeral_low) {
    forward_memoized_area(tenured_area, area_dword(a->low, tenured_area->low));
  }

  
  a->active = (BytePtr) compact_dynamic_heap();
  
  /* Need to do this before protection kicks back in */
  zero_last_page(a->active);

  if (to) {
    tenure_to_area(to);
  }

  /*
    If the EGC is enabled:
     If there's no room for the youngest generation, untenure everything.
     If this was a full GC and there's now room for the youngest generation,
     tenure everything.
     */
  resize_dynamic_heap(a->active,
		      (GCephemeral_low == 0) ? lisp_heap_gc_threshold : 0);

  if (a->older != NULL) {
    unsigned nfree = (a->high - a->active);


    if (nfree < a->threshold) {
      untenure_from_area(tenured_area);
    } else {
      if (GCephemeral_low == 0) {
        tenure_to_area(tenured_area);
      }
    }
  }

  lisp_global(GC_NUM) += (1<<fixnumshift);
  if (note) {
    note->gccount += (1<<fixnumshift);
  }

  if (GCDebug) {
    check_all_areas();
  }

  other_tcr = tcr;
  do {
    other_tcr->gc_context = NULL;
    other_tcr = other_tcr->next;
  } while (other_tcr != tcr);
  
  lisp_global(IN_GC) = 0;

  nrs_GC_EVENT_STATUS_BITS.vcell |= gc_postgc_pending;
  get_time(stop);

  {
    lispsymbol * total_gc_microseconds = (lispsymbol *) &(nrs_TOTAL_GC_MICROSECONDS);
    lispsymbol * total_bytes_freed = (lispsymbol *) &(nrs_TOTAL_BYTES_FREED);
    LispObj val;
    struct timeval *timeinfo;

    val = total_gc_microseconds->vcell;
    if ((fulltag_of(val) == fulltag_misc) &&
        (header_subtag(header_of(val)) == subtag_macptr)) {
      timersub(&stop, &start, &stop);
      timeinfo = (struct timeval *) ((macptr *) (untag(val)))->address;
      timeradd(timeinfo,  &stop, timeinfo);
      timeradd(timeinfo+timeidx,  &stop, timeinfo+timeidx);
    }

    val = total_bytes_freed->vcell;
    if ((fulltag_of(val) == fulltag_misc) &&
        (header_subtag(header_of(val)) == subtag_macptr)) {
      long long justfreed = oldfree - a->active;
      *( (long long *) ((macptr *) (untag(val)))->address) += justfreed;
    }
  }
}

      
    
/*
  Total the (physical) byte sizes of all ivectors in the indicated memory range
*/

unsigned
unboxed_bytes_in_range(LispObj *start, LispObj *end)
{
  unsigned total=0, elements, tag, subtag, bytes;
  LispObj header;

  while (start < end) {
    header = *start;
    tag = fulltag_of(header);
    
    if ((tag == fulltag_nodeheader) ||
        (tag == fulltag_immheader)) {
      elements = header_element_count(header);
      if (tag == fulltag_nodeheader) {
        start += ((elements+2) & ~1);
      } else {
        subtag = header_subtag(header);

        if (subtag <= max_32_bit_ivector_subtag) {
          bytes = 4 + (elements<<2);
        } else if (subtag <= max_8_bit_ivector_subtag) {
          bytes = 4 + elements;
        } else if (subtag <= max_16_bit_ivector_subtag) {
          bytes = 4 + (elements<<1);
        } else if (subtag == subtag_double_float_vector) {
          bytes = 8 + (elements<<3);
        } else {
          bytes = 4 + ((elements+7)>>3);
        }
        bytes = (bytes+7) & ~7;
        total += bytes;
        start += (bytes >> 2);
      }
    } else {
      start += 2;
    }
  }
  return total;
}


/* 
  This assumes that it's getting called with a simple-{base,general}-string
  or code vector as an argument and that there's room for the object in the
  destination area.
*/


LispObj
purify_displaced_object(LispObj obj, area *dest, unsigned disp)
{
  BytePtr 
    free = dest->active,
    *old = (BytePtr *) untag(obj);
  LispObj 
    header = header_of(obj), 
    new;
  unsigned 
    subtag = header_subtag(header), 
    element_count = header_element_count(header),
    physbytes;

  switch(subtag) {
  case subtag_simple_base_string:
    physbytes = 4 + element_count;
    break;

  case subtag_simple_general_string:
    physbytes = 4 + (element_count << 1);
    break;

  case subtag_code_vector:
    physbytes = 4 + (element_count << 2);
    break;

  default:
    Bug(NULL, "Can't purify object at 0x%08x", obj);
    return obj;
  }
  physbytes = (physbytes+7)&~7;
  dest->active += physbytes;

  new = (LispObj)free+disp;

  memcpy(free, (BytePtr)old, physbytes);
  /* Leave a trail of breadcrumbs.  Or maybe just one breadcrumb. */
  /* Actually, it's best to always leave a trail, for two reasons.
     a) We may be walking the same heap that we're leaving forwaring
     pointers in, so we don't want garbage that we leave behind to
     look like a header.
     b) We'd like to be able to forward code-vector locatives, and
     it's easiest to do so if we leave a {forward_marker, dword_locative}
     pair at every doubleword in the old vector.
     */
  while(physbytes) {
    *old++ = (BytePtr) forward_marker;
    *old++ = (BytePtr) free;
    free += 8;
    physbytes -= 8;
  }
  return new;
}

LispObj
purify_object(LispObj obj, area *dest)
{
  return purify_displaced_object(obj, dest, fulltag_of(obj));
}


#define FORWARD_ONLY 0
#define COPY_CODE (1<<0)
#define COPY_STRINGS (1<<1)

void
copy_ivector_reference(LispObj *ref, BytePtr low, BytePtr high, area *dest, int what_to_copy)
{
  LispObj obj = *ref, header;
  unsigned tag = fulltag_of(obj), header_tag, header_subtag;

  if ((tag == fulltag_misc) &&
      (((BytePtr)obj) > low) &&
      (((BytePtr)obj) < high)) {
    header = deref(obj, 0);
    if (header == forward_marker) { /* already copied */
      *ref = (untag(deref(obj,1)) + tag);
    } else {
      header_tag = fulltag_of(header);
      if (header_tag == fulltag_immheader) {
        header_subtag = header_subtag(header);
        if (((header_subtag == subtag_code_vector) && (what_to_copy & COPY_CODE)) ||
            ((what_to_copy & COPY_STRINGS) && 
             ((header_subtag == subtag_simple_base_string) ||
              (header_subtag == subtag_simple_general_string)))) {
          *ref = purify_object(obj, dest);
        }
      }
    }
  }
}

void
purify_locref(LispObj *locaddr, BytePtr low, BytePtr high, area *to, int what)
{
  LispObj
    loc = *locaddr,
    header;
  unsigned
    tag = fulltag_of(loc);

  if (((BytePtr)loc > low) &&
      ((BytePtr)loc < high)) {
    LispObj *p = (LispObj *)(untag(loc));
    switch (tag) {
    case fulltag_even_fixnum:
    case fulltag_odd_fixnum:
      if (*p == forward_marker) {
        *locaddr = (p[1]+tag);
      } else {
        /* Grovel backwards until the header's found; copy
           the code vector to to space, then treat it as if it 
           hasn't already been copied. */
        do {
          p -= 2;
          tag += 8;
          header = *p;
        } while ((header & code_header_mask) != subtag_code_vector);
        *locaddr = purify_displaced_object((LispObj)p, to, tag);
      }
      break;

    case fulltag_misc:
      copy_ivector_reference(locaddr, low, high, to, what);
      break;
    }
  }
}

void
purify_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to, int what)
{
  LispObj header;
  unsigned tag;

  while (start < end) {
    header = *start;
    if (header == forward_marker) {
      start += 2;
    } else {
      tag = fulltag_of(header);
      if (tag == fulltag_immheader) {
        start = (LispObj *)skip_over_ivector((unsigned)start, header);
      } else {
        if (tag != fulltag_nodeheader) {
          copy_ivector_reference(start, low, high, to, what);
        }
        start++;
        copy_ivector_reference(start, low, high, to, what);
        start++;
      }
    }
  }
}
        
/* Purify references from tstack areas */
void
purify_tstack_area(area *a, BytePtr low, BytePtr high, area *to, int what)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) (a->active),
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = (LispObj *) *current;
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      purify_range(current+2, end, low, high, to, what);
    }
  }
}

/* Purify a vstack area */
void
purify_vstack_area(area *a, BytePtr low, BytePtr high, area *to, int what)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  if (((unsigned)p) & 4) {
    copy_ivector_reference(p, low, high, to, what);
    p++;
  }
  purify_range(p, q, low, high, to, what);
}

void
purify_cstack_area(area *a, BytePtr low, BytePtr high, area *to, int what)
{
  BytePtr
    current,
    next,
    limit = a->high;

  for (current = a->active; current != limit; current = next) {
    next = *((BytePtr *)current);
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) && 
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      purify_locref(&((lisp_frame *) current)->savelr, low, high, to, what);
    } else {
      /* Clear low 2 bits of "next", just in case */
      next = (BytePtr) (((unsigned)next) & ~3);
    }
  }
}

void
purify_xp(ExceptionInformation *xp, BytePtr low, BytePtr high, area *to, int what)
{
  unsigned long *regs = (unsigned long *) xpGPRvector(xp);
  int r;

  /* registers >= fn should be treated as roots.
     The PC, LR, loc_pc, and CTR should be treated as "locatives".
   */

  for (r = fn; r < 32; r++) {
    copy_ivector_reference((LispObj*) (&(regs[r])), low, high, to, what);
  };

  purify_locref((LispObj*) (&(regs[loc_pc])), low, high, to, what);

  purify_locref((LispObj*) (&(xpPC(xp))), low, high, to, what);
  purify_locref((LispObj*) (&(xpLR(xp))), low, high, to, what);
  purify_locref((LispObj*) (&(xpCTR(xp))), low, high, to, what);

  /* Don't purify loc_g. It doesn't point at a code_vector, and purify_locref
     handles only code vectors.
   */
}

void
purify_tcr_tlb(TCR *tcr, BytePtr low, BytePtr high, area *to, int what)
{
  unsigned n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  purify_range(start, end, low, high, to, what);
}

void
purify_tcr_xframes(TCR *tcr, BytePtr low, BytePtr high, area *to, int what)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  xp = tcr->gc_context;
  if (xp) {
    purify_xp(xp, low, high, to, what);
  }

  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    purify_xp(xframes->curr, low, high, to, what);
  }
}

void
purify_dohs(BytePtr low, BytePtr high, area *to, int what)
{
  doh_block_ptr doh_block = (doh_block_ptr) lisp_global(DOH_HEAD);
  while( doh_block ) {
    purify_range( &doh_block->data[0], &doh_block->data[doh_block_slots],
                 low, high, to, what );
    doh_block = doh_block->link;
  }
}

void
purify_areas(BytePtr low, BytePtr high, area *target, int what)
{
  area *next_area;
  area_code code;
      
  for (next_area = active_dynamic_area; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
    switch (code) {
    case AREA_TSTACK:
      purify_tstack_area(next_area, low, high, target, what);
      break;
      
    case AREA_VSTACK:
      purify_vstack_area(next_area, low, high, target, what);
      break;
      
    case AREA_CSTACK:
      purify_cstack_area(next_area, low, high, target, what);
      break;
      
    case AREA_STATIC:
    case AREA_DYNAMIC:
      purify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, target, what);
      break;
      
    default:
      break;
    }
  }
}

/*
  So far, this is mostly for save_application's benefit.
  We -should- be able to return to lisp code after doing this,
  however.

*/


int
purify(TCR *tcr)
{
  extern area *extend_readonly_area(unsigned);
  area 
    *a = active_dynamic_area,
    *new_pure_area;

  TCR  *other_tcr;
  unsigned max_pure_size;
  OSErr err;
  BytePtr new_pure_start;


  max_pure_size = unboxed_bytes_in_range((LispObj *)a->low, (LispObj *) a->active);
  new_pure_area = extend_readonly_area(max_pure_size);
  if (new_pure_area) {
    new_pure_start = new_pure_area->active;
    lisp_global(IN_GC) = (1<<fixnumshift);

    /* 
      First, loop thru *all-packages* and purify the pnames of all
      interned symbols.  Then walk every place that could reference
      a heap-allocated object (all_areas, the xframe_list) and
      purify code_vectors (and update the odd case of a shared
      reference to a pname.)
       
      Make the new_pure_area executable, just in case.

      Caller will typically GC again (and that should recover quite a bit of
      the dynamic heap.)
      */

    {
      lispsymbol *rawsym = (lispsymbol *)(&(nrs_ALL_PACKAGES));
      LispObj pkg_list = rawsym->vcell, htab, obj;
      package *p;
      cons *c;
      unsigned elements, i;

      while (fulltag_of(pkg_list) == fulltag_cons) {
        c = (cons *) untag(pkg_list);
        p = (package *) untag(c->car);
        pkg_list = c->cdr;
        c = (cons *) untag(p->itab);
        htab = c->car;
        elements = header_element_count(header_of(htab));
        for (i = 1; i<= elements; i++) {
          obj = deref(htab,i);
          if (fulltag_of(obj) == fulltag_misc) {
            rawsym = (lispsymbol *) untag(obj);
            copy_ivector_reference(&(rawsym->pname), a->low, a->active, new_pure_area, COPY_STRINGS);
          }
        }
        c = (cons *) untag(p->etab);
        htab = c->car;
        elements = header_element_count(header_of(htab));
        for (i = 1; i<= elements; i++) {
          obj = deref(htab,i);
          if (fulltag_of(obj) == fulltag_misc) {
            rawsym = (lispsymbol *) untag(obj);
            copy_ivector_reference(&(rawsym->pname), a->low, a->active, new_pure_area, COPY_STRINGS);
          }
        }
      }
    }
    
    purify_areas(a->low, a->active, new_pure_area, COPY_CODE);
    
    other_tcr = tcr;
    do {
      purify_tcr_xframes(other_tcr, a->low, a->active, new_pure_area, COPY_CODE);
      purify_tcr_tlb(other_tcr, a->low, a->active, new_pure_area, COPY_CODE);
      other_tcr = other_tcr->next;
    } while (other_tcr != tcr);

    purify_dohs(a->low, a->active, new_pure_area, COPY_CODE);

    {
      unsigned puresize = (unsigned) (new_pure_area->active-new_pure_start);
      if (puresize != 0) {
        xMakeDataExecutable(new_pure_start, puresize);
  
      }
    }
    ProtectMemory(new_pure_area->low,
		  align_to_power_of_2(new_pure_area->active-new_pure_area->low,
				      12));
    lisp_global(IN_GC) = 0;
    just_purified_p = true;
    return 0;
  }
  return -1;
}

void
impurify_locref(LispObj *p, LispObj low, LispObj high, int delta)
{
  LispObj q = *p;
  
  switch (fulltag_of(q)) {
  case fulltag_misc:
  case fulltag_even_fixnum:
  case fulltag_odd_fixnum:
    if ((q >= low) && (q < high)) {
      *p = (q+delta);
    }
  }
}

  
void
impurify_noderef(LispObj *p, LispObj low, LispObj high, int delta)
{
  LispObj q = *p;
  
  if ((fulltag_of(q) == fulltag_misc) &&
      (q >= low) && 
      (q < high)) {
    *p = (q+delta);
  }
}
  

void
impurify_cstack_area(area *a, LispObj low, LispObj high, int delta)
{
  BytePtr
    current,
    next,
    limit = a->high;

  for (current = a->active; current != limit; current = next) {
    next = *((BytePtr *)current);
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) && 
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      impurify_locref(&((lisp_frame *) current)->savelr, low, high, delta);
    } else {
      /* Clear low 2 bits of "next", just in case */
      next = (BytePtr) (((unsigned)next) & ~3);
    }
  }
}

void
impurify_xp(ExceptionInformation *xp, LispObj low, LispObj high, int delta)
{
  unsigned long *regs = (unsigned long *) xpGPRvector(xp);
  int r;

  /* registers >= fn should be treated as roots.
     The PC, LR, loc_pc, and CTR should be treated as "locatives".
   */

  for (r = fn; r < 32; r++) {
    impurify_noderef((LispObj*) (&(regs[r])), low, high, delta);
  };

  impurify_locref((LispObj*) (&(regs[loc_pc])), low, high, delta);

  impurify_locref((LispObj*) (&(xpPC(xp))), low, high, delta);
  impurify_locref((LispObj*) (&(xpLR(xp))), low, high, delta);
  impurify_locref((LispObj*) (&(xpCTR(xp))), low, high, delta);

}


void
impurify_range(LispObj *start, LispObj *end, LispObj low, LispObj high, int delta)
{
  LispObj header;
  unsigned tag;

  while (start < end) {
    header = *start;
    tag = fulltag_of(header);
    if (tag == fulltag_immheader) {
      start = (LispObj *)skip_over_ivector((unsigned)start, header);
    } else {
      if (tag != fulltag_nodeheader) {
        impurify_noderef(start, low, high, delta);
        }
      start++;
      impurify_noderef(start, low, high, delta);
      start++;
    }
  }
}




void
impurify_tcr_tlb(TCR *tcr,  LispObj low, LispObj high, int delta)
{
  unsigned n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);
  
  impurify_range(start, end, low, high, delta);
}

void
impurify_tcr_xframes(TCR *tcr, LispObj low, LispObj high, int delta)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  xp = tcr->gc_context;
  if (xp) {
    impurify_xp(xp, low, high, delta);
  }

  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    impurify_xp(xframes->curr, low, high, delta);
  }
}

void
impurify_tstack_area(area *a, LispObj low, LispObj high, int delta)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) (a->active),
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = (LispObj *) *current;
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      impurify_range(current+2, end, low, high, delta);
    }
  }
}
void
impurify_vstack_area(area *a, LispObj low, LispObj high, int delta)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  if (((unsigned)p) & 4) {
    impurify_noderef(p, low, high, delta);
    p++;
  }
  impurify_range(p, q, low, high, delta);
}


void
impurify_areas(LispObj low, LispObj high, int delta)
{
  area *next_area;
  area_code code;
      
  for (next_area = active_dynamic_area; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
    switch (code) {
    case AREA_TSTACK:
      impurify_tstack_area(next_area, low, high, delta);
      break;
      
    case AREA_VSTACK:
      impurify_vstack_area(next_area, low, high, delta);
      break;
      
    case AREA_CSTACK:
      impurify_cstack_area(next_area, low, high, delta);
      break;
      
    case AREA_STATIC:
    case AREA_DYNAMIC:
      impurify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, delta);
      break;
      
    default:
      break;
    }
  }
}

int
impurify(TCR *tcr)
{
  area *r = find_readonly_area();

  if (r) {
    area *a = active_dynamic_area;
    BytePtr ro_base = r->low, ro_limit = r->active, oldfree = a->active;
    unsigned n = ro_limit - ro_base;
    int delta = oldfree-ro_base;
    TCR *other_tcr;

    if (n) {
      lisp_global(IN_GC) = 1;
      resize_dynamic_heap(oldfree, n);
      a->active += n;
      bcopy(ro_base, oldfree, n);
      munmap(ro_base, n);
      a->ndwords = area_dword(a, a->active);
      pure_space_active = r->active = r->low;
      r->ndwords = 0;

      impurify_areas((LispObj)ro_base, (LispObj)ro_limit, delta);

      other_tcr = tcr;
      do {
        impurify_tcr_xframes(other_tcr, (LispObj)ro_base, (LispObj)ro_limit, delta);
        impurify_tcr_tlb(other_tcr, (LispObj)ro_base, (LispObj)ro_limit, delta);
        other_tcr = other_tcr->next;
      } while (other_tcr != tcr);
      lisp_global(IN_GC) = 0;
    }
    return 0;
  }
  return -1;
}
