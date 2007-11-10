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
#include "lisp_globals.h"
#include "bits.h"
#include "gc.h"
#include "area.h"
#include "Threads.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

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

#ifdef PPC
#ifdef PPC64
  case fulltag_imm_0:
  case fulltag_imm_1:
  case fulltag_imm_2:
  case fulltag_imm_3:
#else
  case fulltag_imm:
#endif
#endif

    return;

#ifndef PPC64
  case fulltag_nil:
    if (n != lisp_nil) {
      Bug(NULL,"Object tagged as nil, not nil : 0x%08x", n);
    }
    return;
#endif

#ifdef PPC
#ifdef PPC64
  case fulltag_nodeheader_0: 
  case fulltag_nodeheader_1: 
  case fulltag_nodeheader_2: 
  case fulltag_nodeheader_3: 
  case fulltag_immheader_0: 
  case fulltag_immheader_1: 
  case fulltag_immheader_2: 
  case fulltag_immheader_3: 
#else
  case fulltag_nodeheader:
  case fulltag_immheader:
#endif
#endif

    Bug(NULL, "Header not expected : 0x%lx", n);
    return;

  case fulltag_misc:
  case fulltag_cons:
    a = heap_area_containing((BytePtr)ptr_from_lispobj(n));
    
    if (a == NULL) {
      /* Can't do as much sanity checking as we'd like to
         if object is a defunct stack-consed object.
         If a dangling reference to the heap, that's
         bad .. */
      a = active_dynamic_area;
      if ((n > (ptr_to_lispobj(a->active))) &&
          (n < (ptr_to_lispobj(a->high)))) {
        Bug(NULL, "Node points to heap free space: 0x%lx", n);
      }
      return;
    }
    break;
  }
  /* Node points to heap area, so check header/lack thereof. */
  header = header_of(n);
  header_tag = fulltag_of(header);
  if (tag == fulltag_cons) {
    if ((nodeheader_tag_p(header_tag)) ||
        (immheader_tag_p(header_tag))) {
      Bug(NULL, "Cons cell at 0x%lx has bogus header : 0x%lx", n, header);
    }
    return;
  }

  if ((!nodeheader_tag_p(header_tag)) &&
      (!immheader_tag_p(header_tag))) {
    Bug(NULL,"Vector at 0x%lx has bogus header : 0x%lx", n, header);
  }
  return;
}

Boolean GCDebug = false, GCverbose = false;



void
check_range(LispObj *start, LispObj *end, Boolean header_allowed)
{
  LispObj node, *current = start, *prev;
  int tag;
  natural elements;

  while (current < end) {
    prev = current;
    node = *current++;
    tag = fulltag_of(node);
    if (immheader_tag_p(tag)) {
      if (! header_allowed) {
        Bug(NULL, "Header not expected at 0x%lx\n", prev);
      }
      current = (LispObj *)skip_over_ivector((natural)prev, node);
    } else if (nodeheader_tag_p(tag)) {
      if (! header_allowed) {
        Bug(NULL, "Header not expected at 0x%lx\n", prev);
      }
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
    case AREA_MANAGED_STATIC:
      check_range((LispObj *)a->low, (LispObj *)a->active, true);
      break;

    case AREA_VSTACK:
      {
        LispObj* low = (LispObj *)a->active;
        LispObj* high = (LispObj *)a->high;
        
        if (((natural)low) & node_size) {
          check_node(*low++);
        }
        check_range(low, high, false);
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
          next = ptr_from_lispobj(*current);
          end = ((next >= start) && (next < limit)) ? next : limit;
          if (current[1] == 0) {
            check_range(current+2, end, true);
          }
        }
      }
      break;
    }
    a = a->succ;
    code = (a->code);
  }
}

natural
static_dnodes_for_area(area *a)
{
  if (a->low == tenured_area->low) {
    return tenured_area->static_dnodes;
  }
  return 0;
}







bitvector GCmarkbits = NULL, GCdynamic_markbits = NULL;
LispObj GCarealow, GCareadynamiclow;
natural GCndnodes_in_area, GCndynamic_dnodes_in_area;
LispObj GCweakvll = (LispObj)NULL;
LispObj GCephemeral_low;
natural GCn_ephemeral_dnodes;


/* Sooner or later, this probably wants to be in assembler */
/* Return false if n is definitely not an ephemeral node, true if
   it might be */
void
mark_root(LispObj n)
{
  int tag_n = fulltag_of(n);
  natural dnode, bits, *bitsp, mask;

  if (!is_node_fulltag(tag_n)) {
    return;
  }

  dnode = gc_area_dnode(n);
  if (dnode >= GCndnodes_in_area) {
    return;
  }
  set_bits_vars(GCmarkbits,dnode,bitsp,bits,mask);
  if (bits & mask) {
    return;
  }
  *bitsp = (bits | mask);

  if (tag_n == fulltag_cons) {
    cons *c = (cons *) ptr_from_lispobj(untag(n));
    rmark(c->car);
    rmark(c->cdr);
    return;
  }
  {
    LispObj *base = (LispObj *) ptr_from_lispobj(untag(n));
    natural
      header = *((natural *) base),
      subtag = header_subtag(header),
      element_count = header_element_count(header),
      total_size_in_bytes,      /* including 4/8-byte header */
      suffix_dnodes;
    tag_n = fulltag_of(header);

#ifdef PPC
#ifdef PPC64
    if ((nodeheader_tag_p(tag_n)) ||
        (tag_n == ivector_class_64_bit)) {
      total_size_in_bytes = 8 + (element_count<<3);
    } else if (tag_n == ivector_class_8_bit) {
      total_size_in_bytes = 8 + element_count;
    } else if (tag_n == ivector_class_32_bit) {
      total_size_in_bytes = 8 + (element_count<<2);
    } else {
      /* ivector_class_other_bit contains 16-bit arrays & bitvector */
      if (subtag == subtag_bit_vector) {
        total_size_in_bytes = 8 + ((element_count+7)>>3);
      } else {
        total_size_in_bytes = 8 + (element_count<<1);
      }
    }
#else
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
#endif
#endif


    suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift) -1;

    if (suffix_dnodes) {
      set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
    }

    if (nodeheader_tag_p(tag_n)) {
      if (subtag == subtag_hash_vector) {
        /* Don't invalidate the cache here.  It should get
           invalidated on the lisp side, if/when we know
           that rehashing is necessary. */
        LispObj flags = ((hash_table_vector_header *) base)->flags;

        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) base)->cache_key = undefined;
          ((hash_table_vector_header *) base)->cache_value = lisp_nil;
        }
        deref(ptr_to_lispobj(base),1) = GCweakvll;
        GCweakvll = n;
        return;
      }

      if (subtag == subtag_pool) {
        deref(ptr_to_lispobj(base), 1) = lisp_nil;
      }
      
      if (subtag == subtag_weak) {
        natural weak_type = (natural) base[2];
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
        deref(ptr_to_lispobj(base),1) = GCweakvll;
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
  natural eph_dnode;

  if (nodeheader_tag_p(tag_n)) {
    return (header_subtag(n) == subtag_hash_vector);
  }
 
  if ((tag_n == fulltag_cons) ||
      (tag_n == fulltag_misc)) {
    eph_dnode = area_dnode(n, GCephemeral_low);
    if (eph_dnode < GCn_ephemeral_dnodes) {
      mark_root(n);             /* May or may not mark it */
      return true;              /* but return true 'cause it's an ephemeral node */
    }
  }
  return false;                 /* Not a heap pointer or not ephemeral */
}
  
#ifdef PPC
#ifdef PPC64
/* Any register (srr0, the lr or ctr) or stack location that
   we're calling this on should have its low 2 bits clear; it'll
   be tagged as a "primary" object, but the pc/lr/ctr should
   never point to a tagged object or contain a fixnum.
   
   If the "pc" appears to be pointing into a heap-allocated
   code vector that's not yet marked, back up until we find
   the code-vector's prefix (the 32-bit word containing the
   value 'CODE' whic precedes the code-vector's first instruction)
   and mark the entire code-vector.
*/
void
mark_pc_root(LispObj xpc)
{
  if ((xpc & 3) != 0) {
    Bug(NULL, "Bad PC locative!");
  } else {
    natural dnode = gc_area_dnode(xpc);
    if ((dnode < GCndnodes_in_area) &&
        !ref_bit(GCmarkbits,dnode)) {
      LispObj
        *headerP,
        header;
      opcode *program_counter;

      for(program_counter=(opcode *)ptr_from_lispobj(xpc & ~4);
          dnode < GCndnodes_in_area;
          program_counter-=2, --dnode) {
        if (*program_counter == PPC64_CODE_VECTOR_PREFIX) {
          headerP = ((LispObj *)program_counter)-1;
          header = *headerP;
          set_n_bits(GCmarkbits, dnode, (2+header_element_count(header))>>1);
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
#else /* PPC64 */
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
    natural dnode = gc_area_dnode(pc);
    if ((dnode < GCndnodes_in_area) &&
        !ref_bit(GCmarkbits,dnode)) {
      LispObj
        *headerP,
        header;

      for(headerP = (LispObj*)ptr_from_lispobj(untag(pc));
          dnode < GCndnodes_in_area;
          headerP-=2, --dnode) {
        header = *headerP;

        if ((header & code_header_mask) == subtag_code_vector) {
          set_n_bits(GCmarkbits, dnode, (2+header_element_count(header))>>1);
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
#endif /* PPC64 */
#endif /* PPC */

#ifdef PPC
#ifdef PPC64
#define RMARK_PREV_ROOT fulltag_imm_3
#define RMARK_PREV_CAR fulltag_misc
#else
#define RMARK_PREV_ROOT fulltag_imm
#define RMARK_PREV_CAR fulltag_nil
#endif
#endif


natural
GCstack_limit = 0;


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
  natural dnode, bits, *bitsp, mask;

  if (!is_node_fulltag(tag_n)) {
    return;
  }

  dnode = gc_area_dnode(n);
  if (dnode >= GCndnodes_in_area) {
    return;
  }
  set_bits_vars(markbits,dnode,bitsp,bits,mask);
  if (bits & mask) {
    return;
  }
  *bitsp = (bits | mask);

  if (current_stack_pointer() > GCstack_limit) {
    if (tag_n == fulltag_cons) {
      rmark(deref(n,1));
      rmark(deref(n,0));
    } else {
      LispObj *base = (LispObj *) ptr_from_lispobj(untag(n));
      natural
        header = *((natural *) base),
        subtag = header_subtag(header),
        element_count = header_element_count(header),
        total_size_in_bytes,
        suffix_dnodes;
      tag_n = fulltag_of(header);
#ifdef PPC
#ifdef PPC64
      if ((nodeheader_tag_p(tag_n)) ||
          (tag_n == ivector_class_64_bit)) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else if (tag_n == ivector_class_8_bit) {
        total_size_in_bytes = 8 + element_count;
      } else if (tag_n == ivector_class_32_bit) {
        total_size_in_bytes = 8 + (element_count<<2);
      } else {
        /* ivector_class_other_bit contains 16-bit arrays & bitvector */
        if (subtag == subtag_bit_vector) {
          total_size_in_bytes = 8 + ((element_count+7)>>3);
        } else {
          total_size_in_bytes = 8 + (element_count<<1);
        }
      }
#else
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
#endif
#endif

      suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift)-1;

      if (suffix_dnodes) {
        set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
      }

      if (!nodeheader_tag_p(tag_n)) return;

      if (subtag == subtag_hash_vector) {
        /* Splice onto weakvll, then return */
        /* In general, there's no reason to invalidate the cached
           key/value pair here.  However, if the hash table's weak,
           we don't want to retain an otherwise unreferenced key
           or value simply because they're referenced from the
           cache.  Clear the cached entries iff the hash table's
           weak in some sense.
        */
        LispObj flags = ((hash_table_vector_header *) base)->flags;

        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) base)->cache_key = undefined;
          ((hash_table_vector_header *) base)->cache_value = lisp_nil;
        }
        deref(ptr_to_lispobj(base),1) = GCweakvll;
        GCweakvll = n;
        return;
      }

      if (subtag == subtag_pool) {
        deref(n, 1) = lisp_nil;
      }

      if (subtag == subtag_weak) {
        natural weak_type = (natural) base[2];
        if (weak_type >> population_termination_bit)
          element_count -= 2;
        else
          element_count -= 1;
      }
      while (element_count) {
        rmark(deref(n,element_count));
        element_count--;
      }

      if (subtag == subtag_weak) {
        deref(n, 1) = GCweakvll;
        GCweakvll = n;
      }

    }
  } else {
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

    case RMARK_PREV_ROOT:
      return;

    case fulltag_cons:
      goto ClimbCdr;

    case RMARK_PREV_CAR:
      goto ClimbCar;

      /* default: abort() */
    }

  DescendCons:
    prev = this;
    this = next;

  MarkCons:
    next = deref(this,1);
    this += node_size;
    tag_n = fulltag_of(next);
    if (!is_node_fulltag(tag_n)) goto MarkCdr;
    dnode = gc_area_dnode(next);
    if (dnode >= GCndnodes_in_area) goto MarkCdr;
    set_bits_vars(markbits,dnode,bitsp,bits,mask);
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
    this -= node_size;
    tag_n = fulltag_of(next);
    if (!is_node_fulltag(tag_n)) goto Climb;
    dnode = gc_area_dnode(next);
    if (dnode >= GCndnodes_in_area) goto Climb;
    set_bits_vars(markbits,dnode,bitsp,bits,mask);
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
      LispObj *base = (LispObj *) ptr_from_lispobj(untag(this));
      natural
        header = *((natural *) base),
        subtag = header_subtag(header),
        element_count = header_element_count(header),
        total_size_in_bytes,
        suffix_dnodes;

      tag_n = fulltag_of(header);

#ifdef PPC
#ifdef PPC64
      if ((nodeheader_tag_p(tag_n)) ||
          (tag_n == ivector_class_64_bit)) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else if (tag_n == ivector_class_8_bit) {
        total_size_in_bytes = 8 + element_count;
      } else if (tag_n == ivector_class_32_bit) {
        total_size_in_bytes = 8 + (element_count<<2);
      } else {
        /* ivector_class_other_bit contains 16-bit arrays & bitvector */
        if (subtag == subtag_bit_vector) {
          total_size_in_bytes = 8 + ((element_count+7)>>3);
        } else {
          total_size_in_bytes = 8 + (element_count<<1);
        }
      }
#else
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
#endif
#endif

      suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift)-1;

      if (suffix_dnodes) {
        set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
      }

      if (!nodeheader_tag_p(tag_n)) goto Climb;

      if (subtag == subtag_hash_vector) {
        /* Splice onto weakvll, then climb */
        LispObj flags = ((hash_table_vector_header *) base)->flags;

        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) base)->cache_key = undefined;
          ((hash_table_vector_header *) base)->cache_value = lisp_nil;
        }

        deref(ptr_to_lispobj(base),1) = GCweakvll;
        GCweakvll = this;
        goto Climb;
      }

      if (subtag == subtag_pool) {
        deref(this, 1) = lisp_nil;
      }

      if (subtag == subtag_weak) {
        natural weak_type = (natural) base[2];
        if (weak_type >> population_termination_bit)
          element_count -= 2;
        else
          element_count -= 1;
      }

      this = untag(this) + ((element_count+1) << node_shift);
      goto MarkVectorLoop;
    }

  ClimbVector:
    prev = *((LispObj *) ptr_from_lispobj(this));
    *((LispObj *) ptr_from_lispobj(this)) = next;

  MarkVectorLoop:
    this -= node_size;
    next = *((LispObj *) ptr_from_lispobj(this));
    tag_n = fulltag_of(next);
    if (nodeheader_tag_p(tag_n)) goto MarkVectorDone;
    if (!is_node_fulltag(tag_n)) goto MarkVectorLoop;
    dnode = gc_area_dnode(next);
    if (dnode >= GCndnodes_in_area) goto MarkVectorLoop;
    set_bits_vars(markbits,dnode,bitsp,bits,mask);
    if (bits & mask) goto MarkVectorLoop;
    *bitsp = (bits | mask);
    *(ptr_from_lispobj(this)) = prev;
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

LispObj *
skip_over_ivector(natural start, LispObj header)
{
  natural 
    element_count = header_element_count(header),
    subtag = header_subtag(header),
    nbytes;

#ifdef PPC64
  switch (fulltag_of(header)) {
  case ivector_class_64_bit:
    nbytes = element_count << 3;
    break;
  case ivector_class_32_bit:
    nbytes = element_count << 2;
    break;
  case ivector_class_8_bit:
    nbytes = element_count;
    break;
  case ivector_class_other_bit:
  default:
    if (subtag == subtag_bit_vector) {
      nbytes = (element_count+7)>>3;
    } else {
      nbytes = element_count << 1;
    }
  }
  return ptr_from_lispobj(start+(~15 & (nbytes + 8 + 15)));
#else
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
  return ptr_from_lispobj(start+(~7 & (nbytes + 4 + 7)));
#endif



}


void
check_refmap_consistency(LispObj *start, LispObj *end, bitvector refbits)
{
  LispObj x1, *base = start;
  int tag;
  natural ref_dnode, node_dnode;
  Boolean intergen_ref;

  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      start = skip_over_ivector(ptr_to_lispobj(start), x1);
    } else {
      intergen_ref = false;
      if ((tag == fulltag_misc) || (tag == fulltag_cons)) {        
        node_dnode = gc_area_dnode(x1);
        if (node_dnode < GCndnodes_in_area) {
          intergen_ref = true;
        }
      }
      if (intergen_ref == false) {        
        x1 = start[1];
        tag = fulltag_of(x1);
        if ((tag == fulltag_misc) || (tag == fulltag_cons)) {
          node_dnode = gc_area_dnode(x1);
          if (node_dnode < GCndnodes_in_area) {
            intergen_ref = true;
          }
        }
      }
      if (intergen_ref) {
        ref_dnode = area_dnode(start, base);
        if (!ref_bit(refbits, ref_dnode)) {
          Bug(NULL, "Missing memoization in doublenode at 0x%08X", start);
          set_bit(refbits, ref_dnode);
        }
      }
      start += 2;
    }
  }
}



void
mark_memoized_area(area *a, natural num_memo_dnodes)
{
  bitvector refbits = a->refbits;
  LispObj *p = (LispObj *) a->low, x1, x2;
  natural inbits, outbits, bits, bitidx, *bitsp, nextbit, diff, memo_dnode = 0;
  Boolean keep_x1, keep_x2;

  if (GCDebug) {
    check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
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
    We need to ensure that there are no bits set at or beyond
    "num_memo_dnodes" in the bitvector.  (This can happen as the EGC
    tenures/untenures things.)  We find bits by grabbing a fullword at
    a time and doing a cntlzw instruction; and don't want to have to
    check for (< memo_dnode num_memo_dnodes) in the loop.
    */

  {
    natural 
      bits_in_last_word = (num_memo_dnodes & bitmap_shift_count_mask),
      index_of_last_word = (num_memo_dnodes >> bitmap_shift);

    if (bits_in_last_word != 0) {
      natural mask = ~((1L<<(nbits_in_word-bits_in_last_word))-1L);
      refbits[index_of_last_word] &= mask;
    }
  }
        
  set_bitidx_vars(refbits, 0, bitsp, bits, bitidx);
  inbits = outbits = bits;
  while (memo_dnode < num_memo_dnodes) {
    if (bits == 0) {
      int remain = nbits_in_word - bitidx;
      memo_dnode += remain;
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
        memo_dnode += diff;
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
      memo_dnode++;
      bitidx++;
    }
  }
  if (GCDebug) {
    p = (LispObj *) a->low;
    check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
  }
}



void
mark_simple_area_range(LispObj *start, LispObj *end)
{
  LispObj x1, *base;
  int tag;

  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      start = (LispObj *)ptr_from_lispobj(skip_over_ivector(ptr_to_lispobj(start), x1));
    } else if (!nodeheader_tag_p(tag)) {
      ++start;
      mark_root(x1);
      mark_root(*start++);
    } else {
      int subtag = header_subtag(x1);
      natural element_count = header_element_count(x1);
      natural size = (element_count+1 + 1) & ~1;

      if (subtag == subtag_hash_vector) {
        LispObj flags = ((hash_table_vector_header *) start)->flags;

        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) start)->cache_key = undefined;
          ((hash_table_vector_header *) start)->cache_value = lisp_nil;
        }

        start[1] = GCweakvll;
        GCweakvll = (LispObj) (((natural) start) + fulltag_misc);
      } else {

        if (subtag == subtag_pool) {
          start[1] = lisp_nil;
        }

        if (subtag == subtag_weak) {
          natural weak_type = (natural) start[2];
          if (weak_type >> population_termination_bit)
            element_count -= 2;
          else
            element_count -= 1; 
          start[1] = GCweakvll;
          GCweakvll = (LispObj) (((natural) start) + fulltag_misc);    
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
    next = (LispObj *) ptr_from_lispobj(*current);
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

#if 0
  fprintf(stderr, "mark VSP range: 0x%lx:0x%lx\n", start, end);
#endif
  if (((natural)start) & (sizeof(natural))) {
    /* Odd number of words.  Mark the first (can't be a header) */
    mark_root(*start);
    ++start;
  }
  mark_simple_area_range(start, end);
}

#ifdef PPC
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
      next = (BytePtr) (((natural)next) & ~3);
    }
  }
}
#endif

void
reapweakv(LispObj weakv)
{
  /*
    element 2 of the weak vector should be tagged as a cons: if it
    isn't, just mark it as a root.  if it is, cdr through it until a
    "marked" cons is encountered.  If the car of any unmarked cons is
    marked, mark the cons which contains it; otherwise, splice the
    cons out of the list.  N.B. : elements 0 and 1 are already marked
    (or are immediate, etc.)
  */
  LispObj *prev = ((LispObj *) ptr_from_lispobj(untag(weakv))+(1+2)), cell = *prev;
  LispObj termination_list = lisp_nil;
  natural weak_type = (natural) deref(weakv,2);
  Boolean alistp = ((weak_type & population_type_mask) == population_weak_alist),
    terminatablep = ((weak_type >> population_termination_bit) != 0);
  Boolean done = false;
  cons *rawcons;
  natural dnode, car_dnode;
  bitvector markbits = GCmarkbits;

  if (terminatablep) {
    termination_list = deref(weakv,1+3);
  }

  if (fulltag_of(cell) != fulltag_cons) {
    mark_root(cell);
  } else if (alistp) {
    /* weak alist */
    while (! done) {
      dnode = gc_area_dnode(cell);
      if ((dnode >= GCndnodes_in_area) ||
          (ref_bit(markbits, dnode))) {
        done = true;
      } else {
        /* Cons cell is unmarked. */
        LispObj alist_cell, thecar;
        unsigned cell_tag;

        rawcons = (cons *) ptr_from_lispobj(untag(cell));
        alist_cell = rawcons->car;
        cell_tag = fulltag_of(alist_cell);

        if ((cell_tag == fulltag_cons) &&
            ((car_dnode = gc_area_dnode(alist_cell)) < GCndnodes_in_area) &&
            (! ref_bit(markbits, car_dnode)) &&
            (is_node_fulltag(fulltag_of(thecar = car(alist_cell)))) &&
            ((car_dnode = gc_area_dnode(thecar)) < GCndnodes_in_area) &&
            (! ref_bit(markbits, car_dnode))) {
          *prev = rawcons->cdr;
          if (terminatablep) {
            rawcons->cdr = termination_list;
            termination_list = cell;
          }
        } else {
          set_bit(markbits, dnode);
          prev = (LispObj *)(&(rawcons->cdr));
          mark_root(alist_cell);
        }
        cell = *prev;
      }
    }
  } else {
    /* weak list */
    while (! done) {
      dnode = gc_area_dnode(cell);
      if ((dnode >= GCndnodes_in_area) ||
          (ref_bit(markbits, dnode))) {
        done = true;
      } else {
        /* Cons cell is unmarked. */
        LispObj thecar;
        unsigned cartag;

        rawcons = (cons *) ptr_from_lispobj(untag(cell));
        thecar = rawcons->car;
        cartag = fulltag_of(thecar);

        if (is_node_fulltag(cartag) &&
            ((car_dnode = gc_area_dnode(thecar)) < GCndnodes_in_area) &&
            (! ref_bit(markbits, car_dnode))) {
          *prev = rawcons->cdr;
          if (terminatablep) {
            rawcons->cdr = termination_list;
            termination_list = cell;
          }
        } else {
          set_bit(markbits, dnode);
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
  hash_table_vector_header
    *hashp = (hash_table_vector_header *) ptr_from_lispobj(untag(hashv));
  natural
    dnode,
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
      dnode = gc_area_dnode(weakelement);
      if ((dnode < GCndnodes_in_area) && 
          ! ref_bit(markbits, dnode)) {
        pairp[0] = slot_unbound;
        pairp[1] = lisp_nil;
        hashp->weak_deletions_count += (1<<fixnumshift);
      }
    }
    pairp += 2;
  }
}    
    


Boolean
mark_weak_hash_vector(hash_table_vector_header *hashp, natural elements)
{
  natural flags = hashp->flags, key_dnode, val_dnode;
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
    mark_root(deref(ptr_to_lispobj(hashp),i));
  }

  elements -= skip;

  for (i = 0; i<elements; i+=2, pairp+=2) {
    key = pairp[0];
    val = pairp[1];
    key_marked = val_marked = true;
    key_tag = fulltag_of(key);
    val_tag = fulltag_of(val);
    if (is_node_fulltag(key_tag)) {
      key_dnode = gc_area_dnode(key);
      if ((key_dnode < GCndnodes_in_area) &&
          ! ref_bit(GCmarkbits,key_dnode)) {
        key_marked = false;
      }
    }
    if (is_node_fulltag(val_tag)) {
      val_dnode = gc_area_dnode(val);
      if ((val_dnode < GCndnodes_in_area) &&
          ! ref_bit(GCmarkbits,val_dnode)) {
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
  natural
    elements = header_element_count(header_of(weak_alist)),
    dnode;
  int pair_tag;
  Boolean marked_new = false;
  LispObj alist, pair, key, value;
  bitvector markbits = GCmarkbits;

  if (weak_type >> population_termination_bit) {
    elements -= 1;
  }
  for(alist = deref(weak_alist, elements);
      (fulltag_of(alist) == fulltag_cons) &&
      ((dnode = gc_area_dnode(alist)) < GCndnodes_in_area) &&
      (! ref_bit(markbits,dnode));
      alist = cdr(alist)) {
    pair = car(alist);
    pair_tag = fulltag_of(pair);
    if ((is_node_fulltag(pair_tag)) &&
        ((dnode = gc_area_dnode(pair_tag)) < GCndnodes_in_area) &&
        (! ref_bit(markbits,dnode))) {
      if (pair_tag == fulltag_cons) {
        key = car(pair);
        if ((! is_node_fulltag(fulltag_of(key))) ||
            ((dnode = gc_area_dnode(key)) >= GCndnodes_in_area) ||
            ref_bit(markbits,dnode)) {
          /* key is marked, mark value if necessary */
          value = cdr(pair);
          if (is_node_fulltag(fulltag_of(value)) &&
              ((dnode = gc_area_dnode(value)) < GCndnodes_in_area) &&
              (! ref_bit(markbits,dnode))) {
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
        natural weak_type = deref(this,2);
        deref(this,1) = pending;
        pending = this;
        if ((weak_type & population_type_mask) == population_weak_alist) {
          if (mark_weak_alist(this, weak_type)) {
            marked_new = true;
          }
        }
      } else if (subtag == subtag_hash_vector) {
        natural elements = header_element_count(header), i;

        hashp = (hash_table_vector_header *) ptr_from_lispobj(untag(this));
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
  natural *regs = (natural *) xpGPRvector(xp);

#ifdef PPC
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
  mark_pc_root(ptr_to_lispobj(xpPC(xp)));
  mark_pc_root(ptr_to_lispobj(xpLR(xp)));
  mark_pc_root(ptr_to_lispobj(xpCTR(xp)));
#endif /* PPC */

}
void
mark_tcr_tlb(TCR *tcr)
{
  natural n = tcr->tlb_limit;
  LispObj 
    *start = tcr->tlb_pointer,
    *end = (LispObj *) ((BytePtr)start+n),
    node;

  while (start < end) {
    node = *start;
    if (node != no_thread_local_binding_marker) {
      mark_root(node);
    }
    start++;
  }
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
      

void *postGCptrs = NULL;

void
postGCfree(void *p)
{
  *(void **)p = postGCptrs;
  postGCptrs = p;
}

void
freeGCptrs()
{
  void *p, *next;

  for (p = postGCptrs; p; p = next) {
    next = *((void **)p);
    free(p);
  }
  postGCptrs = NULL;
}

void
reap_gcable_ptrs()
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next, ptr;
  xmacptr_flag flag;
  natural dnode;
  xmacptr *x;

  while((next = *prev) != (LispObj)NULL) {
    dnode = gc_area_dnode(next);
    x = (xmacptr *) ptr_from_lispobj(untag(next));

    if ((dnode >= GCndnodes_in_area) ||
        (ref_bit(GCmarkbits,dnode))) {
      prev = &(x->link);
    } else {
      *prev = x->link;
      flag = (xmacptr_flag)(x->flags);
      ptr = x->address;

      if (ptr) {
        switch (flag) {
        case xmacptr_flag_recursive_lock:
	  destroy_recursive_lock((RECURSIVE_LOCK)ptr_from_lispobj(ptr));
          break;

        case xmacptr_flag_ptr:
	  postGCfree((void *)ptr_from_lispobj(ptr));
          break;

        case xmacptr_flag_rwlock:
          rwlock_destroy((rwlock *)ptr_from_lispobj(ptr));
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



#if  WORD_SIZE == 64
unsigned short *_one_bits = NULL;

unsigned short
logcount16(unsigned short n)
{
  unsigned short c=0;
  
  while(n) {
    n = n & (n-1);
    c++;
  }
  return c;
}

void
gc_init()
{
  int i;
  
  _one_bits = malloc(sizeof(unsigned short) * (1<<16));

  for (i = 0; i < (1<<16); i++) {
    _one_bits[i] = dnode_size*logcount16(i);
  }
}

#define one_bits(x) _one_bits[x]

#else
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

void
gc_init()
{
}

#endif

/* A "pagelet" contains 32 doublewords.  The relocation table contains
   a word for each pagelet which defines the lowest address to which
   dnodes on that pagelet will be relocated.

   The relocation address of a given pagelet is the sum of the relocation
   address for the preceding pagelet and the number of bytes occupied by
   marked objects on the preceding pagelet.
*/

LispObj
calculate_relocation()
{
  LispObj *relocptr = GCrelocptr;
  LispObj current = GCareadynamiclow;
  bitvector 
    markbits = GCdynamic_markbits;
  qnode *q = (qnode *) markbits;
  natural npagelets = ((GCndynamic_dnodes_in_area+(nbits_in_word-1))>>bitmap_shift);
  natural thesebits;
  LispObj first = 0;

  do {
    *relocptr++ = current;
    thesebits = *markbits++;
    if (thesebits == ALL_ONES) {
      current += nbits_in_word*dnode_size;
      q += 4; /* sic */
    } else {
      if (!first) {
        first = current;
        while (thesebits & BIT0_MASK) {
          first += dnode_size;
          thesebits += thesebits;
        }
      }
      current += one_bits(*q++);
      current += one_bits(*q++);
      current += one_bits(*q++);
      current += one_bits(*q++);
    }
  } while(--npagelets);
  *relocptr++ = current;
  return first ? first : current;
}

#ifdef PPC64
LispObj
dnode_forwarding_address(natural dnode, int tag_n)
{
  natural pagelet, nbits;
  unsigned int near_bits;
  LispObj new;

  if (GCDebug) {
    if (! ref_bit(GCdynamic_markbits, dnode)) {
      Bug(NULL, "unmarked object being forwarded!\n");
    }
  }

  pagelet = dnode >> bitmap_shift;
  nbits = dnode & bitmap_shift_count_mask;
  near_bits = ((unsigned int *)GCdynamic_markbits)[dnode>>(dnode_shift+1)];

  if (nbits < 32) {
    new = GCrelocptr[pagelet] + tag_n;;
    /* Increment "new" by the count of 1 bits which precede the dnode */
    if (near_bits == 0xffffffff) {
      return (new + (nbits << 4));
    } else {
      near_bits &= (0xffffffff00000000 >> nbits);
      if (nbits > 15) {
        new += one_bits(near_bits & 0xffff);
      }
      return (new + (one_bits(near_bits >> 16))); 
    }
  } else {
    new = GCrelocptr[pagelet+1] + tag_n;
    nbits = 64-nbits;

    if (near_bits == 0xffffffff) {
      return (new - (nbits << 4));
    } else {
      near_bits &= (1<<nbits)-1;
      if (nbits > 15) {
        new -= one_bits(near_bits >> 16);
      }
      return (new -  one_bits(near_bits & 0xffff));
    }
  }
}
#else
LispObj
dnode_forwarding_address(natural dnode, int tag_n)
{
  natural pagelet, nbits;
  unsigned short near_bits;
  LispObj new;

  if (GCDebug) {
    if (! ref_bit(GCdynamic_markbits, dnode)) {
      Bug(NULL, "unmarked object being forwarded!\n");
    }
  }

  pagelet = dnode >> 5;
  nbits = dnode & 0x1f;
  near_bits = ((unsigned short *)GCdynamic_markbits)[dnode>>4];

  if (nbits < 16) {
    new = GCrelocptr[pagelet] + tag_n;;
    /* Increment "new" by the count of 1 bits which precede the dnode */
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
#endif


LispObj
locative_forwarding_address(LispObj obj)
{
  int tag_n = fulltag_of(obj);
  natural dnode;


#ifdef PPC
  /* Locatives can be tagged as conses, "fulltag_misc"
     objects, or as fixnums.  Immediates, headers, and nil
     shouldn't be "forwarded".  Nil never will be, but it
     doesn't hurt to check ... */
#ifdef PPC64
  if ((tag_n & lowtag_mask) != lowtag_primary) {
    return obj;
  }
#else
  if ((1<<tag_n) & ((1<<fulltag_immheader) |
                    (1<<fulltag_nodeheader) |
                    (1<<fulltag_imm) |
                    (1<<fulltag_nil))) {
    return obj;
  }
#endif
#endif

  dnode = gc_dynamic_area_dnode(obj);

  if ((dnode >= GCndynamic_dnodes_in_area) ||
      (obj < GCfirstunmarked)) {
    return obj;
  }

  return dnode_forwarding_address(dnode, tag_n);
}

LispObj
node_forwarding_address(LispObj node)
{
  int tag_n;
  natural dnode = gc_dynamic_area_dnode(node);

  if ((dnode >= GCndynamic_dnodes_in_area) ||
      (node < GCfirstunmarked)) {
    return node;
  }

  tag_n = fulltag_of(node);
  if (!is_node_fulltag(tag_n)) {
    return node;
  }

  return dnode_forwarding_address(dnode, tag_n);
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
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}

void
forward_range(LispObj *range_start, LispObj *range_end)
{
  LispObj *p = range_start, node, new;
  int tag_n;
  natural nwords;
  hash_table_vector_header *hashp;

  while (p < range_end) {
    node = *p;
    tag_n = fulltag_of(node);
    if (immheader_tag_p(tag_n)) {
      p = (LispObj *) skip_over_ivector((natural) p, node);
    } else if (nodeheader_tag_p(tag_n)) {
      nwords = header_element_count(node);
      nwords += (1 - (nwords&1));
      if ((header_subtag(node) == subtag_hash_vector) &&
          ((((hash_table_vector_header *)p)->flags) & nhash_track_keys_mask)) {
        natural skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1;
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
forward_memoized_area(area *a, natural num_memo_dnodes)
{
  bitvector refbits = a->refbits;
  LispObj *p = (LispObj *) a->low, x1, x2, new;
  natural bits, bitidx, *bitsp, nextbit, diff, memo_dnode = 0, hash_dnode_limit = 0;
  int tag_x1;
  hash_table_vector_header *hashp = NULL;
  Boolean header_p;

  if (GCDebug) {
    check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
  }

  /* This is pretty straightforward, but we have to note
     when we move a key in a hash table vector that wants
     us to tell it about that. */

  set_bitidx_vars(refbits, 0, bitsp, bits, bitidx);
  while (memo_dnode < num_memo_dnodes) {
    if (bits == 0) {
      int remain = nbits_in_word - bitidx;
      memo_dnode += remain;
      p += (remain+remain);
      bits = *++bitsp;
      bitidx = 0;
    } else {
      nextbit = count_leading_zeros(bits);
      if ((diff = (nextbit - bitidx)) != 0) {
        memo_dnode += diff;
        bitidx = nextbit;
        p += (diff+diff);
      }
      x1 = p[0];
      x2 = p[1];
      tag_x1 = fulltag_of(x1);
      bits &= ~(BIT0_MASK >> bitidx);
      header_p = (nodeheader_tag_p(tag_x1));

      if (header_p &&
          (header_subtag(x1) == subtag_hash_vector)) {
        hashp = (hash_table_vector_header *) p;
        if (hashp->flags & nhash_track_keys_mask) {
          hash_dnode_limit = memo_dnode + ((header_element_count(x1)+2)>>1);
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
        if (memo_dnode < hash_dnode_limit) {
          hashp->flags |= nhash_key_moved_mask;
          hash_dnode_limit = 0;
          hashp = NULL;
        }
      }
      p++;
      memo_dnode++;
      bitidx++;

    }
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
    next = ptr_from_lispobj(*current);
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

#ifdef DEBUG
  fprintf(stderr,"Forward range 0x%x/0x%x (owner 0x%x)\n",p,q,a->owner);
#endif
  if (((natural)p) & sizeof(natural)) {
    update_noderef(p);
    p++;
  }
  forward_range(p, q);
}

#ifdef PPC
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
    }
  }
}

#endif

void
forward_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp);

#ifdef PPC
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
#endif

}

void
forward_tcr_tlb(TCR *tcr)
{
  natural n = tcr->tlb_limit;
  LispObj 
    *start = tcr->tlb_pointer, 
    *end = (LispObj *) ((BytePtr)start+n),
    node;

  while (start < end) {
    node = *start;
    if (node != no_thread_local_binding_marker) {
      update_noderef(start);
    }
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
    if (xframes->curr == xp) {
      Bug(NULL, "forward xframe twice ???");
    }
    forward_xp(xframes->curr);
  }
}

void
forward_and_resolve_static_references(area *a)
{
  natural 
    nstatic = static_dnodes_for_area(a),
    nstatic_bitmap_words = nstatic >> bitmap_shift;
  if (nstatic != 0) {
    /* exploit the fact that a cons is the same size as a dnode. */
    cons *pagelet_start = (cons *) a->low, *work;
    bitvector markbits = GCmarkbits, 
      usedbits = tenured_area->static_used;
    natural marked, used, used_but_not_marked, ndeleted = 0, i;

    while (nstatic_bitmap_words--) {
      marked = *markbits++;
      used = *usedbits;
      if (marked != used) {
        *usedbits = marked;
      }
      used |= marked;
      used_but_not_marked = used & ~marked;

      while (marked) {
        i = count_leading_zeros(marked);
        marked &= ~(BIT0_MASK >> i);
        work = pagelet_start+i;
        update_noderef(&work->cdr);
        update_noderef(&work->car);
      }

      while (used_but_not_marked) {
        i = count_leading_zeros(used_but_not_marked);
        used_but_not_marked &= ~(BIT0_MASK >> i);
        work = pagelet_start+i;
        if ((work->cdr != undefined) &&
            (work->cdr != slot_unbound)) {
          work->car = slot_unbound;
          work->cdr = slot_unbound;
          ndeleted++;
        }
      }
      usedbits++;
      pagelet_start += nbits_in_word;
    }
    lisp_global(DELETED_STATIC_PAIRS) += box_fixnum(ndeleted);
  }
}


/*
  Compact the dynamic heap (from GCfirstunmarked through its end.)
  Return the doublenode address of the new freeptr.
  */

LispObj
compact_dynamic_heap()
{
  LispObj *src = ptr_from_lispobj(GCfirstunmarked), *dest = src, node, new;
  natural 
    elements, 
    dnode = gc_area_dnode(GCfirstunmarked), 
    node_dnodes = 0, 
    imm_dnodes = 0, 
    bitidx, 
    *bitsp, 
    bits, 
    nextbit, 
    diff;
  int tag;
  bitvector markbits = GCmarkbits;
    /* keep track of whether or not we saw any
       code_vector headers, and only flush cache if so. */
  Boolean GCrelocated_code_vector = false;

  if (dnode < GCndnodes_in_area) {
    lisp_global(FWDNUM) += (1<<fixnum_shift);
  
    set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
    while (dnode < GCndnodes_in_area) {
      if (bits == 0) {
        int remain = nbits_in_word - bitidx;
        dnode += remain;
        src += (remain+remain);
        bits = *++bitsp;
        bitidx = 0;
      } else {
        /* Have a non-zero markbits word; all bits more significant
           than "bitidx" are 0.  Count leading zeros in "bits"
           (there'll be at least "bitidx" of them.)  If there are more
           than "bitidx" leading zeros, bump "dnode", "bitidx", and
           "src" by the difference. */
        nextbit = count_leading_zeros(bits);
        if ((diff = (nextbit - bitidx)) != 0) {
          dnode += diff;
          bitidx = nextbit;
          src += (diff+diff);
        }

        if (GCDebug) {
          if (dest != ptr_from_lispobj(locative_forwarding_address(ptr_to_lispobj(src)))) {
            Bug(NULL, "Out of synch in heap compaction.  Forwarding from 0x%lx to 0x%lx,\n expected to go to 0x%lx\n", 
                src, dest, locative_forwarding_address(ptr_to_lispobj(src)));
          }
        }

        node = *src++;
        tag = fulltag_of(node);
        if (nodeheader_tag_p(tag)) {
          elements = header_element_count(node);
          node_dnodes = (elements+2)>>1;
          dnode += node_dnodes;
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
            while(--node_dnodes) {
              *dest++ = node_forwarding_address(*src++);
              *dest++ = node_forwarding_address(*src++);
            }
          }
          set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
        } else if (immheader_tag_p(tag)) {
          *dest++ = node;
          *dest++ = *src++;
          elements = header_element_count(node);
          tag = header_subtag(node);

#ifdef PPC
#ifdef PPC64
          switch(fulltag_of(tag)) {
          case ivector_class_64_bit:
            imm_dnodes = ((elements+1)+1)>>1;
            break;
          case ivector_class_32_bit:
            if (tag == subtag_code_vector) {
              GCrelocated_code_vector = true;
            }
            imm_dnodes = (((elements+2)+3)>>2);
            break;
          case ivector_class_8_bit:
            imm_dnodes = (((elements+8)+15)>>4);
            break;
          case ivector_class_other_bit:
            if (tag == subtag_bit_vector) {
              imm_dnodes = (((elements+64)+127)>>7);
            } else {
              imm_dnodes = (((elements+4)+7)>>3);
            }
          }
#else
          if (tag <= max_32_bit_ivector_subtag) {
            if (tag == subtag_code_vector) {
              GCrelocated_code_vector = true;
            }
            imm_dnodes = (((elements+1)+1)>>1);
          } else if (tag <= max_8_bit_ivector_subtag) {
            imm_dnodes = (((elements+4)+7)>>3);
          } else if (tag <= max_16_bit_ivector_subtag) {
            imm_dnodes = (((elements+2)+3)>>2);
          } else if (tag == subtag_bit_vector) {
            imm_dnodes = (((elements+32)+63)>>6);
          } else {
            imm_dnodes = elements+1;
          }
#endif
#endif

          dnode += imm_dnodes;
          while (--imm_dnodes) {
            *dest++ = *src++;
            *dest++ = *src++;
          }
          set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
        } else {
          *dest++ = node_forwarding_address(node);
          *dest++ = node_forwarding_address(*src++);
          bits &= ~(BIT0_MASK >> bitidx);
          dnode++;
          bitidx++;
        }
      }
  
    }

    {
      natural nbytes = (natural)ptr_to_lispobj(dest) - (natural)GCfirstunmarked;
      if ((nbytes != 0) && GCrelocated_code_vector) {
        xMakeDataExecutable((LogicalAddress)ptr_from_lispobj(GCfirstunmarked), nbytes);
      }
    }
  }
  return ptr_to_lispobj(dest);
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

#define MARK_RECURSIVELY_USING_STACK 1
#if !MARK_RECURSIVELY_USING_STACK
#warning recursive marker disabled for testing; remember to re-enable it
#endif

void 
gc(TCR *tcr, signed_natural param)
{
  xframe_list *xframes = (tcr->xframe);
  struct timeval start, stop;
  area *a = active_dynamic_area, *to = NULL, *from = NULL, *note = NULL;
  unsigned timeidx = 1;
  xframe_list *x;
  LispObj
    pkg,
    itabvec = 0;
  BytePtr oldfree = a->active;
  TCR *other_tcr;
  natural static_dnodes;
  
#if MARK_RECURSIVELY_USING_STACK
  if ((natural) (tcr->cs_limit) == CS_OVERFLOW_FORCE_LIMIT) {
    GCstack_limit = CS_OVERFLOW_FORCE_LIMIT;
  } else {
    GCstack_limit = (natural)(tcr->cs_limit)+(natural)page_size;
  }
#else
  GCstack_limit = CS_OVERFLOW_FORCE_LIMIT;
#endif

  GCephemeral_low = lisp_global(OLDEST_EPHEMERAL);
  if (GCephemeral_low) {
    GCn_ephemeral_dnodes=area_dnode(oldfree, GCephemeral_low);
  } else {
    GCn_ephemeral_dnodes = 0;
  }
  
  if (GCn_ephemeral_dnodes) {
    GCverbose = ((nrs_GC_EVENT_STATUS_BITS.vcell & egc_verbose_bit) != 0);
  } else {
    GCverbose = ((nrs_GC_EVENT_STATUS_BITS.vcell & gc_verbose_bit) != 0);
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

  if (GCverbose) {
    if (GCephemeral_low) {
      fprintf(stderr,
              "\n\n;;; Starting Ephemeral GC of generation %d",
              (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0); 
    } else {
      fprintf(stderr,"\n\n;;; Starting full GC");
    }
    fprintf(stderr, ",  %ld bytes allocated.\n", area_dnode(oldfree,a->low) << dnode_shift);
  }

  get_time(start);
  lisp_global(IN_GC) = (1<<fixnumshift);
  
  
  GCDebug = ((nrs_GC_EVENT_STATUS_BITS.vcell & gc_integrity_check_bit) != 0);

  if (just_purified_p) {
    just_purified_p = false;
  } else {
    if (GCDebug) {
      check_all_areas();
    }
  }

  if (from) {
    untenure_from_area(from);
  }
  static_dnodes = static_dnodes_for_area(a);
  GCmarkbits = a->markbits;
  GCarealow = ptr_to_lispobj(a->low);
#ifdef DEBUG
  fprintf(stderr, "GC: low = #x%x, high = #x%x\n",a->low,oldfree);
#endif
  GCareadynamiclow = GCarealow+(static_dnodes << dnode_shift);
  GCndnodes_in_area = gc_area_dnode(oldfree);


  if (GCndnodes_in_area) {
    GCndynamic_dnodes_in_area = GCndnodes_in_area-static_dnodes;
    GCdynamic_markbits = 
      GCmarkbits + ((GCndnodes_in_area-GCndynamic_dnodes_in_area)>>bitmap_shift);

    zero_bits(GCmarkbits, GCndnodes_in_area);
    GCweakvll = (LispObj)NULL;


    if (GCn_ephemeral_dnodes == 0) {
      /* For GCTWA, mark the internal package hash table vector of
       *PACKAGE*, but don't mark its contents. */
      {
        LispObj
          itab;
        natural
          dnode, ndnodes;
      
        pkg = nrs_PACKAGE.vcell;
        if ((fulltag_of(pkg) == fulltag_misc) &&
            (header_subtag(header_of(pkg)) == subtag_package)) {
          itab = ((package *)ptr_from_lispobj(untag(pkg)))->itab;
          itabvec = car(itab);
          dnode = gc_area_dnode(itabvec);
          if (dnode < GCndnodes_in_area) {
            ndnodes = (header_element_count(header_of(itabvec))+1) >> 1;
            set_n_bits(GCmarkbits, dnode, ndnodes);
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
#ifdef PPC
          mark_cstack_area(next_area);
#endif
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
      mark_memoized_area(tenured_area, area_dnode(a->low,tenured_area->low));
    }

    other_tcr = tcr;
    do {
      mark_tcr_xframes(other_tcr);
      mark_tcr_tlb(other_tcr);
      other_tcr = other_tcr->next;
    } while (other_tcr != tcr);




    /* Go back through *package*'s internal symbols, marking
       any that aren't worthless.
    */
    
    if (itabvec) {
      natural
        i,
        n = header_element_count(header_of(itabvec));
      LispObj
        sym,
        *raw = 1+((LispObj *)ptr_from_lispobj(untag(itabvec)));

      for (i = 0; i < n; i++) {
        sym = *raw++;
        if (fulltag_of(sym) == fulltag_misc) {
          lispsymbol *rawsym = (lispsymbol *)ptr_from_lispobj(untag(sym));
          natural dnode = gc_area_dnode(sym);
          
          if ((dnode < GCndnodes_in_area) &&
              (!ref_bit(GCmarkbits,dnode))) {
            /* Symbol is in GC area, not marked.
               Mark it if fboundp, boundp, or if
               it has a plist or another home package.
            */
            
            if (FBOUNDP(rawsym) ||
                BOUNDP(rawsym) ||
                (rawsym->flags != 0) || /* SPECIAL, etc. */
                (rawsym->plist != lisp_nil) ||
                ((rawsym->package_predicate != pkg) &&
                 (rawsym->package_predicate != lisp_nil))) {
              mark_root(sym);
            }
          }
        }
      }
    }

    (void)markhtabvs();

    if (itabvec) {
      natural
        i,
        n = header_element_count(header_of(itabvec));
      LispObj
        sym,
        *raw = 1+((LispObj *)ptr_from_lispobj(untag(itabvec)));

      for (i = 0; i < n; i++, raw++) {
        sym = *raw;
        if (fulltag_of(sym) == fulltag_misc) {
          lispsymbol *rawsym = (lispsymbol *)ptr_from_lispobj(untag(sym));
          natural dnode = gc_area_dnode(sym);

          if ((dnode < GCndnodes_in_area) &&
              (!ref_bit(GCmarkbits,dnode))) {
            *raw = unbound_marker;
          }
        }
      }
    }
  
    reap_gcable_ptrs();

    GCrelocptr = global_reloctab;
    GCfirstunmarked = calculate_relocation();

    forward_range((LispObj *) ptr_from_lispobj(GCareadynamiclow), (LispObj *) ptr_from_lispobj(GCfirstunmarked));

    other_tcr = tcr;
    do {
      forward_tcr_xframes(other_tcr);
      forward_tcr_tlb(other_tcr);
      other_tcr = other_tcr->next;
    } while (other_tcr != tcr);

  
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
#ifdef PPC
          forward_cstack_area(next_area);
#endif
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
      forward_memoized_area(tenured_area, area_dnode(a->low, tenured_area->low));
    } else {
      /* Full GC, need to process static space */
      forward_and_resolve_static_references(a);
    }
  
    a->active = (BytePtr) ptr_from_lispobj(compact_dynamic_heap());
#ifdef DEBUG
    fprintf(stderr, "GC done, new active ptr = #x%x\n",a->active);
#endif

    if (to) {
      tenure_to_area(to);
    }

    zero_memory_range(a->active, oldfree);

    resize_dynamic_heap(a->active,
                        (GCephemeral_low == 0) ? lisp_heap_gc_threshold : 0);

    /*
      If the EGC is enabled: If there's no room for the youngest
      generation, untenure everything.  If this was a full GC and
      there's now room for the youngest generation, tenure everything.
    */
    if (a->older != NULL) {
      natural nfree = (a->high - a->active);


      if (nfree < a->threshold) {
        untenure_from_area(tenured_area);
      } else {
        if (GCephemeral_low == 0) {
          tenure_to_area(tenured_area);
        }
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

  
  lisp_global(IN_GC) = 0;

  nrs_GC_EVENT_STATUS_BITS.vcell |= gc_postgc_pending;
  get_time(stop);

  {
    lispsymbol * total_gc_microseconds = (lispsymbol *) &(nrs_TOTAL_GC_MICROSECONDS);
    lispsymbol * total_bytes_freed = (lispsymbol *) &(nrs_TOTAL_BYTES_FREED);
    LispObj val;
    struct timeval *timeinfo, elapsed;

    val = total_gc_microseconds->vcell;
    if ((fulltag_of(val) == fulltag_misc) &&
        (header_subtag(header_of(val)) == subtag_macptr)) {
      timersub(&stop, &start, &elapsed);
      timeinfo = (struct timeval *) ptr_from_lispobj(((macptr *) ptr_from_lispobj(untag(val)))->address);
      timeradd(timeinfo,  &elapsed, timeinfo);
      timeradd(timeinfo+timeidx,  &elapsed, timeinfo+timeidx);
    }

    val = total_bytes_freed->vcell;
    if ((fulltag_of(val) == fulltag_misc) &&
        (header_subtag(header_of(val)) == subtag_macptr)) {
      long long justfreed = oldfree - a->active;
      *( (long long *) ptr_from_lispobj(((macptr *) ptr_from_lispobj(untag(val)))->address)) += justfreed;
      if (GCverbose) {
        if (justfreed <= heap_segment_size) {
          justfreed = 0;
        }
        if (note == tenured_area) {
          fprintf(stderr,";;; Finished full GC.  Freed %lld bytes in %d.%06d s\n\n", justfreed, elapsed.tv_sec, elapsed.tv_usec);
        } else {
          fprintf(stderr,";;; Finished Ephemeral GC of generation %d.  Freed %lld bytes in %d.%06d s\n\n", 
                  (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0,
                  justfreed, 
                  elapsed.tv_sec, elapsed.tv_usec);
        }
      }
    }
  }
#ifdef DEBUG
  fprintf(stderr, "Finished GC of %s\n", 
          (note == tenured_area) ?
          "tenured area" : 
          ((from == g2_area) ? "generation 2" : 
           ((from == g1_area) ? "generation 1" : "generation 0")));
#endif
}

      
    
  /*
    Total the (physical) byte sizes of all ivectors in the indicated memory range
  */

  natural
    unboxed_bytes_in_range(LispObj *start, LispObj *end)
  {
    natural total=0, elements, tag, subtag, bytes;
    LispObj header;

    while (start < end) {
      header = *start;
      tag = fulltag_of(header);
    
      if ((nodeheader_tag_p(tag)) ||
          (immheader_tag_p(tag))) {
        elements = header_element_count(header);
        if (nodeheader_tag_p(tag)) {
          start += ((elements+2) & ~1);
        } else {
          subtag = header_subtag(header);

#ifdef PPC
#ifdef PPC64
          switch(fulltag_of(header)) {
          case ivector_class_64_bit:
            bytes = 8 + (elements<<3);
            break;
          case ivector_class_32_bit:
            bytes = 8 + (elements<<2);
            break;
          case ivector_class_8_bit:
            bytes = 8 + elements;
            break;
          case ivector_class_other_bit:
          default:
            if (subtag == subtag_bit_vector) {
              bytes = 8 + ((elements+7)>>3);
            } else {
              bytes = 8 + (elements<<1);
            }
          }
#else
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
#endif
#endif

          bytes = (bytes+dnode_size-1) & ~(dnode_size-1);
          total += bytes;
          start += (bytes >> node_shift);
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
    purify_displaced_object(LispObj obj, area *dest, natural disp)
  {
    BytePtr 
      free = dest->active,
      *old = (BytePtr *) ptr_from_lispobj(untag(obj));
    LispObj 
      header = header_of(obj), 
      new;
    natural 
      subtag = header_subtag(header), 
      element_count = header_element_count(header),
      physbytes;

    switch(subtag) {
    case subtag_simple_base_string:
      physbytes = node_size + (element_count << 2);
      break;

    case subtag_code_vector:
      physbytes = node_size + (element_count << 2);
      break;

    default:
      Bug(NULL, "Can't purify object at 0x%08x", obj);
      return obj;
    }
    physbytes = (physbytes+(dnode_size-1))&~(dnode_size-1);
    dest->active += physbytes;

    new = ptr_to_lispobj(free)+disp;

    memcpy(free, (BytePtr)old, physbytes);
    /* Leave a trail of breadcrumbs.  Or maybe just one breadcrumb. */
    /* Actually, it's best to always leave a trail, for two reasons.
       a) We may be walking the same heap that we're leaving forwaring
       pointers in, so we don't want garbage that we leave behind to
       look like a header.
       b) We'd like to be able to forward code-vector locatives, and
       it's easiest to do so if we leave a {forward_marker, dnode_locative}
       pair at every doubleword in the old vector.
    */
    while(physbytes) {
      *old++ = (BytePtr) forward_marker;
      *old++ = (BytePtr) free;
      free += dnode_size;
      physbytes -= dnode_size;
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
    natural tag = fulltag_of(obj), header_tag, header_subtag;

    if ((tag == fulltag_misc) &&
        (((BytePtr)ptr_from_lispobj(obj)) > low) &&
        (((BytePtr)ptr_from_lispobj(obj)) < high)) {
      header = deref(obj, 0);
      if (header == forward_marker) { /* already copied */
        *ref = (untag(deref(obj,1)) + tag);
      } else {
        header_tag = fulltag_of(header);
        if (immheader_tag_p(header_tag)) {
          header_subtag = header_subtag(header);
          if (((header_subtag == subtag_code_vector) && (what_to_copy & COPY_CODE)) ||
              ((what_to_copy & COPY_STRINGS) && 
               ((header_subtag == subtag_simple_base_string)))) {
            *ref = purify_object(obj, dest);
          }
        }
      }
    }
  }

  void
    purify_locref(LispObj *locaddr, BytePtr low, BytePtr high, area *to, int what)
  {
#ifdef PPC
    LispObj
      loc = *locaddr,
      *headerP;
    opcode
      *p,
      insn;
    natural
      tag = fulltag_of(loc);

    if (((BytePtr)ptr_from_lispobj(loc) > low) &&

        ((BytePtr)ptr_from_lispobj(loc) < high)) {

      headerP = (LispObj *)ptr_from_lispobj(untag(loc));
      switch (tag) {
      case fulltag_even_fixnum:
      case fulltag_odd_fixnum:
#ifdef PPC64
      case fulltag_cons:
      case fulltag_misc:
#endif
        if (*headerP == forward_marker) {
          *locaddr = (headerP[1]+tag);
        } else {
          /* Grovel backwards until the header's found; copy
             the code vector to to space, then treat it as if it 
             hasn't already been copied. */
          p = (opcode *)headerP;
          do {
            p -= 2;
            tag += 8;
            insn = *p;
#ifdef PPC64
          } while (insn != PPC64_CODE_VECTOR_PREFIX);
          headerP = ((LispObj*)p)-1;
          *locaddr = purify_displaced_object(((LispObj)headerP), to, tag);
#else
        } while ((insn & code_header_mask) != subtag_code_vector);
        *locaddr = purify_displaced_object(ptr_to_lispobj(p), to, tag);
#endif
      }
      break;

#ifndef PPC64
    case fulltag_misc:
      copy_ivector_reference(locaddr, low, high, to, what);
      break;
#endif
    }
  }
#endif
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
      if (immheader_tag_p(tag)) {
        start = (LispObj *)skip_over_ivector((natural)start, header);
      } else {
        if (!nodeheader_tag_p(tag)) {
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
    next = (LispObj *) ptr_from_lispobj(*current);
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

  if (((natural)p) & sizeof(natural)) {
    copy_ivector_reference(p, low, high, to, what);
    p++;
  }
  purify_range(p, q, low, high, to, what);
}

#ifdef PPC
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
      /* Clear low bits of "next", just in case */
      next = (BytePtr) (((natural)next) & ~(sizeof(natural)-1));
    }
  }
}
#endif

void
purify_xp(ExceptionInformation *xp, BytePtr low, BytePtr high, area *to, int what)
{
  unsigned long *regs = (unsigned long *) xpGPRvector(xp);

#ifdef PPC
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
#endif

}

void
purify_tcr_tlb(TCR *tcr, BytePtr low, BytePtr high, area *to, int what)
{
  natural n = tcr->tlb_limit;
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
#ifdef PPC
      purify_cstack_area(next_area, low, high, target, what);
#endif
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
purify(TCR *tcr, signed_natural param)
{
  extern area *extend_readonly_area(unsigned);
  area 
    *a = active_dynamic_area,
    *new_pure_area;

  TCR  *other_tcr;
  natural max_pure_size;
  OSErr err;
  BytePtr new_pure_start;


  max_pure_size = unboxed_bytes_in_range((LispObj *)(a->low + (static_dnodes_for_area(a) << dnode_shift)), 
                                         (LispObj *) a->active);
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
      natural elements, i;

      while (fulltag_of(pkg_list) == fulltag_cons) {
        c = (cons *) ptr_from_lispobj(untag(pkg_list));
        p = (package *) ptr_from_lispobj(untag(c->car));
        pkg_list = c->cdr;
        c = (cons *) ptr_from_lispobj(untag(p->itab));
        htab = c->car;
        elements = header_element_count(header_of(htab));
        for (i = 1; i<= elements; i++) {
          obj = deref(htab,i);
          if (fulltag_of(obj) == fulltag_misc) {
            rawsym = (lispsymbol *) ptr_from_lispobj(untag(obj));
            copy_ivector_reference(&(rawsym->pname), a->low, a->active, new_pure_area, COPY_STRINGS);
          }
        }
        c = (cons *) ptr_from_lispobj(untag(p->etab));
        htab = c->car;
        elements = header_element_count(header_of(htab));
        for (i = 1; i<= elements; i++) {
          obj = deref(htab,i);
          if (fulltag_of(obj) == fulltag_misc) {
            rawsym = (lispsymbol *) ptr_from_lispobj(untag(obj));
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


    {
      natural puresize = (unsigned) (new_pure_area->active-new_pure_start);
      if (puresize != 0) {
        xMakeDataExecutable(new_pure_start, puresize);
  
      }
    }
    ProtectMemory(new_pure_area->low,
		  align_to_power_of_2(new_pure_area->active-new_pure_area->low,
				      log2_page_size));
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
#ifdef PPC64
  case fulltag_cons:
#endif
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
  

#ifdef PPC
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
      /* Clear low bits of "next", just in case */
      next = (BytePtr) (((natural)next) & ~(sizeof(natural)-1));
    }
  }
}
#endif

void
impurify_xp(ExceptionInformation *xp, LispObj low, LispObj high, int delta)
{
  natural *regs = (natural *) xpGPRvector(xp);

#ifdef PPC
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
#endif

}


void
impurify_range(LispObj *start, LispObj *end, LispObj low, LispObj high, int delta)
{
  LispObj header;
  unsigned tag;

  while (start < end) {
    header = *start;
    tag = fulltag_of(header);
    if (immheader_tag_p(tag)) {
      start = (LispObj *)skip_over_ivector((natural)start, header);
    } else {
      if (!nodeheader_tag_p(tag)) {
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
    next = (LispObj *) ptr_from_lispobj(*current);
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

  if (((natural)p) & sizeof(natural)) {
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
#ifdef PPC
      impurify_cstack_area(next_area, low, high, delta);
#endif
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
impurify(TCR *tcr, signed_natural param)
{
  area *r = find_readonly_area();

  if (r) {
    area *a = active_dynamic_area;
    BytePtr ro_base = r->low, ro_limit = r->active, oldfree = a->active,
      oldhigh = a->high, newhigh; 
    unsigned n = ro_limit - ro_base;
    int delta = oldfree-ro_base;
    TCR *other_tcr;

    if (n) {
      lisp_global(IN_GC) = 1;
      newhigh = (BytePtr) (align_to_power_of_2(oldfree+n,
                                               log2_heap_segment_size));
      if (newhigh > oldhigh) {
        grow_dynamic_area(newhigh-oldhigh);
      }
      a->active += n;
      bcopy(ro_base, oldfree, n);
      munmap(ro_base, n);
      a->ndnodes = area_dnode(a, a->active);
      pure_space_active = r->active = r->low;
      r->ndnodes = 0;

      impurify_areas(ptr_to_lispobj(ro_base), ptr_to_lispobj(ro_limit), delta);

      other_tcr = tcr;
      do {
        impurify_tcr_xframes(other_tcr, ptr_to_lispobj(ro_base), ptr_to_lispobj(ro_limit), delta);
        impurify_tcr_tlb(other_tcr, ptr_to_lispobj(ro_base), ptr_to_lispobj(ro_limit), delta);
        other_tcr = other_tcr->next;
      } while (other_tcr != tcr);
      lisp_global(IN_GC) = 0;
    }
    return 0;
  }
  return -1;
}


void
adjust_locref(LispObj *loc, LispObj base, LispObj limit, signed_natural delta)
{
  LispObj p = *loc;
  
  if (area_dnode(p, base) < limit) {
    *loc = p+delta;
  }
}

/* like adjust_locref() above, but only changes the contents of LOC if it's
   a tagged lisp pointer */
void
adjust_noderef(LispObj *loc, LispObj base, LispObj limit, signed_natural delta)
{
  LispObj p = *loc;
  int tag_n = fulltag_of(p);

  if (is_node_fulltag(tag_n)) {
    if (area_dnode(p, base) < limit) {
      *loc = p+delta;
    }
  }
}

/* 
   If *loc is a tagged pointer into the address range denoted by BASE and LIMIT,
   nuke it (set it to NIL.)
*/
void
nuke_noderef(LispObj *loc, LispObj base, LispObj limit)
{
  LispObj p = *loc;
  int tag_n = fulltag_of(p);

  if (is_node_fulltag(tag_n)) {
    if (area_dnode(p, base) < limit) {
      *loc = lisp_nil;
    }
  }
}


void
adjust_pointers_in_xp(ExceptionInformation *xp, 
                      LispObj base, 
                      LispObj limit, 
                      signed_natural delta) 
{
  natural *regs = (natural *) xpGPRvector(xp);
#ifdef PPC
  int r;
  for (r = fn; r < 32; r++) {
    adjust_noderef((LispObj *) (&(regs[r])),
                   base,
                   limit,
                   delta);
  }
  adjust_locref((LispObj*) (&(regs[loc_pc])), base, limit, delta);
  adjust_locref((LispObj*) (&(xpPC(xp))), base, limit, delta);
  adjust_locref((LispObj*) (&(xpLR(xp))), base, limit, delta);
  adjust_locref((LispObj*) (&(xpCTR(xp))), base, limit, delta);
#endif

}

void
nuke_pointers_in_xp(ExceptionInformation *xp, 
                      LispObj base, 
                      LispObj limit) 
{
  natural *regs = (natural *) xpGPRvector(xp);
#ifdef PPC
  int r;
  for (r = fn; r < 32; r++) {
    nuke_noderef((LispObj *) (&(regs[r])),
                   base,
                   limit);
  }
#endif

}

void
adjust_pointers_in_range(LispObj *range_start,
                         LispObj *range_end,
                         LispObj base,
                         LispObj limit,
                         signed_natural delta)
{
  LispObj *p = range_start, node, new;
  int tag_n;
  natural nwords;
  hash_table_vector_header *hashp;

  while (p < range_end) {
    node = *p;
    tag_n = fulltag_of(node);
    if (immheader_tag_p(tag_n)) {
      p = (LispObj *) skip_over_ivector((natural) p, node);
    } else if (nodeheader_tag_p(tag_n)) {
      nwords = header_element_count(node);
      nwords += (1 - (nwords&1));
      if ((header_subtag(node) == subtag_hash_vector) &&
          ((((hash_table_vector_header *)p)->flags) & nhash_track_keys_mask)) {
        hashp = (hash_table_vector_header *) p;
        hashp->flags |= nhash_key_moved_mask;
      }
      p++;
      while (nwords--) {
        adjust_noderef(p, base, limit, delta);
        p++;
      }
    } else {
      /* just a cons */
      adjust_noderef(p, base, limit, delta);
      p++;
      adjust_noderef(p, base, limit, delta);
      p++;
    }
  }
}

void
nuke_pointers_in_range(LispObj *range_start,
                         LispObj *range_end,
                         LispObj base,
                         LispObj limit)
{
  LispObj *p = range_start, node, new;
  int tag_n;
  natural nwords;

  while (p < range_end) {
    node = *p;
    tag_n = fulltag_of(node);
    if (immheader_tag_p(tag_n)) {
      p = (LispObj *) skip_over_ivector((natural) p, node);
    } else if (nodeheader_tag_p(tag_n)) {
      nwords = header_element_count(node);
      nwords += (1 - (nwords&1));
      p++;
      while (nwords--) {
        nuke_noderef(p, base, limit);
        p++;
      }
    } else {
      /* just a cons */
      nuke_noderef(p, base, limit);
      p++;
      nuke_noderef(p, base, limit);
      p++;
    }
  }
}

void
adjust_pointers_in_tstack_area(area *a,
                               LispObj base,
                               LispObj limit,
                               LispObj delta)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) a->active,
    *end = start,
    *area_limit = (LispObj *) (a->high);

  for (current = start;
       end != area_limit;
       current = next) {
    next = ptr_from_lispobj(*current);
    end = ((next >= start) && (next < area_limit)) ? next : area_limit;
    if (current[1] == 0) {
      adjust_pointers_in_range(current+2, end, base, limit, delta);
    }
  }
}

void
nuke_pointers_in_tstack_area(area *a,
                             LispObj base,
                             LispObj limit)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) a->active,
    *end = start,
    *area_limit = (LispObj *) (a->high);

  for (current = start;
       end != area_limit;
       current = next) {
    next = ptr_from_lispobj(*current);
    end = ((next >= start) && (next < area_limit)) ? next : area_limit;
    if (current[1] == 0) {
      nuke_pointers_in_range(current+2, end, base, limit);
    }
  }
}

void
adjust_pointers_in_vstack_area(area *a,
                               LispObj base,
                               LispObj limit,
                               LispObj delta)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  if (((natural)p) & sizeof(natural)) {
    adjust_noderef(p, base, limit, delta);
    p++;
  }
  adjust_pointers_in_range(p, q, base, limit, delta);
}

void
nuke_pointers_in_vstack_area(area *a,
                             LispObj base,
                             LispObj limit)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  if (((natural)p) & sizeof(natural)) {
    nuke_noderef(p, base, limit);
    p++;
  }
  nuke_pointers_in_range(p, q, base, limit);
}

#ifdef PPC
void
adjust_pointers_in_cstack_area(area *a,
                               LispObj base,
                               LispObj limit,
                               LispObj delta)
{
  BytePtr
    current,
    next,
    area_limit = a->high,
    low = a->low;

  for (current = a->active; (current >= low) && (current < area_limit); current = next) {
    next = *((BytePtr *)current);
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) &&
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      adjust_noderef(&((lisp_frame *) current)->savefn, base, limit, delta);
      adjust_locref(&((lisp_frame *) current)->savelr, base, limit, delta);
    }
  }
}
#endif



void
adjust_pointers_in_tcrs(TCR *current, LispObj base, LispObj limit, signed_natural delta)
{
  TCR *tcr = current;
  xframe_list *xframes;
  LispObj *tlb_start, *tlb_end;
  ExceptionInformation *xp;

  do {
    xp = tcr->gc_context;
    if (xp) {
      adjust_pointers_in_xp(xp, base, limit, delta);
    }
    for (xframes = (xframe_list *) tcr->xframe;
         xframes;
         xframes = xframes->prev) {
      adjust_pointers_in_xp(xframes->curr, base, limit, delta);
    }
    adjust_pointers_in_range(tcr->tlb_pointer,
                             (LispObj *) ((BytePtr)tcr->tlb_pointer+tcr->tlb_limit),
                             base,
                             limit,
                             delta);
    tcr = tcr->next;
  } while (tcr != current);
}

void
nuke_pointers_in_tcrs(TCR *current, LispObj base, LispObj limit)
{
  TCR *tcr = current;
  xframe_list *xframes;
  LispObj *tlb_start, *tlb_end;
  ExceptionInformation *xp;

  do {
    xp = tcr->gc_context;
    if (xp) {
      nuke_pointers_in_xp(xp, base, limit);
    }
    for (xframes = (xframe_list *) tcr->xframe;
         xframes;
         xframes = xframes->prev) {
      nuke_pointers_in_xp(xframes->curr, base, limit);
    }
    nuke_pointers_in_range(tcr->tlb_pointer,
                           (LispObj *) ((BytePtr)tcr->tlb_pointer+tcr->tlb_limit),
                           base,
                           limit);
    tcr = tcr->next;
  } while (tcr != current);
}

void
adjust_gcable_ptrs(LispObj base, LispObj limit, signed_natural delta)
{
  /* These need to be special-cased, because xmacptrs are immediate
     objects that contain (in their "link" fields") tagged pointers
     to other xmacptrs */
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((next = *prev) != (LispObj)NULL) {
    adjust_noderef(prev, base, limit, delta);
    if (delta < 0) {
      /* Assume that we've already moved things */
      next = *prev;
    }
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}
    

void
adjust_pointers_in_dynamic_area(area *a, 
                                LispObj base, 
                                LispObj limit,
                                signed_natural delta)
{
  natural 
    nstatic = static_dnodes_for_area(a),
    nstatic_bitmap_words = nstatic >> bitmap_shift;
  LispObj 
    *low = (LispObj *) (a->low),
    *active = (LispObj *) (a->active),
    *dynamic_low = low + (2 * nstatic);

  adjust_pointers_in_range(dynamic_low, active, base, limit, delta);

  if (nstatic && (nstatic <= a->ndnodes)) {
    cons *pagelet_start = (cons *) a->low, *work;
    bitvector usedbits = tenured_area->static_used;
    natural used, i;
    
    while (nstatic_bitmap_words--) {
      used = *usedbits++;

      while (used) {
        i = count_leading_zeros(used);
        used &= ~(BIT0_MASK >> i);
        work = pagelet_start+i;
        adjust_noderef(&(work->cdr), base, limit, delta);
        adjust_noderef(&(work->car), base, limit, delta);
      }
      pagelet_start += nbits_in_word;
    }
  }
}

void
nuke_pointers_in_dynamic_area(area *a, 
                              LispObj base, 
                              LispObj limit)
{
  natural 
    nstatic = static_dnodes_for_area(a),
    nstatic_bitmap_words = nstatic >> bitmap_shift;
  LispObj 
    *low = (LispObj *) (a->low),
    *active = (LispObj *) (a->active),
    *dynamic_low = low + (2 * nstatic);

  nuke_pointers_in_range(dynamic_low, active, base, limit);

  if (nstatic && (nstatic <= a->ndnodes)) {
    cons *pagelet_start = (cons *) a->low, *work;
    bitvector usedbits = tenured_area->static_used;
    natural used, i;
    
    while (nstatic_bitmap_words--) {
      used = *usedbits++;

      while (used) {
        i = count_leading_zeros(used);
        used &= ~(BIT0_MASK >> i);
        work = pagelet_start+i;
        nuke_noderef(&(work->cdr), base, limit);
        nuke_noderef(&(work->car), base, limit);
      }
      pagelet_start += nbits_in_word;
    }
  }
}

    
void
adjust_all_pointers(LispObj base, LispObj limit, signed_natural delta)
{
  area *next_area;
  area_code code;

  for (next_area = active_dynamic_area; 
       (code = next_area->code) != AREA_VOID;
       next_area = next_area->succ) {
    switch (code) {
    case AREA_TSTACK:
      adjust_pointers_in_tstack_area(next_area, base, limit, delta);
      break;
      
    case AREA_VSTACK:
      adjust_pointers_in_vstack_area(next_area, base, limit, delta);
      break;

    case AREA_CSTACK:
      adjust_pointers_in_cstack_area(next_area, base, limit, delta);
      break;

    case AREA_STATIC:
    case AREA_MANAGED_STATIC:
      adjust_pointers_in_range((LispObj *) (next_area->low),
                               (LispObj *) (next_area->active),
                               base,
                               limit,
                               delta);
      break;

    case AREA_DYNAMIC:
      adjust_pointers_in_dynamic_area(next_area, base, limit, delta);
      break;
    }
  }
  adjust_pointers_in_tcrs(get_tcr(false), base, limit, delta);
  adjust_gcable_ptrs(base, limit, delta);
}

void
nuke_all_pointers(LispObj base, LispObj limit)
{
  area *next_area;
  area_code code;

  for (next_area = active_dynamic_area; 
       (code = next_area->code) != AREA_VOID;
       next_area = next_area->succ) {
    switch (code) {
    case AREA_TSTACK:
      nuke_pointers_in_tstack_area(next_area, base, limit);
      break;
      
    case AREA_VSTACK:
      nuke_pointers_in_vstack_area(next_area, base, limit);
      break;

    case AREA_CSTACK:
      /* There aren't any "nukable" pointers in a cstack area */
      break;

    case AREA_STATIC:
    case AREA_MANAGED_STATIC:
      nuke_pointers_in_range((LispObj *) (next_area->low),
                               (LispObj *) (next_area->active),
                               base,
                               limit);
      break;

    case AREA_DYNAMIC:
      nuke_pointers_in_dynamic_area(next_area, base, limit);
      break;
    }
  }
  nuke_pointers_in_tcrs(get_tcr(false), base, limit);
}

#ifndef MREMAP_MAYMOVE
#define MREMAP_MAYMOVE 1
#endif

#ifdef FREEBSD
void *
freebsd_mremap(void *old_address, 
	       size_t old_size, 
	       size_t new_size, 
	       unsigned long flags)
{
  return old_address;
}
#define mremap freebsd_mremap

#endif

#ifdef DARWIN
void *
darwin_mremap(void *old_address, 
	      size_t old_size, 
	      size_t new_size, 
	      unsigned long flags)
{
  void *end = (void *) ((char *)old_address+old_size);

  if (old_size == new_size) {
    return old_address;
  }
  if (new_size < old_size) {
    munmap(end, old_size-new_size);
    return old_address;
  }
  {
    void * new_address = mmap(NULL,
                              new_size,
                              PROT_READ|PROT_WRITE,
                              MAP_PRIVATE | MAP_ANON,
                              -1,
                              0);
    if (new_address !=  MAP_FAILED) {
      vm_copy(mach_task_self(),
              (vm_address_t)old_address,
              old_size,
              (vm_address_t)new_address);
      munmap(old_address, old_size);
    }
    return new_address;
  }
}

#define mremap darwin_mremap
#endif

Boolean
resize_used_bitvector(natural new_dnodes, bitvector *newbits)
{
  natural
    old_dnodes = tenured_area->static_dnodes,
    old_page_aligned_size =
    (align_to_power_of_2((align_to_power_of_2(old_dnodes, log2_nbits_in_word)>>3),
                         log2_page_size)),
    new_page_aligned_size =
    (align_to_power_of_2((align_to_power_of_2(new_dnodes, log2_nbits_in_word)>>3),
                         log2_page_size));
  bitvector old_used = tenured_area->static_used, new_used = NULL;

  if (old_page_aligned_size == new_page_aligned_size) {
    *newbits = old_used;
    return true;
  }

  if (old_used == NULL) {
    new_used = mmap(NULL,
                    new_page_aligned_size,
                    PROT_READ|PROT_WRITE,
                    MAP_PRIVATE | MAP_ANON,
                    -1,
                    0);
    if (new_used == MAP_FAILED) {
      *newbits = NULL;
      return false;
    } else {
      *newbits = new_used;
      return true;
    }
  }
  if (new_page_aligned_size == 0) {
    munmap(old_used, old_page_aligned_size);
    *newbits = NULL;
    return true;
  }
    
  /* Have to try to remap the old bitmap.  That's implementation-dependent,
     and (naturally) Mach sucks, but no one understands how.
  */
  new_used = mremap(old_used, 
                    old_page_aligned_size, 
                    new_page_aligned_size, 
                    MREMAP_MAYMOVE);
  if (new_used == MAP_FAILED) {
    *newbits = NULL;
    return false;
  }
  *newbits = new_used;
  return true;
}

  
int
grow_hons_area(signed_natural delta_in_bytes)
{
  bitvector new_used;
  area *ada = active_dynamic_area;
  natural 
    delta_in_dnodes = delta_in_bytes >> dnode_shift,
    current_static_dnodes = tenured_area->static_dnodes,
    new_static_dnodes;
    
  delta_in_dnodes = align_to_power_of_2(delta_in_dnodes,log2_nbits_in_word);
  new_static_dnodes = current_static_dnodes+delta_in_dnodes;
  delta_in_bytes = delta_in_dnodes << dnode_shift;
  if (grow_dynamic_area((natural) delta_in_bytes)) {
    LispObj 
      base = (LispObj) (ada->low + (current_static_dnodes*dnode_size)),
      oldactive = (LispObj) ada->active,
      limit = area_dnode(oldactive, base);
    if (!resize_used_bitvector(new_static_dnodes, &new_used)) {
      shrink_dynamic_area(delta_in_bytes);
      return -1;
    }
    tenured_area->static_used = new_used;
    adjust_all_pointers(base, limit, delta_in_bytes);
    memmove((void *)(base+delta_in_bytes),(void *)base,oldactive-base);
    ada->ndnodes = area_dnode(ada->high, ada->low);
    ada->active += delta_in_bytes;
    {
      LispObj *p;
      natural i;
      for (p = (LispObj *)(tenured_area->low + (current_static_dnodes << dnode_shift)), i = 0;
           i< delta_in_dnodes;
           i++ ) {
        *p++ = undefined;
        *p++ = undefined;
      }
      tenured_area->static_dnodes += delta_in_dnodes;
      xMakeDataExecutable(tenured_area->low+(tenured_area->static_dnodes<<dnode_shift),
                          ada->active-(tenured_area->low+(tenured_area->static_dnodes<<dnode_shift)));
          
    }
    return 0;
  }
  return -1;
}

int 
shrink_hons_area(signed_natural delta_in_bytes)
{
  area *ada = active_dynamic_area;
  signed_natural 
    delta_in_dnodes = delta_in_bytes >> dnode_shift;
  natural 
    current_static_dnodes = tenured_area->static_dnodes,
    new_static_dnodes;
  LispObj base, limit, oldactive;
  bitvector newbits;

    
  delta_in_dnodes = -align_to_power_of_2(-delta_in_dnodes,log2_nbits_in_word);
  new_static_dnodes = current_static_dnodes+delta_in_dnodes;
  delta_in_bytes = delta_in_dnodes << dnode_shift;
  oldactive = (LispObj) (ada->active);

  resize_used_bitvector(new_static_dnodes, &newbits);
  tenured_area->static_used = newbits; /* redundant */

  memmove(ada->low+(new_static_dnodes << dnode_shift),
          ada->low+(current_static_dnodes << dnode_shift),
          oldactive-(natural)(ada->low+(current_static_dnodes << dnode_shift)));
  tenured_area->static_dnodes = new_static_dnodes;
  ada->active -= -delta_in_bytes; /* delta_in_bytes is negative */
  shrink_dynamic_area(-delta_in_bytes);

  base = (LispObj) (tenured_area->low + 
                    (new_static_dnodes << dnode_shift));
  limit = area_dnode(tenured_area->low + 
                     (current_static_dnodes << dnode_shift), base);
  nuke_all_pointers(base, limit);

  base = (LispObj) (tenured_area->low + 
                    (current_static_dnodes << dnode_shift));
  limit = area_dnode(oldactive, base);
  adjust_all_pointers(base, limit, delta_in_bytes);

  xMakeDataExecutable(tenured_area->low+(tenured_area->static_dnodes<<dnode_shift),
                      ada->active-(tenured_area->low+(tenured_area->static_dnodes<<dnode_shift)));
  return 0;
}

int
change_hons_area_size(TCR *tcr, signed_natural delta_in_bytes)
{
  if (delta_in_bytes > 0) {
    return grow_hons_area(delta_in_bytes);
  }
  if (delta_in_bytes < 0) {
    return shrink_hons_area(delta_in_bytes);
  }
  return 0;
}

