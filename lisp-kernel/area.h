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

#ifndef __AREA_H__
#define __AREA_H__ 1


#include "bits.h"
#include "memprotect.h"



typedef enum {
  AREA_VOID = 0,		/* Not really an area at all */
  AREA_CSTACK = 4,              /* A control stack */
  AREA_VSTACK = 8,              /* A value stack.  The GC sees it as being doubleword-aligned */
  AREA_TSTACK = 12,             /* A temp stack.  It -is- doubleword-aligned */
  AREA_READONLY = 16,           /* A (cfm) read-only section. */
  AREA_STATICLIB = 20,          /* A static section in a shared library.  May
                                 contain pointers to other static sections, but
                                 not to any dynamic areas */
  AREA_STATIC = 24,             /* A (cfm or other) static section: contains
                                 roots, but not GCed */
  AREA_DYNAMIC = 28		/* A heap. Only one such area is "the heap."*/
} area_code;

typedef struct area {
  NATURAL_POINTER_FIELD(struct area,pred);  /* linked list predecessor */
  NATURAL_POINTER_FIELD(struct area,succ);  /* linked list successor */
  NATURAL_POINTER_FIELD(char,low);        /* arithmetic lower limit on addresses (inclusive) */
  NATURAL_POINTER_FIELD(char,high);        /* arithmetic upper limit on addresses (exclusive) */
  NATURAL_POINTER_FIELD(char,active);       /* low bound (stack) or high bound (heap) */
  NATURAL_POINTER_FIELD(char,softlimit);		/* only makes sense for dynamic heaps & stacks */
  NATURAL_POINTER_FIELD(char,hardlimit);		/* only makes sense for dynamic heaps & stacks */
  natural code;
  NATURAL_POINTER_FIELD(natural, markbits);           /* markbits for active area */
  natural ndnodes;		/* "active" size of dynamic area or stack */
  NATURAL_POINTER_FIELD(struct area,older);		/* if ephemeral, the next older ephemeral area
				 or the dynamic area */
  NATURAL_POINTER_FIELD(struct area,younger); /* if ephemeral, the next "younger" ephemeral area
                                  if there is one.  If dynamic, the oldest ephemeral
				 area. */
  NATURAL_POINTER_FIELD(char, h);			/* The pointer allocated to contain this area, or NULL
				 if the operating system allocated it for us. */
  NATURAL_POINTER_FIELD(protected_area,softprot);  /* "soft" protected_area */
  NATURAL_POINTER_FIELD(protected_area,hardprot);  /* "hard" protected_area */
  natural owner;               /* position in external_containers linked list */
  NATURAL_POINTER_FIELD(natural, refbits); /* intergenerational references.
                                               May or may not be the same as markbits */
  natural threshold; /* egc threshold (boxed "fullword count") or 0 */
  LispObj gccount;              /* boxed generation GC count. */
} area;


/*
  Areas are kept in a doubly-linked list.
  The list header is just a distinguished element of
  that list; by convention, the "active" dynamic
  area is described by that header's successor, and areas
  that may have entries in their "markbits" vector (heaps)
  precede (in the area_list->succ sense) those  that don't (stacks).
  The list header's "area" pointer is an "AREA_VOID" area; the header
  (once allocated during kernel initialization) never
  moves or changes.  Lisp code can get its hands on
  the list header via a nilreg global, and carefully,
  atomically, traverse it to do ROOM, etc.
*/


area *new_area(BytePtr, BytePtr, area_code);
void add_area(area *);
area *remove_area(area *);
area *area_containing(BytePtr);
area *stack_area_containing(BytePtr);
area *heap_area_node_containing(BytePtr);
void tenure_to_area(area *);
void untenure_from_area(area *);

/* serialize add_area/remove_area */
void *area_lock;

#define reserved_area ((area *)(all_areas))
#define active_dynamic_area ((area *)(reserved_area->succ))

typedef struct area_list {
  area *the_area;
  struct area_list *next;
} area_list;

/* The useable size of a tsp or vsp stack segment.
  */
/* #define STACK_SEGMENT_SIZE (64<<10) */
#define MIN_CSTACK_SIZE (1<<17)
#define CSTACK_HARDPROT (100<<10)
#define CSTACK_SOFTPROT (100<<10)
#define MIN_VSTACK_SIZE (1<<16)
#define VSTACK_HARDPROT (1<<12)
#define VSTACK_SOFTPROT (1<<16)
#define MIN_TSTACK_SIZE (1<<18)
#define TSTACK_HARDPROT 0
#define TSTACK_SOFTPROT (1<<16)
#define CS_OVERFLOW_FORCE_LIMIT ((unsigned)0xfffffff0)

#ifdef LINUX
#define IMAGE_BASE_ADDRESS 0x31000000
#endif
#ifdef DARWIN
#define IMAGE_BASE_ADDRESS 0x01000000
#endif

#define PURESPACE_RESERVE 0x04000000 /* 64MB */
#define STATIC_RESERVE heap_segment_size
#define STATIC_BASE_ADDRESS 0x00001000

extern LispObj image_base;
extern BytePtr pure_space_start, pure_space_active, pure_space_limit;
extern BytePtr static_space_start, static_space_active, static_space_limit;
extern area *find_readonly_area(void);

#endif /* __AREA_H__ */
