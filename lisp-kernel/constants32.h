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

#ifndef __constants32__
#define __constants32__ 1

#define rcontext 13

#define nbits_in_word 32
#define nbits_in_byte 8
#define ntagbits 3	/* But only 2 are significant to lisp */
#define nlisptagbits 2
#define nfixnumtagbits 2
#define num_subtag_bits 8
#define fixnumshift 2
#define fixnum_shift 2
#define fulltagmask 7
#define tagmask	 3
#define fixnummask 3
#define subtagmask ((1<<num_subtag_bits)-1)
#define ncharcodebits 16
#define charcode_shift (nbits_in_word-ncharcodebits)
#define node_size 4
#define node_shift 2

/*  Tags. */
/*  There are two-bit tags and three-bit tags. */
/*  A FULLTAG is the value of the low three bits of a tagged object. */
/*  A TAG is the value of the low two bits of a tagged object. */
/*  A TYPECODE is either a TAG or the value of a "tag-misc" object's header-byte. */

/*  There are 4 primary TAG values.  Any object which lisp can "see" can be classified  */
/*  by its TAG.  (Some headers have FULLTAGS that are congruent modulo 4 with the */
/*  TAGS of other objects, but lisp can't "see" headers.) */


#define tag_fixnum 0	/*  All fixnums, whether odd or even */
#define tag_list 1	/*  Conses and NIL */
#define tag_misc 2	/*  Heap-consed objects other than lists: vectors, symbols, functions, floats ... */
#define tag_imm	 3	/*  Immediate-objects: characters, UNBOUND, other markers. */

/*  And there are 8 FULLTAG values.  Note that NIL has its own FULLTAG (congruent mod 4 to tag-list), */
/*  that FULLTAG-MISC is > 4 (so that code-vector entry-points can be branched to, since the low */
/*  two bits of the PC are ignored) and that both FULLTAG-MISC and FULLTAG-IMM have header fulltags */
/*  that share the same TAG. */
/*  Things that walk memory (and the stack) have to be careful to look at the FULLTAG of each */
/*  object that they see. */

#define fulltag_even_fixnum 0	/*  I suppose EVENP/ODDP might care; nothing else does. */
#define fulltag_cons	 1	/*  a real (non_null) cons.  Shares TAG with fulltag_nil. */
#define fulltag_nodeheader 2	/*  Header of heap_allocated object that contains lisp_object pointers */
#define fulltag_imm	 3	/*  a "real" immediate object.  Shares TAG with fulltag_immheader. */
#define fulltag_odd_fixnum 4	/*   */
#define fulltag_nil	 5	/*  NIL and nothing but.  (Note that there's still a hidden NILSYM.) */
#define fulltag_misc	 6	/*  Pointer "real" tag_misc object.  Shares TAG with fulltag_nodeheader. */
#define fulltag_immheader 7	/*  Header of heap-allocated object that contains unboxed data. */



/*  Order of CAR and CDR doesn't seem to matter much - there aren't */
/*  too many tricks to be played with predecrement/preincrement addressing. */
/*  Keep them in the confusing MCL 3.0 order, to avoid confusion. */

typedef struct cons {
  LispObj cdr;
  LispObj car;
} cons;


#define misc_header_offset -fulltag_misc
#define misc_subtag_offset misc_header_offset+3		/*  low byte of header */
#define misc_data_offset misc_header_offset+4		/*  first word of data */
#define misc_dfloat_offset misc_header_offset+8		/*  double-floats are doubleword-aligned */

#define max_64_bit_constant_index ((0x7fff + misc_dfloat_offset)>>3)
#define max_32_bit_constant_index ((0x7fff + misc_data_offset)>>2)
#define max_16_bit_constant_index ((0x7fff + misc_data_offset)>>1)
#define max_8_bit_constant_index (0x7fff + misc_data_offset)
#define max_1_bit_constant_index ((0x7fff + misc_data_offset)<<5)

/*  T is almost adjacent to NIL: since NIL is a misaligned CONS, it spans */
/*  two doublewords.  The arithmetic difference between T and NIL is */
/*  such that the least-significant bit and exactly one other bit is */
/*  set in the result. */

#define t_offset (8+(8-fulltag_nil)+fulltag_misc)
#define t_value (lisp_nil+t_offset)

/*  The order in which various header values are defined is significant in several ways: */
/*  1) Numeric subtags precede non-numeric ones; there are further orderings among numeric subtags. */
/*  2) All subtags which denote CL arrays are preceded by those that don't, */
/*     with a further ordering which requires that (< header-arrayH header-vectorH ,@all-other-CL-vector-types) */
/*  3) The element-size of ivectors is determined by the ordering of ivector subtags. */
/*  4) All subtags are >= fulltag-immheader . */

#define SUBTAG(tag,subtag) ((tag) | ((subtag) << ntagbits))
#define IMM_SUBTAG(subtag) SUBTAG(fulltag_immheader,(subtag))
#define NODE_SUBTAG(subtag) SUBTAG(fulltag_nodeheader,(subtag))

	
/* Numeric subtags. */

#define subtag_bignum IMM_SUBTAG(0)
#define min_numeric_subtag subtag_bignum

#define subtag_ratio NODE_SUBTAG(1)
#define max_rational_subtag subtag_ratio

#define subtag_single_float IMM_SUBTAG(1)
#define subtag_double_float IMM_SUBTAG(2)
#define min_float_subtag subtag_single_float
#define max_float_subtag subtag_double_float
#define max_real_subtag subtag_double_float

#define subtag_complex NODE_SUBTAG(3)
#define max_numeric_subtag subtag_complex


/*  CL array types.  There are more immediate types than node types; all CL array subtags must be > than */
/*  all non-CL-array subtags.  So we start by defining the immediate subtags in decreasing order, starting */
/*  with that subtag whose element size isn't an integral number of bits and ending with those whose */
/*  element size - like all non-CL-array fulltag-immheader types - is 32 bits. */

#define subtag_bit_vector IMM_SUBTAG(31)
#define subtag_double_float_vector IMM_SUBTAG(30)
#define subtag_s16_vector IMM_SUBTAG(29)
#define subtag_u16_vector IMM_SUBTAG(28)
#define subtag_simple_general_string IMM_SUBTAG(27)
#define min_16_bit_ivector_subtag subtag_simple_general_string
#define max_16_bit_ivector_subtag subtag_s16_vector
#define max_string_subtag subtag_simple_general_string

#define subtag_simple_base_string IMM_SUBTAG(26)
#define subtag_s8_vector IMM_SUBTAG(25)
#define subtag_u8_vector IMM_SUBTAG(24)
#define min_8_bit_ivector_subtag subtag_u8_vector
#define max_8_bit_ivector_subtag subtag_simple_base_string
#define min_string_subtag subtag_simple_base_string

#define subtag_s32_vector IMM_SUBTAG(23)
#define subtag_u32_vector IMM_SUBTAG(22)
#define subtag_single_float_vector IMM_SUBTAG(21)
#define max_32_bit_ivector_subtag subtag_s32_vector
#define min_cl_ivector_subtag subtag_single_float_vector


#define subtag_vectorH NODE_SUBTAG(21)
#define subtag_arrayH NODE_SUBTAG(20)
#define subtag_simple_vector NODE_SUBTAG(22)	/*  Only one such subtag) */
#define min_vector_subtag subtag_vectorH
#define min_array_subtag subtag_arrayH

/*  So, we get the remaining subtags (n: (n > max-numeric-subtag) & (n < min-array-subtag)) */
/*  for various immediate/node object types. */

#define subtag_macptr IMM_SUBTAG(3)
#define min_non_numeric_imm_subtag subtag_macptr

#define subtag_dead_macptr IMM_SUBTAG(4)
#define subtag_code_vector IMM_SUBTAG(5)
#define subtag_creole IMM_SUBTAG(6)

#define max_non_array_imm_subtag ((19<<ntagbits)|fulltag_immheader)

#define subtag_catch_frame NODE_SUBTAG(4)
#define subtag_function NODE_SUBTAG(5)
#define subtag_sgbuf NODE_SUBTAG(6)
#define subtag_symbol NODE_SUBTAG(7)
#define subtag_lock NODE_SUBTAG(8)
#define subtag_hash_vector NODE_SUBTAG(9)
#define subtag_pool NODE_SUBTAG(10)
#define subtag_weak NODE_SUBTAG(11)
#define subtag_package NODE_SUBTAG(12)
#define subtag_mark NODE_SUBTAG(13)
#define subtag_instance NODE_SUBTAG(14)
#define subtag_struct NODE_SUBTAG(15)
#define subtag_istruct NODE_SUBTAG(16)
#define max_non_array_node_subtag ((19<<ntagbits)|fulltag_immheader)
	
/*  The objects themselves look something like this: */

typedef struct lispsymbol {
  LispObj header;
  LispObj pname;
  LispObj vcell;
  LispObj fcell;
  LispObj package_predicate;
  LispObj flags;
  LispObj plist;
  LispObj binding_index;
} lispsymbol;

typedef struct ratio {
  LispObj header;
  LispObj numer;
  LispObj denom;
} ratio;

typedef struct double_float {
  LispObj header;
  LispObj pad;
  LispObj value_high;
  LispObj value_low;
} double_float;

typedef struct single_float {
  LispObj header;
  LispObj value;
} single_float;

typedef struct macptr {
  LispObj header;
  LispObj address;
  LispObj class;
  LispObj type;
} macptr;

typedef struct xmacptr {
  LispObj header;
  LispObj address;
  LispObj class;
  LispObj type;
  LispObj flags;
  LispObj link;
} xmacptr;
  

typedef struct eabi_c_frame {
  struct eabi_c_frame *backlink;
  unsigned savelr;
  unsigned params[8];
} eabi_c_frame;

/* PowerOpen ABI C frame */

typedef struct c_frame {
  struct c_frame *backlink;
  unsigned crsave;
  unsigned savelr;
  unsigned unused[2];
  unsigned savetoc;		/* Used with CFM */
  unsigned params[8];		/* Space for callee to save r3-r10 */
} c_frame;

typedef struct lisp_frame {
  struct lisp_frame *backlink;
  LispObj savefn;
  LispObj savelr;
  LispObj savevsp;
} lisp_frame;

typedef struct special_binding {
  struct special_binding *link;
  struct lispsymbol *sym;
  LispObj value;
} special_binding;

/* The GC (at least) needs to know what a
   package looks like, so that it can do GCTWA. */
typedef struct package {
  LispObj header;
  LispObj itab;			/* itab and etab look like (vector (fixnum . fixnum) */
  LispObj etab;
  LispObj used;
  LispObj used_by;
  LispObj names;
  LispObj shadowed;
} package;

/*
  The GC also needs to know what a catch_frame looks like.
*/

typedef struct catch_frame {
  LispObj header;
  LispObj catch_tag;
  LispObj link;
  LispObj mvflag;
  LispObj csp;
  LispObj db_link;
  LispObj regs[8];
  LispObj xframe;
  LispObj tsp_segment;
} catch_frame;

#define catch_frame_element_count ((sizeof(catch_frame)/sizeof(LispObj))-1)
#define catch_frame_header make_header(subtag_catch_frame,catch_frame_element_count)

#define unbound SUBTAG(fulltag_imm, 6)
#define undefined unbound
#define unbound_marker unbound
#define subtag_character SUBTAG(fulltag_imm, 9)
#define slot_unbound SUBTAG(fulltag_imm, 10)
#define slot_unbound_marker slot_unbound
#define no_thread_local_binding_marker SUBTAG(fulltag_imm,30)

/* 
  All exception frames in a thread are linked together 
  */
typedef struct xframe_list {
  ExceptionInformationPowerPC *curr;
  struct xframe_list *prev;
} xframe_list;

#define fixnum_bitmask(n)  (1<<((n)+fixnumshift))

/* 
  The GC (at least) needs to know about hash-table-vectors and their flag bits.
*/

typedef struct hash_table_vector_header {
  LispObj header;
  LispObj link;                 /* If weak */
  LispObj flags;                /* a fixnum; see below */
  LispObj free_alist;           /* preallocated conses for finalization_alist */
  LispObj finalization_alist;   /* key/value alist for finalization */
  LispObj weak_deletions_count; /* incremented when GC deletes weak pair */
  LispObj hash;                 /* backpointer to hash-table */
  LispObj deleted_count;        /* number of deleted entries */
  LispObj cache_idx;            /* index of last cached pair */
  LispObj cache_key;            /* value of last cached key */
  LispObj cache_value;          /* last cached value */
} hash_table_vector_header;

/*
  Bits (masks)  in hash_table_vector.flags:
*/

/* GC should track keys when addresses change */ 
#define nhash_track_keys_mask fixnum_bitmask(28) 

/* GC should set when nhash_track_keys_bit & addresses change */
#define nhash_key_moved_mask  fixnum_bitmask(27) 

/* weak on key or value (need new "weak both" encoding.) */
#define nhash_weak_mask       fixnum_bitmask(12)

/* weak on value */
#define nhash_weak_value_mask fixnum_bitmask(11)

/* finalizable */
#define nhash_finalizable_mask fixnum_bitmask(10)


/* Lfun bits */

#define lfbits_nonnullenv_mask fixnum_bitmask(0)
#define lfbits_keys_mask fixnum_bitmask(1)
#define lfbits_restv_mask fixnum_bitmask(7)
#define lfbits_optinit_mask fixnum_bitmask(14)
#define lfbits_rest_mask fixnum_bitmask(15)
#define lfbits_aok_mask fixnum_bitmask(16)
#define lfbits_lap_mask fixnum_bitmask(23)
#define lfbits_trampoline_mask fixnum_bitmask(24)
#define lfbits_evaluated_mask fixnum_bitmask(25)
#define lfbits_cm_mask fixnum_bitmask(26)         /* combined_method */
#define lfbits_nextmeth_mask fixnum_bitmask(26)   /* or call_next_method with method_mask */
#define lfbits_gfn_mask fixnum_bitmask(27)        /* generic_function */
#define lfbits_nextmeth_with_args_mask fixnum_bitmask(27)   /* or call_next_method_with_args with method_mask */
#define lfbits_method_mask fixnum_bitmask(28)     /* method function */
/* PPC only but want it defined for xcompile */
#define lfbits_noname_mask fixnum_bitmask(29)

/*
  known values of an "extended" (gcable) macptr's flags word:
*/

typedef enum {
  xmacptr_flag_none = 0,        /* Maybe already disposed by Lisp */
  xmacptr_flag_recursive_lock,  /* recursive-lock */
  xmacptr_flag_ptr,             /* malloc/free */
  xmacptr_flag_rwlock,          /* read/write lock */
  xmacptr_flag_semaphore        /* semaphore */
} xmacptr_flag;

/* Creole */

#define doh_quantum 400
#define doh_block_slots ((doh_quantum >> 2) - 3)

typedef struct doh_block {
  struct doh_block *link;
  unsigned size;
  unsigned free;
  LispObj data[doh_block_slots];
} doh_block, *doh_block_ptr;


#define population_weak_list (0<<fixnum_shift)
#define population_weak_alist (1<<fixnum_shift)
#define population_termination_bit (16+fixnum_shift)
#define population_type_mask ((1<<population_termination_bit)-1)

#define gc_retain_pages_bit fixnum_bitmask(0)
#define gc_integrity_check_bit fixnum_bitmask(2)
#define gc_allow_stack_overflows_bit fixnum_bitmask(5)
#define gc_postgc_pending fixnum_bitmask(26)

#include "lisp-errors.h"




#define nil_value 0x00002015

#define TCR_BIAS (0)

typedef struct tcr {
  struct tcr *next;
  struct tcr *prev;
  union {
    double d;
    struct {unsigned h, l;} words;
  } lisp_fpscr;			/* lisp thread's fpscr (in low word) */
  special_binding *db_link;	/* special binding chain head */
  LispObj catch_top;		/* top catch frame */
  LispObj *save_vsp;		/* VSP when in foreign code */
  LispObj *save_tsp;		/* TSP when in foreign code */
  struct area *cs_area;		/* cstack area pointer */
  struct area *vs_area;		/* vstack area pointer */
  struct area *ts_area;		/* tstack area pointer */
  LispObj cs_limit;		/* stack overflow limit */
  unsigned long long bytes_allocated;
  int interrupt_level;		/* for w-o-i preemption */
  int interrupt_pending;	/* likewise */
  xframe_list *xframe;		/* exception-frame linked list */
  int *errno_loc;		/* per-thread (?) errno location */
  LispObj ffi_exception;	/* fpscr bits from ff-call */
  LispObj osid;			/* OS thread id */
  int valence;			/* odd when in foreign code */
  int foreign_exception_status;	/* non-zero -> call lisp_exit_hook */
  void *native_thread_info;	/* platform-dependent */
  void *native_thread_id;	/* mach_thread_t, pid_t, etc. */
  void *last_allocptr;
  void *save_allocptr;
  void *save_allocbase;
  void *reset_completion;
  void *activate;
  int suspend_count;
  ExceptionInformation *suspend_context;
  ExceptionInformation *pending_exception_context;
  void *suspend;		/* suspension semaphore */
  void *resume;			/* resumption semaphore */
  int flags;
  ExceptionInformation *gc_context;
  int suspend_total;
  int suspend_total_on_exception_entry;
  unsigned tlb_limit;
  LispObj *tlb_pointer;
  unsigned shutdown_count;
} TCR;

/* 
  These were previously global variables.  There are lots of implicit
  assumptions about the size of a heap segment, so they might as well
  be constants.
*/

#define heap_segment_size 0x00010000
#define log2_heap_segment_size 16


#endif

