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

#ifndef __constants64__
#define __constants64__ 1


#define nbits_in_word 64
#define nbits_in_byte 8
#define ntagbits 4	
#define nlisptagbits 3
#define nfixnumtagbits 2
#define num_subtag_bits 8
#define fixnumshift 3
#define fixnum_shift 3
#define fulltagmask 15
#define tagmask	 7
#define fixnummask 3
#define subtagmask ((1<<num_subtag_bits)-1)
#define ncharcodebits 16
#define charcode_shift (nbits_in_word-ncharcodebits)
#define node_size 8
#define node_shift 3

#define lowtagmask 3
#define lowtag_mask lowtagmask

#define lowtag_primary 0
#define lowtag_imm 1
#define lowtag_immheader 2
#define lowtag_nodeheader 3

#define tag_fixnum 0

#define fulltag_even_fixnum 0
#define fulltag_imm_0 1
#define fulltag_immheader_0 2
#define fulltag_nodeheader_0 3
#define fulltag_cons 4
#define fulltag_imm_1 5
#define fulltag_immheader_1 6
#define fulltag_nodeheader_1 7
#define fulltag_odd_fixnum 8
#define fulltag_imm_2 9
#define fulltag_immheader_2 10
#define fulltag_nodeheader_2 11
#define fulltag_misc 12
#define fulltag_imm_3 13
#define fulltag_immheader_3 14
#define fulltag_nodeheader_3 15

#define SUBTAG(tag,subtag) ((tag) | ((subtag) << ntagbits))
#define cl_array_subtag_mask 0x80
#define CL_ARRAY_SUBTAG(tag,subtag) (cl_array_subtag_mask | (SUBTAG(tag,subtag)))

#define subtag_arrayH CL_ARRAY_SUBTAG(fulltag_nodeheader_1,0)
#define subtag_vectorH CL_ARRAY_SUBTAG(fulltag_nodeheader_2,0)
#define subtag_simple_vector CL_ARRAY_SUBTAG(fulltag_nodeheader_3,0)
#define min_vector_subtag subtag_vectorH	

#define ivector_class_64_bit fulltag_immheader_3
#define ivector_class_32_bit fulltag_immheader_2
#define ivector_class_other_bit fulltag_immheader_1
#define ivector_class_8_bit fulltag_immheader_0

#define subtag_s64_vector CL_ARRAY_SUBTAG(ivector_class_64_bit,1)
#define subtag_u64_vector CL_ARRAY_SUBTAG(ivector_class_64_bit,2)
#define subtag_double_float_vector CL_ARRAY_SUBTAG(ivector_class_64_bit,4)
#define subtag_s32_vector CL_ARRAY_SUBTAG(ivector_class_32_bit,1)
#define subtag_u32_vector CL_ARRAY_SUBTAG(ivector_class_32_bit,2)
#define subtag_single_float_vector CL_ARRAY_SUBTAG(ivector_class_32_bit,3)
#define subtag_s16_vector CL_ARRAY_SUBTAG(ivector_class_other_bit,1)
#define subtag_u16_vector CL_ARRAY_SUBTAG(ivector_class_other_bit,2)
#define subtag_bit_vector CL_ARRAY_SUBTAG(ivector_class_other_bit,7)
#define subtag_s8_vector CL_ARRAY_SUBTAG(ivector_class_8_bit,1)
#define subtag_u8_vector CL_ARRAY_SUBTAG(ivector_class_8_bit,2)
#define subtag_simple_base_string CL_ARRAY_SUBTAG(ivector_class_8_bit,5)


/* There's some room for expansion in non-array ivector space. */
#define subtag_bignum SUBTAG(ivector_class_64_bit,0)
#define subtag_double_float SUBTAG(ivector_class_64_bit,3)
#define subtag_macptr SUBTAG(ivector_class_64_bit,5)
#define subtag_dead_macptr SUBTAG(ivector_class_64_bit,6)
#define subtag_code_vector SUBTAG(ivector_class_32_bit,0)
#define subtag_xcode_vector SUBTAG(ivector_class_32_bit,1)


/*
 Size doesn't matter for non-CL-array gvectors; I can't think of a good
 reason to classify them in any particular way.  Let's put funcallable
 things in the first slice by themselves, though it's not clear that
 that helps FUNCALL much.
*/
#define gvector_funcallable fulltag_nodeheader_0
	
#define subtag_function SUBTAG(gvector_funcallable,0)
#define subtag_symbol SUBTAG(gvector_funcallable,1)
#define subtag_catch_frame SUBTAG(fulltag_nodeheader_1,0)
#define subtag_lisp_thread SUBTAG(fulltag_nodeheader_1,1)
#define subtag_lock SUBTAG(fulltag_nodeheader_1,2)
#define subtag_hash_vector SUBTAG(fulltag_nodeheader_1,3)
#define subtag_pool SUBTAG(fulltag_nodeheader_1,4)
#define subtag_weak SUBTAG(fulltag_nodeheader_1,5)
#define subtag_package SUBTAG(fulltag_nodeheader_1,6)
#define subtag_svar SUBTAG(fulltag_nodeheader_1,7)
#define subtag_slot_vector SUBTAG(0,fulltag_nodeheader_2)
#define subtag_instance SUBTAG(1,fulltag_nodeheader_2)
#define subtag_struct SUBTAG(2,fulltag_nodeheader_2)
#define subtag_istruct SUBTAG(3,fulltag_nodeheader_2)
#define subtag_value_cell SUBTAG(4,fulltag_nodeheader_2)
#define subtag_xfunction SUBTAG(5,fulltag_nodeheader_2)


#define nil_value (0x2000+fulltag_misc+sizeof(struct lispsymbol))
#define t_value (0x2000+fulltag_misc)	
#define misc_bias fulltag_misc
#define cons_bias fulltag_cons

	
#define misc_header_offset -fulltag_misc
#define misc_subtag_offset misc_header_offset+7       /* low byte of header */
#define misc_data_offset misc_header_offset+4		/* first word of data */
#define misc_dfloat_offset misc_header_offset		/* double-floats are doubleword-aligned */

#define subtag_single_float SUBTAG(fulltag_imm_0,0)

#define subtag_go_tag SUBTAG(fulltag_imm_1,2) /* deprecated */
#define subtag_block_tag SUBTAG(fulltag_imm_1,3) /* deprecated */

#define subtag_character SUBTAG(fulltag_imm_2,0)

#define subtag_unbound SUBTAG(fulltag_imm_3,0)
#define unbound_marker subtag_unbound
#define undefined unbound_marker
#define unbound unbound_marker
#define subtag_slot_unbound SUBTAG(fulltag_imm_3,1)
#define slot_unbound_marker subtag_slot_unbound
#define subtag_illegal SUBTAG(fulltag_imm_3,2)
#define illegal_marker subtag_illegal
#define subtag_no_thread_local_binding SUBTAG(fulltag_imm_3,3)
#define no_thread_local_binding_marker subtag_no_thread_local_binding        
#define subtag_forward_marker SUBTAG(fulltag_imm_3,15)
	
#define max_64_bit_constant_index ((0x7fff + misc_dfloat_offset)>>3)
#define max_32_bit_constant_index ((0x7fff + misc_data_offset)>>2)
#define max_16_bit_constant_index ((0x7fff + misc_data_offset)>>1)
#define max_8_bit_constant_index (0x7fff + misc_data_offset)
#define max_1_bit_constant_index ((0x7fff + misc_data_offset)<<5)


/* The objects themselves look something like this: */

/*  Order of CAR and CDR doesn't seem to matter much - there aren't */
/*  too many tricks to be played with predecrement/preincrement addressing. */
/*  Keep them in the confusing MCL 3.0 order, to avoid confusion. */

typedef struct cons {
  LispObj cdr;
  LispObj car;
} cons;



typedef struct lispsymbol {
  LispObj header;
  LispObj pname;
  LispObj vcell;
  LispObj fcell;
  LispObj package_plist;
  LispObj flags;
} lispsymbol;

typedef struct ratio {
  LispObj header;
  LispObj numer;
  LispObj denom;
} ratio;

typedef struct double_float {
  LispObj header;
  LispObj value;
} double_float;


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
  LispObj params[8];
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


/* 
  All exception frames in a thread are linked together 
  */
typedef struct xframe_list {
  ExceptionInformationPowerPC *curr;
  struct xframe_list *prev;
} xframe_list;

#define fixnum_bitmask(n)  (1LL<<((n)+fixnumshift))

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





typedef struct tcr {
  NATURAL_POINTER_FIELD(struct tcr,next);
  NATURAL_POINTER_FIELD(struct tcr,prev);
  struct {
    float f;
    u_int32_t tag;
  } single_float_convert;
  union {
    double d;
    struct {u_int32_t h, l;} words;
  } lisp_fpscr;			/* lisp thread's fpscr (in low word) */
  NATURAL_POINTER_FIELD(special_binding,db_link);	/* special binding chain head */
  LispObj catch_top;		/* top catch frame */
  NATURAL_POINTER_FIELD(LispObj,save_vsp);  /* VSP when in foreign code */
  NATURAL_POINTER_FIELD(LispObj,save_tsp);  /* TSP when in foreign code */
  NATURAL_POINTER_FIELD(struct area,cs_area); /* cstack area pointer */
  NATURAL_POINTER_FIELD(struct area,vs_area); /* vstack area pointer */
  NATURAL_POINTER_FIELD(struct area,ts_area); /* tstack area pointer */
  LispObj cs_limit;		/* stack overflow limit */
  natural bytes_allocated;
  signed_natural interrupt_level;      /* for w-o-i preemption */
  signed_natural interrupt_pending;	/* likewise */
  NATURAL_POINTER_FIELD(xframe_list,xframe); /* exception-frame linked list */
  NATURAL_POINTER_FIELD(int,errno_loc);		/* per-thread (?) errno location */
  LispObj ffi_exception;	/* fpscr bits from ff-call */
  LispObj osid;			/* OS thread id */
  signed_natural valence;			/* odd when in foreign code */
  signed_natural foreign_exception_status;	/* non-zero -> call lisp_exit_hook */
  NATURAL_POINTER_FIELD(void,native_thread_info);	/* platform-dependent */
  NATURAL_POINTER_FIELD(void,native_thread_id);	/* mach_thread_t, pid_t, etc. */
  NATURAL_POINTER_FIELD(void,last_allocptr);
  NATURAL_POINTER_FIELD(void,save_allocptr);
  NATURAL_POINTER_FIELD(void,save_allocbase);
  NATURAL_POINTER_FIELD(void,reset_completion);
  NATURAL_POINTER_FIELD(void,activate);
  signed_natural suspend_count;
  NATURAL_POINTER_FIELD(ExceptionInformation,suspend_context);
  NATURAL_POINTER_FIELD(ExceptionInformation,pending_exception_context);
  NATURAL_POINTER_FIELD(void,suspend);		/* suspension semaphore */
  NATURAL_POINTER_FIELD(void,resume);			/* resumption semaphore */
  u_int32_t flags_pad;
  int32_t flags;
  NATURAL_POINTER_FIELD(ExceptionInformation,gc_context);
  signed_natural suspend_total;
  signed_natural suspend_total_on_exception_entry;
  natural tlb_limit;
  NATURAL_POINTER_FIELD(LispObj,tlb_pointer);
  natural shutdown_count;
} TCR;

#define t_offset -(sizeof(lispsymbol))


#endif

