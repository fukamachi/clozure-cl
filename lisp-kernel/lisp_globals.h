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

#ifndef __lisp_globals__
#define __lisp_globals__


extern LispObj lisp_nil;

#define GET_TCR (-1)		/* address of get_tcr() for callbacks */
#define TCR_COUNT (-2)		/* next tcr's tcr_id */
#define INTERRUPT_SIGNAL  (-3)  /* signal to use for PROCESS-INTERRUPT */
#define KERNEL_IMPORTS (-4)	/* some things we need to have imported for us. */
#define TCR_LOCK (-5)		/* lock on the TCR queue */
#define EMULATOR_REGISTERS (-6)	/* Where the 68K registers are kept. */
#define APPMAIN (-7)		/* application's (c-runtime) main() function */
#define SUBPRIMS_BASE (-8)	/* where the dynamic subprims wound up */
#define RET1VALN (-9)		/* magic multiple-values return address */
#define TCR_KEY (-10)     	/* tsd key for per-thread tcr */
#define bad_GC_LOCK (-11)       /* rwlock for GC */
#define EXCEPTION_LOCK (-12)	/* serialize exception handling */
#define GO_TAG_COUNTER (-13)
#define BLOCK_TAG_COUNTER (-14)
#define INTFLAG (-15)
#define GC_INHIBIT_COUNT (-16)
#define OS_TRAP_CALL (-17)
#define TB_TRAP_CALL (-18)
#define ALTIVEC_PRESENT (-19)   /* non-zero if AltiVec present. */
#define FWDNUM (-20)            /* fixnum: GC "forwarder" call count. */
#define GC_NUM (-21)            /* fixnum: GC call count. */
#define GCABLE_POINTERS (-22)   /* linked-list of weak macptrs. */
#define HEAP_START (-23)        /* start of lisp heap */
#define HEAP_END (-24)          /* end of lisp heap */
#define BAD_CURRENT_CS (-25)        /* area describing control-stack */
#define BAD_CURRENT_VS (-26)        /* area describing value-stack */
#define BAD_CURRENT_TS (-27)        /* area describing temp-stack */
#define BAD_CS_OVERFLOW_LIMIT (-28) /* value for control-stack overflow check */
#define ALL_AREAS (-29)         /* doubly-linked list of stack & heap areas */
#define LEXPR_RETURN (-30)      /* magic &lexpr cleanup code */
#define LEXPR_RETURN1V (-31)    /* single-value &lexpr cleanup code */
#define IN_GC (-32)             /* non-zero when lisp addresses may be invalid */
#define METERING_INFO (-33)     /* address of lisp_metering global */
#define DOH_HEAD (-34)          /* Homer ? */
#define SHORT_FLOAT_ZERO (-35)  /* low half of 1.0d0 */
#define DOUBLE_FLOAT_ONE (-36)  /* high half of 1.0d0 */
#define LISP_RETURN_HOOK (-37)	/* install lisp exception handling */
#define LISP_EXIT_HOOK (-38)	/* install foreign exception handling */
#define OLDEST_EPHEMERAL (-39)  /* doubleword address of oldest ephemeral object or 0 */
#define TENURED_AREA (-40)      /* the tenured area */
#define ERRNO (-41)             /* address of errno */
#define ARGV (-42)              /* pointer to &argv[0] */
#define HOST_PLATFORM (-43)	/* for platform-specific initialization */
#define BATCH_FLAG (-44)	/* -b arg */
#define BAD_FPSCR_SAVE (-45)	/* saved FPSCR for FFI */
#define BAD_FPSCR_SAVE_HIGH (-46)	/* high word of FP reg used to save FPSCR */
#define IMAGE_NAME (-47)	/* --image-name arg */
#define INITIAL_TCR (-48)	/* initial thread tcr */
#define READONLY_SECTION_END (-1020)
#define READONLY_SECTION_START (-1021)
#define STATIC_HEAP_NEXT (-1022) /* next static heap triplet in application or NULL */
#define STATIC_HEAP_END (-1023) /* end of (primary) static heap */
#define STATIC_HEAP_START (-1024) /* start of (primary) static heap */
#define SUBPRIMS_TARGET_0 (-512) /* absolute address of subprim 0 jump target */
#define SUBPRIMS_TARGET_N (-257) /* absolute address of subprim 255 jump target */


#define lisp_global(g) (((LispObj *) (nil_value-fulltag_nil))[(g)])
#define nrs_symbol(s) (((lispsymbol *) (nil_value+(8-fulltag_nil)+8))[(s)])

#define nrs_T 				(nrs_symbol(0))		/* t */
#define nrs_NILSYM			(nrs_symbol(1))		/* nil */
#define nrs_ERRDISP			(nrs_symbol(2))		/* %err-disp */
#define nrs_CMAIN			(nrs_symbol(3))		/* cmain */
#define nrs_EVAL			(nrs_symbol(4))		/* eval */
#define nrs_APPEVALFN			(nrs_symbol(5))		/* apply-evaluated-function */
#define nrs_ERROR			(nrs_symbol(6))		/* error */
#define nrs_DEFUN			(nrs_symbol(7))		/* %defun */
#define nrs_DEFVAR			(nrs_symbol(8))		/* %defvar */
#define nrs_DEFCONSTANT			(nrs_symbol(9))		/* %defconstant */
#define nrs_MACRO			(nrs_symbol(10))	/* %macro */
#define nrs_KERNELRESTART		(nrs_symbol(11))	/* %kernel-restart */
#define nrs_PACKAGE			(nrs_symbol(12))	/* *package* */
#define nrs_TOTAL_BYTES_FREED           (nrs_symbol(13))        /* *total-bytes-freed* */
#define nrs_KALLOWOTHERKEYS		(nrs_symbol(14))	/* :allow-other-keys */
#define nrs_TOPLCATCH			(nrs_symbol(15))	/* %toplevel-catch% */
#define nrs_TOPLFUNC			(nrs_symbol(16))	/* %toplevel-function% */
#define nrs_CALLBACKS			(nrs_symbol(17))	/* %pascal-functions% */
#define nrs_ALLMETEREDFUNS		(nrs_symbol(18))	/* *all-metered-functions* */
#define nrs_TOTAL_GC_MICROSECONDS       (nrs_symbol(19))        /* *total-gc-microseconds* */
#define nrs_BUILTIN_FUNCTIONS           (nrs_symbol(20))        /* %builtin-functions% */
#define nrs_UDF				(nrs_symbol(21))	/* %unbound-function% */
#define nrs_INIT_MISC			(nrs_symbol(22))        /* %init-misc% */
#define nrs_MACRO_CODE                  (nrs_symbol(23))        /* %macro-code% */
#define nrs_CLOSURE_CODE		(nrs_symbol(24))        /* %closure-code% */
#define nrs_NEW_GCABLE_PTR		(nrs_symbol(25))	/* %new-gcable-ptr */
#define nrs_GC_EVENT_STATUS_BITS	(nrs_symbol(26))	/* *gc-event-status-bits* */
#define nrs_POST_GC_HOOK		(nrs_symbol(27))	/* *post-gc-hook* */
#define nrs_HANDLERS			(nrs_symbol(28))	/* %handlers% */
#define nrs_ALL_PACKAGES		(nrs_symbol(29))	/* %all-packages% */
#define nrs_KEYWORD_PACKAGE		(nrs_symbol(30))	/* *keyword-package* */
#define nrs_FINALIZATION_ALIST		(nrs_symbol(31))	/* %finalization-alist% */
#define nrs_FOREIGN_THREAD_CONTROL      (nrs_symbol(32))        /* %foreign-thread-control */
#define num_nilreg_symbols 33
#define nilreg_symbols_end ((BytePtr) &(nrs_symbol(num_nilreg_symbols)))
#endif
