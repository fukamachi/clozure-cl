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

#ifndef __LISP_EXCEPTIONS_H__
#define __LISP_EXCEPTIONS_H__ 1


#include <stdlib.h>
#include "memprotect.h"
#include "gc.h"

typedef enum {
  kDebugger,
  kContinue,
  kExit
} ErrAction;



void
zero_page(BytePtr);

void
zero_heap_segment(BytePtr);

extern protected_area_ptr AllProtectedAreas;

protected_area_ptr find_protected_area(BytePtr);

OSStatus
handle_protection_violation(ExceptionInformationPowerPC *, siginfo_t *);

protected_area_ptr 
new_protected_area(BytePtr, BytePtr, lisp_protection_kind, unsigned, Boolean);

void
unprotect_area_prefix(protected_area_ptr, size_t);

void
protect_area_prefix(protected_area_ptr, size_t);

void
protect_area(protected_area_ptr);


void
resize_dynamic_heap(BytePtr, unsigned);

OSStatus
PMCL_exception_handler(int, ExceptionInformationPowerPC *, TCR *, siginfo_t *);

ErrAction
error_action( void );

void
install_pmcl_exception_handlers();

void
unprotect_all_areas(void);

void
exception_cleanup(void);

void
exception_init();

#define UUO_MASK 0xfc00000f
#define IS_UUO(i) (((i) & UUO_MASK) == 0xb)
/* If an instruction is a UUO, the minor opcode is in bits 21:27 */
#define UUO_MINOR(u) (((u) >> 4) & 0x7f)


typedef unsigned opcode, *pc;

#ifdef LINUX
/*
  Different (recent) versions of glibc disagree about how
  a ucontext is laid out (and about what an mcontext is.)
  There's something like a pointer to a pt_regs structure
  in the 12th word in both cases.  (Yes, this is an extremely
  ugly hack; it would be better to conditionalize on the values
  of GLIBC_VERSION/GLIBC_MINOR , but the discrepancy exists
  in various flavors of glibc 2.3.)
*/
#define XP_PTREGS(x) (((struct pt_regs **)(x))[12])
#define xpGPRvector(x) (XP_PTREGS(x)->gpr)
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (UInt32)(new)
#define xpPC(x) (*((pc*)(&(XP_PTREGS(x)->nip))))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(XP_PTREGS(x)->link))))
#define xpCTR(x) (*(pc*)(&(XP_PTREGS(x)->ctr)))
#define xpXER(x) (XP_PTREGS(x)->xer)
#define xpCCR(x) (XP_PTREGS(x)->ccr)
#define xpMSR(x) (XP_PTREGS(x)->msr)
#define xpDSISR(x) (XP_PTREGS(x)->dsisr)
#define xpDAR(x) (XP_PTREGS(x)->dar)
#define xpTRAP(x) (XP_PTREGS(x)->trap)
#define xpFPSCR(x) (XP_PTREGS(x)->gpr[PT_FPSCR])
#define xpFPRvector(x) ((double *)(&(XP_PTREGS(x)->gpr[PT_FPR0])))
#define xpFPR(x,fprno) (xpFPRvector(x)[fprno])
/* 
   Work around a Darwin G5 bug (present in OSX 10.2.7, 10.2.8, and later
   versions.  See below for details.
*/
#define DarwinSigReturn(context)
#endif

#ifdef DARWIN
#define xpGPRvector(x) (&(x->uc_mcontext->ss.r0))
#define xpGPR(x,gprno) ((xpGPRvector(x))[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (UInt32)(new)
#define xpPC(x) (*((pc*) &(x->uc_mcontext->ss.srr0)))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(x->uc_mcontext->ss.lr))))
#define xpCTR(x) (*(pc*)(&(x->uc_mcontext->ss.ctr)))
#define xpXER(x) (x->uc_mcontext->ss.xer)
#define xpCCR(x) (x->uc_mcontext->ss.cr)
#define xpMSR(x) (x->uc_mcontext->ss.srr1)
#define xpDSISR(x) (x->uc_mcontext->es.dsisr)
#define xpDAR(x) (x->uc_mcontext->es.dar)
#define xpTRAP(x) (x->uc_mcontext->es.exception)
#define xpFPSCR(x) (x->uc_mcontext->fs.fpscr)
#define xpFPRvector(x) (x->uc_mcontext->fs.fpregs)
#define xpFPR(x,fprno) (xpFPRvector(x)[fprno])
/* There's a bug in some versions of Darwin on G5 processors: FP context
   isn't restored correctly on exit from a signal handler if the integer
   context appears to be unmodified (the 64-bit context isn't set up
   correctly by the kernel: only the first N bytes are copied out of
   the kernel, where N = size of 32-bit context.
*/
#define DarwinSigReturn(x) syscall(103,x)
#endif

OSStatus
handle_uuo(ExceptionInformationPowerPC *, opcode, pc);


/* 
  Unconditional traps (tw, twi instructions) are used by the
  operating system.  We use conditional traps.
  */

int
is_conditional_trap(opcode);

#define kNameBufLen 256
#define TRAP_LOOKUP_TRIES 5   /* # instrs to scan before trap instr */

void
callback_for_trap (LispObj, ExceptionInformationPowerPC *, unsigned, unsigned, unsigned, unsigned);

unsigned
register_codevector_contains_pc (unsigned, unsigned);

void
callback_to_lisp (LispObj, ExceptionInformationPowerPC *, unsigned, unsigned, unsigned, unsigned, unsigned);

OSStatus
handle_trap(ExceptionInformationPowerPC *, opcode, pc);

unsigned
scan_for_instr( unsigned, unsigned, pc );

size_t
exception_fn_name( ExceptionInformationPowerPC *, int, char *, size_t );

size_t
symbol_name( unsigned, char *, size_t );

void
non_fatal_error( char * );


#define UUO_INTERR (11)
#define UUO_INTCERR (12)
#define UUO_INTERR2 (13)
#define UUO_INTCERR2 (14)

#define UUO_FPUX_BINOP (22)
#define UUO_ZERO_FPSCR (25)

OSStatus
box_signed_integer(ExceptionInformationPowerPC *, unsigned, unsigned);

OSStatus
box_unsigned_integer(ExceptionInformationPowerPC *, unsigned, unsigned);

OSStatus
fix_fixnum_overflow(ExceptionInformationPowerPC *, unsigned, unsigned);

OSStatus
add_fixnums(ExceptionInformationPowerPC *, unsigned, unsigned, unsigned);

OSStatus
sub_fixnums(ExceptionInformationPowerPC *, unsigned, unsigned, unsigned);

OSStatus
handle_error(ExceptionInformationPowerPC *, unsigned, unsigned, unsigned, unsigned);

void
adjust_exception_pc(ExceptionInformationPowerPC *, int);


/* PPC instructions */
#define match_instr(instr, mask, target)   (((instr) & (mask)) == (target))
#define RS_field(instr)  (((instr) >> 21) & 0x1f)
#define RT_field(instr)  (RS_field(instr))
#define TO_field(instr)  (RT_field(instr))
#define RA_field(instr)  (((instr) >> 16) & 0x1f)
#define RB_field(instr)  (((instr) >> 11) & 0x1f)
#define D_field(instr)   ((instr) & 0xffff)

#define RT(val) ((val & 0xf) << 21)
#define RS(val) (RT(val))
#define RA(val) ((val & 0xf) << 16)
#define RB(val) ((val & 0xf) << 11)
#define D(val) (val & 0xffff)

#define RS_MASK RS(-1)
#define RT_MASK RS_MASK
#define TO_MASK RS_MASK
#define RA_MASK RA(-1)
#define RB_MASK RB(-1)
#define D_MASK  D(-1)



#define OP(x) (((x) & 0x3f) << 26)
#define OP_MASK OP (0x3f)

/* Main opcode + TO field of a D form instruction */
#define OPTO(x,to) (OP(x) | (((to) & 0x1f) << 21))
#define OPTO_MASK (OP_MASK | TO_MASK)
#define OPTORA(x,to,ra) (OPTO(x,to) | ((ra) << 16))
#define OPTORA_MASK (OP_TO_MASK | RA_MASK)




/* An X form instruction.  */
#define X(op, xop) (OP (op) | (((xop) & 0x3ff) << 1))

/* An X form instruction with the RC bit specified.  */
#define XRC(op, xop, rc) (X ((op), (xop)) | ((rc) & 1))

/* The mask for an X form instruction.  */
#define X_MASK XRC(0x3f, 0x3ff, 1)

/* An XO form instruction */
#define XO(op, xop, oe, rc) \
  (OP (op) | ((((unsigned long)(xop)) & 0x1ff) << 1) | ((((unsigned long)(oe)) & 1) << 10) | (((unsigned long)(rc)) & 1))
#define XO_MASK XO (0x3f, 0x1ff, 1, 1)



/* The bits in the TO field of a TW or TWI instruction */
#define TO_LT (1<<4)		/* signed < */
#define TO_GT (1<<3)		/* signed > */
#define TO_EQ (1<<2)		/* = */
#define TO_LO (1<<1)		/* unsigned < */
#define TO_HI (1<<0)		/* unsigned > */
#define TO_NE (TO_LT|TO_GT)

/* True if major opcode of "instr" is "op" */
#define major_opcode_p(instr, op) match_instr((instr),OP_MASK,OP(op))

/* True if "instr" is an X form instruction with major opcode "major"
   and minor opcode "minor" */
#define X_opcode_p(instr,major,minor) match_instr((instr),X_MASK,X(major,minor))

#define major_opcode_TWI 3
#define major_opcode_ADDI 14
#define major_opcode_RLWINM 21
#define major_opcode_X31 31		/* an "X" form instruction; see minor opcode */
#define major_opcode_LWZ 32
#define major_opcode_LBZ 34
#define major_opcode_STW 36
#define major_opcode_FPU_SINGLE 59
#define major_opcode_FPU_DOUBLE 63

#define minor_opcode_TW 4
#define minor_opcode_SUBF 40
#define minor_opcode_STWX 151


#define D_instruction(major,rt,ra,imm) (OP(major)|((rt)<<21)|((ra)<<16)|((imm)&D_MASK))
#define TWI_instruction(rt,ra,imm)     D_instruction(major_opcode_TWI,rt,ra,imm)
#define LBZ_instruction(rt,ra,imm)     D_instruction(major_opcode_LBZ,rt,ra,imm)
#define LWZ_instruction(rt,ra,imm)     D_instruction(major_opcode_LWZ,rt,ra,imm)

#define D_RT_IMM_MASK                  (OP_MASK|RT_MASK|D_MASK)
#define D_RA_IMM_MASK                  (OP_MASK|RA_MASK|D_MASK)

#define X_instruction(major,minor,rt,ra,rb) (X(major,minor)|((rt)<<21)|((ra)<<16)|((rb)<<11))

#define unmasked_register              0

#define LISP_BREAK_INSTRUCTION 0x7F810808

/* twllt allocptr,allocbase */
#define ALLOC_TRAP_INSTRUCTION 0x7C495008

/* twlgei allocptr,0 */
#define GC_TRAP_INSTRUCTION 0x0CA90000

/* clrrwi allocptr,allocptr,3 */
#define UNTAG_ALLOCPTR_INSTRUCTION 0x55290038

/* stw rX,misc_header_offset(allocptr) */
#define STORE_HEADER_ALLOCPTR_INSTRUCTION 0x9009FFFA
#define STORE_HEADER_ALLOCPTR_MASK D_RA_IMM_MASK

/* stw rX,cons.cXr(allocptr) */
#define STORE_CAR_ALLOCPTR_INSTRUCTION 0x90090003
#define STORE_CDR_ALLOCPTR_INSTRUCTION 0x9009ffff
#define STORE_CXR_ALLOCPTR_MASK D_RA_IMM_MASK

void Bug(ExceptionInformationPowerPC *, const char *format_string, ...);
int gc_from_xp(ExceptionInformationPowerPC *);
int purify_from_xp(ExceptionInformationPowerPC *);
int impurify_from_xp(ExceptionInformationPowerPC *);

typedef char* vector_buf;

void put_altivec_registers(vector_buf);
void get_altivec_registers(vector_buf);

void pc_luser_xp(ExceptionInformation *, TCR *);

int altivec_available;

/* Yet another way to look at a branch instruction ... */
typedef union {
  struct {unsigned op:6, li:24, aa:1, lk:1;} b;
  unsigned opcode;
} branch_instruction;



void
Fatal(StringPtr, StringPtr);


Ptr
allocate(unsigned);

Ptr
zalloc(unsigned);

void
deallocate(Ptr);

  /* Enable exceptions (at least, enable another thread's attempts to
     suspend this one) by restoring the signal mask.
  */


#define ALLOW_EXCEPTIONS(context) \
  pthread_sigmask(SIG_SETMASK, &context->uc_sigmask, NULL);

#ifdef DARWIN
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGEMT
#endif
#ifdef LINUX
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGPWR
#endif


#ifdef LINUX
register void *current_r2 __asm__("r2");
#endif

#endif
