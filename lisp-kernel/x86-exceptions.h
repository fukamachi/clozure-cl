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

typedef u8_t opcode, *pc;

#ifdef LINUX
#ifdef X8664
#define xpGPRvector(x) ((natural *)(&((x)->uc_mcontext.gregs)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (xpGPR(x,Iip))
#define xpMMXreg(x,n)  *((natural *)(&((x)->uc_mcontext.fpregs->_st[n])))
#endif
#endif

#ifdef DARWIN
#define DARWIN_USE_PSEUDO_SIGRETURN 1
#include <sys/syscall.h>
#define DarwinSigReturn(context) syscall(SYS_sigreturn,context)
#ifdef X8664
#define xpGPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__ss.__rax)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (xpGPR(x,Iip))
#define xpFPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__fs.__fpu_stmm0)))
#define xpMMXreg(x,n)  (xpFPRvector(x)[gprno])
#endif
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

pthread_mutex_t *mach_exception_lock;

#endif

#ifdef FREEBSD
#ifdef X8664
#include <machine/fpu.h>
#define xpGPRvector(x) ((natural *)(&((x)->uc_mcontext)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) xpGPR(x,Iip)
#define xpMMXreg(x,n) *((natural *)(&(((struct savefpu *)(&(x)->uc_mcontext.mc_fpstate))->sv_fp[n])))
#endif
#endif

#ifdef SOLARIS
#ifdef X8664
#define xpGPRvector(x) ((x)->uc_mcontext.gregs)
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) xpGPR(x,Iip)
#define xpMMXreg(x,n)  *((natural *)(&(x)->uc_mcontext.fpregs.fp_reg_set.fpchip_state.st[n]))
#endif
#endif


#ifdef DARWIN
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGEMT
#endif
#ifdef LINUX
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGPWR
#endif
#ifdef FREEBSD
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGEMT
#endif
#ifdef SOLARIS
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGEMT
#endif

#ifdef USE_SIGALTSTACK
void setup_sigaltstack(area *);
void switch_to_foreign_stack(void*, ...);
#endif

#define INTN_OPCODE 0xcd

#define UUO_GC_TRAP    0xc4
#define UUO_ALLOC_TRAP 0xc5
#define UUO_DEBUG_TRAP 0xca
#define UUO_DEBUG_TRAP_WITH_STRING 0xcd

#define XUUO_OPCODE_0 0x0f
#define XUUO_OPCODE_1 0x0b

#define XUUO_TLB_TOO_SMALL 1
#define XUUO_INTERRUPT_NOW 2

void
pc_luser_xp(ExceptionInformation*, TCR*, signed_natural*);


typedef enum {
  ID_unrecognized_alloc_instruction,
  ID_load_allocptr_reg_from_tcr_save_allocptr_instruction,
  ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction,
  ID_branch_around_alloc_trap_instruction,
  ID_alloc_trap_instruction,
  ID_set_allocptr_header_instruction,
  ID_clear_tcr_save_allocptr_tag_instruction
} alloc_instruction_id;

#ifdef LINUX
#define SIGNUM_FOR_INTN_TRAP SIGSEGV
#define IS_MAYBE_INT_TRAP(info,xp) (((info->si_code) &0x7f) == 0)
#define SIGRETURN(context)
#endif

#ifdef FREEBSD
extern void freebsd_sigreturn(ExceptionInformation *);
#define SIGNUM_FOR_INTN_TRAP SIGBUS
#define IS_MAYBE_INT_TRAP(info,xp) (xp->uc_mcontext.mc_trapno == T_PROTFLT)
#define SIGRETURN(context) freebsd_sigreturn(context)
#endif

#ifdef DARWIN
#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Not really, but our Mach handler fakes that */
#define IS_MAYBE_INT_TRAP(info,xp) (info->si_code == EXC_I386_GPFLT)
/* The x86 version of sigreturn just needs the context argument; the
   hidden, magic "flavor" argument that sigtramp uses is ignored. */
#define SIGRETURN(context) syscall(SYS_sigreturn,context)
#endif

/* Please go away. */
#ifdef DARWIN_GS_HACK
extern Boolean ensure_gs_pthread(void);
extern void set_gs_address(void *);
#endif
