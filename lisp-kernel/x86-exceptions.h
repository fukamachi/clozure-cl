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
#define xpMMXreg(x,n)  *((natural *)&((x)->uc_mcontext.fpregs->_st[n]))
#endif
#endif

#ifdef FREEBSD
#ifdef X8664
#define xpGPRvector(x) ((natural *)(&((x)->uc_mcontext)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) xpGPR(x,Iip)
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

#define XUUO_OPCODE_0 0x0f
#define XUUO_OPCODE_1 0x0b

#define XUUO_TLB_TOO_SMALL 1
#define XUUO_INTERRUPT_NOW 2

void
pc_luser_xp(ExceptionInformation*, TCR*, Boolean);

