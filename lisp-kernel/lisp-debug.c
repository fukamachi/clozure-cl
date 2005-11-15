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
#include "area.h"
#include "Threads.h"
#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>

#include <sys/socket.h>
#include <sys/stat.h>


typedef enum {
  debug_continue,		/* stay in the repl */
  debug_exit_success,		/* return 0 from lisp_Debugger */
  debug_exit_fail,		/* return non-zero from lisp_Debugger */
  debug_kill
} debug_command_return;

typedef debug_command_return (*debug_command) (ExceptionInformation *,
					       siginfo_t *,
					       int);

#define DEBUG_COMMAND_FLAG_REQUIRE_XP 1 /* function  */
#define DEBUG_COMMAND_FLAG_AUX_REGNO  (2 | DEBUG_COMMAND_FLAG_REQUIRE_XP)
#define DEBUG_COMMAND_FLAG_AUX_SPR (4 | DEBUG_COMMAND_FLAG_REQUIRE_XP)
#define DEBUG_COMMAND_REG_FLAGS 7
#define DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY 8
#define DEBUG_COMMAND_FLAG_EXCEPTION_REASON_ARG 16

typedef struct {
  debug_command f;
  char *help_text;
  unsigned flags;
  char *aux_prompt;
  int c;
} debug_command_entry;


extern
debug_command_entry debug_command_entries[];


#ifdef LINUX
#define fpurge __fpurge
#endif

int
readc()
{
  int c;
  while (1) {
    c = getchar();
    switch(c) {
    case '\n':
      continue;
    case EOF:
      if (ferror(stdin)) {
	if (errno == EINTR) {
	  continue;
	}
      }
      /* fall through */
    default:
      return c;
    }
  }
}

void
show_lisp_register(ExceptionInformation *xp, char *label, int r)
{

  LispObj val = xpGPR(xp, r);

  fprintf(stderr, "r%02d (%s) = %s\n", r, label, print_lisp_object(val));
}


void
describe_memfault(ExceptionInformation *xp, siginfo_t *info)
{
#ifdef PPC
  void *addr = (void *)xpDAR(xp);
  natural dsisr = xpDSISR(xp);

  fprintf(stderr, "%s operation to %s address 0x%lx\n",
	  dsisr & (1<<25) ? "Write" : "Read",
	  dsisr & (1<<27) ? "protected" : "unmapped",
	  addr);
#endif
}

#ifdef PPC
void
describe_ppc_illegal(ExceptionInformation *xp)
{
  pc where = xpPC(xp);
  opcode the_uuo = *where, instr2;
  Boolean described = false;

  if (IS_UUO(the_uuo)) {
    unsigned 
      minor = UUO_MINOR(the_uuo),
      rt = 0x1f & (the_uuo >> 21),
      ra = 0x1f & (the_uuo >> 16),
      rb = 0x1f & (the_uuo >> 11),
      errnum = 0x3ff & (the_uuo >> 16);

    switch(minor) {
    case UUO_INTERR:
      switch (errnum) {
      case error_udf_call:
        fprintf(stderr, "ERROR: undefined function call: %s\n",
                print_lisp_object(xpGPR(xp,fname)));
        described = true;
        break;
        
      default:
        fprintf(stderr, "ERROR: lisp error %d\n", errnum);
        described = true;
        break;
      }
      break;
      
    default:
      break;
    }
  }
  if (!described) {
    fprintf(stderr, "Illegal instruction (0x%08x) at 0x%lx\n",
            the_uuo, where);
  }
}
#endif

#ifdef PPC
void
describe_ppc_trap(ExceptionInformation *xp)
{
  pc where = xpPC(xp);
  opcode the_trap = *where, instr;
  int  err_arg1, err_arg2, ra, rs;
  char *name = NULL;
  Boolean identified = false;

  if ((the_trap & OP_MASK) == OP(major_opcode_TRI)) {
    /* TWI/TDI.  If the RA field is "nargs", that means that the
       instruction is either a number-of-args check or an
       event-poll.  Otherwise, the trap is some sort of
       typecheck. */

    if (RA_field(the_trap) == nargs) {
      switch (TO_field(the_trap)) {
      case TO_NE:
	if (xpGPR(xp, nargs) < D_field(the_trap)) {
	  fprintf(stderr, "Too few arguments (no opt/rest)\n");
	} else {
	  fprintf(stderr, "Too many arguments (no opt/rest)\n");
	}
	identified = true;
	break;
	
      case TO_GT:
	fprintf(stderr, "Event poll !\n");
	identified = true;
	break;
	
      case TO_HI:
	fprintf(stderr, "Too many arguments (with opt)\n");
	identified = true;
	break;
	
      case TO_LT:
	fprintf(stderr, "Too few arguments (with opt/rest/key)\n");
	identified = true;
	break;
	
      default:                /* some weird trap, not ours. */
	identified = false;
	break;
      }
    } else {
      /* A type or boundp trap of some sort. */
      switch (TO_field(the_trap)) {
      case TO_EQ:
	/* Boundp traps are of the form:
	   treqi rX,unbound
	   where some preceding instruction is of the form:
	   lwz/ld rX,symbol.value(rY).
	   The error message should try to say that rY is unbound. */
	
	if (D_field(the_trap) == unbound) {
#ifdef PPC64
	  instr = scan_for_instr(LD_instruction(RA_field(the_trap),
                                                unmasked_register,
                                                offsetof(lispsymbol,vcell)-fulltag_misc),
				 D_RT_IMM_MASK,
				 where);
#else
	  instr = scan_for_instr(LWZ_instruction(RA_field(the_trap),
						 unmasked_register,
						 offsetof(lispsymbol,vcell)-fulltag_misc),
				 D_RT_IMM_MASK,
				 where);
#endif
	  if (instr) {
	    ra = RA_field(instr);
	    if (lisp_reg_p(ra)) {
	      fprintf(stderr, "Unbound variable: %s\n",
		      print_lisp_object(xpGPR(xp,ra)));
	      identified = true;	
	    }
	  }
	}
	break;
	
      case TO_NE:
	/* A type check.  If the type (the immediate field of the trap
	   instruction) is a header type, an "lbz
	   rX,misc_header_offset(rY)" should precede it, in which case
	   we say that "rY is not of header type <type>."  If the type
	   is not a header type, then rX should have been set by a
	   preceding "clrlwi rX,rY,29/30".  In that case, scan
	   backwards for an RLWINM instruction that set rX and report
	   that rY isn't of the indicated type. */
	err_arg2 = D_field(the_trap);
	if (nodeheader_tag_p(err_arg2) ||
	    immheader_tag_p(err_arg2)) {
	  instr = scan_for_instr(LBZ_instruction(RA_field(the_trap),
						 unmasked_register,
						 misc_subtag_offset),
				 D_RT_IMM_MASK,
				 where);
	  if (instr) {
	    ra = RA_field(instr);
	    if (lisp_reg_p(ra)) {
	      fprintf(stderr, "value 0x%lX is not of the expected header type 0x%02X\n", xpGPR(xp, ra), err_arg2);
	      identified = true;
	    }
	  }
	} else {		
	  /* Not a header type, look for rlwinm whose RA field matches the_trap's */
	  instr = scan_for_instr((OP(major_opcode_RLWINM) | (the_trap & RA_MASK)),
				 (OP_MASK | RA_MASK),
				 where);
	  if (instr) {
	    rs = RS_field(instr);
	    if (lisp_reg_p(rs)) {
	      fprintf(stderr, "value 0x%lX is not of the expected type 0x%02X\n",
		      xpGPR(xp, rs), err_arg2);
	      identified = true;
	    }
	  }
	}
	break;
      }
    }
  } else {
    /* a "TW <to>,ra,rb" instruction."
       twltu sp,rN is stack-overflow on SP.
       twgeu rX,rY is subscript out-of-bounds, which was preceded
       by an "lwz rM,misc_header_offset(rN)" instruction.
       rM may or may not be the same as rY, but no other header
       would have been loaded before the trap. */
    switch (TO_field(the_trap)) {
    case TO_LO:
      if (RA_field(the_trap) == sp) {
	fprintf(stderr, "Stack overflow! Run away! Run away!\n");
	identified = true;
      }
      break;
      
    case (TO_HI|TO_EQ):
      instr = scan_for_instr(OP(major_opcode_LWZ) | (D_MASK & misc_header_offset),
			     (OP_MASK | D_MASK),
			     where);
      if (instr) {
	ra = RA_field(instr);
	if (lisp_reg_p(ra)) {
	  fprintf(stderr, "Bad index %d for vector %lX length %d\n",
		  unbox_fixnum(xpGPR(xp, RA_field(the_trap))),
		  xpGPR(xp, ra),
		  unbox_fixnum(xpGPR(xp, RB_field(the_trap))));
	  identified = true;
	}
      }
      break;
    }
  }

  if (!identified) {
    fprintf(stderr, "Unknown trap: 0x%08x\n", the_trap);
  }


}
#endif

debug_command_return
debug_lisp_registers(ExceptionInformation *xp, siginfo_t *info, int arg)
{
#ifdef PPC
  TCR *xpcontext = (TCR *)ptr_from_lispobj(xpGPR(xp, rcontext));

  fprintf(stderr, "rcontext = 0x%lX ", xpcontext);
  if (!active_tcr_p(xpcontext)) {
    fprintf(stderr, "(INVALID)\n");
  } else {
    fprintf(stderr, "\nnargs = %d\n", xpGPR(xp, nargs) >> fixnumshift);
    show_lisp_register(xp, "fn", fn);
    show_lisp_register(xp, "arg_z", arg_z);
    show_lisp_register(xp, "arg_y", arg_y);
    show_lisp_register(xp, "arg_x", arg_x);
    show_lisp_register(xp, "temp0", temp0);
    show_lisp_register(xp, "temp1/next_method_context", temp1);
    show_lisp_register(xp, "temp2/nfn", temp2);
    show_lisp_register(xp, "temp3/fname", temp3);
    /*    show_lisp_register(xp, "new_fn", new_fn); */
    show_lisp_register(xp, "save0", save0);
    show_lisp_register(xp, "save1", save1);
    show_lisp_register(xp, "save2", save2);
    show_lisp_register(xp, "save3", save3);
    show_lisp_register(xp, "save4", save4);
    show_lisp_register(xp, "save5", save5);
    show_lisp_register(xp, "save6", save6);
    show_lisp_register(xp, "save7", save7);
  }
#endif

  return debug_continue;
}

debug_command_return
debug_advance_pc(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  adjust_exception_pc(xp,4);
  return debug_continue;
}

debug_command_return
debug_identify_exception(ExceptionInformation *xp, siginfo_t *info, int arg)
{
#ifdef PPC
  pc program_counter = xpPC(xp);
  opcode instruction = 0;

  switch (arg) {
  case SIGILL:
  case SIGTRAP:
    instruction = *program_counter;
    if (major_opcode_p(instruction, major_opcode_TRI) ||
	X_opcode_p(instruction,major_opcode_X31,minor_opcode_TR)) {
      describe_ppc_trap(xp);
    } else {
      describe_ppc_illegal(xp);
    }
    break;
  case SIGSEGV:
  case SIGBUS:
    describe_memfault(xp, info);
    break;
  default:
    break;
  }
#endif
  return debug_continue;
}

char *
debug_get_string_value(char *prompt)
{
  static char buf[128];
  char *p;

  do {
    fpurge(stdin);
    fprintf(stderr, "\n %s :",prompt);
    buf[0] = 0;
    fgets(buf, sizeof(buf)-1, stdin);
  } while (0);
  p = strchr(buf, '\n');
  if (p) {
    *p = 0;
    return buf;
  }
  return NULL;
}

unsigned
debug_get_u32_value(char *prompt)
{
  char s[32];
  int n;
  unsigned val;

  do {
    fpurge(stdin);
    fprintf(stderr, "\n  %s :", prompt);
    fgets(s, 24, stdin);
    n = sscanf(s, "%i", &val);
  } while (n != 1);
  return val;
}

unsigned
debug_get_u5_value(char *prompt)
{
  char s[32];
  int n;
  unsigned val;

  do {
    fpurge(stdin);
    fprintf(stderr, "\n  %s :", prompt);
    fgets(s, 24, stdin);
    n = sscanf(s, "%i", &val);
  } while ((n != 1) || (val > 31));
  return val;
}

debug_command_return
debug_show_symbol(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  char *pname = debug_get_string_value("symbol name");
  
  if (pname != NULL) {
    plsym(xp, pname);
  }
  return debug_continue;
}

      

debug_command_return
debug_set_gpr(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  char buf[32];
  unsigned val;

  sprintf(buf, "value for GPR %d", arg);
  val = debug_get_u32_value(buf);
  set_xpGPR(xp, arg, val);
  return debug_continue;
}


debug_command_return
debug_show_registers(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  int a, b, c, d, i;

#ifdef PPC64
  for (a = 0, b = 16; a < 16; a++, b++) {
    fprintf(stderr,"r%02d = 0x%016lX    r%02d = 0x%016lX\n",
	    a, xpGPR(xp, a),
	    b, xpGPR(xp, b));
  }
  
  fprintf(stderr, "\n PC = 0x%016lX     LR = 0x%016lX\n",
          xpPC(xp), xpLR(xp));
  fprintf(stderr, "CTR = 0x%016lX    CCR = 0x%08X\n",
          xpCTR(xp), xpCCR(xp));
  fprintf(stderr, "XER = 0x%08X            MSR = 0x%016lX\n",
          xpXER(xp), xpMSR(xp));
  fprintf(stderr,"DAR = 0x%016lX  DSISR = 0x%08X\n",
	  xpDAR(xp), xpDSISR(xp));
#else
  for (a = 0, b = 8, c = 16, d = 24; a < 8; a++, b++, c++, d++) {
    fprintf(stderr,"r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X\n",
	    a, xpGPR(xp, a),
	    b, xpGPR(xp, b),
	    c, xpGPR(xp, c),
	    d, xpGPR(xp, d));
  }
  fprintf(stderr, "\n PC = 0x%08X   LR = 0x%08X  CTR = 0x%08X  CCR = 0x%08X\n",
	  xpPC(xp), xpLR(xp), xpCTR(xp), xpCCR(xp));
  fprintf(stderr, "XER = 0x%08X  MSR = 0x%08X  DAR = 0x%08X  DSISR = 0x%08X\n",
	  xpXER(xp), xpMSR(xp), xpDAR(xp), xpDSISR(xp));
#endif
  return debug_continue;
}

debug_command_return
debug_show_fpu(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  double *dp, d;
  int *np, n, i;
#ifdef PPC
  dp = xpFPRvector(xp);
  np = (int *) dp;
  
  for (i = 0; i < 32; i++) {
    fprintf(stderr, "f%02d : 0x%08X%08X (%f)\n", i,  *np++, *np++, *dp++);
  }
  fprintf(stderr, "FPSCR = %08X\n", xpFPSCR(xp));
#endif
  return debug_continue;
}

debug_command_return
debug_kill_process(ExceptionInformation *xp, siginfo_t *info, int arg) {
  return debug_kill;
}

debug_command_return
debug_win(ExceptionInformation *xp, siginfo_t *info, int arg) {
  return debug_exit_success;
}

debug_command_return
debug_lose(ExceptionInformation *xp, siginfo_t *info, int arg) {
  return debug_exit_fail;
}

debug_command_return
debug_help(ExceptionInformation *xp, siginfo_t *info, int arg) {
  debug_command_entry *entry;

  for (entry = debug_command_entries; entry->f; entry++) {
    /* If we have an XP or don't need one, call the function */
    if (xp || !(entry->flags & DEBUG_COMMAND_FLAG_REQUIRE_XP)) {
      fprintf(stderr, "(%c)  %s\n", entry->c, entry->help_text);
    }
  }
  return debug_continue;
}
	      

  

debug_command_return
debug_backtrace(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  extern LispObj current_stack_pointer();
  extern void plbt_sp(LispObj);
  extern void plbt(ExceptionInformation *);

  if (xp) {
    plbt(xp);
  } else {
    plbt_sp(current_stack_pointer());
  }
  return debug_continue;
}

debug_command_return
debug_thread_reset(ExceptionInformation *xp, siginfo_t *info, int arg)
{
  reset_lisp_process(xp);
  return debug_exit_success;
}


debug_command_entry debug_command_entries[] = 
{
  {debug_set_gpr,
   "Set specified GPR to new value",
   DEBUG_COMMAND_FLAG_AUX_REGNO,
   "GPR to set (0-31) ?",
   'G'},
  {debug_advance_pc,
   "Advance the program counter by one instruction (use with caution!)",
   DEBUG_COMMAND_FLAG_REQUIRE_XP | DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY,
   NULL,
   'A'},
  {debug_identify_exception,
   "Describe the current exception in greater detail",
   DEBUG_COMMAND_FLAG_REQUIRE_XP | DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY |
   DEBUG_COMMAND_FLAG_EXCEPTION_REASON_ARG,
   NULL,
   'D'},
  {debug_show_registers, 
   "Show raw GPR/SPR register values", 
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'R'},
  {debug_lisp_registers,
   "Show Lisp values of tagged registers",
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'L'},
  {debug_show_fpu,
   "Show FPU registers",
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'F'},
  {debug_show_symbol,
   "Find and describe symbol matching specified name",
   0,
   NULL,
   'S'},
  {debug_backtrace,
   "Show backtrace",
   0,
   NULL,
   'B'},
  {debug_win,
   "Exit from this debugger, asserting that any exception was handled",
   0,
   NULL,
   'X'},
#ifdef DARWIN
  {debug_lose,
   "Propagate the exception to another handler (debugger or OS)",
   DEBUG_COMMAND_FLAG_REQUIRE_XP | DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY,
   NULL,
   'P'},
#endif
#if 0
  {debug_thread_reset,
   "Reset current thread (as if in response to stack overflow)",
   DEBUG_COMMAND_FLAG_REQUIRE_XP,
   NULL,
   'T'},
#endif
  {debug_kill_process,
   "Kill OpenMCL process",
   0,
   NULL,
   'K'},
  {debug_help,
   "Show this help",
   0,
   NULL,
   '?'},
  /* end-of-table */
  {NULL,
   NULL,
   0,
   NULL,
   0}
};

debug_command_return
apply_debug_command(ExceptionInformation *xp, int c, siginfo_t *info, int why) 
{
  if (c == EOF) {
    return debug_kill;
  } else {
    debug_command_entry *entry;
    debug_command f;
    c = toupper(c);

    for (entry = debug_command_entries; f = entry->f; entry++) {
      if (toupper(entry->c) == c) {
	/* If we have an XP or don't need one, call the function */
	if ((xp || !(entry->flags & DEBUG_COMMAND_FLAG_REQUIRE_XP)) &&
	    ((why > debug_entry_exception) || 
	     !(entry->flags & DEBUG_COMMAND_FLAG_EXCEPTION_ENTRY_ONLY))) {
	  int arg = 0;
	  if ((entry->flags & DEBUG_COMMAND_REG_FLAGS)
	      == DEBUG_COMMAND_FLAG_AUX_REGNO) {
	    arg = debug_get_u5_value("register number");
	  }
	  if (entry->flags & DEBUG_COMMAND_FLAG_EXCEPTION_REASON_ARG) {
	    arg = why;
	  }
	  return (f)(xp, info, arg);
	}
	break;
      }
    }
    return debug_continue;
  }
}

debug_identify_function(ExceptionInformation *xp, siginfo_t *info) 
{
#ifdef PPC
  if (xp) {
    if (active_tcr_p((TCR *)(ptr_from_lispobj(xpGPR(xp, rcontext))))) {
      LispObj f = xpGPR(xp, fn), codev;
      pc where = xpPC(xp);
      
      if (!(codev = register_codevector_contains_pc(f, where))) {
        f = xpGPR(xp, nfn);
        codev =  register_codevector_contains_pc(f, where);
      }
      if (codev) {
        fprintf(stderr, " While executing: %s\n", print_lisp_object(f));
      }
    } else {
      fprintf(stderr, " In foreign code at address 0x%08lx\n", xpPC(xp));
    }
  }
#endif
}

extern pid_t main_thread_pid;

OSStatus
lisp_Debugger(ExceptionInformation *xp, 
	      siginfo_t *info, 
	      int why, 
	      char *message, 
	      ...)
{
  va_list args;
  debug_command_return state = debug_continue;

  if (threads_initialized) {
    suspend_other_threads();
  }

  va_start(args,message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
  if (lisp_global(BATCH_FLAG)) {
    terminate_lisp();
  }
  if (xp) {
    if (why > debug_entry_exception) {
      debug_identify_exception(xp, info, why);
    }
    debug_identify_function(xp, info);
  }
  fprintf(stderr, "? for help\n");
  while (state == debug_continue) {
    fprintf(stderr, "[%d] OpenMCL kernel debugger: ", main_thread_pid);
    state = apply_debug_command(xp, readc(), info, why);
  }
  switch (state) {
  case debug_exit_success:
    if (threads_initialized) {
      resume_other_threads();
    }
    return 0;
  case debug_exit_fail:
    if (threads_initialized) {
      resume_other_threads();
    }
    return -1;
  case debug_kill:
    terminate_lisp();
  }
}

