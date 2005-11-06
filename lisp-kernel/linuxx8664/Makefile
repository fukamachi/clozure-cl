#
#   Copyright (C) 2005 Clozure Associates
#   This file is part of OpenMCL.  
#
#   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
#   License , known as the LLGPL and distributed with OpenMCL as the
#   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
#   which is distributed with OpenMCL as the file "LGPL".  Where these
#   conflict, the preamble takes precedence.  
#
#   OpenMCL is referenced in the preamble as the "LIBRARY."
#
#   The LLGPL is also available online at
#   http://opensource.franz.com/preamble.html


VPATH = ../
RM = /bin/rm
AS = as
M4 = m4
ASFLAGS = --64
M4FLAGS = -DLINUX -DX86 -DX8664
CDEFINES = -DLINUX -D_REENTRANT -DX86 -DX8664
CDEBUG = -g
COPT = #-O2



.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -m64 -o $@

SPOBJ = pad.o x86-spjump.o x86-spentry.o x86-subprims.o
ASMOBJ = asmutils.o imports.o

COBJ  = pmcl-kernel.o gc.o bits.o  lisp-exceptions.o \
	image.o thread_manager.o lisp-debug.o 

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= $(COBJ) asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 x86-constants.s macros.s errors.s uuo.s \
	x86-constants64.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h x86-constants64.h


KSPOBJ = $(SPOBJ)
all:	../../xppccl64


OSLIBS = -ldl -lm -lpthread


../../xppccl64:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC) -m64 $(CDEBUG)  -Wl,--export-dynamic  -o $@ -T ./elf_x86_64.x $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../xppccl64

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../xppccl64
	strip -g ../../xppccl64
