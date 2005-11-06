#
#   Copyright (C) 1994-2001 Digitool, Inc
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
# Versions of GNU as >= 2.9.1 all seem to work
# AS = gas-2.9.1
AS = as
M4 = m4
ASFLAGS = -mregnames -mppc64 -a64 -maltivec
M4FLAGS = -DLINUX -DPPC -DPPC64
CDEFINES = -DLINUX -D_REENTRANT -DPPC -DPPC64
CDEBUG = -g
COPT = #-O2



.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -m64 -o $@

SPOBJ = pad.o ppc-spjump.o ppc-spentry.o ppc-subprims.o
ASMOBJ = ppc-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc.o bits.o  lisp-exceptions.o \
	image.o thread_manager.o lisp-debug.o 

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= $(COBJ) ppc-asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 ppc-constants.s macros.s errors.s uuo.s ppc-constants64.s

CHEADERS = area.h bits.h ppc-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h ppc-constants64.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ = $(SPOBJ)
all:	../../ppccl64


# No:

# KSPOBJ=
# all:	../../ppccl64 ../../subprims.so

OSLIBS = -ldl -lm -lpthread


../../ppccl64:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC) -m64 $(CDEBUG)  -Wl,--export-dynamic  -o $@ -T ./elf64ppc.x $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../ppccl64

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../ppccl64
	strip -g ../../ppccl64
