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
ASFLAGS = -mregnames -mppc32 -maltivec
M4FLAGS = -DLINUX
CDEFINES = -DLINUX -D_REENTRANT
CDEBUG = -g
COPT = -O2

# The only version of GCC I have that supports both ppc32 and ppc64
# compilation uses the -m32 option to target ppc32.  This may not be
# definitive; there seem to be a bewildering array of similar options
# in other GCC versions.  It's assumed here that if "-m32" is recognized,
# it's required as well.

PPC32 = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-m32 ") && /bin/echo "-m32")

# Likewise, some versions of GAS may need a "-a32" flag, to force the
# output file to be 32-bit compatible.

A32 = $(shell ($(AS) --help -v 2>&1 | grep -q -e "-a32") && /bin/echo "-a32")

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(A32) $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) $(PPC32) -o $@

SPOBJ = pad.o spjump.o spentry.o subprims.o
ASMOBJ = asmutils.o imports.o

COBJ  = pmcl-kernel.o gc.o bits.o  lisp-exceptions.o \
	image.o thread_manager.o lisp-debug.o 

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= $(COBJ) asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 constants.s macros.s errors.s uuo.s

CHEADERS = area.h bits.h constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ = $(SPOBJ)
all:	../../ppccl


# No:

# KSPOBJ=
# all:	../../ppccl ../../subprims.so

OSLIBS = -ldl -lm -lpthread


../../ppccl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC) $(PPC32) $(CDEBUG)  -Wl,--export-dynamic  -o $@ -T ./elf32ppclinux.x $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../ppccl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../ppccl
	strip -g ../../ppccl
