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


# For versions of GCC prior to 3.3, the option "-traditional-cpp" meant
# "don't use precompiled headers", which was good advice since they didn't
# work too well.  Beginning with GCC 3.3, the "-traditional-cpp" means 
# "use a broken preprocessor", which is (in a sense) the opposite of what
# it used to mean.

# Try to determine the version of GCC in use.  Invoke gcc with the
# -v flag, and look for a line containing the phrase "specs from" in
# the output.  Use sed to extract the full pathname of ths specs file
# printed in that line, then strip off the trailing "/specs".
gccdir = $(shell $(CC) -v 2>&1 | grep "specs from" | sed -e 's/.*from //' -e 's|/specs||')
# $(gccdir) is set to the directory containing the specs file, without the
# trailing slash.  The make intrinsic 'notdir' will strip a leading directory
# prefix from that pathname, leaving us with a string that should match
# the gcc version number
gccversion:=$(notdir $(gccdir))
oldgcc:=$(shell expr $(gccversion) "<" "3.3")
ifeq ($(oldgcc),1)
BROKEN_PREPROCESSOR_WORKAROUND = -traditional-cpp
endif

MDYNAMIC_NO_PIC = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-mdynamic-no-pic") && /bin/echo "-mdynamic-no-pic")

OPENMCL_MAJOR_VERSION=0
OPENMCL_MINOR_VERSION=14

VPATH = ..
RM = /bin/rm
LD = ld
LDFLAGS = -arch ppc -dynamic  -o $@ -e start -seg1addr 0x00003000 -sectalign __TEXT __text 0x1000 -pagezero_size 0x3000
# -stack_addr 0xC0000000 -stack_size 0x00100000
AS = as
M4 = gm4
M4FLAGS = -DDARWIN
ASFLAGS = -force_cpusubtype_ALL
CDEFINES = -DDARWIN $(BROKEN_PREPROCESSOR_WORKAROUND) -DOPENMCL_MAJOR_VERSION=$(OPENMCL_MAJOR_VERSION) -DOPENMCL_MINOR_VERSION=$(OPENMCL_MINOR_VERSION)
CDEBUG = -g
COPT = #-O2

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) $(MDYNAMIC_NO_PIC) -o $@

SPOBJ = spjump.o spentry.o  subprims.o 
ASMOBJ = asmutils.o imports.o

COBJ  = pmcl-kernel.o gc.o bits.o  lisp-exceptions.o \
	thread_manager.o lisp-debug.o image.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= imports.o $(COBJ) asmutils.o 

SPINC =	lisp.s m4macros.m4 constants.s macros.s errors.s uuo.s

CHEADERS = area.h bits.h constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ= $(SPOBJ)
all:	../../dppccl


# No:

# KSPOBJ=
# all:	../../ppccl ../../subprims.so

OSLIBS = -lcrt1.o -lcc_dynamic -lSystem


../../dppccl:	 $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(LD)  $(LDFLAGS) $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)   $(OSLIBS)

# Can leave this unset when compiling on/for Jaguar
SAMPLER_ARCH_DEFINES=-DUSE_MACH

../../darwin_sampler.so: sampler.c
	$(CC) -bundle  $< $(CDEFINES) $(SAMPLER_ARCH_DEFINES) $(CDEBUG) $(COPT) -o $@

# This will fail to compile on pre-Jaguar systems
../../jaguar_sampler.so: sampler.c
	$(CC) -bundle  $< $(CDEFINES)  $(CDEBUG) $(COPT) -o $@

$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


MAGIC_PADDING_VALUE=4544
pad.o:	../pad.s
	$(M4) -DMAGIC_PADDING_VALUE=$(MAGIC_PADDING_VALUE) $< | as -o $@

thread_manager.o: thread_manager.c 

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../dppccl 

# Some earlier versions of this Makefile built "subprims_r.o".  
# (That file is now defunct.)
clean:	cclean
	$(RM) -f $(SPOBJ) $(KSPOBJ) subprims_r.o

strip:	../../dppccl
	strip -s retain ../../dppccl
