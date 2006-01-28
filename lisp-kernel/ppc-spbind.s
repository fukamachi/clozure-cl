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

/* Subprimitives related to special-variable binding,  reference, and assignment */
        	
	include(lisp.s)
	_beginfile
        .align 2
/* This never affects the symbol's vcell */
/* Non-null symbol in arg_y, new value in arg_z */        
_spentry(bind)
        __(ldr(imm3,symbol.binding_index(arg_y)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(beq 9f)
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(arg_z,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:
        __(mr arg_z,arg_y)
        __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)

/* arg_z = symbol: bind it to its current value */        
_spentry(bind_self)
        __(ldr(imm3,symbol.binding_index(arg_z)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(cmpri(cr1,temp1,no_thread_local_binding_marker))
        __(beq 9f)
        __(mr temp0,temp1)
        __(bne cr1,1f)
        __(ldr(temp0,symbol.vcell(arg_z)))
1:              
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(temp0,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:      __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)

/* Bind symbol in arg_z to NIL */               
_spentry(bind_nil)
        __(ldr(imm3,symbol.binding_index(arg_z)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(beq- 9f)
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(li imm0,nil_value)
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(imm0,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:      __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)

       
/* Bind symbol in arg_z to its current value ;  trap if symbol is unbound */
_spentry(bind_self_boundp_check)
        __(ldr(imm3,symbol.binding_index(arg_z)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpri(imm3,0))
        __(trlle(imm0,imm3))           /* tlb too small */
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(ldrx(temp1,imm2,imm3))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(beq 9f)              /* no real tlb index */
        __(cmpri(temp1,no_thread_local_binding_marker))
        __(mr temp0,temp1)
        __(bne 1f)
        __(ldr(temp0,symbol.vcell(arg_z)))
1:      __(treqi(temp0,unbound_marker))       
        __(vpush(temp1))
        __(vpush(imm3))
        __(vpush(imm1))
        __(strx(temp0,imm2,imm3))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)
9:      __(lwi(arg_y,XSYMNOBIND))
        __(set_nargs(2))
        __(b _SPksignalerr)


_spentry(unbind)
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))   
        __(ldr(imm3,binding.sym(imm1)))
        __(ldr(temp1,binding.val(imm1)))
        __(ldr(imm1,binding.link(imm1)))
        __(strx(temp1,imm2,imm3))
        __(str(imm1,tcr.db_link(rcontext)))
        __(blr)

_spentry(unbind_n)
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))   
1:      __(subi imm0,imm0,1)
        __(ldr(imm3,binding.sym(imm1)))
        __(ldr(temp1,binding.val(imm1)))
        __(cmpri(imm0,0))
        __(ldr(imm1,binding.link(imm1)))
        __(strx(temp1,imm2,imm3))
        __(bne 1b)
        __(str(imm1,tcr.db_link(rcontext)))
        __(blr)

 /*
   Clobbers imm1,imm2,imm5,arg_x, arg_y
*/
_spentry(unbind_to)
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
1:      __(ldr(imm5,binding.sym(imm1)))
        __(ldr(arg_y,binding.val(imm1)))
        __(ldr(imm1,binding.link(imm1)))
        __(cmpr(imm0,imm1))
        __(strx(arg_y,imm2,imm5))
        __(bne 1b)
        __(str(imm1,tcr.db_link(rcontext)))
        __(blr)
	


/*
   Restore the special bindings from the top of the tstack, 
   leaving the tstack frame allocated. 
   Note that there might be 0 saved bindings, in which case 
   do nothing. 
   Note also that this is -only- called from an unwind-protect 
   cleanup form, and that .SPnthrowXXX is keeping one or more 
   values in a frame on top of the tstack. 
*/
                        
_spentry(progvrestore)
	__(ldr(imm0,tsp_frame.backlink(tsp)))	/* ignore .SPnthrowXXX values frame */
	__(ldr(imm0,tsp_frame.data_offset(imm0)))
	__(cmpri(cr0,imm0,0))
	__(unbox_fixnum(imm0,imm0))
	__(bne+ cr0,_SPunbind_n)
	__(blr)

/* Bind CCL::*INTERRUPT-LEVEL* to 0.  If its value had been negative, check 
   for pending interrupts after doing so.  "nargs" can be freely used for an
   interrupt trap in this context. */
_spentry(bind_interrupt_level_0)
        __(ldr(imm4,tcr.tlb_pointer(rcontext)))
        __(ldr(temp0,INTERRUPT_LEVEL_BINDING_INDEX(imm4)))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(cmpri(temp0,0))
        __(li imm3,INTERRUPT_LEVEL_BINDING_INDEX)
        __(vpush(temp0))
        __(vpush(imm3))
        __(vpush(imm1))
        __(str(rzero,INTERRUPT_LEVEL_BINDING_INDEX(imm4)))
        __(str(vsp,tcr.db_link(rcontext)))
        __(beqlr)
        __(mr nargs,temp0)
        __(bgt 1f)
        __(ldr(nargs,tcr.interrupt_pending(rcontext)))
1:      __(trgti(nargs,0))        
        __(blr)

/* Bind CCL::*INTERRUPT-LEVEL* to the fixnum -1.  (This has the effect
   of disabling interrupts.) */
_spentry(bind_interrupt_level_m1)
        __(li imm2,-fixnumone)
        __(li imm3,INTERRUPT_LEVEL_BINDING_INDEX)
        __(ldr(imm4,tcr.tlb_pointer(rcontext)))
        __(ldr(temp0,INTERRUPT_LEVEL_BINDING_INDEX(imm4)))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(vpush(temp0))
        __(vpush(imm3))
        __(vpush(imm1))
        __(str(imm2,INTERRUPT_LEVEL_BINDING_INDEX(imm4)))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)

        
/* Bind CCL::*INTERRUPT-LEVEL* to the value in arg_z.  If that value's 0,
   do what _SPbind_interrupt_level_0 does */
_spentry(bind_interrupt_level)
        __(cmpri(arg_z,0))
        __(li imm3,INTERRUPT_LEVEL_BINDING_INDEX)
        __(ldr(imm4,tcr.tlb_pointer(rcontext)))
        __(ldr(temp0,INTERRUPT_LEVEL_BINDING_INDEX(imm4)))
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(beq _SPbind_interrupt_level_0)
        __(vpush(temp0))
        __(vpush(imm3))
        __(vpush(imm1))
        __(str(arg_z,INTERRUPT_LEVEL_BINDING_INDEX(imm4)))
        __(str(vsp,tcr.db_link(rcontext)))
        __(blr)

/* Unbind CCL::*INTERRUPT-LEVEL*.  If the value changes from negative to
   non-negative, check for pending interrupts.  This is often called in
   a context where nargs is significant, so save and restore nargs around
   any interrupt polling */
        
_spentry(unbind_interrupt_level)        
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))   
        __(ldr(imm1,tcr.db_link(rcontext)))
        __(ldr(temp1,INTERRUPT_LEVEL_BINDING_INDEX(imm2)))
        __(cmpri(cr1,temp1,0))
        __(ldr(temp1,binding.val(imm1)))
        __(ldr(imm1,binding.link(imm1)))
        __(cmpri(cr0,temp1,0))
        __(str(temp1,INTERRUPT_LEVEL_BINDING_INDEX(imm2)))
        __(str(imm1,tcr.db_link(rcontext)))
        __(bgelr cr1)
        __(bltlr cr0)
        __(mr imm2,nargs)
        __(check_pending_interrupt([cr1]))
        __(mr nargs,imm2)
        __(blr)

                        
/* on entry: arg_z = symbol.  On exit, arg_z = value (possibly
	unbound_marker), arg_y = symbol, imm3 = symbol.binding-index */
_spentry(specref)
        __(ldr(imm3,symbol.binding_index(arg_z)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpr(imm3,imm0))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(mr arg_y,arg_z)
        __(bge 1f)
        __(ldrx(arg_z,imm2,imm3))
        __(cmpri(arg_z,no_thread_local_binding_marker))
        __(bnelr)
1:     	__(ldr(arg_z,symbol.vcell(arg_y)))
        __(blr)


_spentry(specrefcheck)
        __(ldr(imm3,symbol.binding_index(arg_z)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(cmpr(imm3,imm0))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(mr arg_y,arg_z)
        __(bge 1f)
        __(ldrx(arg_z,imm2,imm3))
        __(cmpri(arg_z,no_thread_local_binding_marker))
        __(bne 2f)
1:     	__(ldr(arg_z,symbol.vcell(arg_y)))
2:      __(treqi(arg_z,unbound_marker))
        __(blr)
	
/* arg_y = special symbol, arg_z = new value. */        
_spentry(specset)
        __(ldr(imm3,symbol.binding_index(arg_y)))
        __(ldr(imm0,tcr.tlb_limit(rcontext)))
        __(ldr(imm2,tcr.tlb_pointer(rcontext)))
        __(cmpr(imm3,imm0))
        __(bge 1f)
        __(ldrx(temp1,imm2,imm3))
        __(cmpri(temp1,no_thread_local_binding_marker))
        __(beq 1f)
        __(strx(arg_z,imm2,imm3))
        __(blr)
1:     	__(mr arg_x,arg_y)
        __(li arg_y,symbol.vcell-misc_data_offset)
        __(b _SPgvset)

        
