;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(eval-when (:compile-toplevel :execute)
  (require "PPC32-ARCH")
  (require "PPC-LAPMACROS"))

; This assumes that macros & special-operators
; have something that's not FUNCTIONP in their
; function-cells.
(defppclapfunction %function ((sym arg_z))
  (check-nargs 1)
  (cmpwi cr1 sym ppc32::nil-value)
  (let ((symptr temp0)
        (symbol temp1)
        (def arg_z))
    (li symptr (+ ppc32::nilsym-offset ppc32::nil-value))
    (mr symbol sym)
    (if (:cr1 :ne)
      (progn
        (trap-unless-typecode= sym ppc32::subtag-symbol)
        (mr symptr sym)))
    (lwz def ppc32::symbol.fcell symptr)
    (extract-typecode imm0 def)
    (cmpwi cr0 imm0 ppc32::subtag-function)
    (beqlr+)
    (uuo_interr ppc32::error-udf symbol)))



; Traps unless sym is NIL or some other symbol.
(defppclapfunction %symbol->symptr ((sym arg_z))
  (cmpwi cr0 arg_z ppc32::nil-value)
  (if (:cr0 :eq)
    (progn
      (li arg_z (+ ppc32::nilsym-offset ppc32::nil-value))
      (blr)))
  (trap-unless-typecode= arg_z ppc32::subtag-symbol)
  (blr))

; Traps unless symptr is a symbol; returns NIL if symptr is NILSYM.
(defppclapfunction %symptr->symbol ((symptr arg_z))
  (li imm1 (+ ppc32::nilsym-offset ppc32::nil-value))
  (cmpw cr0 imm1 symptr)
  (if (:cr0 :eq)
    (progn 
      (li arg_z ppc32::nil-value)
      (blr)))
  (trap-unless-typecode= symptr ppc32::subtag-symbol imm0)
  (blr))

(defppclapfunction %svar-sym-value ((svar arg_z))
  (mr temp0 svar)
  (ba .SPsvar-specref))

(defppclapfunction %svar-set-sym-value ((svar arg_y) (val arg_z))
  (mr temp0 svar)
  (ba .SPsvar-specset))

(defppclapfunction %svar-binding-address ((svar arg_z))
  (lwz imm3 ppc32::svar.idx svar)
  (lwz imm2 ppc32::tcr.tlb-limit rcontext)
  (lwz imm4 ppc32::tcr.tlb-pointer rcontext)
  (cmplw imm3 imm2)
  (bge @sym)
  (lwzx temp0 imm4 imm3)
  (cmpwi temp0 ppc32::subtag-no-thread-local-binding)
  (beq @sym)
  (slwi imm3 imm3 ppc32::fixnumshift)
  (vpush imm4)
  (vpush imm3)
  (set-nargs 2)
  (la vsp 8 vsp)
  (ba .SPvalues)
  @sym
  (lwz arg_z ppc32::svar.symbol svar)
  (li arg_y 'ppc32::symbol.vcell)
  (vpush arg_z)
  (vpush arg_y)
  (set-nargs 2)
  (la vsp 8 vsp)
  (ba .SPvalues))
