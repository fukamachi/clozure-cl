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
  (require "PPC-ARCH" "ccl:compiler;ppc;ppc-arch")
  (require "PPC-LAPMACROS" "ccl:compiler;ppc;ppc-lapmacros"))

; This assumes that macros & special-operators
; have something that's not FUNCTIONP in their
; function-cells.
(defppclapfunction %function ((sym arg_z))
  (check-nargs 1)
  (cmpwi cr1 sym ppc::nil-value)
  (let ((symptr temp0)
        (symbol temp1)
        (def arg_z))
    (li symptr (+ arch::nilsym-offset ppc::nil-value))
    (mr symbol sym)
    (if (:cr1 :ne)
      (progn
        (trap-unless-typecode= sym arch::subtag-symbol)
        (mr symptr sym)))
    (lwz def arch::symbol.fcell symptr)
    (extract-typecode imm0 def)
    (cmpwi cr0 imm0 arch::subtag-function)
    (beqlr+)
    (uuo_interr arch::error-udf symbol)))



; Traps unless sym is NIL or some other symbol.
(defppclapfunction %symbol->symptr ((sym arg_z))
  (cmpwi cr0 arg_z ppc::nil-value)
  (if (:cr0 :eq)
    (progn
      (li arg_z (+ arch::nilsym-offset ppc::nil-value))
      (blr)))
  (trap-unless-typecode= arg_z arch::subtag-symbol)
  (blr))

; Traps unless symptr is a symbol; returns NIL if symptr is NILSYM.
(defppclapfunction %symptr->symbol ((symptr arg_z))
  (li imm1 (+ arch::nilsym-offset ppc::nil-value))
  (cmpw cr0 imm1 symptr)
  (if (:cr0 :eq)
    (progn 
      (li arg_z ppc::nil-value)
      (blr)))
  (trap-unless-typecode= symptr arch::subtag-symbol imm0)
  (blr))

(defppclapfunction %%sym-value ((name arg_z))
  (mr arg_y name)
  (ba .SPspecref))

(defppclapfunction %%set-sym-value ((name arg_y) (val arg_z))
  (ba .SPspecset))

;;; Return the address of the innermost binding-stack triplet for
;;; symbol in the current thread, or 0 if there's no thread-local
;;; binding.
(defppclapfunction %%symbol-binding-address ((sym arg_z))
  (mr arg_y sym)
  (mflr loc-pc)
  (bla .SPspecref)
  (mtlr loc-pc)
  (mr arg_z imm1)
  (blr))
