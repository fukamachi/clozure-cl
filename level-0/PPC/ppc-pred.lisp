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
  (require "PPC-ARCH")
  (require "PPC-LAPMACROS"))

(defppclapfunction eql ((x arg_y) (y arg_z))
  (check-nargs 2)
  @tail
  (cmpw cr0 x y)
  (extract-lisptag imm0 x)
  (extract-lisptag imm1 y)
  (cmpwi cr1 imm0 arch::tag-misc)
  (cmpwi cr2 imm1 arch::tag-misc)
  (beq cr0 @win)
  (bne cr1 @lose)
  (bne cr2 @lose)
  ; Objects are both of tag-misc.  Headers must match exactly;
  ; dispatch on subtag.
  (getvheader imm0 x)
  (getvheader imm1 y)
  (cmpw cr0 imm0 imm1)
  (extract-lowbyte imm1 imm1)
  (cmpwi cr1 imm1 arch::subtag-macptr)
  (cmpwi cr2 imm1 arch::max-numeric-subtag)
  (beq cr1 @macptr)
  (bne cr0 @lose)
  (bgt cr2 @lose)
  (cmpwi cr0 imm1 arch::subtag-ratio)
  (cmpwi cr1 imm1 arch::subtag-complex)
  (beq cr0 @node)
  (beq cr1 @node)
  ; A single-float looks a lot like a macptr to me.
  ; A double-float is simple, a bignum involves a loop.
  (cmpwi cr0 imm1 arch::subtag-bignum)
  (cmpwi cr1 imm1 arch::subtag-double-float)
  (beq cr0 @bignum)
  (bne cr1 @one-unboxed-word)                     ; single-float case
  ; This is the double-float case.
  (lwz imm0 arch::double-float.value x)
  (lwz imm1 arch::double-float.value y)
  (cmpw cr0 imm0 imm1)
  (lwz imm0 arch::double-float.val-low x)
  (lwz imm1 arch::double-float.val-low y)
  (cmpw cr1 imm0 imm1)
  (bne cr0 @lose)
  (bne cr1 @lose)
  @win
  (li arg_z (+ arch::t-offset ppc::nil-value))
  (blr)
  @macptr
  (extract-lowbyte imm0 imm0)
  (cmpw cr0 imm1 imm0)
  (bne- cr0 @lose)
  @one-unboxed-word
  (lwz imm0 arch::misc-data-offset x)
  (lwz imm1 arch::misc-data-offset y)
  (cmpw cr0 imm0 imm1)
  (beq cr0 @win)
  @lose
  (li arg_z ppc::nil-value)
  (blr)
  @bignum
  ; Way back when, we got x's header into imm0.  We know
  ; that y's header is identical.  Use the element-count 
  ; from imm0 to control the loop.  There's no such thing
  ; as a 0-element bignum, so the loop must always execute
  ; at least once.
  (header-size imm0 imm0)
  (li imm1 arch::misc-data-offset)
  @bignum-next
  (cmpwi cr1 imm0 1)                    ; last time through ?
  (lwzx imm2 x imm1)
  (lwzx imm3 y imm1)
  (cmpw cr0 imm2 imm3)
  (subi imm0 imm0 1)
  (la imm1 4 imm1)
  (bne cr0 @lose)
  (bne cr1 @bignum-next)
  (li arg_z (+ arch::t-offset ppc::nil-value))
  (blr)
  @node
  ; Have either a ratio or a complex.  In either case, corresponding
  ; elements of both objects must be EQL.  Recurse on the first
  ; elements.  If true, tail-call on the second, else fail.
  (vpush x)
  (vpush y)
  (save-lisp-context)
  (lwz x arch::misc-data-offset x)
  (lwz y arch::misc-data-offset y)
  (bl @tail)
  (cmpwi cr0 arg_z ppc::nil-value)
  (restore-full-lisp-context)
  (vpop y)
  (vpop x)
  (beq cr0 @lose)
  (lwz x (+ 4 arch::misc-data-offset) x)
  (lwz y (+ 4 arch::misc-data-offset) y)
  (b @tail))
  


(defppclapfunction equal ((x arg_y) (y arg_z))
  (check-nargs 2)
  @top
  (cmpw cr0 x y)
  (extract-fulltag imm0 x)
  (extract-fulltag imm1 y)
  (cmpw cr1 imm0 imm1)
  (cmpwi cr2 imm0 arch::fulltag-cons)
  (cmpwi cr3 imm0 arch::fulltag-misc)
  (beq cr0 @win)
  (bne cr1 @lose)
  (beq cr2 @cons)
  (bne cr3 @lose)
  (extract-typecode imm0 x)
  (extract-typecode imm1 y)
  (cmpwi cr0 imm0 arch::subtag-macptr)
  (cmpwi cr2 imm0 arch::subtag-istruct)
  (cmpwi cr1 imm0 arch::subtag-vectorH)
  (cmpw cr3 imm0 imm1)
  (ble cr0 @eql)
  (cmplwi cr0 imm1 arch::subtag-vectorH)
  (beq cr2 @same)
  (blt cr1 @lose)
  (bge cr0 @go)
  @lose
  (li arg_z ppc::nil-value)
  (blr)
  @same
  (bne cr3 @lose)
  @go
  (set-nargs 2)
  (lwz fname 'hairy-equal nfn)
  (ba .SPjmpsym)
  @eql
  (set-nargs 2)
  (lwz fname 'eql nfn)
  (ba .SPjmpsym)
  @cons
  (vpush x)
  (vpush y)
  (mflr loc-pc)
  (save-lisp-context)
  (lwz imm0 arch::tcr.cs-limit rcontext) ; stack probe
  (twllt arch::sp imm0)
  (%car x x)
  (%car y y)
  (bl @top)
  (cmpwi :cr0 arg_z ppc::nil-value)  
  (mr nfn fn)
  (restore-full-lisp-context)           ; gets old fn to fn  
  (vpop y)
  (vpop x)
  (beq cr0 @lose)
  (%cdr x x)
  (%cdr y y)
  (b @top)
  @win
  (li arg_z (+ arch::t-offset ppc::nil-value))
  (blr))


      







