;;; -*- Mode: Lisp; Package: CCL -*-
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

;;; level-0;ppc;ppc-hash.lisp


;(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv"))




; This should stay in LAP so that it's fast
; Equivalent to cl:mod when both args are positive fixnums
(defppclapfunction fast-mod ((number arg_y) (divisor arg_z))
  (divwu imm0 number divisor)
  (mullw arg_z imm0 divisor)
  (subf arg_z arg_z number)
  (blr))

; not used today
(defppclapfunction fixnum-rotate ((number arg_y) (count arg_z))
  (unbox-fixnum imm0 count)
  (unbox-fixnum imm1 number)
  (rlwnm imm1 imm1 imm0 0 31)
  (box-fixnum arg_z imm1)
  (blr))



(defppclapfunction %dfloat-hash ((key arg_z))
  (lwz imm0 arch::double-float.value key)
  (lwz imm1 arch::double-float.val-low key)
  (add imm0 imm0 imm1)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %sfloat-hash ((key arg_z))
  (lwz imm0 arch::single-float.value key)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %macptr-hash ((key arg_z))
  (lwz imm0 arch::macptr.address key)
  (slwi imm1 imm0 24)
  (add imm0 imm0 imm1)
  (clrrwi arg_z imm0 arch::fixnumshift)
  (blr))

(defppclapfunction %bignum-hash ((key arg_z))
  (let ((header imm3)
        (offset imm2)
        (ndigits imm1)
        (immhash imm0))
    (li immhash 0)
    (li offset arch::misc-data-offset)
    (getvheader header key)
    (header-size ndigits header)
    (let ((next header))
      @loop
      (cmpwi cr0 ndigits 1)
      (subi ndigits ndigits 1)
      (lwzx next key offset)
      (addi offset offset 4)
      (rotlwi immhash immhash 13)
      (add immhash immhash next)
      (bne cr0 @loop))
    (clrrwi arg_z immhash arch::fixnumshift)
    (blr)))

      


(defppclapfunction %get-fwdnum ()
  (ref-global arg_z arch::fwdnum)
  (blr))


(defppclapfunction %get-gc-count ()
  (ref-global arg_z arch::gc-count)
  (blr))


; Setting a key in a hash-table vector needs to 
; ensure that the vector header gets memoized as well
#+ppc-target
(defppclapfunction %set-hash-table-vector-key ((vector arg_x) (index arg_y) (value arg_z))
  (la imm0 arch::misc-data-offset index)
  (stwx value vector imm0)
  (blr))


; end of ppc-hash.lisp
