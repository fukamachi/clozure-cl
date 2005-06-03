;;-*- Mode: Lisp; Package: CCL -*-
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


;(in-package "CCL")

(defppclapfunction %fixnum-to-bignum-set ((bignum arg_y) (fixnum arg_z))
  (unbox-fixnum imm0 fixnum)
  (rotldi imm0 imm0 32)
  (std imm0 ppc64::misc-data-offset bignum)
  (blr))

;;; Multiply the (32-bit) digits X and Y, producing a 64-bit result.
;;; Add the 32-bit "prev" digit and the 32-bit carry-in digit to that 64-bit
;;; result; return the halves as (VALUES high low).
(defppclapfunction %multiply-and-add4 ((x-arg 0) (y arg_x) (prev arg_y) (carry-in arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-prev imm2)
        (unboxed-carry-in imm3)
        (result64 imm4)
        (high arg_y)
        (low arg_z))
    (ld temp0 x-arg vsp)
    (extrdi unboxed-x temp0 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (extrdi unboxed-y y 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (extrdi unboxed-prev prev 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (extrdi unboxed-carry-in carry-in 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (mulld result64 unboxed-x unboxed-y)
    (add result64 result64 prev)
    (add result64 result64 carry-in)
    (clrlsldi low result64 32 ppc64::fixnumshift)
    (clrrdi high result64 32)
    (srdi high high (- 32 ppc64::fixnumshift))
    (std high 0 vsp)
    (set-nargs 2)
    (vpush low)
    (la temp0 '2 vsp)
    (ba .SPvalues)))

(defppclapfunction %multiply-and-add3 ((x arg_x) (y arg_y) (carry-in arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-carry-in imm2)
        (result64 imm3)
        (high arg_y)
        (low arg_z))
    (extrdi unboxed-x arg_x 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (extrdi unboxed-y y 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (extrdi unboxed-carry-in carry-in 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (mulld result64 unboxed-x unboxed-y)
    (add result64 result64 carry-in)
    (clrlsldi low result64 32 ppc64::fixnumshift)
    (clrrdi high result64 32)
    (srdi high high (- 32 ppc64::fixnumshift))
    (std high 0 vsp)
    (set-nargs 2)
    (vpush low)
    (la temp0 '2 vsp)
    (ba .SPvalues)))

;;; Return the (possibly truncated) 32-bit quotient and remainder
;;; resulting from dividing hi:low by divisor.
(defppclapfunction %floor ((num-high arg_x) (num-low arg_y) (divisor arg_z))
  (let ((unboxed-num imm0)
        (unboxed-low imm1)
        (unboxed-divisor imm2)
        (unboxed-quo imm3)
        (unboxed-rem imm4))
    (sldi unboxed-num num-high (- 32 ppc64::fixnumshift))
    (extrdi unboxed-low num-low 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (extrdi unboxed-divisor divisor 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (or unboxed-num unboxed-low unboxed-num)
    (divdu unboxed-quo unboxed-num unboxed-divisor)
    (mulld unboxed-rem unboxed-quo unboxed-divisor)
    (sub unboxed-rem unboxed-num unboxed-rem)
    (clrlsldi arg_y unboxed-quo 32 ppc64::fixnumshift)
    (clrlsldi arg_z unboxed-rem 32 ppc64::fixnumshift)
    (mr temp0 vsp)
    (vpush arg_y)
    (vpush arg_z)
    (set-nargs 2)
    (ba .SPvalues)))

;;; Multiply two (UNSIGNED-BYTE 32) arguments, return the high and
;;; low halves of the 64-bir result
(defppclapfunction %multiply ((x arg_y) (y arg_z))
  (let ((unboxed-x imm0)
        (unboxed-y imm1)
        (unboxed-high imm2)
        (unboxed-low imm3))
    (extrdi unboxed-x x 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (extrdi unboxed-y y 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
    (mullw unboxed-low unboxed-x unboxed-y)
    (mulhwu unboxed-high unboxed-x unboxed-y)
    (box-fixnum arg_z unboxed-low)
    (box-fixnum arg_y unboxed-high)
    (mr temp0 vsp)
    (vpush arg_y)
    (set-nargs 2)
    (vpush arg_z)
    (ba .SPvalues)))

;;; Any words in the "tail" of the bignum should have been
;;; zeroed by the caller.
(defppclapfunction %set-bignum-length ((newlen arg_y) (bignum arg_z))
  (sldi imm0 newlen (- ppc64::misc-subtag-offset ppc64::fixnumshift))
  (ori imm0 imm0 ppc64::subtag-bignum)
  (std imm0 ppc64::misc-header-offset bignum)
  (blr))


(defppclapfunction %signed-bignum-ref ((bignum arg_y) (index arg_z))
  (srdi imm0 index 1)
  (la imm0 ppc64::misc-data-offset imm0)
  (lwax imm0 bignum imm0)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %fixnum-from-two-digit-bignum ((bignum arg_z))
  (ld imm0 ppc64::misc-data-offset bignum)
  (rotldi imm0 imm0 32)
  (box-fixnum arg_z imm0)
  (blr))

;;; If the two least-significant words of bignum can be represented
;;; in a fixnum, return that fixnum; else return nil.
(defppclapfunction %maybe-fixnum-from-two-digit-bignum ((bignum arg_z))
  (ld imm0 ppc64::misc-data-offset bignum)
  (rotldi imm0 imm0 32)
  (box-fixnum arg_z imm0)
  (unbox-fixnum imm1 arg_z)
  (cmpd imm0 imm1)
  (beqlr)
  (li arg_z nil)
  (blr))


(defppclapfunction %digit-logical-shift-right ((digit arg_y) (count arg_z))
  (extrdi imm0 digit 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
  (unbox-fixnum imm1 count)
  (srw imm0 imm0 imm1)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %ashr ((digit arg_y) (count arg_z))
  (extrdi imm0 digit 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
  (unbox-fixnum imm1 count)
  (sraw imm0 imm0 imm1)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %ashl ((digit arg_y) (count arg_z))
  (extrdi imm0 digit 32 (- (- 64 ppc64::fixnumshift) (- 32 1)))
  (unbox-fixnum imm1 count)
  (slw imm0 imm0 imm1)
  (clrlsldi arg_z imm0 32 ppc64::fixnumshift)
  (blr))

(defppclapfunction macptr->fixnum ((ptr arg_z))
  (macptr-ptr arg_z ptr)
  (blr))

