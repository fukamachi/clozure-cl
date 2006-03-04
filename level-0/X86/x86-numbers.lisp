;-*- Mode: Lisp; Package: CCL -*-
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



(in-package "CCL")



(defx86lapfunction %fixnum-signum ((number arg_z))
  (movq ($ '-1) (% arg_x))
  (movq ($ '1) (% arg_y))
  (testq (% number) (% number))
  (cmovsq (% arg_x) (% arg_z))
  (cmovnsq (% arg_y) (% arg_z))
  (single-value-return))

;;; see %logcount (ppc-bignum.lisp)
(defx86lapfunction %ilogcount ((number arg_z))
  (let ((rshift imm0)
        (temp imm1))
    (unbox-fixnum number rshift)
    (xorq (% arg_z) (% arg_z))
    (testq (% rshift) (% rshift))
    (jmp @test)
    @next
    (lea (@ -1 (% rshift)) (% temp))
    (and (% temp) (% rshift))            ; sets flags
    (lea (@ '1 (% arg_z)) (% arg_z))    ; doesn't set flags
    @test
    (jne @next)
    (single-value-return)))

(defx86lapfunction %iash ((number arg_y) (count arg_z))
  (unbox-fixnum count imm1)
  (unbox-fixnum number imm0)
  (xorq (% rcx) (% rcx))                ;rcx = temp1
  (testq (% count) (% count))
  (jge @left)
  (subb (% imm1.b) (% cl))
  (sar (% cl) (% imm0))
  (xorb (% cl) (% cl))
  (box-fixnum imm0 arg_z)
  (single-value-return)
  @left
  (movb (% imm1.b) (% cl))
  (shl (% cl) (% arg_z))
  (xorb (% cl) (% cl))
  (single-value-return))

(defparameter *double-float-zero* 0.0d0)
(defparameter *short-float-zero* 0.0s0)


(defx86lapfunction %fixnum-intlen ((number arg_z))
  (unbox-fixnum arg_z imm0)
  (movq (% imm0) (% imm1))
  (notq (% imm1))
  (testq (% imm0) (% imm0))
  (cmovsq (% imm1) (% imm0))
  (bsrq (% imm0) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))


;;; Caller guarantees that result fits in a fixnum.

(defx86lapfunction %truncate-double-float->fixnum ((arg arg_z))
  (get-double-float arg fp1)
  (cvttsd2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))


(defx86lapfunction %truncate-short-float->fixnum ((arg arg_z))
  (get-single-float arg fp1)
  (cvttss2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))

;;; DOES round to even

(defx86lapfunction %round-nearest-double-float->fixnum ((arg arg_z))
  (get-double-float arg fp1)
  (cvtsd2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))


(defx86lapfunction %round-nearest-short-float->fixnum ((arg arg_z))
  (get-double-float arg fp1)
  (cvtss2si (% fp1) (% imm0))
  (box-fixnum imm0 arg_z)  
  (single-value-return))


;;; We'll get a SIGFPE if divisor is 0.  We need a 3rd imm reg here.
(defx86lapfunction %fixnum-truncate ((dividend arg_y) (divisor arg_z))
  (unbox-fixnum dividend imm0)
  (cqto)                                ; imm1 := sign_extend(imm0)
  (pushq (% rbp))
  (unbox-fixnum divisor rbp)
  (idivq (% rbp))
  (popq (% rbp))
  (movq (% rsp) (% temp0))
  (box-fixnum imm1 arg_y)
  (pushq (% arg_y))
  (box-fixnum imm0 arg_z)
  (pushq (% arg_z))
  (set-nargs 2)
  (jmp-subprim .SPvalues))

(defx86lapfunction called-for-mv-p ()
  (ref-global ret1valaddr imm0)
  (movq (@ x8664::lisp-frame.return-address (% rbp)) (% imm1))
  (cmpq (% imm0) (% imm1))
  (movq ($ t) (% imm0))
  (movq ($ nil) (% arg_z))
  (cmoveq (% imm0) (% arg_z))
  (single-value-return))
  
#|
Date: Mon, 3 Feb 1997 10:04:08 -0500
To: info-mcl@digitool.com, wineberg@franz.scs.carleton.ca
From: dds@flavors.com (Duncan Smith)
Subject: Re: More info on the random number generator
Sender: owner-info-mcl@digitool.com
Precedence: bulk

The generator is a Linear Congruential Generator:

   X[n+1] = (aX[n] + c) mod m

where: a = 16807  (Park&Miller recommend 48271)
       c = 0
       m = 2^31 - 1

See: Knuth, Seminumerical Algorithms (Volume 2), Chapter 3.

The period is: 2^31 - 2  (zero is excluded).

What makes this generator so simple is that multiplication and addition mod
2^n-1 is easy.  See Knuth Ch. 4.3.2 (2nd Ed. p 272).

    ab mod m = ...

If         m = 2^n-1
           u = ab mod 2^n
           v = floor( ab / 2^n )

    ab mod m = u + v                   :  u+v < 2^n
    ab mod m = ((u + v) mod 2^n) + 1   :  u+v >= 2^n

What we do is use 2b and 2n so we can do arithemetic mod 2^32 instead of
2^31.  This reduces the whole generator to 5 instructions on the 680x0 or
80x86, and 8 on the 60x.

-Duncan

|#
; Use the two fixnums in state to generate a random fixnum >= 0 and < 65536
; Scramble those fixnums up a bit.

(defun %next-random-seed (state)
  (let* ((seed (dpb (ldb (byte 16 13) (%svref state 1))
                    (byte 16 16)
                    (ldb (byte 16 13) (%svref state 2)))))
    (multiple-value-bind (seed-low seed-high) (%multiply seed (* 2 48271))
      (setq seed (logand #xffffffff (+ seed-low seed-high)))
      (setf (%svref state 1) (ash (ldb (byte 16 16) seed) 13)
            (%svref state 2) (ash (ldb (byte 16 0) seed) 13))
      (dpb (ldb (byte 8 0) seed-low) (byte 8 8) (ldb (byte 8 3) seed-high)))))



;;; n1 and n2 must be positive (esp non zero)

(eval-when (:compile-toplevel)
  (warn "Fix %fixnum-gcd"))
#+not-ready
(defx86lapfunction %fixnum-gcd ((n1 arg_y)(n2 arg_z))
  (let ((temp imm0)
	(u imm1)
	(v imm2)
	(ut0 imm3)
	(vt0 imm4))
    (unbox-fixnum u n1)
    (unbox-fixnum v n2)
    (neg temp u)
    (and temp temp u)
    (cntlzd ut0 temp)
    (subfic ut0 ut0 63)
    (neg temp v)
    (and temp temp v)
    (cntlzd vt0 temp)
    (subfic vt0 vt0 63)
    (cmpw cr2 ut0 vt0)
    (srd u u ut0)
    (srd v v vt0)
    (addi ut0 ut0 ppc64::fixnum-shift)
    (addi vt0 vt0 ppc64::fixnum-shift)
    @loop
    (cmpd cr0 u v)
    (sld arg_z u ut0)
    (bgt cr0 @u>v)
    (blt cr0 @u<v)
    (blelr cr2)
    (sld arg_z u vt0)
    (single-value-return)
    @u>v
    (sub u u v)
    @shiftu
    (andi. temp u (ash 1 1))
    (srdi u u 1)
    (beq cr0 @shiftu)
    (b @loop)
    @u<v
    (sub v v u)
    @shiftv
    (andi. temp v (ash 1 1))
    (srdi v v 1)
    (beq cr0 @shiftv)
    (b @loop)))
    



;;; End of x86-numbers.lisp
