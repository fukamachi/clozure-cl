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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  #+x8632-target
  (require "X8632-ARCH")
  #+x8664-target
  (require "X8664-ARCH")
  (require "X86-LAPMACROS"))





;; rewrite in LAP someday (soon).
(defun %init-misc (val uvector)
  (dotimes (i (uvsize uvector) uvector)
    (setf (uvref uvector i) val)))


;;; Make a new vector of size newsize whose subtag matches that of oldv-arg.
;;; Blast the contents of the old vector into the new one as quickly as
;;; possible; leave remaining elements of new vector undefined (0).
;;; Return new-vector.
(defun %extend-vector (start oldv newsize)
  (declare (fixnum start))
  (let* ((new (%alloc-misc newsize (typecode oldv)))
         (oldsize (uvsize oldv)))
    (declare (fixnum oldsize))
    (do* ((i 0 (1+ i))
          (j start (1+ j)))
         ((= i oldsize) new)
      (declare (fixnum i j))
      (setf (uvref new j) (uvref oldv i)))))
    




;;; argument is a vector header or an array header.  Or else.
(defx86lapfunction %array-header-data-and-offset ((a arg_z))
  (let ((offset arg_y)
        (temp temp0))
    (movl ($ '0) (%l offset))
    (movq (% a) (% temp))
    @loop
    (movq (@ target::arrayH.data-vector (% temp)) (% a))
    (extract-subtag a imm0)
    (addq (@ target::arrayH.displacement (% temp)) (% offset))
    (rcmp (% imm0) ($ target::subtag-vectorH))
    (movq (% a) (% temp))
    (jle @loop)
    (push (% a))
    (push (% offset))
    (set-nargs 2)
    (lea (@ '2 (% rsp)) (% temp0))
    (jmp-subprim  .SPvalues)))



(defx86lapfunction %boole-clr ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq ($ 0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-set ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq ($ -1) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-1 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-2 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-c1 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-c2 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-and ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (andq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-ior ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (orq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-xor ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (xorq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-eqv ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (xorq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-nand ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (andq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-nor ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (orq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-andc1 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (notq (% imm0))
  (andq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-andc2 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (andq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-orc1 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (notq (% imm0))
  (orq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defx86lapfunction %boole-orc2 ((idx 0) (b0 arg_x) (b1 arg_y) (dest arg_z))
  (pop (% temp0))
  (discard-reserved-frame)
  (movq (@ x8664::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notq (% imm0))
  (orq (@ x8664::misc-data-offset (% b0) (% temp0)) (% imm0))
  (movq (% imm0) (@ x8664::misc-data-offset (% dest) (% temp0)))
  (single-value-return))

(defparameter *simple-bit-boole-functions* ())

(setq *simple-bit-boole-functions*
      (vector
       #'%boole-clr
       #'%boole-set
       #'%boole-1
       #'%boole-2
       #'%boole-c1
       #'%boole-c2
       #'%boole-and
       #'%boole-ior
       #'%boole-xor
       #'%boole-eqv
       #'%boole-nand
       #'%boole-nor
       #'%boole-andc1
       #'%boole-andc2
       #'%boole-orc1
       #'%boole-orc2))

(defun %simple-bit-boole (op b1 b2 result)
  (let* ((f (svref *simple-bit-boole-functions* op)))
    (dotimes (i (ash (the fixnum (+ (length result) 63)) -6) result)
      (funcall f i b1 b2 result))))

(defx86lapfunction %aref2 ((array arg_x) (i arg_y) (j arg_z))
  (check-nargs 3)
  (jmp-subprim .SParef2))

(defx86lapfunction %aset2 ((array 0) (i arg_x) (j arg_y) (newval arg_z))
  (check-nargs 4)
  (pop (% temp0))
  (discard-reserved-frame)
  (jmp-subprim .SPaset2))





  

