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





;; rewrite in LAP someday.
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


;;; If the bit-arrays are all simple-bit-vectorp, we can do the operations
;;; 32 bits at a time.  (other case have to worry about alignment/displacement.)
#+ppc32-target
(defppclapfunction %simple-bit-boole ((op 0) (b1 arg_x) (b2 arg_y) (result arg_z))
  (la imm0 4 vsp)
  (save-lisp-context imm0)
  (vector-size imm4 result imm4)
  (srwi. imm3 imm4 5)
  (clrlwi imm4 imm4 27)
  (bl @get-dispatch)
  (cmpwi cr1 imm4 0)
  (mflr loc-pc)
  (lwz temp0 op vsp)
  (add loc-pc loc-pc temp0)
  (add loc-pc loc-pc temp0)
  (mtctr loc-pc)
  (li imm0 ppc32::misc-data-offset)
  (b @testw)
  @nextw
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (lwzx imm1 b1 imm0)
  (lwzx imm2 b2 imm0)
  (bctrl)
  (stwx imm1 result imm0)
  (addi imm0 imm0 4)
  @testw
  (bne cr0 @nextw)
  (beq cr1 @done)
  ;; Not sure if we need to make this much fuss about the partial word
  ;; in this simple case, but what the hell.
  (lwzx imm1 b1 imm0)
  (lwzx imm2 b2 imm0)
  (bctrl)
  (lwzx imm2 result imm0)
  (slw imm2 imm2 imm4)
  (srw imm2 imm2 imm4)
  (subfic imm4 imm4 32)
  (srw imm1 imm1 imm4)
  (slw imm1 imm1 imm4)
  (or imm1 imm1 imm2)
  (stwx imm1 result imm0)
  @done
  (restore-full-lisp-context)
  (blr)

  @get-dispatch 
  (blrl)
  @disptach
  (li imm1 0)                           ; boole-clr
  (blr)
  (li imm1 -1)                          ; boole-set
  (blr)
  (blr)                                 ; boole-1
  (blr)                             
  (mr imm1 imm2)                        ; boole-2
  (blr)
  (not imm1 imm1)                       ; boole-c1
  (blr)
  (not imm1 imm2)                       ; boole-c2
  (blr)
  (and imm1 imm1 imm2)                  ; boole-and
  (blr)
  (or imm1 imm1 imm2)                   ; boole-ior
  (blr)
  (xor imm1 imm1 imm2)                  ; boole-xor
  (blr)
  (eqv imm1 imm1 imm2)                  ; boole-eqv
  (blr)
  (nand imm1 imm1 imm2)                 ; boole-nand
  (blr)
  (nor imm1 imm1 imm2)                  ; boole-nor
  (blr)
  (andc imm1 imm2 imm1)                 ; boole-andc1
  (blr)
  (andc imm1 imm1 imm2)                 ; boole-andc2
  (blr)
  (orc imm1 imm2 imm1)                  ; boole-orc1
  (blr)
  (orc imm1 imm1 imm2)                  ; boole-orc2
  (blr))

#+ppc64-target
(defppclapfunction %simple-bit-boole ((op 0) (b1 arg_x) (b2 arg_y) (result arg_z))
  (la imm0 8 vsp)
  (save-lisp-context imm0)
  (vector-size imm4 result imm4)
  (srdi. imm3 imm4 6)
  (clrldi imm4 imm4 (- 64 6))
  (bl @get-dispatch)
  (cmpdi cr1 imm4 0)                    ; at most low 6 bits set in imm4
  (mflr loc-pc)
  (ld temp0 op vsp)
  (add loc-pc loc-pc temp0)
  (mtctr loc-pc)
  (li imm0 ppc64::misc-data-offset)
  (b @testd)
  @nextd
  (cmpdi cr0 imm3 1)
  (subi imm3 imm3 1)
  (ldx imm1 b1 imm0)
  (ldx imm2 b2 imm0)
  (bctrl)
  (stdx imm1 result imm0)
  (addi imm0 imm0 8)
  @testd
  (bne cr0 @nextd)
  (beq cr1 @done)
  ;; Not sure if we need to make this much fuss about the partial word
  ;; in this simple case, but what the hell.
  (ldx imm1 b1 imm0)
  (ldx imm2 b2 imm0)
  (bctrl)
  (ldx imm2 result imm0)
  (sld imm2 imm2 imm4)
  (srd imm2 imm2 imm4)
  (subfic imm4 imm4 64)
  (srd imm1 imm1 imm4)
  (sld imm1 imm1 imm4)
  (or imm1 imm1 imm2)
  (stdx imm1 result imm0)
  @done
  (restore-full-lisp-context)
  (blr)

  @get-dispatch 
  (blrl)
  @disptach
  (li imm1 0)                           ; boole-clr
  (blr)
  (li imm1 -1)                          ; boole-set
  (blr)
  (blr)                                 ; boole-1
  (blr)                             
  (mr imm1 imm2)                        ; boole-2
  (blr)
  (not imm1 imm1)                       ; boole-c1
  (blr)
  (not imm1 imm2)                       ; boole-c2
  (blr)
  (and imm1 imm1 imm2)                  ; boole-and
  (blr)
  (or imm1 imm1 imm2)                   ; boole-ior
  (blr)
  (xor imm1 imm1 imm2)                  ; boole-xor
  (blr)
  (eqv imm1 imm1 imm2)                  ; boole-eqv
  (blr)
  (nand imm1 imm1 imm2)                 ; boole-nand
  (blr)
  (nor imm1 imm1 imm2)                  ; boole-nor
  (blr)
  (andc imm1 imm2 imm1)                 ; boole-andc1
  (blr)
  (andc imm1 imm1 imm2)                 ; boole-andc2
  (blr)
  (orc imm1 imm2 imm1)                  ; boole-orc1
  (blr)
  (orc imm1 imm1 imm2)                  ; boole-orc2
  (blr))



  

