;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
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
  (require "NUMBER-MACROS")
  (require :number-case-macro))


;;; make a float from hi - high 24 bits mantissa (ignore implied higher bit)
;;;                   lo -  low 28 bits mantissa
;;;                   exp  - take low 11 bits
;;;                   sign - sign(sign) => result
;;; hi result - 1 bit sign: 11 bits exp: 20 hi bits of hi arg
;;; lo result - 4 lo bits of hi arg: 28 lo bits of lo arg
;;; no error checks, no tweaks, no nuthin 

;;; sign is -1, 1, maybe zero



#+x8664-target
(defx86lapfunction %make-float-from-fixnums ((float 8)(hi 0) (lo arg_x) (exp arg_y) (sign arg_z))
  (enter-function)
  (mov (% sign) (% imm1))
  (sar ($ 63) (% imm1))
  (shl ($ 63) (% imm1))
  (pop (% imm0))                        ;hi
  (shl ($ (- 28 x8664::fixnumshift)) (% imm0))
  (or (% imm0) (% imm1))
  (unbox-fixnum lo imm0)
  (or (% imm0) (% imm1))
  (mov (% exp) (% imm0))
  (shl ($ (- ieee-double-float-exponent-offset x8664::fixnumshift)) (% imm0))
  (or (% imm0) (% imm1))
  (pop (% arg_z))
  (mov (% imm1) (@ x8664::double-float.value (% arg_z)))
  (discard-reserved-frame)                  ; discard empty frame
  (single-value-return))


;;; Maybe we should trap - or something - on NaNs.
(defx86lapfunction %%double-float-abs! ((n arg_y)(val arg_z))
  (simple-function-entry)
  (mov (@ x8664::double-float.value (% n)) (% imm0))
  (btr ($ 63) (% imm0))
  (mov (% imm0) (@ x8664::double-float.value (% val)))
  (single-value-return))


(defx86lapfunction %short-float-abs ((n arg_z))
  (simple-function-entry)
  (btr (% 63) n)
  (single-value-return))




(defx86lapfunction dfloat-significand-zeros ((dfloat arg_z))
  (simple-function-entry)
  (movq (@ target::double-float.value (% dfloat)) (% imm1))
  (shl ($ (1+ IEEE-double-float-exponent-width)) (% imm1))
  (bsrq (% imm1) (% imm0))
  (xorq ($ (1- target::nbits-in-word)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; This exploits the fact that the single float is already
;;; shifted left 32 bits.  We don't want to count the tag
;;; bit as significant, so bash the argument into a fixnum
;;; first.
(defx86lapfunction sfloat-significand-zeros ((sfloat arg_z))
  (simple-function-entry)
  (xorb (%b sfloat) (%b sfloat))
  (shl ($ (1+ IEEE-single-float-exponent-width)) (% sfloat))
  (bsrq (% imm1) (% imm0))
  (xorq ($ (1- target::nbits-in-word)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %%scale-dfloat! ((float arg_x)(int arg_y)(result arg_z))
  (simple-function-entry)
  (unbox-fixnum int imm0)
  (get-double-float float fp1)
  (shl ($ IEEE-double-float-exponent-offset) (% imm0))
  (movd (% imm0) (% fp2))
  (mulsd (% fp2) (% fp1))
  (put-double-float fp1 result)
  (single-value-return))

(defx86lapfunction %%scale-sfloat! ((float arg_y)(int arg_z))
  (simple-function-entry)
  (unbox-fixnum int imm0)
  (shl ($ IEEE-double-float-exponent-offset) (% imm0))
  (movd (% imm0) (% fp2))
  (get-single-float float fp1)
  (mulss (% fp2) (% fp1))
  (put-single-float fp1 arg_z)
  (single-value-return))

(defx86lapfunction %copy-double-float ((f1 arg_y) (f2 arg_z))
  (simple-function-entry)
  (get-double-float f1 fp1)
  (put-double-float fp1 f2)
  (single-value-return))

(defx86lapfunction %short-float->double-float ((src arg_y) (result arg_z))
  (simple-function-entry)
  (get-single-float (% fp1) src)
  (cvtss2sd (% fp1) (% fp1))
  (put-double-float (% fp1) result)
  (single-value-return))

(defx86lapfunction %double-float->short-float ((src arg_z))
  (simple-function-entry)
  (get-double-float fp1 src)
  (cvtsd2ss (% fp1) (% fp1))
  (put-single-float fp1 arg_z)
  (single-value-return))

(defx86lapfunction %int-to-sfloat ((int arg_z))
  (simple-function-entry)
  (int-to-single int imm0 fp1)
  (put-single-float fp1 arg_z)
  (single-value-return))
  

(defx86lapfunction %int-to-dfloat ((int arg_y) (dfloat arg_z))
  (simple-function-entry)
  (int-to-double int imm0 fp1)
  (put-double-float fp1 arg_z)
  (single-value-return))



;;; Manipulate the MXCSR.  It'll fit in a fixnum, but we have to
;;; load and store it through memory.  On x8664, we can hide the
;;; 32-bit MXCSR value in a fixnum on the stack; on a 32-bit x86,
;;; we might need to use a scratch location in the TCR or something.

;;; Return the MXCSR as a fixnum
(defx86lapfunction %get-mxcsr ()
  (simple-function-entry)
  (pushq ($ '0))
  (ldmxcsr (@ 4 (% rsp)))
  (pop (% arg_z))
  (shr ($ (- 32 x8664::fixnumshift)) (% arg_z))
  (single-value-return))

;;; Store the fixnum in arg_z in the MXCSR.  Just to be
;;; on the safe side, mask the arg with X86::MXCSR-WRITE-MASK,
;;; so that only known control and status bits are written to.
(defx86lapfunction %set-mxcsr ((val arg_z))
  (simple-function-entry)
  (mov (% val) (% temp0))
  (andl ($ x86::mxcsr-write-mask) (%l temp0))
  (shl ($ (- 32 x8664::fixnumshift)) (% temp0))
  (push (% temp0))
  (stmxcsr (@ 4 (% rsp)))
  (add ($ '1) (% rsp))
  (single-value-return))


;;; Get the bits that contain exception masks and rounding mode.

(defun %get-mxcsr-control ()
  (ldb x86::mxcsr-control-and-rounding-byte (the fixnum (%get-mxcsr))))

;;; Get the bits that describe current exceptions.
(defun %get-mxcsr-status ()
  (ldb x86::mxcsr-status-byte (the fixnum (%get-mxcsr))))

;;; Set the bits that describe current exceptions, presumably to clear them.
(defun %set-mxcsr-status (arg)
  (%set-mxcsr (dpb arg x86::mxcsr-status-byte (%get-mxcsr))))

;;; Set the bits that mask/unmask exceptions and control rounding.
;;; Clear the bits which describe current exceptions.
(defun %set-mxcsr-status (arg)
  (%set-mxcsr (dpb arg x86::mxcsr-status-byte 0)))

;;; Return the MXCSR value in effect after the last ff-call.
(defx86lapfunction %get-post-ffi-mxcsr ()
  (simple-function-entry)
  (xor (% arg_z) (% arg_z))
  (movl (@ (% rcontext) x8664::tcr.ffi-exception) (%l imm0))
  (movl (%l arg_z) (@ (% rcontext) x8664::tcr.ffi-exception))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; Return the status bits from the last ff-call that represent
;;; unmasked exceptions
(defun %ffi-exception-status ()
  (logandc2 (%get-post-ffi-mxcsr)
            (ldb mxcsr-control-byte (%get-mxcsr))))
  

;;; See if the binary double-float operation OP set any enabled
;;; exception bits in the mxcsr
(defun %df-check-exception-2 (operation op0 op1 fp-status)
  (declare (type (unsigned-byte 6) fp-status))
  (unless (zerop fp-status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status 
			   operation 
			   (%copy-double-float op0 (%make-dfloat)) 
			   (%copy-double-float op1 (%make-dfloat)))))

(defun %sf-check-exception-2 (operation op0 op1 fp-status)
  (declare (type (unsigned-byte 6) fp-status))
  (unless (zerop fp-status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status 
			   operation
			   #+32-bit-target
			   (%copy-short-float op0 (%make-sfloat))
			   #+64-bit-target op0
			   #+32-bit-target
			   (%copy-short-float op1 (%make-sfloat))
			   #+64-bit-target op1)))

(defun %df-check-exception-1 (operation op0 fp-status)
  (declare (fixnum fp-status))
  (unless (zerop status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status 
                           operation 
                           (%copy-double-float op0 (%make-dfloat)))))

(defun %sf-check-exception-1 (operation op0 fp-status)
  (declare (type (unsigned-byte 6) fp-status))
  (unless (zerop status)
    (%set-mxcsr-status 0)
    ;; Ensure that operands are heap-consed
    (%fp-error-from-status fp-status 
			   operation
			   #+32-bit-target
			   (%copy-short-float op0 (%make-sfloat))
			   #+64-bit-target op0)))


(defun fp-condition-from-mxcsr (status-bits)
  (declare (fixnum status-bits))
  (cond 
   ((logbitp x86::mxcsr-ie-bit status-bits)
    'floating-point-invalid-operation)
   ((logbitp x86::mxcsr-oe-bit status-bits)
    'floating-point-overflow)
   ((logbitp x86::mxcsr-ue-bit status-bits)
    'floating-point-underflow)
   ((logbitp x86::mxcsr-ze-bit status-bits)
    'division-by-zero)
   ((logbitp x86::mxcsr-pe-bit status-bits)
    'floating-point-inexact)))

;;; This assumes that one of {ie ze oe ue} is set.
(defun %fp-error-from-status (status-bits  operation &rest operands)
  (declare (type (unsigned-byte 6) status-bits))
  (let* ((condition-class (fp-condition-from-mxcsr status-bits)))
    (if condition-class
      (error (make-instance condition-class
               :operation operation
               :operands operands)))))

(defun fp-minor-opcode-operation (minor-opcode)
  (case minor-opcode
    (25 '*)
    (18 '/)
    (20 '-)
    (21 '+)
    (t 'unknown)))

;;; Don't we already have about 20 versions of this ?
(defx86lapfunction %double-float-from-macptr! ((ptr arg_x) (byte-offset arg_y) (dest arg_z))
  (ldr imm0 target::macptr.address ptr)
  (unbox-fixnum imm1 byte-offset)
  (lfdx fp1 imm0 imm1)
  (put-double-float fp1 dest)
  (blr))


(defvar *rounding-mode-alist*
  '((:nearest . 0) (:zero . 1) (:positive . 2) (:negative . 3)))

(defun get-fpu-mode (&optional (mode nil mode-p))
  (let* ((flags (%get-mxcsr-control)))
    (declare (type (unsigned-byte 8) flags))
    (let* ((rounding-mode
            (car (nth (logand (ash flags
                                   (- x86::mxcsr-om-bit
                                      x86::mxcsr-control-bit-shift))
                              3)
                      *rounding-mode-alist*)))
           (overflow (not (logbitp (- x86::mxcsr-om-bit
                                    x86::mxcsr-control-bit-shift) flags)))
           (underflow (not (logbitp (- x86::mxcsr-um-bit
                                     x86::mxcsr-control-bit-shift) flags)))
           (division-by-zero (not (logbitp (- x86::mxcsr-zm-bit
                                              x86::mxcsr-control-bit-shift) flags)))
           (invalid (not (logbitp (- x86::mxcsr-im-bit
                                   x86::mxcsr-control-bit-shift) flags)))
           (inexact (not (logbitp (- x86::mxcsr-pm-bit
                                   x86::mxcsr-control-bit-shift) flags))))
    (if mode-p
      (ecase mode
        (:rounding-mode rounding-mode)
        (:overflow overflow)
        (:underflow underflow)
        (:division-by-zero division-by-zero)
        (:invalid invalid)
        (:inexact inxexact))
      `(:rounding-mode ,rounding-mode
        :overflow ,overflow
        :underflow ,underflow
        :division-by-zero ,division-by-zero
        :invalid ,invalid
        :inexact ,inexact)))))

;;; did we document this?
(defun set-fpu-mode (&key (rounding-mode :nearest rounding-p)
                          (overflow t overflow-p)
                          (underflow t underflow-p)
                          (division-by-zero t zero-p)
                          (invalid t invalid-p)
                          (inexact t inexact-p))
  (let* ((mask (logior (if rounding-p
                         (ash #x03
                              (- x86::mxcsr-om-bit
                                 x86::mxcsr-control-bit-shift))
                         #x00)
                       (if invalid-p
                         (ash 1 (- x86::mxcsr-im-bit
                                   x86::mxcsr-control-bit-shift))
                         #x00)
                       (if overflow-p
                         (ash 1 (- x86::mxcsr-om-bit
                                   x86::mxcsr-control-bit-shift))
                         #x00)
                       (if underflow-p
                         (ash 1 (- x86::mxcsr-um-bit
                                   x86::mxcsr-control-bit-shift))
                         #x00)
                       (if zero-p
                         (ash 1 (- x86::mxcsr-zm-bit
                                   x86::mxcsr-control-bit-shift))
                         #x00)
                       (if inexact-p
                         (ash 1 (- x86::mxcsr-pm-bit
                                   x86::mxcsr-control-bit-shift))
                         #x00)))
         (new (logior (ash (or
                            (cdr (assoc rounding-mode *rounding-mode-alist*))
                            (error "Unknown rounding mode: ~s" rounding-mode))
                           (- x86::mxcsr-om-bit
                              x86::mxcsr-control-bit-shift))
                      (if invalid 0 (ash 1 (- x86::mxcsr-im-bit
                                              x86::mxcsr-control-bit-shift)))
                      (if overflow 0 (ash 1 (- x86::mxcsr-om-bit
                                               x86::mxcsr-control-bit-shift)))
                      (if underflow 0 (ash 1 (- x86::mxcsr-um-bit
                                                x86::mxcsr-control-bit-shift)))
                      (if division-by-zero 0 (ash 1 (- x86::mxcsr-zm-bit
                                                       x86::mxcsr-control-bit-shift)))
                      (if inexact 0 (ash 1 (ash 1 (- x86::mxcsr-pm-bit
                                                     x86::mxcsr-control-bit-shift)))))))
    (declare (type (unsigned-byte 8) new mask))
    (%set-mscsr-control (logior (logand new mask)
                                (logandc2 (%get-mxcsr-control) mask)))))


;;; Copy a single float pointed at by the macptr in single
;;; to a double float pointed at by the macptr in double

(defx86lapfunction %single-float-ptr->double-float-ptr ((single arg_y) (double arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 single)
  (lfs fp0 0 imm0)
  (macptr-ptr imm0 double)
  (stfd fp0 0 imm0)
  (blr))

;;; Copy a double float pointed at by the macptr in double
;;; to a single float pointed at by the macptr in single.
(defx86lapfunction %double-float-ptr->single-float-ptr ((double arg_y) (single arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 double)
  (lfd fp0 0 imm0)
  (macptr-ptr imm0 single)
  (stfs fp0 0 imm0)
  (blr))


(defx86lapfunction %set-ieee-single-float-from-double ((src arg_y) (macptr arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 macptr)
  (get-double-float fp1 src)
  (stfs fp1 0 imm0)
  (blr))

#+x866-target
(defx86lapfunction host-single-float-from-unsigned-byte-32 ((u32 arg_z))
  (simple-function-entry)
  (shl ($ (- 32 x8664::fixnumshift)) (% arg_z))
  (movb ($ x8664::subtag-single-float) (% arg_z.b))
  (single-value-return))

(defx86lapfunction single-float-bits ((f arg_z))
  (simple-function-entry)
  (shr ($ (- 32 x8664::fixnumshift)) (% f))
  (single-value-return))

(defun double-float-bits (f)
  (values (uvref f target::double-float.val-high-cell)
          (uvref f target::double-float.val-low-cell)))

(defun double-float-from-bits (high low)
  (let* ((f (%make-dfloat)))
    (setf (uvref f target::double-float.val-hig-cell) high
          (uvref f target::double-float.val-low-cell) low)
    f))

;;; Return T if n is negative, else NIL.
#+x8664-target
(defx86lapfunction %double-float-sign ((n arg_z))
  (simple-function-entry)
  (movl (@ x8664::double-float.val-high (% n)) (% imm0.l))
  (testl (% imm0.l) (% imm0.l))
  (movl ($ x8664::t-value) (% imm0.l))
  (movl ($ x8664::nil-value) (% arg_z.l))
  (cmovlq (% imm0) (% arg_z))
  (single-value-return))

#+x8664-target
(defx86lapfunction %short-float-sign ((n arg_z))
  (simple-function-entry)
  (testq (% n) (% n))
  (movl ($ x8664::t-value) (% imm0.l))
  (movl ($ x8664::nil-value) (% arg_z.l))
  (cmovlq (% imm0) (% arg_z))
  (single-value-return))
  

