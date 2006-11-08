;;; x86-trap-support
;;;
;;;   Copyright (C) 2005-2006 Clozure Associates and contributors
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

;;; The order in which GPRs appear in an exception context generally
;;; has nothing to do with how they're encoded in instructions/uuos,
;;; and is OS-dependent.

#+linuxx8664-target
(progn
  (defconstant gp-regs-offset (+ (get-field-offset :ucontext.uc_mcontext)
                                 (get-field-offset :mcontext_t.gregs)))
  (defmacro xp-gp-regs (xp) xp)
  (defconstant flags-register-offset #$REG_EFL)
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(13                                ;rax
      14                                ;rcx
      12                                ;rdx
      11                                ;rbx
      15                                ;rsp
      10                                ;rbp
      9                                 ;rsi
      8                                 ;rdi
      0                                 ;r8
      1                                 ;r9
      2                                 ;r10
      3                                 ;r11
      4                                 ;r12
      5                                 ;r13
      6                                 ;r14
      7                                 ;r15
      )))

#+freebsdx8664-target
(progn
  (defconstant gp-regs-offset (get-field-offset :ucontext_t.uc_mcontext))
  (defmacro xp-gp-regs (xp) xp)
  (defconstant flags-register-offset 22)
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(7					;rax
      4					;rcx
      3					;rdx
      8					;rbx
      23                                ;rsp
      9					;rbp
      2                                 ;rsi
      1                                 ;rdi
      5                                 ;r8
      6                                 ;r9
      10				;r10
      11                                ;r11
      12				;r12
      13				;r13
      14				;r14
      15                                ;r15
      )))

#+darwinx8664-target
;;; Apple has decided that compliance with some Unix standard or other
;;; requires gratuitously renaming ucontext/mcontext structures and
;;; their components.  Do you feel more compliant now ?
(progn
  (eval-when (:compile-toplevel :execute)
    (def-foreign-type nil
        (:struct :portable_mcontext64
                 (:es :x86_exception_state64_t)
                 (:ss :x86_thread_state64_t)
                 (:fs :x86_float_state64_t)))
    (def-foreign-type nil
        (:struct :portable_uc_stack
                 (:ss_sp (:* :void))
                 (:ss_size (:unsigned 64))
                 (:ss_flags  (:signed 32))))
    (def-foreign-type nil
        (:struct :portable_ucontext64
                 (:onstack (:signed 32))
                 (:sigmask (:unsigned 32))
                 (:stack (:struct :portable_uc_stack))
                 (:link :address)
                 (:uc_mcsize (:unsigned 64))
                 (:uc_mcontext64 (:* (:struct :portable_mcontext64))))))
  (defconstant gp-regs-offset 0)
  (defmacro xp-gp-regs (xp)
    `(pref (pref ,xp :portable_ucontext64.uc_mcontext64) :portable_mcontext64.ss))

  (defconstant flags-register-offset 17)
  (defparameter *encoded-gpr-to-indexed-gpr*
    #(0					;rax
      2					;rcx
      3					;rdx
      1					;rbx
      7                                 ;rsp
      6					;rbp
      5                                 ;rsi
      4                                 ;rdi
      8                                 ;r8
      9                                 ;r9
      10				;r10
      11                                ;r11
      12				;r12
      13				;r13
      14				;r14
      15                                ;r15
      )))

(defun indexed-gpr-lisp (xp igpr)
  (%get-object (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift))))
(defun (setf indexed-gpr-lisp) (new xp igpr)
  (%set-object (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift)) new))
(defun encoded-gpr-lisp (xp gpr)
  (indexed-gpr-lisp xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
(defun (setf encoded-gpr-lisp) (new xp gpr)
  (setf (indexed-gpr-lisp xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new))
(defun indexed-gpr-integer (xp igpr)
  (%get-signed-long-long (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift))))
(defun (setf indexed-gpr-integer) (new xp igpr)
  (setf
   (%get-signed-long-long (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift)))
   new))
(defun encoded-gpr-integer (xp gpr)
  (indexed-gpr-integer xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
(defun (setf encoded-gpr-integer) (new xp gpr)
  (setf (indexed-gpr-integer xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new))
(defun indexed-gpr-macptr (xp igpr)
  (%get-ptr (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift))))
(defun (setf indexed-gpr-macptr) (new xp igpr)
  (setf (%get-ptr (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift))) new))
(defun indexed-gpr-macptr (xp igpr)
  (%get-ptr (xp-gp-regs xp) (+ gp-regs-offset (ash igpr x8664::word-shift))))
(defun encoded-gpr-macptr (xp gpr)
  (indexed-gpr-macptr xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
(defun (setf encoded-gpr-macptr) (new xp gpr)
  (setf (indexed-gpr-macptr xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new))
(defun xp-flags-register (xp)
  (%get-signed-long-long (xp-gp-regs xp) (+ gp-regs-offset (ash flags-register-offset x8664::fixnumshift))))
  


(defun %get-xcf-byte (xcf-ptr delta)
  (let* ((containing-object (%get-object xcf-ptr x8664::xcf.containing-object))
         (byte-offset (%get-object xcf-ptr x8664::xcf.relative-pc)))
    (if containing-object
      (locally (declare (optimize (speed 3) (safety 0))
                        (type (simple-array (unsigned-byte 8) (*)) containing-object))
        (aref containing-object (the fixnum (+ byte-offset delta))))
      (%get-unsigned-byte (%int-to-ptr byte-offset) delta))))

                                  
(defun decode-arithmetic-error (xp xcf)
  (declare (ignore xp xcf))
  (values 'unknown nil))

;;; UUOs are handled elsewhere.  This should handle all signals other than
;;; those generated by UUOs (and the non-UUO cases of things like SIGSEGV.)
;;; If the signal number is 0, other arguments (besides the exception context XP)
;;; may not be meaningful.
(defcallback xcmain (:address xp :address xcf :int signal :int code  :int)
  (declare (ignorable rpc))
  (let* ((frame-ptr (macptr->fixnum xcf)))
    (cond ((zerop signal)               ;thread interrupt
           (cmain))
          ((= signal #$SIGFPE)
           (multiple-value-bind (operation operands)
               (decode-arithmetic-error xp xcf)
             (let* ((condition-name
                     (cond ((or (= code #$FPE_INTDIV)
                                (= code #$FPE_FLTDIV))
                            'division-by-zero)
                           ((= code #$FPE_FLTOVF)
                            'floating-point-overflow)
                           ((= code #$FPE_FLTUND)
                            'floating-point-underflow)
                           ((= code #$FPE_FLTRES)
                            'floating-point-inexact)
                           (t
                            'floating-point-invalid-operation))))
               (%error (make-condition condition-name
                                       :operation operation
                                       :operands operands)
                       ()
                       frame-ptr))))
          ((= signal #$SIGSEGV)
           ;; Stack overflow.
           (let* ((on-tsp (not (eql 0 code))))
           (unwind-protect
                (%error
                 (make-condition
                  'stack-overflow-condition 
                  :format-control "Stack overflow on ~a stack."
                  :format-arguments (list
                                     (if on-tsp "temp" "value"))
                                     )
                 nil frame-ptr)
             (ff-call (%kernel-import target::kernel-import-restore-soft-stack-limit)
                      :unsigned-fullword code
                      :void))))))
  0)

