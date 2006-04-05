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
  (defconstant gp-regs-offset (/ (+ (get-field-offset :ucontext.uc_mcontext)
                                    (get-field-offset :mcontext_t.gregs))
                                 8))
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
      ))
  (defun indexed-gpr-lisp (xp igpr)
    (%get-object xp (+ gp-regs-offset (ash igpr x8664::word-shift))))
  (defun (setf indexed-gpr-lisp) (new xp igpr)
    (%set-object xp (+ gp-regs-offset (ash igpr x8664::word-shift)) new))
  (defun encoded-gpr-lisp (xp gpr)
    (indexed-gpr-lisp xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
  (defun (setf encoded-gpr-lisp) (new xp gpr)
    (setf (indexed-gpr-lisp xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new))
  (defun indexed-gpr-integer (xp igpr)
    (%get-signed-long-long xp (+ gp-regs-offset (ash igpr x8664::word-shift))))
  (defun (setf indexed-gpr-integer) (new xp igpr)
    (setf
     (%get-signed-long-long xp (+ gp-regs-offset (ash igpr x8664::word-shift)))
     new))
  (defun encoded-gpr-integer (xp gpr)
    (indexed-gpr-integer xp (aref *encoded-gpr-to-indexed-gpr* gpr)))
  (defun (setf encoded-gpr-integer) (new xp gpr)
    (setf (indexed-gpr-integer xp (aref *encoded-gpr-to-indexed-gpr* gpr)) new)))
      
      


(defun handle-gc-hooks ()
  (let ((bits *gc-event-status-bits*))
    (declare (fixnum bits))
    (cond ((logbitp $gc-postgc-pending-bit bits)
           (setq *gc-event-status-bits*
                 (logand (lognot (ash 1 $gc-postgc-pending-bit))
                         bits))
           (let ((f *post-gc-hook*))
             (when (functionp f) (funcall f)))))))



(defcallback xcmain ()
  (cmain))


