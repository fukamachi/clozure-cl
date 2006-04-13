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

(defun xp-argument-list (xp)
  (let ((nargs (ldb (byte (- 16 x8664::fixnumshift) 0)
                    (encoded-gpr-lisp xp x8664::nargs.q)))
        (arg-x (encoded-gpr-lisp xp x8664::arg_x))
        (arg-y (encoded-gpr-lisp xp x8664::arg_y))
        (arg-z (encoded-gpr-lisp xp x8664::arg_z)))
    (cond ((eql nargs 0) nil)
          ((eql nargs 1) (list arg-z))
          ((eql nargs 2) (list arg-y arg-z))
          (t (let ((args (list arg-x arg-y arg-z)))
               (if (eql nargs 3)
                 args
                 (let ((sp (encoded-gpr-macptr xp x8664::rsp)))
                   (dotimes (i (- nargs 3))
                     (push (%get-object sp (* i target::node-size)) args))
                   args)))))))

(defun handle-udf-call (xp frame-ptr)
  (let* ((args (xp-argument-list xp))
         (values (multiple-value-list
                  (%kernel-restart-internal
                   $xudfcall
                   (list (encoded-gpr-lisp xp x8664::fname) args)
                   frame-ptr)))
         (stack-argcnt (max 0 (- (length args) 3)))
         (rsp (%i+ (encoded-gpr-lisp xp x8664::rsp)
                   (if (zerop stack-argcnt)
                     0
                     (+ stack-argcnt 2))))
         (f #'(lambda (values) (apply #'values values))))
    (setf (encoded-gpr-lisp xp x8664::rsp) rsp
          (encoded-gpr-lisp xp x8664::nargs.q) 1
          (encoded-gpr-lisp xp x8664::arg_z) values
          (encoded-gpr-lisp xp x8664::fn) f)
    ;; 16 is REG_RIP, at least on Linux.
    (setf (indexed-gpr-lisp xp 16) f)))
  
(defcallback %xerr-disp (:address xp
                                  :int fn-reg
                                  :int relative-pc
                                  :unsigned-byte op0
                                  :unsigned-byte op1
                                  :unsigned-byte op2
                                  :int)
  (declare (type (unsigned-byte 8) op0 op1 op2 fn-reg))
  (let* ((fn (unless (eql 0 fn-reg)
               (indexed-gpr-lisp xp fn-reg))))
    (with-xp-stack-frames (xp fn frame-ptr)
      (let* ((skip 2))
        (if (and (= op0 #xcd)
                 (>= op1 #x80))
          (cond ((< op1 #x90)
                 (setq skip 3)
                 (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op1))
                       (%slot-unbound-trap
                        (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                        (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                        frame-ptr)))
                ((< op1 #xa0)
                 ;; #x9x - register X is a symbol.  It's unbound.
                 (%kernel-restart-internal $xvunbnd
                                           (list
                                            (encoded-gpr-lisp
                                             xp
                                             (ldb (byte 4 0) op1)))
                                           frame-ptr))
                ((< op1 #xb0)
                 (%err-disp-internal $xfunbnd (encoded-gpr-lisp
                                               xp
                                               (ldb (byte 4 0) op1))
                                     frame-ptr))
                ((< op1 #xc0)
                 (set skip 3)
                 (%err-disp-internal 
                  #.(car (rassoc 'type-error *kernel-simple-error-classes*))
                  (list (encoded-gpr-lisp
                         xp
                         (ldb (byte 4 0) op1))
                        (logand op2 63))
                  frame-ptr))
                ((= op1 #xc0)
                 (%error 'too-few-arguments
                         (list :nargs (ash (ldb (byte 16 0)
                                                (encoded-gpr-lisp
                                                 xp
                                                 x8664::nargs.q))
                                           (- x8664::word-shift))
                               :fn fn)
                         frame-ptr))
                ((= op1 #xc1)
                 (%error 'too-many-arguments
                         (list :nargs (ash (ldb (byte 16 0)
                                                (encoded-gpr-lisp
                                                 xp
                                                 x8664::nargs.q))
                                           (- x8664::word-shift))
                               :fn fn)
                         frame-ptr))
                ((= op1 #xc2)
                 ;;; FIX THIS: determine too-many/too-few from flags.
                 (%error 'too-many-arguments
                         (list :nargs (ash (ldb (byte 16 0)
                                                (encoded-gpr-lisp
                                                 xp
                                                 x8664::nargs.q))
                                           (- x8664::word-shift))
                               :fn fn)
                         frame-ptr))
                ;; #xc3 is nominally a stack overflow; unused.
                ((= op1 #xc6)
                 (%error (make-condition 'type-error
                                         :datum (encoded-gpr-lisp xp x8664::temp0)
                                         :expected-type '(or symbol function)
                                         :format-control
                                         "~S is not of type ~S, and can't be FUNCALLed or APPLYed")
                         nil frame-ptr))
                ((= op1 #xc7)
                 (handle-udf-call xp frame-ptr)
                 (setq skip 0))
                ((= op1 #xc8)
                 (setq skip 3)
                 (%error (%rsc-string $xarroob)
                         (list (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                               (encoded-gpr-lisp xp (ldb (byte 4 0) op2)))
                         frame-ptr))
                ((= op1 #xc9)
                 (%err-disp-internal $xnotfun
                                     (list (encoded-gpr-lisp xp x8664::temp0))
                                     frame-ptr))
                ;; #xca = uuo-error-debug-trap
                ((= op1 #xcc)
                 ;; external entry point or foreign variable
                 (setq skip 3)
                 (let* ((eep-or-fv (encoded-gpr-lisp xp (ldb (byte 4 4) op2))))
                   (etypecase eep-or-fv
                     (external-entry-point
                      (resolve-eep eep-or-fv)
                      (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                            (eep.address eep-or-fv)))
                     (foreign-variable
                      (resolve-foreign-variable eep-or-fv)
                      (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                            (fv.addr eep-or-fv))))))
                ((< op1 #xe0)
                 (setq skip 3)
                 (let* ((typename
                         (cond ((= op2 x8664::tag-fixnum) 'fixnum)
                               ((= op2 x8664::tag-single-float) 'single-float)
                               ((= op2 x8664::subtag-character 'character))
                               ((= op2 x8664::fulltag-cons) 'cons)
                               ((= op2 x8664::tag-misc) 'uvector)
                               ((= op2 x8664::fulltag-symbol) 'symbol)
                               ((= op2 x8664::fulltag-function) 'function)
                               (t (let* ((class (logand op2 x8664::fulltagmask))
                                         (high4 (ash op2 (- x8664::ntagbits))))
                                    (cond ((= class x8664::fulltag-nodeheader-0)
                                           (svref *nodeheader-0-types* high4))
                                          ((= class x8664::fulltag-nodeheader-1)
                                           (svref *nodeheader-1-types* high4))
                                          ((= class x8664::fulltag-immheader-0)
                                           (svref *immheader-0-types* high4))
                                          ((= class x8664::fulltag-immheader-1)
                                           (svref *immheader-1-types* high4))
                                          ((= class x8664::fulltag-immheader-2)
                                           (svref *immheader-2-types* high4))
                                          (t (list 'bogus op2))))))))
                   (%error (make-condition 'type-error
                                           :datum (encoded-gpr-lisp
                                                   xp
                                                   (ldb (byte 4 0) op1))
                                           :expected-type typename)
                           nil
                           frame-ptr)))
                ((< op1 #xf0)
                 (%error (make-condition 'type-error
                                         :datum (encoded-gpr-lisp
                                                 xp
                                                 (ldb (byte 4 0) op1))
                                         :expected-type 'list)
                         nil
                         frame-ptr))
                (t
                 (%error (make-condition 'type-error
                                         :datum (encoded-gpr-lisp
                                                 xp
                                                 (ldb (byte 4 0) op1))
                                         :expected-type 'fixnum)
                         nil
                         frame-ptr)))
          (%error "%Unknown trap: #x~x~%xp: ~s, fn: ~s, pc: #x~x"
                  (list (list op0 op1 op2) xp fn relative-pc)
                  frame-ptr))
        skip))))


          
                 
                 
                
                
                 





                    
                
            
