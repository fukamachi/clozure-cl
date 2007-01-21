;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2007, Clozure Associates and contributors
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

;;; LinuxPPC64
;;; Structures whose size is less than 64 bits are passed "right-justified"
;;; in a GPR.
;;; Structures passed by value are passed in GPRs as N doublewords.
;;; If the structure would require > 64-bit alignment, this might result
;;; in some GPRs/parameter area words being skipped.  (We don't handle this).
;;; All structures - of any size - are returned by passing a pointer
;;; in the first argument.

(defun linux64::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (typep ftype 'foreign-record-type))))

(defun linux64::expand-ff-call (callform args)
  (let* ((result-type-spec (or (car (last args)) :void)))
    (multiple-value-bind (result-type error)
        (parse-foreign-type result-type-spec)
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (setq result-type *void-foreign-type*
                result-type-spec :void)
          (argforms :address)
          (argforms (pop args)))
        (unless (evenp (length args))
          (error "~s should be an even-length list of alternating foreign types and values" args))        
        (do* ((args args (cddr args)))
             ((null args))
          (let* ((arg-type-spec (car args))
                 (arg-value-form (cadr args)))
            (if (or (member arg-type-spec *foreign-representation-type-keywords*
                            :test #'eq)
                    (typep arg-type-spec 'unsigned-byte))
              (progn
                (argforms arg-type-spec)
                (argforms arg-value-form))
              (let* ((ftype (parse-foreign-type arg-type-spec)))
                (if (typep ftype 'foreign-record-type)
                  (let* ((bits (ensure-foreign-type-bits ftype)))
                    (if (< bits 64)
                      (progn
                        (argforms :unsigned-doubleword)
                        (argforms `(ash (%%get-unsigned-long-long ,arg-value-form) ,(- bits 64))))
                      (progn
                        (argforms (ceiling bits 64))
                        (argforms arg-value-form))))
                  (progn
                    (argforms (foreign-type-to-representation-type ftype))
                    (argforms arg-value-form)))))))
        (argforms (foreign-type-to-representation-type result-type))
        `(,@callform ,@(argforms))))))
