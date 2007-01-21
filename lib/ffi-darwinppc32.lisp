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

;;; If a record type has a single scalar field, return the type
;;; of that field.
(defun darwin32::record-type-has-single-scalar-field (record-type)
  (when (typep record-type 'foreign-structure-type)
    (ensure-foreign-type-bits record-type)
    (let* ((fields (foreign-record-type-fields record-type)))
      (when (null (cdr fields))
        (let* ((f0 (car fields))
               (type (foreign-record-field-type f0)))
          (typecase type
            ((or foreign-record-type foreign-array-type) nil)
            (otherwise type)))))))

;;; If type denotes a foreign record type, return T if it would
;;; be "returned" by passing it as the first argument to the callee.
;;; On DarwinPPC32, this is true of all record types except for
;;; those for which RECORD-TYPE-HAS-SINGLE-SCALAR-FIELD returns
;;; true.
(defun darwin32::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (and (typep ftype 'foreign-record-type)
           (not (darwin32::record-type-has-single-scalar-field ftype))))))


;;; Structures that contain a single scalar field are "returned"
;;; as a value with that field's type.
;;; Other structures are "returned" by passing a pointer to a structure
;;; of the appropriate type as the first argument.
;;; Structures that contain a single scalar field are passed by value
;;; by passing the value of that field as a scalar.
;;; Structures that contain more than one field are passed by value
;;; as a sequence of N 32-bit words; %ff-call understands an unsigned
;;; integer argument "type" specifier to denote this.

(defun darwin32::expand-ff-call (callform args)
  (let* ((result-type-spec (or (car (last args)) :void))
         (enclosing-form nil))
    (multiple-value-bind (result-type error)
        (parse-foreign-type result-type-spec)
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (let* ((single-scalar (darwin32::record-type-has-single-scalar-field result-type))
                 (result-form (pop args)))
            (if single-scalar
              (progn
                (setq enclosing-form `(setf ,(%foreign-access-form result-form single-scalar 0 nil))
                      result-type single-scalar
                      result-type-spec (foreign-type-to-representation-type result-type)))
                      
              (progn
                (argforms :address)
                (argforms result-form)
                (setq result-type *void-foreign-type*
                      result-type-spec :void)))))
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
                  (let* ((single-scalar (darwin32::record-type-has-single-scalar-field ftype)))
                    (if single-scalar
                      (progn
                        (argforms (foreign-type-to-representation-type single-scalar))
                        (argforms (%foreign-access-form arg-value-form single-scalar 0 nil)))
                      (let* ((bits (ensure-foreign-type-bits ftype)))
                        (argforms (ceiling bits 32))
                        (argforms arg-value-form))))
                  (progn
                    (argforms (foreign-type-to-representation-type ftype))
                    (argforms arg-value-form)))))))
        (argforms (foreign-type-to-representation-type result-type))
        (let* ((call `(,@callform ,@(argforms))))
          (if enclosing-form
            `(,@enclosing-form ,call)
            call))))))
                  
            
            
                          