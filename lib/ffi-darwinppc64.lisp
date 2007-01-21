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

;;; On DarwinPPC64:
;;; Structures whose size is exactly 16 bytes are passed in 2 GPRs,
;;; regardless of the types of their elements, when they are passed
;;; by value.
;;; Structures which contain unions are passed in N GPRs when passed
;;; by value
;;; All other structures passed by value are passed by passing their
;;; constituent elements as scalars.  (For bitfields, the containing
;;; integer counts as a constituent element.)
;;; Structures whose size is exactly 16 bytes are returned in GPR3
;;; and GPR4.
;;; Structures which contain unions are "returned" by passing a pointer
;;; to a structure instance in the first argument.
;;; All other structures are returned by returning their constituent
;;; elements as scalars.  (Note that - in some cases - we may need
;;; to reserve space in the foreign stack frame to handle scalar
;;; return values that don't fit in registers.  Need a way to tell
;;; %ff-call about this, as well as runtime support.)


(defun darwin64::record-type-contains-union (rtype)
  ;;; RTYPE is a FOREIGN-RECORD-TYPE object.
  ;;; If it, any of its fields, or any fields in an
  ;;; embedded structure or array field is a union,
  ;;; return true.
  ;;; (If this function returns true, we can't
  ;;; pass a structure of type RTYPE - or return one -
  ;;; by passing or returning the values of all of
  ;;; its fields, since some fields are aliased.
  ;;; However, if the record's size is exactly 128
  ;;; bits, we can pass/return  it in two GPRs.)
  (ensure-foreign-type-bits rtype)
  (or (eq (foreign-record-type-kind rtype) :union)
      (dolist (f (foreign-record-type-fields rtype))
        (let* ((fieldtype (foreign-record-field-type f)))
          (if (and (typep fieldtype 'foreign-record-type)
                   (darwin64::record-type-contains-union fieldtype))
            (return t))
          (if (typep fieldtype 'foreign-array-type)
            (let* ((atype (foreign-array-type-element-type fieldtype)))
              (if (and (typep atype 'foreign-record-type)
                       (darwin64::record-type-contains-union atype))
                (return t))))))))

;;; On DarwinPPC64, we only have to pass a structure as a first
;;; argument if the type contains a union
(defun darwin64::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (and (typep ftype 'foreign-record-type)
           (not (= (ensure-foreign-type-bits ftype) 128))
           (darwin64::record-type-contains-union ftype)))))

;;; Generate code to set the fields in a structure R of record-type
;;; RTYPE, based on the register values in REGBUF (8 64-bit GPRs,
;;; followed by 13 64-bit GPRs.)
;;; This also handles the 16-byte structure case.
;;; (It doesn't yet handle embedded arrays or bitfields.)
(defun darwin64::struct-from-regbuf-values (r rtype regbuf)
  (let* ((bits (ccl::ensure-foreign-type-bits rtype)))
    (collect ((forms))
      (cond ((= bits 128)               ;(and (eql day 'tuesday) ...)
             (forms `(setf (ccl::%%get-signed-longlong ,r 0)
                      (ccl::%%get-signed-longlong ,regbuf 0)
                      (ccl::%%get-signed-longlong ,r 8)
                      (ccl::%%get-signed-longlong ,regbuf 8))))
            (t
             (let* ((gpr-offset 0)
                    (fpr-offset (* 8 8)))
               (flet ((next-gpr-offset ()
                        (prog1 gpr-offset
                          (incf gpr-offset 8)))
                      (next-fpr-offset ()
                        (prog1 fpr-offset
                          (incf gpr-offset 8)
                          (incf fpr-offset 8))))
                 (labels ((do-fields (fields accessors)
                            (dolist (field fields)
                              (let* ((field-type (foreign-record-field-type field))
                                     (field-accessor-list (append accessors (list (foreign-record-field-name field))))
                                     (valform ()))
                                (etypecase field-type
                                  (foreign-record-type
                                   (do-fields (foreign-record-type-fields field-type)
                                     field-accessor-list))
                                  (foreign-pointer-type
                                   (setq valform
                                         `(%get-ptr ,regbuf ,(next-gpr-offset))))
                                  (foreign-double-float-type
                                   (setq valform
                                         `(%get-double-float  ,regbuf ,(next-fpr-offset))))
                                  (foreign-single-float-type
                                   (setq valform
                                         `(%get-single-float-from-double-ptr
                                           ,regbuf ,(next-fpr-offset))))
                                  (foreign-integer-type
                                   (let* ((bits (foreign-integer-type-bits field-type))
                                          (signed (foreign-integer-type-signed field-type)))
                                     (case bits
                                       (64
                                        (setq valform
                                              `(,(if signed
                                                     '%%get-signed-longlong
                                                     '%%get-unsigned-longlong)
                                                ,regbuf
                                                ,(next-gpr-offset))))
                                       (32
                                        (setq valform
                                              `(,(if signed
                                                     '%get-signed-long
                                                     '%get-unsigned-long)
                                                ,regbuf
                                                (+ 4 ,(next-gpr-offset)))))
                                       (16
                                        (setq valform
                                              `(,(if signed
                                                     '%get-signed-word
                                                     '%get-unsigned-word)
                                                ,regbuf
                                                (+ 6 ,(next-gpr-offset)))))
                                       (8
                                        (setq valform
                                              `(,(if signed
                                                     '%get-signed-byte
                                                     '%get-unsigned-byte)
                                                ,regbuf
                                                (+ 7 ,(next-gpr-offset))))))))
                                  (foreign-array-type
                                   (error "Embedded array-type."))
                                  )
                                (when valform
                                  (forms `(setf ,(%foreign-access-form
                                                  r
                                                  rtype
                                                  0
                                                  field-accessor-list)
                                           ,valform)))))))
                   (do-fields (foreign-record-type-fields rtype) nil ))
                 `(progn ,@(forms) nil))))))))
                                  

(defun darwin64::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
         (regbuf nil)
         (result-temp nil)
         (result-form nil)
         (struct-result-type nil)
         (structure-arg-temp nil))
    (multiple-value-bind (result-type error)
        (parse-foreign-type result-type-spec)
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (eq (car args) :monitor-exception-ports)
          (argforms (pop args)))
        (when (typep result-type 'foreign-record-type)
          (setq result-form (pop args)
                struct-result-type result-type
                result-type *void-foreign-type*
                result-type-spec :void)
          (if (darwin64::record-type-returns-structure-as-first-arg struct-result-type)
            (progn
              (argforms :address)
              (argforms result-form))
            (progn
              (setq regbuf (gensym)
                    result-temp (gensym))
              (argforms :registers)
              (argforms regbuf))))
        (let* ((valform nil))
          (labels ((do-fields (rtype fields accessors)
                     (dolist (field fields)
                       (let* ((field-type (foreign-record-field-type field))
                              (field-accessor-list (append accessors (list (foreign-record-field-name field))))
                              (access-form ()))
                         (typecase field-type
                           (foreign-record-type
                            (do-fields rtype (foreign-record-type-fields field-type) field-accessor-list))
                           ((or foreign-pointer-type foreign-integer-type
                                foreign-single-float-type foreign-double-float-type)
                            (setq access-form
                                  (%foreign-access-form valform rtype 0 field-accessor-list))))
                         (when access-form
                           (argforms (foreign-type-to-representation-type field-type))
                           (argforms access-form)
                           (setq valform structure-arg-temp))))))
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
                      (if (darwin64::record-type-contains-union ftype)
                        (progn
                          (argforms (ceiling (foreign-record-type-bits ftype) 64))
                          (argforms arg-value-form))
                        (progn
                          (unless structure-arg-temp
                            (setq structure-arg-temp (gensym)))
                          (setq valform `(%setf-macptr ,structure-arg-temp ,arg-value-form))
                          (do-fields ftype (foreign-record-type-fields ftype) nil)))
                      (progn
                        (argforms (foreign-type-to-representation-type ftype))
                        (argforms (funcall arg-coerce arg-type-spec arg-value-form))))))))
            (argforms (foreign-type-to-representation-type result-type))
            (let* ((call (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))
              (when structure-arg-temp
                (setq call `(let* ((,structure-arg-temp (%null-ptr)))
                             (declare (dynamic-extent ,structure-arg-temp)
                                      (type macptr ,structure-arg-temp))
                             ,call)))
              (if regbuf
                `(let* ((,result-temp (%null-ptr)))
                  (declare (dynamic-extent ,result-temp)
                           (type macptr ,result-temp))
                  (%setf-macptr ,result-temp ,result-form)
                  (%stack-block ((,regbuf (+ (* 8 8) (* 8 13))))
                    ,call
                    ,(darwin64::struct-from-regbuf-values result-temp struct-result-type regbuf)))
                call))))))))
