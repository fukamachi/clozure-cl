;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2003-2004 Clozure Associates and contributors.
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
  #+apple-objc
  (use-interface-dir :cocoa)
  #+gnu-objc
  (use-interface-dir :gnustep))

(defpackage "NS"
  (:use))


;;; Force all symbols interned in the NS package to be external
;;; symbols.
(package-force-export "NS")

(defconstant objc-type-flags (byte 3 20))
(defconstant objc-type-index (byte 20 0))
(defconstant objc-flag-instance 0)
(defconstant objc-flag-class 1)
(defconstant objc-flag-metaclass 2)

(defvar *objc-class-class*)
(defvar *objc-metaclass-class*)

(defun recognize-objc-object (p)
  (let* ((idx (objc-class-id p)))
    (if idx
      (%set-macptr-type p (dpb objc-flag-class objc-type-flags idx))
      (if (setq idx (objc-metaclass-id p))
	(%set-macptr-type p (dpb objc-flag-metaclass objc-type-flags idx))
	(when #+apple-objc (not (%null-ptr-p (#_malloc_zone_from_ptr p)))
	      #+gnu-objc t
	      (with-macptrs ((parent (pref p
					   #+apple-objc :objc_object.isa
					   #+gnu-objc :objc_object.class_pointer)))
		(if (setq idx (objc-class-id parent))
		  (%set-macptr-type p idx))))))))

(defun %objc-domain-class-of (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance (id->objc-class index))
      (#.objc-flag-class (id->objc-metaclass index))
      (#.objc-flag-metaclass *objc-metaclass-class*))))

  
(defun %objc-domain-classp (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type)))
    (declare (fixnum type flags))
    (not (= flags objc-flag-instance))))

(defun %objc-domain-instance-class-wrapper (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance (id->objc-class-wrapper index))
      (#.objc-flag-class (id->objc-metaclass-wrapper index))
      (#.objc-flag-metaclass (%class.own-wrapper *objc-metaclass-class*)))))

(defun %objc-domain-class-own-wrapper (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance nil)
      (#.objc-flag-class (id->objc-class-wrapper index))
      (#.objc-flag-metaclass (id->objc-metaclass-wrapper index)))))

(defun %objc-domain-slots-vector (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance nil)	;maybe soon
      (#.objc-flag-class (id->objc-class-slots-vector index))
      (#.objc-flag-metaclass (id->objc-metaclass-slots-vector index)))))
	  
(register-foreign-object-domain :objc
				:recognize #'recognize-objc-object
				:class-of #'%objc-domain-class-of
				:classp #'%objc-domain-classp
				:instance-class-wrapper #'%objc-domain-instance-class-wrapper
				:class-own-wrapper #'%objc-domain-class-own-wrapper
				:slots-vector #'%objc-domain-slots-vector)



(defclass objc:objc-object (foreign-standard-object)
    ())

;;; "Real" OBJC-CLASSes and OBJC-METACLASSEs are subtypes of this
;;; abstract class.  We need to keep track of those classes that're
;;; implemented in lisp separately (so that they can be restored after
;;; SAVE-APPLICATION).

(defclass objc:objc-class-object (foreign-class objc:objc-object)
    ((foreign :initform t)
     (peer :initform nil)))

(defclass objc:objc-metaclass (objc:objc-class-object)
    ())

(setq *objc-metaclass-class* (find-class 'objc:objc-metaclass))

(defclass objc:objc-class (objc:objc-class-object)
    ())

(defmethod objc-metaclass-p ((c class))
  nil)

(defmethod objc-metaclass-p ((c objc:objc-class-object))
  (%objc-metaclass-p c))


(defmethod print-object ((c objc:objc-class) stream)
  (print-unreadable-object (c stream)
    (format stream "~s ~:[~;[MetaClass] ~]~s (#x~x)" 'objc:objc-class (objc-metaclass-p c) (class-name c) (%ptr-to-int c))))

(defmethod print-object ((c objc:objc-metaclass) stream)
  (print-unreadable-object (c stream)
    (format stream "~s ~s (#x~x)" 'objc:objc-metaclass (class-name c) (%ptr-to-int c))))

(defmethod print-object ((o objc:objc-object) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~a (#x~x)" (nsobject-description o) (%ptr-to-int o))))

(defmethod slot-value-using-class ((class objc:objc-class-object)
				   instance
				   (slotd standard-effective-slot-definition))
  (%std-slot-vector-value (%objc-domain-slots-vector instance) slotd))

(defmethod slot-boundp-using-class ((class objc:objc-class-object)
				    instance
				    (slotd standard-effective-slot-definition))
  (%std-slot-vector-boundp (%objc-domain-slots-vector instance) slotd))

(defmethod (setf slot-value-using-class)
    (new
     (class objc:objc-class-object)
     instance
     (slotd standard-effective-slot-definition))
  (%set-std-slot-vector-value (%objc-domain-slots-vector instance) slotd new))


(defun initialize-objc-class-slots (class class-name wrapper &optional foreign)
;  (format t "~&initialize-objc-class-slots ~s" class-name)
;  (force-output)
  (let* ((super (pref class :objc_class.super_class))
	 (super-id (unless (%null-ptr-p super) (objc-class-id super)))
	 (super-slots (if super-id
			(id->objc-class-slots-vector super-id)
			(instance.slots (find-class 'objc:objc-object))))
	 (super-cpl (%slot-ref super-slots %class.cpl)))
    (gvector :slot-vector
	     class
	     nil			;direct-methods
	     nil			;prototype
	     class-name
	     (cons class super-cpl)	;cpl
	     wrapper
	     (list (slot-vector.instance super-slots)) ;local-supers
	     nil ;subclasses
	     nil ;dependents
	     (make-class-ctype class) ;ctype
	     nil ;direct-slots
	     nil ;slots
	     t ;kernel-p
	     nil ;local-default-initargs
	     nil ;default-initargs
	     nil ;alist
	     foreign
	     nil
	     )))

(defun initialize-objc-metaclass-slots (class class-name wrapper &optional foreign peer)
;  (format t "~&initialize-objc-metaclass-slots ~s" class-name)
;  (force-output)
  (let* ((super (pref class :objc_class.super_class))
	 (super-id (unless (%null-ptr-p super) (objc-metaclass-id super)))
	 (super-slots (if super-id
			(id->objc-metaclass-slots-vector super-id)
			(instance.slots (find-class 'objc:objc-class))))
	 (super-cpl (%slot-ref super-slots %class.cpl))
	 (eslotds (class-slots (find-class 'objc:objc-class))))
    (setup-slot-lookup wrapper eslotds)
    (gvector :slot-vector
	     class
	     nil			;direct-methods
	     nil			;prototype
	     class-name
	     (cons class super-cpl)	;cpl
	     wrapper
	     (list (slot-vector.instance super-slots)) ;local-supers
	     nil ;subclasses
	     nil ;dependents
	     (make-class-ctype class) ;ctype
	     nil ;direct-slots
	     eslotds ;slots
	     t ;kernel-p
	     nil ;local-default-initargs
	     nil ;default-initargs
	     nil ;alist
	     foreign
	     peer
	     )))

