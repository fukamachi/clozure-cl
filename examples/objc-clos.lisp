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
;;;
;;; TO DO
;;;  - Issues with OFFSET/LOCATION in foreign direct and effective slot definitions
;;;  - MAP-OBJC-CLASS needs to INITIALIZE-INSTANCE and FINALIZE-INHERITANCE
;;;    for predefined classes
;;;  - OBJC-FOREIGN-ARG-TYPE in BRIDGE.LISP needs to handle BITFIELDs, 
;;;    ARRAYs and UNIONs
;;;  - Need to fully handle init keywords and ObjC init messages
;;;  - Need to add getter and setter functions for more foriegn slot types
;;;  - Canonicalization and retention for ObjC objects
;;;  - Support redef of CLOS parts, but not changes in ObjC parts
;;;  - Provide Lisp structs for NS-POINT, NS-RECT, etc.?

;;; Package and module stuff

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  #+apple-objc
  (use-interface-dir :cocoa)
  #+gnu-objc
  (use-interface-dir :gnustep))

;;; We need OBJC-FOREIGN-ARG-TYPE from the bridge to process ivar types

(require "BRIDGE")

;;; All class names and instance variable names are interned in the NS package
;;; Force all symbols interned in the NS package to be external

(defpackage "NS"
  (:use))

(package-force-export "NS")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                 Testing                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Until foreign slot definition objects automatically show up where they're
;;; supposed to, this function manually sets them up for a given ObjC class 
;;; (and its superclasses)

(defun init-objc-class-slot-definitions (c)
  (unless (eql c (%null-ptr))
    (init-objc-class-slot-definitions (pref c :objc_class.super_class))
    (setf (slot-value c 'direct-slots) (%compute-foreign-direct-slots c))
    (update-slots c (compute-slots c))
    (values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     OBJC Foreign Object Domain                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant objc-type-flags (byte 3 20))
(defconstant objc-type-index (byte 20 0))
(defconstant objc-flag-instance 0)
(defconstant objc-flag-class 1)
(defconstant objc-flag-metaclass 2)

(defvar *objc-class-class*)
(defvar *objc-metaclass-class*)

(defvar *objc-object-slot-vectors* (make-hash-table :test #'eql))

(defun recognize-objc-object (p)
  (let* ((idx (objc-class-id p)))
    (if idx
      (%set-macptr-type p (dpb objc-flag-class objc-type-flags idx))
      (if (setq idx (objc-metaclass-id p))
	(%set-macptr-type p (dpb objc-flag-metaclass objc-type-flags idx))
	(if (setq idx (%objc-instance-class-index p))
	  (%set-macptr-type p idx))))))

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
      (#.objc-flag-instance (gethash p *objc-object-slot-vectors*))
      (#.objc-flag-class (id->objc-class-slots-vector index))
      (#.objc-flag-metaclass (id->objc-metaclass-slots-vector index)))))
	  
(register-foreign-object-domain :objc
				:recognize #'recognize-objc-object
				:class-of #'%objc-domain-class-of
				:classp #'%objc-domain-classp
				:instance-class-wrapper
				#'%objc-domain-instance-class-wrapper
				:class-own-wrapper
				#'%objc-domain-class-own-wrapper
				:slots-vector #'%objc-domain-slots-vector)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ObjC Objects, Classes and Metaclasses                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (format stream "~s ~:[~;[MetaClass] ~]~s (#x~x)" 
	    'objc:objc-class 
	    (objc-metaclass-p c) 
	    (class-name c) 
	    (%ptr-to-int c))))

(defmethod print-object ((c objc:objc-metaclass) stream)
  (print-unreadable-object (c stream)
    (format stream "~s ~s (#x~x)" 
	    'objc:objc-metaclass 
	    (class-name c) 
	    (%ptr-to-int c))))

(defmethod print-object ((o objc:objc-object) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~a (#x~x)" (nsobject-description o) (%ptr-to-int o))))



(defun initialize-objc-class-slots (class class-name wrapper &optional foreign)
;  (format t "~&initialize-objc-class-slots ~s (#x~x)" class-name (%ptr-to-int class))  
; (force-output)
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

(defun initialize-objc-metaclass-slots (class class-name wrapper 
					&optional foreign peer)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              Slot Protocol                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Accessing Lisp slots

(defmethod slot-boundp-using-class ((class objc:objc-class-object)
				    instance
				    (slotd standard-effective-slot-definition))
  (%std-slot-vector-boundp (%objc-domain-slots-vector instance) slotd))

(defmethod slot-value-using-class ((class objc:objc-class-object)
				   instance
				   (slotd standard-effective-slot-definition))
  (%std-slot-vector-value (%objc-domain-slots-vector instance) slotd))

(defmethod (setf slot-value-using-class)
    (new
     (class objc:objc-class-object)
     instance
     (slotd standard-effective-slot-definition))
  (%set-std-slot-vector-value (%objc-domain-slots-vector instance) slotd new))


;;; Metaclasses for foreign slots

(defclass foreign-direct-slot-definition (direct-slot-definition)
  ((foreign :initarg :foreign))
  (:default-initargs :type :id))

(defclass foreign-effective-slot-definition (effective-slot-definition)
  ((getter :type function :accessor foreign-slot-definition-getter)
   (setter :type function :accessor foreign-slot-definition-setter)))


;;; Use the foreign slot metaclasses if the slot has a :FOREIGN attribute
;;  of T

(defmethod direct-slot-definition-class ((class objc:objc-class-object)
					 &rest initargs)
  (if (getf initargs :foreign)
      (find-class 'foreign-direct-slot-definition)
    (find-class 'standard-direct-slot-definition)))

(defmethod effective-slot-definition-class ((class objc:objc-class-object)
					    &rest initargs)
  (if (getf initargs :foreign)
      (find-class 'foreign-effective-slot-definition)
    (find-class 'standard-effective-slot-definition)))


;;; Create FOREIGN-DIRECT-SLOT-DEFINITIONs for each foreign instance variable
;;; in the OBJC-CLASS C

(defun %compute-foreign-direct-slots (c)
  (when (objc-object-p c)
    (with-macptrs ((ivars (pref c :objc_class.ivars)))
      (unless (%null-ptr-p ivars)
	(loop with ns-package = (find-package "NS")
	      with n = (pref ivars :objc_ivar_list.ivar_count)
	      for i from 1 to n
	      for ivar = (pref ivars :objc_ivar_list.ivar_list) 
	          then (%inc-ptr ivar (record-length :objc_ivar))
	      for name = (%get-cstring (pref ivar :objc_ivar.ivar_name))
	      for sym = (compute-lisp-name name ns-package)
	      when (eql (schar name 0) #\_)
	        do (unexport sym ns-package)
	      do (format t "~S: ~S~%" name (pref ivar :objc_ivar.ivar_offset))
	      collect 
	      (make-direct-slot-definition
	       c
	       (list
		:name sym
		:allocation :instance
		:foreign t
		:class (find-class 'foreign-effective-slot-definition)
		:type (objc-foreign-arg-type 
		       (%get-cstring (pref ivar :objc_ivar.ivar_type)))
;		:offset (pref ivar :objc_ivar.ivar_offset)
)))))))
	

(defun %objc-ivar-offset-in-class (name c)
  ;; If C is a non-null ObjC class that contains an instance variable
  ;; named NAME, return that instance variable's offset,  else return
  ;; NIL.
  (when (objc-class-p c)
    (with-macptrs ((ivars (pref c :objc_class.ivars)))
      (unless (%null-ptr-p ivars)
	(loop with n = (pref ivars :objc_ivar_list.ivar_count)
	      for i from 1 to n
	      for ivar = (pref ivars :objc_ivar_list.ivar_list) 
	          then (%inc-ptr ivar (record-length :objc_ivar))
	      when (string= name (%get-cstring (pref ivar :objc_ivar.ivar_name)))
	        do (return-from %objc-ivar-offset-in-class (pref ivar :objc_ivar.ivar_offset)))))))

(defun %objc-ivar-offset (name c)
  (labels ((locate-objc-slot (name class)
	     (unless (%null-ptr-p class)
		 (or (%objc-ivar-offset-in-class name class)
		     (with-macptrs ((super (pref class :objc_class.super_class)))
		       (unless (or (%null-ptr-p super) (eql super class))
			 (locate-objc-slot name super)))))))
    (when (objc-class-p c)
      (or (locate-objc-slot name c)
	  (error "No ObjC instance variable named ~S in ~S" name c)))))
  

;;; Return the getter and setter functions for a foreign slot
;;; NOTE: Should be changed to use FOREIGN-TYPE-TO-REPRESENTATION-TYPE

(defclass unhandled-foreign-slot ()
    ((ftype :initarg :ftype :accessor unhandled-foreign-slot-ftype)))

(defmethod print-object ((ufs unhandled-foreign-slot) stream)
  (print-unreadable-object (ufs stream :type t :identity t)
    (format stream "for foreign type ~s" (unhandled-foreign-slot-ftype ufs))))

(defun compute-foreign-slot-accessors (eslotd)
  (let ((ftype (%slot-definition-type eslotd)))
    (flet ((unhandled-foreign-slot-type (ptr &optional offset)
	     (declare (ignore ptr offset))
             (make-instance 'unhandled-foreign-slot :ftype ftype)))
      (case ftype
	(:unsigned-byte (values #'%get-unsigned-byte #'%set-byte))
	(:signed-byte (values #'%get-signed-byte #'%set-byte))
	(:unsigned-word (values #'%get-unsigned-word #'%set-word))
	(:signed-word (values #'%get-signed-word #'%set-word))
	(:unsigned-fullword (values #'%get-unsigned-long #'%set-long))
	(:signed-fullword (values #'%get-signed-long #'%set-long))
	(:unsigned-longlong 
	 (values #'%%get-unsigned-longlong #'%%set-unsigned-longlong))
	(:signed-longlong 
	 (values #'%%get-signed-longlong #'%%set-signed-longlong))
	(:single-float (values #'%get-single-float #'%set-single-float))
	(:double-float (values #'%get-double-float #'%set-double-float))
	((:id :address) (values #'%get-ptr #'%set-ptr))
	(t 
	 (cond 
	  ((and (consp ftype) (eq (first ftype) :*))
	   (values #'%get-ptr #'%set-ptr))
	  (t (values #'unhandled-foreign-slot-type 
		     #'unhandled-foreign-slot-type))))))))


;;; Augment SLOT-CLASS's COMPUTE-EFFECTIVE-SLOT-DEFINITION with an :AROUND
;;; method for OBJC-CLASSes that sets up foreign slot info

(defmethod compute-effective-slot-definition :around ((class objc:objc-class-object)
						      name
						      direct-slots)
  (let* ((first (first direct-slots))
	 (eslotd (call-next-method)))
    (when (typep first 'foreign-direct-slot-definition)
      (setq eslotd
	    (make-effective-slot-definition
	     class
	     :name name
	     :allocation (%slot-definition-allocation eslotd)
	     :foreign t
	     :documentation (%slot-definition-documentation eslotd)
	     :class (%slot-definition-class first)
	     :initargs (%slot-definition-initargs eslotd)
	     :initfunction (%slot-definition-initfunction eslotd)
	     :initform (%slot-definition-initform eslotd)
	     :type (%slot-definition-type eslotd)))
      (multiple-value-bind (getter setter) (compute-foreign-slot-accessors eslotd)
	(setf (foreign-slot-definition-getter eslotd) getter)
	(setf (foreign-slot-definition-setter eslotd) setter)))
    eslotd))


;;; Determine the location of each slot

(defmethod compute-slots :around ((class objc:objc-class-object))
  (flet ((foreign-slot-p (s) (typep s 'foreign-effective-slot-definition)))
    (let* ((cpl (%class-precedence-list class))
	   (slots (call-next-method))
	   (instance-slots 
	    (remove-if #'foreign-slot-p 
		       (remove :class slots :key #'%slot-definition-allocation)))
	   (class-slots (remove :instance slots :key #'%slot-definition-allocation))
	   (foreign-slots (remove-if-not #'foreign-slot-p slots)))
      (setq instance-slots
	    (sort-effective-instance-slotds instance-slots class cpl))
      (format t "Instance slots: ~S~%Class Slots: ~S~%Foreign Slots: ~S~%"
	      instance-slots class-slots foreign-slots)
      (loop for islot in instance-slots
	    for loc = 1 then (1+ loc)
	    do (setf (%slot-definition-location islot) loc))
      (dolist (cslot class-slots)
	(setf (%slot-definition-location cslot)
	      (assoc (%slot-definition-name cslot)
		     (%class-get (%slot-definition-class cslot) :class-slots)
		     :test #'eq)))
      (dolist (fslot foreign-slots)
	(setf (%slot-definition-location fslot)
	      (%objc-ivar-offset 
	       (compute-objc-variable-name (%slot-definition-name fslot)) class)))
      (append instance-slots class-slots foreign-slots))))


;;; Accessing foreign slots

(defmethod slot-boundp-using-class ((class objc:objc-class-object)
				    instance
				    (slotd foreign-effective-slot-definition))
  (declare (ignore class instance slotd))
  ;; foreign slots are always bound
  t)

(defmethod slot-makunbound-using-class ((class objc:objc-class-object)
					instance
					(slotd foreign-effective-slot-definition))
  (declare (ignore instance))
  (error "Foreign slots cannot be unbound: ~S" (slot-definition-name slotd)))

(defmethod slot-value-using-class ((class objc:objc-class-object)
				   instance
				   (slotd foreign-effective-slot-definition))
  (funcall (foreign-slot-definition-getter slotd)
	   instance
	   (slot-definition-location slotd)))

(defmethod (setf slot-value-using-class) (value
					  (class objc:objc-class-object)
					  instance
					  (slotd foreign-effective-slot-definition))
  (funcall (foreign-slot-definition-setter slotd)
	   instance
	   (slot-definition-location slotd)
	   value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            Instance Allocation and Initialization Protocols            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-instance ((class objc:objc-class-object) &rest initargs)
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod allocate-instance ((class objc:objc-class) &key &allow-other-keys)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((instance (%send class 'alloc)) ; For now; Use SEND macro eventually
	 (len (length (%wrapper-instance-slots (class-own-wrapper class))))
	 (slot-vector 
	  (allocate-typed-vector :slot-vector (1+ len) (%slot-unbound-marker))))
    (setf (slot-vector.instance slot-vector) instance)
    (setf (gethash instance *objc-object-slot-vectors*) slot-vector) 
    instance))

(defmethod initialize-instance ((instance objc:objc-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance objc:objc-object) &rest initargs)
  (apply #'shared-initialize instance nil initargs))

(defmethod shared-initialize ((instance objc:objc-object) slot-names 
			      &rest initargs)
  (let ((class (class-of instance)))
    ;; Call appropriate ObjC init method
    (multiple-value-bind (ks vs) (keys-and-vals initargs)
      (apply #'%send instance (lisp-to-objc-init ks) vs))
    ;; Initialize CLOS slots
    (dolist (slotd (class-slots class))
      (when (not (typep slotd 'foreign-direct-slot-definition)) ; For now
	(let ((sname (slot-definition-name slotd))
	      (slot-type (slot-definition-type slotd))
	      (typepred (slot-value slotd 'type-predicate))
	      (initfunction (slot-definition-initfunction slotd)))
	  (multiple-value-bind (ignore newval foundp)
			       (get-properties initargs
					       (slot-definition-initargs slotd))
	    (declare (ignore ignore))
	    (if foundp
		(if (funcall typepred newval)
		    (setf (slot-value instance sname) newval)
		  (report-bad-arg newval slot-type))
	      (let ((curval (slot-value instance sname)))
		(when (and (or (eq slot-names t) 
			       (member sname slot-names :test #'eq))
			   (eq curval (%slot-unbound-marker))
			   initfunction)
		  (let ((newval (funcall initfunction)))
		    (unless (funcall typepred newval)
		      (report-bad-arg newval slot-type))))))))))
    instance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Class Definition and Finalization Protocols               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defclass demo-view (ns:ns-view) 
  ((x :foreign t)
   y
   (r :foreign t :type :<NSR>ect))
  (:metaclass ns:+ns-object))
|#

;;; Create the ObjC class/metaclass pair and dress it up in its minimal CLOS garb
;;; This currently requires that exactly one of DIRECT-SUPERCLASSES be a
;;; already existing subclass of OBJC:OBJC-CLASS

(defun compute-objc-variable-name (sym)
  (let* ((pname (string sym))
	 (first-alpha (position-if #'alpha-char-p pname)))
    (string-downcase
     (apply #'string-cat 
	    (mapcar #'string-capitalize (split-if-char #\- pname :elide)))
     :end (if first-alpha (1+ first-alpha) 1))))

(defmethod allocate-instance ((metaclass objc:objc-metaclass) 
			      &key name direct-superclasses direct-slots
			      &allow-other-keys)
  (let ((class-name (compute-objc-classname name))
	(superclass-name 
	 (loop for s in direct-superclasses
	       when (typep s 'objc:objc-class)
	         collect s into objc-supers
	       finally 
	       (if (= (length objc-supers) 1)
		   (return (compute-objc-classname (class-name (first objc-supers))))
		 (error "Exactly one OBJC:OBJC-CLASS must appear in ~S, found ~S" 
			direct-superclasses
			(length objc-supers)))))
	(ivars 
	 (loop for splist in direct-slots
	       when (getf splist :foreign)
	         collect (list (compute-objc-variable-name (getf splist :name)) 
			       (or (getf splist :type) :id)))))
    (%define-objc-class (note-objc-class class-name superclass-name ivars))))

(defmethod shared-initialize ((class objc:objc-class) slot-names &rest initargs)
  ;; *** validate superclasses
  ;; *** create direct slot definition objects
  ;; *** dependency maintenance
  ;; *** maybe finalize inheritance
  class)

(defmethod validate-superclass ((c1 objc:objc-class) (c2 objc:objc-class))
  t)

(defmethod finalize-inheritance ((class objc:objc-class))
  ;; *** compute class precedence list
  ;; *** create effective slot definition objects
  )

(defmethod make-instances-obsolete ((class objc:objc-class))
  ;; What should we do here?
  class)

