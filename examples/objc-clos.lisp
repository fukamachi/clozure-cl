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
;;;  - Need to fully handle init keywords and ObjC init messages
;;;  - Need to add getter and setter functions for more foreign slot types
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

(defparameter *objc-import-private-ivars* t "When true, the CLASS-DIRECT-SLOTS of imported ObjC classes will contain slot definitions for instance variables whose name starts with an underscore.  Note that this may exacerbate compatibility problems.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                 Testing                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Enable some debugging output.
(defparameter *objc-clos-debug* nil)

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
(defvar *objc-canonical-instances* (make-hash-table :test #'eql :weak :value))

(defun raw-macptr-for-instance (instance)
  (let* ((p (%null-ptr)))
    (%set-macptr-domain p 1)		; not an ObjC object, but EQL to one
    (%setf-macptr p instance)
    p))

(defun register-canonical-objc-instance (instance raw-ptr)
  ;(terminate-when-unreachable instance)
  ;(retain-objc-instance instance)
  (setf (gethash raw-ptr *objc-canonical-instances*) instance))

(defun canonicalize-objc-instance (instance)
  (or (gethash instance *objc-canonical-instances*)
      (register-canonical-objc-instance
       (setq instance (%inc-ptr instance 0))
       (raw-macptr-for-instance instance))))

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
      (#.objc-flag-class (objc-class-id->objc-metaclass index))
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
      (#.objc-flag-class (id->objc-metaclass-wrapper (objc-class-id->objc-metaclass-id index)))
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
    ((foreign :initform nil :initarg :foreign)
     (peer :initform nil :initarg :peer)))

(defclass objc:objc-metaclass (objc:objc-class-object)
    ())

(setq *objc-metaclass-class* (find-class 'objc:objc-metaclass))

(defclass objc:objc-class (objc:objc-class-object)
    ())

(setq *objc-class-class* (find-class 'objc:objc-class))

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
    (format stream
	    (if (typep o 'ns::ns-string)
	      "~s (#x~x)"
	      "~a (#x~x)")
	    (nsobject-description o) (%ptr-to-int o))))


(defun make-objc-class-object-slots-vector (class meta)
  (let* ((n (1+ (length (extract-instance-effective-slotds meta))))
	 (slots (allocate-typed-vector :slot-vector n (%slot-unbound-marker))))
    (setf (slot-vector.instance slots) class)
    slots))

(defun make-objc-metaclass-slots-vector (metaclass)
  (make-objc-class-object-slots-vector metaclass *objc-metaclass-class*))

(defun make-objc-class-slots-vector (class)
  (make-objc-class-object-slots-vector class *objc-class-class*))



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

(defconstant objc-bitfield-offset-mask (ash 1 28) "When set in a foreign
DIRECT slot definition's offset, indicates that the low 27 bits are a bit
offset into the slots of the relevant class.")

(defconstant objc-bitfield-offset-bit-offset (byte 22 6) "bit offset of
most significant bitfield bit in word; corresponding byte offset will
be word-aligned")

(defconstant objc-bitfield-offset-byte-offset (byte 22 0) "byte offset
of field, relative to start of class's own slots")

(defclass foreign-direct-slot-definition (direct-slot-definition)
  ((foreign-type :initarg :foreign-type :initform :id :accessor foreign-slot-definition-foreign-type)
   (offset :initarg :offset
	   :initform nil
	   :accessor foreign-direct-slot-definition-offset
	   :documentation "A byte- (or, if certain high bits are set, bit-)
offset, relative to the start of the instance's slots.  The corresponding
effective slot definition's offset is a product of this value and the
instance_size of its ObjC superclass."))
  (:default-initargs :foreign-type :id))

(defclass foreign-effective-slot-definition (effective-slot-definition)
  ((foreign-type :initarg :foreign-type :initform :id :accessor foreign-slot-definition-foreign-type)
   (getter :type function :accessor foreign-slot-definition-getter)
   (setter :type function :accessor foreign-slot-definition-setter)))


;;; Use the foreign slot metaclasses if the slot has a :FOREIGN-TYPE attribute
;;  

(defmethod direct-slot-definition-class ((class objc:objc-class-object)
					 &rest initargs)
  (if (getf initargs :foreign-type)
    (find-class 'foreign-direct-slot-definition)
    (find-class 'standard-direct-slot-definition)))

(defmethod effective-slot-definition-class ((class objc:objc-class-object)
					    &rest initargs)
  (if (getf initargs :foreign-type)
    (find-class 'foreign-effective-slot-definition)
    (find-class 'standard-effective-slot-definition)))

;;; A little structure used to drive the state-driven ivar-parsing mechanism.
(defstruct
    (ivar-parse-state (:constructor %make-ivar-parse-state (class-origin)))
  (class-origin 0 :type fixnum)
  (last-byte-offset-seen nil :type (or null fixnum))
  (bitfield-offset nil :type (or null fixnum)))

(defun make-ivar-parse-state (class)
  (%make-ivar-parse-state (superclass-instance-size class)))


;;; Create FOREIGN-DIRECT-SLOT-DEFINITIONs for each foreign instance variable
;;; in the OBJC-CLASS C

(defun %compute-foreign-direct-slots (c)
  (when (objc-object-p c)
    (with-macptrs ((ivars (pref c :objc_class.ivars)))
      (unless (%null-ptr-p ivars)
	(let* ((ns-package (find-package "NS"))
	       (n (pref ivars :objc_ivar_list.ivar_count))
	       (state (make-ivar-parse-state c)))
	  (collect ((dslotds))
	    (do* ((i 0 (1+ i))
		  (ivar (pref ivars :objc_ivar_list.ivar_list)
			(%inc-ptr ivar (record-length :objc_ivar))))
		 ((= i n) (dslotds))
	      (declare (fixnum i))
	      (with-macptrs ((nameptr (pref ivar :objc_ivar.ivar_name)))
		(let* ((is-private (eql (%get-unsigned-byte nameptr 0)
				    (char-code #\_))))
		  (when (or (not is-private)
			    *objc-import-private-ivars*)
		    (let* ((name (%get-cstring nameptr))
			   (sym (compute-lisp-name name ns-package)))
		      (when is-private
			(unexport sym ns-package))
		      (dslotds
		       (make-direct-slot-definition-from-ivar
			state
			(pref ivar :objc_ivar.ivar_offset)
			(with-string-from-cstring
			    (s (pref ivar :objc_ivar.ivar_type))
			  (objc-foreign-type-for-ivar s))
			c
			(list
			 :name sym
			 :allocation :instance
			 :class c ))))))))))))))

(defun make-direct-slot-definition-from-ivar (state
					      ivar-offset
					      slot-type
					      class
					      initargs)
  (let* ((byte-offset (- ivar-offset (ivar-parse-state-class-origin state)))
	 (offset byte-offset))
    (if (or (eq slot-type 'bit)
	    (and (consp slot-type) (eq (car slot-type) 'bitfield)))
      (let* ((width (if (eq slot-type 'bit) 1 (cadr slot-type)))
	     (bit-offset
	      (if (eql offset (ivar-parse-state-last-byte-offset-seen state))
		(ivar-parse-state-bitfield-offset state)
		(or (ivar-parse-state-bitfield-offset state) 0))))
	(setf (ivar-parse-state-last-byte-offset-seen state) offset
	      (ivar-parse-state-bitfield-offset state) (+ bit-offset width))
	(setq offset (logior objc-bitfield-offset-mask
			     (dpb bit-offset
				  objc-bitfield-offset-bit-offset
				  offset)))))
    (let* ((slot 
	    (make-direct-slot-definition
	     class
	     `(:foreign-type ,slot-type :offset ,offset ,@initargs))))
      slot)))
	   

(defun set-objc-foreign-direct-slot-offsets (dslotds)
  (let* ((byte-offset 0))
    (dolist (d dslotds)
      (let* ((type (foreign-slot-definition-foreign-type d))
	     (ftype (parse-foreign-type type))
	     (type-alignment (progn (ensure-foreign-type-bits ftype)
				    (foreign-type-alignment ftype))))
	(if (= type-alignment 1)
	  (break "Bitfields not handled yet: ~s" type))
	(setq byte-offset
	      (align-offset byte-offset (ceiling type-alignment 8)))
	(setf (foreign-direct-slot-definition-offset d) byte-offset)
	
	(setq byte-offset
	      (+ byte-offset
		 (ceiling (foreign-type-bits ftype) 8)))))))
	

;;; When an ObjC class is created by the user, the OFFSET fields in
;;; its foreign direct slot definitions are generally not set.  We
;;; can compute them fairly easily, but this is stateful (a slot's
;;; offset may depend on its predecessor's offset.)  Intercept the
;;; attempt to set the classes direct slots and ensure that all
;;; of those slots have proper offsets.
;;; (In any case that I can think of, we should either find that
;;; all foreign direct slots have non-null offsets or that none
;;; do.  If any don't, recompute all of them.
(defmethod (setf class-direct-slots) :before (dslotds (class objc::objc-class))
  (let* ((foreign-dslotds
	  (loop for d in dslotds
		when (typep d 'foreign-direct-slot-definition)
		collect d)))
    (unless
      (dolist (d foreign-dslotds t)
	(if (not (foreign-direct-slot-definition-offset d))
	  (return nil)))
      (set-objc-foreign-direct-slot-offsets foreign-dslotds))))
					       

(defun lisp-defined-slot-name-to-objc-slot-name (lisp-name)
  (lisp-to-objc-message (list lisp-name)))

;;; This is only going to be called on a class created by the user;
;;; the byte part of each foreign direct slotd's offset field should
;;; already have been set.
(defun %make-objc-ivars (class)
  (let* ((start-offset (superclass-instance-size class))
	 (foreign-dslotds (loop for s in (class-direct-slots class)
				when (typep s 'foreign-direct-slot-definition)
				collect s)))
    (if (null foreign-dslotds)
      (values (%null-ptr) start-offset)
      (let* ((n (length foreign-dslotds))
	     (offset start-offset)
	     (ivars (malloc (+ 4 (* n (%foreign-type-or-record-size
				       :objc_ivar :bytes))))))
      (setf (pref ivars :objc_ivar_list.ivar_count) n)
      (do* ((l foreign-dslotds (cdr l))
	    (dslotd (car l) (car l))
	    (ivar (pref ivars :objc_ivar_list.ivar_list)
		  (%inc-ptr ivar (%foreign-type-or-record-size
				 :objc_ivar :bytes))))
	   ((null l) (values ivars (align-offset offset 4)))
	(let* ((string (lisp-defined-slot-name-to-objc-slot-name (slot-definition-name dslotd)))
	       (type (parse-foreign-type (foreign-slot-definition-foreign-type dslotd)))
	       (encoding (encode-objc-type type)))
	  (setq offset
	    (+ start-offset
		   (ldb objc-bitfield-offset-byte-offset
			(foreign-direct-slot-definition-offset dslotd))))
	  (setf (pref ivar :objc_ivar.ivar_name) (make-cstring string)
		(pref ivar :objc_ivar.ivar_type) (make-cstring encoding)
		(pref ivar :objc_ivar.ivar_offset) offset)
	  (setq offset (+ offset (ceiling (foreign-type-bits type) 8)))))))))

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

;;; Maintain the class wrapper of an ObjC class or metaclass.

(defmethod (setf class-own-wrapper) :after (wrapper (class objc::objc-metaclass))
  (setf (id->objc-metaclass-wrapper (objc-metaclass-id class)) wrapper))

(defmethod (setf class-own-wrapper) :after (wrapper (class objc::objc-class))
  (setf (id->objc-class-wrapper (objc-class-id class)) wrapper))

;;; Return the getter and setter functions for a foreign slot
;;; NOTE: Should be changed to use FOREIGN-TYPE-TO-REPRESENTATION-TYPE


(defun compute-foreign-slot-accessors (eslotd)
  (let* ((ftypespec (foreign-slot-definition-foreign-type eslotd))
	 (ftype (parse-foreign-type ftypespec)))
    (etypecase ftype
      (foreign-integer-type
       (let* ((bits (foreign-integer-type-bits ftype))
	      (align (foreign-integer-type-alignment ftype))
	      (signed (foreign-integer-type-signed ftype)))
	 (if (= bits align)
	   (ecase bits
	     (1 (values #'%get-bit #'%set-bit))
	     (8 (values (if signed #'%get-signed-byte #'%get-unsigned-byte)
			#'%set-byte))
	     (16 (values (if signed #'%get-signed-word #'%get-unsigned-word)
			 #'%set-word))
	     (32 (values (if signed #'%get-signed-long #'%get-unsigned-long)
			 #'%set-long))
	     (64 (if signed
		   (values #'%%get-signed-longlong #'%%set-signed-longlong)
		   (values #'%%get-unsigned-longlong #'%%set-unsigned-longlong))))
	     (values #'(lambda (ptr offset)
			  (%get-bitfield ptr offset bits))
		      #'(lambda (ptr offset new)
			  (setf (%get-bitfield ptr offset bits) new))))))
      (foreign-double-float-type
       (values #'%get-double-float #'%set-double-float))
      (foreign-single-float-type
       (values #'%get-single-float #'%set-single-float))
      (foreign-pointer-type
       ;; If we're pointing to a structure whose first field is
       ;; a pointer to a structure named :OBJC_CLASS, we're of
       ;; type :ID and can (fairly) safely use %GET-PTR.
       ;; Otherwise, reference the field as a raw  macptr.
       (let* ((to (foreign-pointer-type-to ftype)))
	 (if
	   (and (typep to 'foreign-record-type)
		(eq :struct (foreign-record-type-kind to))
		(progn
		  (ensure-foreign-type-bits to)
		  (let* ((first-field (car (foreign-record-type-fields to)))
			 (first-field-type
			  (if first-field
			    (foreign-record-field-type first-field))))
		    (and (typep first-field-type 'foreign-pointer-type)
			 (let* ((first-to (foreign-pointer-type-to
					   first-field-type)))
			   (and (typep first-to 'foreign-record-type)
				(eq :struct
				    (foreign-record-type-kind first-to))
				(eq :objc_class
				    (foreign-record-type-name first-to))))))))
	   (values #'%get-ptr #'%set-ptr)
	   (values #'(lambda (ptr offset)
		       (let* ((p (%null-ptr)))
			 (%set-macptr-domain p 1)
			 (%setf-macptr p (%get-ptr ptr offset))))
		   #'%set-ptr))))
      (foreign-mem-block-type
       (let* ((nbytes (%foreign-type-or-record-size ftype :bytes)))
	 (values #'%inc-ptr #'(lambda (pointer offset new)
				(setf (%composite-pointer-ref
				       nbytes
				       pointer
				       offset)
				      new))))))))
    


;;; Shadow SLOT-CLASS's COMPUTE-EFFECTIVE-SLOT-DEFINITION with a
;;; method for OBJC-CLASSes that sets up foreign slot info.

(defmethod compute-effective-slot-definition :around ((class objc:objc-class-object)
						      name
						      direct-slots)
  (let* ((first (first direct-slots)))
    (if (not (typep first 'foreign-direct-slot-definition))
      (call-next-method)
      (let* ((initer (dolist (s direct-slots)
		       (when (%slot-definition-initfunction s)
			 (return s))))
	     (documentor (dolist (s direct-slots)
			   (when (%slot-definition-documentation s)
			     (return s))))
	     (initargs (let* ((initargs nil))
			 (dolist (dslot direct-slots initargs)
			   (dolist (dslot-arg (%slot-definition-initargs  dslot))
			     (pushnew dslot-arg initargs :test #'eq)))))
	     (eslotd
	       (make-effective-slot-definition
		class
		:name name
		:allocation :instance
		:type (or (%slot-definition-type first) t)
		:documentation (when documentor (nth-value
				      1
				      (%slot-definition-documentation
				       documentor)))
		:class (%slot-definition-class first)
		:initargs initargs
		:initfunction (if initer
				(%slot-definition-initfunction initer))
		:initform (if initer (%slot-definition-initform initer))
		:foreign-type (foreign-slot-definition-foreign-type first))))
      (multiple-value-bind (getter setter) (compute-foreign-slot-accessors eslotd)
	(setf (foreign-slot-definition-getter eslotd) getter)
	(setf (foreign-slot-definition-setter eslotd) setter))
      eslotd))))


;;; Determine the location of each slot
;;; An effective slot's location is
;;; a) a function of the class's origin (superclass-instance-size)
;;;    and the corresponding direct class's offset, if it's defined in the
;;;    class (has a corresponding direct-slot-definition in the class)
;;; b) Exactly the same as the superclass's version's location, because
;;;    of single inheritance.

(defun determine-foreign-slot-location (class slot-name)
  (or
   (let* ((origin (superclass-instance-size class)))
     (dolist (d (class-direct-slots class))
       (when (and (eq slot-name (slot-definition-name d))
		   (typep d 'foreign-direct-slot-definition))
	  (return (+ origin
		     (ldb objc-bitfield-offset-byte-offset
			  (foreign-direct-slot-definition-offset d)))))))
   (dolist (super (class-direct-superclasses class))
     (when (typep super 'objc:objc-class) ; can be at most 1
       (let* ((e (find slot-name (class-slots super) :key #'slot-definition-name)))
	 (when e (return (slot-definition-location e))))))
   (error "Can't find slot definition for ~s in ~s" slot-name class)))
	  

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
      (when *objc-clos-debug*
	(format t "Instance slots: ~S~%Class Slots: ~S~%Foreign Slots: ~S~%"
		instance-slots class-slots foreign-slots))
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
	      (determine-foreign-slot-location
	       class
	       (%slot-definition-name fslot))))
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
    (apply #'initialize-instance instance initargs)))

(defun remove-slot-initargs (class initargs)
  (let* ((slot-initargs (class-slot-initargs class))) ; cache this, maybe
    (collect ((non-slot-initargs))
      (do* ((key (pop initargs) (pop initargs))
	    (val (pop initargs) (pop initargs)))
	   ((null initargs) (non-slot-initargs))
	(unless (member key slot-initargs :test #'eq)
	  (non-slot-initargs key)
	  (non-slot-initargs val))))))

(defmethod allocate-instance ((class objc:objc-class) &rest initargs &key &allow-other-keys)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((instance
	  (multiple-value-bind (ks vs) (keys-and-vals (remove-slot-initargs
						       class
						       initargs))
	    (apply #'%send ; For now; Use SEND macro eventually
		   (%send class 'alloc) (lisp-to-objc-init ks) vs))))
    (unless (%null-ptr-p instance)
      (let* ((len (length (%wrapper-instance-slots (class-own-wrapper class))))
	     (raw-ptr (raw-macptr-for-instance instance)) 
	     (slot-vector
	      (unless (zerop len)
		(allocate-typed-vector :slot-vector (1+ len) (%slot-unbound-marker)))))
	(setf (slot-vector.instance slot-vector) raw-ptr)
	(register-canonical-objc-instance instance raw-ptr)))))

(defmethod terminate ((instance objc:objc-object))
  (objc-message-send instance "release"))



(defmethod initialize-instance ((instance objc:objc-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance objc:objc-object) &rest initargs)
  (apply #'shared-initialize instance nil initargs))

(defmethod initialize-instance :after ((class objc:objc-class) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-value class 'foreign)
    (multiple-value-bind (ivars instance-size)
	(%make-objc-ivars class)
      (%add-objc-class class ivars instance-size))))

(defmethod shared-initialize ((instance objc:objc-object) slot-names 
			      &rest initargs)
  (let ((class (class-of instance)))
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

(defmethod shared-initialize :after ((spec foreign-effective-slot-definition)
				     slot-names
				     &key &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value spec 'type-predicate) #'true))

;;; The CLASS-OF an existing OBJC:OBJC-CLASS is an OBJC:OBJC-METACLASS,
;;; but not necessarily the one specified as a :metaclass option to
;;; DEFCLASS or ENSURE-CLASS.  Allow an existing class to be reinitialized,
;;; as long as the specified :metaclass and the class's own class have
;;; the same metaclass and specified metaclass is a root class.

(defmethod ensure-class-using-class ((class objc:objc-class)
				     name
				     &rest keys &key)
  (multiple-value-bind (metaclass initargs)
      (ensure-class-metaclass-and-initargs class keys)
    (let* ((existing-metaclass (class-of class)))
      (if (and (eq (class-of metaclass)
		   (class-of existing-metaclass))
	       ;; A root metaclass has the corresponding class as
	       ;; its superclass, and that class has no superclass.
	       (with-macptrs ((super (pref metaclass :objc_class.super_class)))
		 (and (not (%null-ptr-p super))
		      (not (%objc-metaclass-p super))
		      (%null-ptr-p (pref super :objc_class.super_class)))))
	;; Whew! it's ok to reinitialize the class.
	(progn
	  (apply #'reinitialize-instance class initargs)
	  (setf (find-class name) class))
	(error "Can't change metaclass of ~s to ~s." class metaclass)))))

  

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
			      &key name direct-superclasses
			      &allow-other-keys)
  (let ((superclass
	 (loop for s in direct-superclasses
	       when (typep s 'objc:objc-class)
	         collect s into objc-supers
	       finally 
	       (if (= (length objc-supers) 1)
		   (return (first objc-supers))
		 (error "Exactly one OBJC:OBJC-CLASS must appear in ~S, found ~S" 
			direct-superclasses
			(length objc-supers))))))
    (%allocate-objc-class name superclass)))

(defmethod shared-initialize ((class objc:objc-class-object) slot-names &rest initargs)
  (%shared-initialize class slot-names initargs))

(defmethod validate-superclass ((c1 objc:objc-class) (c2 objc:objc-class))
  t)

(defmethod finalize-inheritance ((class objc:objc-class))
  ;; *** compute class precedence list
  ;; *** create effective slot definition objects
  )

(defmethod make-instances-obsolete ((class objc:objc-class))
  ;; What should we do here?
  class)

;;; Reader/writer methods for instances of OBJC:OBJC-CLASS
(defmethod reader-method-class ((class objc:objc-class)
				(dslotd direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))

(defmethod writer-method-class ((class objc:objc-class)
				(dslotd direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))