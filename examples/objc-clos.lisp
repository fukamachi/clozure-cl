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
;;;  - Both method creation and invocation should be faster and cons less
;;;  - Resolve messages with repeated keywords
;;;    (rename them to :range1:range2 or don't use &key in GFs and methods)
;;;  - How to integrate SEND-SUPER with CALL-NEXT-METHOD?
;;;  - Variable arity ObjC methods
;;;  - Pass-by-ref structures need to keep track of IN, OUT, IN/OUT info
;;;  - Need to canonicalize and retain every returned :ID
;;;  - Support :BEFORE, :AFTER and :AROUND for ObjC methods
;;;  - User-defined ObjC methods via DEFMETHOD (or DEFINE-OBJ-METHOD)
;;;  - Need to fully handle init keywords and ObjC init messages

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


;;; ObjC messages that cannot currently be translated into CLOS methods

(defparameter *troublesome-messages*
  '(
    ;; Multicolon messages that don't respect the name translation rules
    "performv::" "translateTo::" "indexOf:::" "scaleTo::" "forward::" 
    "exchange::"
    ;; Messages involving the nonexistent NSButtonState
    "focusRingImageForState:" "useDisabledEffectForState:"
    "isBorderedForState:" "imageForState:" "useHighlightEffectForState:"
    "isOpaqueForState:" "bezelStyleForState:"
    ;; Messages containing repeated keywords
    "orderString:range:string:range:flags:"
    "parseSuiteOfPairsKey:separator:value:separator:allowOmitLastSeparator:" 
    "perform:with:with:" 
    "perform:withObject:withObject:" 
    "performSelector:withObject:withObject:" 
    ;; Variable arity messages
    "appendFormat:" "arrayWithObjects:" "encodeValuesOfObjCTypes:"
    "decodeValuesOfObjCTypes:" "dictinaryWithObjectsAndKeys:"
    "handleFailureInFunction:object:file:lineNumber:description:"
    "handleFailureInMethod:object:file:lineNumber:description:"
    "initWithFormat:" "initWithObjects:" "initWithObjectsAndKeys:"
    "initWithFormat:locale:" "localizedStringWithFormat:" "raise:format:"
    "setWithObjects:" "stringByAppendingFormat:" "stringWithFormat:"
    ;; Seems to involve a (:STRUCT :?) argument
    "percentEscapeDecodeBuffer:range:stripWhitespace:"))

(defun troublesome-message-p (msg)
  (if (member msg *troublesome-messages* :test #'string=) t nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                 Testing                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Enable some debugging output.
(defparameter *objc-clos-debug* nil)




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

(defun release-canonical-nsobject (object)
  object)

  

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
          (#.objc-flag-instance (or (gethash p *objc-object-slot-vectors*)
                                    ; try to allocate the slot vector on demand
                                    (let* ((raw-ptr (raw-macptr-for-instance p))
                                           (slot-vector (create-foreign-instance-slot-vector (class-of 
p))))
                                      (when slot-vector
                                        (setf (slot-vector.instance slot-vector) raw-ptr)
                                        (setf (gethash raw-ptr *objc-object-slot-vectors*) slot-vector)
					(register-canonical-objc-instance p raw-ptr)
					(initialize-instance p))
                                      slot-vector)
                                    (error "~s has no slots." p)))
          (#.objc-flag-class (id->objc-class-slots-vector index))
          (#.objc-flag-metaclass (id->objc-metaclass-slots-vector index)))))
	  
(defloadvar *objc-object-domain*
    (register-foreign-object-domain :objc
				:recognize #'recognize-objc-object
				:class-of #'%objc-domain-class-of
				:classp #'%objc-domain-classp
				:instance-class-wrapper
				#'%objc-domain-instance-class-wrapper
				:class-own-wrapper
				#'%objc-domain-class-own-wrapper
				:slots-vector #'%objc-domain-slots-vector))

;;; P is known to be a (possibly null!) instance of some ObjC class.
(defun %set-objc-instance-type (p)
  (unless (%null-ptr-p p)
    (let* ((parent (pref p :objc_object.isa))
           (id (objc-class-id parent)))
      (when id
        (%set-macptr-domain p *objc-object-domain*)
        (%set-macptr-type p id)))))

;;; P is known to be of type :ID.  It may be null.
(defun %set-objc-id-type (p)
  (let* ((idx (objc-class-id p)))
    (if idx
      (progn
        (%set-macptr-domain p *objc-object-domain*)
        (%set-macptr-type p (dpb objc-flag-class objc-type-flags idx)))
      (if (setq idx (objc-metaclass-id p))
        (progn
          (%set-macptr-domain p *objc-object-domain*)  
          (%set-macptr-type p (dpb objc-flag-metaclass objc-type-flags idx)))
        (%set-objc-instance-type p)))))


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
	    (if (slot-boundp c 'name)
              (class-name c)
              "<unnamed>")
	    (%ptr-to-int c))))

(defmethod print-object ((c objc:objc-metaclass) stream)
  (print-unreadable-object (c stream)
    (format stream "~s ~s (#x~x)" 
	    'objc:objc-metaclass 
	    (if (slot-boundp c 'name)
              (class-name c)
              "<unnamed>") 
	    (%ptr-to-int c))))

(defmethod print-object ((o objc:objc-object) stream)
  (if (objc-object-p o)
    (print-unreadable-object (o stream :type t)
      (format stream
              (if (typep o 'ns::ns-string)
                "~s (#x~x)"
                "~a (#x~x)")
              (nsobject-description o) (%ptr-to-int o)))
    (format stream "#<Bogus ObjC Object #x~X>" (%ptr-to-int o))))



  


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

(defclass foreign-direct-slot-definition (direct-slot-definition)
  ((foreign-type  :initform :id :accessor foreign-slot-definition-foreign-type)
   (bit-offset :initarg :bit-offset
               :initform nil
               :accessor foreign-direct-slot-definition-bit-offset
               :documentation "A bit-offset, relative to the start of the
               instance's slots.  The corresponding effective slot definition's
                offset is strictly determined by this value")))

(defmethod shared-initialize :after ((slotd foreign-direct-slot-definition)
                                     slot-names
                                     &key (foreign-type :id))
  (declare (ignore slot-names))
  (unless (typep foreign-type 'foreign-type)
    (setq foreign-type (parse-foreign-type foreign-type)))
  (setf (foreign-slot-definition-foreign-type slotd) foreign-type))


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


(defun set-objc-foreign-direct-slot-offsets (dslotds bit-offset)
  (dolist (d dslotds)
    (let* ((ftype (foreign-slot-definition-foreign-type d))
           (type-alignment (progn (ensure-foreign-type-bits ftype)
                                  (foreign-type-alignment ftype))))
      (setf (foreign-direct-slot-definition-bit-offset d)
            (setq bit-offset
                  (align-offset bit-offset type-alignment)))
      (setq bit-offset (+ bit-offset (foreign-type-bits ftype))))))

(defmethod (setf class-direct-slots) :before (dslotds (class objc::objc-class))
  #-apple-objc-2.0
  (let* ((foreign-dslotds
	  (loop for d in dslotds
		when (typep d 'foreign-direct-slot-definition)
		collect d))
         (bit-offset (dolist (c (class-direct-superclasses class) 0)
                       (when (typep c 'objc::objc-class)
                         (return
                           (ash (%objc-class-instance-size c)
                                3))))))
    (unless
        (dolist (d foreign-dslotds t)
          (if (not (foreign-direct-slot-definition-bit-offset d))
            (return nil)))
      (set-objc-foreign-direct-slot-offsets foreign-dslotds bit-offset)))
  #+apple-objc-2.0
  ;; Add ivars for each foreign direct slot, then ask the runtime for
  ;; the ivar's byte offset.  (Note that the ObjC 2.0 ivar initialization
  ;; protocol doesn't seem to offer support for bitfield-valued ivars.)
  (dolist (dslotd dslotds)
    (when (typep dslotd 'foreign-direct-slot-definition)
      (let* ((string (lisp-defined-slot-name-to-objc-slot-name (slot-definition-name dslotd)))
             (type (foreign-slot-definition-foreign-type dslotd))
             (encoding (progn
                         (ensure-foreign-type-bits type)
                         (encode-objc-type type)))
             (size (ceiling (foreign-type-bits type) 8))
             (align (round (log (ceiling (foreign-type-alignment type) 8) 2))))
        (with-cstrs ((name string)
                     (encoding encoding))
          (unless (eql #$NO (#_class_addIvar class name size align encoding))
            (with-macptrs ((ivar (#_class_getInstanceVariable class name)))
              (unless (%null-ptr-p ivar)
                (let* ((offset (#_ivar_getOffset ivar)))
                  (setf (foreign-direct-slot-definition-bit-offset dslotd)
                        (ash offset 3)))))))))))

					       

(defun lisp-defined-slot-name-to-objc-slot-name (lisp-name)
  (lisp-to-objc-message (list lisp-name)))

;;; This is only going to be called on a class created by the user;
;;; each foreign direct slotd's offset field should already have been
;;; set to the slot's bit offset.
#-apple-objc-2.0
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
	   ((null l) (values ivars (ash (align-offset offset 32) 3)))
	(let* ((string (lisp-defined-slot-name-to-objc-slot-name (slot-definition-name dslotd)))
	       (type (foreign-slot-definition-foreign-type dslotd))
	       (encoding (progn
                           (ensure-foreign-type-bits type)
                           (encode-objc-type type))))
	  (setq offset (foreign-direct-slot-definition-bit-offset dslotd))
	  (setf (pref ivar :objc_ivar.ivar_name) (make-cstring string)
		(pref ivar :objc_ivar.ivar_type) (make-cstring encoding)
		(pref ivar :objc_ivar.ivar_offset) (ash offset -3))
          (incf offset (foreign-type-bits type))))))))
  
  

(defun %objc-ivar-offset-in-class (name c)
  ;; If C is a non-null ObjC class that contains an instance variable
  ;; named NAME, return that instance variable's offset,  else return
  ;; NIL.
  #+apple-objc-2.0
  (with-cstrs ((name name))
    (with-macptrs ((ivar (#_class_getInstanceVariable c name)))
      (unless (%null-ptr-p ivar)
        (#_ivar_getOffset ivar))))
  #-apple-objc-2.0
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
		     (with-macptrs ((super #+apple-objc-2.0
                                           (#_class_getSuperclass class)
                                           #-apple-objc-2.0
                                           (pref class :objc_class.super_class)))
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
  (let* ((ftype (foreign-slot-definition-foreign-type eslotd)))
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

(defun bit-offset-to-location (bit-offset foreign-type)
  (ensure-foreign-type-bits foreign-type)
  (let* ((bits (foreign-type-bits foreign-type)))
    (if (or (= bits 1)
            (not (= bits (foreign-type-alignment foreign-type))))
      bit-offset
      (ash bit-offset -3))))

;;; Determine the location of each slot
;;; An effective slot's location is
;;; a) a function of the class's origin (superclass-instance-size)
;;;    and the corresponding direct class's offset, if it's defined in the
;;;    class (has a corresponding direct-slot-definition in the class)
;;; b) Exactly the same as the superclass's version's location, because
;;;    of single inheritance.

(defun determine-foreign-slot-location (class slot-name)
  (or
   (dolist (d (class-direct-slots class))
     (when (and (eq slot-name (slot-definition-name d))
                (typep d 'foreign-direct-slot-definition))
       (return (bit-offset-to-location
                (foreign-direct-slot-definition-bit-offset d)
                (foreign-slot-definition-foreign-type d )))))
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
    (loop for l = initargs then (cddr l)
	  when (null l) do (return-from remove-slot-initargs new-initargs)
	  unless (member (first l)  slot-initargs :test #'eq)
	    append (list (first l) (second l))  into new-initargs)))

(defun create-foreign-instance-slot-vector (class)
  (let* ((max 0))
    (dolist (slotd (class-slots class)
	     (unless (zerop max)
	       (allocate-typed-vector :slot-vector (1+ max) (%slot-unbound-marker))))
      (when (typep slotd 'standard-effective-slot-definition)
	(let* ((loc (slot-definition-location slotd)))
	  (if (> loc max)
	    (setq max loc)))))))

	       
					 
(defmethod allocate-instance ((class objc:objc-class) &rest initargs &key &allow-other-keys)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((instance
	  (multiple-value-bind (ks vs) (keys-and-vals (remove-slot-initargs
						       class
						       initargs))
	    ; The second %SEND below should be SEND eventually
	    (apply #'%send (%send class 'alloc) (lisp-to-objc-init ks) vs))))
    (unless (%null-ptr-p instance)
      (let* ((raw-ptr (raw-macptr-for-instance instance)) 
	     (slot-vector (create-foreign-instance-slot-vector class)))
	(when slot-vector
	  (setf (slot-vector.instance slot-vector) raw-ptr)
	  (setf (gethash raw-ptr *objc-object-slot-vectors*) slot-vector))
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
    #-apple-objc-2.0
    (multiple-value-bind (ivars instance-size)
	(%make-objc-ivars class)
      (%add-objc-class class ivars instance-size))
    #+apple-objc-2.0
    (%add-objc-class class)))

(defmethod shared-initialize ((instance objc:objc-object) slot-names 
			      &rest initargs)
  (let ((class (class-of instance)))
    ;; Initialize CLOS slots
    (dolist (slotd (class-slots class))
      (when (not (typep slotd 'foreign-effective-slot-definition)) ; For now
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
	      (let* ((loc (slot-definition-location slotd))
		     (curval (%standard-instance-instance-location-access
			     instance loc)))
		(when (and (or (eq slot-names t) 
			       (member sname slot-names :test #'eq))
			   (eq curval (%slot-unbound-marker))
			   initfunction)
		  (let ((newval (funcall initfunction)))
		    (unless (funcall typepred newval)
		      (report-bad-arg newval slot-type))
		    (setf (%standard-instance-instance-location-access
			   instance loc)
			  newval)))))))))
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
	       (with-macptrs ((super #+apple-objc-2.0
                                     (#_class_getSuperclass metaclass)
                                     #-apple-objc-2.0
                                     (pref metaclass :objc_class.super_class)))
		 (and (not (%null-ptr-p super))
		      (not (%objc-metaclass-p super))
		      (%null-ptr-p
                       #+apple-objc-2.0
                       (#_class_getSuperclass super)
                       #-apple-objc-2.0
                       (pref super :objc_class.super_class)))))
	;; Whew! it's ok to reinitialize the class.
	(progn
	  (apply #'reinitialize-instance class initargs)
	  (setf (find-class name) class))
	(error "Can't change metaclass of ~s to ~s." class metaclass)))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Class Definition and Finalization Protocols               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod make-instances-obsolete ((class objc:objc-class))
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
  (find-class 'standard-writer-method))


;;; This (interesting) code has never been enabled, and is (slightly)
;;; broken by the new (lazy, declaration-based) implementation of SEND
;;; and friends.
;;; We probably want to un-break this (and figure out how to define
;;; ObjC gf's in the new world), and some of the code for compiling
;;; arbitrary message sends may be useful in other contexts.

#+objc-generic-functions
(progn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 Generic Function and Method  Protocols                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The classes of ObjC generic functions and methods

(defclass objc-generic-function (standard-generic-function) 
  ()
  (:metaclass funcallable-standard-class))

(defclass objc-method (standard-method) ())


;;; Return the generic function name, lambda list and keywords corresponding 
;;; to a given ObjC MSG

(defun gfify (msg)
  (let* ((mcomps (split-if-char #\: msg :elide))
	 (ncolons (count #\: msg))
	 (prefix (if (zerop ncolons) "@" "")))
    (values (compute-lisp-name 
	     (if (zerop ncolons)
		 (string-cat prefix (first mcomps))
	       (first mcomps))
	     (find-package "NS"))
	    (if (zerop ncolons) '(%self) '(%self %arg &key))
	    (mapcar #'compute-lisp-name (rest mcomps)))))


;;; Special dcode for ObjC generic functions
;;; Currently, the list of keywords is used as the qualifier for an ObjC method
;;; This dcode just scans the list of methods looking for one whose qualifer
;;; matches the keywords in this call

(defun %%objc-dcode (dt args)
  (flet ((invoke-method (largs)
	   (multiple-value-bind (keys vals) (keys-and-vals (cddr largs))
	     (declare (ignore vals))
	     (dolist (m (%gf-dispatch-table-methods dt))
	       (when (equal (method-qualifiers m) keys)
		 (return-from %%objc-dcode (apply (method-function m) largs))))
	     (apply #'no-applicable-method (%gf-dispatch-table-gf dt) largs))))
    ;; If only one arg is present, ARGS is apparently not encoded
    (if (numberp args)
	(with-list-from-lexpr (l args) (invoke-method l))
      (invoke-method (list args)))))


;;; Ensure that the generic function corresponding to MSG exists

(defun ensure-objc-generic-function (msg)
  (cond 
   ((null (message-descriptors msg))
    (error "Unknown ObjC message: ~S" msg))
   ((troublesome-message-p msg) nil)
   (t
    (multiple-value-bind (gf-name lambda-list) (gfify msg)	    
      (let ((gf (ensure-generic-function
		 gf-name
		 :lambda-list lambda-list
		 :generic-function-class (find-class 'objc-generic-function)
		 :method-class (find-class 'objc-method))))
	(setf (%gf-dcode gf) #'%%objc-dcode)
	gf)))))


;;; Create the method function corresponding to the given ObjC MSG

(defun make-objc-method-function (msg lambda-list keys)
  (let ((msgdescs (message-descriptors msg)))
    (compile 
     nil
     (if (= (length msgdescs) 1)
	 ;; The type signature is unique
	 `(lambda ,lambda-list
	    ,(build-message-send 
	      msg (msg-desc-type-signature (first msgdescs)) keys))
       ;; The type signature is ambiguous
       `(lambda ,lambda-list
	  (cond
	   ,@(loop for md in msgdescs
		  collect
		  `((or 
		     ,@(loop for c in (msg-desc-classes md)
			     collect
			     `(typep %self ',(class-name c))))
		    (locally
		      (declare (,(class-name (first (msg-desc-classes md)))
				%self))
		      ,(build-message-send 
			msg (msg-desc-type-signature md) keys))))))))))


;;; Build the message-sending code for the given message with the given
;;; type signature and keys

(defun build-message-send (msg tsig keys)
  (let* ((rvars nil)
	 (args (if (zerop (count #\: msg))
		   nil
		 (loop 
		  for a in (cons '%arg keys)
		  for ftype in (rest tsig)
		  for r/s-assoc = (coerceable-foreign-record-p ftype)
		  for sname = (gensym)
		  if r/s-assoc
		    do (push (list sname (fudge-objc-type ftype)) rvars)
		    and collect
		    (generate-structure-to-foreign-record-copier-form 
		     (record-structure-association-structure-name r/s-assoc)
		     (record-structure-association-record-name r/s-assoc)
		     :struct-name a :record-name sname)
		  else collect a))))
       (if (requires-stret-p (first tsig))
	   ;; STRET message send
	   (let ((r (gensym)))
	     `(rlet ((,r ,(fudge-objc-type (first tsig))) ,@rvars)
	        (send/stret ,r %self ,msg ,@args)
		,(create-structure-from-record-form r (cadar tsig))))
	 ;; Normal message send
	 `(rlet ,rvars
	    (send %self ,msg ,@args)))))


;;; Ensure that the method corresponding to CLASS's method for MSG exists

(defun ensure-objc-method (msg)
  (cond 
   ((null (message-descriptors msg))
    (error "Unknown ObjC message: ~S" msg))
   ((troublesome-message-p msg) nil)
   (t
    (flet ((keywordify (sym)
	     (intern (string sym) (find-package 'keyword))))
      (multiple-value-bind (gf-name lambda-list keys) (gfify msg)
	(let* ((gf (ensure-objc-generic-function msg))
	       (lambda-list (append lambda-list keys))
	       (m
		(ensure-method
		 gf-name
		 nil
		 :function (make-objc-method-function msg lambda-list keys)
		 :qualifiers (mapcar #'keywordify keys)
		 :lambda-list lambda-list)))
	  (setf (%gf-dcode gf) #'%%objc-dcode)
	  m))))))


;;; Generate ObjC methods for all messages in *TYPE-SIGNATURE-TABLE*

(defun define-all-objc-methods ()
  (declare (special *type-signature-table*))
  (maphash #'(lambda (msg ignore) 
	       (declare (ignore ignore))
	       (ensure-objc-method msg))
	   *type-signature-table*))


;;; Lisp structures analogous to common Cocoa records

(defstruct (ns-range (:constructor make-ns-range (location length)))
  location
  length)

(defun ns-make-range (loc len)
  (make-ns-range loc len))

(defstruct (ns-point (:constructor make-ns-point (x y)))
  x
  y)

(defun ns-make-point (x y)
  (make-ns-point (coerce x 'single-float) (coerce y 'single-float)))

(defstruct (ns-size (:constructor make-ns-size (width height)))
  width
  height)

(defun ns-make-size (w h)
  (make-ns-size 
   (coerce w 'single-float) 
   (coerce h 'single-float)))

;;; Note that this is linear: four fields, rather than an ns-point
;;; and an ns-size.
(defstruct (ns-rect
	     (:constructor make-ns-rect
			   (origin.x origin.y size.width size.height)))
  origin.x
  origin.y
  size.width
  size.height)

(defun ns-make-rect (ox oy sw sh)
  (make-ns-rect
   (coerce ox 'single-float)
   (coerce oy 'single-float)
   (coerce sw 'single-float)
   (coerce sh 'single-float)))

(defstruct (ns-decimal
	    (:constructor make-ns-decimal
			  (_exponent _length _is-negative _is-compact _reserved _mantissa)))
  _exponent
  _length
  _is-negative
  _is-compact
  _reserved
  _mantissa)

;;; Also linear
(defstruct (cg-rect
	    (:constructor make-cg-rect
			  (origin.x origin.y size.width size.height)))
  origin.x
  origin.y
  size.width
  size.height)

(defstruct (ns-affine-transform-struct
	    (:constructor make-ns-affine-transform-struct
			  (m11 m12 m21 m22 tx ty)))
  m11 m12 m21 m22 tx ty)


(defun generate-foreign-record-to-structure-copier-form (record-type-name structure-class-name &key (struct-name (gensym)) (record-name (gensym)))
  (let* ((slot-names (mapcar #'slot-definition-name (class-slots (find-class structure-class-name))))
	 (record-type (%foreign-type-or-record record-type-name))
	 (accessor-names (foreign-record-accessor-names record-type)))
    (unless (eq (length slot-names) (length accessor-names))
      (error "Slot names ~s don't match record accessors ~s"
	     slot-names accessor-names))
    (let* ((body (mapcar #'(lambda (slot-name accessor)
			     `(setf (slot-value ,struct-name ',slot-name)
			       ,(%foreign-access-form record-name
						      record-type
						      0
						      accessor)))
			 slot-names accessor-names)))
      `(progn ,@body ,struct-name))))

(defun generate-structure-to-foreign-record-copier-form
    (structure-class-name record-type-name
			  &key
			  (struct-name (gensym))
			  (record-name (gensym)))
  (let* ((slot-names (mapcar #'slot-definition-name (class-slots (find-class structure-class-name))))
	 (record-type (%foreign-type-or-record record-type-name))
	 (accessor-names (foreign-record-accessor-names record-type)))
    (unless (eq (length slot-names) (length accessor-names))
      (error "Slot names ~s don't match record accessors ~s"
	     slot-names accessor-names))
    (let* ((body (mapcar #'(lambda (slot-name accessor)
			     `(setf ,(%foreign-access-form record-name
							   record-type
							   0
							   accessor)
			       (slot-value ,struct-name ',slot-name)))
			 slot-names accessor-names)))
      `(progn ,@body ,record-name))))

(defun generate-foreign-record-to-structure-creator-form
    (record-type-name constructor-name &key (record-name (gensym)))
  (let* ((record-type (%foreign-type-or-record record-type-name))
	 (accessor-names (foreign-record-accessor-names record-type))
	 (args (mapcar #'(lambda (accessor)
			   (%foreign-access-form record-name
						 record-type
						 0
						 accessor))
		       accessor-names)))
    `(,constructor-name ,@args)))

	   
(defstruct record-structure-association
  record-name
  structure-name
  structure-constructor-name)

(defparameter *record-structure-associations* ())

(defun record-structure-association-from-record-name (r)
  (find r *record-structure-associations* :key #'record-structure-association-record-name))

(defun need-record-structure-association-from-record-name (r)
  (or (record-structure-association-from-record-name r)
      (error "No lisp structure associated with foreign record named ~s" r)))
  
(defun record-structure-association-from-structure-name (r)
  (find r *record-structure-associations* :key #'record-structure-association-structure-name))

(defun associate-record-with-structure (record-name structure-name constructor-name)
  (let* ((already-r (record-structure-association-from-record-name record-name))
	 (already-s (record-structure-association-from-structure-name structure-name))
	 (already (or already-r already-s))
	 (different (not (eq already-r already-s))))
    (if already
      (if different
	(if already-r
	  (error "~&Record named ~s is already associated with structure named ~s"
		 (record-structure-association-record-name already-r)
		 (record-structure-association-structure-name already-r))
	  (if already-s
	    (error "~&Structure named ~s is already associated with record named ~s"
		   (record-structure-association-structure-name already-s)
		   (record-structure-association-record-name already-s))))
	(setf (record-structure-association-structure-constructor-name already)
	      constructor-name))
      (push (make-record-structure-association
	     :record-name record-name
	     :structure-name structure-name
	     :structure-constructor-name constructor-name)
	    *record-structure-associations*))
    t))

(defun create-structure-from-record-form (var record-type)
  (let* ((a (need-record-structure-association-from-record-name
	     record-type))
	 (constructor
	  (record-structure-association-structure-constructor-name a)))
    (generate-foreign-record-to-structure-creator-form
     record-type constructor :record-name var)))

(defun coerceable-foreign-record-p (ftype)
  (and (consp ftype) 
       (eq (first ftype) :struct) 
       (find (second ftype) *record-structure-associations*
	     :key #'record-structure-association-record-name)))
    
(associate-record-with-structure :_<NSR>ect 'ns-rect 'make-ns-rect)
(associate-record-with-structure :_<NSP>oint 'ns-point 'make-ns-point)
(associate-record-with-structure :_<NSS>ize 'ns-size 'make-ns-size)
(associate-record-with-structure :_<NSR>ange 'ns-range 'make-ns-range)
(associate-record-with-structure :<NSD>ecimal 'ns-decimal 'make-ns-decimal)
(associate-record-with-structure :<CGR>ect 'cg-rect 'make-cg-rect)
(associate-record-with-structure :_<NSA>ffine<T>ransform<S>truct 
				 'ns-affine-transform-struct 
				 'make-ns-affine-transform-struct)
) ; #+objc-generic-functions
