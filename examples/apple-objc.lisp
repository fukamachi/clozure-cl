;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2002-2003 Clozure Associates
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

;;; We need to be able to point the CoreFoundation and Cocoa libraries
;;; at some bundle very early in the process.  If you want to use some
;;; other bundle path, you need to do something like:
;;; ? (declaim (special *default-bundle-path*))
;;; ? (setq *default-bundle-path* "/maybe/an/absolute/bundle.app")
;;; Note that the bundle pathname is merged with "ccl:" and with
;;; "Contents/MacOS/dppccl".
;;; A bundled application should set this to NIL: there'd be no need
;;; to set the CFBundlePath environment variable in that case, since the
;;; executable path is the bundle path.
(defvar *default-bundle-path* "OpenMCL.app/")

;;; Utilities for interacting with Apple's Objective-C runtime.

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "NAME-TRANSLATION")
  (require "PROCESS-OBJC-MODULES"))

(defloadvar *NSApp* nil )

(defun fake-cfbundle-path ()
  (when *default-bundle-path*
    (let* ((fakepath
            (native-translated-namestring
             (merge-pathnames
              "Contents/MacOS/dppccl"
              (merge-pathnames *default-bundle-path* (truename "CCL:"))))))
      (setenv "CFProcessPath" fakepath))))

(defloadvar *cocoa-event-process* *initial-process*)

(defun run-in-cocoa-process-and-wait  (f)
  (let* ((process *cocoa-event-process*)
	 (success (cons nil nil))
	 (done (make-semaphore)))
    (process-interrupt process #'(lambda ()
				   (unwind-protect
					(progn
					  (setf (car success) (funcall f)))
				     (signal-semaphore done))))
    (wait-on-semaphore done)
    (car success)))



(def-ccl-pointers cocoa-framework ()
  (fake-cfbundle-path)
  (run-in-cocoa-process-and-wait
   #'(lambda ()
       ;; We need to load and "initialize" the CoreFoundation library
       ;; in the thread that's going to process events.  Looking up a
       ;; symbol in the library should cause it to be initialized
       (open-shared-library "/System/Library/Frameworks/Cocoa.framework/Cocoa")
       (let* ((current (#_CFRunLoopGetCurrent))
              (main (external-call "_CFRunLoopGetMain" :address)))
         ;; Sadly, it seems that OSX versions > 10.2 only want the
         ;; main CFRunLoop to be owned by the initial thread.  I
         ;; suppose that we could try to run the event process on that
         ;; thread, but that'd require some reorganization.
         (or
          (eql current main)
          (progn (external-call "__CFRunLoopSetCurrent"
                                :address main)
                 t))))))
         
       

(defvar *objc-readtable* (copy-readtable nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-syntax-from-char #\] #\) *objc-readtable*))

;;; We use the convention that [:super ....] denotes a send to the
;;; defining object's superclass's method, and that a return value
;;; specification of the form (:-> ... x) indicates a message send
;;; that returns a structure (by reference) via the pointer x.

(set-macro-character
 #\[
 (nfunction
  |objc-[-reader|
  (lambda (stream ignore)
    (declare (ignore ignore))
    (let* ((tail (read-delimited-list #\] stream))
	   (structptr nil))
      (let* ((return (car (last tail))))
	(when (and (consp return) (eq (car return) :->))
	  (rplaca (last tail) :void)
	  (setq structptr (car (last return)))))
      (if (eq (car tail) :super)
	(if structptr
	  `(objc-message-send-super-stret ,structptr super ,@(cdr tail))
	  `(objc-message-send-super super ,@(cdr tail)))
	(if structptr
	  `(objc-message-send-stret ,structptr ,@tail)
	  `(objc-message-send ,@tail))))))
 nil
 *objc-readtable*)

(eval-when (:compile-toplevel :execute)
  (setq *readtable* *objc-readtable*))


;;; An instance of NSConstantString (which is a subclass of NSString)
;;; consists of a pointer to the NSConstantString class (which the
;;; global "_NSConstantStringClassReference" conveniently refers to), a
;;; pointer to an array of 8-bit characters (doesn't have to be #\Nul
;;; terminated, but doesn't hurt) and the length of that string (not
;;; counting any #\Nul.)
;;; The global reference to the "NSConstantString" class allows us to
;;; make instances of NSConstantString, ala the @"foo" construct in
;;; ObjC.  Sure it's ugly, but it seems to be exactly what the ObjC
;;; compiler does.

(defloadvar *NSConstantString-class*
  (foreign-symbol-address "__NSConstantStringClassReference"))


;;; Execute the body with the variable NSSTR bound to a
;;; stack-allocated NSConstantString instance (made from
;;; *NSConstantString-class*, CSTRING and LEN).
(defmacro with-nsstr ((nsstr cstring len) &body body)
  `(rlet ((,nsstr :<NSC>onstant<S>tring
	   :isa *NSConstantString-class*
	   :bytes ,cstring
	   :num<B>ytes ,len))
      ,@body))

;;; Make a persistent (heap-allocated) NSConstantString.

(defun %make-nsstring (string)
  "Make a persistent (heap-allocated) NSConstantString from the
argument lisp string."
  (make-record :<NSC>onstant<S>tring
	       :isa *NSConstantString-Class*
	       :bytes (make-cstring string)
	       :num<B>ytes (length string)))


;;; Intern NSConstantString instances.
(defvar *objc-constant-strings* (make-hash-table :test #'equal))

(defstruct objc-constant-string
  string
  nsstringptr)

(defun ns-constant-string (string)
  (or (gethash string *objc-constant-strings*)
      (setf (gethash string *objc-constant-strings*)
	    (make-objc-constant-string :string string
				       :nsstringptr (%make-nsstring string)))))

(def-ccl-pointers objc-strings ()
  (maphash #'(lambda (string cached)
	       (setf (objc-constant-string-nsstringptr cached)
		     (%make-nsstring string)))
	   *objc-constant-strings*))

(defmethod make-load-form ((s objc-constant-string) &optional env)
  (declare (ignore env))
  `(ns-constant-string ,(objc-constant-string-string s)))

(defmacro @ (string)
  `(objc-constant-string-nsstringptr ,(ns-constant-string string)))

(set-dispatch-macro-character
 #\#
 #\@
 (nfunction
  |objc-#@-reader|
  (lambda (stream subchar numarg)
    (declare (ignore subchar numarg))
    (let* ((string (read stream)))
      (check-type string string)
      `(@ ,string))))
 *objc-readtable*)


;;; Registering named objc classes.


(defun objc-class-name-string (name)
  (etypecase name
    (symbol (lisp-to-objc-classname name))
    (string name)))

;;; We'd presumably cache this result somewhere, so we'd only do the
;;; lookup once per session (in general.)
(defun lookup-objc-class (name &optional error-p)
  (with-cstrs ((cstr (objc-class-name-string name)))
    (let* ((p (#_objc_lookUpClass cstr)))
      (if (%null-ptr-p p)
	(if error-p
	  (error "ObjC class ~a not found" name))
	p))))

(defvar *objc-class-descriptors* (make-hash-table :test #'equal))


(defstruct objc-class-descriptor
  name
  classptr)

(def-ccl-pointers invalidate-objc-class-descriptors ()
  (maphash #'(lambda (name descriptor)
	       (declare (ignore name))
	       (setf (objc-class-descriptor-classptr descriptor) nil))
	   *objc-class-descriptors*))

(defun %objc-class-classptr (class-descriptor &optional (error-p t))
  (or (objc-class-descriptor-classptr class-descriptor)
      (setf (objc-class-descriptor-classptr class-descriptor)
	    (lookup-objc-class (objc-class-descriptor-name class-descriptor)
			       error-p))))

(defun load-objc-class-descriptor (name)
  (let* ((descriptor (or (gethash name *objc-class-descriptors*)
			 (setf (gethash name *objc-class-descriptors*)
			       (make-objc-class-descriptor  :name name)))))
    (%objc-class-classptr descriptor nil)
    descriptor))

(defmacro objc-class-descriptor (name)
  `(load-objc-class-descriptor ,name))

(defmethod make-load-form ((o objc-class-descriptor) &optional env)
  (declare (ignore env))
  `(load-objc-class-descriptor ,(objc-class-descriptor-name o)))

(defmacro @class (name)
  (let* ((name (objc-class-name-string name)))
    `(the (@metaclass ,name) (%objc-class-classptr ,(objc-class-descriptor name)))))

;;; This isn't quite the inverse operation of LOOKUP-OBJC-CLASS: it
;;; returns a simple C string.  and can be applied to a class or any
;;; instance (returning the class name.)
(defun objc-class-name (object)
  (with-macptrs (p)
    (%setf-macptr p (#_object_getClassName object))
    (unless (%null-ptr-p p)
      (%get-cstring p))))


;;; Likewise, we want to cache the selectors ("SEL"s) which identify
;;; method names.  They can vary from session to session, but within
;;; a session, all methods with a given name (e.g, "init") will be
;;; represented by the same SEL.
(defun get-selector-for (method-name &optional error)
  (with-cstrs ((method-name method-name))
    (let* ((p (#_sel_getUid method-name)))
      (if (%null-ptr-p p)
	(if error
	  (error "Can't find ObjC selector for ~a" method-name))
	p))))

(defvar *objc-selectors* (make-hash-table :test #'equal))

(defstruct objc-selector
  name
  %sel)

(defun %get-SELECTOR (selector &optional (error-p t))
  (or (objc-selector-%sel selector)
      (setf (objc-selector-%sel selector)
	    (get-selector-for (objc-selector-name selector) error-p))))

(def-ccl-pointers objc-selectors ()
  (maphash #'(lambda (name sel)
	       (declare (ignore name))
	       (setf (objc-selector-%sel sel) nil))
	   *objc-selectors*))

(defun load-objc-selector (name)
  (let* ((selector (or (gethash name *objc-selectors*)
		       (setf (gethash name *objc-selectors*)
			     (make-objc-selector :name name)))))
    (%get-SELECTOR selector nil)
    selector))

(defmacro @SELECTOR (name)
  `(%get-selector ,(load-objc-selector name)))

(defmethod make-load-form ((s objc-selector) &optional env)
  (declare (ignore env))
  `(load-objc-selector ,(objc-selector-name s)))

;;; Add a faster way to get the message from a SEL by taking advantage of the
;;; fact that a selector is really just a canonicalized, interned C string
;;; containing the message.  (This is an admitted modularity violation;
;;; there's a more portable but slower way to do this if we ever need to.)

(defun lisp-string-from-sel (sel)
  (%get-cstring sel))

;;; #_objc_msgSend takes two required arguments (the receiving object
;;; and the method selector) and 0 or more additional arguments;
;;; there'd have to be some macrology to handle common cases, since we
;;; want the compiler to see all of the args in a foreign call.

(defmacro objc-message-send (receiver selector-name &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  `(external-call "_objc_msgSend"
    :id ,receiver
    :<SEL> (@selector ,selector-name)
    ,@argspecs))

;;; A method that returns a structure (whose size is > 4 bytes) does
;;; so by copying the structure into a pointer passed as its first
;;; argument; that means that we have to invoke the method via
;;; #_objc_msgSend_stret.
(defmacro objc-message-send-stret (structptr receiver selector-name &rest argspecs)
  (if (evenp (length argspecs))
    (setq argspecs (append argspecs '(:void)))
    (unless (member (car (last argspecs)) '(:void nil))
      (error "Invalid result spec for structure return: ~s"
	     (car (last argspecs)))))
  `(external-call "_objc_msgSend_stret"
    :address ,structptr
    :id ,receiver
    :<SEL> (@selector ,selector-name)
    ,@argspecs))

;;; #_objc_msgSendSuper is similar to #_objc_msgSend; its first argument
;;; is a pointer to a structure of type objc_super {self,  the defining
;;; class's superclass}.  It only makes sense to use this inside an
;;; objc method.
(defmacro objc-message-send-super (super selector-name &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  `(external-call "_objc_msgSendSuper"
    :address ,super
    :<SEL> (@selector ,selector-name)
    ,@argspecs))

;;; Send to superclass method, returning a structure.
(defmacro objc-message-send-super-stret
    (structptr super selector-name &rest argspecs)
  (if (evenp (length argspecs))
    (setq argspecs (append argspecs '(:void)))
    (unless (member (car (last argspecs)) '(:void nil))
      (error "Invalid result spec for structure return: ~s"
	     (car (last argspecs)))))
  `(external-call "_objc_msgSendSuper_stret"
    :address ,structptr
    :address ,super
    :<SEL> (@selector ,selector-name)
    ,@argspecs))



;;; The first 8 words of non-fp arguments get passed in R3-R10
(defvar *objc-gpr-offsets*
  #(4 8 12 16 20 24 28 32))

;;; The first 13 fp arguments get passed in F1-F13 (and also "consume"
;;; a GPR or two.)  It's certainly possible for an FP arg and a non-
;;; FP arg to share the same "offset", and parameter offsets aren't
;;; strictly increasing.
(defvar *objc-fpr-offsets*
  #(36 44 52 60 68 76 84 92 100 108 116 124 132))

;;; Just to make things even more confusing: once we've filled in the
;;; first 8 words of the parameter area, args that aren't passed in
;;; FP-regs get assigned offsets starting at 32.  That almost makes
;;; sense (even though it conflicts with the last offset in
;;; *objc-gpr-offsets* (assigned to R10), but we then have to add
;;; this constant to the memory offset.
(defconstant objc-forwarding-stack-offset 8)

(defvar *objc-id-type* (parse-foreign-type :id))
(defvar *objc-sel-type* (parse-foreign-type :<SEL>))
(defvar *objc-char-type* (parse-foreign-type :char))

(defun accessor-for-type-char (c)
  (case c
    ((#\@ @\: #\^ #\#) '%get-ptr)
    (#\c '%get-signed-byte)
    (#\C '%get-unsigned-byte)
    (#\s '%get-signed-word)
    (#\S '%get-unsigned-word)
    ((#\i #\l) '%get-signed-long)
    ((#\I #\L) '%get-unsigned-long)
    (#\q '%%get-signed-longlong)
    (#\Q '%%get-unsigned-longlong)
    (#\f '%get-single-float)
    (#\d '%get-double-float)
    ((#\{ #\( #\[) '%inc-ptr)))

(defun encode-objc-arg-type (type)
  (if (or (eq type *objc-id-type*)
	  (foreign-type-= type *objc-id-type*))
    "@"
    (if (or (eq type *objc-sel-type*)
	    (foreign-type-= type *objc-sel-type*))
      ":"
      (if (eq (foreign-type-class type) 'root)
	"v"
	(typecase type
	  (foreign-pointer-type
	   (let* ((target (foreign-pointer-type-to type)))
	     (if (or (eq target *objc-char-type*)
		     (foreign-type-= target *objc-char-type*))
	       "*"
	       (format nil "^~a" (encode-objc-arg-type target)))))
	  (foreign-double-float-type "d")
	  (foreign-single-float-type "f")
	  (foreign-integer-type
	   (let* ((signed (foreign-integer-type-signed type))
		  (bits (foreign-integer-type-bits type)))
	     (cond ((= bits 8)
		    (if signed "c" "C"))
		   ((= bits 16)
		    (if signed "s" "S"))
		   ((= bits 32)
		    ;; Should be some way of noting "longness".
		    (if signed "i" "I"))
		   ((= bits 64)
		    (if signed "q" "Q")))))
	  (foreign-record-type
	   (ensure-foreign-type-bits type)
	   (let* ((name (unescape-foreign-name
			 (or (foreign-record-type-name type) "?")))
		  (kind (foreign-record-type-kind type))
		  (fields (foreign-record-type-fields type)))
	     (with-output-to-string (s)
	       (format s "~c~a=" (if (eq kind :struct) #\{ #\() name)
	       (dolist (f fields (format s "~a" (if (eq kind :struct) #\} #\))))
		 (format s "~a" (encode-objc-arg-type
				 (foreign-record-field-type f)))))))
	  (foreign-array-type
	   (ensure-foreign-type-bits type)
	   (let* ((dims (foreign-array-type-dimensions type))
		  (element-type (foreign-array-type-element-type type)))
	     (if dims (format nil "[~d~a]"
			      (car dims)
			      (encode-objc-arg-type element-type))
	       (if (or (eq element-type *objc-char-type*)
		       (foreign-type-= element-type *objc-char-type*))
		 "*"
		 (format nil "^~a" (encode-objc-arg-type element-type))))))
	  (t (break "type = ~s" type)))))))
		 
(defun encode-objc-method-arglist (arglist result-spec)
  (let* ((gprs-used 0)
	 (fprs-used 0)
	 (arg-info
	  (flet ((current-memory-arg-offset ()
		   (+ 32 (* 4 (- gprs-used 8))
		      objc-forwarding-stack-offset)))
	    (flet ((current-gpr-arg-offset ()
		     (if (< gprs-used 8)
		       (svref *objc-gpr-offsets* gprs-used)
		       (current-memory-arg-offset)))
		   (current-fpr-arg-offset ()
		     (if (< fprs-used 13)
		       (svref *objc-fpr-offsets* fprs-used)
		       (current-memory-arg-offset))))
	      (let* ((result nil))
		(dolist (argspec arglist (nreverse result))
		  (let* ((arg (parse-foreign-type argspec))
			 (offset 0)
			 (size 0))
		    (typecase arg
		      (foreign-double-float-type
		       (setq size 8 offset (current-fpr-arg-offset))
		       (incf fprs-used)
		       (incf gprs-used 2))
		      (foreign-single-float-type
		       (setq size 4 offset (current-fpr-arg-offset))
		       (incf fprs-used)
		       (incf gprs-used 1))
		      (foreign-pointer-type
		       (setq size 4 offset (current-gpr-arg-offset))
		       (incf gprs-used))
		      (foreign-integer-type
		       (let* ((bits (foreign-type-bits arg)))
			 (setq size (ceiling bits 8)
			       offset (current-gpr-arg-offset))
			 (incf gprs-used (ceiling bits 32))))
		      ((or foreign-record-type foreign-array-type)
		       (let* ((bits (ensure-foreign-type-bits arg)))
			 (setq size (ceiling bits 8)
			       offset (current-gpr-arg-offset))
			 (incf gprs-used (ceiling bits 32))))
		      (t (break "argspec = ~s, arg = ~s" argspec arg)))
		    (push (list (encode-objc-arg-type arg) offset size) result))))))))
    (declare (fixnum gprs-used fprs-used))
    (let* ((max-parm-end
	    (- (apply #'max (mapcar #'(lambda (i) (+ (cadr i) (caddr i)))
				    arg-info))
	       objc-forwarding-stack-offset)))
      (format nil "~a~d~:{~a~d~}"
	      (encode-objc-arg-type
	       (parse-foreign-type result-spec))
	      max-parm-end
	      arg-info))))

(defun %make-method-vector ()
  (let* ((method-vector (malloc 16)))
    (setf (%get-signed-long method-vector 0) 0
	  (%get-signed-long method-vector 4) 0
	  (%get-signed-long method-vector 8) 0
	  (%get-signed-long method-vector 12) -1)
    method-vector))
  

;;; Make a meta-class object (with no instance variables or class
;;; methods.)
(defun %make-basic-meta-class (nameptr superptr rootptr)
  (let* ((method-vector (%make-method-vector)))
    (make-record :objc_class
		 :isa (pref rootptr :objc_class.isa)
		 :super_class (pref superptr :objc_class.isa)
		 :name nameptr
		 :version 0
		 :info #$CLS_META
		 :instance_size 0
		 :ivars (%null-ptr)
		 :method<L>ists method-vector
		 :cache (%null-ptr)
		 :protocols (%null-ptr))))

(defun %make-class-object (metaptr superptr nameptr ivars instance-size)
  (let* ((method-vector (%make-method-vector)))
    (make-record :objc_class
		 :isa metaptr
		 :super_class superptr
		 :name nameptr
		 :version 0
		 :info #$CLS_CLASS
		 :instance_size instance-size
		 :ivars ivars
		 :method<L>ists method-vector
		 :cache (%null-ptr)
		 :protocols (%null-ptr))))

(defstruct objc-class-info
  classname
  superclassname
  ivars
  objc-class)

(defvar *lisp-objc-classes* (make-hash-table :test #'equal))

(defstruct ivar-info
  classname
  name					;symbol
  string
  type-encoding
  foreign-type
  accessor
  %offset)

(defun lookup-ivar-info (ivar-name classname)
  (let* ((class-info (or (gethash classname *lisp-objc-classes*)
			 (error "Unknown objc class : ~s" classname))))
    (or (find ivar-name (objc-class-info-ivars class-info) :key #'ivar-info-name)
	(error "Unknown instance variable ~s in class ~s" ivar-name classname))))

(defun find-class-ivar-offset (classname ivar-string)
  (let* ((class (lookup-objc-class classname t)))
    (with-cstrs ((s ivar-string))
      (with-macptrs ((ivar))
	(%setf-macptr ivar (#_class_getInstanceVariable class s))
	(if (%null-ptr-p ivar)
	  (error "Unknown instance variable ~s in class ~s"
		 ivar-string classname)
	  (pref ivar :objc_ivar.ivar_offset))))))
  
(defun ivar-offset (info)
  (or (ivar-info-%offset info)
      (setf (ivar-info-%offset info)
	    (find-class-ivar-offset (ivar-info-classname info)
				    (ivar-info-string info)))))

(defmethod make-load-form ((ivar ivar-info) &optional env)
  (declare (ignore env))
  `(lookup-ivar-info ',(ivar-info-name ivar) ',(ivar-info-classname ivar)))



(defun %encode-objc-ivar-type (spec)
  (let* ((type (parse-foreign-type spec))
	 (encoding (encode-objc-arg-type type)))
    (values encoding type (accessor-for-type-char (schar encoding 0)))))


(defun spec-to-name-string-type (spec)
  (if (atom spec)
    (values spec (string-downcase spec) :id)
    (if (atom (car spec))
      (values (car spec) (string-downcase (car spec)) (or (cadr spec) :id))
      (values (caar spec) (cadar spec) (or (cadr spec) :id)))))

(defun %make-objc-ivars (info-list start-offset)
  (declare (list info-list) (fixnum start-offset))
  (if (null info-list)
    (values (%null-ptr) start-offset)
    (let* ((n (length info-list))
	   (offset start-offset)
	   (ivars (malloc (+ 4 (* n (%foreign-type-or-record-size
				     :objc_ivar :bytes))))))
      (setf (pref ivars :objc_ivar_list.ivar_count) n)
      (do* ((l info-list (cdr l))
	    (info (car l) (car l))
	    (ivar (pref ivars :objc_ivar_list.ivar_list)
		  (%inc-ptr ivar (%foreign-type-or-record-size
				 :objc_ivar :bytes))))
	   ((null l) (values ivars (align-offset offset 4)))
	(let* ((string (ivar-info-string info))
	       (type (ivar-info-foreign-type info))
	       (alignment-bits (or (progn (ensure-foreign-type-bits type)
					  (foreign-type-alignment type))
				   8))
	       (alignment-bytes (ceiling alignment-bits 8))
	       (encoding (ivar-info-type-encoding info)))
	  (setq offset (align-offset offset alignment-bytes))
	  (setf (pref ivar :objc_ivar.ivar_name) (make-cstring string)
		(pref ivar :objc_ivar.ivar_type) (make-cstring encoding)
		(pref ivar :objc_ivar.ivar_offset) offset
		offset (+ offset (ceiling (foreign-type-bits type) 8))))))))

(defun ivar-info-from-spec (classname spec)
  (multiple-value-bind (name string typespec)
      (spec-to-name-string-type spec)
    (multiple-value-bind (type-encoding ftype accessor)
	(%encode-objc-ivar-type typespec)
      (declare (ignore ignore))
      (make-ivar-info :classname classname
		      :name name
		      :string string
		      :type-encoding type-encoding
		      :accessor accessor
		      :foreign-type ftype
		      :%offset nil))))

;;; If class info exists, re-use it (and whine if it doesn't match what
;;; would be freshly generated.)  We can't really redefine objc classes
;;; at runtime.
(defun note-objc-class (classname superclassname specs)
  (let* ((ivars (mapcar #'(lambda (spec) (ivar-info-from-spec classname spec)) specs))
	 (class-info (gethash classname *lisp-objc-classes*)))
    (if (not class-info)
      (setf (gethash classname *lisp-objc-classes*)
	    (make-objc-class-info :classname classname
				  :superclassname superclassname
				  :ivars ivars
				  :objc-class (load-objc-class-descriptor classname)))
      (let* ((changed nil)
	     (existing-ivars (objc-class-info-ivars class-info)))
	(unless (equal superclassname (objc-class-info-superclassname class-info))
	  (setf (objc-class-info-superclassname class-info) superclassname
		changed t))
	(unless (do* ((ivars ivars (cdr ivars))
		      (existing existing-ivars (cdr existing))
		      (new (car ivars) (car ivars))
		      (old (car existing) (car existing)))
		     ((null ivars) (null existing))
		  (unless (and (eq (ivar-info-name old) (ivar-info-name new))
			       (equal
				(ivar-info-type-encoding old)
				(ivar-info-type-encoding new))
			       (eq (ivar-info-accessor old)
				   (ivar-info-accessor new)))
		    (setf (ivar-info-name old) (ivar-info-name new)
			  (ivar-info-type-encoding old) (ivar-info-type-encoding new)
			  (ivar-info-accessor old) (ivar-info-accessor new))
		    (return nil))))
	(when changed
	  (warn "Definition of class ~s has changed.  Recompile subclasses and~
client methods" classname))
	class-info))))
	
(defun %make-objc-class (name superclass-name instance-vars)
  (let* ((nameptr (make-cstring name))
	 (superptr (%objc-class-classptr
		    (load-objc-class-descriptor superclass-name)))
	 (metaclass (%make-basic-meta-class nameptr superptr (@class "NSObject"))))
    (multiple-value-bind (ivars instance-size)
	(%make-objc-ivars instance-vars (pref superptr :objc_class.instance_size))

      (%make-class-object metaclass superptr nameptr ivars instance-size))))

(defun %define-objc-class (info)
  (let* ((descriptor (objc-class-info-objc-class info)))
    (or (%objc-class-classptr descriptor nil)
	(let* ((class (%make-objc-class (objc-class-info-classname info)
					(objc-class-info-superclassname info)
					(objc-class-info-ivars info))))
	  (#_objc_addClass class )
	  (%objc-class-classptr descriptor)))))

(defun ensure-lisp-objc-class-defined (classname
				       &optional (info
						  (gethash classname
							   *lisp-objc-classes*)))
  (when info
    (ensure-lisp-objc-class-defined (objc-class-info-superclassname info))
    (%define-objc-class info)))

(def-ccl-pointers define-lisp-objc-classes ()
  (maphash #'(lambda (classname info)
	       (ensure-lisp-objc-class-defined classname info))
	   *lisp-objc-classes*))

  
(defmacro def-objc-class (class-name superclass-name &rest instance-vars)
  (let* ((class-name (objc-class-name-string class-name))
	 (superclass-name (objc-class-name-string superclass-name)))
    `(progn
      (eval-when (:compile-toplevel)
	(note-objc-class ,class-name ,superclass-name ',instance-vars))
      (eval-when (:load-toplevel :execute)
	(%define-objc-class (note-objc-class ,class-name ,superclass-name ',instance-vars))))))
  
;;; This is intended (mostly) for debugging (e.g., to support inspecting/
;;; describing ObjC objects.)

(defloadvar *all-objc-classes* (%null-ptr))

(defloadvar *num-objc-classes* 0)

(defun all-objc-classes ()
  (let* ((n (#_objc_getClassList (%null-ptr) 0))
	 (p *all-objc-classes*))
    (when (> n *num-objc-classes*)
      (%setf-macptr p (#_realloc p (* 4 n)))
      (#_objc_getClassList p (setq *num-objc-classes* n)))
    (values p *num-objc-classes*)))

;;; If P is an ObjC class (or metaclass), return the class & metaclass,
;;; else return (VALUES NIL NIL).
(defun objc-class-p (p)
  (if (typep p 'macptr)
    (with-macptrs (class metaclass)
      (multiple-value-bind (classlist n) (all-objc-classes)
	(declare (fixnum n))
	(do* ((i 0 (1+ i))
	      (j 0 (+ j 4)))
	     ((= i n) (values nil nil))
	  (declare (fixnum i j))
	  (%setf-macptr class (%get-ptr classlist j))
	  (%setf-macptr metaclass (pref class :objc_class.isa))
	  (if (or (eql p class)
		  (eql p metaclass))
	    ;; Return heap-consed copies of class & metaclass
	    (return (values (%setf-macptr (%null-ptr) class)
			    (%setf-macptr (%null-ptr) metaclass)))))))
    (values nil nil)))

;;; If P is an ObjC instance, return a pointer to its class.
;;; This assumes that all instances are allocated via something that's
;;; ultimately malloc-based.
(defun objc-instance-p (p)
  (and (typep p 'macptr)
       (not (%null-ptr-p (#_malloc_zone_from_ptr p)))
       ;; #_malloc_zone_from_pointer seems pretty robust.
       ;; If it returned a non-null "zone", it's probably safe
       ;; to indirect through P.
       (with-macptrs ((parent (pref p :objc_object.isa)))
	 (or (objc-class-p parent)
	     (values (objc-instance-p parent))))))

;;; If an instance, return (values :INSTANCE <class>).
;;; If a class, return (values :CLASS <metaclass>).
;;; If a metaclass, return (values :METACLASS <class>).
;;; Else return (values NIL NIL).
(defun objc-object-p (p)
  (multiple-value-bind (class metaclass) (objc-class-p p)
    (if (eql p class)
      (values :class metaclass)
      (if (eql p metaclass)
	(values :metaclas class)
	(if (setq class (objc-instance-p p))
	  (values :instance class)
	  (values nil nil))))))
       

;;; Stub until BRIDGE is loaded
(defun update-type-signatures-for-method (m) (declare (ignore m)))


;;; If the class contains an mlist that contains a method that
;;; matches (is EQL to) the selector, remove the mlist and
;;; set its IMP; return the containing mlist.
;;; If the class doesn't contain any matching mlist, create
;;; an mlist with one method slot, initialize the method, and
;;; return the new mlist.  Doing it this way ensures
;;; that the objc runtime will invalidate any cached references
;;; to the old IMP, at least as far as objc method dispatch is
;;; concerned.
(defun %mlist-containing (classptr selector typestring imp)
  (%stack-block ((iter 4))
    (setf (%get-ptr iter) (%null-ptr))
    (loop
	(let* ((mlist (#_class_nextMethodList classptr iter)))
	  (when (%null-ptr-p mlist)
	    (let* ((mlist (make-record :objc_method_list
				       :method_count 1))
		   (method (pref mlist :objc_method_list.method_list)))
	      (setf (pref method :objc_method.method_name) selector
		    (pref method :objc_method.method_types)
		    (make-cstring typestring)
		    (pref method :objc_method.method_imp) imp)
              (update-type-signatures-for-method method)
	      (return mlist)))
	  (do* ((n (pref mlist :objc_method_list.method_count))
		(i 0 (1+ i))
		(method (pref mlist :objc_method_list.method_list)
			(%incf-ptr method (%foreign-type-or-record-size
					   :objc_method :bytes))))
	       ((= i n))
	    (declare (fixnum i n))
	    (when (eql selector (pref method :objc_method.method_name))
	      (#_class_removeMethods classptr mlist)
	      (setf (pref method :objc_method.method_imp) imp)
	      (return-from %mlist-containing mlist)))))))
	      

(defun %add-objc-method (classptr selector typestring imp)
  (#_class_addMethods classptr
		      (%mlist-containing classptr selector typestring imp)))

(defvar *lisp-objc-methods* (make-hash-table :test #'eq))

(defstruct lisp-objc-method
  class-descriptor
  sel
  typestring
  class-p				;t for class methods
  imp					; callback ptr
  )

(defun %add-lisp-objc-method (m)
  (let* ((class (%objc-class-classptr (lisp-objc-method-class-descriptor m)))
	 (sel (%get-selector (lisp-objc-method-sel m)))
	 (typestring (lisp-objc-method-typestring m))
	 (imp (lisp-objc-method-imp m)))
    (%add-objc-method
     (if (lisp-objc-method-class-p m)
       (pref class :objc_class.isa)
       class)
     sel
     typestring
     imp)))

(def-ccl-pointers add-objc-methods ()
  (maphash #'(lambda (impname m)
	       (declare (ignore impname))
	       (%add-lisp-objc-method m))
	   *lisp-objc-methods*))

(defun %define-lisp-objc-method (impname classname selname typestring imp
					 &optional class-p)
  (%add-lisp-objc-method
   (setf (gethash impname *lisp-objc-methods*)
	 (make-lisp-objc-method
	  :class-descriptor (load-objc-class-descriptor classname)
	  :sel (load-objc-selector selname)
	  :typestring typestring
	  :imp imp
	  :class-p class-p)))
  impname)
    
(defmacro ivar-ref (classname instance ivar-name)
  (let* ((info (lookup-ivar-info ivar-name classname)))
    `(,(ivar-info-accessor info) ,instance (ivar-offset ,info))))

(defun objc-class-info-all-ivars (class-info)
  (append (let* ((super-info 
		  (gethash (objc-class-info-superclassname class-info)
			   *lisp-objc-classes*)))
	    (if super-info
	      (objc-class-info-all-ivars super-info)))
	  (objc-class-info-ivars class-info)))

(defmacro with-ivar-symbol-macros (classname instance &body body)
  (let* ((class-info (or (gethash classname *lisp-objc-classes*)
			 (error "Unknown objective-C class name ~s" classname)))
	 (ivars (objc-class-info-all-ivars class-info)))
    `(symbol-macrolet (,@(mapcar #'(lambda (ivar)
				     `(,(ivar-info-name ivar)
				       (,(ivar-info-accessor ivar)
					,instance
					(ivar-offset (load-time-value ,ivar)))))
				 ivars))
      ,@body)))

;;; If any of the argspecs denote a value of type :<BOOL>, push an
;;; appropriate SETQ on the front of the body.  (Order doesn't matter.)
(defun coerce-foreign-boolean-args (argspecs body)
  (do* ((argspecs argspecs (cddr argspecs))
	(type (car argspecs) (car argspecs))
	(var (cadr argspecs) (cadr argspecs)))
       ((null argspecs) body)
    (when (eq type :<BOOL>)
      (push `(setq ,var (not (eql ,var 0))) body))))
      
(defun lisp-boolean->foreign-boolean (form)
  (let* ((val (gensym)))
    `((let* ((,val (progn ,@form)))
	(if (and ,val (not (eql 0 ,val))) 1 0)))))

;;; Return, as multiple values:
;;;  the selector name, as a string
;;;  the ObjC class name, as a string
;;;  the foreign result type
;;;  the foreign argument type/argument list
;;;  the body
;;;  a string which encodes the foreign result and argument types
(defun parse-objc-method (selector-arg class-arg body)
  (let* ((class-name (objc-class-name-string class-arg))
	 (selector-form selector-arg)
	 (selector nil)
	 (argspecs nil)
	 (resulttype nil))
    (flet ((bad-selector (why) (error "Can't parse method selector ~s : ~a"
				   selector-arg why)))
      (typecase selector-form
	(string
	 (let* ((specs (pop body)))
	     (setq selector selector-form)
	     (if (evenp (length specs))
	       (setq argspecs specs resulttype :id)
	       (setq resulttype (car (last specs))
		     argspecs (butlast specs)))))
	(cons				;sic
	 (setq resulttype (pop selector-form))
	 (unless (consp selector-form)
	   (bad-selector "selector-form not a cons"))
	 (arch::collect ((components)
			 (specs))
	   ;; At this point, selector-form should be either a list of
	   ;; a single symbol (a lispified version of the selector name
	   ;; of a selector that takes no arguments) or a list of keyword/
	   ;; variable pairs.  Each keyword is a lispified component of
	   ;; the selector name; each "variable" is either a symbol
	   ;; or a list of the form (<foreign-type> <symbol>), where
	   ;; an atomic variable is shorthand for (:id <symbol>).
	   (if (and (null (cdr selector-form))
		    (car selector-form)
		    (typep (car selector-form) 'symbol)
		    (not (typep (car selector-form) 'keyword)))
	     (components (car selector-form))
	     (progn
	       (unless (evenp (length selector-form))
		 (bad-selector "Odd length"))
	       (do* ((s selector-form (cddr s))
		     (comp (car s) (car s))
		     (var (cadr s) (cadr s)))
		    ((null s))
		 (unless (typep comp 'keyword) (bad-selector "not a keyword"))
		 (components comp)
		 (cond ((atom var)
			(unless (and var (symbolp var))
			  (bad-selector "not a non-null symbol"))
			(specs :id)
			(specs var))
		       ((and (consp (cdr var))
			     (null (cddr var))
			     (cadr var)
			     (symbolp (cadr var)))
			(specs (car var))
			(specs (cadr var)))
		       (t (bad-selector "bad variable/type clause"))))))
	   (setq argspecs (specs)
		 selector (lisp-to-objc-message (components)))))
	(t (bad-selector "general failure")))
      ;; If the result type is of the form (:STRUCT <typespec> <name>),
      ;; make <name> be the first argument (of type :address) and
      ;; make the resulttype :void
      (when (and (consp resulttype)
		 (eq (car resulttype) :struct))
	(destructuring-bind (typespec name) (cdr resulttype)
	(if (and (typep name 'symbol)
		 (typep (parse-foreign-type typespec)
			'foreign-record-type))
	  (progn
	    (push name argspecs)
	    (push :address argspecs)
	    (setq resulttype :void))
	  (bad-selector "Bad struct return type"))))
      (values selector
	      class-name
	      resulttype
	      argspecs
	      body
	      (do* ((argtypes ())
		    (argspecs argspecs (cddr argspecs)))
		   ((null argspecs) (encode-objc-method-arglist
				     `(:id :<sel> ,@(nreverse argtypes))
				     resulttype))
		(push (car argspecs) argtypes))))))

(defun objc-method-definition-form (class-p selector-arg class-arg body env)
  (multiple-value-bind (selector-name
			class-name
			resulttype
			argspecs
			body
			typestring)
      (parse-objc-method selector-arg class-arg body)
      (multiple-value-bind (body decls) (parse-body body env)
	(setq body (coerce-foreign-boolean-args argspecs body))
	(if (eq resulttype :<BOOL>)
	  (setq body (lisp-boolean->foreign-boolean body)))
	(let* ((impname (intern (format nil "~c[~a ~a]"
					(if class-p #\+ #\-)
					class-name
					selector-name)))
	       (self (intern "SELF"))
	       (_cmd (intern "_CMD"))
	       ;; SUPER should probably be a gensym, but that'd break
	       ;; the old [:super ...] construct
	       (super 'super) 
	       (params `(:id ,self :<sel> ,_cmd ,@argspecs)))
	  `(progn
	    (with-ivar-symbol-macros
		,class-name ,self
		(defcallback ,impname
		    (:without-interrupts nil
					 #+openmcl-native-threads :error-return
					 #+openmcl-native-threads (condition objc-callback-error-return) ,@params ,resulttype)
		  (declare (ignorable ,_cmd))
		  ,@decls
		  (rlet ((,super :objc_super
			   :receiver self
			   :class
			   ,@(if class-p
				 `((pref
				    (pref (@class ,class-name)
				     :objc_class.isa)
				    :objc_class.super_class))
				 `((pref (@class ,class-name) :objc_class.super_class)))))
		    (macrolet ((send-super (msg &rest args &environment env) 
				 (make-optimized-send nil msg args env nil ',super ,class-name))
			       (send-super/stret (s msg &rest args &environment env) 
				 (make-optimized-send nil msg args env s ',super ,class-name)))
		      (flet ((%send-super (msg &rest args)
			       (make-general-send nil msg args nil ,super ,class-name))
			     (%send-super/stret (s msg &rest args)
			       (make-general-send nil msg args s ,super ,class-name)))
			,@body)))))
	    (%define-lisp-objc-method
	     ',impname
	     ,class-name
	     ,selector-name
	     ,typestring
	     ,impname
	     ,class-p))))))

(defmacro define-objc-method ((selector-arg class-arg)
			      &body body &environment env)
  (objc-method-definition-form nil selector-arg class-arg body env))

(defmacro define-objc-class-method ((selector-arg class-arg)
				     &body body &environment env)
  (objc-method-definition-form t selector-arg class-arg body env))


;;; Getting & setting instance variables.

;;; This works best if the value is a pointer of some sort.  If it's
;;; hard to arrange that, lookup the instance variable's offset (see
;;; below) and use (SETF (CCL:GET-??? ...) ...) directly.
(defun set-objc-instance-variable (instance name value)
  (with-cstrs ((cname (if (typep name 'string)
			name
			(unescape-foreign-name name))))
    (if (%null-ptr-p (#_object_setInstanceVariable instance cname value))
      (error "Unknown instance varaiable ~s in ~s" name (objc-class-name instance)))))

;;; This returns a pointer (conses).  If you want to avoid either of
;;; those behaviors, lookup the instance variable's offset and use
;;; CCL::%GET-xxx directly.
(defun get-objc-instance-variable (instance name)
  (with-cstrs ((cname (if (typep name 'string)
			name
			(unescape-foreign-name name))))
    (rlet ((valptr (* t)))
      (if (%null-ptr-p (#_object_getInstanceVariable instance cname valptr))
	(error "Unknown instance varaiable ~s in ~s" name (objc-class-name instance))
	(%get-ptr valptr)))))
    
;;; One might like something a little higher-level than what this offers,
;;; and one might like to do the lookup at macroexpand-time.  The latter
;;; can only happen if the class is defined at macroexpand-time, which
;;; isn't generally guaranteed.  If we're going to have to lookup the
;;; ivar's offset at runtime, we might as well keep things simple.
(defun %ivar-offset (class varname)
  (with-cstrs ((name (unescape-foreign-name varname)))
    (with-macptrs ((ivar))
      (%setf-macptr ivar
		    (#_class_getInstanceVariable
		     class
		     name))
      (if (%null-ptr-p ivar)
	(error "Unknown instance variable: ~s" varname)
	(pref ivar :objc_ivar.ivar_offset)))))




	  



(defloadvar *nsstring-newline* #@"
")


;;; Execute BODY with an autorelease pool

(defun create-autorelease-pool ()
  (objc-message-send
   (objc-message-send (@class "NSAutoreleasePool") "alloc") "init"))

(defun release-autorelease-pool (p)
  (objc-message-send p "release"))

(defmacro with-autorelease-pool (&body body)
  (let ((pool-temp (gensym)))
    `(let ((,pool-temp (create-autorelease-pool)))
      (unwind-protect
	   ,@body
	(release-autorelease-pool ,pool-temp)))))

;;; This can fail if the nsstring contains non-8-bit characters.
(defun lisp-string-from-nsstring (nsstring)
  (with-macptrs (cstring)
    (%setf-macptr cstring [nsstring "cString" (* :char)])
    (unless (%null-ptr-p cstring)
      (%get-cstring cstring))))

(defmacro with-ns-exceptions-as-errors (&body body)
  (let* ((nshandler (gensym))
         (cframe (gensym)))
    `(rletZ ((,nshandler :<NSH>andler2))
      (unwind-protect
           (progn
             (external-call "__NSAddHandler2" :address ,nshandler :void)
             (catch ,nshandler
               (with-c-frame ,cframe
                 (%associate-jmp-buf-with-catch-frame
                  ,nshandler
                  (%fixnum-ref (%current-tcr) arch::tcr.catch-top)
                  ,cframe)
                 (progn
                   ,@body))))
        (check-ns-exception ,nshandler)))))

(defun check-ns-exception (nshandler)
  (with-macptrs ((exception (external-call "__NSExceptionObjectFromHandler2"
                                           :address nshandler
                                           :address)))
    (if (%null-ptr-p exception)
      (external-call "__NSRemoveHandler2" :address nshandler :void)
      (error (ns-exception->lisp-condition (%inc-ptr exception 0))))))

