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


(defun show-uvector (u)
  (dotimes (i (uvsize u) (values))
    (format t "~&~d : ~s" i (uvref u i))
    (force-output)))

;;; Utilities for interacting with the Apple/GNU Objective-C runtime
;;; systems.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+darwinppc-target (pushnew :apple-objc *features*)
  #+linuxppc-target (pushnew :gnu-objc *features*)
  #-(or darwinppc-target linuxppc-target)
  (error "Not sure what ObjC runtime system to use."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\#
   #\@
   (nfunction
    |objc-#@-reader|
    (lambda (stream subchar numarg)
      (declare (ignore subchar numarg))
      (let* ((string (read stream)))
	(check-type string string)
	`(@ ,string))))))

(eval-when (:compile-toplevel :execute)
  #+apple-objc
  (use-interface-dir :cocoa)
  #+gnu-objc
  (use-interface-dir :gnustep))

(defpackage "OBJC"
  (:use)
  (:export "OBJC-OBJECT" "OBJC-CLASS-OBJECT" "OBJC-CLASS" "OBJC-METACLASS"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SPLAY-TREE")
  (require "NAME-TRANSLATION")
  (require "PROCESS-OBJC-MODULES")
  (require "OBJC-CLOS"))

(defloadvar *NSApp* nil )


(defun ensure-objc-classptr-resolved (classptr)
  #+apple-objc (declare (ignore classptr))
  #+gnu-objc
  (unless (logtest #$_CLS_RESOLV (pref classptr :objc_class.info))
    (external-call "__objc_resolve_class_links" :void)))

(let* ((objc-class-map (make-splay-tree #'%ptr-eql
					#'(lambda (x y)
					    (< (the (unsigned-byte 32)
						 (%ptr-to-int x))
					       (the (unsigned-byte 32)
						 (%ptr-to-int Y))))))
       (objc-metaclass-map (make-splay-tree #'%ptr-eql
					    #'(lambda (x y)
						(< (the (unsigned-byte 32)
						     (%ptr-to-int x))
						   (the (unsigned-byte 32)
						     (%ptr-to-int Y))))))
       (objc-class-lock (make-lock))
       (next-objc-class-id 0)
       (class-table-size 1024)
       (c (make-array 1024))
       (m (make-array 1024))
       (cw (make-array 1024))
       (mw (make-array 1024))
       (csv (make-array 1024))
       (msv (make-array 1024)))

  (flet ((assign-next-class-id ()
           (let* ((id next-objc-class-id))
             (if (= (incf next-objc-class-id) class-table-size)
               (let* ((old-size class-table-size)
                      (new-size (* 2 class-table-size)))
                 (declare (fixnum old-size new-size))
                 (macrolet ((extend (v)
                              `(setq ,v (%extend-vector old-size ,v new-size))))
                   (extend c)
                   (extend m)
                   (extend cw)
                   (extend mw)
                   (extend csv)
                   (extend msv))
                 (setq class-table-size new-size)))
             id)))
    (defun id->objc-class (i)
      (svref c i))
    (defun (setf id->objc-class) (new i)
      (setf (svref c i) new))
    (defun id->objc-metaclass (i)
      (svref m i))
    (defun (setf id->objc-metaclass) (new i)
      (setf (svref m i) new))
    (defun id->objc-class-wrapper (i)
      (svref cw i))
    (defun (setf id->objc-class-wrapper) (new i)
      (setf (svref cw i) new))
    (defun id->objc-metaclass-wrapper (i)
      (svref mw i))
    (defun (setf id->objc-metaclass-wrapper) (new i)
      (setf (svref mw i) new))
    (defun id->objc-class-slots-vector (i)
      (svref csv i))
    (defun (setf id->objc-class-slots-vector) (new i)
      (setf (svref csv i) new))
    (defun id->objc-metaclass-slots-vector (i)
      (svref msv i))
    (defun (setf id->objc-metaclass-slots-vector) (new i)
      (setf (svref msv i) new))
    
    (defun %clear-objc-class-maps ()
      (with-lock-grabbed (objc-class-lock)
        (fill c 0)
        (fill m 0)
        (fill cw 0)
        (fill mw 0)
        (fill csv 0)
        (fill msv 0)
        (setf (splay-tree-root objc-class-map) nil
              (splay-tree-root objc-metaclass-map) nil
              (splay-tree-count objc-class-map) 0
              (splay-tree-count objc-metaclass-map) 0
              next-objc-class-id 0)))
    (defun map-objc-class (class &optional foreign)
      "ensure that the class (and metaclass) are mapped to a small integer"
      (with-lock-grabbed (objc-class-lock)
	(labels ((ensure-mapped-class (class)
		   (with-macptrs ((super (pref class :objc_class.super_class)))
		     (unless (%null-ptr-p super)
		       (ensure-mapped-class super)))
		   (or (splay-tree-get objc-class-map class)
		       (let* ((id (assign-next-class-id))
			      (class (%inc-ptr class 0))
			      (meta (pref class #+apple-objc :objc_class.isa #+gnu-objc :objc_class.class_pointer)))
			 (ensure-objc-classptr-resolved class)
			 (splay-tree-put objc-class-map class id)
			 (splay-tree-put objc-metaclass-map meta id)
			 (setf (svref c id) class
			       (svref m id) meta)
			 (let* ((class-name (objc-to-lisp-classname
					     (%get-cstring
					      (pref class :objc_class.name))
					     "NS"))
				(metaclass-name (intern (concatenate 'string "+" (string class-name)) (symbol-package class-name)))
				(class-wrapper (%cons-wrapper class))
				(meta-wrapper (%cons-wrapper meta))
				(class-slot-vector
				 (initialize-objc-class-slots class
							      class-name
							      class-wrapper
							      foreign))
				(meta-slot-vector
				 (initialize-objc-metaclass-slots
				  meta
				  metaclass-name
				  meta-wrapper
				  foreign
				  class)))
			   (when (eq (find-package "NS")
				     (symbol-package class-name))
			     (export class-name "NS")
			     (export metaclass-name "NS"))
			 (setf (svref cw id) class-wrapper
			       (svref mw id) meta-wrapper
			       (svref csv id) class-slot-vector
			       (svref msv id) meta-slot-vector
			       (find-class class-name) class
			       (find-class metaclass-name) meta)
			 )
			 id))))
	  (ensure-mapped-class class))))
    (defun objc-class-id (class)
      (with-lock-grabbed (objc-class-lock)
        (splay-tree-get objc-class-map class)))
    (defun objc-metaclass-id (meta)
      (with-lock-grabbed (objc-class-lock)
        (splay-tree-get objc-metaclass-map meta)))
    (defun objc-class-map () objc-class-map)
    (defun objc-metaclass-map () objc-metaclass-map)))

(pushnew #'%clear-objc-class-maps *save-exit-functions* :test #'eq
         :key #'function-name)

(defun do-all-objc-classes (f)
  (map-splay-tree (objc-class-map) #'(lambda (id)
				       (funcall f (id->objc-class id)))))



#+darwinppc-target
(progn
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

(pushnew 'remap-all-library-classes *lisp-system-pointer-functions*)



)					;#+darwinppc-target

#+gnu-objc
(progn


(defparameter *gnustep-system-root* "/usr/GNUstep/" "The root of all evil.")
(defparameter *gnustep-libraries-pathname*
  (merge-pathnames "System/Library/Libraries/" *gnustep-system-root*))

(defloadvar *pending-loaded-classes* ())

(defcallback register-class-callback (:address class :address category :void)
  (let* ((id (map-objc-class class)))
    (unless (%null-ptr-p category)
      (let* ((cell (or (assoc id *pending-loaded-classes*)
                       (let* ((c (list id)))
                         (push c *pending-loaded-classes*)
                         c))))
        (push (%inc-ptr category 0) (cdr cell))))))


(defun init-gnustep-framework ()
  (or (getenv "GNUSTEP_SYSTEM_ROOT")
      (setenv "GNUSTEP_SYSTEM_ROOT" *gnustep-system-root*))
  (open-shared-library "libobjc.so.1")
  (setf (%get-ptr (foreign-symbol-address "_objc_load_callback"))
        register-class-callback)
  (open-shared-library (namestring (merge-pathnames "libgnustep-base.so"
                                                    *gnustep-libraries-pathname*)))
  (open-shared-library (namestring (merge-pathnames "libgnustep-gui.so"
                                                    *gnustep-libraries-pathname*))))

(def-ccl-pointers gnustep-framework ()
  (init-gnustep-framework))
)



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
   #+apple-objc
  (foreign-symbol-address "__NSConstantStringClassReference")
  #+gnu-objc
  (with-cstrs ((name "NSConstantString"))
      (#_objc_lookup_class name)))


;;; Execute the body with the variable NSSTR bound to a
;;; stack-allocated NSConstantString instance (made from
;;; *NSConstantString-class*, CSTRING and LEN).
(defmacro with-nsstr ((nsstr cstring len) &body body)
  #+apple-objc
  `(rlet ((,nsstr :<NSC>onstant<S>tring
	   :isa *NSConstantString-class*
	   :bytes ,cstring
	   :num<B>ytes ,len))
      ,@body)
  #+gnu-objc
  `(rlet ((,nsstr :<NXC>onstant<S>tring
	   :isa *NSConstantString-class*
	   :c_string ,cstring
	   :len ,len))
    ,@body))

;;; Make a persistent (heap-allocated) NSConstantString.

(defun %make-nsstring (string)
  "Make a persistent (heap-allocated) NSConstantString from the
argument lisp string."
  #+apple-objc
  (make-record :<NSC>onstant<S>tring
	       :isa *NSConstantString-Class*
	       :bytes (make-cstring string)
	       :num<B>ytes (length string))
  #+gnu-objc
  (make-record :<NXC>onstant<S>tring
	       :isa *NSConstantString-Class*
	       :c_string (make-cstring string)
	       :len (length string))
  )


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




;;; Registering named objc classes.


(defun objc-class-name-string (name)
  (etypecase name
    (symbol (lisp-to-objc-classname name))
    (string name)))

;;; We'd presumably cache this result somewhere, so we'd only do the
;;; lookup once per session (in general.)
(defun lookup-objc-class (name &optional error-p)
  (with-cstrs ((cstr (objc-class-name-string name)))
    (let* ((p (#+apple-objc #_objc_lookUpClass #+gnu-objc
	       #_objc_lookup_class
	       cstr)))
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
  #+apple-objc
  (with-macptrs (p)
    (%setf-macptr p (#_object_getClassName object))
    (unless (%null-ptr-p p)
      (%get-cstring p)))
  #+gnu-objc
  (unless (%null-ptr-p object)
    (with-macptrs ((parent (pref object :objc_object.class_pointer)))
      (unless (%null-ptr-p parent)
        (if (logtest (pref parent :objc_class.info) #$_CLS_CLASS)
          (%get-cstring (pref parent :objc_class.name))
          (%get-cstring (pref object :objc_class.name)))))))


;;; Likewise, we want to cache the selectors ("SEL"s) which identify
;;; method names.  They can vary from session to session, but within
;;; a session, all methods with a given name (e.g, "init") will be
;;; represented by the same SEL.
(defun get-selector-for (method-name &optional error)
  (with-cstrs ((method-name method-name))
    (let* ((p (#+apple-objc #_sel_getUid
	       #+gnu-objc #_sel_get_any_uid
	       method-name)))
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
  (%get-cstring
   #+apple-objc sel
   #+gnu-objc (#_sel_get_name sel)))

;;; #_objc_msgSend takes two required arguments (the receiving object
;;; and the method selector) and 0 or more additional arguments;
;;; there'd have to be some macrology to handle common cases, since we
;;; want the compiler to see all of the args in a foreign call.

;;; I don't remmber what the second half of the above comment might
;;; have been talking about.

(defmacro objc-message-send (receiver selector-name &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  #+apple-objc
  `(external-call "_objc_msgSend"
    :id ,receiver
    :<SEL> (@selector ,selector-name)
    ,@argspecs)
  #+gnu-objc
    (let* ((r (gensym))
	 (s (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,r ,receiver)
		    (,s (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup"
					:id ,r
					:<SEL> ,s
					:<IMP>)))
      (ff-call ,imp :id ,r :<SEL> ,s ,@argspecs))))

;;; A method that returns a structure (whose size is > 4 bytes on
;;; darwin, in all cases on linuxppc) does so by copying the structure
;;; into a pointer passed as its first argument; that means that we
;;; have to invoke the method via #_objc_msgSend_stret in the #+apple-objc
;;; case.

(defmacro objc-message-send-stret (structptr receiver selector-name &rest argspecs)
  (if (evenp (length argspecs))
    (setq argspecs (append argspecs '(:void)))
    (unless (member (car (last argspecs)) '(:void nil))
      (error "Invalid result spec for structure return: ~s"
	     (car (last argspecs)))))
  #+apple-objc
  `(external-call "_objc_msgSend_stret"
    :address ,structptr
    :id ,receiver
    :<SEL> (@selector ,selector-name)
    ,@argspecs)
    #+gnu-objc
    (let* ((r (gensym))
	 (s (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,r ,receiver)
		    (,s (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup"
					 :id ,r
					 :<SEL> ,s
					 :<IMP>)))
      (ff-call ,imp :address ,structptr :id ,r :<SEL> ,s ,@argspecs))))

;;; #_objc_msgSendSuper is similar to #_objc_msgSend; its first argument
;;; is a pointer to a structure of type objc_super {self,  the defining
;;; class's superclass}.  It only makes sense to use this inside an
;;; objc method.
(defmacro objc-message-send-super (super selector-name &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  #+apple-objc
  `(external-call "_objc_msgSendSuper"
    :address ,super
    :<SEL> (@selector ,selector-name)
    ,@argspecs)
  #+gnu-objc
  (let* ((sup (gensym))
	 (sel (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,sup ,super)
		    (,sel (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup_super"
					 :<S>uper_t ,sup
					 :<SEL> ,sel
					 :<IMP>)))
      (ff-call ,imp
       :id (pref ,sup :<S>uper.self)
       :<SEL> ,sel
       ,@argspecs))))

;;; Send to superclass method, returning a structure.
(defmacro objc-message-send-super-stret
    (structptr super selector-name &rest argspecs)
  (if (evenp (length argspecs))
    (setq argspecs (append argspecs '(:void)))
    (unless (member (car (last argspecs)) '(:void nil))
      (error "Invalid result spec for structure return: ~s"
	     (car (last argspecs)))))
  #+apple-objc
  `(external-call "_objc_msgSendSuper_stret"
    :address ,structptr
    :address ,super
    :<SEL> (@selector ,selector-name)
    ,@argspecs)
  #+gnu-objc
  (let* ((sup (gensym))
	 (sel (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,sup ,super)
		    (,sel (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup_super"
					 :<S>uper_t ,sup
					 :<SEL> ,sel
					 :<IMP>)))
      (ff-call ,imp
       :address ,structptr
       :id (pref ,sup :<S>uper.self)
       :<SEL> ,sel
       ,@argspecs))))



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

;;; In Apple Objc, a class's methods are stored in a (-1)-terminated
;;; vector of method lists.  In GNU ObjC, method lists are linked
;;; together.
(defun %make-method-vector ()
  #+apple-objc
  (let* ((method-vector (malloc 16)))
    (setf (%get-signed-long method-vector 0) 0
	  (%get-signed-long method-vector 4) 0
	  (%get-signed-long method-vector 8) 0
	  (%get-signed-long method-vector 12) -1)
    method-vector))
  

;;; Make a meta-class object (with no instance variables or class
;;; methods.)
(defun %make-basic-meta-class (nameptr superptr rootptr)
  #+apple-objc
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
		 :protocols (%null-ptr)))
  #+gnu-objc
  (make-record :objc_class
               :class_pointer (pref rootptr :objc_class.class_pointer)
               :super_class (pref superptr :objc_class.class_pointer)
               :name nameptr
               :version 0
               :info #$_CLS_META
               :instance_size 0
               :ivars (%null-ptr)
               :methods (%null-ptr)
               :dtable (%null-ptr)
               :subclass_list (%null-ptr)
               :sibling_class (%null-ptr)
               :protocols (%null-ptr)
               :gc_object_type (%null-ptr)))

(defun %make-class-object (metaptr superptr nameptr ivars instance-size)
  #+apple-objc
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
		 :protocols (%null-ptr)))
  #+gnu-objc
  (make-record :objc_class
		 :class_pointer metaptr
		 :super_class superptr
		 :name nameptr
		 :version 0
		 :info #$_CLS_CLASS
		 :instance_size instance-size
		 :ivars ivars
		 :methods (%null-ptr)
		 :dtable (%null-ptr)
		 :protocols (%null-ptr)))

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

(defun %class-find-ivar-offset (class ivar-string)
  #+apple-objc
  (with-cstrs ((s ivar-string))
    (with-macptrs ((ivar))
      (%setf-macptr ivar (#_class_getInstanceVariable class s))
      (unless (%null-ptr-p ivar)
	(pref ivar :objc_ivar.ivar_offset))))
  #+gnu-objc
  (with-cstrs ((s ivar-string))
    (do* ((class class (pref class :objc_class.super_class)))
         ((%null-ptr-p class))
      (let* ((offset (with-macptrs ((ivars (pref class :objc_class.ivars)))
                       (unless (%null-ptr-p ivars)
                         (do* ((i 0 (1+ i))
                               (n (pref ivars :objc_ivar_list.ivar_count))
                               (ivar (pref ivars :objc_ivar_list.ivar_list)
                                     (%inc-ptr ivar (record-length :objc_ivar))))
                              ((= i n))
                           (with-macptrs ((name (pref ivar :objc_ivar.ivar_name)))
                             (unless (%null-ptr-p name)
                               (if (eql 0 (#_strcmp name s))
                                 (return (pref ivar :objc_ivar.ivar_offset))))))))))
        (when offset (return offset))))))

(defun find-class-ivar-offset (classname ivar-string)
  (or 
   (%class-find-ivar-offset (lookup-objc-class classname t) ivar-string)
   (error "Unknown instance variable ~s in class ~s" ivar-string classname)))


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

#+gnu-objc
(progn
(defloadvar *gnu-objc-runtime-mutex*
    (%get-ptr (foreign-symbol-address "__objc_runtime_mutex")))
(defmacro with-gnu-objc-mutex-locked ((mutex) &body body)
  (let* ((mname (gensym)))
    `(let ((,mname ,mutex))
      (unwind-protect
	   (progn
	     (external-call "objc_mutex_lock" :address ,mname :void)
	     ,@body)
	(external-call "objc_mutex_lock" :address ,mname :void)))))
)

(defun %objc-metaclass-p (class)
  (logtest (pref class :objc_class.info)
	   #+apple-objc #$CLS_META
	   #+gnu-objc #$_CLS_META))
	   
(defun %add-objc-class (class)
  #+apple-objc
  (#_objc_addClass class)
  ;; Reading the fine print (e.g., the source), we learn that it's
  ;; necessary to grab a mutex (lock) around the call to
  ;; #___objc_add_class_to_hash.  Naturally.  Why would anyone want to
  ;; (easily) add a new class procedurally ?
  #+gnu-objc
  (with-gnu-objc-mutex-locked (*gnu-objc-runtime-mutex*)
      (external-call "__objc_add_class_to_hash" :address class :void)))
  
(defun %define-objc-class (info)
  (let* ((descriptor (objc-class-info-objc-class info)))
    (or (%objc-class-classptr descriptor nil)
	(let* ((class (%make-objc-class (objc-class-info-classname info)
					(objc-class-info-superclassname info)
					(objc-class-info-ivars info))))
	  (%add-objc-class class)
	  (map-objc-class class)
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


;;; If P is an ObjC class (or metaclass), return the class & metaclass,
;;; else return (VALUES NIL NIL).
(defun objc-class-p (p)
  (if (typep p 'macptr)
    (let* ((id (or (objc-class-id p) (objc-metaclass-id p))))
      (if id
	(values (id->objc-class id) (id->objc-metaclass id))
	(values nil nil)))))

;;; If P is an ObjC instance, return a pointer to its class.
;;; This assumes that all instances are allocated via something that's
;;; ultimately malloc-based.
(defun objc-instance-p (p)
  (and (typep p 'macptr)
       #+apple-objc
       (not (%null-ptr-p (#_malloc_zone_from_ptr p)))
       ;; #_malloc_zone_from_pointer seems pretty robust.
       ;; If it returned a non-null "zone", it's probably safe
       ;; to indirect through P.
       (with-macptrs ((parent (pref p
				    #+apple-objc :objc_object.isa
				    #+gnu-objc :objc_object.class_pointer)))
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
	(values :metaclass class)
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
  #-apple-objc (declare (ignore classptr selector typestring imp))
  #+apple-objc
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
			(%incf-ptr method (record-length :objc_method))))
	       ((= i n))
	    (declare (fixnum i n))
	    (when (eql selector (pref method :objc_method.method_name))
	      (#_class_removeMethods classptr mlist)
	      (setf (pref method :objc_method.method_imp) imp)
	      (return-from %mlist-containing mlist)))))))
	      

(defun %add-objc-method (classptr selector typestring imp)
  #+apple-objc
  (#_class_addMethods classptr
		      (%mlist-containing classptr selector typestring imp))
  #+gnu-objc
  ;;; We have to do this ourselves, and have to do it with the runtime
  ;;; mutex held.
  (with-gnu-objc-mutex-locked (*gnu-objc-runtime-mutex*)
    (flet ((find-mlist ()
	     (do* ((sel-uid (#_sel_get_uid selector))
		   (prev (%inc-ptr classptr :objc_class.methods)
			 (%inc-ptr mlist :objc_method_list.method_next))
		   (mlist (%get-ptr prev) (%get-ptr prev)))
		  ((%null-ptr-p mlist)
		   (let* ((new-mlist (make-record :objc_method_list
						  :method_count 1))
			  (method (pref new-mlist :objc_method_list.method_list))
			  (ctypestring (make-cstring typestring))
			  (newsel (#_sel_register_typed_name
				   (#_sel_get_uid selector) ctypestring)))
		     (setf (pref method :objc_method.method_name) newsel
			   (pref method :objc_method.method_types) ctypestring
			   (pref method :objc_method.method_imp) imp)
		     new-mlist))
	       (let* ((existing 
		       (do* ((method (pref mlist :objc_method_list.method_list)
				     (%inc-ptr method (record-length :objc_method)))
			     (i 0 (1+ i))
			     (n (pref mlist :objc_method_list.method_count)))
			    ((= i n))
			 (with-macptrs ((method-sel (pref method :objc_method.method_name)))
			   (unless (%null-ptr-p method-sel)
			     (when (eql sel-uid (#_sel_get_uid method-sel))
			       (setf (pref method :objc_method.method_imp)
				     imp)
			       (return mlist)))))))
		 (when existing
		   (setf (%get-ptr prev) (%null-ptr)
			 (pref existing :objc_method_list.method_next)
			 (%null-ptr))
		   (return existing))))))
      (let* ((mlist (find-mlist)))
	(setf (pref mlist :objc_method_list.method_next)
	      (pref classptr :objc_class.methods)
	      (pref classptr :objc_class.methods) mlist)
	(#___objc_update_dispatch_table_for_class classptr)))))

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
       (pref class #+apple-objc :objc_class.isa #+gnu-objc :objc_class.class_pointer)
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
	 (ccl::collect ((components)
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
	       (super (gensym "SUPER")) 
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
			   :receiver ,self
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
			       (make-general-send nil msg args s ,super ,class-name))
			     (super () ,super))
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

(defun class-get-instance-method (class sel)
  #+apple-objc (#_class_getInstanceMethod class sel)
  #+gnu-objc (#_class_get_instance_method class sel))

(defun class-get-class-method (class sel)
  #+apple-objc (#_class_getClassMethod class sel)
  #+gnu-objc   (#_class_get_class_method class sel))

(defun method-get-number-of-arguments (m)
  #+apple-objc (#_method_getNumberOfArguments m)
  #+gnu-objc (#_method_get_number_of_arguments m))




;;; Getting & setting instance variables.

;;; This works best if the value is a pointer of some sort.  If it's
;;; hard to arrange that, lookup the instance variable's offset (see
;;; below) and use (SETF (CCL:%GET-??? ...) ...) directly.
(defun set-objc-instance-variable (instance name value)
  (let* ((ivar-name (if (typep name 'string)
		      name
		      (unescape-foreign-name name))))
    #+apple-objc
    (with-cstrs ((cname ivar-name))
      (if (%null-ptr-p (#_object_setInstanceVariable instance cname value))
	(error "Unknown instance varaiable ~s in ~s" name (objc-class-name instance)))
      value)
        #+gnu-objc
    (let* ((offset (%class-find-ivar-offset (pref instance :objc_object.class_pointer) ivar-name)))
      (if offset
	(setf (%get-ptr instance offset) value)
	(error "Unknown instance varaiable ~s in ~s" name (objc-class-name instance))))))

;;; This returns a pointer (conses).  If you want to avoid either of
;;; those behaviors, lookup the instance variable's offset and use
;;; CCL::%GET-xxx directly.
(defun get-objc-instance-variable (instance name)
  (let* ((ivar-name (if (typep name 'string)
		      name
		      (unescape-foreign-name name))))
    #+apple-objc
    (with-cstrs ((cname ivar-name)) 
      (rlet ((valptr (* t)))
	(if (%null-ptr-p (#_object_getInstanceVariable instance cname valptr))
	  (error "Unknown instance varaiable ~s in ~s" name (objc-class-name instance))
	  (%get-ptr valptr))))
    #+gnu-objc
    (let* ((offset (%class-find-ivar-offset (pref instance :objc_object.class_pointer) ivar-name)))
      (if offset
	(%get-ptr instance offset)
	(error "Unknown instance varaiable ~s in ~s" name (objc-class-name instance))))))
    
;;; One might like something a little higher-level than what this offers,
;;; and one might like to do the lookup at macroexpand-time.  The latter
;;; can only happen if the class is defined at macroexpand-time, which
;;; isn't generally guaranteed.  If we're going to have to lookup the
;;; ivar's offset at runtime, we might as well keep things simple.
(defun %ivar-offset (class varname)
  (or
   (%class-find-ivar-offset class (unescape-foreign-name varname))
   (error "Unknown instance variable: ~s" varname)))




	  



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
    (%setf-macptr cstring (objc-message-send nsstring "cString" (* :char)))
    (unless (%null-ptr-p cstring)
      (%get-cstring cstring))))

(defmacro with-ns-exceptions-as-errors (&body body)
  #+apple-objc
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
                  (%fixnum-ref (%current-tcr) ppc32::tcr.catch-top)
                  ,cframe)
                 (progn
                   ,@body))))
        (check-ns-exception ,nshandler))))
  #+gnu-objc
  `(progn ,@body)
  )

#+apple-objc
(defun check-ns-exception (nshandler)
  (with-macptrs ((exception (external-call "__NSExceptionObjectFromHandler2"
                                           :address nshandler
                                           :address)))
    (if (%null-ptr-p exception)
      (external-call "__NSRemoveHandler2" :address nshandler :void)
      (error (ns-exception->lisp-condition (%inc-ptr exception 0))))))

#+apple-objc
(progn
  (let* ((class-count 0))
    (declare (fixnum class-count))
    (defun reset-objc-class-count () (setq class-count 0))
    (defun map-objc-classes ()
      (let* ((n (#_objc_getClassList (%null-ptr) 0)))
	(declare (fixnum n))
	(if (> n class-count)
	  (%stack-block ((buffer (the fixnum (ash n ppc32::word-shift))))
	    (#_objc_getClassList buffer n)
	  (do* ((i class-count (1+ i)))
	       ((= i n (setq class-count i)))
	    (declare (fixnum i))
	    (map-objc-class
	     (%get-ptr buffer (the fixnum  (ash i ppc32::word-shift)))
	     t)))))))
  (def-ccl-pointers revive-objc-classes ()
    (reset-objc-class-count)
    (map-objc-classes)))
    