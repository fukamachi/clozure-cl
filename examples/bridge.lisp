;;;; -*- Mode: Lisp; Package: CCL -*-
;;;; bridge.lisp
;;;;
;;;; A Lisp bridge for Cocoa
;;;;
;;;; This provides:
;;;;   (1) Convenient Lisp syntax for instantiating ObjC classes
;;;;   (2) Convenient Lisp syntax for invoking ObjC methods
;;;;   (3) Ability to define CLOS subclasses of ObjC class with Lisp slots
;;;;       and have their instances be usable as ObjC objects (eventually)
;;;;   (4) Ability to define CLOS methods on ObjC classes and have them be
;;;;       invoked by ObjC if they match an ObjC method name (eventually)
;;;;
;;;; Copyright (c) 2003 Randall D. Beer
;;;; 
;;;; This software is licensed under the terms of the Lisp Lesser GNU Public
;;;; License, known as the LLGPL.  The LLGPL consists of a preamble and 
;;;; the LGPL. Where these conflict, the preamble takes precedence.  The 
;;;; LLGPL is available online at http://opensource.franz.com/preamble.html.
;;;;
;;;; Please send comments and bug reports to <beer@eecs.cwru.edu>

;;; Temporary package and module stuff 

(in-package "CCL")

(require "OBJC-RUNTIME")
(require "NAME-TRANSLATION")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              Utilities                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return separate lists of the keys and values in a keyword/value list

(defun keys-and-vals (klist)
  (when (oddp (length klist))
    (error "Invalid keyword/value list: ~S" klist))
  (loop for l = klist then (cddr l)
        until (null l)
        collect (first l) into keys
        collect (second l) into vals
        finally (return (values keys vals))))


;;; Return the typestring for an ObjC METHOD 

(defun method-typestring (method)
  (%get-cstring (pref method :objc_method.method_types)))


;;; Parse the ObjC message from a SENDxxx macro

(defun parse-message (args)
  (let ((f (first args))
	(nargs (length args)))
    (cond ((or (= nargs 1) (= nargs 2))
	   ;; (THING {VARGS})
	   (if (constantp f)
	       (%parse-message (cons (eval f) (rest args)))
	     (values f (rest args) nil)))
	  ;; (THING1 ARG1 ... THINGN ARGN)
	  ((evenp nargs)
	   (multiple-value-bind (ks vs) (keys-and-vals args)
	     (if (every #'constantp ks)
		 (%parse-message (mapcan #'list (mapcar #'eval ks) vs))
	       (values f (rest args) nil))))
	  ;; (THING1 ARG1 ... THINGN ARGN VARGS)
	  (t (multiple-value-bind (ks vs) (keys-and-vals (butlast args))
	       (if (every #'constantp ks)
		   (%parse-message 
		    (nconc (mapcan #'list (mapcar #'eval ks) vs) (last args)))
		 (values f (rest args) nil)))))))


;;; Parse the ObjC message from the evaluated args of a %SENDxxx function

(defun %parse-message (args)
  (let ((f (first args))
	(l (first (last args))))
    (cond ((stringp f)
	   ;; (STRING-with-N-colons ARG1 ... ARGN {LIST}) 
	   (let* ((n (count #\: (the simple-string f)))
		  (args (rest args))
		  (nargs (length args)))
	     (cond ((= nargs n) (values f args nil))
		   ((= nargs (1+ n))
		    (values f (butlast args) l))
		   (t (error "Improperly formatted argument list: ~S" args)))))
	  ((keywordp f)
	   ;; (KEY1 ARG1 ... KEYN ARGN {LIST})
	   (let ((nargs (length args)))
	     (cond ((evenp nargs)
		    (multiple-value-bind (ks vs) (keys-and-vals args)
		      (values (lisp-to-objc-message ks) vs nil)))
		   ((and (> nargs 1) (listp l))
		    (multiple-value-bind (ks vs) (keys-and-vals (butlast args))
		      (values (lisp-to-objc-message ks) vs l)))
		 (t (error "Improperly formatted argument list: ~S" args)))))
	  ((symbolp f)
	   ;; (SYMBOL {LIST})
	   (let ((nargs (length (rest args))))
	     (cond ((= nargs 0) (values (lisp-to-objc-message (list f)) nil nil))
		   ((= nargs 1) (values (lisp-to-objc-message (list f)) nil l))
		   (t (error "Improperly formatted argument list: ~S" args)))))
	   (t (error "Improperly formatted argument list: ~S" args)))))


;;; Return the declared type of FORM in ENV

(defun declared-type (form env)
  (cond ((symbolp form)
         (multiple-value-bind (ignore ignore decls) 
                              (variable-information form env)
           (declare (ignore ignore))
           (or (cdr (assoc 'type decls)) t)))
        ((and (consp form) (eq (first form) 'the))
         (second form))
        (t t)))


;;; Return the current optimization setting of KEY in ENV

(defun optimization-setting (key &optional env)
  (cadr (assoc key (declaration-information 'optimize env))))


;;; Return the ObjC class named CNAME

(defun find-objc-class (cname)
  (%objc-class-classptr 
   (if (symbolp cname)
       (load-objc-class-descriptor (lisp-to-objc-classname cname))
     (load-objc-class-descriptor cname))))


;;; Return the class object of an ObjC object O, signalling an error
;;; if O is not an ObjC object
                      
(defun objc-class-of (o)
  (multiple-value-bind (ignore class) (objc-object-p o)
    (declare (ignore ignore))
    (if (null class)
      (error "~S is not an ObjC object" o)
      class)))


;;; Returns the ObjC class corresponding to the declared type OTYPE if
;;; possible, NIL otherwise 

(defun get-objc-class-from-declaration (otype)
  (cond ((symbolp otype)
         (lookup-objc-class (lisp-to-objc-classname otype)))
        ((and (consp otype) (eq (first otype) '@metaclass))
         (let* ((name (second otype))
                (c
                 (typecase name
                   (string (lookup-objc-class name))
                   (symbol (lookup-objc-class (lisp-to-objc-classname name)))
                   (t (error "Improper metaclass typespec: ~S" otype)))))
           (unless (null c) (objc-class-of c))))))


;;; Returns the selector of MSG 

(defun get-selector (msg)
  (%get-selector (load-objc-selector msg)))


;;; Get the instance method structure corresponding to SEL for CLASS 

(defun get-method (class sel)
  (let ((m (class-get-instance-method class sel)))
    (if (%null-ptr-p m)
      (error "Instances of ObjC class ~S cannot respond to the message ~S" 
             (objc-class-name class)
             (lisp-string-from-sel sel))
      m)))


;;; Get the class method structure corresponding to SEL for CLASS

(defun get-class-method (class sel)
  (let ((m (class-get-class-method class sel)))
    (if (%null-ptr-p m)
      (error "ObjC class ~S cannot respond to the message ~S" 
             (objc-class-name class)
             (lisp-string-from-sel sel))
      m)))


;;; Returns T if the result spec requires a STRET for its return, NIL otherwise
;;; RSPEC may be either a number (in which case it is interpreted as a number
;;; of words) or a foreign type spec acceptable to PARSE-FOREIGN-TYPE. STRETS
;;; must be used when a structure larger than 4 bytes is returned

(defun requires-stret-p (rspec)
  (if (numberp rspec) 
    (> rspec 1)
    (> (ensure-foreign-type-bits (parse-foreign-type rspec)) 32)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Stret Convenience Stuff                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Allocate any temporary storage necessary to hold strets required AT TOPLEVEL
;;; in the value forms.  Special recognition is given to SENDs involving strets
;;; and to stret pseudo-functions NS-MAKE-POINT, NS-MAKE-RANGE, NS-MAKE-RECT and
;;; NS-MAKE-SIZE

(defmacro slet (varforms &body body &environment env)
  (multiple-value-bind (clean-body decls) (parse-body body env nil)
    (loop with r and s
          for (var val) in varforms
          do (multiple-value-setq (r s) (sletify val t var))
          collect r into rvarforms
          unless (null s) collect s into stretforms
          finally 
          (return
           `(rlet ,rvarforms
              ,@decls
              ,@stretforms
              ,@clean-body)))))


;;; Note that SLET* does not allow declarations 

(defmacro slet* (varforms &body body &environment env)
  (if (= (length varforms) 1)
      `(slet ,varforms ,@body)
    `(slet ,(list (first varforms))
       (slet* ,(rest varforms) ,@body))))


;;; Collect the info necessary to transform a SLET into an RLET 

(defun sletify (form &optional errorp (var (gensym)))
  (if (listp form)
    (case (first form)
      (ns-make-point 
       (assert (= (length form) 3))
       `(,var :<NSP>oint :x ,(second form) :y ,(third form)))
      (ns-make-rect 
       (assert (= (length form) 5))
       `(,var :<NSR>ect :origin.x ,(second form) :origin.y ,(third form)
               :size.width ,(fourth form) :size.height ,(fifth form)))
      (ns-make-range 
       (assert (= (length form) 3))
       `(,var :<NSR>ange :location ,(second form) :length ,(third form)))
      (ns-make-size
       (assert (= (length form) 3))
       `(,var :<NSS>ize :width ,(second form) :height ,(third form)))
      (send
       (let ((rtype (caar (message-type-signatures (parse-message (cddr form))))))
         (if (requires-stret-p rtype)
           (values `(,var ,rtype) `(send/stret ,var ,@(rest form)))
           (if errorp
             (error "NonSTRET SEND in ~S" form)
             form))))
      (send-super
       (let ((rtype (caar (message-type-signatures (parse-message (cddr form))))))
         (if (requires-stret-p rtype)
           (values `(,var ,rtype) `(send-super/stret ,var ,@(rest form)))
           (if errorp
             (error "NonSTRET SEND-SUPER in ~S" form)
             form))))
      (t (if errorp
           (error "Unrecognized STRET call in ~S" form)
           form)))
    (if errorp
      (error "Unrecognized STRET call in ~S" form)
      form)))


;;; Process the arguments to a message send as an implicit SLET, collecting
;;; the info necessary to build the corresponding RLET

(defun sletify-message-args (args)
  (loop with svf and sif
        for a in args
        do (multiple-value-setq (svf sif) (sletify a))
        unless (null sif) collect sif into sifs
        unless (equal svf a)
          do (setf a (first svf))
          and collect svf into svfs
        collect a into nargs
        finally (return (values nargs svfs sifs))))
  
  
;;; Convenience macros for some common Cocoa structures.  More
;;; could be added

(defmacro ns-max-range (r) 
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (+ (pref ,rtemp :<NSR>ange.location) (pref ,rtemp :<NSR>ange.length)))))
(defmacro ns-min-x (r) `(pref ,r :<NSR>ect.origin.x))
(defmacro ns-min-y (r) `(pref ,r :<NSR>ect.origin.y))
(defmacro ns-max-x (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (+ (pref ,r :<NSR>ect.origin.x) 
          (pref ,r :<NSR>ect.size.width)))))
(defmacro ns-max-y (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (+ (pref ,r :<NSR>ect.origin.y)
          (pref ,r :<NSR>ect.size.height)))))
(defmacro ns-mid-x (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (* 0.5 (+ (ns-min-x ,rtemp) (ns-max-x ,rtemp))))))
(defmacro ns-mid-y (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (* 0.5 (+ (ns-min-y ,rtemp) (ns-max-y ,rtemp))))))
(defmacro ns-height (r) `(pref ,r :<NSR>ect.size.height))
(defmacro ns-width (r) `(pref ,r :<NSR>ect.size.width))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Type Stuff                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A hash table from message names to lists of foreign type signature lists

(defvar *type-signature-table* (make-hash-table :test #'equal :size 6750))


;;; Add a new method to the table

(defun update-type-signatures-for-method (m)
  (let* ((sel (pref m :objc_method.method_name))
         (msg (lisp-string-from-sel sel)))
    (when (neq (schar msg 0) #\_)
      (pushnew 
       (compute-method-type-signature m)
       (gethash msg *type-signature-table*)
       :test #'equal))))


;;; Rescan all loaded modules for methods and update the type signature
;;; table accordingly

(defun update-type-signatures ()
  (note-all-library-methods
   #'(lambda (m c)
       (declare (ignore c))
       (ignore-errors
	 ;; Some libraries seem to have methods with bogus-looking
	 ;; type signatures
	 (update-type-signatures-for-method m)))))


;;; Return the type signature(s) associated with MSG

(defun message-type-signatures (msg)
  (values (gethash msg *type-signature-table*)))


;;; Compute the foreign type signature for method M 

(defun compute-method-type-signature (m)
  (cons
   (objc-foreign-arg-type (method-typestring m))
   (%stack-block ((type 4) (offset 4))
     (loop for i from 2 below (method-get-number-of-arguments m)
           do (#_method_getArgumentInfo m i type offset)
           collect 
           (objc-foreign-arg-type (%get-cstring (%get-ptr type)))))))


;;; Return the foreign type corresponding to the structure encoded in 
;;; TYPESTRING
;;; NOTE:  For some reason, :<NSD>ecimal shows up as {?=b8b4b1b1b18[8S]} 
;;;        and must be special-cased 

(defun extract-foreign-struct-name (typestring)
  (if (string= typestring "{?=b8b4b1b1b18[8S]}" 
               :end1 (min (length typestring) 19))
    :<NSD>ecimal
    (let ((=pos (position #\= typestring)))
      (when (null =pos)
        (error "Improperly formatted structure typestring: ~S" typestring))
      (escape-foreign-name 
       (subseq typestring (if (eql (schar typestring 1) #\_) 2 1) =pos)))))


;;; Return the foreign type spec corresponding to the ObjC type string STR 
        
(defun objc-foreign-arg-type (str)
    (case (schar str 0)
      (#\c :char)
      (#\C :unsigned-byte)
      (#\s :signed-halfword)
      (#\S :unsigned-halfword)
      (#\i :signed-fullword)
      (#\I :unsigned-fullword)
      (#\l :signed-fullword)
      (#\L :unsigned-fullword)
      (#\q :signed-doubleword)
      (#\Q :unsigned-doubleword)
      (#\f :single-float)
      (#\d :double-float)
      (#\v :void)
      (#\@ :id)
      (#\: :<sel>)
      (#\# '(:* (:struct :objc_class)))
      (#\* '(:* :char))
      (#\^ :address)
      (#\b (error "ObjC BITFIELD not yet supported"))
      (#\[ (error "OjbC ARRAY not yet supported"))
      (#\{ (extract-foreign-struct-name str))
      (#\( (error "ObjC UNION type not yet supported"))
      (#\? t)
      ((#\r #\R #\o #\O #\n #\N #\V) (objc-foreign-arg-type (subseq str 1)))
      (t (error "Unrecognized ObjC type string: ~S" str))))


;;; TRANSLATE-FOREIGN-ARG-TYPE doesn't accept :VOID

(defun translate-foreign-result-type (ftype)
  (ensure-foreign-type-bits (parse-foreign-type ftype))
  (if (eq ftype :void)
    :void
    (translate-foreign-arg-type ftype)))


;;; Convert a Lisp object X to a desired foreign type FTYPE 
;;; Currently only handles T/NIL => #$YES/#$NO and NIL => (%null-ptr)
;;; NOTE: Many conversions are done by %FF-CALL 

(defmacro coerce-to-address (x)
  (let ((x-temp (gensym)))
    `(let ((,x-temp ,x))
       (if (null ,x-temp) (%null-ptr) ,x-temp))))

(defmacro coerce-to-bool (x)
  (let ((x-temp (gensym)))
    `(let ((,x-temp ,x))
       (if (or (eq ,x-temp 0) (null ,x-temp)) #$NO #$YES))))
  
(defmacro coerce-to-foreign-type (x ftype)
  (cond ((and (constantp x) (constantp ftype)) 
         (case ftype
           (:id (if (null x) `(%null-ptr) (coerce-to-address x)))
           (:char (coerce-to-bool x))
           (t x)))
        ((constantp ftype) 
         (case ftype
           (:id `(coerce-to-address ,x))
           (:char `(coerce-to-bool ,x))
           (t x)))
        (t `(case ,(if (atom ftype) ftype)
              (:id (coerce-to-address ,x))
              (:char (coerce-to-bool ,x))
              (t ,x)))))


;;; Convert a foreign object X to T or NIL 

(defun coerce-from-bool (x)
  (cond
   ((eq x #$NO) nil)
   ((eq x #$YES) t)
   (t (error "Cannot coerce ~S to T or NIL" x))))


;;; Convert a set of ARGS with given foreign types to an argspec suitable 
;;; for %FF-CALL 

(defun convert-to-argspecs (argtypes result-ftype args evalargs)
  (flet ((foo (tftype) 
           (if (and (consp tftype) (eq (first tftype) :record))
             (/ (second tftype) 32)
             tftype)))
    (nconc
     (loop
       for a in args
       for ftype in argtypes
       do (ensure-foreign-type-bits (parse-foreign-type ftype))
       append (list (foo (translate-foreign-arg-type ftype))
                    (if evalargs
                      (coerce-to-foreign-type a ftype)
                      `(coerce-to-foreign-type ,a ,ftype))))
     (list (foo (translate-foreign-result-type result-ftype))))))
 

;;; Initialize the type signature table

(eval-when (:load-toplevel :execute)
  (with-autorelease-pool 
   (update-type-signatures)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 Support for variable arity messages                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A table to keep track of which messages allow variable numbers of args

(defvar *variable-arity-messages-table* (make-hash-table :test #'equal))

(defmacro define-variable-arity-message (msg)
  `(setf (gethash ,msg *variable-arity-messages-table*) t))

(defun variable-arity-message-p (msg)
  (gethash msg *variable-arity-messages-table*))


;;; Known variable arity messages

(define-variable-arity-message "appendFormat:")
(define-variable-arity-message "arrayWithObjects:")
(define-variable-arity-message "encodeValuesOfObjCTypes:")
(define-variable-arity-message "decodeValuesOfObjCTypes:")
(define-variable-arity-message "dictinaryWithObjectsAndKeys:")
(define-variable-arity-message 
  "handleFailureInFunction:object:file:lineNumber:description:")
(define-variable-arity-message 
  "handleFailureInMethod:object:file:lineNumber:description:")
(define-variable-arity-message "initWithFormat:")
(define-variable-arity-message "initWithObjects:")
(define-variable-arity-message "initWithObjectsAndKeys:")
(define-variable-arity-message "initWithFormat:locale:")
(define-variable-arity-message "localizedStringWithFormat:")
(define-variable-arity-message "raise:format:")
(define-variable-arity-message "setWithObjects:")
(define-variable-arity-message "stringByAppendingFormat:")
(define-variable-arity-message "stringWithFormat:")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Boolean Return Hackery                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Because Cocoa runtime type info encodes BOOL as CHAR, we can't tell which
;;; messages return BOOL (which should be converted to Lisp T or NIL)  and which
;;; truly return CHAR.  To temporarily deal with this problem, the bridge
;;; assumes that *all* messages returning CHAR are actually returning BOOL.
;;; The facility below allows one to define exceptions to this assumption.
;;; Eventually, the right way to deal with issues like this is probably to
;;; process the .h files for all type info rather than relying on Cocoa's
;;; runtime types.

(defvar *returns-boolean-exception-table* (make-hash-table :test #'equal))

(defmacro define-returns-boolean-exception (msg)
  `(setf (gethash ,msg *returns-boolean-exception-table*) t))

(defun returns-boolean-exception-p (msg)
  (gethash msg *returns-boolean-exception-table*))


;;; Known exceptions 

(define-returns-boolean-exception "charValue")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Invoking ObjC Methods                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check that the correct number of ARGs have been supplied to the given MSG

(defun check-message-arg-count (msg args)
  (unless (= (count #\: msg) (length args))
    (error "Incorrect number of arguments (~S) to ObjC message ~S" 
           (length args) msg)))


;;; Check that the correct number of ARGs have been supplied to a method 

(defun check-method-arg-count (m args)
  (unless (= (length args) (- (#_method_getNumberOfArguments m) 2))
    (error "Incorrect number of arguments (~S) to ObjC message ~S" 
           (length args) 
           (%get-cstring (#_sel_getName (pref m :objc_method.method_name))))))


;;; The SEND and SEND/STRET macros

(defmacro send (o msg &rest args &environment env)
  (make-optimized-send o msg args env))

(defmacro send/stret (s o msg &rest args &environment env)
  (make-optimized-send o msg args env s))


;;; Optimize special cases of SEND and SEND/STRET

(defun make-optimized-send (o msg args env &optional s super sclassname)
  ;; Try to determine the class of the receiver
  (let ((class (if sclassname 
                 (find-objc-class sclassname)
                 (get-objc-class-from-declaration (declared-type o env))))
	(vargs nil))
    ;; Get message and args
    (multiple-value-setq (msg args vargs) (parse-message (cons msg args)))
    ;; If the message cannot be determined, use a general send
    (unless (stringp msg)
      (return-from make-optimized-send
	(if (null super)
	    (if (null s) 
		`(%send ,o ,msg ,@args) 
	      `(%send/stret ,o ,msg ,@args))
	  (if (null s) 
	      `(%send-super ,msg ,@args) 
	    `(%send-super/stret ,s ,msg ,@args)))))
    ;; If a vararg exists, make sure that the message can accept it
    (when (and vargs (not (variable-arity-message-p msg)))
      (error "Message ~S cannot accept a variable number of arguments" msg))
    ;; Check the argument count
    (check-message-arg-count msg args)
    ;; Process message arguments inside an implicit SLET
    (multiple-value-bind (args svarforms sinitforms) (sletify-message-args args)
      ;; Analyze the object and message arguments to SEND
      (if class
        ;;***********************************************************************
        ;; If both the message and the class are known at compile-time, 
        ;; construct a direct call
        (let* ((m (get-method class (get-selector msg)))
               (mtsig (compute-method-type-signature m))
               (result-type (first mtsig))
               (argtypes (rest mtsig))
               (argspecs1 (convert-to-argspecs argtypes result-type args nil))
	       (argspecs (append (butlast argspecs1) vargs (last argspecs1))))
          (if (and (null super) (= (optimization-setting 'safety env) 3))
            ;; If SAFETY = 3, then check that the runtime method signature 
            ;; is the same as it was at compile-time
            (let ((otemp (gensym))
                  (ctemp (gensym))
                  (seltemp (gensym)))
              `(let* ((,otemp ,o)
                      (,ctemp (objc-class-of ,otemp))
                      (,seltemp (@selector ,msg)))
                 (get-method ,ctemp ,seltemp)
                 (if (string= (method-typestring (get-method ,ctemp ,seltemp))
                              ,(method-typestring m))
                   ,(build-call otemp seltemp msg argspecs svarforms sinitforms s)
                   (error "The type signature of ~S has changed since compile-time" 
                          ,msg)))))
          ;; Otherwise, we trust the declaration
          (build-call o `(@selector ,msg) msg argspecs svarforms sinitforms s super))
        ;; **********************************************************************
        ;; If only the message is known at compile-time, we can still build a 
        ;; direct call if the type signature is unique
        (let* ((mtsigs (message-type-signatures msg)))
          (cond 
           ((null mtsigs) (error "Unknown message: ~S" msg))
           ((null (rest mtsigs))
            ;; If MSG has a unique type signature at compile-time, build a
            ;; call for that signature
            (let* ((mtsig (first mtsigs))
                   (result-type (first mtsig))
                   (argtypes (rest mtsig))
                   (argspecs1 (convert-to-argspecs argtypes result-type args nil))
		   (argspecs (append (butlast argspecs1) vargs (last argspecs1))))
              (if (= (optimization-setting 'safety env) 3)
                ;; If SAFETY = 3, then check that the runtime method signature
                ;; is the same as it was at compile-time 
                (let ((otemp (gensym))
                      (ctemp (gensym))
                      (seltemp (gensym)))
                  `(let* ((,otemp ,o)
                          (,ctemp (objc-class-of ,otemp))
                          (,seltemp (@selector ,msg)))
                     (get-method ,ctemp ,seltemp)
                     (if (equal (compute-method-type-signature
                                 (get-method ,ctemp ,seltemp))
                                ',mtsig)
                       ,(build-call otemp seltemp msg argspecs svarforms sinitforms s)
                       (error "The type signature of ~S has changed since compile-time" 
                              ,msg))))
                ;; Otherwise, we assume that nothing changes
                (build-call o `(@selector ,msg) msg argspecs svarforms sinitforms s))))
           ;; If the type signature is not unique, build a general call for now
           (t (if (null super)
	    (if (null s) 
		`(%send ,o ,msg ,@args) 
	      `(%send/stret ,o ,msg ,@args))
	  (if (null s)
	      `(%send-super ,msg ,@args)
	    `(%send-super/stret ,s ,msg ,@args))))))))))


;;; WITH-NS-EXCEPTIONS-AS-ERRORS is only available in OpenMCL 0.14 and above

#-openmcl-native-threads
(defmacro with-ns-exceptions-as-errors (&body body)
  `(progn ,@body))


;;; Return a call to the method specified by SEL on object O, with the args
;;; specified by ARGSPECS.  This decides whether a normal or stret call is 
;;; needed and, if the latter, uses the memory S to hold the result. If SUPER
;;; is nonNIL, then this builds a send to super.  Finally, this also 
;;; coerces return #$YES/#$NO values to T/NIL. The entire call takes place 
;;; inside an implicit SLET.

(defun build-call (o sel msg argspecs svarforms sinitforms &optional s super)
  `(with-ns-exceptions-as-errors
     (rlet ,svarforms
       ,@sinitforms
       ,(let ((rspec (first (last argspecs))))
          (if (requires-stret-p rspec)
            (if (null s)
              ;; STRET required but not provided
              (error "The message ~S must be sent using SEND/STRET" msg)
              ;; STRET required and provided, use stret send
              (if (null super)
                ;; Regular stret send
                `(progn
                   (external-call
                    "_objc_msgSend_stret"
                    :address ,s :id ,o :<SEL> ,sel
                    ,@(append (butlast argspecs) (list :void)))
                   ,s)
                ;; Super stret send
                `(progn
                   (external-call
                    "_objc_msgSendSuper_stret"
                    :address ,s :address ,super :<SEL> ,sel
                    ,@(append (butlast argspecs) (list :void)))
                   ,s)))
            (if (null s)
              ;; STRET not required and not provided, use send
              (if (null super)
                ;; Regular send
                (if (and (eq rspec :signed-byte)
                         (not (returns-boolean-exception-p msg)))
                  `(coerce-from-bool
                    (external-call "_objc_msgSend" :id ,o :<SEL> ,sel ,@argspecs))
                  `(external-call "_objc_msgSend" :id ,o :<SEL> ,sel ,@argspecs))
                ;; Super send
                (if (and (eq rspec :signed-byte)
                         (not (returns-boolean-exception-p msg)))
                  `(coerce-from-bool
                    (external-call "_objc_msgSendSuper" 
                                   :address ,super :<SEL> ,sel ,@argspecs))
                  `(external-call "_objc_msgSendSuper" 
                                  :address ,super :<SEL> ,sel ,@argspecs)))
              ;; STRET not required but provided
              (error "The message ~S must be sent using SEND" msg)))))))


;;; The %SEND and %SEND/STRET functions for sending general messages 

(defmacro make-general-send (o msg args &optional s super sclassname)
  `(let ((vargs nil))
     (with-ns-exceptions-as-errors
      ;; Ensure that MSG is a string
      (multiple-value-setq (msg args vargs) (%parse-message (cons ,msg ,args)))
      (check-type ,msg string)
      ;; If a vararg exists, make sure that the message can accept it
      (when (and vargs (not (variable-arity-message-p msg)))
	(error "Message ~S cannot accept a variable number of arguments" msg))
      ;; Lookup method
      (let* ((class ,(if sclassname 
			 `(find-objc-class ,sclassname)  
		       `(objc-class-of ,o)))
	     (sel (get-selector ,msg))
	     (m (get-method class sel)))
	;; Check arg count
	(check-method-arg-count m ,args)
	;; Get method type signature
	(let* ((mtsig (compute-method-type-signature m))
	       (argtypes (rest mtsig))
	       (result-type (first mtsig))
	       (argspecs1 (convert-to-argspecs argtypes result-type ,args t))
	       (argspecs (append (butlast argspecs1) vargs (last argspecs1)))
	       (result-spec (first (last argspecs))))
	  ;; Call method
	  (if (requires-stret-p result-spec)
	      ,(if (null s)
		   ;; STRET required but not provided
		   `(error "The message ~S must be sent using SEND/STRET" ,msg)
		 ;; STRET required and provided
		 (if (null super)
		     ;; Regular stret send, invoke objc_msgSend_stret 
		     `(progn
			(apply #'%ff-call
			       (%reference-external-entry-point 
				(load-time-value 
				 (external "_objc_msgSend_stret")))
			       :address ,s
			       :address ,o
			       :address sel
			       (progn (setf (car (last argspecs)) :void) argspecs))
			,s)
		   ;; Stret send to super, invoke objc_msgSendSuper_stret
		   `(progn 
		      (apply #'%ff-call
			     (%reference-external-entry-point 
			      (load-time-value 
			       (external "_objc_msgSendSuper_stret")))
			     :address ,s
			     :address ,super
			     :address sel
			     (progn (setf (car (last argspecs)) :void) argspecs)))))
	    ,(if (null s)
		 ;; STRET not required and not provided
		 (if (null super)
		     ;; Regular send, invoke objc_msgSend
		     `(let ((r (apply #'%ff-call
				      (%reference-external-entry-point 
				       (load-time-value 
					(external "_objc_msgSend")))
				      :address ,o
				      :address sel
				      argspecs)))
			(if (and (eq result-type :char)
				 (not (returns-boolean-exception-p msg)))
			    (coerce-from-bool r)
			  r))
		  ;;; Send to super, invoke objc_msgSendSuper
		   `(let ((r (apply #'%ff-call
				    (%reference-external-entry-point 
				     (load-time-value 
				      (external "_objc_msgSendSuper")))
				    :address ,super
				    :address sel
				    argspecs)))
		      (if (and (eq result-type :char)
			       (not (returns-boolean-exception-p msg)))
			  (coerce-from-bool r)
			r)))
	       ;; STRET not required but provided
	       `(error "The message ~S must be sent using SEND" msg))))))))

(defun %send (o msg &rest args)
  (declare (optimize (speed 3)) (dynamic-extent args))
  (make-general-send o msg args))
  
(defun %send/stret (s o msg &rest args)
  (declare (optimize (speed 3)) (dynamic-extent args))
  (make-general-send o msg args s))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Instantiating ObjC Class                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A MAKE-INSTANCE like interface to ObjC object creation

(defun make-objc-instance (cname &rest initargs)
  (declare (dynamic-extent initargs))
  (multiple-value-bind (ks vs) (keys-and-vals initargs)
    (declare (dynamic-extent ks vs))
    (when (not (stringp cname))
      (setf cname (lisp-to-objc-classname cname)))
    (apply #'%send 
           (send (find-objc-class cname) 'alloc)
           (lisp-to-objc-init ks)
           vs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Defining CLOS Subclasses of ObjC Classes                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  Defining CLOS Methods on ObjC Classes                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Provide the BRIDGE module

(provide "BRIDGE")
