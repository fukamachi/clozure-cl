;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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


;; This is a hacked-up version of the CMU CL type system.

(eval-when (:load-toplevel)
  (require "SEQUENCES"))


;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
;;;
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier)))

(defun parse-lambda-list (list)
  (let* ((required)
         (optional)
         (keys)
         (aux))
    (let ((restp nil)
          (rest nil)
          (keyp nil)
          (allowp nil)
          (state :required))
      (dolist (arg list)
        (if (and (symbolp arg)
                 (let ((name (symbol-name arg)))
                   (and (/= (length name) 0)
                        (char= (char name 0) #\&))))
          (case arg
            (&optional
             (unless (eq state :required)
               (error "Misplaced &optional in lambda-list: ~S." list))
             (setq state '&optional))
            (&rest
             (unless (member state '(:required &optional))
               (error "Misplaced &rest in lambda-list: ~S." list))
             (setq state '&rest))
            (&key
             (unless (member state '(:required &optional :post-rest
                                     ))
               (error "Misplaced &key in lambda-list: ~S." list))
             (setq keyp t)
             (setq state '&key))
            (&allow-other-keys
             (unless (eq state '&key)
               (error "Misplaced &allow-other-keys in lambda-list: ~S." list))
             (setq allowp t  state '&allow-other-keys))
            (&aux
             (when (member state '(&rest))
               (error "Misplaced &aux in lambda-list: ~S." list))
             (setq state '&aux))
            (t
             (error "Unknown &keyword in lambda-list: ~S." arg)))
          (case state
            (:required (push arg required))
            (&optional (push arg optional))
            (&rest
             (setq restp t  rest arg  state :post-rest))
            (&key (push arg keys))
            (&aux (push arg aux))
            (t
             (error "Found garbage in lambda-list when expecting a keyword: ~S." arg)))))
      
      (values (nreverse required) (nreverse optional) restp rest keyp (nreverse keys) allowp (nreverse aux)))))

(defvar %deftype-expanders% (make-hash-table :test #'eq))
(defvar *type-translators* (make-hash-table :test #'eq))
(defvar *builtin-type-info* (make-hash-table :test #'equal))
(defvar %builtin-type-cells% (make-hash-table :test 'equal))

(defvar *use-implementation-types* t)

(defun info-type-builtin (name)
  (gethash name *builtin-type-info*))

(defun (setf info-type-builtin) (val name)
  (setf (gethash name *builtin-type-info*) val))

(defun info-type-translator (name)
  (gethash name *type-translators*))




; Allow bootstrapping: mostly, allow us to bootstrap the type system
; by having DEFTYPE expanders defined on built-in classes (the user
; shouldn't be allowed to do so, at least not easily.

;(defvar *type-system-initialized* nil)

(defun %deftype (name fn doc)
  (cond ((null fn)
         (remhash name %deftype-expanders%))
        ((and *type-system-initialized*
              (or (built-in-type-p name) (find-class name nil)))
         (error "Cannot redefine type ~S" name))
        (t (setf (gethash name %deftype-expanders%) fn)
           (record-source-file name 'type)))
  (set-documentation name 'type doc)   ; nil clears it.
  name)

(defun %define-type-translator (name fn doc)
  (declare (ignore doc))
  (setf (gethash name *type-translators*) fn)
  name)

;(defun %deftype-expander (name)
;  (or (gethash name %deftype-expanders%)
;      (and *compiling-file* (%cdr (assq name *compile-time-deftype-expanders*)))))
(defun %deftype-expander (name)
  (gethash name %deftype-expanders%))

(defun process-deftype-arglist (arglist &aux (in-optional? nil))
  "Returns a NEW list similar to arglist except
    inserts * as the default default for &optional args."
  (mapcar #'(lambda (item)
              (cond ((eq item '&optional) (setq in-optional? t) item)
                    ((memq item lambda-list-keywords) (setq in-optional? nil) item)
                    ((and in-optional? (symbolp item)) (list item ''*))
                    (t item)))
          arglist))

(defun expand-type-macro (definer name arglist body env)
  (setq name (require-type name 'symbol))
  (multiple-value-bind (lambda doc)
      (parse-macro-internal name arglist body env '*)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (,definer ',name
                   (nfunction ,name ,lambda)
                   ,doc))))

(defmacro deftype (name arglist &body body &environment env)
  "Syntax like DEFMACRO, but defines a new type."
  (expand-type-macro '%deftype name arglist body env))

(defmacro def-type-translator (name arglist &body body &environment env)
  (expand-type-macro '%define-type-translator name arglist body env))


(defun type-expand (form &optional env &aux def)
  (while (setq def (cond ((symbolp form)
                          (gethash form %deftype-expanders%))
                         ((and (consp form) (symbolp (%car form)))
                          (gethash (%car form) %deftype-expanders%))
                         (t nil)))
    (setq form (funcall def (if (consp form) form (list form)) env)))
  form)

(defmethod print-object ((tc type-class) stream)
  (print-unreadable-object (tc stream :type t :identity t)
    (format stream "~s" (type-class-name tc))))

(defmethod print-object ((c ctype) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~S" (type-specifier c))))

(defmethod make-load-form ((c ctype) &optional env)
  (declare (ignore env))
  `(specifier-type ',(type-specifier c)))


(defun make-key-info (&key name type)
  (%istruct 'key-info name type))

(defun type-class-or-lose (name)
  (or (cdr (assq name *type-classes*))
      (error "~S is not a defined type class." name)))

(eval-when (:compile-toplevel :execute)

(defconstant type-class-function-slots
  '((:simple-subtypep . #.type-class-simple-subtypep)
    (:complex-subtypep-arg1 . #.type-class-complex-subtypep-arg1)
    (:complex-subtypep-arg2 . #.type-class-complex-subtypep-arg2)
    (:simple-union . #.type-class-simple-union)
    (:complex-union . #.type-class-complex-union)
    (:simple-intersection . #.type-class-simple-intersection)
    (:complex-intersection . #.type-class-complex-intersection)
    (:simple-= . #.type-class-simple-=)
    (:complex-= . #.type-class-complex-=)
    (:unparse . #.type-class-unparse)))

)

(defun class-typep (form class)
  (memq class (%inited-class-cpl (class-of form))))

;;; CLASS-FUNCTION-SLOT-OR-LOSE  --  Interface
;;;
(defun class-function-slot-or-lose (name)
  (or (cdr (assoc name type-class-function-slots))
      (error "~S is not a defined type class method." name)))


(eval-when (:compile-toplevel :execute)

;;; INVOKE-TYPE-METHOD  --  Interface
;;;
;;;    Invoke a type method on TYPE1 and TYPE2.  If the two types have the same
;;; class, invoke the simple method.  Otherwise, invoke any complex method.  If
;;; there isn't a distinct complex-arg1 method, then swap the arguments when
;;; calling type1's method.  If no applicable method, return DEFAULT.
;;;

(defmacro invoke-type-method (simple complex-arg2 type1 type2 &key
                                     (default '(values nil t))
                                     complex-arg1)
  (let ((simple (class-function-slot-or-lose simple))
        (cslot1 (class-function-slot-or-lose (or complex-arg1 complex-arg2)))
        (cslot2 (class-function-slot-or-lose complex-arg2)))
    (once-only ((n-type1 type1)
                (n-type2 type2))
      (once-only ((class1 `(ctype-class-info ,n-type1))
                  (class2 `(ctype-class-info ,n-type2)))
        `(if (eq ,class1 ,class2)
           (funcall (%svref ,class1 ,simple) ,n-type1 ,n-type2)
           ,(once-only ((complex1 `(%svref ,class1 ,cslot1))
                        (complex2 `(%svref ,class2 ,cslot2)))
              `(cond (,complex2 (funcall ,complex2 ,n-type1 ,n-type2))
                     (,complex1
                      ,(if complex-arg1
                         `(funcall ,complex1 ,n-type1 ,n-type2)
                         `(funcall ,complex1 ,n-type2 ,n-type1)))
                     (t ,default))))))))


;;;; Utilities:

;;; ANY-TYPE-OP, EVERY-TYPE-OP  --  Interface
;;;
;;;    Like ANY and EVERY, except that we handle two-arg uncertain predicates.
;;; If the result is uncertain, then we return Default from the block PUNT.
;;; If LIST-FIRST is true, then the list element is the first arg, otherwise
;;; the second.
;;;
(defmacro any-type-op (op thing list &key (default '(values nil nil))
			        list-first)
  (let ((n-this (gensym))
	  (n-thing (gensym))
	  (n-val (gensym))
	  (n-win (gensym))
	  (n-uncertain (gensym)))
    `(let ((,n-thing ,thing)
	     (,n-uncertain nil))
       (dolist (,n-this ,list
			      (if ,n-uncertain
			        (return-from PUNT ,default)
			        nil))
	   (multiple-value-bind (,n-val ,n-win)
			            ,(if list-first
				         `(,op ,n-this ,n-thing)
				         `(,op ,n-thing ,n-this))
	     (unless ,n-win (setq ,n-uncertain t))
	     (when ,n-val (return t)))))))
;;;
(defmacro every-type-op (op thing list &key (default '(values nil nil))
			          list-first)
  (let ((n-this (gensym))
	  (n-thing (gensym))
	  (n-val (gensym))
	  (n-win (gensym)))
    `(let ((,n-thing ,thing))
       (dolist (,n-this ,list t)
	   (multiple-value-bind (,n-val ,n-win)
			            ,(if list-first
				         `(,op ,n-this ,n-thing)
				         `(,op ,n-thing ,n-this))
	     (unless ,n-win (return-from PUNT ,default))
	     (unless ,n-val (return nil)))))))

)

  
;;; VANILLA-INTERSECTION  --  Interface
;;;
;;;    Compute the intersection for types that intersect only when one is a
;;; hierarchical subtype of the other.
;;;
(defun vanilla-intersection (type1 type2)
  (multiple-value-bind (stp1 win1)
		           (csubtypep type1 type2)
    (multiple-value-bind (stp2 win2)
			       (csubtypep type2 type1)
      (cond (stp1 (values type1 t))
	      (stp2 (values type2 t))
	      ((and win1 win2) (values *empty-type* t))
	      (t
	       (values type1 nil))))))


;;; VANILLA-UNION  --  Interface
;;;
(defun vanilla-union (type1 type2)
  (cond ((csubtypep type1 type2) type2)
	  ((csubtypep type2 type1) type1)
	  (t nil)))


;;; DELEGATE-COMPLEX-{SUBTYPEP-ARG2,INTERSECTION}  --  Interface
;;;
;;;    These functions are used as method for types which need a complex
;;; subtypep method to handle some superclasses, but cover a subtree of the
;;; type graph (i.e. there is no simple way for any other type class to be a
;;; subtype.)  There are always still complex ways, namely UNION and MEMBER
;;; types, so we must give TYPE1's method a chance to run, instead of
;;; immediately returning NIL, T.
;;;
(defun delegate-complex-subtypep-arg2 (type1 type2)
  (let ((subtypep-arg1
	 (type-class-complex-subtypep-arg1
	  (ctype-class-info type1))))
    (if subtypep-arg1
	(funcall subtypep-arg1 type1 type2)
	(values nil t))))
;;;
(defun delegate-complex-intersection (type1 type2)
  (let ((method (type-class-complex-intersection (ctype-class-info type1))))
    (if (and method (not (eq method #'delegate-complex-intersection)))
	(funcall method type2 type1)
	(vanilla-intersection type1 type2))))

;;; HAS-SUPERCLASSES-COMPLEX-SUBTYPEP-ARG1  --  Internal
;;;
;;;    Used by DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1 method.  Info is
;;; a list of conses (SUPERCLASS-CLASS . {GUARD-TYPE-SPECIFIER | NIL}).  Will
;;; never be called with a hairy type as type2, since the hairy type type2
;;; method gets first crack.
;;;
#|
(defun has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  (values
   (and (typep type2 'class)
	(dolist (x info nil)
	  (when (or (not (cdr x))
		    (csubtypep type1 (specifier-type (cdr x))))
	    (return
	     (or (eq type2 (car x))
		 (let ((inherits (layout-inherits (class-layout (car x)))))
		   (dotimes (i (length inherits) nil)
		     (when (eq type2 (layout-class (svref inherits i)))
		       (return t)))))))))
   t))
|#

(eval-when (:compile-toplevel :execute)
;;; DEFINE-SUPERCLASSES  --  Interface
;;;
;;;    Takes a list of specs of the form (superclass &optional guard).
;;; Consider one spec (with no guard): any instance of type-class is also a
;;; subtype of SUPERCLASS and of any of its superclasses.  If there are
;;; multiple specs, then some will have guards.  We choose the first spec whose
;;; guard is a supertype of TYPE1 and use its superclass.  In effect, a
;;; sequence of guards G0, G1, G2 is actually G0, (and G1 (not G0)),
;;; (and G2 (not (or G0 G1))).
;;;
#|
(defmacro define-superclasses (type-class &rest specs)
  (let ((info
	 (mapcar #'(lambda (spec)
		     (destructuring-bind (super &optional guard)
					 spec
		       (cons (find-class super) guard)))
		 specs)))
    `(progn
      (setf (type-class-complex-subtypep-arg1
	     (type-class-or-lose ',type-class))
	    #'(lambda (type1 type2)
		(has-superclasses-complex-subtypep-arg1 type1 type2 ',info)))
       
       (setf (type-class-complex-subtypep-arg2
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-subtypep-arg2)
       
       (setf (type-class-complex-intersection
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-intersection))))
|#

); eval-when (compile eval)

(eval-when (:compile-toplevel :execute)

(defmacro define-type-method ((class method &rest more-methods)
			            lambda-list &body body)
  `(progn
     (let* ((fn #'(lambda ,lambda-list ,@body)))
       ,@(mapcar #'(lambda (method)
		         `(setf (%svref
			           (type-class-or-lose ',class)
                             ,(class-function-slot-or-lose method))
			          fn))
		     (cons method more-methods)))
     nil))

)


(defun ctype-p (x)
  (and (eql (typecode x) ppc32::subtag-istruct)
       (memq (%svref x 0) 
             '#.(cons 'ctype 
                      (cons 'unknown-ctype                             
                            (append (mapcar #'class-name 
                                            (class-direct-subclasses (find-class 'args-ctype)))
                                    (mapcar #'class-name 
                                            (class-direct-subclasses (find-class 'ctype)))))))))


(setf (type-predicate 'ctype) 'ctype-p)


;;;; Function and Values types.
;;;
;;;    Pretty much all of the general type operations are illegal on VALUES
;;; types, since we can't discriminate using them, do SUBTYPEP, etc.  FUNCTION
;;; types are acceptable to the normal type operations, but are generally
;;; considered to be equivalent to FUNCTION.  These really aren't true types in
;;; any type theoretic sense, but we still parse them into CTYPE structures for
;;; two reasons:
;;; -- Parsing and unparsing work the same way, and indeed we can't tell
;;;    whether a type is a function or values type without parsing it.
;;; -- Many of the places that can be annotated with real types can also be
;;;    annotated function or values types.

;; Methods on the VALUES type class.

(defun make-values-ctype (&key
                          required
                          optional
                          rest
                          keyp
                          keywords
                          allowp)
  (%istruct 'values-ctype
            (type-class-or-lose 'values)
            nil
            required
            optional
            rest
            keyp
            keywords
            allowp
           ))

(defun values-ctype-p (x) (istruct-typep x 'values-ctype))
(setf (type-predicate 'values-ctype) 'values-ctype-p)


(define-type-method (values :simple-subtypep :complex-subtypep-arg1)
		    (type1 type2)
  (declare (ignore type2))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type1)))

(define-type-method (values :complex-subtypep-arg2)
		    (type1 type2)
  (declare (ignore type1))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type2)))


(define-type-method (values :unparse) (type)
  (cons 'values (unparse-args-types type)))


;;; TYPE=-LIST  --  Internal
;;;
;;;    Return true if List1 and List2 have the same elements in the same
;;; positions according to TYPE=.  We return NIL, NIL if there is an uncertain
;;; comparison. 
;;;
(defun type=-list (list1 list2)
  (declare (list list1 list2))
  (do ((types1 list1 (cdr types1))
       (types2 list2 (cdr types2)))
      ((or (null types1) (null types2))
       (if (or types1 types2)
	   (values nil t)
	   (values t t)))
    (multiple-value-bind (val win)
			       (type= (first types1) (first types2))
      (unless win
	  (return (values nil nil)))
      (unless val
	  (return (values nil t))))))

(define-type-method (values :simple-=) (type1 type2)
  (let ((rest1 (args-ctype-rest type1))
	  (rest2 (args-ctype-rest type2)))
    (cond ((or (args-ctype-keyp type1) (args-ctype-keyp type2)
	         (args-ctype-allowp type1) (args-ctype-allowp type2))
	     (values nil nil))
	    ((and rest1 rest2 (type/= rest1 rest2))
	     (type= rest1 rest2))
	    ((or rest1 rest2)
	     (values nil t))
	    (t
	     (multiple-value-bind (req-val req-win)
				        (type=-list (values-ctype-required type1)
					              (values-ctype-required type2))
	       (multiple-value-bind (opt-val opt-win)
				          (type=-list (values-ctype-optional type1)
					                (values-ctype-optional type2))
	         (values (and req-val opt-val) (and req-win opt-win))))))))


;; Methods on the FUNCTION type class.


(defun make-function-ctype (&key
                            required
                            optional
                            rest
                            keyp
                            keywords
                            allowp
                            wild-args
                            returns)
  (%istruct 'function-ctype
            (type-class-or-lose 'function)
            nil
            required
            optional
            rest
            keyp
            keywords
            allowp
            wild-args
            returns
           ))

(defun function-ctype-p (x) (istruct-typep x 'function-ctype))
(setf (type-predicate 'function-ctype) 'function-ctype-p)

;;; A flag that we can bind to cause complex function types to be unparsed as
;;; FUNCTION.  Useful when we want a type that we can pass to TYPEP.
;;;
(defvar *unparse-function-type-simplify* nil)

(define-type-method (function :unparse) (type)
  (if *unparse-function-type-simplify*
    'function
    (list 'function
	    (if (function-ctype-wild-args type)
		'*
		(unparse-args-types type))
	    (type-specifier
	     (function-ctype-returns type)))))

;;; Since all function types are equivalent to FUNCTION, they are all subtypes
;;; of each other.
;;;
(define-type-method (function :simple-subtypep) (type1 type2)
  (declare (ignore type1 type2))
  (values t t))

                   
;(define-superclasses function (function))       


;;; The union or intersection of two FUNCTION types is FUNCTION.
;;;
(define-type-method (function :simple-union) (type1 type2)
  (declare (ignore type1 type2))
  (specifier-type 'function))
;;;
(define-type-method (function :simple-intersection) (type1 type2)
  (declare (ignore type1 type2))
  (values (specifier-type 'function) t))


;;; ### Not very real, but good enough for redefining transforms according to
;;; type:
;;;
(define-type-method (function :simple-=) (type1 type2)
  (values (equalp type1 type2) t))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARGUMENT "type
;;; specifier", which is only meaningful in function argument type specifiers
;;; used within the compiler.
;;;

(defun clone-type-class-methods (src-tc dest-tc)
  (do* ((n (uvsize src-tc))
        (i 2 (1+ i)))
       ((= i n) dest-tc)
    (declare (fixnum i n))
    (setf (%svref dest-tc i)
          (%svref src-tc i))))

(clone-type-class-methods (type-class-or-lose 'values) (type-class-or-lose 'constant))

(defun make-constant-ctype (&key type)
  (%istruct 'constant-ctype
            (type-class-or-lose 'constant)
            nil
            type))

(defun constant-ctype-p (x) (istruct-typep x 'constant-ctype))
(setf (type-predicate 'constant-ctype) 'constant-ctype-p)

(define-type-method (constant :unparse) (type)
  `(constant-argument ,(type-specifier (constant-ctype-type type))))

(define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-ctype-type type1) (constant-ctype-type type2)))

(def-type-translator constant-argument (type)
  (make-constant-ctype :type (specifier-type type)))


;;; Parse-Args-Types  --  Internal
;;;
;;;    Given a lambda-list like values type specification and a Args-Type
;;; structure, fill in the slots in the structure accordingly.  This is used
;;; for both FUNCTION and VALUES types.
;;;

(defun parse-args-types (lambda-list result)
  (multiple-value-bind (required optional restp rest keyp keys allowp aux)
		           (parse-lambda-list lambda-list)
    (when aux
      (error "&Aux in a FUNCTION or VALUES type: ~S." lambda-list))
    (setf (args-ctype-required result) (mapcar #'specifier-type required))
    (setf (args-ctype-optional result) (mapcar #'specifier-type optional))
    (setf (args-ctype-rest result) (if restp (specifier-type rest) nil))
    (setf (args-ctype-keyp result) keyp)
    (let* ((key-info ()))
      (dolist (key keys)
	  (when (or (atom key) (/= (length key) 2))
	    (error "Keyword type description is not a two-list: ~S." key))
	  (let ((kwd (first key)))
	    (when (member kwd key-info :test #'eq :key #'(lambda (x) (key-info-name x)))
	      (error "Repeated keyword ~S in lambda list: ~S." kwd lambda-list))
	    (push (make-key-info :name kwd
                               :type (specifier-type (second key))) key-info)))
      (setf (args-ctype-keywords result) (nreverse key-info)))
    (setf (args-ctype-allowp result) allowp)))

;;; Unparse-Args-Types  --  Internal
;;;
;;;    Return the lambda-list like type specification corresponding
;;; to a Args-Type.
;;;
(defun unparse-args-types (type)
  (let* ((result ()))

    (dolist (arg (args-ctype-required type))
      (push (type-specifier arg) result))

    (when (args-ctype-optional type)
      (push '&optional result)
      (dolist (arg (args-ctype-optional type))
	  (push (type-specifier arg) result)))

    (when (args-ctype-rest type)
      (push '&rest result)
      (push (type-specifier (args-ctype-rest type)) result))

    (when (args-ctype-keyp type)
      (push '&key result)
      (dolist (key (args-ctype-keywords type))
	  (push (list (key-info-name key)
                    (type-specifier (key-info-type key))) result)))

    (when (args-ctype-allowp type)
      (push '&allow-other-keys result))

    (nreverse result)))

(def-type-translator function (&optional args result)
  (let ((res (make-function-ctype
	        :returns (values-specifier-type result))))
    (if (eq args '*)
	(setf (function-ctype-wild-args res) t)
	(parse-args-types args res))
    res))

(def-type-translator values (&rest values)
  (let ((res (make-values-ctype)))
    (parse-args-types values res)
    res))

;;; Single-Value-Type  --  Interface
;;;
;;;    Return the type of the first value indicated by Type.  This is used by
;;; people who don't want to have to deal with values types.
;;;
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((values-ctype-p type)
	 (or (car (args-ctype-required type))
	     (car (args-ctype-optional type))
	     (args-ctype-rest type)
	     *universal-type*))
	((eq type *wild-type*)
	 *universal-type*)
	(t
	 type)))


;;; FUNCTION-TYPE-NARGS  --  Interface
;;;
;;;    Return the minmum number of arguments that a function can be called
;;; with, and the maximum number or NIL.  If not a function type, return
;;; NIL, NIL.
;;;
(defun function-type-nargs (type)
  (declare (type ctype type))
  (if (function-ctype-p type)
    (let ((fixed (length (args-ctype-required type))))
	(if (or (args-ctype-rest type)
		  (args-ctype-keyp type)
		  (args-ctype-allowp type))
        (values fixed nil)
        (values fixed (+ fixed (length (args-ctype-optional type))))))
    (values nil nil)))

;;; cons-ctype
(defun wild-ctype-to-universal-ctype (c)
  (if (type= c *wild-type*)
    *universal-type*
    c))

(defun make-cons-ctype (car-ctype-value cdr-ctype-value)
  (%istruct 'cons-ctype
            (type-class-or-lose 'cons)
            nil
            (wild-ctype-to-universal-ctype car-ctype-value)
            (wild-ctype-to-universal-ctype cdr-ctype-value)))

(def-type-translator cons (&optional (car-type-spec '*) (cdr-type-spec '*))
  (make-cons-ctype (specifier-type car-type-spec)
                   (specifier-type cdr-type-spec)))

(define-type-method (cons :unparse) (type)
  (let* ((car-spec (type-specifier (cons-ctype-car-ctype type)))
         (cdr-spec (type-specifier (cons-ctype-cdr-ctype type))))
    (if (and (member car-spec '(t *))
             (member cdr-spec '(t *)))
      'cons
      `(cons ,car-spec ,cdr-spec))))

(define-type-method (cons :simple-=) (type1 type2)
  (declare (cons-ctype type1 type2))
  (and (type= (cons-ctype-car-ctype type1) (cons-ctype-car-ctype type2))
       (type= (cons-ctype-cdr-ctype type1) (cons-ctype-cdr-ctype type2))))

(define-type-method (cons :simple-subtypep) (type1 type2)
  (declare (cons-ctype type1 type2))
  (multiple-value-bind (car-is-subtype car-definitely)
      (csubtypep (cons-ctype-car-ctype type1) (cons-ctype-car-ctype type2))
    (multiple-value-bind (cdr-is-subtype cdr-definitely)
        (csubtypep (cons-ctype-cdr-ctype type1) (cons-ctype-cdr-ctype type2))
      (if (and car-is-subtype cdr-is-subtype)
        (values t t)
        (values nil (or cdr-definitely car-definitely))))))

;;; Values-Types  --  Interface
;;;
;;;    Determine if Type corresponds to a definite number of values.  The first
;;; value is a list of the types for each value, and the second value is the
;;; number of values.  If the number of values is not fixed, then return NIL
;;; and :Unknown.
;;;
(defun values-types (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
	   (values nil :unknown))
	  ((not (values-ctype-p type))
	   (values (list type) 1))
	  ((or (args-ctype-optional type)
	       (args-ctype-rest type)
	       (args-ctype-keyp type)
	       (args-ctype-allowp type))
	   (values nil :unknown))
	  (t
	   (let ((req (args-ctype-required type)))
	     (values (mapcar #'single-value-type req) (length req))))))


;;; Values-Type-Types  --  Internal
;;;
;;;    Return two values:
;;; 1] A list of all the positional (fixed and optional) types.
;;; 2] The rest type (if any).  If keywords allowed, *universal-type*.  If no
;;;    keywords or rest, *empty-type*.
;;;
(defun values-type-types (type)
  (declare (type values-type type))
  (values (append (args-ctype-required type)
		      (args-ctype-optional type))
	    (cond ((args-ctype-keyp type) *universal-type*)
		    ((args-ctype-rest type))
		    (t
		     *empty-type*))))


;;; Fixed-Values-Op  --  Internal
;;;
;;;    Return a list of Operation applied to the types in Types1 and Types2,
;;; padding with Rest2 as needed.  Types1 must not be shorter than Types2.  The
;;; second value is T if Operation always returned a true second value.
;;;
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar #'(lambda (t1 t2)
			      (multiple-value-bind (res win)
					               (funcall operation t1 t2)
			        (unless win (setq exact nil))
			        res))
		        types1
		        (append types2
			          (make-list (- (length types1) (length types2))
				               :initial-element rest2)))
	      exact)))

;;; Coerce-To-Values  --  Internal
;;;
;;; If Type isn't a values type, then make it into one:
;;;    <type>  ==>  (values type &rest t)
;;;
(defun coerce-to-values (type)
  (declare (type ctype type))
  (if (values-ctype-p type)
    type
    (make-values-ctype :required (list type) :rest *universal-type*)))


;;; Args-Type-Op  --  Internal
;;;
;;;    Do the specified Operation on Type1 and Type2, which may be any type,
;;; including Values types.  With values types such as:
;;;    (values a0 a1)
;;;    (values b0 b1)
;;;
;;; We compute the more useful result:
;;;    (values (<operation> a0 b0) (<operation> a1 b1))
;;;
;;; Rather than the precise result:
;;;    (<operation> (values a0 a1) (values b0 b1))
;;;
;;; This has the virtue of always keeping the values type specifier outermost,
;;; and retains all of the information that is really useful for static type
;;; analysis.  We want to know what is always true of each value independently.
;;; It is worthless to know that IF the first value is B0 then the second will
;;; be B1.
;;;
;;; If the values count signatures differ, then we produce result with the
;;; required value count chosen by Nreq when applied to the number of required
;;; values in type1 and type2.  Any &key values become &rest T (anyone who uses
;;; keyword values deserves to lose.)
;;;
;;; The second value is true if the result is definitely empty or if Operation
;;; returned true as its second value each time we called it.  Since we
;;; approximate the intersection of values types, the second value being true
;;; doesn't mean the result is exact.
;;;
(defun args-type-op (type1 type2 operation nreq)
  (declare (type ctype type1 type2) (type function operation nreq))
  (if (or (values-ctype-p type1) (values-ctype-p type2))
    (let ((type1 (coerce-to-values type1))
	    (type2 (coerce-to-values type2)))
	(multiple-value-bind (types1 rest1)
			         (values-type-types type1)
	  (multiple-value-bind (types2 rest2)
			           (values-type-types type2)
	    (multiple-value-bind (rest rest-exact)
				       (funcall operation rest1 rest2)
	      (multiple-value-bind
		  (res res-exact)
		  (if (< (length types1) (length types2))
                (fixed-values-op types2 types1 rest1 operation)
                (fixed-values-op types1 types2 rest2 operation))
		  (let* ((req (funcall nreq
				           (length (args-ctype-required type1))
				           (length (args-ctype-required type2))))
		         (required (subseq res 0 req))
		         (opt (subseq res req))
		         (opt-last (position rest opt :test-not #'type=
					           :from-end t)))
		    (if (find *empty-type* required :test #'type=)
		      (values *empty-type* t)
		      (values (make-values-ctype
			         :required required
			         :optional (if opt-last
					         (subseq opt 0 (1+ opt-last))
					         ())
			         :rest (if (eq rest *empty-type*) nil rest))
			        (and rest-exact res-exact)))))))))
    (funcall operation type1 type2)))

;;; Values-Type-Union, Values-Type-Intersection  --  Interface
;;;
;;;    Do a union or intersection operation on types that might be values
;;; types.  The result is optimized for utility rather than exactness, but it
;;; is guaranteed that it will be no smaller (more restrictive) than the
;;; precise result.
;;;

(defun values-type-union (type1 type2)
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 *wild-type*) (eq type2 *wild-type*)) *wild-type*)
	  ((eq type1 *empty-type*) type2)
	  ((eq type2 *empty-type*) type1)
	  (t
	   (values (args-type-op type1 type2 #'type-union #'min)))))

(defun values-type-intersection (type1 type2)
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*) (values type2 t))
	((eq type2 *wild-type*) (values type1 t))
	(t
	 (args-type-op type1 type2 #'type-intersection #'max))))


;;; Values-Types-Intersect  --  Interface
;;;
;;;    Like Types-Intersect, except that it sort of works on values types.
;;; Note that due to the semantics of Values-Type-Intersection, this might
;;; return {T, T} when there isn't really any intersection (?).
;;;
(defun values-types-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
	   (values t t))
	  ((or (values-ctype-p type1) (values-ctype-p type2))
	   (multiple-value-bind (res win)
			            (values-type-intersection type1 type2)
	     (values (not (eq res *empty-type*))
		       win)))
	  (t
	   (types-intersect type1 type2))))

;;; Values-Subtypep  --  Interface
;;;
;;;    A subtypep-like operation that can be used on any types, including
;;; values types.
;;;

(defun values-subtypep (type1 type2)
  (declare (type ctype type1 type2))
  (cond ((eq type2 *wild-type*) (values t t))
	  ((eq type1 *wild-type*)
	   (values (eq type2 *universal-type*) t))
	  ((not (values-types-intersect type1 type2))
	   (values nil t))
	  (t
	   (if (or (values-ctype-p type1) (values-ctype-p type2))
	     (let ((type1 (coerce-to-values type1))
		     (type2 (coerce-to-values type2)))
	       (multiple-value-bind (types1 rest1)
				          (values-type-types type1)
		   (multiple-value-bind (types2 rest2)
				            (values-type-types type2)
		     (cond ((< (length (values-ctype-required type1))
			         (length (values-ctype-required type2)))
			      (values nil t))
			     ((< (length types1) (length types2))
			      (values nil nil))
			     ((or (values-ctype-keyp type1)
			          (values-ctype-keyp type2))
			      (values nil nil))
			     (t
			      (do ((t1 types1 (rest t1))
			           (t2 types2 (rest t2)))
			          ((null t2)
			           (csubtypep rest1 rest2))
			        (multiple-value-bind
				    (res win-p)
				    (csubtypep (first t1) (first t2))
			          (unless win-p
				      (return (values nil nil)))
			          (unless res
				      (return (values nil t))))))))))
	     (csubtypep type1 type2)))))
  

;;;; Type method interfaces:

;;; Csubtypep  --  Interface
;;;
;;;    Like subtypep, only works on Type structures.
;;;
(defun csubtypep (type1 type2)
  (declare (type ctype type1 type2))
  (unless (typep type1 'ctype)
    (report-bad-arg type1 'ctype))
  (unless (typep type2 'ctype)
    (report-bad-arg type2 'ctype))
  (cond ((or (eq type1 type2)
	       (eq type1 *empty-type*)
	       (eq type2 *wild-type*))
	   (values t t))
	  ((or (eq type1 *wild-type*)
	       (eq type2 *empty-type*))
	   (values nil t))
	  (t
	   (invoke-type-method :simple-subtypep :complex-subtypep-arg2
			           type1 type2
			           :complex-arg1 :complex-subtypep-arg1))))
;;; Type=  --  Interface
;;;
;;;    If two types are definitely equivalent, return true.  The second value
;;; indicates whether the first value is definitely correct.  This should only
;;; fail in the presence of Hairy types.
;;;

(defun type= (type1 type2)
   (declare (type ctype type1 type2))
   (if (eq type1 type2)
     (values t t)
     (invoke-type-method :simple-= :complex-= type1 type2)))

;;; TYPE/=  --  Interface
;;;
;;;    Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL.  This is useful in cases where the
;;; conservative assumption is =.
;;;
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win)
		           (type= type1 type2)
    (if win
	(values (not res) t)
	(values nil nil))))

;;; Type-Union  --  Interface
;;;
;;;    Find a type which includes both types.  Any inexactness is represented
;;; by the fuzzy element types; we return a single value that is precise to the
;;; best of our knowledge.  This result is simplified into the canonical form,
;;; thus is not a UNION type unless there is no other way to represent the
;;; result.
;;; 

(defun type-union (type1 type2)
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
    type1
    (let ((res (invoke-type-method :simple-union :complex-union
				           type1 type2
				           :default :vanilla)))
	(cond ((eq res :vanilla)
	       (or (vanilla-union type1 type2)
		     (make-union-ctype (list type1 type2))))
	      (res)
	      (t
	       (make-union-ctype (list type1 type2)))))))

;;; Type-Intersection  --  Interface
;;;
;;;    Return as restrictive a type as we can discover that is no more
;;; restrictive than the intersection of Type1 and Type2.  The second value is
;;; true if the result is exact.  At worst, we randomly return one of the
;;; arguments as the first value (trying not to return a hairy type).
;;;

(defun type-intersection (type1 type2)
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      (values type1 t)
      (invoke-type-method :simple-intersection :complex-intersection
			  type1 type2
			  :default (values *empty-type* t))))

;;; Types-Intersect  --  Interface
;;;
;;;    The first value is true unless the types don't intersect.  The second
;;; value is true if the first value is definitely correct.  NIL is considered
;;; to intersect with any type.  If T is a subtype of either type, then we also
;;; return T, T.  This way we consider hairy types to intersect with T.
;;;
(defun types-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (multiple-value-bind (val winp)
			   (type-intersection type1 type2)
	(cond ((not winp)
	       (if (or (csubtypep *universal-type* type1)
		       (csubtypep *universal-type* type2))
		   (values t t)
		   (values t nil)))
	      ((eq val *empty-type*) (values nil t))
	      (t (values t t))))))

;;; Type-Specifier  --  Interface
;;;
;;;    Return a Common Lisp type specifier corresponding to this type.
;;;
(defun type-specifier (type)
  (unless (ctype-p type)
    (setq type (require-type type 'ctype)))
  (locally 
      (declare (type ctype type))
    (funcall (type-class-unparse (ctype-class-info type)) type)))

;;; VALUES-SPECIFIER-TYPE  --  Interface
;;;
;;;    Return the type structure corresponding to a type specifier.  We pick
;;; off Structure types as a special case.
;;;

(defun values-specifier-type-internal (orig)
  (or (info-type-builtin orig) ; this table could contain bytes etal and ands ors nots of built-in types - no classes
      
      (let ((spec (type-expand orig)))
        (cond
         ((and (not (eq spec orig))
               (info-type-builtin spec)))
         ((eq (info-type-kind spec) :instance)
          (let* ((class-ctype (%class.ctype (find-class spec))))
            (or (class-ctype-translation class-ctype)
                class-ctype)))
         ((typep spec 'class)
          (let* ((class-ctype (%class.ctype spec)))
            (or (class-ctype-translation class-ctype)
                class-ctype)))
         ((let ((cell (find-builtin-cell spec nil)))
           (and cell (cdr cell))))
         (t
          (let* ((lspec (if (atom spec) (list spec) spec))
                 (fun (info-type-translator (car lspec))))
            (cond (fun (funcall fun lspec nil))
                  ((or (and (consp spec) (symbolp (car spec)))
                       (symbolp spec))
                   (when *type-system-initialized*
                     (signal 'parse-unknown-type :specifier spec))
                   ;;
                   ;; Inhibit caching...
                   nil)
                  (t
                   (error "Bad thing to be a type specifier: ~S." spec)))))))))

(eval-when (:compile-toplevel :execute)
  (defconstant type-cache-size (ash 1 7))
  (defconstant type-cache-mask (1- type-cache-size)))

(defun hash-type-specifier (spec)
  (logand (sxhash spec) type-cache-mask))

(let* ((type-cache-specs (make-array type-cache-size))
       (type-cache-ctypes (make-array type-cache-size))
       (probes 0)
       (hits 0)
       (ncleared 0)
       (locked nil))
  
  (defun clear-type-cache ()
    (%init-misc 0 type-cache-specs)
    (%init-misc 0 type-cache-ctypes)
    (incf ncleared)
    nil)

  (defun values-specifier-type (spec)
    (if (typep spec 'class)
      (let* ((class-ctype (%class.ctype spec)))
        (or (class-ctype-translation class-ctype) class-ctype))
      (if locked
        (or (values-specifier-type-internal spec)
            (make-unknown-ctype :specifier spec))
        (unwind-protect
          (progn
            (setq locked t)
            (if (or (symbolp spec)
                    (and (consp spec) (symbolp (car spec))))
              (let* ((idx (hash-type-specifier spec)))
                (incf probes)
                (if (equal (svref type-cache-specs idx) spec)
                  (progn
                    (incf hits)
                    (svref type-cache-ctypes idx))
                  (let* ((ctype (values-specifier-type-internal spec)))
                    (if ctype
                      (setf (svref type-cache-specs idx) (copy-tree spec)       ; in case it was stack-consed
                            (svref type-cache-ctypes idx) ctype)
                      (make-unknown-ctype :specifier spec)))))
              (values-specifier-type-internal spec)))
          (setq locked nil)))))
  
  (defun type-cache-hit-rate ()
    (values hits probes))
  
  (defun type-cache-locked-p ()
    locked)

  (defun lock-type-cache ()
    (setq locked t)))

                    

  

;;; SPECIFIER-TYPE  --  Interface
;;;
;;;    Like VALUES-SPECIFIER-TYPE, except that we guarantee to never return a
;;; VALUES type.
;;; 
(defun specifier-type (x)
  (let ((res (values-specifier-type x)))
    (when (values-ctype-p res)
      (error "VALUES type illegal in this context:~%  ~S" x))
    res))


;;; Precompute-Types  --  Interface
;;;
;;;    Take a list of type specifiers, compute the translation and define it as
;;; a builtin type.
;;;
 
(defun precompute-types (specs)
  (dolist (spec specs)
    (let ((res (specifier-type spec)))
      (when (numeric-ctype-p res)
        (let ((pred (make-numeric-ctype-predicate res)))
          (when pred (setf (numeric-ctype-predicate res) pred))))
      (unless (unknown-ctype-p res)
	  (setf (info-type-builtin spec) res)
	  (setf (info-type-kind spec) :primitive)))))

;;;; Builtin types.

;;; The NAMED-TYPE is used to represent *, T and NIL.  These types must be
;;; super or sub types of all types, not just classes and * & NIL aren't
;;; classes anyway, so it wouldn't make much sense to make them built-in
;;; classes.
;;;

(defun define-named-ctype (name)
  (let* ((ctype (%istruct 'named-ctype
                          (type-class-or-lose 'named)
                          nil
                          name)))
    (setf (info-type-kind name) :builtin
          (info-type-builtin name) ctype)))


(defvar *wild-type* (define-named-ctype '*))
(defvar *empty-type* (define-named-ctype nil))
(defvar *universal-type* (define-named-ctype t))

(define-type-method (named :simple-=) (type1 type2)
  (values (eq type1 type2) t))

(define-type-method (named :simple-subtypep) (type1 type2)
  (values (or (eq type1 *empty-type*) (eq type2 *wild-type*)) t))

(define-type-method (named :complex-subtypep-arg1) (type1 type2)
  (assert (not (hairy-ctype-p type2)))
  (values (eq type1 *empty-type*) t))

(define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (if (hairy-ctype-p type1)
      (values nil nil)
      (values (not (eq type2 *empty-type*)) t)))

(define-type-method (named :complex-intersection) (type1 type2)
  (vanilla-intersection type1 type2))

(define-type-method (named :unparse) (x)
  (named-ctype-name x))


;;;; Hairy and unknown types:

;;; The Hairy-Type represents anything too wierd to be described reasonably or
;;; to be useful, such as AND, NOT and SATISFIES and unknown types.  We just
;;; remember the original type spec.
;;;

(defun make-hairy-ctype (&key specifier (enumerable t))
  (%istruct 'hairy-ctype
            (type-class-or-lose 'hairy)
            enumerable
            specifier))

(defun hairy-ctype-p (x)
  (istruct-typep x 'hairy-ctype))

(setf (type-predicate 'hairy-ctype) 'hairy-ctype-p)

(define-type-method (hairy :unparse) (x) (hairy-ctype-specifier x))

(define-type-method (hairy :simple-subtypep) (type1 type2)
  (let ((hairy-spec1 (hairy-ctype-specifier type1))
	(hairy-spec2 (hairy-ctype-specifier type2)))
    (cond ((and (consp hairy-spec1) (eq (car hairy-spec1) 'not)
		(consp hairy-spec2) (eq (car hairy-spec2) 'not))
	   (csubtypep (specifier-type (cadr hairy-spec2))
		      (specifier-type (cadr hairy-spec1))))
	  ((equal hairy-spec1 hairy-spec2)
	   (values t t))
	  (t
	   (values nil nil)))))

(define-type-method (hairy :complex-subtypep-arg2) (type1 type2)
  (let ((hairy-spec (hairy-ctype-specifier type2)))
    (cond
      ((and (consp hairy-spec) (eq (car hairy-spec) 'not))
       (multiple-value-bind (val win)
	   (type-intersection type1 (specifier-type (cadr hairy-spec)))
	 (if win
	     (values (eq val *empty-type*) t)
	     (values nil nil))))
      ((and (consp hairy-spec) (eq (car hairy-spec) 'and))
       (block PUNT
	 (values (every-type-op csubtypep type1
				(mapcar #'specifier-type (cdr hairy-spec)))
		 t)))
      (t
       (values nil nil)))))

(define-type-method (hairy :complex-subtypep-arg1) (type1 type2)
  (let ((hairy-spec (hairy-ctype-specifier type1)))
    (cond
      ((and (consp hairy-spec) (eq (car hairy-spec) 'not))
       ;; We're definitely not (exactly) what we're not, and
       ;; definitely not a UNION type that contains exactly
       ;; what we're not; after that, it gets harder.
       ;; I wonder whether it makes more sense to implement ATGM
       ;; and RATIO some other way enirely.
       (let* ((negated-ctype (specifier-type (cadr hairy-spec))))
         (if (or (eq type2 negated-ctype)
                 (and (typep type2 'union-ctype)
                      (member negated-ctype (union-ctype-types type2))))
           (values nil t)
           (values nil nil))))
      ((and (consp hairy-spec) (eq (car hairy-spec) 'and))
       (block PUNT
	 (if (any-type-op csubtypep type2
			  (mapcar #'specifier-type (cdr hairy-spec))
			  :list-first t)
	     (values t t)
	     (values nil nil))))
      (t
       (values nil nil)))))

(define-type-method (hairy :complex-=)
		    (type1 type2)
  (declare (ignore type1 type2))
  (values nil nil))

(define-type-method (hairy :simple-intersection :complex-intersection)
		    (type1 type2)
  (declare (ignore type2))
  (values type1 nil))

(define-type-method (hairy :complex-union) (type1 type2)
  (make-union-ctype (list type1 type2)))

(define-type-method (hairy :simple-=) (type1 type2)
  (if (equal (hairy-ctype-specifier type1)
             (hairy-ctype-specifier type2))
    (values t t)
    (values nil nil)))

(def-type-translator not (&whole x type)
  (declare (ignore type))
  (make-hairy-ctype :specifier x))

(def-type-translator satisfies (&whole x fun)
  (declare (ignore fun))
  (make-hairy-ctype :specifier x))

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet defined).
;;; We make this distinction since we don't want to complain about types that
;;; are hairy but defined.
;;;

(defun make-unknown-ctype (&key specifier (enumerable t))
  (%istruct 'unknown-ctype
            (type-class-or-lose 'hairy)
            enumerable
            specifier))

(defun unknown-ctype-p (x)
  (istruct-typep x 'unknown-ctype))

(setf (type-predicate 'unknown-ctype) 'unknown-ctype-p)

;;;; Numeric types.

;;; A list of all the float formats, in order of decreasing precision.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant float-formats
    '(long-float double-float single-float short-float)))

;;; The type of a float format.
;;;
(deftype float-format () `(member ,@float-formats))

(defun make-numeric-ctype (&key class 
                                format
                                (complexp :real)
                                low
                                high
                                enumerable
                                predicate)
  (let ((ctype (%istruct 'numeric-ctype
                         (type-class-or-lose 'number)
                         enumerable
                         class
                         format
                         complexp
                         low
                         high
                         predicate))) 
    ctype))
    

(defun make-numeric-ctype-predicate (ctype)
  (let ((class (numeric-ctype-class ctype))
        (lo (numeric-ctype-low ctype))
        (hi (numeric-ctype-high ctype)))
    (if (eq class 'integer)
      (if (and hi lo (<= hi most-positive-fixnum)(>= lo most-negative-fixnum))      
        #'(lambda (n)
            (and (fixnump n)
                 (locally (declare (fixnum n hi lo))
                   (and (%i>= n lo)
                        (%i<= n hi)))))))))

(defun numeric-ctype-p (x)
  (istruct-typep x 'numeric-ctype))

(setf (type-predicate 'numeric-ctype) 'numeric-ctype-p)

(define-type-method (number :simple-=) (type1 type2)
  (values
   (and (eq (numeric-ctype-class type1) (numeric-ctype-class type2))
	(eq (numeric-ctype-format type1) (numeric-ctype-format type2))
	(eq (numeric-ctype-complexp type1) (numeric-ctype-complexp type2))
	(equal (numeric-ctype-low type1) (numeric-ctype-low type2))
	(equal (numeric-ctype-high type1) (numeric-ctype-high type2)))
   t))

(define-type-method (number :unparse) (type)
 (let* ((complexp (numeric-ctype-complexp type))
        (low (numeric-ctype-low type))
        (high (numeric-ctype-high type))
        (base (case (numeric-ctype-class type)
                (integer 'integer)
                (rational 'rational)
                (float (or (numeric-ctype-format type) 'float))
                (t 'real))))
    (let ((base+bounds
	     (cond ((and (eq base 'integer) high low)
		      (let ((high-count (logcount high))
			      (high-length (integer-length high)))
		        (cond ((= low 0)
			         (cond ((= high 0) '(integer 0 0))
				         ((= high 1) 'bit)
				         ((and (= high-count high-length)
				               (plusp high-length))
				          `(unsigned-byte ,high-length))
				         (t
				          `(mod ,(1+ high)))))
			        ((and (= low most-negative-fixnum)
				        (= high most-positive-fixnum))
			         'fixnum)
			        ((and (= low (lognot high))
				        (= high-count high-length)
				        (> high-count 0))
			         `(signed-byte ,(1+ high-length)))
			        (t
			         `(integer ,low ,high)))))
		     (high `(,base ,(or low '*) ,high))
		     (low
		      (if (and (eq base 'integer) (= low 0))
		        'unsigned-byte
		        `(,base ,low)))
		     (t base))))
      (ecase complexp
	  (:real 
	   base+bounds)
	  (:complex
	   (if (eq base+bounds 'real)
	     'complex
	     `(complex ,base+bounds)))
	  ((nil)
	   (assert (eq base+bounds 'real))
	   'number)))))

;;; Numeric-Bound-Test  --  Internal
;;;
;;;    Return true if X is "less than or equal" to Y, taking open bounds into
;;; consideration.  Closed is the predicate used to test the bound on a closed
;;; interval (e.g. <=), and Open is the predicate used on open bounds (e.g. <).
;;; Y is considered to be the outside bound, in the sense that if it is
;;; infinite (NIL), then the test suceeds, whereas if X is infinite, then the
;;; test fails (unless Y is also infinite).
;;;
;;;    This is for comparing bounds of the same kind, e.g. upper and upper.
;;; Use Numeric-Bound-Test* for different kinds of bounds.
;;;
(defmacro numeric-bound-test (x y closed open)
  `(cond ((not ,y) t)
	   ((not ,x) nil)
	   ((consp ,x)
	    (if (consp ,y)
	      (,closed (car ,x) (car ,y))
	      (,closed (car ,x) ,y)))
	   (t
	    (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

;;; Numeric-Bound-Test*  --  Internal
;;;
;;;    Used to compare upper and lower bounds.  This is different from the
;;; same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we return true
;;;    if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval, causing
;;;    us to use the Open test for those cases as well.
;;;
(defmacro numeric-bound-test* (x y closed open)
  `(cond ((not ,y) t)
	   ((not ,x) t)
	   ((consp ,x)
	    (if (consp ,y)
	      (,open (car ,x) (car ,y))
	      (,open (car ,x) ,y)))
	   (t
	    (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

;;; Numeric-Bound-Max  --  Internal
;;;
;;;    Return whichever of the numeric bounds X and Y is "maximal" according to
;;; the predicates Closed (e.g. >=) and Open (e.g. >).  This is only meaningful
;;; for maximizing like bounds, i.e. upper and upper.  If Max-P is true, then
;;; we return NIL if X or Y is NIL, otherwise we return the other arg.
;;;
(defmacro numeric-bound-max (x y closed open max-p)
  (once-only ((n-x x)
	        (n-y y))
    `(cond ((not ,n-x) ,(if max-p nil n-y))
	     ((not ,n-y) ,(if max-p nil n-x))
	     ((consp ,n-x)
	      (if (consp ,n-y)
		  (if (,closed (car ,n-x) (car ,n-y)) ,n-x ,n-y)
		  (if (,open (car ,n-x) ,n-y) ,n-x ,n-y)))
	     (t
	      (if (consp ,n-y)
		  (if (,open (car ,n-y) ,n-x) ,n-y ,n-x)
		  (if (,closed ,n-y ,n-x) ,n-y ,n-x))))))


(define-type-method (number :simple-subtypep) (type1 type2)
  (let ((class1 (numeric-ctype-class type1))
	  (class2 (numeric-ctype-class type2))
	  (complexp2 (numeric-ctype-complexp type2))
	  (format2 (numeric-ctype-format type2))
	  (low1 (numeric-ctype-low type1))
	  (high1 (numeric-ctype-high type1))
	  (low2 (numeric-ctype-low type2))
	  (high2 (numeric-ctype-high type2)))
    ;;
    ;; If one is complex and the other isn't, they are disjoint.
    (cond ((not (or (eq (numeric-ctype-complexp type1) complexp2)
		        (null complexp2)))
	     (values nil t))
	    ;;
	    ;; If the classes are specified and different, the types are
	    ;; disjoint unless type2 is rational and type1 is integer.
	    ((not (or (eq class1 class2) (null class2)
		        (and (eq class1 'integer) (eq class2 'rational))))
	     (values nil t))
	    ;;
	    ;; If the float formats are specified and different, the types
	    ;; are disjoint.
	    ((not (or (eq (numeric-ctype-format type1) format2)
		        (null format2)))
	     (values nil t))
	    ;;
	    ;; Check the bounds.
	    ((and (numeric-bound-test low1 low2 >= >)
		    (numeric-bound-test high1 high2 <= <))
	     (values t t))
	    (t
	     (values nil t)))))

;(define-superclasses number (generic-number))

;;; NUMERIC-TYPES-ADJACENT  --  Internal
;;;
;;;    If the high bound of Low is adjacent to the low bound of High, then
;;; return T, otherwise NIL.
;;;
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-ctype-high low))
	  (high-bound (numeric-ctype-low high)))
    (cond ((not (and low-bound high-bound)) nil)
	    ((consp low-bound)
	     (eql (car low-bound) high-bound))
	    ((consp high-bound)
	     (eql (car high-bound) low-bound))
	    ((and (eq (numeric-ctype-class low) 'integer)
		    (eq (numeric-ctype-class high) 'integer))
	     (eql (1+ low-bound) high-bound))
	    (t
	     nil))))

;;; NUMBER :SIMPLE-UNION method  -- Internal
;;;
;;; Return the a numeric type that is a supertype for both type1 and type2.
;;; 
;;; ### Note: we give up early, so keep from dropping lots of information on
;;; the floor by returning overly general types.
;;;
(define-type-method (number :simple-union) (type1 type2)
  (declare (type numeric-ctype type1 type2))
  (cond ((csubtypep type1 type2) type2)
	  ((csubtypep type2 type1) type1)
	  (t
	   (let ((class1 (numeric-ctype-class type1))
	         (format1 (numeric-ctype-format type1))
	         (complexp1 (numeric-ctype-complexp type1))
	         (class2 (numeric-ctype-class type2))
	         (format2 (numeric-ctype-format type2))
	         (complexp2 (numeric-ctype-complexp type2)))
	     (when (and (eq class1 class2)
		          (eq format1 format2)
		          (eq complexp1 complexp2)
		          (or (numeric-types-intersect type1 type2)
			        (numeric-types-adjacent type1 type2)
			        (numeric-types-adjacent type2 type1)))
	       (make-numeric-ctype
	        :class class1
	        :format format1
	        :complexp complexp1
	        :low (numeric-bound-max (numeric-ctype-low type1)
				              (numeric-ctype-low type2)
				              < <= t)
	        :high (numeric-bound-max (numeric-ctype-high type1)
				               (numeric-ctype-high type2)
				               > >= t)))))))

(setf (info-type-kind 'number) :primitive
      (info-type-builtin 'number) (make-numeric-ctype :complexp nil))

(def-type-translator complex (&optional spec)
  (if (eq spec '*)
    (make-numeric-ctype :complexp :complex)
    (let ((type (specifier-type spec)))
	(unless (numeric-ctype-p type)
	  (error "Component type for Complex is not numeric: ~S." spec))
	(when (eq (numeric-ctype-complexp type) :complex)
	  (error "Component type for Complex is complex: ~S." spec))      
	(let ((res (copy-uvector type)))
	  (setf (numeric-ctype-complexp res) :complex)
          (setf (numeric-ctype-predicate res) nil) ; << 
	  res))))

;;; Check-Bound  --  Internal
;;;
;;;    Check that X is a well-formed numeric bound of the specified Type.
;;; If X is *, return NIL, otherwise return the bound.
;;;
(defmacro check-bound (x type)
  `(cond ((eq ,x '*) nil)
	   ((or (typep ,x ',type)
	        (and (consp ,x) (typep (car ,x) ',type) (null (cdr ,x))))
	    ,x)
	   (t
	    (error "Bound is not *, a ~A or a list of a ~A: ~S" ',type ',type ,x))))

(def-type-translator integer (&optional low high)
  (let* ((l (check-bound low integer))
	   (lb (if (consp l) (1+ (car l)) l))
	   (h (check-bound high integer))
	   (hb (if (consp h) (1- (car h)) h)))
    (if (and hb lb (< hb lb))
      *empty-type*
      (make-numeric-ctype :class 'integer  :complexp :real
                          :enumerable (not (null (and l h)))
                          :low lb
                          :high hb))))

(deftype mod (n)
  (unless (and (integerp n) (> n 0))
    (error "Bad N specified for MOD type specifier: ~S." n))
  `(integer 0 ,(1- n)))


(defmacro def-bounded-type (type class format)
  `(def-type-translator ,type (&optional low high)
     (let ((lb (check-bound low ,type))
	     (hb (check-bound high ,type)))
       (unless (numeric-bound-test* lb hb <= <)
	   (error "Lower bound ~S is not less than upper bound ~S." low high))
       (make-numeric-ctype :class ',class :format ',format :low lb :high hb))))

(def-bounded-type rational rational nil)
(def-bounded-type float float nil)
(def-bounded-type real nil nil)

(defmacro define-float-format (f)
  `(def-bounded-type ,f float ,f))

(define-float-format short-float)
(define-float-format single-float)
(define-float-format double-float)
(define-float-format long-float)

(defun numeric-types-intersect (type1 type2)
  (declare (type numeric-ctype type1 type2))
  (let* ((class1 (numeric-ctype-class type1))
	   (class2 (numeric-ctype-class type2))
	   (complexp1 (numeric-ctype-complexp type1))
	   (complexp2 (numeric-ctype-complexp type2))
	   (format1 (numeric-ctype-format type1))
	   (format2 (numeric-ctype-format type2))
	   (low1 (numeric-ctype-low type1))
	   (high1 (numeric-ctype-high type1))
	   (low2 (numeric-ctype-low type2))
	   (high2 (numeric-ctype-high type2)))
    ;;
    ;; If one is complex and the other isn't, then they are disjoint.
    (cond ((not (or (eq complexp1 complexp2)
		        (null complexp1) (null complexp2)))
	     nil)
	    ;;
	    ;; If either type is a float, then the other must either be specified
	    ;; to be a float or unspecified.  Otherwise, they are disjoint.
	    ((and (eq class1 'float) (not (member class2 '(float nil)))) nil)
	    ((and (eq class2 'float) (not (member class1 '(float nil)))) nil)
	    ;;
	    ;; If the float formats are specified and different, the types
	    ;; are disjoint.
	    ((not (or (eq format1 format2) (null format1) (null format2)))
	     nil)
	    (t
	     ;;
	     ;; Check the bounds.  This is a bit odd because we must always have
	     ;; the outer bound of the interval as the second arg.
	     (if (numeric-bound-test high1 high2 <= <)
	       (or (and (numeric-bound-test low1 low2 >= >)
			    (numeric-bound-test* low1 high2 <= <))
		     (and (numeric-bound-test low2 low1 >= >)
			    (numeric-bound-test* low2 high1 <= <)))
	       (or (and (numeric-bound-test* low2 high1 <= <)
			    (numeric-bound-test low2 low1 >= >))
		     (and (numeric-bound-test high2 high1 <= <)
			    (numeric-bound-test* high2 low1 >= >))))))))

;;; Round-Numeric-Bound  --  Internal
;;;
;;;    Take the numeric bound X and convert it into something that can be used
;;; as a bound in a numeric type with the specified Class and Format.  If up-p
;;; is true, then we round up as needed, otherwise we round down.  Up-p true
;;; implies that X is a lower bound, i.e. (N) > N.
;;;
;;; This is used by Numeric-Type-Intersection to mash the bound into the
;;; appropriate type number.  X may only be a float when Class is Float.
;;;
;;; ### Note: it is possible for the coercion to a float to overflow or
;;; underflow.  This happens when the bound doesn't fit in the specified
;;; format.  In this case, we should really return the appropriate
;;; {Most | Least}-{Positive | Negative}-XXX-Float float of desired format.
;;; But these conditions aren't currently signalled in any useful way.
;;;
;;; Also, when converting an open rational bound into a float we should
;;; probably convert it to a closed bound of the closest float in the specified
;;; format.  In general, open float bounds are fucked.
;;;
(defun round-numeric-bound (x class format up-p)
  (if x
    (let ((cx (if (consp x) (car x) x)))
	(ecase class
	  ((nil rational) x)
	  (integer
	   (if (and (consp x) (integerp cx))
           (if up-p (1+ cx) (1- cx))
           (if up-p (ceiling cx) (floor cx))))
	  (float
	   (let ((res (if format (coerce cx format) (float cx))))
	     (if (consp x) (list res) res)))))
    nil))

;;; Number :Simple-Intersection type method  --  Internal
;;;
;;;    Handle the case of Type-Intersection on two numeric types.  We use
;;; Types-Intersect to throw out the case of types with no intersection.  If an
;;; attribute in Type1 is unspecified, then we use Type2's attribute, which
;;; must be at least as restrictive.  If the types intersect, then the only
;;; attributes that can be specified and different are the class and the
;;; bounds.
;;;
;;;    When the class differs, we use the more restrictive class.  The only
;;; interesting case is rational/integer, since rational includes integer.
;;;
;;;    We make the result lower (upper) bound the maximum (minimum) of the
;;; argument lower (upper) bounds.  We convert the bounds into the
;;; appropriate numeric type before maximizing.  This avoids possible confusion
;;; due to mixed-type comparisons (but I think the result is the same).
;;;
(define-type-method (number :simple-intersection) (type1 type2)
  (declare (type numeric-type type1 type2))
  (if (numeric-types-intersect type1 type2)
    (let* ((class1 (numeric-ctype-class type1))
	     (class2 (numeric-ctype-class type2))
	     (class (ecase class1
		        ((nil) class2)
		        ((integer float) class1)
		        (rational (if (eq class2 'integer) 'integer 'rational))))
	     (format (or (numeric-ctype-format type1)
			     (numeric-ctype-format type2))))
	(values
	 (make-numeric-ctype
	  :class class
	  :format format
	  :complexp (or (numeric-ctype-complexp type1)
			    (numeric-ctype-complexp type2))
	  :low (numeric-bound-max
		  (round-numeric-bound (numeric-ctype-low type1)
				           class format t)
		  (round-numeric-bound (numeric-ctype-low type2)
				           class format t)
		  >= > nil)
	  :high (numeric-bound-max
		   (round-numeric-bound (numeric-ctype-high type1)
				            class format nil)
		   (round-numeric-bound (numeric-ctype-high type2)
				            class format nil)
		   <= < nil))
	 t))
    (values *empty-type* t)))

;;; Float-Format-Max  --  Interface
;;;
;;;    Given two float formats, return the one with more precision.  If either
;;; one is null, return NIL.
;;;
(defun float-format-max (f1 f2)
  (when (and f1 f2)
    (dolist (f float-formats (error "Bad float format: ~S." f1))
      (when (or (eq f f1) (eq f f2))
	  (return f)))))


;;; Numeric-Contagion  --  Interface
;;;
;;;    Return the result of an operation on Type1 and Type2 according to the
;;; rules of numeric contagion.  This is always NUMBER, some float format
;;; (possibly complex) or RATIONAL.  Due to rational canonicalization, there
;;; isn't much we can do here with integers or rational complex numbers.
;;;
;;;    If either argument is not a Numeric-Type, then return NUMBER.  This is
;;; useful mainly for allowing types that are technically numbers, but not a
;;; Numeric-Type. 
;;;
(defun numeric-contagion (type1 type2)
  (if (and (numeric-ctype-p type1) (numeric-ctype-p type2))
    (let ((class1 (numeric-ctype-class type1))
	    (class2 (numeric-ctype-class type2))
	    (format1 (numeric-ctype-format type1))
	    (format2 (numeric-ctype-format type2))
	    (complexp1 (numeric-ctype-complexp type1))
	    (complexp2 (numeric-ctype-complexp type2)))
	(cond ((or (null complexp1)
		     (null complexp2))
	       (specifier-type 'number))
	      ((eq class1 'float)
	       (make-numeric-ctype
		  :class 'float
		  :format (ecase class2
			      (float (float-format-max format1 format2))
			      ((integer rational) format1)
			      ((nil) nil))
		  :complexp (if (or (eq complexp1 :complex)
				        (eq complexp2 :complex))
			        :complex
			        :real)))
	      ((eq class2 'float) (numeric-contagion type2 type1))
	      ((and (eq complexp1 :real) (eq complexp2 :real))
	       (make-numeric-ctype
		  :class (and class1 class2 'rational)
		  :complexp :real))
	      (t
	       (specifier-type 'number))))
    (specifier-type 'number)))


;;;; Array types:

;;; The Array-Type is used to represent all array types, including things such
;;; as SIMPLE-STRING.
;;;

(defun make-array-ctype (&key
                         (dimensions '*)
                         (complexp '*)
                         element-type
                         (specialized-element-type *wild-type*))
  (%istruct 'array-ctype
            (type-class-or-lose 'array)
            nil
            dimensions
            complexp
            element-type
            specialized-element-type))

(defun array-ctype-p (x) (istruct-typep x 'array-ctype))
(setf (type-predicate 'array-ctype) 'array-ctype-p)

;;; Specialized-Element-Type-Maybe  --  Internal
;;;
;;;      What this does depends on the setting of the
;;; *use-implementation-types* switch.  If true, return the specialized element
;;; type, otherwise return the original element type.
;;;
(defun specialized-element-type-maybe (type)
  (declare (type array-ctype type))
  (if *use-implementation-types*
      (array-ctype-specialized-element-type type)
      (array-ctype-element-type type)))

(define-type-method (array :simple-=) (type1 type2)
  (values (and (equal (array-ctype-dimensions type1)
		          (array-ctype-dimensions type2))
	         (eq (array-ctype-complexp type1)
		       (array-ctype-complexp type2))
	         (type= (specialized-element-type-maybe type1)
		          (specialized-element-type-maybe type2)))
	    t))

(define-type-method (array :unparse) (type)
  (let ((dims (array-ctype-dimensions type))
	  (eltype (type-specifier (array-ctype-element-type type)))
	  (complexp (array-ctype-complexp type)))
    (cond ((eq dims '*)
	     (if (eq eltype '*)
	       (if complexp 'array 'simple-array)
	       (if complexp `(array ,eltype) `(simple-array ,eltype))))
	    ((= (length dims) 1) 
	     (if complexp
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'bit-vector)
		     (base-char 'base-string)
		     #|(character 'string)|#
		     (* 'vector)
		     (t `(vector ,eltype)))
		   (case eltype
		     (bit `(bit-vector ,(car dims)))
		     (base-char `(base-string ,(car dims)))
		     #|(character `(string ,(car dims)))|#
		     (t `(vector ,eltype ,(car dims)))))
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'simple-bit-vector)
		     (base-char 'simple-base-string)
		     (character 'simple-base-string)
		     ((t) 'simple-vector)
		     (t `(simple-array ,eltype (*))))
		   (case eltype
		     (bit `(simple-bit-vector ,(car dims)))
		     (base-char `(simple-base-string ,(car dims)))
                     (character `(simple-base-string ,(car dims)))
		     ((t) `(simple-vector ,(car dims)))
		     (t `(simple-array ,eltype ,dims))))))
	    (t
	     (if complexp
	       `(array ,eltype ,dims)
	       `(simple-array ,eltype ,dims))))))

(define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-ctype-dimensions type1))
	  (dims2 (array-ctype-dimensions type2))
	  (complexp2 (array-ctype-complexp type2)))
    ;;
    ;; See if dimensions are compatible.
    (cond ((not (or (eq dims2 '*)
		        (and (not (eq dims1 '*))
			       (= (length dims1) (length dims2))
			       (every #'(lambda (x y)
				            (or (eq y '*) (eql x y)))
				        dims1 dims2))))
	     (values nil t))
	    ;;
	    ;; See if complexp is compatible.
	    ((not (or (eq complexp2 '*)
		        (eq (array-ctype-complexp type1) complexp2)))
	     (values nil t))
	    ;;
	    ;; If the type2 eltype is wild, we win.  Otherwise, the types must be
	    ;; identical.
	    ((or (eq (array-ctype-element-type type2) *wild-type*)
	         (type= (specialized-element-type-maybe type1)
		          (specialized-element-type-maybe type2)))
	     (values t t))
	    (t
	     (values nil t)))))

; (define-superclasses array (string string) (vector vector) (array))


(defun array-types-intersect (type1 type2)
  (declare (type array-ctype type1 type2))
  (let ((dims1 (array-ctype-dimensions type1))
	  (dims2 (array-ctype-dimensions type2))
	  (complexp1 (array-ctype-complexp type1))
	  (complexp2 (array-ctype-complexp type2)))
    ;;
    ;; See if dimensions are compatible.
    (cond ((not (or (eq dims1 '*) (eq dims2 '*)
		        (and (= (length dims1) (length dims2))
			       (every #'(lambda (x y)
				            (or (eq x '*) (eq y '*) (= x y)))
				        dims1 dims2))))
	     (values nil t))
	    ;;
	    ;; See if complexp is compatible.
	    ((not (or (eq complexp1 '*) (eq complexp2 '*)
		        (eq complexp1 complexp2)))
	     (values nil t))
	    ;;
	    ;; If either element type is wild, then they intersect.  Otherwise,
	    ;; the types must be identical.
	    ((or (eq (array-ctype-element-type type1) *wild-type*)
	         (eq (array-ctype-element-type type2) *wild-type*)
	         (type= (specialized-element-type-maybe type1)
		          (specialized-element-type-maybe type2)))
           
	     (values t t))
	    (t
	     (values nil t)))))

(define-type-method (array :simple-intersection) (type1 type2)
  (declare (type array-ctype type1 type2))
  (if (array-types-intersect type1 type2)
    (let ((dims1 (array-ctype-dimensions type1))
	    (dims2 (array-ctype-dimensions type2))
	    (complexp1 (array-ctype-complexp type1))
	    (complexp2 (array-ctype-complexp type2))
	    (eltype1 (array-ctype-element-type type1))
	    (eltype2 (array-ctype-element-type type2)))
	(values
	 (specialize-array-type
	  (make-array-ctype
	   :dimensions (cond ((eq dims1 '*) dims2)
			         ((eq dims2 '*) dims1)
			         (t
			          (mapcar #'(lambda (x y) (if (eq x '*) y x))
				            dims1 dims2)))
	   :complexp (if (eq complexp1 '*) complexp2 complexp1)
	   :element-type (if (eq eltype1 *wild-type*) eltype2 eltype1)))
	 t))
    (values *empty-type* t)))

;;; Check-Array-Dimensions  --  Internal
;;;
;;;    Check a supplied dimension list to determine if it is legal.
;;;
(defun check-array-dimensions (dims)
  (typecase dims
    ((member *) dims)
    (integer
     (when (minusp dims)
       (error "Arrays can't have a negative number of dimensions: ~D." dims))
     (when (>= dims array-rank-limit)
       (error "Array type has too many dimensions: ~S." dims))
     (make-list dims :initial-element '*))
    (list
     (when (>= (length dims) array-rank-limit)
       (error "Array type has too many dimensions: ~S." dims))
     (dolist (dim dims)
       (unless (eq dim '*)
	   (unless (and (integerp dim)
		          (>= dim 0) (< dim array-dimension-limit))
	     (error "Bad dimension in array type: ~S." dim))))
     dims)
    (t
     (error "Array dimensions is not a list, integer or *:~%  ~S"
	      dims))))

(def-type-translator array (&optional element-type dimensions)
  (specialize-array-type
   (make-array-ctype :dimensions (check-array-dimensions dimensions)
		         :element-type (specifier-type element-type))))

(def-type-translator simple-array (&optional element-type dimensions)
  (specialize-array-type
   (make-array-ctype :dimensions (check-array-dimensions dimensions)
		         :element-type (specifier-type element-type)
		         :complexp nil)))

(defparameter specialized-array-element-types
  '(bit (unsigned-byte 8) (signed-byte 8) (unsigned-byte 16) 
    (signed-byte 16) (unsigned-byte 32) (signed-byte 32)
    character *unused* short-float double-float))

(defun specialize-array-type (type)
  (let ((eltype (array-ctype-element-type type)))
    
    (setf (array-ctype-specialized-element-type type)
	    (if (eq eltype *wild-type*)
	      *wild-type*
	      (dolist (stype-name specialized-array-element-types
				        (specifier-type 't))
		  (let ((stype (specifier-type stype-name)))
		    (when (csubtypep eltype stype)
		      (return stype))))))
    
    type))


;;;; Member types.

;;; The Member-Type represents uses of the MEMBER type specifier.  We bother
;;; with this at this level because MEMBER types are fairly important and union
;;; and intersection are well defined.

(defun make-member-ctype (&key members)
  (%istruct 'member-ctype
            (type-class-or-lose 'member)
            t
            members))

(defun member-ctype-p (x) (istruct-typep x 'member-ctype))
(setf (type-predicate 'member-ctype) 'member-ctype-p)

(define-type-method (member :unparse) (type)
  (let ((members (member-ctype-members type)))
    (if (equal members '(nil))
	'null
	`(member ,@members))))

(define-type-method (member :simple-subtypep) (type1 type2)
  (values (subsetp (member-ctype-members type1) (member-ctype-members type2))
	    t))


(define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (block PUNT
    (values (every-type-op ctypep type2 (member-ctype-members type1)
			         :list-first t)
	      t)))

;;; We punt if the odd type is enumerable and intersects with the member type.
;;; If not enumerable, then it is definitely not a subtype of the member type.
;;;
(define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (ctype-enumerable type1)) (values nil t))
	  ((types-intersect type1 type2) (values nil nil))
	  (t
	   (values nil t))))

(define-type-method (member :simple-intersection) (type1 type2)
  (let ((mem1 (member-ctype-members type1))
	  (mem2 (member-ctype-members type2)))
    (values (cond ((subsetp mem1 mem2) type1)
		      ((subsetp mem2 mem1) type2)
		      (t
		       (let ((res (intersection mem1 mem2)))
		         (if res
			     (make-member-ctype :members res)
			     *empty-type*))))
	      t)))

(define-type-method (member :complex-intersection) (type1 type2)
  (block PUNT
    (let* ((members))
      (let ((mem2 (member-ctype-members type2)))
	  (dolist (member mem2)
	    (multiple-value-bind (val win)
			             (ctypep member type1)
	      (unless win
	        (return-from PUNT (values type2 nil)))
	      (when val (push member members))))
        
	  (values (cond ((subsetp mem2 members) type2)
		          ((null members) *empty-type*)
		          (t
		           (make-member-ctype :members members)))
		    t)))))


;;; We don't need a :COMPLEX-UNION, since the only interesting case is a union
;;; type, and the member/union interaction is handled by the union type
;;; method.
(define-type-method (member :simple-union) (type1 type2)
  (let ((mem1 (member-ctype-members type1))
	  (mem2 (member-ctype-members type2)))
    (cond ((subsetp mem1 mem2) type2)
	    ((subsetp mem2 mem1) type1)
	    (t
	     (make-member-ctype :members (union mem1 mem2))))))


(define-type-method (member :simple-=) (type1 type2)
  (let ((mem1 (member-ctype-members type1))
	  (mem2 (member-ctype-members type2)))
    (values (and (subsetp mem1 mem2) (subsetp mem2 mem1))
	      t)))

(define-type-method (member :complex-=) (type1 type2)
  (if (ctype-enumerable type1)
    (multiple-value-bind (val win)
			       (csubtypep type2 type1)
	(if (or val (not win))
        (values nil nil)
        (values nil t)))
    (values nil t)))

(def-type-translator member (&rest members)
  (let ((mem (remove-duplicates members)))
    (if mem
	(make-member-ctype :members mem)
	*empty-type*)))


;;;; Union types:

;;; The Union-Type represents uses of the OR type specifier which can't be
;;; canonicalized to something simpler.  Canonical form:
;;;
;;; 1] There is never more than one Member-Type component.
;;; 2] There are never any Union-Type components.
;;;

(defun make-union-ctype (types)
  (declare (list types))
  (%istruct 'union-ctype
            (type-class-or-lose 'union)
            (every #'(lambda (x) (ctype-enumerable x)) types)
            types))

(defun union-ctype-p (x) (istruct-typep x 'union-ctype))
(setf (type-predicate 'union-ctype) 'union-ctype-p)


;;;    If List, then return that, otherwise the OR of the component types.
;;;
(define-type-method (union :unparse) (type)
  (declare (type ctype type))
  (if (type= type (specifier-type 'list))
      'list
      `(or ,@(mapcar #'type-specifier (union-ctype-types type)))))



;;; Two union types are equal if every type in one is equal to some type in the
;;; other.
;;;
(define-type-method (union :simple-=) (type1 type2)
  (block PUNT
    (let ((types1 (union-ctype-types type1))
	    (types2 (union-ctype-types type2)))
      (values (and (dolist (type1 types1 t)
		         (unless (any-type-op type= type1 types2)
		           (return nil)))
		       (dolist (type2 types2 t)
		         (unless (any-type-op type= type2 types1)
		           (return nil))))
	        t))))


;;; Similarly, a union type is a subtype of another if every element of Type1
;;; is a subtype of some element of Type2.
;;;
(define-type-method (union :simple-subtypep) (type1 type2)
  (block PUNT
    (let ((types2 (union-ctype-types type2)))
      (values (dolist (type1 (union-ctype-types type1) t)
		    (unless (any-type-op csubtypep type1 types2)
		      (return nil)))
	        t))))


(define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (block PUNT
    (values (every-type-op csubtypep type2 (union-ctype-types type1)
			         :list-first t)
	      t)))

(define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (block PUNT
    (values (any-type-op csubtypep type1 (union-ctype-types type2)) t)))


(define-type-method (union :complex-union) (type1 type2)
  (let* ((class1 (ctype-class-info type1)))
    (let* ((res))
      (let ((this-type type1))
	  (dolist (type (union-ctype-types type2)
		          (if res
			      (make-union-ctype (cons this-type (nreverse res)))
			      this-type))
	    (cond ((eq (ctype-class-info type) class1)
		     (let ((union (funcall (type-class-simple-union class1)
				               this-type type)))
		       (if union
		         (setq this-type union)
		         (push type res))))
		    ((csubtypep type this-type))
		    ((csubtypep type1 type) (return type2))
		    (t
		     (push type res))))))))

;;; For the union of union types, we let the :COMPLEX-UNION method do the work.
;;;
(define-type-method (union :simple-union) (type1 type2)
  (let ((res type1))
    (dolist (t2 (union-ctype-types type2) res)
      (setq res (type-union res t2)))))


(define-type-method (union :simple-intersection :complex-intersection)
		        (type1 type2)
  (let ((res *empty-type*)
	  (win t))
    (dolist (type (union-ctype-types type2) (values res win))
      (multiple-value-bind (int w)
			         (type-intersection type1 type)
	  (setq res (type-union res int))
	  (unless w (setq win nil))))))

(def-type-translator or (&rest types)
  (reduce #'type-union
	    (mapcar #'specifier-type types)
	    :initial-value *empty-type*))

;;;    We don't actually have intersection types, since the result of
;;; reasonable type intersections is always describable as a union of simple
;;; types.  If something is too hairy to fit this mold, then we make a hairy
;;; type.

(def-type-translator and (&whole spec &rest types)
  (let ((res *wild-type*))
    (dolist (type types res)
      (let ((ctype (specifier-type type)))
	  (multiple-value-bind (int win)
			           (type-intersection res ctype)
	    (unless win
	      (return (make-hairy-ctype :specifier spec)))
	    (setq res int))))))

;;;; foreign-type types


(defun %make-foreign-ctype (foreign-type)
  (%istruct 'foreign-ctype
            (type-class-or-lose 'foreign)
            nil
            foreign-type))

(defun foreign-ctype-p (x) (istruct-typep x 'foreign-ctype))
(setf (type-predicate 'foreign-ctype) 'foreign-ctype-p)

(define-type-method (foreign :unparse) (type)
  `(foreign ,(unparse-foreign-type (foreign-ctype-foreign-type type))))

(define-type-method (foreign :simple-subtypep) (type1 type2)
  (values (foreign-subtype-p (foreign-ctype-foreign-type type1)
			           (foreign-ctype-foreign-type type2))
	    t))

;(define-superclasses foreign (foreign-value))

(define-type-method (foreign :simple-=) (type1 type2)
  (let ((foreign-type-1 (foreign-ctype-foreign-type type1))
	  (foreign-type-2 (foreign-ctype-foreign-type type2)))
    (values (or (eq foreign-type-1 foreign-type-2)
		    (foreign-type-= foreign-type-1 foreign-type-2))
	      t)))

(def-type-translator foreign (&optional (foreign-type nil))
  (typecase foreign-type
    (null
     (make-foreign-ctype))
    (foreign-type
     (make-foreign-ctype foreign-type))
    (t
     (make-foreign-ctype (parse-foreign-type foreign-type)))))

(defun make-foreign-ctype (&optional foreign-type)
  (if foreign-type
      (let ((lisp-rep-type (compute-lisp-rep-type foreign-type)))
	(if lisp-rep-type
	    (specifier-type lisp-rep-type)
	    (%make-foreign-ctype foreign-type)))
      *universal-type*))


;;; CLASS-CTYPES are supposed to help integrate CLOS and the CMU type system.
;;; They mostly just contain a backpointer to the CLOS class; the CPL is then
;;;  used to resolve type relationships.

(defun class-ctype-p (x) (istruct-typep x 'class-ctype))
(setf (type-predicate 'class-ctype) 'class-ctype-p)

;;; Simple methods for TYPE= and SUBTYPEP should never be called when the two
;;; classes are equal, since there are EQ checks in those operations.
;;;
(define-type-method (class :simple-=) (type1 type2)
  (assert (not (eq type1 type2)))
  (values nil t))

(define-type-method (class :simple-subtypep) (type1 type2)
  (assert (not (eq type1 type2)))
  (let* ((class1 (if (class-ctype-p type1) (class-ctype-class type1)))
         (class2 (if (class-ctype-p type2) (class-ctype-class type2))))
    (if (and class1 class2)
      (if (memq class2 (class-direct-superclasses class1))
	(values t t)
	(if (class-has-a-forward-referenced-superclass-p class1)
	  (values nil nil)
	  (let ((supers (%inited-class-cpl class1)))
	    (if (memq class2 supers)
	      (values t t)
	      (values nil t)))))
      (values nil t))))

(define-type-method (class :simple-intersection) (type1 type2)
  (assert (not (eq type1 type2)))
  (let* ((class1 (if (class-ctype-p type1) (class-ctype-class type1)))
         (class2 (if (class-ctype-p type2) (class-ctype-class type2))))
    (if (and class1 class2)
      (cond ((subclassp class1 class2)
             (values type1 t))
            ((subclassp class2 class1)
             (values type2 t))
            (t (values
                (make-hairy-ctype :specifier `(and ,class1 ,class2)
                                  :enumerable nil)
                t)))
      (values nil t))))

(define-type-method (class :unparse) (type)
  (class-name (class-ctype-class type)))


;;; TYPE-DIFFERENCE  --  Interface
;;;
;;;    Return the type that describes all objects that are in X but not in Y.
;;; If we can't determine this type, then return NIL.
;;;
;;;    For now, we only are clever dealing with union and member types.  If
;;; either type is not a union type, then we pretend that it is a union of just
;;; one type.  What we do is remove from X all the types that are a subtype any
;;; type in Y.  If any type in X intersects with a type in Y but is not a
;;; subtype, then we give up.
;;;
;;;    We must also special-case any member type that appears in the union.  We
;;; remove from X's members all objects that are TYPEP to Y.  If Y has any
;;; members, we must be careful that none of those members are CTYPEP to any
;;; of Y's non-member types.  We give up in this case, since to compute that
;;; difference we would have to break the type from X into some collection of
;;; types that represents the type without that particular element.  This seems
;;; too hairy to be worthwhile, given its low utility.
;;;
(defun type-difference (x y)
  (let ((x-types (if (union-ctype-p x) (union-ctype-types x) (list x)))
	  (y-types (if (union-ctype-p y) (union-ctype-types y) (list y))))
    (let* ((res))
      (dolist (x-type x-types)
	  (if (member-ctype-p x-type)
	    (let* ((members))
	      (dolist (mem (member-ctype-members x-type))
		  (multiple-value-bind (val win)
				           (ctypep mem y)
		    (unless win (return-from type-difference nil))
		    (unless val
		      (push mem members))))
	      (when members
		  (push (make-member-ctype :members (nreverse members)) res)))
	    (dolist (y-type y-types (push x-type res))
	      (multiple-value-bind (val win)
				         (csubtypep x-type y-type)
		  (unless win (return-from type-difference nil))
		  (when val (return))
		  (when (types-intersect x-type y-type)
		    (return-from type-difference nil))))))
      
      (let ((y-mem (find-if #'member-ctype-p y-types)))
	  (when y-mem
	    (let ((members (member-ctype-members y-mem)))
	      (dolist (x-type x-types)
	        (unless (member-ctype-p x-type)
		    (dolist (member members)
		      (multiple-value-bind (val win)
				               (ctypep member x-type)
		        (when (or (not win) val)
		          (return-from type-difference nil)))))))))
      (setq res (nreverse res))
      (cond ((null res) *empty-type*)
	      ((null (rest res)) (first res))
	      (t
	       (make-union-ctype res))))))

;;; CTypep  --  Interface
;;;
;;;    If Type is a type that we can do a compile-time test on, then return the
;;; whether the object is of that type as the first value and second value
;;; true.  Otherwise return NIL, NIL.
;;;
;;; We give up on unknown types, pick off FUNCTION and UNION types.  For
;;; structure types, we require that the type be defined in both the current
;;; and compiler environments, and that the INCLUDES be the same.
;;;
(defun ctypep (obj type)
  (declare (type ctype type))
  (etypecase type
    ((or numeric-ctype named-ctype member-ctype array-ctype cons-ctype)
     (values (%typep obj type) t))
    (class-ctype
     (values (not (null (class-typep  obj (class-ctype-class type)))) t)
)
    (union-ctype
     (dolist (mem (union-ctype-types type) (values nil t))
       (multiple-value-bind (val win)
			          (ctypep obj mem)
	   (unless win (return (values nil nil)))
	   (when val (return (values t t))))))
    (function-ctype
     (values (functionp obj) t))
    (unknown-ctype
     (values nil nil))
#|
    (foreign-ctype
     (values (foreign-typep obj (foreign-ctype-foreign-type type)) t))
|#
    (hairy-ctype
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-ctype-specifier type))
	      (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	   (and
	    (if (atom hairy-spec)
	      (values t t)
	      (dolist (spec (cdr hairy-spec) (values t t))
		  (multiple-value-bind (res win)
				           (ctypep obj (specifier-type spec))
		    (unless win (return (values nil nil)))
		    (unless res (return (values nil t)))))))
	   (not
	    (multiple-value-bind
	      (res win)
	      (ctypep obj (specifier-type (cadr hairy-spec)))
	      (if win
		  (values (not res) t)
		  (values nil nil))))
	   (satisfies
	    (let ((fun (second hairy-spec)))
	      (cond ((and (consp fun) (eq (car fun) 'lambda))
		       (values (not (null (funcall (coerce fun 'function) obj)))
			         t))
		      ((and (symbolp fun) (fboundp fun))
		       (values (not (null (funcall fun obj))) t))
		      (t
		       (values nil nil))))))))))

;;; %TYPEP -- internal.
;;;
;;; The actual typep engine.  The compiler only generates calls to this
;;; function when it can't figure out anything more intelligent to do.
;;;
; lose 1 function call -MAYBE
(defun %typep (object specifier)
  (%%typep object
           (if (typep specifier 'ctype)
               specifier
               (specifier-type specifier))))


(defun %%typep (object type)
  ;(if (not (typep type 'ctype))(setq type (specifier-type type)))
  (locally (declare (type ctype type))
    (etypecase type
      (named-ctype
       (ecase (named-ctype-name type)
         ((* t) t)
         ((nil) nil)))
      (numeric-ctype
       (let ((pred (numeric-ctype-predicate type)))
         (if Pred
           (funcall pred object)
           (and (numberp object)
                (let ((num (if (complexp object) (realpart object) object)))
                  (ecase (numeric-ctype-class type)
                    (integer (integerp num))
                    (rational (rationalp num))
                    (float
                     (ecase (numeric-ctype-format type)
                       (short-float (typep num 'short-float))
                       (single-float (typep num 'single-float))
                       (double-float (typep num 'double-float))
                       (long-float (typep num 'long-float))
                       ((nil) (floatp num))))
                    ((nil) t)))
                (flet ((bound-test (val)
                         (let ((low (numeric-ctype-low type))
                               (high (numeric-ctype-high type)))
                           (and (cond ((null low) t)
                                      ((listp low) (> val (car low)))
                                      (t (>= val low)))
                                (cond ((null high) t)
                                      ((listp high) (< val (car high)))
                                      (t (<= val high)))))))
                  (ecase (numeric-ctype-complexp type)
                    ((nil) t)
                    (:complex
                     (and (complexp object)
                          (bound-test (realpart object))
                          (bound-test (imagpart object))))
                    (:real
                     (and (not (complexp object))
                          (bound-test object)))))))))
      (array-ctype
       (and (arrayp object)
            (ecase (array-ctype-complexp type)
              ((t) (not (typep object 'simple-array)))
              ((nil) (typep object 'simple-array))
              (* t))
            (or (eq (array-ctype-dimensions type) '*)
                (do ((want (array-ctype-dimensions type) (cdr want))
                     (got (array-dimensions object) (cdr got)))
                    ((and (null want) (null got)) t)
                  (unless (and want got
                               (or (eq (car want) '*)
                                   (= (car want) (car got))))
                    (return nil))))
            (or (eq (array-ctype-element-type type) *wild-type*)
                (type= (array-ctype-specialized-element-type type)
                       (specifier-type (array-element-type object))))))
      (member-ctype
       (if (member object (member-ctype-members type)) t))
      (class-ctype
       (class-typep object (class-ctype-class type)))
      (union-ctype
       (dolist (type (union-ctype-types type))
         (when (%%typep object type)
           (return t))))
      (cons-ctype
       (and (consp object)
            (%%typep (car object) (cons-ctype-car-ctype type))
            (%%typep (cdr object) (cons-ctype-cdr-ctype type))))
      (unknown-ctype
       ;; Parse it again to make sure it's really undefined.
       (let ((reparse (specifier-type (unknown-ctype-specifier type))))
         (if (typep reparse 'unknown-ctype)
           (error "Unknown type specifier: ~S"
                  (unknown-ctype-specifier reparse))
           (%%typep object reparse))))
      (hairy-ctype
       ;; Now the tricky stuff.
       (let* ((hairy-spec (hairy-ctype-specifier type))
              (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
         (ecase symbol
           (and
            (or (atom hairy-spec)
                (dolist (spec (cdr hairy-spec) t)
                  (unless (%%typep object (specifier-type spec))
                    (return nil)))))
           (not
            (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
              (error "Invalid type specifier: ~S" hairy-spec))
            (not (%%typep object (specifier-type (cadr hairy-spec)))))
           (satisfies
            (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
              (error "Invalid type specifier: ~S" hairy-spec))
            (let ((fn (cadr hairy-spec)))
              (if (funcall (typecase fn
                             (function fn)
                             (symbol (symbol-function fn))
                             (t
                              (coerce fn 'function)))
                           object)
                t
                nil))))))
      #|
    (foreign-ctype
     (foreign-typep object (foreign-ctype-foreign-type type)))
|#
      (function-ctype
       (error "Function types are not a legal argument to TYPEP:~%  ~S"
              (type-specifier type))))))


;;; Ctype-Of  --  Interface
;;;
;;;    Like Type-Of, only returns a Type structure instead of a type
;;; specifier.  We try to return the type most useful for type checking, rather
;;; than trying to come up with the one that the user might find most
;;; informative.
;;;

(defun ctype-of (x)
  (typecase x
    (function (specifier-type 'function))
    (symbol
     (make-member-ctype :members (list x)))
    (number
     (let* ((num (if (complexp x) (realpart x) x))
	      (res (make-numeric-ctype
		      :class (etypecase num
			         (integer 'integer)
			         (rational 'rational)
			         (float 'float))
		      :format (if (floatp num)
			          (if (typep x 'short-float)
                                    'short-float 
                                    'double-float)
			          nil))))
       (cond ((complexp x)
	        (setf (numeric-ctype-complexp res) :complex)
	        (let ((imag (imagpart x)))
		    (setf (numeric-ctype-low res) (min num imag))
		    (setf (numeric-ctype-high res) (max num imag))))
	       (t
	        (setf (numeric-ctype-low res) num)
	        (setf (numeric-ctype-high res) num)))
       res))
    (array
     (let ((etype (specifier-type (array-element-type x))))
       (make-array-ctype :dimensions (array-dimensions x)
			       :complexp (not (typep x 'simple-array))
			       :element-type etype
			       :specialized-element-type etype)))
    (t
     (%class.ctype (class-of x)))))



; These DEFTYPES should only happen while initializing.


(progn
(let-globally ((*type-system-initialized* nil))


(deftype bit () '(integer 0 1))

(deftype eql (val) `(member ,val))

(deftype signed-byte (&optional s)
  (cond ((eq s '*) 'integer)
	  ((and (integerp s) (> s 1))
	   (let ((bound (ash 1 (1- s))))
	     `(integer ,(- bound) ,(1- bound))))
	  (t
	   (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))
  
(deftype unsigned-byte (&optional s)
  (cond ((eq s '*) '(integer 0))
	((and (integerp s) (> s 0))
	 `(integer 0 ,(1- (ash 1 s))))
	(t
	 (error "Bad size specified for UNSIGNED-BYTE type specifier: ~S." s))))

(deftype vector (&optional element-type size)
  `(array ,element-type (,size)))

(deftype simple-vector (&optional size)
  `(simple-array t (,size)))

(deftype base-string (&optional size)
  `(array base-char (,size)))
(deftype simple-base-string (&optional size)
  `(simple-array base-char (,size)))



(deftype string (&optional size)
  `(base-string ,size))

(deftype simple-string (&optional size)
  `(simple-base-string ,size))

(deftype extended-string (&optional size)
  (declare (ignore size))
  'nil)

(deftype simple-extended-string (&optional size)
  (declare (ignore size))
  'nil)

(deftype bit-vector (&optional size)
  `(array bit (,size)))

(deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))

; TYPE-OF sometimes returns random symbols that aren't really type specifiers.

(deftype simple-unsigned-word-vector (&optional size)
  `(simple-array (unsigned-byte 16) (,size)))

(deftype simple-unsigned-byte-vector (&optional size)
  `(simple-array (unsigned-byte 8) (,size)))

(deftype simple-unsigned-long-vector (&optional size)
  `(simple-array (unsigned-byte 32) (,size)))

(deftype simple-signed-word-vector (&optional size)
  `(simple-array (signed-byte 16) (,size)))

(deftype simple-signed-byte-vector (&optional size)
  `(simple-array (signed-byte 8) (,size)))

(deftype simple-signed-long-vector (&optional size)
  `(simple-array (signed-byte 32) (,size)))

(deftype simple-double-float-vector (&optional size)
  `(simple-array double-float (,size)))

(deftype simple-short-float-vector (&optional size)
  `(simple-array short-float (,size)))

(deftype unsigned-word-vector (&optional size)
  `(vector (unsigned-byte 16) ,size))

(deftype single-float-vector (&optional size)
  `(vector short-float ,size))

(deftype unsigned-byte-vector (&optional size)
  `(vector (unsigned-byte 8) ,size))

(deftype unsigned-long-vector (&optional size)
  `(vector (unsigned-byte 32) ,size))

(deftype long-float-vector (&optional size)
  `(vector double-float ,size))

(deftype long-vector (&optional size)
  `(vector (signed-byte 32) ,size))

(deftype double-float-vector (&optional size)
  `(vector double-float ,size))

(deftype byte-vector (&optional size)
  `(vector (signed-byte 8) ,size))

(deftype general-vector (&optional size)
  `(vector t ,size))

(deftype word-vector (&optional size)
  `(vector (signed-byte 16) ,size))

(deftype short-float-vector (&optional size)
  `(vector single-float ,size))

(deftype simple-1d-array (&optional size)
  `(simple-array * (,size)))

(deftype simple-long-vector (&optional size)
  `(simple-array (signed-byte 32) (,size)))

(deftype simple-word-vector (&optional size)
  `(simple-array (signed-byte 16) (,size)))

(deftype simple-short-float-vector (&optional size)
  `(simple-array single-float (,size)))

(deftype simple-byte-vector (&optional size)
  `(simple-array (signed-byte 8) (,size)))

(deftype simple-double-float-vector (&optional size)
  `(simple-array double-float (,size)))

(deftype simple-single-float-vector (&optional size)
  `(simple-array single-float (,size)))

)

(let* ((builtin-translations 
        `((array . array)
          (simple-array . simple-array)
          (cons . cons)
          (vector . vector)
          (null . (member nil))
          (list . (or cons null))
          (sequence . (or list vector))
          (simple-vector . simple-vector)
          (bit-vector . bit-vector)
          (simple-bit-vector . simple-bit-vector)
          (simple-string . simple-string)
          (simple-base-string . simple-base-string)
          (string . string)
          (base-string . base-string)
          (real . real)
          (complex . complex)
          (float . float)
          (double-float . double-float)
          (long-float . double-float)
          (short-float . short-float)
          (single-float . short-float)
          (rational . rational)   ; why not (or ratio integer)?
          (ratio . (and rational (not integer))) ; why not ratio
          (integer . integer)
          (fixnum . (integer ,most-negative-fixnum ,most-positive-fixnum))
          (bignum . (or (integer * (,most-negative-fixnum))
                         (integer (,most-positive-fixnum) *)))
          
          )))
  (dolist (spec builtin-translations)
    (setf (info-type-kind (car spec)) :primitive
          (info-type-builtin (car spec)) (specifier-type (cdr spec)))))

       
(precompute-types '((mod 2) (mod 4) (mod 16) (mod #x100) (mod #x10000)
		    (mod #x100000000)
		    (unsigned-byte 1) 
		    (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
		    (signed-byte 8) (signed-byte 16) (signed-byte 32)
                    ;would be nice to add these on demand vs ad hoc ly
                    ; and isn't or function symbol = or symbol function
                    (or function symbol)
                    (NOT NUMBER)
                    (AND (NOT NUMBER) (NOT MACPTR))
                    (OR FIXNUM CHARACTER SYMBOL (AND (NOT NUMBER) (NOT MACPTR)))))

(precompute-types *cl-types*)
(defparameter *null-type* (specifier-type 'null))


(flet ((set-builtin-class-type-translation (thing)
         (let* ((class-name (if (atom thing) thing (car thing)))
                (spec (if (atom thing) thing (cadr thing)))
                (spectype (specifier-type spec)))
           (when (typep spectype 'class-ctype)
             (dbg class-name))
           (setf (class-ctype-translation
                  (%class.ctype (find-class class-name))) spectype))))
  (mapc #'set-builtin-class-type-translation
        '(
          ;; Root Of All Evil
          t
          ;; Numbers:
          number real ratio complex rational fixnum
          ;;  Integers:
          signed-byte  unsigned-byte bit bignum integer
          ;;  Floats
           float  double-float single-float
          ;; Arrays
          array
          ;;  Simple Arrays
          simple-array
          ;;  Vectors
          vector string base-string bit-vector
          unsigned-byte-vector unsigned-word-vector unsigned-long-vector
          byte-vector word-vector long-vector
          single-float-vector double-float-vector
          general-vector
          ;;   Simple 1-Dimensional Arrays
          simple-1d-array  simple-string simple-base-string simple-bit-vector
          simple-unsigned-byte-vector
          simple-unsigned-long-vector
          simple-unsigned-word-vector
          simple-byte-vector
          simple-word-vector
          simple-long-vector 
          simple-single-float-vector 
          simple-double-float-vector
          simple-vector
          ;; Sequence types
          sequence list  cons null
          
 )
                                                         
        ))
)
;(setq *type-system-initialized* t)




; These deftypes help the CMUCL compiler; the type system doesn't depend on them.

;;; Since OpenMCL's DEFTYPE tries to globally define the type
;;; at compile-time as well as load- and execute time, hide
;;; the definition of these "built-in" types.  (It'd be cleaner
;;; to make DEFTYPE do something saner at compile-time.)
(let* ()                                ; make the following be non-toplevel
(deftype boolean () '(member t nil))

(deftype atom () '(not cons))
;;;
;;; A type specifier.
(deftype type-specifier () '(or list symbol class))
;;;
;;; An index into an array.   Also used for sequence index. 
(deftype index () `(integer 0 (,array-dimension-limit)))
;;;
;;; Array rank, total size...
(deftype array-rank () `(integer 0 (,array-rank-limit)))
(deftype array-total-size () `(integer 0 (,array-total-size-limit)))
;;;
;;; Some thing legal in an evaluated context.
(deftype form () t)
;;;
;;; Maclisp compatibility...
(deftype stringlike () '(or string symbol))
(deftype stringable () '(or string symbol character))
;;;
;;; Save a little typing...
(deftype truth () '(member t))
;;;
;;; A thing legal in places where we want the name of a file.
(deftype filename () '(or string pathname))
;;;
;;; A legal arg to pathname functions.
(deftype pathnamelike () '(or string pathname stream))
;;;
;;; A thing returned by the irrational functions.  We assume that they never
;;; compute a rational result.
(deftype irrational () '(or float (complex float)))
;;;
;;; Character components:
(deftype char-code () `(integer 0 (,char-code-limit)))
;;;
;;; A consed sequence result.  If a vector, is a simple array.
(deftype consed-sequence () '(or list (simple-array * (*))))
;;;
;;; The :end arg to a sequence...
(deftype sequence-end () '(or null index))
;;;
;;; A valid argument to a stream function...
(deftype streamlike () '(or stream (member nil t)))
;;;
;;; A thing that can be passed to funcall & friends.
(deftype callable () '(or function symbol))

;;; Until we decide if and how to wedge this into the type system, make it
;;; equivalent to t.
;;;
(deftype void () t)
;;;
;;; An index into an integer.
(deftype bit-index () `(integer 0 ,most-positive-fixnum))
;;;
;;; Offset argument to Ash (a signed bit index).
(deftype ash-index () 'fixnum)

;;; Not sure how to do this without SATISFIES.
(deftype setf-function-name () `(satisfies setf-function-name-p))

;;; Better than nothing, arguably.
(deftype function-name () `(or symbol setf-function-name))

)                                       ; end of LET* sleaze

(defun array-or-union-ctype-element-type (ctype)
  (if (typep ctype 'array-ctype)
    (type-specifier (array-ctype-element-type ctype))
    (if (typep ctype 'union-ctype)
      `(or ,@(mapcar #'array-or-union-ctype-element-type 
                     (union-ctype-types ctype))))))


;;; Ensure that standard EFFECTIVE-SLOT-DEFINITIONs have a meaningful
;;; type predicate, if we can.
(defmethod shared-initialize :after ((spec effective-slot-definition)
				     slot-names
				     &key type
				     &allow-other-keys)
  (declare (ignore slot-names))
  (unless (eq type t)
    (setf (slot-value spec 'type-predicate)
	  (or (and (typep type 'symbol)
		   (type-predicate type))
              (handler-case
                  (let* ((ctype (specifier-type type)))
                    #'(lambda (value) (%%typep value ctype)))
                (parse-unknown-type (c)
                  (declare (ignore c))
                  #'(lambda (value)
                      ;; If the type's now known, install a new predicate.
                      (let* ((nowctype (specifier-type type)))
                        (unless (typep nowctype 'unknown-ctype)
                          (setf (slot-value spec 'type-predicate)
                                #'(lambda (value) (%%typep value nowctype))))
                        (%%typep value nowctype)))))))))

