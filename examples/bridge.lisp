;;;; -*- Mode: Lisp; Package: CCL -*-
;;;; bridge.lisp
;;;;
;;;; A Lisp bridge for Cocoa
;;;;
;;;; This provides:
;;;;   (1) Convenient Lisp syntax for instantiating ObjC classes
;;;;   (2) Convenient Lisp syntax for invoking ObjC methods
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
                  (message-info (need-objc-message-info f))
		  (args (rest args))
		  (nargs (length args)))
	     (cond ((and (= nargs 1)
                         (getf (objc-message-info-flags message-info)
                               :accepts-varargs))
		    (values f nil l))
		   ((= nargs n) (values f args nil))
		   ((= nargs (1+ n)) (values f (butlast args) l))
		   (t (error "Improperly formatted argument list: ~S" args)))))
	  ((keywordp f)
	   ;; (KEY1 ARG1 ... KEYN ARGN {LIST}) or (KEY LIST)
	   (let ((nargs (length args)))
	     (cond ((and (= nargs 2) (consp l)
                         (let* ((info (need-objc-message-info
                                       (lisp-to-objc-message (list f)))))
                           (getf (objc-message-info-flags info)
                                 :accepts-varargs)))
		    (values (lisp-to-objc-message (list f)) nil l))
		   ((evenp nargs)
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
       (find-class cname)
     (load-objc-class-descriptor cname))))


;;; Return the class object of an ObjC object O, signalling an error
;;; if O is not an ObjC object
                      
(defun objc-class-of (o)
  (if (objc-object-p o)
      (class-of o)
    (progn
      #+debug
      (#_NSLog #@"class name = %s" :address (pref (pref o :objc_object.isa)
                                                  :objc_class.name))
      (error "~S is not an ObjC object" o))))


;;; Returns the ObjC class corresponding to the declared type OTYPE if
;;; possible, NIL otherwise 

(defun get-objc-class-from-declaration (otype)
  (cond ((symbolp otype) (lookup-objc-class (lisp-to-objc-classname otype)))
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


;;; For some reason, these types sometimes show up as :STRUCTs even though they
;;; are not structure tags, but type names

(defun fudge-objc-type (ftype)
  (if (equal ftype '(:STRUCT :<NSD>ecimal))
      :<NSD>ecimal
    ftype))


;;; Returns T if the result spec requires a STRET for its return, NIL otherwise
;;; RSPEC may be either a number (in which case it is interpreted as a number
;;; of words) or a foreign type spec acceptable to PARSE-FOREIGN-TYPE. STRETS
;;; must be used when a structure larger than 4 bytes is returned

(defun requires-stret-p (rspec)
  (when (member rspec '(:DOUBLE-FLOAT :UNSIGNED-DOUBLEWORD :SIGNED-DOUBLEWORD) 
		:test #'eq)
    (return-from requires-stret-p nil))
  (setq rspec (fudge-objc-type rspec))
  (if (numberp rspec) 
    (> rspec 1)
    (> (ensure-foreign-type-bits (parse-foreign-type rspec)) target::nbits-in-word)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Stret Convenience Stuff                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Allocate any temporary storage necessary to hold strets required
;;; AT TOPLEVEL in the value forms.  Special recognition is given to
;;; SENDs involving strets and to stret pseudo-functions
;;; NS-MAKE-POINT, NS-MAKE-RANGE, NS-MAKE-RECT and NS-MAKE-SIZE

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
       (let* ((info (get-objc-message-info (parse-message (cddr form)))))
         (if (null info)
           (error "Can't determine message being sent in ~s" form))
         (let* ((rtype (objc-method-info-result-type
                        (car (objc-message-info-methods info)))))
           (if (getf (objc-message-info-flags info) :returns-structure)
             (values `(,var ,(unparse-foreign-type rtype))
                     `(send/stret ,var ,@(rest form)))
             (if errorp
               (error "NonSTRET SEND in ~S" form)
               form)))))
      (send-super
       (let* ((info (get-objc-message-info (parse-message (cddr form)))))
         (if (null info)
           (error "Can't determine message being sent in ~s" form))
         (let* ((rtype (objc-method-info-result-type
                        (car (objc-message-info-methods info)))))
           (if (getf (objc-message-info-flags info) :returns-structure)
             (values `(,var ,(unparse-foreign-type rtype))
                     `(send-super/stret ,var ,@(rest form)))
             (if errorp
               (error "NonSTRET SEND-SUPER in ~S" form)
               form)))))
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



(defvar *objc-message-info* (make-hash-table :test #'equal :size 500))

(defun result-type-requires-structure-return (result-type)
  ;; Use objc-msg-send-stret for all methods that return
  ;; record types.
  (typep result-type 'foreign-record-type))

(defun postprocess-objc-message-info (message-info)
  (flet ((reduce-to-ffi-type (ftype)
           (if (objc-id-type-p ftype)
             :id
             (unparse-foreign-type ftype))))
    (flet ((ensure-method-signature (m)
             (or (objc-method-info-signature m)
                 (setf (objc-method-info-signature m)
                       (cons (reduce-to-ffi-type
                              (objc-method-info-result-type m))
                             (mapcar #'reduce-to-ffi-type
                                     (objc-method-info-arglist m)))))))
      (let* ((methods (objc-message-info-methods message-info))
             (signatures ())
             (protocol-methods)
             (signature-alist ()))
        (dolist (m methods)
          (let* ((signature (ensure-method-signature m)))
            (pushnew signature signatures :test #'equal)
            (if (getf (objc-method-info-flags m) :protocol)
              (push m protocol-methods)
              (let* ((pair (assoc signature signature-alist :test #'equal)))
                (if pair
                  (push m (cdr pair))
                  (push (cons signature (list m)) signature-alist))))))
        (setf (objc-message-info-ambiguous-methods message-info)
              (mapcar #'cdr
                      (sort signature-alist
                            #'(lambda (x y)
                                (< (length (cdr x))
                                   (length (cdr y)))))))
        (setf (objc-message-info-flags message-info) nil)
        (setf (objc-message-info-protocol-methods message-info)
              protocol-methods)
        (when (cdr signatures)
          (setf (getf (objc-message-info-flags message-info) :ambiguous) t))
        (let* ((first-method (car methods))
               (first-sig (objc-method-info-signature first-method))
               (first-sig-len (length first-sig)))
          (setf (objc-message-info-req-args message-info)
                (1- first-sig-len))
          ;; Whether some arg/result types vary or not, we want to insist
          ;; on (a) either no methods take a variable number of arguments,
          ;; or all do, and (b) either no method uses structure-return
          ;; conventions, or all do. (It's not clear that these restrictions
          ;; are entirely reasonable in the long run; in the short term,
          ;; they'll help get things working.)
          (flet ((method-returns-structure (m)
                   (result-type-requires-structure-return
                    (objc-method-info-result-type m)))
                 (method-accepts-varargs (m)
                   (eq (car (last (objc-method-info-arglist m)))
                       *void-foreign-type*)))
            (let* ((first-result-is-structure (method-returns-structure first-method))
                   (first-accepts-varargs (method-accepts-varargs first-method)))
              (if (dolist (m (cdr methods) t)
                    (unless (eq (method-returns-structure m)
                                first-result-is-structure)
                      (return nil)))
                (if first-result-is-structure
                  (setf (getf (objc-message-info-flags message-info)
                              :returns-structure) t)))
              (if (dolist (m (cdr methods) t)
                    (unless (eq (method-accepts-varargs m)
                                first-accepts-varargs)
                      (return nil)))
                (if first-accepts-varargs
                  (progn
                    (setf (getf (objc-message-info-flags message-info)
                                :accepts-varargs) t)
                    (decf (objc-message-info-req-args message-info))))))))))))
          
;;; -may- need to invalidate cached info whenever new interface files
;;; are made accessible.  Probably the right thing to do is to insist
;;; that (known) message signatures be updated in that case.
(defun get-objc-message-info (message-name)
  (or (gethash message-name *objc-message-info*)
      (let* ((info (lookup-objc-message-info message-name)))
        (when info
          (setf (gethash message-name *objc-message-info*) info)
          (postprocess-objc-message-info info)
          info))))

(defun need-objc-message-info (message-name)
  (or (get-objc-message-info message-name)
      (error "Undeclared message: ~s" message-name)))

;;; Should be called after using new interfaces that may define
;;; new methods on existing messages.
(defun update-objc-method-info ()
  (maphash #'(lambda (message-name info)
               (lookup-objc-message-info message-name info)
               (postprocess-objc-message-info info))
           *objc-message-info*)
  ;; Update info about init messages.
  (register-objc-init-messages))


;;; Of the method declarations (OBJC-METHOD-INFO structures) associated
;;; with the message-declaration (OBJC-MESSAGE-INFO structure) M,
;;; return the one that seems to be applicable for the object O.
;;; (If there's no ambiguity among the declare methods, any method
;;; will do; this just tells runtime %SEND functions how to compose
;;; an %FF-CALL).
(defun %lookup-objc-method-info (m o)
  (let* ((methods (objc-message-info-methods m))
         (ambiguous (getf (objc-message-info-flags m) :ambiguous)))
    (if (not ambiguous)
      (car methods)
      (or 
       (dolist (method methods)
         (let* ((mclass (get-objc-method-info-class method)))
           (if (typep o mclass)
             (return method))))
       (error "Can't determine ObjC method type signature for message ~s, object ~s" (objc-message-info-message-name m) o)))))

(defun %declare-objc-method (message-name class-name class-p result-type args)
  (let* ((info (get-objc-message-info message-name)))
    (unless info
      (setq info (make-objc-message-info :message-name message-name))
      (setf (gethash message-name *objc-message-info*) info))
    (let* ((was-ambiguous (getf (objc-message-info-flags info) :ambiguous))
           (method-info (make-objc-method-info :message-info info
                                               :class-name class-name
                                               :result-type result-type
                                               :arglist args
                                               :flags (if class-p '(:class t)))))
      (push method-info (objc-message-info-methods info))
      (postprocess-objc-message-info info)
      (if (and (getf (objc-message-info-flags info) :ambiguous)
               (not was-ambiguous))
        (warn "previously declared methods on ~s all had the same type signature, but ~s introduces ambiguity" message-name method-info))
      info)))



;;; TRANSLATE-FOREIGN-ARG-TYPE doesn't accept :VOID

(defun translate-foreign-result-type (ftype)
  (ensure-foreign-type-bits (parse-foreign-type ftype))
  (if (eq ftype :void)
    :void
    (translate-foreign-arg-type ftype)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Invoking ObjC Methods                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The SEND and SEND/STRET macros

(defmacro send (o msg &rest args &environment env)
  (make-optimized-send o msg args env))

(defmacro send/stret (s o msg &rest args &environment env)
  (make-optimized-send o msg args env s))




;;; Optimize special cases of SEND and SEND/STRET

(defun make-optimized-send (o msg args env  &optional s super sclassname)
  (multiple-value-bind (msg args vargs) (parse-message (cons msg args))
    (let* ((message-info (get-objc-message-info msg)))
      (if (null message-info)
        (error "Unknown message: ~S" msg))
      ;; If a vararg exists, make sure that the message can accept it
      (when (and vargs (not (getf (objc-message-info-flags message-info)
                                  :accepts-varargs)))
        (error "Message ~S cannot accept a variable number of arguments" msg))
      (unless (= (length args) (objc-message-info-req-args message-info))
        (error "Message ~S requires ~a ~d args, but ~d were provided."
               (if vargs "at least" "exactly")
               (objc-message-info-req-args message-info)
               (length args)))
      (multiple-value-bind (args svarforms sinitforms) (sletify-message-args args)
        (let* ((ambiguous (getf (objc-message-info-flags message-info) :ambiguous))
               (methods (objc-message-info-methods message-info))
               (method (if (not ambiguous) (car methods))))
          (when ambiguous
            (let* ((class (if sclassname 
                            (find-objc-class sclassname)
                            (get-objc-class-from-declaration (declared-type o env)))))
              (if class
                (dolist (m methods)
                  (unless (getf (objc-method-info-flags m) :protocol)
                    (let* ((mclass (or (get-objc-method-info-class m)
                                       (error "Can't find ObjC class named ~s"
                                              (objc-method-info-class-name m)))))
                      (when (and class (subtypep class mclass))
                        (return (setq method m)))))))))
          (if method
            (build-call-from-method-info method
                                         args
                                         vargs
                                         o
                                         msg
                                         svarforms
                                         sinitforms
                                         s
                                         super)
            (build-ambiguous-send-form message-info
                                       args
                                       vargs
                                       o
                                       msg
                                       svarforms
                                       sinitforms
                                       s
                                       super)))))))

    
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
                   (objc-message-send-stret ,s ,o ,(cadr sel)
                    ,@(append (butlast argspecs) (list :void)))
                   ,s)
                ;; Super stret send
                `(progn
                   (objc-message-send-super-stret ,s ,super ,(cadr sel)
                    ,@(append (butlast argspecs) (list :void)))
                   ,s)))
            (if (null s)
              ;; STRET not required and not provided, use send
              (if (null super)
                ;; Regular send
                (if (eq rspec :<BOOL>)
                  `(coerce-from-bool
                    (objc-message-send ,o ,(cadr sel) ,@argspecs))
                  `(objc-message-send ,o ,(cadr sel) ,@argspecs))
                ;; Super send
                (if (eq rspec :<BOOL>)
                  `(coerce-from-bool
                    (objc-message-send-super ,super ,(cadr sel) ,@argspecs))
                  `(objc-message-send-super ,super ,(cadr sel) ,@argspecs)))
              ;; STRET not required but provided
              (error "The message ~S must be sent using SEND" msg)))))))

(defun objc-id-type-p (foreign-type)
  (and (typep foreign-type 'foreign-pointer-type)
       (let* ((to (foreign-pointer-type-to foreign-type)))
         (and (typep to 'foreign-record-type)
              (eq :struct (foreign-record-type-kind to))
              (not (null (progn (ensure-foreign-type-bits to) (foreign-record-type-fields to))))
              (let* ((target (foreign-record-field-type (car (foreign-record-type-fields to)))))
                (and (typep target 'foreign-pointer-type)
                     (let* ((target-to (foreign-pointer-type-to target)))
                       (and (typep target-to 'foreign-record-type)
                            (eq :struct (foreign-record-type-kind target-to))
                            (eq :objc_class (foreign-record-type-name target-to))))))))))

(defun unique-objc-classes-in-method-info-list (method-info-list)
  (if (cdr method-info-list)                     ; if more than 1 class
    (flet ((subclass-of-some-other-class (c)
             (let* ((c-class (get-objc-method-info-class c)))
               (dolist (other method-info-list)
                 (unless (eq other c)
                   (when (subtypep c-class (get-objc-method-info-class other))
                   (return t)))))))
      (remove-if #'subclass-of-some-other-class method-info-list))
    method-info-list))
  
(defun get-objc-method-info-class (method-info)
  (or (objc-method-info-class-pointer method-info)
      (setf (objc-method-info-class-pointer method-info)
            (let* ((c (lookup-objc-class (objc-method-info-class-name method-info) nil)))
              (when c
                (let* ((meta-p (getf (objc-method-info-flags method-info) :class)))
                  (if meta-p
                    (with-macptrs ((m (pref c :objc_class.isa)))
                      (canonicalize-registered-metaclass m))
                    (canonicalize-registered-class c))))))))

;;; Generate some sort of CASE or COND to handle an ambiguous message
;;; send (where the signature of the FF-CALL depends on the type of the
;;; receiver.)
;;; AMBIGUOUS-METHODS is a list of lists of OBJC-METHOD-INFO structures,
;;; where the methods in each sublist share the same type signature.  It's
;;; sorted so that more unique method/signature combinations appear first
;;; (and are easier to special-case via TYPECASE.)
(defun build-send-case (ambiguous-methods
                        args
                        vargs
                        receiver
                        msg
                        s
                        super
                        protocol-methods)
  (flet ((method-class-name (m)
           (let* ((mclass (get-objc-method-info-class m)))
             (unless mclass
               (error "Can't find class with ObjC name ~s"
                      (objc-method-info-class-name m)))
             (class-name mclass))))
    (collect ((clauses))
      (let* ((protocol (gensym)))
        (dolist (method protocol-methods)
          (let* ((protocol-name (objc-method-info-class-name method)))
            (clauses `((let* ((,protocol (lookup-objc-protocol ,protocol-name)))
                         (and ,protocol
                              (not (zerop (objc-message-send ,receiver
                                                             "conformsToProtocol:"
                                                             :address ,protocol
                                                             :<BOOL>)))))
                       ,(build-internal-call-from-method-info
                         method args vargs receiver msg s super))))))
      (do* ((methods ambiguous-methods (cdr methods)))
           ((null (cdr methods))
            (when ambiguous-methods
            (clauses `(t
                       ,(build-internal-call-from-method-info
                         (caar methods) args vargs receiver msg s super)))))
        (clauses `(,(if (cdar methods)
                        `(or ,@(mapcar #'(lambda (m)
                                           `(typep ,receiver
                                             ',(method-class-name m)))
                                       (unique-objc-classes-in-method-info-list
                                        (car methods))))
                        `(typep ,receiver ',(method-class-name (caar methods))))
                   ,(build-internal-call-from-method-info
                     (caar methods) args vargs receiver msg s super))))
      `(cond
        ,@(clauses)))))

(defun build-ambiguous-send-form (message-info args vargs o msg svarforms sinitforms s super)
  (let* ((receiver (gensym))
         (caseform (build-send-case
                    (objc-message-info-ambiguous-methods message-info)
                    args
                    vargs
                    receiver
                    msg
                    s
                    super
                    (objc-message-info-protocol-methods message-info))))
    `(with-ns-exceptions-as-errors
      (rlet ,svarforms
        ,@sinitforms
        (let* ((,receiver ,o))
          ,caseform)))))


;;; Generate the "internal" part of a method call; the "external" part
;;; has established ObjC exception handling and handled structure-return
;;  details
(defun build-internal-call-from-method-info (method-info args vargs o msg s super)
  (let* ((arglist ()))
    (collect ((specs))
      (do* ((args args (cdr args))
            (argtypes (objc-method-info-arglist method-info) (cdr argtypes))
            (reptypes (cdr (objc-method-info-signature method-info)) (cdr reptypes)))
           ((null args) (setq arglist (append (specs) vargs)))
        (let* ((reptype (if (objc-id-type-p (car argtypes)) :id (car reptypes)))
               (arg (car args)))
          (specs reptype)
          (specs arg)))
      ;;(break "~& arglist = ~s" arglist)
      (if (result-type-requires-structure-return
           (objc-method-info-result-type method-info))
        (if (null s)
          ;; STRET required but not provided
          (error "The message ~S must be sent using SEND/STRET" msg)
          (if (null super)
            `(objc-message-send-stret ,s ,o ,msg ,@arglist :void)
            `(objc-message-send-super-stret ,s ,super ,msg ,@arglist :void)))
        (if s
          ;; STRET provided but not required
          (error "The message ~S must be sent using SEND" msg)
          (let* ((result-spec (car (objc-method-info-signature method-info)))
                 (form (if super
                         `(objc-message-send-super ,super ,msg ,@arglist ,result-spec)
                         `(objc-message-send ,o ,msg ,@arglist ,result-spec))))
            form))))))
  
(defun build-call-from-method-info (method-info args vargs o  msg  svarforms sinitforms s super)
  `(with-ns-exceptions-as-errors
    (rlet ,svarforms
      ,@sinitforms
      ,(build-internal-call-from-method-info
        method-info
        args
        vargs
        o
        msg
        s
        super))))

 

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
    (send-objc-init-message (send (find-objc-class cname) 'alloc)
                            ks
                            vs)))

;;; Provide the BRIDGE module

(provide "BRIDGE")
