;;;-*-Mode: LISP; Package: CCL -*-
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

;;; Macros (and functions/constants used at macroexpand-time) ONLY.

(in-package :ccl)

(eval-when (eval compile)
  (require "LEVEL-2")
  (require "BACKQUOTE")
  (require "DEFSTRUCT-MACROS"))

;; Constants

(defmacro defconstant (sym val &optional (doc () doc-p) &environment env)
  (setq sym (require-type sym 'symbol)
        doc (if doc-p (require-type doc 'string)))
  `(progn
     (eval-when (:compile-toplevel)
       (define-compile-time-constant ',sym ',val ,env))
     (eval-when (:load-toplevel :execute)
       (%defconstant ',sym ,val ,@(if doc-p (list doc))))))

;; Lists

(defmacro %car (x)
  `(car (the cons ,x)))

(defmacro %cdr (x)
  `(cdr (the cons ,x)))

(defmacro %caar (x)
 `(%car (%car ,x)))

(defmacro %cadr (x)
 `(%car (%cdr ,x)))

(defmacro %cdar (x)
 `(%cdr (%car ,x)))

(defmacro %cddr (x)
 `(%cdr (%cdr ,x)))

(defmacro %caaar (x)
 `(%car (%car (%car ,x))))

(defmacro %caadr (x)
 `(%car (%car (%cdr ,x))))

(defmacro %cadar (x)
 `(%car (%cdr (%car ,x))))

(defmacro %caddr (x)
 `(%car (%cdr (%cdr ,x))))

(defmacro %cdaar (x)
 `(%cdr (%car (%car ,x))))

(defmacro %cdadr (x)
 `(%cdr (%car (%cdr ,x))))

(defmacro %cddar (x)
 `(%cdr (%cdr (%car ,x))))

(defmacro %cdddr (x)
 `(%cdr (%cdr (%cdr ,x))))

(defmacro %rplaca (x y)
  `(rplaca (the cons ,x) ,y))

(defmacro %rplacd (x y)
  `(rplacd (the cons ,x) ,y))

; These are open-coded by the compiler to isolate platform
; dependencies.

(defmacro %unbound-marker-8 ()
  `(%unbound-marker))

(defmacro %slot-missing-marker ()
  `(%illegal-marker))




(defmacro %null-ptr () '(%int-to-ptr 0))

;;;Assorted useful macro definitions

(defmacro def-accessors (ref &rest names)
  (define-accessors ref names))

(defmacro def-accessor-macros (ref &rest names)
  (define-accessors ref names t))

(defun define-accessors (ref names &optional no-constants
                             &aux (arg (gensym)) (index 0) progn types)
  (when (listp ref)
    (setq types ref
          ref (pop names)))
  (dolist (name names)
    (when name
      (unless (listp name) (setq name (list name)))
      (dolist (sym name)
        (when sym
          (push `(defmacro ,sym (,arg) (list ',ref ,arg ,index)) progn)
          (unless no-constants
	    (push `(defconstant ,sym ,index) progn)))))
    (setq index (1+ index)))
 `(progn
    ,.(nreverse progn)
    ,@(if types `((add-accessor-types ',types ',names)))
    ,index))

(defmacro specialv (var)
  `(locally (declare (special ,var)) ,var))


(defmacro prog1 (valform &rest otherforms)
 (let ((val (gensym)))
 `(let ((,val ,valform))
   ,@otherforms
   ,val)))

(defmacro prog2 (first second &rest others)
 `(progn ,first (prog1 ,second ,@others)))

(defmacro prog (inits &body body &environment env)
  (multiple-value-bind (forms decls) (parse-body body env nil)
    `(block nil
       (let ,inits
         ,@decls
         (tagbody ,@forms)))))

(defmacro prog* (inits &body body &environment env)
  (multiple-value-bind (forms decls) (parse-body body env nil)
    `(block nil
       (let* ,inits
         ,@decls
         (tagbody ,@forms)))))


(defmacro %stack-block ((&rest specs) &body forms &aux vars lets)
  (dolist (spec specs)
    (destructuring-bind (var ptr &key clear) spec
      (push var vars)
      (push `(,var (%new-ptr ,ptr ,clear)) lets)))
  `(let* ,(nreverse lets)
     (declare (dynamic-extent ,@vars))
     (declare (type macptr ,@vars))
     (declare (unsettable ,@vars))
     ,@forms))

(defmacro %vstack-block (spec &body forms)
  `(%stack-block (,spec) ,@forms))

(defmacro dolist ((varsym list &optional ret) &body body &environment env)
  (if (not (symbolp varsym)) (signal-program-error $XNotSym varsym))
  (let* ((toplab (gensym))
         (tstlab (gensym))
         (lstsym (gensym)))
    (multiple-value-bind (forms decls) (parse-body body env nil)
     `(block nil
       (let* ((,lstsym ,list) ,varsym)
        ,@decls
          (tagbody
            (go ,tstlab)
            ,toplab
            (setq ,lstsym (cdr (the list ,lstsym)))
            ,@forms
            ,tstlab
            (setq ,varsym (car ,lstsym))
            (if ,lstsym (go ,toplab)))
          ,@(if ret `((progn  ,ret))))))))


(defmacro dovector ((varsym vector &optional ret) &body body &environment env)
  (if (not (symbolp varsym))(signal-program-error $XNotSym varsym))
  (let* ((toplab (gensym))
         (tstlab (gensym))
         (lengthsym (gensym))
         (indexsym (gensym))
         (vecsym (gensym)))
    (multiple-value-bind (forms decls) (parse-body body env nil)
     `(let* ((,vecsym ,vector)
             (,lengthsym (length ,vecsym))
             (,indexsym 0)
             ,varsym)
        ,@decls
        ,@(let ((type (nx-form-type vector env)))
            (unless (eq type t)
              `((declare (type ,type ,vecsym)))))
        (block nil
          (tagbody
            (go ,tstlab)
            ,toplab
            (setq ,varsym (locally (declare (optimize (speed 3) (safety 0)))
                            (aref ,vecsym ,indexsym))
                  ,indexsym (%i+ ,indexsym 1))
            ,@forms
            ,tstlab
            (if (%i< ,indexsym ,lengthsym) (go ,toplab)))
          ,@(if ret `((progn (setq ,varsym nil) ,ret))))))))

(defmacro report-bad-arg (&rest args)
  `(values (%badarg ,@args)))

(defmacro %cons-restart (name action report interactive test)
 `(gvector :istruct 'restart ,name ,action ,report ,interactive ,test))

(defmacro restart-bind (clauses &body body)
  (let* ((restarts (mapcar #'(lambda (clause) 
                               (list (make-symbol (symbol-name (require-type (car clause) 'symbol)))
                                     `(%cons-restart nil nil nil nil nil)))
                           clauses))
         (bindings (mapcar #'(lambda (clause name)
                              `(make-restart ,(car name) ',(car clause)
                                             ,@(cdr clause)))
                           clauses restarts))
        (cluster (gensym)))
    `(let* (,@restarts)
       (declare (dynamic-extent ,@(mapcar #'car restarts)))
       (let* ((,cluster (list ,@bindings))
              (%restarts% (cons ,cluster %restarts%)))
         (declare (dynamic-extent ,cluster %restarts%))
         (progn
           ,@body)))))

(defmacro handler-bind (clauses &body body)
  (let* ((fns)
         (decls)         
         (bindings (mapcan #'(lambda (clause)
                               (destructuring-bind (condition handler) clause
                                 (if (and (consp handler)(eq (car handler) 'function)
                                          (consp (cadr handler))(eq (car (cadr handler)) 'lambda))
                                   (let ((fn (gensym)))
                                     (push `(,fn ,handler) fns)
                                     (push `(declare (dynamic-extent ,fn)) decls)
                                     `(',condition ,fn))
                                   (list `',condition
                                         `,handler))))
                           clauses))
        (cluster (gensym)))    
    `(let* (,@fns
            (,cluster (list ,@bindings))
            (%handlers% (cons ,cluster %handlers%)))
       (declare (dynamic-extent ,cluster %handlers%))
       ,@decls
       (progn
         ,@body))))

(defmacro restart-case (&environment env form &rest clauses)
  (let ((cluster nil))
    (when clauses (setq cluster (gensym) form (restart-case-form form env cluster)))
    (flet ((restart-case-1 (name arglist &rest forms)
             (let (interactive report test)
               (loop
                 (case (car forms)
                   (:interactive (setq interactive (cadr forms)))
                   (:report (setq report (cadr forms)))
                   (:test (setq test (cadr forms)))
                   (t (return nil)))
                 (setq forms (cddr forms)))
               (when (and report (not (stringp report)))
                 (setq report `#',report))
               (when interactive
                 (setq interactive `#',interactive))
               (when test
                 (setq test `#',test))
               (values (require-type name 'symbol) arglist report interactive test forms))))
      (cond ((null clauses) form)
            ((and (null (cdr clauses)) (null (cadr (car clauses))))
             (let ((block (gensym)) 
                   (restart-name (gensym)))
               (multiple-value-bind (name arglist report interactive test body)
                                    (apply #'restart-case-1 (car clauses))
                 (declare (ignore arglist))
                 `(block ,block
                    (let* ((,restart-name (%cons-restart ',name () ,report ,interactive ,test))
                           (,cluster (list ,restart-name))
                           (%restarts% (cons ,cluster %restarts%)))
                      (declare (dynamic-extent ,restart-name ,cluster %restarts%))
                      (catch ,cluster (return-from ,block ,form)))
                    ,@body))))
            (t
             (let ((block (gensym)) (val (gensym))
                   (index -1) restarts restart-names restart-name cases)
               (while clauses
                 (setq index (1+ index))
                 (multiple-value-bind (name arglist report interactive test body)
                                      (apply #'restart-case-1 (pop clauses))
                   (push (setq restart-name (make-symbol (symbol-name name))) restart-names)
                   (push (list restart-name `(%cons-restart ',name ,index ,report ,interactive ,test))
                         restarts)
                   (when (null clauses) (setq index t))
                   (push `(,index (apply #'(lambda ,arglist ,@body) ,val))
                         cases)))
               `(block ,block
                  (let ((,val (let* (,@restarts
                                     (,cluster (list ,@(reverse restart-names)))
                                     (%restarts% (cons ,cluster %restarts%)))
                                (declare (dynamic-extent ,@restart-names ,cluster %restarts%))
                                (catch ,cluster (return-from ,block ,form)))))
                    (case (pop ,val)
                      ,@(nreverse cases))))))))))


; Anything this hairy should die a slow and painful death.
; Unless, of course, I grossly misunderstand...
(defun restart-case-form (form env clustername)
  (let ((expansion (macroexpand form env))
        (head nil))
    (if (and (listp expansion)          ; already an ugly hack, made uglier by %error case ...
             (memq (setq head (pop expansion)) '(signal error cerror warn %error)))
      (let ((condform nil)
            (signalform nil)
            (cname (gensym)))
        (case head
          (cerror
           (destructuring-bind 
             (continue cond &rest args) expansion
             (setq condform `(condition-arg ,cond (list ,@args) 'simple-error)
                   signalform `(cerror ,continue ,cname))))
          ((signal error warn)
           (destructuring-bind
             (cond &rest args) expansion
             (setq condform `(condition-arg ,cond (list ,@args) ,(if (eq head 'warning)
                                                                   ''simple-warning
                                                                   (if (eq head 'error)
                                                                     ''simple-error
                                                                     ''simple-condition)))
                   signalform `(,head ,cname))))
          (t ;%error
           (destructuring-bind (cond args fp) expansion
             (setq condform `(condition-arg ,cond ,args 'simple-error)
                   signalform `(%error ,cname nil ,fp)))))
        `(let ((,cname ,condform))
           (with-condition-restarts ,cname ,clustername
             ,signalform)))
      form)))
      

(defmacro handler-case (form &rest clauses &aux last)
  (flet ((handler-case (type var &rest body)
           (when (eq type :no-error)
             (signal-program-error "The :no-error clause must be last."))
           (values type var body)))
    (cond ((null clauses) form)
          ((eq (car (setq last (car (last clauses)))) :no-error)
           (let ((error (gensym))
                 (block (gensym))
                 (var   (cadr last)))
             (if var
               `(block ,error
                  (multiple-value-call #'(lambda ,@(cdr last))
                                       (block ,block
                                         (return-from ,error
                                           (handler-case (return-from ,block ,form)
                                             ,@(butlast clauses))))))
               `(block ,error
                  (block ,block
                    (return-from ,error
                      (handler-case (return-from ,block ,form)
                        ,@(butlast clauses))))
                  (locally ,@(cddr last))))))
          ((null (cdr clauses))
           (let ((block   (gensym))
                 (cluster (gensym)))
             (multiple-value-bind (type var body)
                                  (apply #'handler-case (car clauses))
               (if var
                 `(block ,block
                    ((lambda ,var ,@body)
                      (let* ((,cluster (list ',type))
                            (%handlers% (cons ,cluster %handlers%)))
                       (declare (dynamic-extent ,cluster %handlers%))
                       (catch ,cluster (return-from ,block ,form)))))
                 `(block ,block
                    (let* ((,cluster (list ',type))
                           (%handlers% (cons ,cluster %handlers%)))
                      (declare (dynamic-extent ,cluster %handlers%))
                      (catch ,cluster (return-from ,block ,form)))
                    (locally ,@body))))))
          (t (let ((block (gensym)) (cluster (gensym)) (val (gensym))
                   (index -1) handlers cases)
               (while clauses
                 (setq index (1+ index))
                 (multiple-value-bind (type var body)
                                      (apply #'handler-case (pop clauses))                   
                   (push `',type handlers)
                   (push index handlers)
                   (when (null clauses) (setq index t))
                   (push (if var
                           `(,index ((lambda ,var ,@body) ,val))
                           `(,index (locally ,@body))) cases)))
               `(block ,block
                  (let ((,val (let* ((,cluster (list ,@(nreverse handlers)))
                                     (%handlers% (cons ,cluster %handlers%)))
                                (declare (dynamic-extent ,cluster %handlers%))
                                (catch ,cluster (return-from ,block ,form)))))
                    (case (pop ,val)
                      ,@(nreverse cases)))))))))

(defmacro with-simple-restart ((restart-name format-string &rest format-args)
                               &body body
                               &aux (cluster (gensym)) (temp (make-symbol (symbol-name restart-name))))
  (unless (and (stringp format-string)
               (null format-args)
               (not (%str-member #\~ (ensure-simple-string format-string))))
    (let ((stream (gensym)))
      (setq format-string `#'(lambda (,stream) (format ,stream ,format-string ,@format-args)))))
  `(let* ((,temp (%cons-restart ',restart-name
                                'simple-restart
                                ,format-string
                                nil
                                nil))
          (,cluster (list ,temp))
          (%restarts% (cons ,cluster %restarts%)))
     (declare (dynamic-extent ,temp ,cluster %restarts%))
     (catch ,cluster ,@body)))

;Like with-simple-restart but takes a pre-consed restart.  Not CL.
(defmacro with-restart (restart &body body &aux (cluster (gensym)))
  `(let* ((,cluster (list ,restart))
          (%restarts% (cons ,cluster %restarts%)))
     (declare (dynamic-extent ,cluster %restarts%))
     (catch ,cluster ,@body)))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

(defmacro def-kernel-restart (&environment env errno name arglist &body body)
  (multiple-value-bind (body decls)
                       (parse-body body env)
    `(let* ((fn (nfunction ,name (lambda ,arglist ,@decls (block ,name ,@body))))
            (pair (assq ,errno ccl::*kernel-restarts*)))
       (if pair
         (rplacd pair fn)
         (push (cons ,errno fn) ccl::*kernel-restarts*))
       fn)))


;;; Setf.

;  If you change anything here, be sure to make the corresponding change
;  in get-setf-method.
(defmacro setf (&rest args &environment env)
  "Takes pairs of arguments like SETQ.  The first is a place and the second
  is the value that is supposed to go into that place.  Returns the last
  value.  The place argument may be any of the access forms for which SETF
  knows a corresponding setting form."
  (let ((temp (length args))
        (accessor nil))
    (cond ((eq temp 2)
           (let* ((form (car args)) 
                  (value (cadr args)))
             ;This must match get-setf-method .
             (if (atom form)
               (progn
                 (unless (symbolp form)(signal-program-error $XNotSym form))
                 `(setq ,form ,value))
               (multiple-value-bind (ftype local-p)
                                    (function-information (setq accessor (car form)) ENV)
                 (if local-p
                   (if (eq ftype :function)
                     ;Local function, so don't use global setf definitions.
                     (default-setf form value env)
                     `(setf ,(macroexpand-1 form env) ,value))
                   (cond
                    ((setq temp (%setf-method accessor))
                     (if (symbolp temp)
                       `(,temp ,@(cdar args) ,value)
                       (multiple-value-bind (dummies vals storevars setter #|getter|#)
                                            (funcall temp form env)
                         (do* ((d dummies (cdr d))
                               (v vals (cdr v))
                               (let-list nil))
                              ((null d)
                               (setq let-list (nreverse let-list))
                               `(let* ,let-list
                                  (declare (ignorable ,@dummies))
                                  (multiple-value-bind ,storevars ,value
                                    #|,getter|#
                                    ,setter)))
                           (push (list (car d) (car v)) let-list)))))
                    ((and (type-and-refinfo-p (setq temp (or (environment-structref-info accessor env)
                                                             (and #-bccl (boundp '%structure-refs%)
                                                                  (gethash accessor %structure-refs%)))))
                          (not (refinfo-r/o (if (consp temp) (%cdr temp) temp))))
                     (if (consp temp)
                       ;; strip off type, but add in a require-type
                       (let ((type (%car temp)))
                         `(the ,type (setf ,(defstruct-ref-transform (%cdr temp) (%cdar args))
                                           (require-type ,value ',type))))
                       `(setf ,(defstruct-ref-transform temp (%cdar args))
                              ,value)))
                    (t
                     (multiple-value-bind (res win)
                                          (macroexpand-1 form env)
                       (if win
                         `(setf ,res ,value)
                         (default-setf form value env))))))))))
          ((oddp temp)
           (error "Odd number of args to SETF : ~s." args))
          (t (do* ((a args (cddr a)) (l nil))
                  ((null a) `(progn ,@(nreverse l)))
               (push `(setf ,(car a) ,(cadr a)) l))))))


(defun default-setf (setter value &optional env)
  (let* ((reader (car setter))
         (args (cdr setter))
         (gensyms (mapcar #'(lambda (sym) (declare (ignore sym)) (gensym)) args))
         types declares)
    (flet ((form-type (form)
             (nx-form-type form env)))
      (declare (dynamic-extent #'form-type))
      (setq types (mapcar #'form-type args)))
    (dolist (sym gensyms)
      (let ((sym-type (pop types)))
        (unless (eq sym-type t)
          (push `(type ,sym-type ,sym) declares))))
    `(let ,(mapcar #'list gensyms args)
       ,@(and declares (list `(declare ,@(nreverse declares))))
       (funcall #'(setf ,reader) ,value ,@gensyms))))

;; Establishing these setf-inverses is something that should
;; happen at compile-time
(defsetf elt set-elt)
(defsetf car set-car)
(defsetf first set-car)
(defsetf cdr set-cdr)
(defsetf rest set-cdr)
(defsetf uvref uvset)
(defsetf aref aset)
(defsetf svref svset)
(defsetf %svref %svset)
(defsetf char set-char)
(defsetf schar set-schar)
(defsetf %scharcode %set-scharcode)
(defsetf symbol-value set)
(defsetf symbol-plist set-symbol-plist)
(defsetf fill-pointer set-fill-pointer)

; This sux; it calls the compiler twice (once to shove the macro in the
; environment, once to dump it into the file.)
(defmacro defmacro  (name arglist &body body &environment env)
  (unless (symbolp name)(signal-program-error $XNotSym name))
  (unless (listp arglist) (signal-program-error "~S is not a list." arglist))
  (multiple-value-bind (lambda-form doc)
                       (parse-macro-1 name arglist body env)
    (let* ((normalized (normalize-lambda-list arglist t t))
           (body-pos (position '&body normalized))
           (argstring (let ((temp nil))
                        (dolist (arg normalized)
                          (if (eq arg '&aux)
                            (return)
                            (push arg temp)))
                        (format nil "~:a" (nreverse temp)))))
      (if (and body-pos (memq '&optional normalized)) (decf body-pos))
      `(progn
         (eval-when (:compile-toplevel)
           (define-compile-time-macro ',name ',lambda-form ',env))
         (eval-when (:load-toplevel :execute)
           (%macro 
            (nfunction ,name ,lambda-form)
            '(,doc ,body-pos . ,argstring))
           ',name)))))

(defmacro define-symbol-macro (name expansion &environment env)
  (unless (symbolp name)(signal-program-error $XNotSym name))
  `(progn
    (eval-when (:compile-toplevel)
      (define-compile-time-symbol-macro ',name ',expansion ',env))
    (eval-when (:load-toplevel :execute)
      (%define-symbol-macro ',name ',expansion))))

;; ---- allow inlining setf functions
(defmacro defun (spec args &body body &environment env &aux global-name inline-spec)
  (validate-function-name spec)
  (setq args (require-type args 'list))
  (setq body (require-type body 'list))
  (multiple-value-bind (forms decls doc) (parse-body body env t)
    (cond ((symbolp spec)
           (setq global-name spec)
           (setq inline-spec spec)
           (setq body `(block ,spec ,@forms)))
          ((and (consp spec) (eq 'setf (%car spec)))
           (setq inline-spec spec)
           (setq body `(block ,(cadr spec) ,@forms)))
          (t (setq body `(progn ,@forms))))
    (let* ((lambda-expression `(lambda ,args 
                                ,@(if global-name
                                    `((declare (global-function-name ,global-name))))
                                ,@decls ,body))
           (info (if (and inline-spec
                          (or (null env)
                              (definition-environment env t))
                          (nx-declared-inline-p inline-spec env)
                          (not (and (symbolp inline-spec)
                                    (gethash inline-spec *NX1-ALPHATIZERS*))))
                   (cons doc lambda-expression)
                   doc)))
      `(progn
         (eval-when (:compile-toplevel)
           (note-function-info ',spec ',lambda-expression ,env))
         (%defun (nfunction ,spec ,lambda-expression) ',info)
         ',spec))))

(defmacro %defvar-init (var initform doc)
  `(unless (%defvar ',var ,doc)
     (setq ,var ,initform)))

(defmacro defvar (&environment env var &optional (value () value-p) doc)
  (if (and doc (not (stringp doc))) (report-bad-arg doc 'string))
  (if (and (compile-file-environment-p env) (not *fasl-save-doc-strings*))
    (setq doc nil))
 `(progn
    (eval-when (:compile-toplevel)
      (note-variable-info ',var ,value-p ,env))
    ,(if value-p
       `(%defvar-init ,var ,value ,doc)
       `(%defvar ',var))
    ',var))
         
(defmacro def-standard-initial-binding (name &optional (form name) &environment env)
  `(progn
    (eval-when (:compile-toplevel)
      (note-variable-info ',name t ,env))    
    (define-standard-initial-binding ',name #'(lambda () ,form))
    ',name))

(defmacro defparameter (&environment env var value &optional doc)
  (if (and doc (not (stringp doc))) (signal-program-error "~S is not a string." doc))
  (if (and (compile-file-environment-p env) (not *fasl-save-doc-strings*))
    (setq doc nil))
  `(progn
     (eval-when (:compile-toplevel)
       (note-variable-info ',var t ,env))
     (%defparameter ',var ,value ,doc)))

(defmacro defglobal (&environment env var value &optional doc)
  (if (and doc (not (stringp doc))) (signal-program-error "~S is not a string." doc))
  (if (and (compile-file-environment-p env) (not *fasl-save-doc-strings*))
    (setq doc nil))
  `(progn
     (eval-when (:compile-toplevel)
       (note-variable-info ',var :global ,env))
     (%defglobal ',var ,value ,doc)))


(defmacro defloadvar (&environment env var value &optional doc)
  `(progn
     (defvar ,var ,@(if doc `(nil ,doc)))
     (def-ccl-pointers ,var ()
       (setq ,var ,value))
     ',var))


(defmacro qlfun (name args &body body)
  `(nfunction ,name (lambda ,args ,@body)))

(defmacro lfun-bits-known-function (f)
  (let* ((temp (gensym)))
    `(let* ((,temp ,f))
      (%svref ,temp (the fixnum (1- (the fixnum (uvsize ,temp))))))))

; %Pascal-Functions% Entry
; Used by "l1;ppc-callback-support" & "lib;dumplisp"
(def-accessor-macros %svref
  pfe.routine-descriptor
  pfe.proc-info
  pfe.lisp-function
  pfe.sym
  pfe.without-interrupts)

(defmacro cond (&rest args &aux clause)
  (when args
     (setq clause (car args))
     (if (cdr clause)         
         `(if ,(car clause) (progn ,@(cdr clause)) (cond ,@(cdr args)))
       (if (cdr args) `(or ,(car clause) (cond ,@(cdr args)))
                      `(values ,(car clause))))))

(defmacro and (&rest args)
  (if (null args) t
    (if (null (cdr args)) (car args)
      `(if ,(car args) (and ,@(cdr args))))))

(defmacro or (&rest args)
  (if args
    (if (cdr args)
      (do* ((temp (gensym))
            (handle (list nil))
            (forms `(let ((,temp ,(pop args)))
                      (if ,temp ,temp ,@handle))))
           ((null (cdr args))
            (%rplaca handle (%car args))
            forms)
        (%rplaca handle `(if (setq ,temp ,(%car args)) 
                           ,temp 
                           ,@(setq handle (list nil))))
        (setq args (%cdr args)))
      (%car args))))

(defmacro case (key &body forms) 
   (let ((key-var (gensym)))
     `(let ((,key-var ,key))
        (declare (ignorable ,key-var))
        (cond ,@(case-aux forms key-var nil nil)))))

(defmacro ccase (keyplace &body forms)
  (let* ((key-var (gensym))
         (tag (gensym)))
    `(prog (,key-var)
       ,tag
       (setq ,key-var ,keyplace)
       (return (cond ,@(case-aux forms key-var tag keyplace))))))

(defmacro ecase (key &body forms)
  (let* ((key-var (gensym)))
    `(let ((,key-var ,key))
       (declare (ignorable ,key-var))
       (cond ,@(case-aux forms key-var 'ecase nil)))))
       
(defun case-aux (clauses key-var e-c-p placename &optional (used-keys (list (list '%case-core))))
  (if clauses
    (let* ((key-list (caar clauses))
           (stype (if e-c-p (if (eq e-c-p 'ecase) e-c-p 'ccase) 'case))
           (test (cond ((and (not e-c-p)
                             (or (eq key-list 't)
                                 (eq key-list 'otherwise)))
                        t)
                       (key-list
                        (cons 'or
                              (case-key-testers key-var used-keys key-list stype)))))
           (consequent-list (or (%cdar clauses) '(nil))))
      (if (eq test t)
        (progn
          (when (%cdr clauses) (warn "~s or ~s clause in the middle of a ~s statement.  Subsequent clauses ignored."
                                     't 'otherwise 'case))
          (cons (cons t consequent-list) nil))
        (cons (cons test consequent-list)
              (case-aux (%cdr clauses) key-var e-c-p placename used-keys))))
    (when e-c-p
      (setq used-keys `(member ,@(mapcar #'car (cdr used-keys))))
      (if (eq e-c-p 'ecase)
        `((t (values (%err-disp #.$XWRONGTYPE ,key-var ',used-keys))))
        `((t (setf ,placename (ensure-value-of-type ,key-var ',used-keys ',placename))
           (go ,e-c-p)))))))


;;; We don't want to descend list structure more than once (like this has
;;; been doing for the last 18 years or so.)
(defun case-key-testers (symbol used-keys atom-or-list statement-type &optional recursive)
  (if (or recursive (atom atom-or-list))
    (progn
      (if (assoc atom-or-list used-keys)
        (warn "Duplicate keyform ~s in ~s statement." atom-or-list statement-type)
        (nconc used-keys (list (cons atom-or-list t))))
      `((,(if (typep atom-or-list '(and number (not fixnum)))
              'eql
              'eq)
         ,symbol ',atom-or-list)))
    (nconc (case-key-testers symbol used-keys (car atom-or-list) statement-type t)
           (when (cdr atom-or-list)
             (case-key-testers symbol used-keys (%cdr atom-or-list) statement-type nil)))))


; generate the COND body of a {C,E}TYPECASE form
(defun typecase-aux (key-var clauses &optional e-c-p keyform)
  (let* ((construct (if e-c-p (if (eq e-c-p 'etypecase) e-c-p 'ctypecase) 'typecase))
         (types ())
         (t-clause ())
         (body ()))
    (flet ((bad-clause (c) 
             (error "Invalid clause ~S in ~S form." c construct)))
      (dolist (clause clauses)
        (if (atom clause)
          (bad-clause clause)
          (destructuring-bind (typespec &body consequents) clause
            (when (eq construct 'typecase)
              (if (eq typespec 'otherwise)
                (setq typespec t))
              (if (eq typespec t)
                (if t-clause
                  (bad-clause clause)   ; seen one already
                  (setq t-clause `( t nil ,@consequents)))))
            (unless (and (eq construct 'typecase)
                         (eq typespec t))
              (when
                  (dolist (already types t)
                    (when (subtypep typespec already)
                      (warn "Clause ~S ignored in ~S form - shadowed by ~S ." clause construct (assq already clauses))
                      (return)))
                (push typespec types)
                (unless (eq typespec t)
                  (setq typespec `(typep ,key-var ',typespec)))
                (push `(,typespec nil ,@consequents) body))))))
      (when e-c-p
        (setq types `(or ,@(nreverse types)))
        (if (eq construct 'etypecase)
          (push `(t (values (%err-disp #.$XWRONGTYPE ,key-var ',types))) body)
          (push `(t (setf ,keyform (ensure-value-of-type ,key-var ',types ',keyform))
                  (go ,e-c-p)) body))))
    (when t-clause
      (push t-clause body))
    `(cond ,@(nreverse body))))

(defmacro typecase (keyform &body clauses)
  (let ((key-var (gensym)))
    `(let ((,key-var ,keyform))
       (declare (ignorable ,key-var))
       ,(typecase-aux key-var clauses))))

(defmacro etypecase (keyform &body clauses)
  (let ((key-var (gensym)))
    `(let ((,key-var ,keyform))
       (declare (ignorable ,key-var))
       ,(typecase-aux key-var clauses 'etypecase))))

(defmacro ctypecase (keyform &body clauses)
  (let ((key-var (gensym))
        (tag (gensym)))
    `(prog (,key-var)
       ,tag
       (setq ,key-var ,keyform)
       (return ,(typecase-aux key-var clauses tag keyform)))))

(defmacro destructuring-bind (lambda-list expression &body body)
  (multiple-value-bind (bindings decls)
      (%destructure-lambda-list  lambda-list expression nil nil)
    `(let* ,(nreverse bindings)
      ,@(when decls `((declare ,@decls)))
      ,@body)))

(defmacro make-destructure-state (tail whole lambda)
  `(gvector :istruct 'destructure-state ,tail ,whole ,lambda))


; This is supposedly ANSI CL.
(defmacro lambda (&whole lambda-expression (&rest paramlist) &body body)
  (unless (lambda-expression-p lambda-expression)
    (warn "Invalid lambda expression: ~s" lambda-expression))
  `(function (lambda ,paramlist ,@body)))


(defmacro when (test &body body)
 `(if ,test
   (progn ,@body)))

(defmacro unless (test &body body)
 `(if (not ,test)
   (progn ,@body)))

(defmacro return (&optional (form nil form-p))
  `(return-from nil ,@(if form-p `(,form))))

; since they use tagbody, while & until BOTH return NIL
(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (when ,test (go ,toplab)))))

(defmacro until (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (if (not ,test)
        (go ,toplab)))))

(defmacro psetq (&whole call &body pairs &environment env)
  (when pairs
   (if (evenp (length pairs))
     (do* ((l pairs (%cddr l))
           (sym (%car l) (%car l)))
          ((null l) (%pset pairs))
       (unless (symbolp sym) (report-bad-arg sym 'symbol))
       (when (nth-value 1 (macroexpand-1 sym env))
         (return `(psetf ,@pairs))))
     (error "Uneven number of args in the call ~S" call))))

; generates body for psetq.
; "pairs" is a proper list whose length is not odd.
(defun %pset (pairs)
 (when pairs
   (let (vars vals gensyms let-list var val sets)
      (loop
        (setq var (pop pairs)
              val (pop pairs))
        (if (null pairs) (return))
        (push var vars)
        (push val vals)
        (push (gensym) gensyms))
      (dolist (g gensyms)
        (push g sets)
        (push (pop vars) sets)
        (push (list g (pop vals)) let-list))
      (push val sets)
      (push var sets)
      `(progn
         (let ,let-list
           (setq ,@sets))
         nil))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun do-loop (binder setter env var-init-steps end-test result body)
  (let ((toptag (gensym))
        (testtag (gensym)))
    (multiple-value-bind (forms decls) (parse-body body env nil)
      `(block nil
         (,binder ,(do-let-vars var-init-steps)
                  ,@decls
                  (tagbody ; crocks-r-us.
                    (go ,testtag)
                    ,toptag
                    (tagbody
                      ,@forms)
                    (,setter ,@(do-step-vars var-init-steps))
                    ,testtag
                    (unless ,end-test
                      (go ,toptag)))
                  ,@result)))))
)

(defmacro do (&environment env var-init-steps (&optional end-test &rest result) &body body)
  (do-loop 'let 'psetq env var-init-steps end-test result body))

(defmacro do* (&environment env var-init-steps (&optional end-test &rest result) &body body)
  (do-loop 'let* 'setq env var-init-steps end-test result body))


(defun do-let-vars (var-init-steps)
  (if var-init-steps
      (cons (list (do-let-vars-var (car var-init-steps))
                  (do-let-vars-init (car var-init-steps)))
             (do-let-vars (cdr var-init-steps)))))

(defun do-let-vars-var (var-init-step)
  (if (consp var-init-step)
       (car var-init-step)
       var-init-step))

(defun do-let-vars-init (var-init-step)
   (if (consp var-init-step)
        (cadr var-init-step)
        nil))

(defun do-step-vars (var-init-steps)
    (if var-init-steps
        (if (do-step-vars-step? (car var-init-steps))
             (append (list (do-let-vars-var (car var-init-steps))
                           (do-step-vars-step (car var-init-steps)))
                     (do-step-vars (cdr var-init-steps)))
             (do-step-vars (cdr var-init-steps)))))

(defun do-step-vars-step? (var-init-step)
  (if (consp var-init-step)
       (cddr var-init-step)))

(defun do-step-vars-step (var-init-step)
  (if (consp var-init-step)
       (caddr var-init-step)))


(defmacro dotimes ((i n &optional result) &body body &environment env)
  (multiple-value-bind (forms decls)
                       (parse-body body env)
    (if (not (symbolp i))(signal-program-error $Xnotsym i))
    (let* ((toptag (gensym))
           (limit (gensym)))
      `(block nil
        (let ((,limit ,n) (,i 0))
         ,@decls
         (declare (unsettable ,i))
           (if (int>0-p ,limit)
             (tagbody
               ,toptag
               ,@forms
               (locally
                (declare (settable ,i))
                (setq ,i (1+ ,i)))
               (unless (eql ,i ,limit) (go ,toptag))))
           ,result)))))
  
(defun do-syms-result (var resultform)
  (unless (eq var resultform)
    (if (and (consp resultform) (not (quoted-form-p resultform)))
      `(progn (setq ,var nil) ,resultform)
      resultform)))

(defun expand-package-iteration-macro (iteration-function var pkg-spec resultform body env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    (let* ((ftemp (gensym))
           (vtemp (gensym))
           (result (do-syms-result var resultform)))
      `(block nil
        (let* ((,var nil))
          ,@decls
           (flet ((,ftemp (,vtemp) (declare (debugging-function-name nil)) (setq ,var ,vtemp) (tagbody ,@body)))
             (declare (dynamic-extent #',ftemp))
             (,iteration-function ,pkg-spec #',ftemp))
           ,@(when result `(,result)))))))

(defmacro do-symbols ((var &optional pkg result) &body body &environment env)
  (expand-package-iteration-macro 'iterate-over-accessable-symbols var pkg result body env))

(defmacro do-present-symbols ((var &optional pkg result) &body body &environment env)
  (expand-package-iteration-macro 'iterate-over-present-symbols var pkg result body env))

(defmacro do-external-symbols ((var &optional pkg result) &body body &environment env)
  (expand-package-iteration-macro 'iterate-over-external-symbols var pkg result body env))

(defmacro do-all-symbols ((var &optional resultform) 
                          &body body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    (let* ((ftemp (gensym))
           (vtemp (gensym))
           (result (do-syms-result var resultform)))
      `(block nil
        (let* ((,var nil))
         ,@decls
           (flet ((,ftemp (,vtemp) (declare (debugging-function-name nil)) (setq ,var ,vtemp) (tagbody ,@body)))
             (declare (dynamic-extent #',ftemp))
             (iterate-over-all-symbols #',ftemp))
           ,@(when result `(,result)))))))

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defmacro multiple-value-bind (varlist values-form &body body &environment env)
  (multiple-value-bind (body decls)
                       (parse-body body env)
    (let ((ignore (make-symbol "IGNORE")))
      `(multiple-value-call #'(lambda (&optional ,@varlist &rest ,ignore)
                                (declare (ignore ,ignore))
                                ,@decls
                                ,@body)
                            ,values-form))))

(defmacro multiple-value-setq (vars val)
  (if vars
    `(values (setf (values ,@(mapcar #'(lambda (s) (require-type s 'symbol)) vars))  ,val))
    `(prog1 ,val)))

(defmacro nth-value (n form)
  `(car (nthcdr ,n (multiple-value-list ,form))))


(defmacro %i> (x y)
  `(> (the fixnum ,x) (the fixnum ,y)))

(defmacro %i< (x y)
  `(< (the fixnum ,x) (the fixnum ,y)))

(defmacro %i<= (x y)
 `(not (%i> ,x ,y)))

(defmacro %i>= (x y)
 `(not (%i< ,x ,y)))

(defmacro bitset (bit number)
  `(logior (ash 1 ,bit) ,number))

(defmacro bitclr (bit number)
  `(logand (lognot (ash 1 ,bit)) ,number))

(defmacro bitopf ((op bit place) &environment env)
  (multiple-value-bind (vars vals stores store-form access-form)
                       (get-setf-method place env)
    (let* ((constant-bit-p (constantp bit))
           (bitvar (if constant-bit-p bit (gensym))))
      `(let ,(unless constant-bit-p `((,bitvar ,bit)))          ; compiler isn't smart enough
         (let* ,(mapcar #'list `(,@vars ,@stores) `(,@vals (,op ,bitvar ,access-form)))
           ,store-form)))))

(defmacro bitsetf (bit place)
  `(bitopf (bitset ,bit ,place)))

(defmacro bitclrf (bit place)
  `(bitopf (bitclr ,bit ,place)))

(defmacro %svref (v i)
  (let* ((vtemp (make-symbol "VECTOR"))
           (itemp (make-symbol "INDEX")))
      `(let* ((,vtemp ,v)
              (,itemp ,i))
         (locally (declare (optimize (speed 3) (safety 0)))
           (svref ,vtemp ,itemp)))))

(defmacro %svset (v i new)
  (let* ((vtemp (make-symbol "VECTOR"))
           (itemp (make-symbol "INDEX")))
      `(let* ((,vtemp ,v)
              (,itemp ,i))
         (locally (declare (optimize (speed 3) (safety 0)))
           (setf (svref ,vtemp ,itemp) ,new)))))


(defmacro %schar (v i)
  (let* ((vtemp (make-symbol "STRING"))
         (itemp (make-symbol "INDEX")))
    `(let* ((,vtemp ,v)
            (,itemp ,i))
       (locally (declare (optimize (speed 3) (safety 0)))
         (schar ,vtemp ,itemp)))))

(defmacro %set-schar (v i new)
  (let* ((vtemp (make-symbol "STRING"))
           (itemp (make-symbol "INDEX")))
      `(let* ((,vtemp ,v)
              (,itemp ,i))
         (locally (declare (optimize (speed 3) (safety 0)))
           (setf (schar ,vtemp ,itemp) ,new)))))



(defmacro %char-code (c) `(char-code (the character ,c)))
(defmacro %code-char (i) `(code-char (the (unsigned-byte 16) ,i)))

(defmacro %izerop (x) `(eq ,x 0))
(defmacro %iminusp (x) `(< (the fixnum ,x) 0))
(defmacro %i+ (&rest (&optional (n0 0) &rest others))
  (if others
    `(the fixnum (+ (the fixnum ,n0) (%i+ ,@others)))
    `(the fixnum ,n0)))
(defmacro %i- (x y &rest others) 
  (if (not others)
    `(the fixnum (- (the fixnum ,x) (the fixnum ,y)))
    `(the fixnum (- (the fixnum ,x) (the fixnum (%i+ ,y ,@others))))))


(defmacro %i* (x y) `(the fixnum (* (the fixnum ,x) (the fixnum ,y))))

(defmacro %ilogbitp (b i)
  `(logbitp (the (integer 0 29) ,b) (the fixnum ,i)))

;;; Seq-Dispatch does an efficient type-dispatch on the given Sequence.

(defmacro seq-dispatch (sequence list-form array-form)
  `(if (sequence-type ,sequence)
       ,list-form
       ,array-form))


(defsetf %get-byte %set-byte)
(defsetf %get-unsigned-byte %set-byte)
(defsetf %get-signed-byte %set-byte)
(defsetf %get-word %set-word)
(defsetf %get-signed-word %set-word)
(defsetf %get-unsigned-word %set-word)
(defsetf %get-long %set-long)
(defsetf %get-signed-long %set-long)
(defsetf %get-unsigned-long %set-long)
(defsetf %get-full-long %set-long)
(defsetf %get-point %set-long)
(defsetf %get-string %set-string)
(defsetf %get-ptr %set-ptr)
(defsetf %get-double-float %set-double-float)
(defsetf %get-single-float %set-single-float)
(defsetf %get-bit %set-bit)
(defsetf %get-unsigned-long-long %set-unsigned-long-long)
(defsetf %%get-unsigned-longlong %%set-unsigned-longlong)
(defsetf %get-signed-long-long %set-signed-long-long)
(defsetf %%get-signed-longlong %%set-signed-longlong)
(defsetf %get-bitfield %set-bitfield)

(defmacro %ilognot (int) `(%i- -1 ,int))

(defmacro %ilogior2 (x y) 
  `(logior (the fixnum ,x) (the fixnum ,y)))

(defmacro %ilogior (body &body args)
   (while args
     (setq body (list '%ilogior2 body (pop args))))
   body)

(defmacro %ilogand2 (x y)
  `(logand (the fixnum ,x) (the fixnum ,y)))

(defmacro %ilogand (body &body args)
   (while args
     (setq body (list '%ilogand2 body (pop args))))
   body)

(defmacro %ilogxor2 (x y)
  `(logxor (the fixnum ,x) (the fixnum ,y)))

(defmacro %ilogxor (body &body args)
   (while args
     (setq body (list '%ilogxor2 body (pop args))))
   body)

(defmacro with-macptrs (varlist &rest body &aux decls inits)
  (dolist (var varlist)
    (if (consp var)
      (progn
        (push (car var) decls)
        (push (list (%car var)
                    (if (%cdr var)
                      `(%setf-macptr (%null-ptr) ,@(%cdr var))
                      '(%null-ptr))) inits))
      (progn
        (push var decls)
        (push (list var '(%null-ptr)) inits))))
  `(let* ,(nreverse inits)
     (declare (dynamic-extent ,@decls))
     (declare (type macptr ,@decls))
     ,@body))

(defmacro with-loading-file (filename &rest body)
   `(let ((*loading-files* (cons ,filename (locally (declare (special *loading-files*))
                                                    *loading-files*))))
      (declare (special *loading-files*))
      ,@body))

(defmacro with-input-from-string ((var string &key index start end) &body forms &environment env)
  (multiple-value-bind (forms decls) (parse-body forms env nil)
    `(let ((,var
	    ,(cond ((null end)
		    `(make-string-input-stream ,string ,(or start 0)))
		   ((symbolp end)
		    `(if ,end
		      (make-string-input-stream ,string ,(or start 0) ,end)
		      (make-string-input-stream ,string ,(or start 0))))
		   (t
		    `(make-string-input-stream ,string ,(or start 0) ,end)))))
      ,@decls
      (multiple-value-prog1 (unwind-protect
                                 (progn ,@forms)
                              (close ,var))
        ,@(if index `((setf ,index (string-input-stream-index ,var))))))))

(defmacro with-output-to-string ((var &optional string &key (element-type 'base-char element-type-p))
                                 &body body 
                                 &environment env)
  (multiple-value-bind (forms decls) (parse-body body env nil)
    `(let ((,var ,(if string
                    `(%make-string-output-stream ,string)
                    `(make-string-output-stream :element-type ,(if element-type-p element-type `'base-char)))))
       ,@decls
       (unwind-protect
         (progn
           ,@forms
           ,@(if string () `((get-output-stream-string ,var))))
         (close ,var)))))

(defmacro with-output-to-truncating-string-stream ((var len) &body body
						   &environment env)
  (multiple-value-bind (forms decls) (parse-body body env nil)
    `(let* ((,var (make-truncating-string-stream ,len)))
      ,@decls
      (unwind-protect
	   (progn
	     ,@forms
	     (values (get-output-stream-string ,var)
		     (slot-value ,var 'truncated)))
	(close ,var)))))

(defmacro with-open-file ((var . args) &body body &aux (stream (gensym))(done (gensym)))
  `(let (,stream ,done)
     (unwind-protect
       (multiple-value-prog1
         (let ((,var (setq ,stream (open ,@args))))
           ,@body)
         (setq ,done t))
       (when ,stream (close ,stream :abort (null ,done))))))

(defmacro with-compilation-unit ((&key override) &body body)
  `(let* ((*outstanding-deferred-warnings* (%defer-warnings ,override)))
     (multiple-value-prog1 (progn ,@body) (report-deferred-warnings))))

; Yow! Another Done Fun.
(defmacro with-standard-io-syntax (&body body &environment env)
  (multiple-value-bind (decls body) (parse-body body env)
    `(let ((*package* (find-package "CL-USER"))
           (*print-array* t)
           (*print-base* 10.)
           (*print-case* :upcase)
           (*print-circle* nil)
           (*print-escape* t)
           (*print-gensym* t)
           (*print-length* nil)
           (*print-level* nil)
           (*print-lines* nil) ; This doesn't exist as of 5/15/90 - does now
           (*print-miser-width* nil)
           (*print-pprint-dispatch* nil)
           (*print-pretty* nil)
           (*print-radix* nil)
           (*print-readably* t)
           (*print-right-margin* nil)
           (*read-base* 10.)
           (*read-default-float-format* 'single-float)
           (*read-eval* t) ; Also MIA as of 5/15/90
           (*read-suppress* nil)
           (*readtable* %initial-readtable%))
       ,@decls
       ,@body)))
           
(defmacro print-unreadable-object (&environment env (object stream &key type identity) &body forms)
  (multiple-value-bind (body decls) (parse-body forms env)
    (if body
      (let ((thunk (gensym)))
        `(let ((,thunk #'(lambda () ,@decls ,@body)))
           (declare (dynamic-extent ,thunk))
          (%print-unreadable-object ,object ,stream ,type ,identity ,thunk)))
      `(%print-unreadable-object ,object ,stream ,type ,identity nil))))
;; Pointers and Handles

;;Add function to lisp system pointer functions, and run it if it's not already
;; there.
(defmacro def-ccl-pointers (name arglist &body body &aux (old (gensym)))
  `(flet ((,name ,arglist ,@body))
     (let ((,old (member ',name *lisp-system-pointer-functions* :key #'function-name)))
       (if ,old
         (rplaca ,old #',name)
         (progn
           (push #',name *lisp-system-pointer-functions*)
           (,name))))))

(defmacro def-load-pointers (name arglist &body body &aux (old (gensym)))
  `(flet ((,name ,arglist ,@body))
     (let ((,old (member ',name *lisp-user-pointer-functions* :key #'function-name)))
       (if ,old
         (rplaca ,old #',name)
         (progn
           (push #',name *lisp-user-pointer-functions*)
           (,name))))))

;Queue up some code to run after ccl all loaded up, or, if ccl is already
;loaded up, just run it right now.
(defmacro queue-fixup (&rest body &aux (fn (gensym)))
  `(let ((,fn #'(lambda () ,@body)))
     (if (eq %lisp-system-fixups% T)
       (funcall ,fn)
       (push (cons ,fn *loading-file-source-file*) %lisp-system-fixups%))))

(defmacro %incf-ptr (p &optional (by 1))
  (if (symbolp p)  ;once-only
    `(%setf-macptr (the macptr ,p) (%inc-ptr ,p ,by))
    (let ((var (gensym)))
      `(let ((,var ,p)) (%setf-macptr (the macptr ,var) (%inc-ptr ,var ,by))))))

(defmacro with-string-from-cstring ((s ptr) &body body)
  (let* ((len (gensym))
	 (p (gensym)))
    `(let* ((,p ,ptr)
	    (,len (%cstrlen ,p))
	    (,s (make-string ,len)))
      (declare (fixnum ,len))
      (%copy-ptr-to-ivector ,p 0 ,s 0 ,len)
      (locally
	  ,@body))))


(defmacro with-cstr ((sym str &optional start end) &rest body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    (if (and (base-string-p str) (null start) (null end))
      (let ((strlen (%i+ (length str) 1)))
        `(%stack-block ((,sym ,strlen))
           ,@decls
           (%cstr-pointer ,str ,sym)
           ,@body))
      (let ((strname (gensym))
            (start-name (gensym))
            (end-name (gensym)))
        `(let ((,strname ,str)
               ,@(if (or start end)
                   `((,start-name ,(or start 0))
                     (,end-name ,(or end `(length ,strname))))))
           (%vstack-block (,sym
                           (the fixnum
                             (1+
                              (the fixnum
                                ,(if (or start end)
                                     `(byte-length
                                       ,strname ,start-name ,end-name)
                                     `(length ,strname))))))
             ,@decls
             ,(if (or start end)
                `(%cstr-segment-pointer ,strname ,sym ,start-name ,end-name)
                `(%cstr-pointer ,strname ,sym))
             ,@body))))))





(defmacro with-pointers (speclist &body body)
   (with-specs-aux 'with-pointer speclist body))



(defmacro with-cstrs (speclist &body body)
   (with-specs-aux 'with-cstr speclist body))





(defun with-specs-aux (name spec-list body)
  (setq body (cons 'progn body))
  (dolist (spec (reverse spec-list))
     (setq body (list name spec body)))
  body)


(defmacro type-predicate (type)
  `(get-type-predicate ,type))

(defsetf type-predicate set-type-predicate)

(defmacro defmethod (name &rest args &environment env)
  (multiple-value-bind (function-form specializers-form qualifiers lambda-list documentation specializers)
                       (parse-defmethod name args env)
    
    `(progn
       (eval-when (:compile-toplevel)
         (note-function-info ',name nil ,env))
       (compiler-let ((*nx-method-warning-name* 
                       (list ',name
                             ,@(mapcar #'(lambda (x) `',x) qualifiers)
                             ',specializers)))
	 (ensure-method ',name ,specializers-form
                        :function ,function-form
                        :qualifiers ',qualifiers
                        :lambda-list ',lambda-list
                        ,@(if documentation `(:documentation ,documentation)))))))


(defun seperate-defmethod-decls (decls)
  (let (outer inner)
    (dolist (decl decls)
      (if (neq (car decl) 'declare)
        (push decl outer)
        (let (outer-list inner-list)
          (dolist (d (cdr decl))
            (if (and (listp d) (eq (car d) 'dynamic-extent))
              (let (in out)
                (dolist (fspec (cdr d))
                  (if (and (listp fspec)
                           (eq (car fspec) 'function)
                           (listp (cdr fspec))
                           (null (cddr fspec))
                           (memq (cadr fspec) '(call-next-method next-method-p)))
                    (push fspec in)
                    (push fspec out)))
                (when out
                  (push `(dynamic-extent ,@(nreverse out)) outer-list))
                (when in
                  (push `(dynamic-extent ,@(nreverse in)) inner-list)))
              (push d outer-list)))
          (when outer-list
            (push `(declare ,@(nreverse outer-list)) outer))
          (when inner-list
            (push `(declare ,@(nreverse inner-list)) inner)))))
    (values (nreverse outer) (nreverse inner))))
		   

(defun parse-defmethod (name args env)
  (validate-function-name name)
  (let (qualifiers lambda-list parameters specializers specializers-form refs types temp)
    (until (listp (car args))
      (push (pop args) qualifiers))
    (setq lambda-list (pop args))
    (while (and lambda-list (not (memq (car lambda-list) lambda-list-keywords)))
      (let ((p (pop lambda-list)))
        (cond ((consp p)
               (unless (and (consp (%cdr p)) (null (%cddr p)))
                 (signal-program-error "Illegal arg ~S" p))
               (push (%car p) parameters)
               (push (%car p) refs)
               (setq p (%cadr p))
               (cond ((and (consp p) (eq (%car p) 'eql)
                           (consp (%cdr p)) (null (%cddr p)))
                      (push `(list 'eql ,(%cadr p)) specializers-form)
                      (push p specializers))
                     ((or (setq temp (non-nil-symbol-p p))
                          (specializer-p p))
                      (push `',p specializers-form)
                      (push p specializers)
                      (unless (or (eq p t) (not temp))
                        ;Should be `(guaranteed-type ...).
                        (push `(type ,p ,(%car parameters)) types)))
                     (t (signal-program-error "Illegal arg ~S" p))))
              (t
               (push p parameters)
               (push t specializers-form)
               (push t specializers)))))
    (setq lambda-list (nreconc parameters lambda-list))
    (multiple-value-bind (body decls doc) (parse-body args env t)
      (multiple-value-bind (outer-decls inner-decls) 
                           (seperate-defmethod-decls decls)
        (let* ((methvar (make-symbol "NEXT-METHOD-CONTEXT"))
               (cnm-args (gensym))
               (lambda-form `(lambda ,(list* '&method methvar lambda-list)
                               (declare ;,@types
                                (ignorable ,@refs))
                               ,@outer-decls
                               (block ,(if (consp name) (cadr name) name)
                                 (flet ((call-next-method (&rest ,cnm-args)
                                          (declare (dynamic-extent ,cnm-args))
                                          (if ,cnm-args
                                            (apply #'%call-next-method-with-args ,methvar ,cnm-args)
                                            (%call-next-method ,methvar)))
                                        (next-method-p () (%next-method-p ,methvar)))
                                   (declare (inline call-next-method next-method-p))
                                   ,@inner-decls
                                   ,@body)))))
          (values
           (if name `(nfunction ,name ,lambda-form) `(function ,lambda-form))
           `(list ,@(nreverse specializers-form))
           (nreverse qualifiers)
	   lambda-list
           doc
           (nreverse specializers)))))))

(defmacro anonymous-method (name &rest args &environment env)
  (multiple-value-bind (function-form specializers-form qualifiers method-class documentation)
                       (parse-defmethod name args env)
    
    `(%anonymous-method
      ,function-form
      ,specializers-form
      ',qualifiers
      ,@(if (or method-class documentation) `(',method-class))
      ,@(if documentation `(,documentation)))))



(defmacro defclass (class-name superclasses slots &rest class-options &environment env)
  (flet ((duplicate-options (where) (signal-program-error "Duplicate options in ~S" where))
         (illegal-option (option) (signal-program-error "Illegal option ~s" option))
         (make-initfunction (form)
           (cond ((or (eq form 't)
                      (equal form ''t))
                  '(function true))
                 ((or (eq form 'nil)
                      (equal form ''nil))
                  '(function false))
                 (t
                  `(function (lambda () ,form))))))
    (setq class-name (require-type class-name '(and symbol (not null))))
    (setq superclasses (mapcar #'(lambda (s) (require-type s 'symbol)) superclasses))
    (let* ((options-seen ())
           (signatures ())
           (slot-names))
      (flet ((canonicalize-defclass-option (option)
               (let* ((option-name (car option)))
                 (if (member option-name options-seen :test #'eq)
                   (duplicate-options class-options)
                   (push option-name options-seen))
                 (case option-name
                   (:default-initargs
                       (let ((canonical ()))
                         (let (key val (tail (cdr option)))
                           (loop (when (null tail) (return nil))
			       (setq key (pop tail)
				     val (pop tail))
			     (push ``(,',key ,',val  ,,(make-initfunction val)) canonical))
                           `(':direct-default-initargs (list ,@(nreverse canonical))))))
                   (:metaclass
                    (unless (and (cadr option)
                                 (typep (cadr option) 'symbol))
                      (illegal-option option))
                    `(:metaclass  ',(cadr option)))
                   (:documentation
                    `(:documentation ',(cadr option)))
                   (t
                    (list `',option-name `',(cdr option))))))
             (canonicalize-slot-spec (slot)
               (if (null slot) (signal-program-error "Illegal slot NIL"))
               (if (not (listp slot)) (setq slot (list slot)))
               (let* ((slot-name (require-type (car slot) 'symbol))
		      (initargs nil)
                      (other-options ())
		      (initform nil)
		      (initform-p nil)
		      (initfunction nil)
		      (type nil)
		      (type-p nil)
		      (allocation nil)
		      (allocation-p nil)
		      (documentation nil)
		      (documentation-p nil)
		      (readers nil)
		      (writers nil))
                 (when (memq slot-name slot-names)
                   (SIGNAL-PROGRAM-error "Duplicate slot name ~S" slot-name))
                 (push slot-name slot-names)
                 (do ((options (cdr slot) (cddr options))
                      name)
                     ((null options))
                   (when (null (cdr options)) (signal-program-error "Illegal slot spec ~S" slot))
                   (case (car options)
                     (:reader
                      (setq name (cadr options))
		      (push name signatures)
                      (push name readers))
                     (:writer                      
                      (setq name (cadr options))
                      (push name signatures)
                      (push name writers))
                     (:accessor
                      (setq name (cadr options))
                      (push name signatures)
                      (push name readers)
                      (push `(setf ,name) signatures)
                      (push `(setf ,name) writers))
                     (:initarg
                      (push (require-type (cadr options) 'symbol) initargs))
                     (:type
                      (if type-p
			(duplicate-options slot)
			(setq type-p t))
                      ;(when (null (cadr options)) (signal-program-error "Illegal options ~S" options))
                      (setq type (cadr options)))
                     (:initform
                      (if initform-p
			(duplicate-options slot)
			(setq initform-p t))
                      (let ((option (cadr options)))
                        (setq initform `',option
                              initfunction
                              (if (constantp option)
                                `(constantly ,option)
                                `#'(lambda () ,option)))))
                     (:allocation
                      (if allocation-p
			(duplicate-options slot)
			(setq allocation-p t))
                      (setq allocation (cadr options)))
                     (:documentation
                      (if documentation-p
			(duplicate-options slot)
			(setq documentation-p t))
                      (setq documentation (cadr options)))
                     (t
                      (let* ((pair (or (assq (car options) other-options)
                                       (car (push (list (car options)) other-options)))))
                        (push (cadr options) (cdr pair))))))
                 `(list :name ',slot-name
		   ,@(when allocation `(:allocation ',allocation))
		   ,@(when initform-p `(:initform ,initform
					:initfunction ,initfunction))
		   ,@(when initargs `(:initargs ',initargs))
		   ,@(when readers `(:readers ',readers))
		   ,@(when writers `(:writers ',writers))
		   ,@(when type-p `(:type ',type))
		   ,@(when documentation-p `(:documentation ,documentation))
                   ,@(mapcan #'(lambda (opt)
                                 `(',(car opt) ',(cdr opt))) other-options)))))
	(let* ((direct-superclasses superclasses)
	       (direct-slot-specs (mapcar #'canonicalize-slot-spec slots))
	       (other-options (apply #'append (mapcar #'canonicalize-defclass-option class-options ))))
	  `(progn
	    (eval-when (:compile-toplevel)
	      (%compile-time-defclass ',class-name ,env)
	      (progn
		,@(mapcar #'(lambda (s) `(note-function-info ',s nil ,env))
			  signatures)))
	      (ensure-class-for-defclass ',class-name
			    :direct-superclasses ',direct-superclasses
			    :direct-slots ,`(list ,@direct-slot-specs)
			    ,@other-options)))))))

(defmacro define-method-combination (name &rest rest &environment env)
  (setq name (require-type name 'symbol))
  (cond ((or (null rest) (and (car rest) (symbolp (car rest))))
         `(short-form-define-method-combination ',name ',rest))
        ((listp (car rest))
         (destructuring-bind (lambda-list method-group-specifiers . forms) rest
           (long-form-define-method-combination 
            name lambda-list method-group-specifiers forms env)))
        (t (%badarg (car rest) '(or (and null symbol) list)))))

(defmacro defgeneric (function-name lambda-list &rest options-and-methods &environment env)
  (fboundp function-name)             ; type-check
  (multiple-value-bind (method-combination generic-function-class options methods)
                       (parse-defgeneric function-name t lambda-list options-and-methods)
    (let ((gf (gensym)))
      `(progn
         (eval-when (:compile-toplevel)
           (note-function-info ',function-name nil ,env))
         (let ((,gf (%defgeneric
                     ',function-name ',lambda-list ',method-combination ',generic-function-class 
                     ',(apply #'append options))))
           (%set-defgeneric-methods ,gf ,@methods)
           ,gf)))))



(defun parse-defgeneric (function-name global-p lambda-list options-and-methods)
  (check-generic-function-lambda-list lambda-list)
  (let ((method-combination '(standard))
        (generic-function-class 'standard-generic-function)
        options declarations methods option-keywords method-class)
    (flet ((bad-option (o)
             (signal-program-error "Bad option: ~s to ~s." o 'defgeneric)))
      (dolist (o options-and-methods)
        (let ((keyword (car o))
              (defmethod (if global-p 'defmethod 'anonymous-method)))
          (if (eq keyword :method)
            (push `(,defmethod ,function-name ,@(%cdr o)) methods)
            (cond ((and (not (eq keyword 'declare))
			(memq keyword (prog1 option-keywords (push keyword option-keywords))))		   
                   (signal-program-error "Duplicate option: ~s to ~s" keyword 'defgeneric))
                  ((eq keyword :method-combination)
                   (unless (symbolp (cadr o))
                     (bad-option o))
                   (setq method-combination (cdr o)))
                  ((eq keyword :generic-function-class)
                   (unless (and (cdr o) (symbolp (cadr o)) (null (%cddr o)))
                     (bad-option o))
                   (setq generic-function-class (%cadr o)))
                  ((eq keyword 'declare)
		   (push (cadr o) declarations))
                  ((eq keyword :argument-precedence-order)
                   (dolist (arg (cdr o))
                     (unless (and (symbolp arg) (memq arg lambda-list))
                       (bad-option o)))
                   (push (list keyword (cdr o)) options))
                  ((eq keyword :method-class)
                   (push o options)
                   (when (or (cddr o) (not (symbolp (setq method-class (%cadr o)))))
                     (bad-option o)))
                  ((eq keyword :documentation)
                   (push o options)
                   (when (or (cddr o) (not (stringp (%cadr o))))
                     (bad-option o)))
                  (t (bad-option o)))))))
    (when method-class
      (dolist (m methods)
        (push `(:method-class ,method-class) (cddr m))))
    (when declarations
      (setq options `((:declarations ,declarations) ,@options)))
    (values method-combination generic-function-class options methods)))

                 
(defmacro def-aux-init-functions (class &rest functions)
  `(set-aux-init-functions ',class (list ,@functions)))






; A powerful way of defining REPORT-CONDITION...
; Do they really expect that each condition type has a unique method on PRINT-OBJECT
; which tests *print-escape* ?  Scary if so ...

(defmacro define-condition (name (&rest supers) &optional ((&rest slots)) &body options)
  ; If we could tell what environment we're being expanded in, we'd
  ; probably want to check to ensure that all supers name conditions
  ; in that environment.
  (let ((classopts nil)
        (duplicate nil)
        (docp nil)
	(default-initargs-p nil)
        (reporter nil))
    (dolist (option options)
      (unless (and (consp option)
                   (consp (%cdr option)))
        (error "Invalid option ~s ." option))
      (ecase (%car option)
	(:default-initargs 
	    (unless (plistp (cdr option)) 
	      (signal-program-error "~S is not a plist." (%cdr option))) 
	    (if default-initargs-p 
	      (setq duplicate t) 
	      (push (setq default-initargs-p option) classopts))) 
        (:documentation 
	 (unless (null (%cddr option)) 
	   (error "Invalid option ~s ." option)) 
	 (if docp
	   (setq duplicate t)
           (push (setq docp option) classopts)))
        (:report 
	 (unless (null (%cddr option)) 
	   (error "Invalid option ~s ." option)) 
         (if reporter
           (setq duplicate t)
           (progn
             (if (or (lambda-expression-p (setq reporter (%cadr option)))
                     (symbolp reporter))
               (setq reporter `(function ,reporter))
               (if (stringp reporter)
                 (setq reporter `(function (lambda (c s) (declare (ignore c)) (write-string ,reporter s))))
                 (error "~a expression is not a string, symbol, or lambda expression ." (%car option))))
             (setq reporter `((defmethod report-condition ((c ,name) s)
                                (funcall ,reporter c s))))))))
      (if duplicate (error "Duplicate option ~s ." option)))
    `(progn
       (defclass ,name ,(or supers '(condition)) ,slots ,@classopts)
       ,@reporter
       ',name)))

(defmacro with-condition-restarts (&environment env condition restarts &body body)
  (multiple-value-bind (body decls)
                       (parse-body body env)
    (let ((cond (gensym))
          (r (gensym)))
          `(let* ((*condition-restarts* *condition-restarts*))
             ,@decls
             (let ((,cond ,condition))
               (dolist (,r ,restarts) (push (cons ,r ,cond) *condition-restarts*))
               ,@body)))))
  
(defmacro setf-find-class (name arg1 &optional (arg2 () 2-p) (arg3 () 3-p))
  (cond (3-p ;might want to pass env (arg2) to find-class someday?
         `(set-find-class ,name (progn ,arg1 ,arg2 ,arg3)))
        (2-p
         `(set-find-class ,name (progn ,arg1 ,arg2)))
        (t `(set-find-class ,name ,arg1))))

(defsetf find-class setf-find-class)

(defmacro restoring-interrupt-level (var &body body)
  `(unwind-protect
    (progn ,@body)
    (restore-interrupt-level ,var)
    (%interrupt-poll)))

(defmacro without-interrupts (&body body)
  (let* ((level (gensym)))
    `(let* ((,level (disable-lisp-interrupts)))
      (restoring-interrupt-level ,level ,@body))))


;; undoes the effect of one enclosing without-interrupts during execution of body.
(defmacro ignoring-without-interrupts (&body body)
  (let* ((level (gensym)))
    `(let ((,level (interrupt-level)))
       (unwind-protect
	    (progn
	      (setf (interrupt-level) 0)
	      ,@body)
	 (setf (interrupt-level) ,level)))))

(defmacro error-ignoring-without-interrupts (format-string &rest format-args)
  `(ignoring-without-interrupts
    (error ,format-string ,@format-args)))


;init-list-default: if there is no init pair for <keyword>,
;    add a <keyword> <value> pair to init-list
(defmacro init-list-default (the-init-list &rest args)
  (let ((result)
       (init-list-sym (gensym)))
   (do ((args args (cddr args)))
       ((not args))
     (setq result 
           (cons `(if (eq '%novalue (getf ,init-list-sym ,(car args) 
                                          '%novalue))
                    (setq ,init-list-sym (cons ,(car args) 
                                               (cons ,(cadr args) 
                                                     ,init-list-sym))))
                 result)))                                                                                
   `(let ((,init-list-sym ,the-init-list))
      (progn ,@result)
      ,init-list-sym)
   ))

; This can only be partially backward-compatible: even if only
; the "name" arg is supplied, the old function would create the
; package if it didn't exist.
; Should see how well this works & maybe flush the whole idea.

(defmacro in-package (&whole call name &rest gratuitous-backward-compatibility)
  (let ((form nil))
    (cond (gratuitous-backward-compatibility
           (cerror "Macroexpand into a call to the old IN-PACKAGE function."
                   "Macro call ~S contains extra arguments." call )
           (setq form `(ccl::old-in-package ,name ,@gratuitous-backward-compatibility)))
        (t
         (when (quoted-form-p name)
           (warn "Unquoting argument ~S to ~S." name 'in-package )
           (setq name (cadr name)))    
         (setq form `(set-package ,(string name)))))
         `(eval-when (:execute :load-toplevel :compile-toplevel)
            ,form)))

(defmacro defpackage (name &rest options)
  (let* ((size nil)
         (all-names-size 0)
         (intern-export-size 0)
         (shadow-etc-size 0)
	 (documentation nil)
         (all-names-hash (let ((all-options-alist nil))
                           (dolist (option options)
                             (let ((option-name (car option)))
                               (when (memq option-name
                                           '(:nicknames :shadow :shadowing-import-from
                                             :use :import-from :intern :export))
                                 (let ((option-size (length (cdr option)))
                                       (cell (assq option-name all-options-alist)))
                                   (declare (fixnum option-size))
                                   (if cell
                                     (incf (cdr cell) option-size)
                                     (push (cons option-name option-size) all-options-alist))
                                   (when (memq option-name '(:shadow :shadowing-import-from :import-from :intern))
                                     (incf shadow-etc-size option-size))
                                   (when (memq option-name '(:export :intern))
                                     (incf intern-export-size option-size))))))
                           (dolist (cell all-options-alist)
                             (let ((option-size (cdr cell)))
                               (when (> option-size all-names-size)
                                 (setq all-names-size option-size))))
                           (when (> all-names-size 0)
                             (make-hash-table :test 'equal :size all-names-size))))
         (intern-export-hash (when (> intern-export-size 0)
                               (make-hash-table :test 'equal :size intern-export-size)))
         (shadow-etc-hash (when (> shadow-etc-size 0)
                            (make-hash-table :test 'equal :size shadow-etc-size)))
         (external-size nil)
         (nicknames nil)
         (shadow nil)
         (shadowing-import-from-specs nil)
         (use :default)
         (import-from-specs nil)
         (intern nil)
         (export nil))
    (declare (fixnum all-names-size intern-export-size shadow-etc-size))
    (labels ((string-or-name (s) (string s))
             (duplicate-option (o)
               (signal-program-error "Duplicate ~S option in ~S ." o options))
             (duplicate-name (name option-name)
               (signal-program-error "Name ~s, used in ~s option, is already used in a conflicting option ." name option-name))
             (all-names (option-name tail already)
               (when (eq already :default) (setq already nil))
               (when all-names-hash
                 (clrhash all-names-hash))
               (dolist (name already)
                 (setf (gethash (string-or-name name) all-names-hash) t))
               (dolist (name tail already)
                 (setq name (string-or-name name))
                 (unless (gethash name all-names-hash)          ; Ok to repeat name in same option.
                   (when (memq option-name '(:shadow :shadowing-import-from :import-from :intern))
                     (if (gethash name shadow-etc-hash)
                       (duplicate-name name option-name))
                     (setf (gethash name shadow-etc-hash) t))
                   (when (memq option-name '(:export :intern))
                     (if (gethash name intern-export-hash)
                       (duplicate-name name option-name))
                     (setf (gethash name intern-export-hash) t))
                   (setf (gethash name all-names-hash) t)
                   (push name already)))))
      (dolist (option options)
        (let ((args (cdr option)))
          (ecase (%car option)
                 (:size 
                  (if size 
                    (duplicate-option :size) 
                    (setq size (car args))))		 
                 (:external-size 
                  (if external-size 
                    (duplicate-option :external-size) 
                    (setq external-size (car args))))
                 (:nicknames (setq nicknames (all-names nil args nicknames)))
                 (:shadow (setq shadow (all-names :shadow args shadow)))
                 (:shadowing-import-from
                  (destructuring-bind (from &rest shadowing-imports) args
                    (push (cons (string-or-name from)
                                (all-names :shadowing-import-from shadowing-imports nil))
                          shadowing-import-from-specs)))
                 (:use (setq use (all-names nil args use)))
                 (:import-from
                  (destructuring-bind (from &rest imports) args
                    (push (cons (string-or-name from)
                                (all-names :import-from imports nil))
                          import-from-specs)))
                 (:intern (setq intern (all-names :intern args intern)))
                 (:export (setq export (all-names :export args export)))
		 (:documentation
		  (if documentation
		    (duplicate-option :documentation)
		    (setq documentation (cadr option)))))))
      `(eval-when (:execute :compile-toplevel :load-toplevel)
         (%define-package ',(string-or-name name)
	  ',size 
	  ',external-size 
	  ',nicknames
	  ',shadow
	  ',shadowing-import-from-specs
	  ',use
	  ',import-from-specs
	  ',intern
	  ',export
	  ',documentation)))))


(defmacro %cons-pkg-iter (pkgs types)
  `(vector ,pkgs ,types #'%start-with-package-iterator
           nil nil nil nil))

(defmacro with-package-iterator ((mname package-list first-type &rest other-types)
                                 &body body)
  (setq mname (require-type mname 'symbol))
  (let ((state (make-symbol "WITH-PACKAGE-ITERATOR_STATE"))
        (types 0))
    (declare (fixnum types))
    (dolist (type (push first-type other-types))
      (case type
        (:external (setq types (bitset $pkg-iter-external types)))
        (:internal (setq types (bitset $pkg-iter-internal types)))
        (:inherited (setq types (bitset $pkg-iter-inherited types)))
        (t (%badarg type '(member :internal :external :inherited)))))
    `(let ((,state (%cons-pkg-iter ,package-list ',types)))
       (declare (dynamic-extent ,state))
       (macrolet ((,mname () `(funcall (%svref ,',state #.pkg-iter.state) ,',state)))
         ,@body))))

; Does NOT evaluate the constructor, but DOES evaluate the destructor & initializer
(defmacro defresource (name &key constructor destructor initializer)
  `(defparameter ,name (make-resource #'(lambda () ,constructor)
                                      ,@(when destructor
                                          `(:destructor ,destructor))
                                      ,@(when initializer
                                          `(:initializer ,initializer)))))

(defmacro using-resource ((var resource) &body body)
  (let ((resource-var (gensym)))
  `(let ((,resource-var ,resource)
         ,var)
     (unwind-protect
       (progn
         (setq ,var (allocate-resource ,resource-var))
         ,@body)
       (when ,var
         (free-resource ,resource-var ,var))))))

(defmacro with-lock-grabbed ((lock &optional
                                   (whostate "Lock"))
                             &body body)
  (declare (ignore whostate))
  `(with-recursive-lock (,lock) ,@body))

(defmacro with-lock-grabbed-maybe ((lock &optional
					 (whostate "Lock"))
				   &body body)
  (declare (ignore whostate))
  `(with-recursive-lock-maybe (,lock) ,@body))

(defmacro with-standard-abort-handling (abort-message &body body)
  (let ((stream (gensym)))
    `(restart-case
       (catch :abort
         (catch-cancel
           ,@body))
       (abort () ,@(when abort-message
                     `(:report (lambda (,stream)
                                 (write-string ,abort-message ,stream)))))
       (abort-break ()))))
       



(defmacro %lexpr-count (l)
  `(%lisp-word-ref ,l 0))

(defmacro %lexpr-ref (lexpr count i)
  `(%lisp-word-ref ,lexpr (%i- ,count ,i)))

; args will be list if old style clos
(defmacro apply-with-method-context (magic function args)
  (let ((m (gensym))
        (f (gensym))
        (as (gensym)))
      `((lambda (,m ,f ,as)
          (if (listp ,as)
            (%apply-with-method-context ,m ,f ,as)
            (%apply-lexpr-with-method-context ,m ,f ,as))) ,magic ,function ,args)))

(defmacro defcallback (name arglist &body body &environment env)
  (define-callback name arglist body env))

(defmacro %get-single-float-from-double-ptr (ptr offset)
  `(%double-float->short-float (%get-double-float ,ptr ,offset)))

(defun define-callback (name args body env)
  (let* ((stack-word (gensym))
         (stack-ptr (gensym))
         (arg-names ())
         (arg-types ())
         (return-type :void)
         (args args)
         (woi nil)
	 (monitor nil)
         (dynamic-extent-names ())
         (error-return nil))
    (loop
      (when (null args) (return))
      (when (null (cdr args))
        (setq return-type (car args))
        (return))
      (if (eq (car args) :without-interrupts)
        (setq woi (cadr args) args (cddr args))
	(if (eq (car args) :monitor-exception-ports)
	  (setq monitor (cadr args) args (cddr args))
          (if (eq (car args) :error-return)
            (setq error-return
                  #+poweropen-target (cadr args)
                  #-poweropen-target (warn "~s not yet implemented on this platform"
                                           :error-return)
                  args (cddr args))
            (progn
              (push (foreign-type-to-representation-type (pop args)) arg-types)
              (push (pop args) arg-names))))))
    (setq arg-names (nreverse arg-names)
          arg-types (nreverse arg-types))
    (setq return-type (foreign-type-to-representation-type return-type))
    (when (eq return-type :void)
      (setq return-type nil))
    (let* ((offset #+poweropen-target 0 #+eabi-target 96)
	   #+eabi-target (gpr 0)
	   #+eabi-target (fpr 32)
           (need-stack-pointer (or arg-names return-type error-return))
           (lets
             (mapcar
	      #+poweropen-target
	      #'(lambda (name type)
		  (let* ((delta 4)
			 (bias 0))
		    (prog1
			(list name
			      `(,
				(if (typep type 'unsigned-byte)
				  (progn (setq delta (* 4 type)) '%inc-ptr)
				  (ecase type
				    (:single-float '%get-single-float)
				    (:double-float (setq delta 8)'%get-double-float)
				    (:signed-doubleword (setq delta 8) '%%get-signed-longlong)
				    (:signed-fullword '%get-signed-long)
				    (:signed-halfword (setq bias 2) '%get-signed-word)
				    (:signed-byte (setq bias 3) '%get-signed-byte)
				    (:unsigned-doubleword (setq delta 8) '%%get-unsigned-longlong)
				    (:unsigned-fullword '%get-unsigned-long)
				    (:unsigned-halfword (setq bias 2) '%get-unsigned-word)
				    (:unsigned-byte (setq bias 3) '%get-unsigned-byte)
				    (:address '%get-ptr)))
				,stack-ptr
				(+ ,offset ,bias)))
		      (when (or (eq type :address)
				(typep type 'unsigned-byte))
			(push name dynamic-extent-names))
		      (incf offset delta))))
	      #+eabi-target
	      #'(lambda (name type)
		  (let* ((nextgpr gpr)
			 (nextfpr fpr)
			 (nextoffset offset)
			 (target gpr)
			 (bias 0))
		    (prog1
			(list name
			      `(,
				(case type
				  (:single-float
				   (incf nextfpr 8)
				   (if (< fpr 96)
				     (setq target fpr)
				     (setq target (+ offset (logand offset 4))
					   nextoffset (+ target 8)))
				   '%get-single-float-from-double-ptr)
				  (:double-float
				   (incf nextfpr 8)
				   (if (< fpr 96)
				     (setq target fpr)
				     (setq target (+ offset (logand offset 4))
					   nextoffset (+ target 8)))
				   '%get-double-float)
				  (:signed-doubleword
				   (if (< gpr 56)
				     (setq target (+ gpr (logand gpr 4))
					   nextgpr (+ 8 target))
				     (setq target (+ offset (logand offset 4))
					   nextoffset (+ 8 offset)))
				   '%%get-signed-longlong)
				  (:unsigned-doubleword
				   (if (< gpr 56)
				     (setq target (+ gpr (logand gpr 4))
					   nextgpr (+ 8 target))
				     (setq target (+ offset (logand offset 4))
					   nextoffset (+ 8 offset)))
				   '%%get-unsigned-longlong)
				  (t
				   (incf nextgpr 4)
				   (if (< gpr 64)
				     (setq target gpr)
				     (setq target offset nextoffset (+ offset 4)))
				   (ecase type
				     (:signed-fullword '%get-signed-long)
				     (:signed-halfword (setq bias 2) '%get-signed-word)
				     (:signed-byte (setq bias 3) '%get-signed-byte)
				     (:unsigned-fullword '%get-unsigned-long)
				     (:unsigned-halfword (setq bias 2) '%get-unsigned-word)
				     (:unsigned-byte (setq bias 3) '%get-unsigned-byte)
				     (:address '%get-ptr))))
				,stack-ptr
				(+ ,target ,bias)))
		      (when (eq type :address)
			(push name dynamic-extent-names))
		      (setq gpr nextgpr fpr nextfpr offset nextoffset))))
	      arg-names arg-types)))
      (multiple-value-bind (body decls doc) (parse-body body env t)
        `(progn
           (declaim (special ,name))
           (define-callback-function
             (nfunction ,name
                        (lambda (,stack-word)
                          (declare (ignorable ,stack-word))
                          (block ,name
                            (with-macptrs (,@(and need-stack-pointer (list `(,stack-ptr))))
                              ,(when need-stack-pointer
                                 `(%setf-macptr-to-object ,stack-ptr ,stack-word))
                              ,(defcallback-body stack-ptr lets dynamic-extent-names
                                                 decls body return-type error-return
                                                 #+poweropen-target
                                                 (- ppc32::c-frame.savelr ppc32::c-frame.param0)
                                                 #-poweropen-target 0)))))
             ,doc
             ,woi
	     ,monitor))))))

(defun defcallback-body (stack-ptr lets dynamic-extent-names decls body return-type error-return error-delta)
  (let* ((result (gensym))
         (condition-name (if (atom error-return) 'error (car error-return)))
         (error-return-function (if (atom error-return) error-return (cadr error-return)))
         (body
          `(let ,lets
	    (declare (dynamic-extent ,@dynamic-extent-names))
	    ,@decls
	    (let ((,result (progn ,@body)))
	      (declare (ignorable ,result))
	      , (when return-type
		  `(setf (,
			  (case return-type
			    (:address '%get-ptr)
			    (:signed-doubleword '%%get-signed-longlong)
			    (:unsigned-doubleword '%%get-unsigned-longlong)
			    (:double-float '%get-double-float)
			    (:single-float '%get-single-float)
			    (t '%get-long)) ,stack-ptr) ,result))))))
    (if error-return
      (let* ((cond (gensym)))
        `(handler-case ,body
          (,condition-name (,cond) (,error-return-function ,cond ,stack-ptr (%inc-ptr ,stack-ptr ,error-delta)))))
      body)))
  

(defmacro errchk (form)
  (let* ((res (gensym)))
    `(let* ((,res ,form))
       (if (eql 0 ,res)
         0
         (signal-posix-error ,res)))))

(defmacro define-toplevel-command (group-name name arglist &body body &environment env)
  (let* ((key (make-keyword name)))
    (multiple-value-bind (body decls doc) (parse-body body env)
      `(%define-toplevel-command ',group-name ,key ',name 
	(nfunction ,name (lambda ,arglist
			   ,@decls
			   (block ,name
			     ,@body)))
	,doc
        ',(mapcar #'symbol-name arglist)))))

(defmacro with-toplevel-commands (group-name &body body)
  `(let* ((*active-toplevel-commands* *active-toplevel-commands*))
    (progn
      (%use-toplevel-commands ',group-name)
      ,@body)))

(defmacro assert (test-form &optional (places ()) string &rest args)
  "ASSERT Test-Form [(Place*) [String Arg*]]
  If the Test-Form is not true, then signal a correctable error.  If Places
  are specified, then new values are prompted for when the error is proceeded.
  String and Args are the format string and args to the error call."
  (let* ((TOP (gensym))
         (setf-places-p (not (null places))))
    `(tagbody
       ,TOP
       (unless ,test-form
         (%assertion-failure ,setf-places-p ',test-form ,string ,@args)
         ,@(if places
             `((write-line "Type expressions to set places to, or nothing to leave them alone."
                           *query-io*)
               ,@(mapcar #'(lambda (place &aux (new-val (gensym))
                                          (set-p (gensym)))
                             `(multiple-value-bind
                                (,new-val ,set-p)
                                (assertion-value-prompt ',place)
                                (when ,set-p (setf ,place (values-list ,new-val)))))
                         places)))
         (go ,TOP)))))


(defmacro check-type (place typespec &optional string)
  "CHECK-TYPE Place Typespec [String]
  Signal a correctable error if Place does not hold an object of the type
  specified by Typespec."
  `(progn
     (setf ,place 
           (ensure-value-of-type 
            ,place 
            ',typespec 
            ',place 
            ,(if string string (list 'quote typespec))))
     nil))

(defmacro with-hash-table-iterator ((mname hash-table) &body body &environment env)
  (let ((state (gensym)))
    (multiple-value-bind (body decls) (parse-body body env)
      `(let ((,state (vector nil nil ,hash-table nil nil)))
	(declare (dynamic-extent ,state))
	(unwind-protect
	     (macrolet ((,mname () `(do-hash-table-iteration ,',state)))
	       (start-hash-table-iterator ,state)
	       (locally ,@decls ,@body))
	  (finish-hash-table-iterator ,state))))))

(eval-when (compile load eval)
(defmacro pprint-logical-block ((stream-symbol list
				 &key (prefix nil) (per-line-prefix nil)
				      (suffix ""))
				&body body)
  (cond ((eq stream-symbol nil) (setq stream-symbol '*standard-output*))
	((eq stream-symbol T) (setq stream-symbol '*terminal-io*)))
  (when (not (symbolp stream-symbol))
    (warn "STREAM-SYMBOL arg ~S to PPRINT-LOGICAL-BLOCK is not a bindable symbol"
	  stream-symbol)
    (setq stream-symbol '*standard-output*))
  (when (and prefix per-line-prefix)
    (warn "prefix ~S and per-line-prefix ~S cannot both be specified ~
           in PPRINT-LOGICAL-BLOCK")
    (setq per-line-prefix nil))
  `(maybe-initiate-xp-printing
     #'(lambda (,stream-symbol)
	 (let ((+l ,list)
	       (+p ,(or prefix per-line-prefix ""))
	       (+s ,suffix))
	   (pprint-logical-block+
	     (,stream-symbol +l +p +s ,(not (null per-line-prefix)) T nil)
	     ,@ body nil)))
     (decode-stream-arg ,stream-symbol)))


;Assumes var and args must be variables.  Other arguments must be literals or variables.

(defmacro pprint-logical-block+ ((var args prefix suffix per-line? circle-check? atsign?)
				 &body body)
  (when (and circle-check? atsign?)
    (setq circle-check? 'not-first-p))
  `(let ((*current-level* (1+ *current-level*))
	 (*current-length* -1)
	 ;(*parents* *parents*)
	 ,@(if (and circle-check? atsign?) `((not-first-p (plusp *current-length*)))))
     (unless (check-block-abbreviation ,var ,args ,circle-check?)
       (start-block ,var ,prefix ,per-line? ,suffix)
       (when
         (catch 'line-limit-abbreviation-exit
           (block logical-block
             (macrolet ((pprint-pop () `(pprint-pop+ ,',args ,',var))
                        (pprint-exit-if-list-exhausted ()
                          `(if (null ,',args) (return-from logical-block nil))))
               ,@ body))
           (end-block ,var ,suffix)
           nil)
         (end-block ,var ,suffix)
         (throw 'line-limit-abbreviation-exit T)))))
) ; eval-when

(defmacro %old-class-local-shared-slotds (class &optional default)
  (if default                           ; so setf works
    `(%class-get ,class '%old-class-local-shared-slotds ,default)
    `(%class-get ,class '%old-class-local-shared-slotds)))

(defmacro with-slot-values (slot-entries instance-form &body body)
; Simplified form of with-slots.  Expands into a let instead of a symbol-macrolet
; Thus, you can access the slot values, but you can't setq them.
  (let ((instance (gensym)) var slot-name bindings)
    (dolist (slot-entry slot-entries)
      (cond ((symbolp slot-entry)
             (setq var slot-entry slot-name slot-entry))
            ((and (listp slot-entry) (cdr slot-entry) (null (cddr slot-entry))
                  (symbolp (car slot-entry)) (symbolp (cadr slot-entry)))
             (setq var (car slot-entry) slot-name (cadr slot-entry)))
            (t (error "Malformed slot-entry: ~a to with-slot-values.~@
                       Should be a symbol or a list of two symbols."
                      slot-entry)))
      (push `(,var (slot-value ,instance ',slot-name)) bindings))
    `(let ((,instance ,instance-form))
       (let ,(nreverse bindings)
         ,@body))))

(defmacro with-slots (slot-entries instance-form &body body)
  (let ((instance (gensym)) var slot-name bindings)
    (dolist (slot-entry slot-entries)
      (cond ((symbolp slot-entry)
             (setq var slot-entry slot-name slot-entry))
            ((and (listp slot-entry) (cdr slot-entry) (null (cddr slot-entry))
                  (symbolp (car slot-entry)) (symbolp (cadr slot-entry)))
             (setq var (car slot-entry) slot-name (cadr slot-entry)))
            (t (error "Malformed slot-entry: ~a to with-slots.~@
                       Should be a symbol or a list of two symbols."
                      slot-entry)))
      (push `(,var (slot-value ,instance ',slot-name)) bindings))
    `(let ((,instance ,instance-form))
       ,@(unless bindings (list `(declare (ignore ,instance))))
       (symbol-macrolet ,(nreverse bindings)
         ,@body))))

(defmacro with-accessors (slot-entries instance-form &body body)
  (let ((instance (gensym)) var reader bindings)
    (dolist (slot-entry slot-entries)
      (cond ((and (listp slot-entry) (cdr slot-entry) (null (cddr slot-entry))
                  (symbolp (car slot-entry)) (symbolp (cadr slot-entry)))
             (setq var (car slot-entry) reader (cadr slot-entry)))
            (t (error "Malformed slot-entry: ~a to with-accessors.~@
                       Should be a list of two symbols."
                      slot-entry)))
      (push `(,var (,reader ,instance)) bindings))
    `(let ((,instance ,instance-form))
       ,@(unless bindings (list `(declare (ignore ,instance))))
       (symbol-macrolet ,(nreverse bindings)
         ,@body))))

; I wanted to call this ":method"
(defmacro reference-method (gf &rest qualifiers-and-specializers)
  (let ((qualifiers (butlast qualifiers-and-specializers))
        (specializers (car (last qualifiers-and-specializers))))
    (if (null specializers) (report-bad-arg qualifiers-and-specializers '(not null)))
    `(find-method #',gf ',qualifiers (mapcar #'find-specializer ',specializers))))

(defmacro time (form)
  `(report-time ',form #'(lambda () (progn ,form))))

(defmacro with-error-reentry-detection (&body body)
  (let ((thunk (gensym)))
    `(let ((,thunk #'(lambda () ,@body)))
       (declare (dynamic-extent ,thunk))
       (funcall-with-error-reentry-detection ,thunk))))

(defmacro scan-for-instr (mask opcode fn pc-index &optional (tries *trap-lookup-tries*))
  `(%scan-for-instr ,mask ,opcode ,fn ,pc-index ,tries))

(defmacro codevec-header-p (word)
  `(eql ppc32::subtag-code-vector
    (logand ,word ppc32::subtag-mask)))

(defmacro match-instr (instr mask bits-to-match)
  `(eql (logand ,instr ,mask) ,bits-to-match))

(defmacro with-xp-stack-frames ((xp trap-function &optional stack-frame) &body body)
  (let ((thunk (gensym))
        (sf (or stack-frame (gensym))))
    `(let ((,thunk #'(lambda (&optional ,sf)
                       ,@(unless stack-frame `((declare (ignore ,sf))))
                       ,@body)))
       (declare (dynamic-extent ,thunk))
       (funcall-with-xp-stack-frames ,xp ,trap-function ,thunk))))

(defmacro signal-eof-error (stream)
  `(error 'end-of-file :stream ,stream))

(defmacro check-eof (valform stream eof-error-p eof-value)
  (let* ((val (gensym)))
    `(let ((,val ,valform))
      (if (eq ,val :eof)
        (if ,eof-error-p
          (signal-eof-error ,stream)
          ,eof-value)
        ,val))))

(defmacro designated-input-stream (input-stream)
  `(if ,input-stream
    (if (eq t ,input-stream)
      *terminal-io*
      ,input-stream)
    *standard-input*))

(defmacro pref (pointer accessor)
  (destructuring-bind (type-name &rest accessors) (decompose-record-accessor accessor)
    (%foreign-access-form pointer (%foreign-type-or-record type-name) 0 accessors)))

(defmacro rref (pointer accessor &key (storage :pointer storage-p))
  (when storage-p
    (warn "Use of :storage option ignored: ~a" storage))
  `(pref ,pointer ,accessor))

(defmacro rlet (spec &body body)
  `(%stack-block ,(rlet-sizes spec)
     ,@(rlet-inits spec)
     ,@body))

(defmacro rletZ (spec &body body)
  `(%stack-block ,(rlet-sizes spec t)
     ,@(rlet-inits spec)
     ,@body))

(defun rlet-sizes (inits &optional clear-p &aux result)
  (dolist (item inits (nreverse result))
    (push `(,(car item)
            ,(%foreign-type-or-record-size (cadr item) :bytes)
            ,@(if clear-p '(:clear t)))
          result)))

(defun rlet-inits (inits &aux result)
  (dolist (item inits result)
    (let* ((name (car item))
	   (record-name (cadr item))
	   (inits (cddr item))
	   (ftype (%foreign-type-or-record record-name)))
      (if (typep ftype 'foreign-record-type)
        (setq result (nconc result (%foreign-record-field-forms name ftype record-name inits)))
	(progn
	  ;(setq result (nconc result `((%assert-macptr-ftype ,name ,ftype))))
	  (when inits
	    (if (and ftype (null (cdr inits)))
              (setq result
                    (nconc result
                           `((setf ,(%foreign-access-form name ftype 0 nil)
			      ,(car inits)))))
              (error "Unexpected or malformed initialization forms: ~s in field type: ~s"
                     inits record-name))))))))

(defun %foreign-record-field-forms (ptr record-type record-name inits)
  (unless (evenp (length inits))
    (error "Unexpected or malformed initialization forms: ~s in field type: ~s"
                     inits record-name))
  (let* ((result ()))
    (do* ()
	 ((null inits)
	  `((progn
	      ;(%assert-macptr-ftype ,ptr ,record-type)
	      ,@(nreverse result))))
      (let* ((accessor (decompose-record-accessor (pop inits)))
	     (valform (pop inits)))
	(push `(setf ,(%foreign-access-form ptr record-type 0  accessor) ,valform)
	      result)))))
  
(defmacro get-field-offset (accessor)
  (destructuring-bind (type-name field-name) (decompose-record-accessor accessor)
    (let* ((record-type (require-type (%foreign-type-or-record type-name) 'foreign-record-type))
           (field (%find-foreign-record-type-field record-type field-name))
           (bit-offset (foreign-record-field-offset field)))
      `(values ,(floor bit-offset 8) ,(foreign-record-field-type field) ,bit-offset))))

(defmacro record-length (recname)
  (%foreign-type-or-record-size recname :bytes))

(defmacro make-record (record-name &rest initforms)
  (let* ((ftype (%foreign-type-or-record record-name))
         (bits (ensure-foreign-type-bits ftype))
	 (bytes (if bits
		  (ceiling bits 8)
		  (error "Unknown size for foreign type ~S."
			 (unparse-foreign-type ftype))))
	 (p (gensym))
	 (bzero (read-from-string "#_bzero")))    
    `(let* ((,p (malloc ,bytes)))
      (,bzero ,p ,bytes)
      ,@(%foreign-record-field-forms p ftype record-name initforms)
      ,p)))

(defmacro with-terminal-input (&body body)
  (let* ((got-it (gensym)))
    `(let* ((,got-it (%request-terminal-input)))
      (unwind-protect
	   (progn ,@body)
	(%restore-terminal-input ,got-it)))))





(defmacro %with-recursive-lock-ptr ((lockptr) &body body)
  `(unwind-protect
    (progn
      (%lock-recursive-lock ,lockptr)
      ,@body)
    (%unlock-recursive-lock ,lockptr)))

(defmacro %with-recursive-lock-ptr-maybe ((lockptr) &body body)
  `(when (%try-recursive-lock ,lockptr)
    (unwind-protect
	 (progn ,@body)
      (%unlock-recursive-lock ,lockptr))))

(defmacro with-recursive-lock ((lock) &body body)
  (let* ((p (gensym)))
    `(let* ((,p (recursive-lock-ptr ,lock)))
      (%with-recursive-lock-ptr (,p) ,@body))))

(defmacro with-recursive-lock-maybe ((lock) &body body)
  (let* ((p (gensym)))
    `(let* ((,p (recursive-lock-ptr ,lock)))
      (%with-recursive-lock-ptr-maybe (,p) ,@body))))

(defmacro with-read-lock ((lock) &body body)
  (let* ((p (gensym)))
    `(let* ((,p ,lock))
      (unwind-protect
           (progn
             (read-lock-rwlock ,p)
             ,@body)
        (unlock-rwlock ,p)))))


(defmacro with-write-lock ((lock) &body body)
  (let* ((p (gensym)))
    `(let* ((,p ,lock))
      (unwind-protect
           (progn
             (write-lock-rwlock ,p)
             ,@body)
        (unlock-rwlock ,p)))))



(defmacro without-gcing (&body body)
  `(unwind-protect
    (progn
      (%lock-gc-lock)
      ,@body)
    (%unlock-gc-lock)))

(defmacro with-other-threads-suspended (&body body)
  `(unwind-protect
    (progn
      (%suspend-other-threads)
      ,@body)
    (%resume-other-threads)))

(defmacro with-package-read-lock ((p) &body body)
  `(with-read-lock ((pkg.lock ,p)) ,@body))

(defmacro with-package-write-lock ((p) &body body)
  `(with-write-lock ((pkg.lock ,p)) ,@body))

(defmacro with-package-lock ((p) &body body)
  `(with-package-write-lock (,p) ,@body))

;;; Lock %all-packages-lock%, for shared read access to %all-packages%

(defmacro with-package-list-read-lock (&body body)
  `(with-read-lock (%all-packages-lock%) ,@body))

;;; Lock %all-packages-lock%, to allow modification to %all-packages%
(defmacro with-package-list-write-lock (&body body)
  `(with-write-lock (%all-packages-lock%) ,@body))

(defmacro atomic-incf-decf (place delta &environment env)
  (setq place (macroexpand place env))
  (if (consp place)
    (let* ((sym (car place))
	   (struct-transform (or (environment-structref-info sym env)
                                 (gethash sym %structure-refs%))))
      (if struct-transform
        (setq place (defstruct-ref-transform struct-transform (cdr place))
              sym (car place)))
      (ecase sym
	(the `(the ,(cadr place) (atomic-incf-decf ,(caddr place) ,delta)))
	(car `(%atomic-incf-car ,(cadr place) ,delta))
	(cdr `(%atomic-incf-cdr ,(cadr place) ,delta))
	((svref %svref) `(%atomic-incf-gvector ,@(cdr place) ,delta))))
    (if (and (symbolp place) (eq :special (variable-information place env)))
      (let* ((base (gensym))
             (offset (gensym)))
        `(multiple-value-bind (,base ,offset)
          (%symbol-binding-address ',place)
          (%atomic-incf-node ,delta ,base ,offset)))
      (error "~S is not a special variable"  place))))
    
(defmacro atomic-incf (place)
  `(atomic-incf-decf ,place 1))

(defmacro atomic-decf (place)
  `(atomic-incf-decf ,place -1))

; Some of these macros were stolen from CMUCL.  Sort of ...

(defmacro iterate (name binds &body body)
  "Iterate Name ({(Var Initial-Value)}*) Declaration* Form*
  This is syntactic sugar for Labels.  It creates a local function Name with
  the specified Vars as its arguments and the Declarations and Forms as its
  body.  This function is then called with the Initial-Values, and the result
  of the call is return from the macro."
  (dolist (x binds)
    (unless (and (listp x)
                 (= (length x) 2))
      (error "Malformed iterate variable spec: ~S." x)))

  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)


(defun collect-normal-expander (n-value fun forms)
  `(progn
     ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
     ,n-value))


)

(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create a Let* which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let* around the result of the
  evaluation of Body.  Within the body, each Var is bound to the corresponding
  temporary variable."
  (iterate frob
           ((specs specs)
            (body body))
    (if (null specs)
      `(progn ,@body)
      (let ((spec (first specs)))
        (when (/= (length spec) 2)
          (error "Malformed Once-Only binding spec: ~S." spec))
        (let ((name (first spec))
              (exp-temp (gensym)))
          `(let ((,exp-temp ,(second spec))
                 (,name (gensym)))
             `(let ((,,name ,,exp-temp))
                ,,(frob (rest specs) body))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun form-symbol (first &rest others)
  (intern (apply #'concatenate 'simple-base-string (string first) (mapcar #'string others))))
)


;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
       ,@(mapcar #'(lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond (,n-tail
                               (setf (cdr ,n-tail) ,n-res)
                               (setq ,n-tail ,n-res))
                              (t
                               (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                 forms)
       ,n-value)))

;;;
;;;    The ultimate collection macro...
;;;

(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."
  
  
  (let ((macros ())
        (binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
        (error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
            (name (first spec))
            (default (second spec))
            (kind (or (third spec) 'collect)))
        
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
          (let ((n-tail (gensym)))
            (if default
              (push `(,n-tail (last ,n-value)) binds)
              (push n-tail binds))
            (push `(,name (&rest args)
                          (collect-list-expander ',n-value ',n-tail args))
                  macros))
          (push `(,name (&rest args)
                        (collect-normal-expander ',n-value ',kind args))
                macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))


;;; DEFENUM -- Internal Interface.
;;;
(defmacro defenum ((&key (prefix "") (suffix "") (start 0) (step 1))
                   &rest identifiers)
  (let ((results nil)
        (index 0)
        (start (eval start))
        (step (eval step)))
    (dolist (id identifiers)
      (multiple-value-bind
        (root docs)
        (if (consp id)
          (values (car id) (cdr id))
          (values id nil))
        (push `(defconstant ,(intern (concatenate 'simple-base-string
                                                  (string prefix)
                                                  (string root)
                                                  (string suffix)))
                 ,(+ start (* step index))
                 ,@docs)
              results))
      (incf index))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(nreverse results))))


;;; This does something like special binding, but the "bindings" established
;;; aren't thread-specific.

(defmacro let-globally ((&rest vars) &body body &environment env)
  (multiple-value-bind (body decls) (parse-body body env)
    (let* ((initforms nil)
           (psetform nil)
           (specvars nil)
           (restoreform nil))
      (flet ((pair-name-value (p)
               (if (atom p)
                 (values (require-global-symbol p env) nil)
                 (if (and (consp (%cdr p)) (null (%cddr p)))
                   (values (require-global-symbol (%car p) env) (%cadr p))
                   (error "Invalid variable initialization form : ~s")))))
        (declare (inline pair-name-value))
        (dolist (v vars)
          (let* ((oldval (gensym))
                 (newval (gensym)))
            (multiple-value-bind (var valueform) (pair-name-value v)
              (push var specvars)
              (push var restoreform)
              (push oldval restoreform)
              (push `(,oldval (uvref ',var #.ppc32::symbol.vcell-cell)) initforms)
              (push `(,newval ,valueform) initforms)
              (push var psetform)
              (push newval psetform))))
        `(let ,(nreverse initforms)
           ,@decls
           (locally (declare (special ,@(nreverse specvars)))
             (unwind-protect
               (progn (psetq ,@(nreverse psetform)) ,@body)
               (psetq ,@(nreverse restoreform)))))))))
;;; From CLX.

;;; The good news is that this uses an interlocked load/store sequence
;;; and is fairly efficient.
;;; The bad news is that it only handles a few types of "place" forms.
;;; The good news is that CLX only uses a few types of "place" forms.

(defmacro conditional-store (place old-value new-value &environment env)
  (setq place (macroexpand place env))
  (if (atom place)
    ;; CLX uses special variables' value cells as place forms.
    (if (and (symbolp place)
             (eq :special (ccl::variable-information place env)))
      (let* ((base (gensym))
             (offset (gensym)))
        `(multiple-value-bind (,base ,offset)
          (ccl::%symbol-binding-address ',place)
          (ccl::%store-node-conditional ,offset ,base ,old-value ,new-value)))
      (error "~s is not a special variable ." place))
    (let* ((sym (car place))
           (struct-transform (or (ccl::environment-structref-info sym env)
                                 (gethash sym ccl::%structure-refs%))))
      (if struct-transform
        (setq place (ccl::defstruct-ref-transform struct-transform (cdr place))
              sym (car place)))
      (if (member  sym '(svref ccl::%svref ccl::struct-ref))
        (let* ((v (gensym)))
          `(let* ((,v ,(cadr place)))
            (ccl::store-gvector-conditional ,(caddr place)
             ,v ,old-value ,new-value)))
        (error "Don't know how to do conditional store to ~s" place)))))

(defmacro step (form)
  form)
