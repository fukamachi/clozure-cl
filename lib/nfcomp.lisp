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


;; :lib:nfcomp.lisp - New fasl compiler.

(eval-when (:compile-toplevel :load-toplevel :execute)
   (require 'level-2))

(require 'optimizers)
(require 'hash)

(eval-when (:compile-toplevel :execute)

(require 'backquote)
(require 'defstruct-macros)


(defmacro short-fixnum-p (fixnum)
  `(and (fixnump ,fixnum) (< (integer-length ,fixnum) 16)))




(require "FASLENV" "ccl:xdump;faslenv")

(require "PPC-ARCH")
) ;eval-when (:compile-toplevel :execute)

;File compiler options.  Not all of these need to be exported/documented, but
;they should be in the product just in case we need them for patches....
(defvar *fasl-save-local-symbols* t)
(defvar *fasl-deferred-warnings* nil)
(defvar *fasl-non-style-warnings-signalled-p* nil)
(defvar *fasl-warnings-signalled-p* nil)
(defvar *compile-verbose* nil) ; Might wind up getting called *compile-FILE-verbose*
(defvar *fasl-save-doc-strings*  t)
(defvar *fasl-save-definitions* nil)
(defvar *compile-file-pathname* nil) ; pathname of src arg to COMPILE-FILE
(defvar *compile-file-truename* nil) ; truename ...
(defvar *fasl-target* (backend-name *host-backend*))
(defvar *fasl-backend* *host-backend*)
(defvar *fcomp-external-format* :default)

(defvar *compile-print* nil) ; Might wind up getting called *compile-FILE-print*

;Note: errors need to rebind this to NIL if they do any reading without
; unwinding the stack!
(declaim (special *compiling-file*)) ; defined in l1-init.

(defvar *fasl-source-file* nil "Name of file currently being read from.
Will differ from *compiling-file* during an INCLUDE")

(defparameter *fasl-package-qualified-symbols* '(*loading-file-source-file* set-package %define-package)
  "These symbols are always fasdumped with full package qualification.")

(defun setup-target-features (backend features)
  (if (eq backend *host-backend*)
    features
    (let* ((new nil)
	   (nope (backend-target-specific-features *host-backend*)))
      (dolist (f features)
	(unless (memq f nope) (pushnew f new)))
      (dolist (f (backend-target-specific-features backend)
	       (progn (pushnew :cross-compiling new)new))
	(pushnew f new)))))

(defun compile-file-pathname (pathname &rest ignore &key output-file &allow-other-keys)
  (declare (ignore ignore))
  (setq pathname (merge-pathnames pathname))
  (merge-pathnames (if output-file
                     (merge-pathnames output-file *.fasl-pathname*)
                     *.fasl-pathname*) 
                   pathname))

(defun compile-file (src &key output-file
                         (verbose *compile-verbose*)
                         (print *compile-print*)
                         load
                         features
                         (target *fasl-target* target-p)
                         (save-local-symbols *fasl-save-local-symbols*)
                         (save-doc-strings *fasl-save-doc-strings*)
                         (save-definitions *fasl-save-definitions*)
			 (external-format :default)
                         force)
  (let* ((backend *host-backend*))
    (when (and target-p (not (setq backend (find-backend target))))
      (warn "Unknown :TARGET : ~S.  Reverting to ~s ..." target *fasl-target*)
      (setq target *fasl-target*  backend *host-backend*))
    (loop
	(restart-case
	 (return (%compile-file src output-file verbose print load features
				save-local-symbols save-doc-strings save-definitions force backend external-format))
	 (retry-compile-file ()
			     :report (lambda (stream) (format stream "Retry compiling ~s" src))
			     nil)
	 (skip-compile-file ()
			    :report (lambda (stream) (format stream "Skip compiling ~s" src))
			    (return))))))


(defun %compile-file (src output-file verbose print load features
                          save-local-symbols save-doc-strings save-definitions force target-backend external-format
			  &aux orig-src)

  (setq orig-src (merge-pathnames src))
  (let* ((output-default-type (backend-target-fasl-pathname target-backend)))
    (setq src (fcomp-find-file orig-src))
    (let* ((newtype (pathname-type src)))
      (when (and newtype (not (pathname-type orig-src)))
        (setq orig-src (merge-pathnames orig-src (make-pathname :type newtype :defaults nil)))))
    (setq output-file (merge-pathnames
		       (if output-file ; full-pathname in case output-file is relative
			 (full-pathname (merge-pathnames output-file output-default-type) :no-error nil) 
			 output-default-type)
		       orig-src))
    ;; This should not be necessary, but it is.
    (setq output-file (namestring output-file))
    (when (physical-pathname-p orig-src) ; only back-translate to things likely to exist at load time
      (setq orig-src (back-translate-pathname orig-src '("home" "ccl"))))
    (let* ((*fasl-non-style-warnings-signalled-p* nil)
           (*fasl-warnings-signalled-p* nil))
      (when (and (not force)
		 (probe-file output-file)
		 (not (fasl-file-p output-file)))
	(unless (y-or-n-p
		 (format nil
			 "Compile destination ~S is not ~A file!  Overwrite it?"
			 output-file (pathname-type
				      (backend-target-fasl-pathname
				       *target-backend*))))
	(return-from %compile-file nil)))
      (let* ((*features* (append (if (listp features) features (list features)) (setup-target-features target-backend *features*)))
             (*fasl-deferred-warnings* nil) ; !!! WITH-COMPILATION-UNIT ...
             (*fasl-save-local-symbols* save-local-symbols)
             (*fasl-save-doc-strings* save-doc-strings)
             (*fasl-save-definitions* save-definitions)
             (*fcomp-warnings-header* nil)
             (*compile-file-pathname* orig-src)
             (*compile-file-truename* src)
             (*package* *package*)
             (*readtable* *readtable*)
             (*compile-print* print)
             (*compile-verbose* verbose)
             (*fasl-target* (backend-name target-backend))
	     (*fasl-backend* target-backend)
	     (*target-ftd* (backend-target-foreign-type-data target-backend))
             (defenv (new-definition-environment))
             (lexenv (new-lexical-environment defenv))
	     (*fcomp-external-format* external-format))
        (let ((forms nil))
          (let* ((*outstanding-deferred-warnings* (%defer-warnings nil)))
            (rplacd (defenv.type defenv) *outstanding-deferred-warnings*)
            (setq forms (fcomp-file src orig-src lexenv))
            (setf (deferred-warnings.warnings *outstanding-deferred-warnings*) 
                  (append *fasl-deferred-warnings* (deferred-warnings.warnings *outstanding-deferred-warnings*))
                  (deferred-warnings.defs *outstanding-deferred-warnings*)
                  (append (defenv.defined defenv) (deferred-warnings.defs *outstanding-deferred-warnings*)))
            (when *compile-verbose* (fresh-line))
            (multiple-value-bind (any harsh) (report-deferred-warnings)
              (setq *fasl-warnings-signalled-p* (or *fasl-warnings-signalled-p* any)
                    *fasl-non-style-warnings-signalled-p* (or *fasl-non-style-warnings-signalled-p* harsh))))
          (fasl-scan-forms-and-dump-file forms output-file lexenv)))
      (when load (load output-file :verbose (or verbose *load-verbose*)))
      (values (truename (pathname output-file)) 
              *fasl-warnings-signalled-p* 
              *fasl-non-style-warnings-signalled-p*))))

(defvar *fcomp-locked-hash-tables*)
(defvar *fcomp-load-forms-environment* nil)

; This is separated out so that dump-forms-to-file can use it
(defun fasl-scan-forms-and-dump-file (forms output-file &optional env)
  (let ((*fcomp-locked-hash-tables* nil)
	(*fcomp-load-forms-environment* env))
    (unwind-protect
      (multiple-value-bind (hash gnames goffsets) (fasl-scan forms)
        (fasl-dump-file gnames goffsets forms hash output-file))
      (fasl-unlock-hash-tables))))

#-bccl
(defun nfcomp (src &optional dest &rest keys)
  (when (keywordp dest) (setq keys (cons dest keys) dest nil))
  (apply #'compile-file src :output-file dest keys))

#-bccl
(%fhave 'fcomp #'nfcomp)

(defparameter *default-file-compilation-policy* (new-compiler-policy))

(defun current-file-compiler-policy ()
  *default-file-compilation-policy*)

(defun set-current-file-compiler-policy (&optional new-policy)
  (setq *default-file-compilation-policy* 
        (if new-policy (require-type new-policy 'compiler-policy) (new-compiler-policy))))

(defparameter *compile-time-evaluation-policy*
  (new-compiler-policy :force-boundp-checks t))

(defun %compile-time-eval (form env)
  (funcall (compile-named-function
            `(lambda () ,form) nil t env nil nil
            *compile-time-evaluation-policy*)))


;;; No methods by default, not even for structures.  This really sux.
(defgeneric make-load-form (object &optional environment))

;;; Well, no usable methods by default.  How this is better than
;;; getting a NO-APPLICABLE-METHOD error frankly escapes me,
(defun no-make-load-form-for (object)
  (error "No ~S method is defined for ~s" 'make-load-form object))

(defmethod make-load-form ((s standard-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form-for s))

(defmethod make-load-form ((s structure-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form-for s))

(defmethod make-load-form ((c condition) &optional environment)
  (declare (ignore environment))
  (no-make-load-form-for c))

(defmethod make-load-form ((c class) &optional environment)
  (let* ((name (class-name c))
	 (found (if name (find-class name nil environment))))
    (if (eq found c)
      `(find-class ',name)
      (error "Class ~s does not have a proper name." c))))


;;;;          FCOMP-FILE - read & compile file
;;;;          Produces a list of (opcode . args) to run on loading, intermixed
;;;;          with read packages.

(defparameter *fasl-eof-forms* nil)

(defparameter cfasl-load-time-eval-sym (make-symbol "LOAD-TIME-EVAL"))
(%macro-have cfasl-load-time-eval-sym
    #'(lambda (call env) (declare (ignore env)) (list 'eval (list 'quote call))))
;Make it a constant so compiler will barf if try to bind it, e.g. (LET #,foo ...)
(define-constant cfasl-load-time-eval-sym cfasl-load-time-eval-sym)


(defparameter *reading-for-cfasl* nil "Used by the reader for #,")



(declaim (special *nx-compile-time-types*
;The following are the global proclaimed values.  Since compile-file binds
;them, this means you can't ever globally proclaim these things from within a
;file compile (e.g. from within eval-when compile, or loading a file) - the
;proclamations get lost when compile-file exits.  This is sort of intentional
;(or at least the set of things which fall in this category as opposed to
;having a separate compile-time variable is sort of intentional).
                    *nx-proclaimed-inline*    ; inline and notinline
                    *nx-proclaimed-ignore*    ; ignore and unignore
                    *nx-known-declarations*   ; declaration
                    *nx-speed*                ; optimize speed
                    *nx-space*                ; optimize space
                    *nx-safety*               ; optimize safety
                    *nx-cspeed*))             ; optimize compiler-speed

(defvar *fcomp-load-time*)
(defvar *fcomp-inside-eval-always* nil)
(defvar *fcomp-eval-always-functions* nil)   ; used by the LISP package
(defvar *fcomp-output-list*)
(defvar *fcomp-toplevel-forms*)
(defvar *fcomp-warnings-header*)
(defvar *fcomp-stream-position* nil)
(defvar *fcomp-previous-position* nil)
(defvar *fcomp-indentation*)
(defvar *fcomp-print-handler-plist* nil)
(defvar *fcomp-last-compile-print*
  '(INCLUDE (NIL . T)
    DEFSTRUCT ("Defstruct" . T) 
    DEFCONSTANT "Defconstant" 
    DEFSETF "Defsetf" 
    DEFTYPE "Deftype" 
    DEFCLASS "Defclass" 
    DEFGENERIC "Defgeneric"
    DEFMETHOD "Defmethod"
    DEFMACRO "Defmacro" 
    DEFPARAMETER "Defparameter" 
    DEFVAR "Defvar" 
    DEFUN ""))

(setf (getf *fcomp-print-handler-plist* 'defun) ""
      (getf *fcomp-print-handler-plist* 'defvar) "Defvar"
      (getf *fcomp-print-handler-plist* 'defparameter) "Defparameter"
      (getf *fcomp-print-handler-plist* 'defmacro) "Defmacro"
      (getf *fcomp-print-handler-plist* 'defmethod) "Defmethod"  ; really want more than name (use the function option)
      (getf *fcomp-print-handler-plist* 'defgeneric) "Defgeneric"
      (getf *fcomp-print-handler-plist* 'defclass) "Defclass"
      (getf *fcomp-print-handler-plist* 'deftype) "Deftype"
      (getf *fcomp-print-handler-plist* 'defsetf) "Defsetf"
      (getf *fcomp-print-handler-plist* 'defconstant) "Defconstant"
      (getf *fcomp-print-handler-plist* 'defstruct) '("Defstruct" . t)
      (getf *fcomp-print-handler-plist* 'include) '(nil . t))


(defun fcomp-file (filename orig-file env)  ; orig-file is back-translated
  (let* ((*package* *package*)
         (*compiling-file* filename)
         (*nx-compile-time-types* *nx-compile-time-types*)
         (*nx-proclaimed-inline* *nx-proclaimed-inline*)
         (*nx-known-declarations* *nx-known-declarations*)
         (*nx-proclaimed-ignore* *nx-proclaimed-ignore*)
         (*nx-speed* *nx-speed*)
         (*nx-space* *nx-space*)
         (*nx-debug* *nx-debug*)
         (*nx-safety* *nx-safety*)
         (*nx-cspeed* *nx-cspeed*)
         (*fcomp-load-time* t)
         (*fcomp-output-list* nil)
         (*fcomp-indentation* 0)
         (*fcomp-last-compile-print* (cons nil (cons nil nil))))
    (push (list $fasl-arch (backend-target-architecture *fasl-backend*)) *fcomp-output-list*)
    (fcomp-read-loop filename orig-file env :not-compile-time)
    (nreverse *fcomp-output-list*)))

(defun fcomp-find-file (file &aux path)
  (unless (or (setq path (probe-file file))
              (setq path (probe-file (merge-pathnames file *.lisp-pathname*))))
    (error "File ~S not found" file))
  (namestring path))

; orig-file is back-translated when from fcomp-file
; when from fcomp-include it's included filename merged with *compiling-file*
; which is not back translated
(defun fcomp-read-loop (filename orig-file env processing-mode)
  (when *compile-verbose*
    (format t "~&;~A ~S..."
            (if (eq filename *compiling-file*) "Compiling" " Including")
            filename))
  (with-open-file (stream filename
			  :element-type 'base-char
			  :external-format *fcomp-external-format*)
    (let* ((old-file (and (neq filename *compiling-file*) *fasl-source-file*))           
           (*fasl-source-file* filename)
           (*fcomp-toplevel-forms* nil)
           (*fasl-eof-forms* nil)
           (*loading-file-source-file* (namestring orig-file)) ; why orig-file???
           (eofval (cons nil nil))
           (read-package nil)
           form)
      (declare (special *fasl-eof-forms* *fcomp-toplevel-forms* *fasl-source-file*))
      ;This should really be something like `(set-loading-source ,filename)
      ;but then couldn't compile level-1 with this...
 ;-> In any case, change this to be a fasl opcode, so don't make an lfun just
 ;   to do this... 
; There are other reasons - more compelling ones than "fear of tiny lfuns" -
; for making this a fasl opcode.
      (fcomp-output-form $fasl-src env *loading-file-source-file*)
      (loop
        (let* ((*fcomp-stream-position* (file-position stream)))
          (unless (eq read-package *package*)
            (fcomp-compile-toplevel-forms env)
            (setq read-package *package*))
          (let ((*reading-for-cfasl*
                 (and *fcomp-load-time* cfasl-load-time-eval-sym)))
            (declare (special *reading-for-cfasl*))
            (let ((pos (file-position stream)))
              (handler-bind
                  ((error #'(lambda (c) ; we should distinguish read errors from others?
                              (format *error-output* "~&Read error between positions ~a and ~a in ~a." pos (file-position stream) filename)
                              (signal c))))
                (setq form (read stream nil eofval)))))
          (when (eq eofval form) (return))
          (fcomp-form form env processing-mode)
          (setq *fcomp-previous-position* *fcomp-stream-position*)))
      (while (setq form *fasl-eof-forms*)
        (setq *fasl-eof-forms* nil)
        (fcomp-form-list form env processing-mode))
      (when old-file
        (fcomp-output-form $fasl-src env (namestring *compile-file-pathname*)))
      (fcomp-compile-toplevel-forms env))))

#|
;Gross me out...  Hope this isn't the final scheme!
(defun fcomp-standard-source (name &aux (len (length name)) str slen)
   (dolist (log.phys *logical-directory-alist*)
       (setq str (cdr log.phys) slen (length str))
       (when (and (%i<= slen len) (string-equal str name :end2 slen))
         (return-from fcomp-standard-source
                      (%str-cat (car log.phys)
                                ";"
                                (%substr name slen len)))))
   name)
|#


(defun fcomp-form (form env processing-mode
                        &aux print-stuff 
                        (load-time (and processing-mode (neq processing-mode :compile-time)))
                        (compile-time-too (or (eq processing-mode :compile-time) 
                                              (eq processing-mode :compile-time-too))))
  (let* ((*fcomp-indentation* *fcomp-indentation*)
         (*compile-print* *compile-print*))
    (when *compile-print*
      (cond ((and (consp form) (setq print-stuff (getf *fcomp-print-handler-plist* (car form))))
             (rplaca (rplacd (cdr *fcomp-last-compile-print*) nil) nil)
             (rplaca *fcomp-last-compile-print* nil)         
             (let ((print-recurse nil))
               (when (consp print-stuff)
                 (setq print-recurse (cdr print-stuff) print-stuff (car print-stuff)))
               (cond ((stringp print-stuff)
                      (if (equal print-stuff "")
                        (format t "~&~vT~S~%" *fcomp-indentation* (second form))
                        (format t "~&~vT~S [~A]~%" *fcomp-indentation* (second form) print-stuff)))
                     ((not (null print-stuff))
                      (format t "~&~vT" *fcomp-indentation*)
                      (funcall print-stuff form *standard-output*)
                      (terpri *standard-output*)))
               (if print-recurse
                 (setq *fcomp-indentation* (+ *fcomp-indentation* 4))
                 (setq *compile-print* nil))))
            (t (unless (and (eq load-time (car *fcomp-last-compile-print*))
                            (eq compile-time-too (cadr *fcomp-last-compile-print*))
                            (eq *fcomp-indentation* (cddr *fcomp-last-compile-print*)))
                 (rplaca *fcomp-last-compile-print* load-time)
                 (rplaca (rplacd (cdr *fcomp-last-compile-print*) compile-time-too) *fcomp-indentation*)
                 (format t "~&~vTToplevel Forms...~A~%"
                         *fcomp-indentation*
                         (if load-time
                           (if compile-time-too
                             "  (Compiletime, Loadtime)"
                             "")
                           (if compile-time-too
                             "  (Compiletime)"
                             "")))))))
    (fcomp-form-1 form env processing-mode)))
           
(defun fcomp-form-1 (form env processing-mode &aux sym body)
  (if (consp form) (setq sym (%car form) body (%cdr form)))
  (case sym
    (progn (fcomp-form-list body env processing-mode))
    (eval-when (fcomp-eval-when body env processing-mode))
    (compiler-let (fcomp-compiler-let body env processing-mode))
    (locally (fcomp-locally body env processing-mode))
    (macrolet (fcomp-macrolet body env processing-mode))
    ((%include include) (fcomp-include form env processing-mode))
    (t
     ;Need to macroexpand to see if get more progn's/eval-when's and so should
     ;stay at toplevel.  But don't expand if either the evaluator or the
     ;compiler might not - better safe than sorry... 
     ; Good advice, but the hard part is knowing which is which.
     (cond 
      ((and (non-nil-symbol-p sym)
            (macro-function sym env)            
	      (not (evaluator-special-form-p sym))
	      (not (compiler-macro-function sym env))
            (not (eq sym '%defvar-init))        ;  a macro that we want to special-case
	      (multiple-value-bind (new win) (macroexpand-1 form env)
	        (if win (setq form new))
	        win))
       (fcomp-form form env processing-mode))
      ((and (not *fcomp-inside-eval-always*)
            (memq sym *fcomp-eval-always-functions*))
       (let* ((*fcomp-inside-eval-always* t))
         (fcomp-form-1 `(eval-when (:execute :compile-toplevel :load-toplevel) ,form) env processing-mode)))
      (t
       (when (or (eq processing-mode :compile-time) (eq processing-mode :compile-time-too))
         (%compile-time-eval form env))
       (when (and processing-mode (neq processing-mode :compile-time))
         (case sym
           ((%defconstant) (fcomp-load-%defconstant form env))
           ((%defparameter) (fcomp-load-%defparameter form env))
           ((%defvar %defvar-init) (fcomp-load-defvar form env))
           ((%defun) (fcomp-load-%defun form env))
           ((set-package %define-package)
            (fcomp-random-toplevel-form form env)
            (fcomp-compile-toplevel-forms env))
           ((%macro) (fcomp-load-%macro form env))
           ;      ((%deftype) (fcomp-load-%deftype form))
           ;      ((define-setf-method) (fcomp-load-define-setf-method form))
           (t (fcomp-random-toplevel-form form env)))))))))

(defun fcomp-form-list (forms env processing-mode)
  (dolist (form forms) (fcomp-form form env processing-mode)))

(defun fcomp-compiler-let (form env processing-mode &aux vars varinits)
  (fcomp-compile-toplevel-forms env)
  (dolist (pair (pop form))
    (push (nx-pair-name pair) vars)
    (push (%compile-time-eval (nx-pair-initform pair) env) varinits))
  (progv (nreverse vars) (nreverse varinits)
                 (fcomp-form-list form env processing-mode)
                 (fcomp-compile-toplevel-forms env)))

(defun fcomp-locally (body env processing-mode)
  (fcomp-compile-toplevel-forms env)
  (multiple-value-bind (body decls) (parse-body body env)
    (let* ((env (augment-environment env :declare (decl-specs-from-declarations decls))))
      (fcomp-form-list body env processing-mode)
      (fcomp-compile-toplevel-forms env))))

(defun fcomp-macrolet (body env processing-mode)
  (fcomp-compile-toplevel-forms env)
  (let ((outer-env (augment-environment env 
                                        :macro
                                        (mapcar #'(lambda (m)
                                                    (destructuring-bind (name arglist &body body) m
                                                      (list name (enclose (parse-macro name arglist body env)
                                                                          env))))
                                                (car body)))))
    (multiple-value-bind (body decls) (parse-body (cdr body) outer-env)
      (let* ((env (augment-environment 
                   outer-env
                   :declare (decl-specs-from-declarations decls))))
        (fcomp-form-list body env processing-mode)
        (fcomp-compile-toplevel-forms env)))))

(defun fcomp-symbol-macrolet (body env processing-mode)
  (fcomp-compile-toplevel-forms env)
  (let* ((outer-env (augment-environment env :symbol-macro (car body))))
    (multiple-value-bind (body decls) (parse-body (cdr body) env)
      (let* ((env (augment-environment outer-env 
                                       :declare (decl-specs-from-declarations decls))))
        (fcomp-form-list body env processing-mode)
        (fcomp-compile-toplevel-forms env)))))
                                                               
(defun fcomp-eval-when (form env processing-mode &aux (eval-times (pop form)))
  (when (or (atom eval-times) (eq (%car eval-times) 'quote))
    (report-bad-arg eval-times 'list))
  (let* ((compile-time-too  (eq processing-mode :compile-time-too))
         (compile-time-only (eq processing-mode :compile-time))
         (at-compile-time nil)
         (at-load-time nil)
         (at-eval-time nil))
    (dolist (when eval-times)
      (if (or (eq when 'compile) (eq when :compile-toplevel))
        (setq at-compile-time t)
        (if (or (eq when 'eval) (eq when :execute))
          (setq at-eval-time t)
          (if (or (eq when 'load) (eq when :load-toplevel))
            (setq at-load-time t)
            (warn "Unknown EVAL-WHEN time ~s in ~S while compiling ~S."
                  when eval-times *fasl-source-file*)))))
    (fcomp-compile-toplevel-forms env)        ; always flush the suckers
    (cond (compile-time-only
           (if at-eval-time (fcomp-form-list form env :compile-time)))
          (at-load-time
           (fcomp-form-list form env (if (or at-compile-time (and at-eval-time compile-time-too))
                                       :compile-time-too
                                       :not-compile-time)))
          ((or at-compile-time (and at-eval-time compile-time-too))
           (fcomp-form-list form env :compile-time))))
  (fcomp-compile-toplevel-forms env))

(defun fcomp-include (form env processing-mode &aux file)
  (fcomp-compile-toplevel-forms env)
  (verify-arg-count form 1 1)
  (setq file (nx-transform (%cadr form) env))
  (unless (constantp file) (report-bad-arg file '(or string pathname)))
  (let ((actual (merge-pathnames (eval-constant file)
                                 (directory-namestring *compiling-file*))))
    (when *compile-print* (format t "~&~vTIncluding file ~A~%" *fcomp-indentation* actual))
    (let ((*fcomp-indentation* (+ 4 *fcomp-indentation*))
          (*package* *package*))
      (fcomp-read-loop (fcomp-find-file actual) actual env processing-mode)
      (fcomp-output-form $fasl-src env *loading-file-source-file*))
    (when *compile-print* (format t "~&~vTFinished included file ~A~%" *fcomp-indentation* actual))))

(defun define-compile-time-constant (symbol initform env)
  (note-variable-info symbol t env)
  (let ((definition-env (definition-environment env)))
    (when definition-env
      (multiple-value-bind (value error) 
                           (ignore-errors (values (%compile-time-eval initform env) nil))
        (when error
          (warn "Compile-time evaluation of DEFCONSTANT initial value form for ~S while ~
                 compiling ~S signalled the error: ~&~A" symbol *fasl-source-file* error))
        (push (cons symbol (if error (%unbound-marker-8) value)) (defenv.constants definition-env))))
    symbol))

(defun fcomp-load-%defconstant (form env)
  (destructuring-bind (sym valform &optional doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (setq doc nil))
    (if (quoted-form-p sym)
      (setq sym (%cadr sym)))
    (if (and (typep sym 'symbol) (or (numberp valform) (quoted-form-p valform) (null valform) (eq valform t)))
      (fcomp-output-form $fasl-defconstant env sym (eval-constant valform) (eval-constant doc))
      (fcomp-random-toplevel-form form env))))

(defun fcomp-load-%defparameter (form env)
  (destructuring-bind (sym valform &optional doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (setq doc nil))
    (if (quoted-form-p sym)
      (setq sym (%cadr sym)))
    (let* ((fn (fcomp-function-arg valform env)))
      (if (and (typep sym 'symbol) (or fn (constantp valform)))
        (fcomp-output-form $fasl-defparameter env sym (or fn (eval-constant valform)) (eval-constant doc))
        (fcomp-random-toplevel-form form env)))))

; Both the simple %DEFVAR and the initial-value case (%DEFVAR-INIT) come here.
; Only try to dump this as a special fasl operator if the initform is missing
;  or is "harmless" to evaluate whether needed or not (constant or function.)
; Hairier initforms could be handled by another fasl operator that takes a thunk
; and conditionally calls it.
(defun fcomp-load-defvar (form env)
  (destructuring-bind (sym &optional (valform nil val-p) doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (setq doc nil))
    (if (quoted-form-p sym)             ; %defvar quotes its arg, %defvar-init doesn't.
      (setq sym (%cadr sym)))
    (let* ((sym-p (typep sym 'symbol)))
      (if (and sym-p (not val-p))
        (fcomp-output-form $fasl-defvar env sym)
        (let* ((fn (if sym-p (fcomp-function-arg valform env))))
          (if (and sym-p (or fn (constantp valform)))
            (fcomp-output-form $fasl-defvar-init env sym (or fn (eval-constant valform)) (eval-constant doc))
            (fcomp-random-toplevel-form (macroexpand-1 form env) env)))))))
      
(defun define-compile-time-macro (name lambda-expression env)
  (let ((definition-env (definition-environment env)))
    (if definition-env
      (push (list* name 
                   'macro 
                   (compile-named-function lambda-expression name t env)) 
            (defenv.functions definition-env)))
    name))

(defun define-compile-time-symbol-macro (name expansion env)
  (let* ((definition-env (definition-environment env)))
    (if definition-env
      (push (cons name expansion) (defenv.symbol-macros definition-env)))
    name))


(defun fcomp-proclaim-type (type syms)
  (dolist (sym syms)
    (if (symbolp sym)
    (push (cons sym type) *nx-compile-time-types*)
      (warn "~S isn't a symbol in ~S type declaration while compiling ~S."
            sym type *fasl-source-file*))))

(defun compile-time-proclamation (specs env &aux  sym (defenv (definition-environment env)))
  (when defenv
    (dolist (spec specs)
      (setq sym (pop spec))
      (case sym
        (type
         (fcomp-proclaim-type (car spec) (cdr spec)))
        (special
         (dolist (sym spec)
           (push (cons (require-type sym 'symbol) nil) (defenv.specials defenv))))
        (notspecial
         (let ((specials (defenv.specials defenv)))
           (dolist (sym spec (setf (defenv.specials defenv) specials))
             (let ((pair (assq sym specials)))
               (when pair (setq specials (nremove pair specials)))))))
        (optimize
         (%proclaim-optimize spec))
        (inline
         (dolist (sym spec)
           (push (cons (maybe-setf-function-name sym) (cons 'inline 'inline)) (lexenv.fdecls defenv))))
        (notinline
         (dolist (sym spec)
           (unless (compiler-special-form-p sym)
             (push (cons (maybe-setf-function-name sym) (cons 'inline 'notinline)) (lexenv.fdecls defenv)))))
        (declaration
         (dolist (sym spec)
           (pushnew (require-type sym 'symbol) *nx-known-declarations*)))
        (ignore
         (dolist (sym spec)
           (push (cons (require-type sym 'symbol) t) *nx-proclaimed-ignore*)))
        (unignore
         (dolist (sym spec)
           (push (cons (require-type sym 'symbol) nil) *nx-proclaimed-ignore*)))
        (ftype 
         (let ((ftype (car spec))
               (fnames (cdr spec)))
           ;; ----- this part may be redundant, now that the lexenv.fdecls part is being done
           (if (and (consp ftype)
                    (consp fnames)
                    (eq (%car ftype) 'function))
             (dolist (fname fnames)
               (note-function-info fname nil env)))
           (dolist (fname fnames)
             (push (list* (maybe-setf-function-name fname) sym ftype) (lexenv.fdecls defenv)))))
        (otherwise
         (if (memq (if (consp sym) (%car sym) sym) *cl-types*)
           (fcomp-proclaim-type sym spec)       ; A post-cltl2 cleanup issue changes this
           nil)                         ; ---- probably ought to complain
         )))))

(defun fcomp-load-%defun (form env)
  (destructuring-bind (fn &optional doc) (cdr form)
    (unless *fasl-save-doc-strings*
      (if (consp doc)
        (if (and (eq (car doc) 'quote) (consp (cadr doc)))
          (setf (car (cadr doc)) nil))
        (setq doc nil)))
    (if (and (constantp doc)
             (setq fn (fcomp-function-arg fn env)))
      (progn
        (setq doc (eval-constant doc))
        (fcomp-output-form $fasl-defun env fn doc))
      (fcomp-random-toplevel-form form env))))

(defun fcomp-load-%macro (form env &aux fn doc)
  (verify-arg-count form 1 2)
  (if (and (constantp (setq doc (caddr form)))
           (setq fn (fcomp-function-arg (cadr form) env)))
    (progn
      (setq doc (eval-constant doc))
      (fcomp-output-form $fasl-macro env fn doc))
    (fcomp-random-toplevel-form form env)))

(defun define-compile-time-structure (sd refnames predicate env)
  (let ((defenv (definition-environment env)))
    (when defenv
      (setf (defenv.structures defenv) (alist-adjoin (sd-name sd) sd (defenv.structures defenv)))
      (let* ((structrefs (defenv.structrefs defenv)))
        (when (and (null (sd-type sd))
                   predicate)
          (setq structrefs (alist-adjoin predicate (sd-name sd) structrefs)))
        (dolist (slot (sd-slots sd))
          (unless (fixnump (ssd-name slot))
            (setq structrefs
                (alist-adjoin (if refnames (pop refnames) (ssd-name slot))
                              (ssd-type-and-refinfo slot)
                              structrefs))))
        (setf (defenv.structrefs defenv) structrefs)))))



(defun fcomp-transform (form env)
  (nx-transform form env))

(defun fcomp-random-toplevel-form (form env)
  (unless (constantp form)
    (unless (or (atom form) (compiler-special-form-p (%car form)))
      ;Pre-compile any lfun args.  This is an efficiency hack, since compiler
      ;reentering itself for inner lambdas tends to be more expensive than
      ;top-level compiles.
      ;This assumes the form has been macroexpanded, or at least none of the
      ;non-evaluated macro arguments could look like functions.
      (let (lfun (args (%cdr form)))
        (while args
          (multiple-value-bind (arg win) (fcomp-transform (%car args) env)
            (when (or (setq lfun (fcomp-function-arg arg env))
                      win)
              (when lfun (setq arg `',lfun))
              (labels ((subst-l (new ptr list)
                         (if (eq ptr list) (cons new (cdr list))
		             (cons (car list) (subst-l new ptr (%cdr list))))))
                (setq form (subst-l arg args form))))
            (setq args (%cdr args))))))
    (push form *fcomp-toplevel-forms*)))

(defun fcomp-function-arg (expr env)
  (when (consp expr)
    (if (and (eq (%car expr) 'nfunction)
             (symbolp (car (%cdr expr)))
             (lambda-expression-p (car (%cddr expr))))
      (fcomp-named-function (%caddr expr) (%cadr expr) env)
      (if (and (eq (%car expr) 'function)
               (lambda-expression-p (car (%cdr expr))))
        (fcomp-named-function (%cadr expr) nil env)))))

(defun fcomp-compile-toplevel-forms (env)
  (when *fcomp-toplevel-forms*
    (let* ((forms (nreverse *fcomp-toplevel-forms*))
           (*fcomp-stream-position* *fcomp-previous-position*)
           (lambda (if (null (cdr forms))
                     `(lambda () (progn ,@forms))
                     `(lambda ()
                        (macrolet ((load-time-value (value)
                                     (declare (ignore value))
                                     (compiler-function-overflow)))
                          ,@forms)))))
      (setq *fcomp-toplevel-forms* nil)
      ;(format t "~& Random toplevel form: ~s" lambda)
      (handler-case (fcomp-output-form
                     $fasl-lfuncall
                     env
                     (fcomp-named-function lambda nil env))
        (compiler-function-overflow ()
          (if (null (cdr forms))
            (error "Form ~s cannot be compiled - size exceeds compiler limitation"
                   (%car forms))
            ; else compile each half :
            (progn
              (dotimes (i (floor (length forms) 2))
                (declare (fixnum i))
                (push (pop forms) *fcomp-toplevel-forms*))
              (fcomp-compile-toplevel-forms env)
              (setq *fcomp-toplevel-forms* (nreverse forms))
              (fcomp-compile-toplevel-forms env))))))))

(defun fcomp-output-form (opcode env &rest args)
  (when *fcomp-toplevel-forms* (fcomp-compile-toplevel-forms env))
  (push (cons opcode args) *fcomp-output-list*))

;Compile a lambda expression for the sole purpose of putting it in a fasl
;file.  The result will not be funcalled.  This really shouldn't bother
;making an lfun, but it's simpler this way...
(defun fcomp-named-function (def name env)
  (let* ((env (new-lexical-environment env)))
    (multiple-value-bind (lfun warnings)
                         (compile-named-function
                          def name t
                          env
                          *fasl-save-definitions*
                          *fasl-save-local-symbols*
                          *default-file-compilation-policy*
                          cfasl-load-time-eval-sym
			  *fasl-target*)
      (fcomp-signal-or-defer-warnings warnings env)
      lfun)))

; For now, defer only UNDEFINED-FUNCTION-REFERENCEs, signal all others via WARN.
; Well, maybe not WARN, exactly.
(defun fcomp-signal-or-defer-warnings (warnings env)
  (let ((init (null *fcomp-warnings-header*))
        (some *fasl-warnings-signalled-p*)
        (harsh *fasl-non-style-warnings-signalled-p*))
    (dolist (w warnings)
      (setf (compiler-warning-file-name w) *fasl-source-file*)
      (setf (compiler-warning-stream-position w) *fcomp-stream-position*)
      (if (and (typep w 'undefined-function-reference) 
               (eq w (setq w (macro-too-late-p w env))))
        (push w *fasl-deferred-warnings*)
        (progn
          (multiple-value-setq (harsh some *fcomp-warnings-header*)
                               (signal-compiler-warning w init *fcomp-warnings-header* harsh some))
          (setq init nil))))
    (setq *fasl-warnings-signalled-p* some
          *fasl-non-style-warnings-signalled-p* harsh)))

; If W is an UNDEFINED-FUNCTION-REFERENCE which refers to a macro (either at compile-time in ENV
; or globally), cons up a MACRO-USED-BEFORE-DEFINITION warning and return it; else return W.

(defun macro-too-late-p (w env)
  (let* ((args (compiler-warning-args w))
         (name (car args)))
    (if (or (macro-function name)
            (let* ((defenv (definition-environment env))
                   (info (if defenv (assq name (defenv.functions defenv)))))
              (and (consp (cdr info))
                   (eq 'macro (cadr info)))))
      (make-instance 'macro-used-before-definition
        :file-name (compiler-warning-file-name w)
        :function-name (compiler-warning-function-name w)
        :warning-type ':macro-used-before-definition
        :args args)
      w)))


              
;;;;          fasl-scan - dumping reference counting
;;;;
;;;;
;These should be constants, but it's too much trouble when need to change 'em.
(defparameter FASL-FILE-ID #xFF00)  ;Overall file format, shouldn't change much
(defparameter FASL-VERSION #xFF3b)  ;Fasl block format.

(defvar *fasdump-hash*)
(defvar *fasdump-read-package*)
(defvar *fasdump-global-offsets*)
(defvar *make-load-form-hash*)

;Return a hash table containing subexp's which are referenced more than once.
(defun fasl-scan (forms)
  (let* ((*fasdump-hash* (make-hash-table :size (length forms)          ; Crude estimate
                                          :rehash-threshold 0.9
                                          :test 'eq))
         (*make-load-form-hash* (make-hash-table :test 'eq))
         (*fasdump-read-package* nil)
         (*fasdump-global-offsets* nil)
         (gsymbols nil))
    (dolist (op forms)
      (if (packagep op) ; old magic treatment of *package*
        (setq *fasdump-read-package* op)
        (dolist (arg (cdr op)) (fasl-scan-form arg))))

    #-bccl (when (eq *compile-verbose* :debug)
             (format t "~&~S forms, ~S entries -> "
                     (length forms)
                     (hash-table-count *fasdump-hash*)))
    (maphash #'(lambda (key val)
                 (when (%izerop val) (remhash key *fasdump-hash*)))
             *fasdump-hash*)
    #-bccl (when (eq *compile-verbose* :debug)
             (format t "~S." (hash-table-count *fasdump-hash*)))
    (values *fasdump-hash*
            gsymbols
            *fasdump-global-offsets*)))

;During scanning, *fasdump-hash* values are one of the following:
;  nil - form hasn't been referenced yet.
;   0 - form has been referenced exactly once
;   T - form has been referenced more than once
;  (load-form scanning-p referenced-p initform)
;     form should be replaced by load-form
;     scanning-p is true while we're scanning load-form
;     referenced-p is nil if unreferenced,
;                     T if referenced but not dumped yet,
;                     0 if dumped already (fasl-dump-form uses this)
;     initform is a compiled version of the user's initform
(defun fasl-scan-form (form)
  (when form
    (let ((info (gethash form *fasdump-hash*)))
      (cond ((null info)
             (fasl-scan-dispatch form))
            ((eql info 0)
             (puthash form *fasdump-hash* t))
            ((listp info)               ; a make-load-form form
             (when (cadr info)
               (error "Circularity in ~S for ~S" 'make-load-form form))
             (let ((referenced-cell (cddr info)))
               (setf (car referenced-cell) t)   ; referenced-p
               (setf (gethash (car info) *fasdump-hash*) t)))))))




(defun fasl-scan-dispatch (exp)
  (let ((type-code (typecode exp)))
    (declare (fixnum type-code))
    (case type-code
      (#.arch::tag-fixnum
       (fasl-scan-fixnum exp))
      (#.arch::tag-list (fasl-scan-list exp))
      (#.arch::tag-imm)
      (t
       (if (= (the fixnum (logand type-code arch::full-tag-mask)) arch::fulltag-immheader)            
           (case type-code
             ((#.arch::subtag-macptr #.arch::subtag-dead-macptr) (fasl-unknown exp))
             (t (fasl-scan-ref exp)))
           (case type-code
             ((#.arch::subtag-pool #.arch::subtag-weak #.arch::subtag-lock) (fasl-unknown exp))
             (#.arch::subtag-symbol (fasl-scan-ppc-symbol exp))
             ((#.arch::subtag-instance #.arch::subtag-struct)
              (fasl-scan-user-form exp))
             (#.arch::subtag-package (fasl-scan-ref exp))
             (#.arch::subtag-istruct
              (if (memq (uvref exp 0) *istruct-make-load-form-types*)
                (progn
                  (if (hash-table-p exp)
                    (fasl-lock-hash-table exp))
                  (fasl-scan-user-form exp))
                (fasl-scan-gvector exp)))
             (t (fasl-scan-gvector exp))))))))
              

(defun fasl-scan-ref (form)
  (puthash form *fasdump-hash* 0))

(defun fasl-scan-fixnum (fixnum)
  (unless (short-fixnum-p fixnum) (fasl-scan-ref fixnum)))

(defparameter *istruct-make-load-form-types*
  '(lexical-environment shared-library-descriptor shared-library-entry-point external-entry-point
    ctype unknown-ctype class-ctype foreign-ctype union-ctype member-ctype 
    array-ctype numeric-ctype hairy-ctype named-ctype constant-ctype args-ctype
    hash-table))




(defun fasl-scan-gvector (vec)
  (fasl-scan-ref vec)
  (dotimes (i (uvsize vec)) 
    (declare (fixnum i))
    (fasl-scan-form (%svref vec i))))

(defun funcall-lfun-p (form)
  (and (listp form)
       (eq (%car form) 'funcall)
       (listp (%cdr form))
       (or (functionp (%cadr form))
           (eql (typecode (%cadr form)) arch::subtag-xfunction))
       (null (%cddr form))))

(defun fasl-scan-list (list)
  (cond ((eq (%car list) cfasl-load-time-eval-sym)
         (let ((form (car (%cdr list))))
           (fasl-scan-form (if (funcall-lfun-p form)
                             (%cadr form)
                             form))))
        (t (when list
             (fasl-scan-ref list)
             (fasl-scan-form (%car list))
             (fasl-scan-form (%cdr list))))))

(defun fasl-scan-user-form (form)
  (multiple-value-bind (load-form init-form) (make-load-form form *fcomp-load-forms-environment*)
    (labels ((simple-load-form (form)
               (or (atom form)
                   (let ((function (car form)))
                     (or (eq function 'quote)
                         (and (symbolp function)
                              ;; using fboundp instead of symbol-function
                              ;; see comments in symbol-function
                              (or (functionp (fboundp function))
                                  (eq function 'progn))
                              ;; (every #'simple-load-form (cdr form))
                              (dolist (arg (cdr form) t)
                                (unless (simple-load-form arg)
                                  (return nil))))))))
             (load-time-eval-form (load-form form type)
               (cond ((quoted-form-p load-form)
                      (%cadr load-form))
                     ((self-evaluating-p load-form)
                      load-form)
                     ((simple-load-form load-form)
                      `(,cfasl-load-time-eval-sym ,load-form))
                     (t (multiple-value-bind (lfun warnings)
                                             (or
                                              (gethash load-form *make-load-form-hash*)
                                              (fcomp-named-function `(lambda () ,load-form) nil nil))
                          (when warnings
                            (cerror "Ignore the warnings"
                                    "Compiling the ~s ~a form for~%~s~%produced warnings."
                                    'make-load-form type form))
                          (setf (gethash load-form *make-load-form-hash*) lfun)
                          `(,cfasl-load-time-eval-sym (funcall ,lfun)))))))
      (declare (dynamic-extent #'simple-load-form #'load-time-eval-form))
      (let* ((compiled-initform
              (and init-form (load-time-eval-form init-form form "initialization")))
             (info (list (load-time-eval-form load-form form "creation")
                         T              ; scanning-p
                         nil            ; referenced-p
                         compiled-initform  ;initform-info
                         )))
        (puthash form *fasdump-hash* info)
        (fasl-scan-form (%car info))
        (setf (cadr info) nil)        ; no longer scanning load-form
        (when init-form
          (fasl-scan-form compiled-initform))))))

(defun fasl-scan-ppc-symbol (form)
  (fasl-scan-ref form)
  (fasl-scan-form (symbol-package form)))
  


;;;;          Pass 3 - dumping
;;;;
;;;;
(defvar *fasdump-epush*)
(defvar *fasdump-stream*)
(defvar *fasdump-eref*)

(defun fasl-dump-file (gnames goffsets forms hash filename)
  (let ((opened? nil)
        (finished? nil))
    (unwind-protect
      (with-open-file (*fasdump-stream* filename :direction :output
                                        :element-type '(unsigned-byte 8)
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
        (setq opened? t)
        (fasl-set-filepos 0)
        (fasl-out-word 0)             ;Will become the ID word
        (fasl-out-word 1)             ;One block in the file
        (fasl-out-long 12)            ;Block starts at file pos 12
        (fasl-out-long 0)             ;Length will go here
        (fasl-dump-block gnames goffsets forms hash)  ;Write the block
        (let ((pos (fasl-filepos)))
          (fasl-set-filepos 8)        ;Back to length longword
          (fasl-out-long (- pos 12))) ;Write length
        (fasl-set-filepos 0)          ;Seem to have won, make us legal
        (fasl-out-word FASL-FILE-ID)
        (setq finished? t)
        filename)
      (when (and opened? (not finished?))
        (delete-file filename)))))

(defun fasl-dump-block (gnames goffsets forms hash)
  (let ((etab-size (hash-table-count hash)))
    (when (> etab-size 65535)
      (error "Too many multiply-referenced objects in fasl file.~%Limit is ~d. Were ~d." 65535 etab-size))
    (fasl-out-word FASL-VERSION)          ; Word 0
    (fasl-out-long  0)
    (fasl-out-byte $fasl-etab-alloc)
    (fasl-out-long etab-size)
    (fasl-dump gnames goffsets forms hash)
    (fasl-out-byte $fasl-end)))

(defun fasl-dump (gnames goffsets forms hash)
  (let* ((*fasdump-hash* hash)
         (*fasdump-read-package* nil)
         (*fasdump-epush* nil)
         (*fasdump-eref* -1)
         (*fasdump-global-offsets* goffsets))
    (when gnames
      (fasl-out-byte $fasl-globals)
      (fasl-dump-form gnames))
    (dolist (op forms)
      (if (packagep op)
        (setq *fasdump-read-package* op)
        (progn
          (fasl-out-byte (car op))
          (dolist (arg (cdr op)) (fasl-dump-form arg)))))))

;During dumping, *fasdump-hash* values are one of the following:
;   nil - form has no load form, is referenced at most once.
;   fixnum - form has already been dumped, fixnum is the etab index.
;   T - form hasn't been dumped yet, is referenced more than once.
;  (load-form . nil) - form should be replaced by load-form.
(defun fasl-dump-form (form)
  (let ((info (gethash form *fasdump-hash*)))
    (cond ((fixnump info)
           (fasl-out-byte $fasl-eref)
           (fasl-out-word info))
          ((consp info)
           (fasl-dump-user-form form info))
          (t
           (setq *fasdump-epush* info)
           (fasl-dump-dispatch form)))))

(defun fasl-dump-user-form (form info)
  (let* ((load-form (car info))
         (referenced-p (caddr info))
         (initform (cadddr info)))
    (when referenced-p
      (unless (gethash load-form *fasdump-hash*)
        (error "~s was not in ~s.  This shouldn't happen." 'load-form '*fasdump-hash*)))
    (when initform
      (fasl-out-byte $fasl-prog1))      ; ignore the initform
    (fasl-dump-form load-form)
    (when referenced-p
      (setf (gethash form *fasdump-hash*) (gethash load-form *fasdump-hash*)))
    (when initform
      (fasl-dump-form initform))))

(defun fasl-out-opcode (opcode form)
  (if *fasdump-epush*
    (progn
      (setq *fasdump-epush* nil)
      (fasl-out-byte (fasl-epush-op opcode))
      (fasl-dump-epush form))
    (fasl-out-byte opcode)))

(defun fasl-dump-epush (form)
  #-bccl (when (fixnump (gethash form *fasdump-hash*))
           (error "Bug! Duplicate epush for ~S" form))
  (puthash form *fasdump-hash* (setq *fasdump-eref* (1+ *fasdump-eref*))))









(defun fasl-dump-dispatch (exp)
  (let* ((typecode (typecode exp)))
    (declare (fixnum typecode))
    (case typecode
      (#.arch::tag-fixnum (fasl-dump-fixnum exp))
      (#.arch::tag-list (fasl-dump-list exp))
      (#.arch::tag-imm (if (characterp exp) (fasl-dump-char exp) (fasl-dump-t_imm exp)))
      (t
       (if (= (the fixnum (logand typecode arch::fulltagmask)) arch::fulltag-immheader)
         ; Double-floats and simple-base-strings get special treatment.  For everything else,
         ; dump a $fasl-imm opcode, the subtag (typecode), and the raw data.  (For
         ; double-float vectors, skip the first 4 bytes of raw data.)
         ; ppc Code vectors have to be dumped in "normalized" form.
         (if (= typecode arch::subtag-double-float)
           (fasl-dump-dfloat exp)
           (let* ((n (uvsize exp))
		  (out-typecode (if (= typecode arch::subtag-xcode-vector) arch::subtag-code-vector typecode))
                  (nb (subtag-bytes typecode n)))
             (declare (fixnum n nb))
	     (if (and (not (eq *fasl-backend* *host-backend*))
		      (= typecode arch::subtag-code-vector))
		 (break "Dumping a native code-vector! ~s" exp))
             (if (= typecode arch::subtag-simple-base-string)
               (fasl-out-opcode $fasl-str exp)
               (progn
                 (fasl-out-opcode $fasl-ivec exp)
                 (fasl-out-byte out-typecode)))
             (fasl-out-size n)
             (if (= typecode arch::subtag-double-float-vector)
               ; Account for alignment word
               (fasl-out-ivect exp 4 (the fixnum (- nb 4)))
               (if (= out-typecode arch::subtag-code-vector)
                 (fasl-out-codevector exp nb)
                 (fasl-out-ivect exp 0 nb)))))
         (if (= typecode arch::subtag-package)
           (fasl-dump-package exp)
           (if (= typecode arch::subtag-symbol)
             (fasl-dump-symbol exp)
             (let* ((n (uvsize exp))
		    (out-typecode (if (= typecode arch::subtag-xfunction) arch::subtag-function typecode)))
               (declare (fixnum n))
	     (if (and (not (eq *fasl-backend* *host-backend*))
		      (= typecode arch::subtag-function))
		 (break "Dumping a native function! ~s" exp))
	       
               (fasl-out-opcode $fasl-gvec exp)
               (fasl-out-byte out-typecode)
               (fasl-out-size n)
               (dotimes (i n)
                 (fasl-dump-form (%svref exp i)))))))))))

(defun fasl-out-codevector (codevector size-in-bytes)
  (declare  (ignore size-in-bytes))
  (fasl-out-ivect codevector))
           


(defun fasl-dump-t_imm (imm)
  (fasl-out-opcode $fasl-timm imm)
  (fasl-out-long (%address-of imm)))

(defun fasl-dump-char (char)     ; << maybe not
  (let ((code (%char-code char)))
    (cond ((%i> code #xff)
           (fasl-out-opcode $fasl-xchar char)
           (fasl-out-word code))
          (t (fasl-out-opcode $fasl-char char)
             (fasl-out-byte code)))))

(defun fasl-dump-fixnum (fixnum)
  (if (short-fixnum-p fixnum)
    (progn
      (fasl-out-opcode $fasl-word-fixnum fixnum)
      (fasl-out-word fixnum))
    (progn
      (fasl-out-opcode $fasl-fixnum fixnum)
      (fasl-out-long fixnum))))
 


(defun fasl-dump-dfloat (float)
  (fasl-out-opcode $fasl-float float)
  (fasl-out-ivect float 4 8))
                                                    
                 
  


(defun fasl-dump-package (pkg)
  (let ((name (package-name pkg)))
    (if (simple-base-string-p name)
      (progn
        (fasl-out-opcode $fasl-pkg pkg)
        (fasl-out-string name))
      (progn
        (fasl-out-opcode $fasl-xpkg pkg)
        (fasl-out-xstring name)))))



(defun fasl-dump-list (list)
  (cond ((null list) (fasl-out-opcode $fasl-nil list))
        ((eq (%car list) cfasl-load-time-eval-sym)
         (let* ((form (car (%cdr list)))
                (opcode $fasl-eval))
           (when (funcall-lfun-p form)
             (setq opcode $fasl-lfuncall
                   form (%cadr form)))
           (if *fasdump-epush*
             (progn
               (fasl-out-byte (fasl-epush-op opcode))
               (fasl-dump-form form)
               (fasl-dump-epush list))
             (progn
               (fasl-out-byte opcode)
               (fasl-dump-form form)))))
        (t (fasl-dump-cons list))))

(defun fasl-dump-cons (cons &aux (end cons) (cdr-len 0))
  (declare (fixnum cdr-len))
  (while (and (consp (setq end (%cdr end)))
              (null (gethash end *fasdump-hash*)))
    (incf cdr-len))
  (if (eql 0 cdr-len)
    (fasl-out-opcode $fasl-cons cons)
    (progn
      (fasl-out-opcode (if end $fasl-list* $fasl-list) cons)
      ;; If the length doesn't fit in a 16-bit word, output
      ;; a word of 0 followed by a 32-bit length
      (if (> cdr-len #xffff)
	(progn
	  (fasl-out-word 0)
	  (fasl-out-long cdr-len))
	(fasl-out-word cdr-len))))
  (dotimes (i (the fixnum (1+ cdr-len)))
    (fasl-dump-form (%car cons))
    (setq cons (%cdr cons)))
  (when (or (eql 0 cdr-len) end)      ;cons or list*
    (fasl-dump-form end)))



(defun fasl-dump-symbol (sym &aux (pkg (symbol-package sym))
                                  (name (symbol-name sym))
                                  (base (simple-base-string-p name)))
  (cond ((null pkg) 
         (if base
           (progn 
             (fasl-out-opcode $fasl-mksym sym)
             (fasl-out-string name))
           (progn
             (fasl-out-opcode $fasl-mkxsym sym)
             (fasl-out-xstring name))))

        (*fasdump-epush*
         (if base
           (progn
             (fasl-out-byte (fasl-epush-op $fasl-pkg-intern))
             (fasl-dump-form pkg)
             (fasl-dump-epush sym)
             (fasl-out-string name))
           (progn
             (fasl-out-byte (fasl-epush-op $fasl-pkg-xintern))
             (fasl-dump-form pkg)
             (fasl-dump-epush sym)
             (fasl-out-xstring name))))
        (t
         (if base
           (progn
             (fasl-out-byte $fasl-pkg-intern)
             (fasl-dump-form pkg)
             (fasl-out-string name))
           (progn
             (fasl-out-byte $fasl-pkg-xintern)
             (fasl-dump-form pkg)
             (fasl-out-xstring name))))))


(defun fasl-unknown (exp)
  (error "Can't dump ~S - unknown type" exp))

(defun fasl-out-string (str)
  (fasl-out-size (length str))
  (fasl-out-ivect str))

(defun fasl-out-xstring (str) 
  ; really the same as fasl-out-string wherein byte-size = length
  ; could save 40 bytes or so by exploiting that
  (fasl-out-size (subtag-bytes arch::subtag-simple-general-string (length str)))
  (fasl-out-ivect str))



(defun fasl-out-size (size)
  (if (%i< size #xFF) (fasl-out-byte size)
      (progn (fasl-out-byte #xFF)
             (if (%i< size #xFFFF) (fasl-out-word size)
                 (progn (fasl-out-word #xFFFF) (fasl-out-long size))))))

(defun fasl-out-ivect (iv &optional 
                          (start 0) 
                          (nb 
			   (subtag-bytes (typecode iv) (uvsize iv))))
  (stream-write-ivector *fasdump-stream* iv start nb))


(defun fasl-out-long (long)
  (fasl-out-word (ash long -16))
  (fasl-out-word (logand long #xFFFF)))

(defun fasl-out-word (word)
  (fasl-out-byte (%ilsr 8 word))
  (fasl-out-byte word))

(defun fasl-out-byte (byte)
  (write-byte (%ilogand2 byte #xFF) *fasdump-stream*))

(defun fasl-filepos ()
  (file-position *fasdump-stream*))

(defun fasl-set-filepos (pos)
  (file-position *fasdump-stream* pos)
  #-bccl (unless (eq (file-position *fasdump-stream*) pos)
           (error "Unable to set file position to ~S" pos)))

;;; Concatenate fasl files.

;;; Format of a fasl file as expected by the fasloader.
;;;
;;; #xFF00         2 bytes - File version
;;; Block Count    2 bytes - Number of blocks in the file
;;; addr[0]        4 bytes - address of 0th block
;;; length[0]      4 bytes - length of 0th block
;;; addr[1]        4 bytes - address of 1st block
;;; length[1]      4 bytes - length of 1st block
;;; ...
;;; addr[n-1]      4 bytes
;;; length[n-1]    4 bytes
;;; length[0] + length[1] + ... + length [n-1] bytes of data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (fasl-concatenate out-file fasl-files &key :if-exists)
;;
;; out-file     name of file in which to store the concatenation
;; fasl-files   list of names of fasl files to concatenate
;; if-exists    as for OPEN, defaults to :error
;;
;; function result: pathname to the output file.
;; It works to use the output of one invocation of fasl-concatenate
;; as an input of another invocation.
;;
(defun fasl-concatenate (out-file fasl-files &key (if-exists :error))
  (%fasl-concatenate out-file fasl-files if-exists (pathname-type *.fasl-pathname*)))

(defun %fasl-concatenate (out-file fasl-files if-exists file-ext)
  (let ((count 0)
        (created? nil)
        (finished? nil)
	(ext-pathname (make-pathname :type file-ext)))
    (declare (fixnum count))
    (flet ((fasl-read-halfword (f)
	     (dpb (read-byte f) (byte 8 8) (read-byte f)))
	   (fasl-write-halfword (h f)
	     (write-byte (ldb (byte 8 8) h) f)
	     (write-byte (ldb (byte 8 0) h) f)
	     h))
      (flet ((fasl-read-fullword (f)
	       (dpb (fasl-read-halfword f) (byte 16 16) (fasl-read-halfword f)))
	     (fasl-write-fullword (w f)
	       (fasl-write-halfword (ldb (byte 16 16) w) f)
	       (fasl-write-halfword (ldb (byte 16 0) w) f)
	       w))
	(dolist (file fasl-files)
	  (setq file (merge-pathnames file ext-pathname))
	  (unless (equal (pathname-type file) file-ext)
	    (error "Not a ~A file: ~s" file-ext file))
	  (with-open-file (instream file :element-type '(unsigned-byte 8))
	    (unless (eql fasl-file-id (fasl-read-halfword instream))
	      (error "Bad ~A file ID in ~s" file-ext file))
	    (incf count (fasl-read-halfword instream))))
	(unwind-protect
	     (with-open-file (outstream
			      (setq out-file (merge-pathnames out-file ext-pathname))
			      :element-type '(unsigned-byte 8)
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists if-exists)
	       (setq created? t)
	       (let ((addr-address 4)
		     (data-address (+ 4 (* count 8))))
		 (fasl-write-halfword 0 outstream) ;  will be $fasl-id
		 (fasl-write-halfword count outstream)
		 (dotimes (i (* 2 count))
		   (fasl-write-fullword 0 outstream)) ; for addresses/lengths
		 (dolist (file fasl-files)
		   (with-open-file (instream (merge-pathnames file ext-pathname)
					     :element-type '(unsigned-byte 8))
		     (fasl-read-halfword instream) ; skip ID
		     (let* ((fasl-count (fasl-read-halfword instream))
			    (addrs (make-array fasl-count))
			    (sizes (make-array fasl-count))
			    addr0)
		       (declare (fixnum fasl-count)
				(dynamic-extent addrs sizes))
		       (dotimes (i fasl-count)
			 (setf (svref addrs i) (fasl-read-fullword instream)
			       (svref sizes i) (fasl-read-fullword instream)))
		       (setq addr0 (svref addrs 0))
		       (file-position outstream addr-address)
		       (dotimes (i fasl-count)
			 (fasl-write-fullword
			  (+ data-address (- (svref addrs i) addr0))
			  outstream)
			 (fasl-write-fullword (svref sizes i) outstream)
			 (incf addr-address 8))
		       (file-position outstream data-address)
		       (dotimes (i fasl-count)
			 (file-position instream (svref addrs i))
			 (let ((fasl-length (svref sizes i)))
			   (dotimes (j fasl-length)
			     (write-byte (read-byte instream) outstream))
			   (incf data-address fasl-length))))))
		 (stream-length outstream data-address)
		 (file-position outstream 0)
		 (fasl-write-halfword fasl-file-id outstream)
		 (setq finished? t)))
	  (when (and created? (not finished?))
	    (delete-file out-file))))
      out-file)))

(provide 'nfcomp)

