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

(in-package "CCL")

;L1-readloop.lisp


(defvar *break-on-signals* nil)
(defvar *break-on-warnings* nil)
(defvar *break-on-errors* t "Not CL.")
(defvar *debugger-hook* nil)
(defvar *backtrace-on-break* nil)
(defvar *** nil)
(defvar ** nil)
(defvar * nil)
(defvar /// nil)
(defvar // nil)
(defvar / nil)
(defvar +++ nil)
(defvar ++ nil)
(defvar + nil)
(defvar - nil)

(defvar *continuablep* nil)
(defvar *in-read-loop* nil 
 "Not CL. Is T if waiting for input in the read loop")
(defvar *listener-p* nil
  "Bound true by READ-LOOP. This is how we tell if a process is a Listener")

(defparameter *inhibit-error* nil "If non-nil, ERROR just throws")
(defvar *did-startup* nil)


(defun process-is-listener-p (process)
  (symbol-value-in-process '*listener-p* process))

(defun process-is-toplevel-listener-p (process)
  (and (symbol-value-in-process '*in-read-loop* process)
       (eql 0 (symbol-value-in-process '*break-level* process))))


(defmacro catch-cancel (&body body)
  `(catch :cancel ,@body))

(defmacro throw-cancel (&optional value)
  `(throw :cancel ,value))

(defun toplevel ()
  (throw :toplevel nil))

; This is the old way we did this.
; It has the drawback that it doesn't throw out,
; just restarts the process without cleaning up.
#|
      (progn
        (process-interrupt *initial-process*
                           #'(lambda (p)
                               (let ((function.args (process.initial-form p)))
                                 (apply #'process-preset
                                        p
                                        (car function.args)
                                        (cdr function.args))))
                           p)
        (loop
          (suspend-current-process))))))
|#

(defun cancel ()
 (throw :cancel :cancel))

; It's not clear that this is the right behavior, but aborting CURRENT-PROCESS -
; when no one's sure just what CURRENT-PROCESS is - doesn't seem right either.
(defun interactive-abort ()
  (interactive-abort-in-process *current-process*))

(defun interactive-abort-in-process (p)
  (if p (process-interrupt p 
                           #'(lambda ()
                               (unless *inhibit-abort*
                                 (lds (if *in-read-loop* 
                                        (abort-break)
                                        (abort))
                                      (abort))
                                 )))))


; What process-to-abort does now (5/5/95):
; - all processes idling: cmd-. & opt-cmd-. abort event-processor
; - one process busy: cmd-. aborts the busy process; opt-cmd-. gives dialog
; - two or more processes busy: cmd-. & opt-cmd-. gives dialog
; (a busy process is a non-idling listener, or any other that's not event-processor)

#+notyet
(defun process-to-abort (what)
  (let ((l (mapcan #'(lambda (x)
                       (unless (or (process-exhausted-p x)
                                   (not (find-restart-in-process 'abort x))
                                   ; idling listeners:
                                   #|
                                   (and (symbol-value-in-process '*in-read-loop* x)
                                        (eq 0 (symbol-value-in-process '*break-level* x)))|#
                                   )
                         (list x)))
                   (reverse *active-processes*))))
    (cond
      ((null (cdr l)) (car l)) ; *current-process*
      ((and (null (cddr l))
            (not (option-key-p)))
       (if (eq (car l) *event-processor*) (cadr l) (car l)))
      (t (let ((p (catch-cancel
                    (select-item-from-list l
                                           :window-title what
                                           :help-spec 15010
                                           :list-spec 15011
                                           :button-spec 15013))))
           (if (neq p :cancel) (car p)))))))

(defun abort (&optional condition)
  (invoke-restart-no-return (find-restart 'abort condition)))

(defun continue (&optional condition)
  (let ((r (find-restart 'continue condition)))
    (if r (invoke-restart r))))

(defun muffle-warning (&optional condition)
  (invoke-restart-no-return (find-restart 'muffle-warning condition)))

(defun abort-break ()
  (invoke-restart-no-return 'abort-break))

#| Doing it this way prevents abort from clearing input in the listener
(defun abort-break ()
  (let ((res (find-restart-2 'abort)))
    (if  res (invoke-restart-no-return res) (abort))))

; find second restart
(defun find-restart-2 (name &aux res)
  (dolist (cluster %restarts% res)
    (dolist (restart cluster)
      (when (eq (restart-name restart) name)                 
	(if res (return-from find-restart-2 restart)(setq res restart))))))
|#

(defglobal *quit-acknowledge* nil)
(defglobal *quit-acknowledge-lock* (make-lock))

(defun quit (&optional (exit-status 0))
  (let* ((ip *initial-process*)
	 (cp *current-process*))
    (when (process-verify-quit ip)
      (process-interrupt ip
			 #'(lambda ()
			     (process-exit-application *current-process*
				   #'(lambda ()
				       (#_exit exit-status)))))
      (unless (eq cp ip)
	(when (try-lock *quit-acknowledge-lock*)
	  (let-globally ((*quit-acknowledge* (make-semaphore)))
			(timed-wait-on-semaphore *quit-acknowledge* 1)))
	(process-kill cp)))))




(defglobal *quitting* nil)




(defun prepare-to-quit (&optional part)
  (when *quit-acknowledge* (signal-semaphore *quit-acknowledge*))
  (let-globally ((*quitting* t))
    (when (or (null part) (eql 0 part))
      (dolist (f *lisp-cleanup-functions*)
	(funcall f)))
    (let* ((stragglers ()))
      (dolist (p (all-processes))
	(unless (or (eq p *initial-process*)
		    (not (process-active-p p)))
	  (if (process-persistent p)
	    (process-reset p :shutdown)
	    (process-kill p))))
      (dolist (p (all-processes))
        (unless (eq p *initial-process*)
          (unless (process-wait-with-timeout
                   "Shutdown wait"
                   5
                   #'process-exhausted-p
                   p)
            (push p stragglers))))
      (dolist (p stragglers)
	(unless (process-wait-with-timeout
		 "deathwatch"
		 (* 5 *ticks-per-second*)
		 #'(lambda () (process-exhausted-p p)))
	  (maybe-finish-process-kill p :kill))))
    (shutdown-lisp-threads)
    (while *open-file-streams*
      (close (car *open-file-streams*)))
    (setf (interrupt-level) -1)       ; can't abort after this
    ))







;; Application classes

(defstruct command-line-argument
  keyword
  help-string
  option-char
  long-name
  may-take-operand
  allow-multiple			; option can appear multiple times
)

(defvar *standard-help-argument*
  (make-command-line-argument
   :keyword :help
   :help-string "this text"
   :option-char #\h
   :long-name "help"))

(defvar *standard-version-argument*
  (make-command-line-argument
   :keyword :version
   :help-string "print (LISP-APPLICATION-VERSION) and exit"
   :option-char #\V
   :long-name "version"))

(defclass application ()
    ((command-line-arguments
      :initform
      (list *standard-help-argument* *standard-version-argument*))))
       
			     
(defun %usage-exit (banner exit-status other-args)
  (with-cstrs ((banner banner)
	       (other-args other-args))
    (ff-call (%kernel-import ppc32::kernel-import-usage-exit)
	     :address banner
	     :signed-fullword exit-status
	     :address other-args
	     :void)))

;;; Returns three values: error-flag, options-alist, non-option-arguments
(defmethod parse-application-arguments ((a application))
  (let* ((cla (slot-value a 'command-line-arguments))
	 (vals (cdr *command-line-argument-list*))
	 (options ())
	 (non-options ()))
    (do* ()
	 ((null vals)
	  (values nil (nreverse options) (nreverse non-options)))
      (let* ((val (pop vals))
	     (val-len (length val))
	     (short-p nil)
	     (option
	      (if (and (>= val-len 2)
		       (eql (schar val 0) #\-))
		(if (eql (schar val 1) #\-)
		  (find val cla
			:key #'command-line-argument-long-name
			:test #'(lambda (k v) (string= k v :start1 2)))
		  (progn
		    (setq short-p t)
		    (find (schar val 1) cla
			  :key #'command-line-argument-option-char))))))
	(if (null option)
	  (if (and (>= val-len 1)
		   (eql (schar val 0) #\-))
	    (return (values :unknown-option val nil))
	    (push val non-options))	;non-option argument
	  ;; We recognized the option.  Is it a duplicate of
	  ;; something already seen?
	  (let* ((key (command-line-argument-keyword option))
		 (operand nil))
	    (when (and (assoc key options)
		       (not (command-line-argument-allow-multiple option)))
	      (return (values :duplicate-option val nil)))
	    (when (command-line-argument-may-take-operand option)
	      ;; A short option name can be followed by the operand,
	      ;; without intervening whitespace.
	      (if (and short-p (> val-len 2))
		(setq operand (subseq val 2))
		(if vals
		  (setq operand (pop vals))
		  (return (values :missing-operand val nil)))))
	    (push (cons key operand) options)))))))

(defmethod summarize-option-syntax ((a application))
  (flet ((summarize-option (o)
	   (format nil "~8t-~a, --~a : ~a~%"
		   (command-line-argument-option-char o)
		   (command-line-argument-long-name o)
		   (command-line-argument-help-string o))))
    (format nil "~{~a~}" (mapcar #'summarize-option
				 (slot-value a 'command-line-arguments)))))

  
;;; Process the "help" option, report parsing errors.
(defmethod process-application-arguments ((a application) error-flag opts args)
  (declare (ignore args))
  (if (null error-flag)
    (if (assoc :help opts)
      (%usage-exit "" 0 (summarize-option-syntax a))
      (if (assoc :version opts)
	(progn
	  (format t "~&~a~&" (application-version-string a))
	  (force-output t)
	  (#_exit 0))))
    (%usage-exit
     (format nil
	     (case error-flag
	       (:missing-argument "Missing argument to ~a option")
	       (:duplicate-argument "Duplicate ~a option")
	       (:unknown-option "Unknown option: ~a")
	       (t "~a"))
	     opts)
     #$EX_USAGE
     (summarize-option-syntax a))))
	       

;;; an example method to base a specialization on
(defmethod toplevel-function ((a application) init-file)
  (declare (ignore init-file))
  (multiple-value-bind (error-flag options args)
      (parse-application-arguments a)
    (process-application-arguments a error-flag options args)))

(defmethod application-version-string ((a application))
  "Return a string which (arbitrarily) represents the application version.
Default version returns OpenMCL version info."
  (format nil "~&~d.~d~@[.~d~]~@[-~a~]~&"
	  *openmcl-major-version*
	  *openmcl-minor-version*
	  (unless (zerop *openmcl-revision*)
	    *openmcl-revision*)
	  *openmcl-suffix*))


(defun find-restart-in-process (name p)
  (without-interrupts
   (let ((restarts (symbol-value-in-process '%restarts% p)))
     (dolist (cluster restarts)
       (dolist (restart cluster)
         (when (and (or (eq restart name) (eq (restart-name restart) name)))
           (return-from find-restart-in-process restart)))))))



; specialize this for your application
(defmethod open-application ((self application) startup)
  (declare (ignore startup))
  nil)
  
; specialize this for your application
(defmethod open-application-document ((a application) path &optional startup)
  (declare (ignore path startup)))

(defmethod application-name          ((app application)) nil)
(defmethod application-init-file     ((app application)) nil)


(defclass lisp-development-system (application) 
  ((command-line-arguments
    :initform
    (list *standard-help-argument*
	  *standard-version-argument*
	  (make-command-line-argument
	   :option-char #\n
	   :long-name "no-init"
	   :keyword :noinit
	   :help-string "suppress loading of init file")
	  (make-command-line-argument
	   :option-char #\e
	   :long-name "eval"
	   :keyword :eval
	   :help-string "evaluate <form> (may need to quote <form> in shell)"
	   :may-take-operand t
	   :allow-multiple t)
	  (make-command-line-argument
	   :option-char #\l
	   :long-name "load"
	   :keyword :load
	   :help-string "load <file>"
	   :may-take-operand t
	   :allow-multiple t)
	  (make-command-line-argument
	   :option-char #\T
	   :long-name "set-lisp-heap-gc-threshold"
	   :help-string "set lisp-heap-gc-threshold to <n>"
	   :keyword :gc-threshold
	   :may-take-operand t
	   :allow-multiple nil)))))

(defparameter *application*
  (make-instance 'lisp-development-system))

(defvar *load-lisp-init-file* t)
(defvar *lisp-startup-parameters* ())

(defmethod process-application-arguments ((a lisp-development-system)
					  error-flag options args)
  (declare (ignorable error-flag))
  (call-next-method)			; handle help, errors
  (if args
    (%usage-exit (format nil "Unrecognized non-option arguments: ~a" args)
		 #$EX_USAGE
		 (summarize-option-syntax a))
    (setq *load-lisp-init-file* (not (assoc :noinit options))
	  *lisp-startup-parameters*
	  (mapcan #'(lambda (x)
		      (and (member (car x) '(:load :eval :gc-threshold)) (list x)))
		  options))))
	

(defmethod toplevel-function ((a lisp-development-system) init-file)
  (call-next-method)
  (let* ((lockptr (recursive-lock-ptr *terminal-input-lock*)))
    (make-mcl-listener-process
     "listener"
     *terminal-input*
     *terminal-output*
     #'(lambda () (%unlock-recursive-lock lockptr))
     #'(lambda ()
	 (%lock-recursive-lock lockptr)
	 (setq *interactive-abort-process*
	       *current-process*)
	 (startup-ccl (and *load-lisp-init-file* init-file))
	 (listener-function)
	 nil)
     nil)
    (%unlock-recursive-lock lockptr))
  (%set-toplevel #'(lambda ()
		     (loop
			 (%nanosleep *periodic-task-seconds* *periodic-task-nanoseconds*)
			 (housekeeping))))
  (toplevel))

(defmethod application-file-creator ((app lisp-development-system))
  *ccl-file-creator*)

(defmethod application-init-file ((app lisp-development-system))
  "home:openmcl-init")


; redefined by hide-listener-support
(defmethod application-error ((a application) condition error-pointer)
  (declare (ignore condition error-pointer))
  (quit))

(defun error-header (kind)
  (let ((pname (process-name *current-process*)))
    (if (and pname (not (string-equal pname *main-listener-process-name*)))
      (format nil "~A in process ~A(~d)" kind pname
	      (process-serial-number *current-process*))
      (format nil "~A" kind))))

(defun signal (condition &rest args)
  (setq condition (condition-arg condition args 'simple-condition))
  (lds
   (let* ((*break-on-signals* *break-on-signals*))
     (let* ((old-bos *break-on-signals*))
       (when (unknown-ctype-p (let* ((*break-on-signals* nil)) (specifier-type old-bos)))
	 (setq *break-on-signals* nil)
	 (warn "~S : Ignoring invalid type specifier ~s." '*break-on-signals old-bos)))
	 
   (when (typep condition *break-on-signals*)
     (let ((*break-on-signals* nil))
       (cbreak-loop "Signal" "Signal the condition." condition (%get-frame-ptr))))))
  (let ((%handlers% %handlers%))
    (while %handlers%
      (do* ((tag (pop %handlers%)) (handlers tag (cddr handlers)))
           ((null handlers))
        (when (typep condition (car handlers))
          (let ((fn (cadr handlers)))
            (cond ((null fn) (throw tag condition))
                  ((fixnump fn) (throw tag (cons fn condition)))
                  (t (funcall fn condition)))))))))

(defvar *error-print-circle* nil)   ; reset to T when we actually can print-circle



;;;***********************************
;;;Mini-evaluator
;;;***********************************

(defun new-lexical-environment (&optional parent)
  (%istruct 'lexical-environment parent nil nil nil nil nil nil))

(defmethod make-load-form ((e lexical-environment) &optional env)
  (declare (ignore env))
  nil)

(defun new-definition-environment (&optional (type 'compile-file))
  (%istruct 'definition-environment (list type)  nil nil nil nil nil nil nil nil nil nil nil nil ))

(defun definition-environment (env &optional clean-only &aux parent)
  (if (and env (not (istruct-typep env 'lexical-environment))) (report-bad-arg env 'lexical-environment))
  (do* () 
       ((or (null env) 
            (listp (setq parent (lexenv.parent-env env)))
            (and clean-only (or (lexenv.variables env) (lexenv.functions env)))))
    (setq env parent))
  (if (consp parent)
    env))

(defvar *symbol-macros* (make-hash-table :test #'eq))

(defun %define-symbol-macro (name expansion)
  (if (or (constant-symbol-p name)
	  (proclaimed-special-p name))
      (signal-program-error "Symbol ~s already globally defined as a ~A"
			    name (if (constant-symbol-p name)
				     'constant
				     'variable)))
  (setf (gethash name *symbol-macros*) expansion)
  name)

(defvar *macroexpand-hook* 'funcall) ; Should be #'funcall. 
;(queue-fixup (setq *macroexpand-hook* #'funcall)) ;  No it shouldn't.

(defun %symbol-macroexpand-1 (sym env)
  (if (and env (not (istruct-typep env 'lexical-environment)))
      (report-bad-arg env 'lexical-environment))
  (do* ((env env (lexenv.parent-env env)))
       ((null env))
    (if (eq (%svref env 0) 'definition-environment)
	(let* ((info (assq sym (defenv.symbol-macros env))))
	  (if info
	    (return-from %symbol-macroexpand-1 (values (cdr info) t))
	    (return)))
	(let* ((vars (lexenv.variables env)))
	  (when (consp vars)
	    (let* ((info (dolist (var vars)
			   (if (eq (var-name var) sym)
			       (return var)))))            
	      (when info
		(if (and (consp (setq info (var-expansion info)))
			 (eq (%car info) :symbol-macro))
		    (return-from %symbol-macroexpand-1 (values (%cdr info) t))
		    (return-from %symbol-macroexpand-1 (values sym nil)))))))))
  ;; Look it up globally.
  (multiple-value-bind (expansion win) (gethash sym *symbol-macros*)
    (if win (values expansion t) (values sym nil))))

(defun macroexpand-1 (form &optional env &aux fn)
  (declare (resident))
  (if (and (consp form)
           (symbolp (%car form)))
    (if (setq fn (macro-function (%car form) env))
      (values (funcall *macroexpand-hook* fn form env) t)
      (values form nil))
    (if (and form (symbolp form))
      (%symbol-macroexpand-1 form env)
      (values form nil))))

(defun macroexpand (form &optional env)
  (declare (resident))
  (multiple-value-bind (new win) (macroexpand-1 form env)
    (do* ((won-at-least-once win))
         ((null win) (values new won-at-least-once))
      (multiple-value-setq (new win) (macroexpand-1 new env)))))

(defun %symbol-macroexpand (form env &aux win won)
  ; Keep expanding until no longer a symbol-macro or no longer a symbol.
  (loop
    (unless (and form (symbolp form)) (return))
    (multiple-value-setq (form win) (macroexpand-1 form env))
    (if win (setq won t) (return)))
  (values form won))

(defun retain-lambda-expression (name lambda-expression env)
  (if (and (let* ((lambda-list (cadr lambda-expression)))
             (and (not (memq '&lap lambda-list))
                  (not (memq '&method lambda-list))
                  (not (memq '&lexpr lambda-list))))
           (nx-declared-inline-p name env)
           (not (gethash name *nx1-alphatizers*))
           ; A toplevel definition defined inside a (symbol-)macrolet should
           ; be inlineable.  It isn't; call DEFINITION-ENVIRONMENT with a
           ; "clean-only" argument to ensure that there are no lexically
           ; bound macros or symbol-macros.
           (definition-environment env t))
    lambda-expression))

; This is different from AUGMENT-ENVIRONMENT.
; If "info" is a lambda expression, then
;  record a cons whose CAR is (encoded-lfun-bits . keyvect) and whose cdr
;  is the lambda expression iff the function named by "name" is 
;  declared/proclaimed INLINE in env
(defun note-function-info (name lambda-expression env)
  (let ((definition-env (definition-environment env)))
    (if definition-env
      (let* ((already (assq (setq name (maybe-setf-function-name name))
                            (defenv.defined definition-env)))
             (info nil))
        (when (lambda-expression-p lambda-expression)
          (multiple-value-bind (lfbits keyvect) (encode-lambda-list lambda-expression t)
            (setq info (cons (cons lfbits keyvect) 
                             (retain-lambda-expression name lambda-expression env)))))
          (if already
            (if info (%rplacd already info))
            (push (cons name info) (defenv.defined definition-env)))))
    name))

; And this is different from FUNCTION-INFORMATION.
(defun retrieve-environment-function-info (name env)
 (let ((defenv (definition-environment env)))
   (if defenv (assq (maybe-setf-function-name name) (defenv.defined defenv)))))

(defun maybe-setf-function-name (name)
  (if (and (consp name) (eq (car name) 'setf))
    (setf-function-name (cadr name))
    name))

; Must differ from -something-, but not sure what ... 
(defun note-variable-info (name info env)
  (let ((definition-env (definition-environment env)))
    (if definition-env (push (cons name info) (defenv.specials definition-env)))
    name))

(defun compile-file-environment-p (env)
  (let ((defenv (definition-environment env)))
    (and defenv (eq 'compile-file (car (defenv.type defenv))))))

(defun cheap-eval (form)
  (cheap-eval-in-environment form nil))

; used by nfcomp too
; Should preserve order of decl-specs; it sometimes matters.
(defun decl-specs-from-declarations (declarations)
  (let ((decl-specs nil))
    (dolist (declaration declarations decl-specs)
      ;(unless (eq (car declaration) 'declare) (say "what"))
      (dolist (decl-spec (cdr declaration))
        (setq decl-specs (nconc decl-specs (list decl-spec)))))))

(defun cheap-eval-in-environment (form env &aux sym)
  (declare (resident))
  (flet ((progn-in-env (body&decls parse-env base-env)
           (multiple-value-bind (body decls) (parse-body body&decls parse-env)
             (setq base-env (augment-environment base-env :declare (decl-specs-from-declarations decls)))
             (while (cdr body)
               (cheap-eval-in-environment (pop body) base-env))
             (cheap-eval-in-environment (car body) base-env))))
    (if form
      (cond ((symbolp form) 
             (multiple-value-bind (expansion win) (macroexpand-1 form env)
               (if win 
                 (cheap-eval-in-environment expansion env) 
                 (let* ((defenv (definition-environment env))
                        (constant (if defenv (assq form (defenv.constants defenv))))
                        (constval (%cdr constant)))
                   (if constant
                     (if (neq (%unbound-marker-8) constval)
                       constval
                       (error "Can't determine value of constant symbol ~s" form))
                     (if (constant-symbol-p form)
                       (%sym-global-value form)
                       (symbol-value form)))))))
            ((atom form) form)
            ((eq (setq sym (%car form)) 'quote)
             (verify-arg-count form 1 1)
             (%cadr form))
            ((eq sym 'function)
             (verify-arg-count form 1 1)
             (cond ((symbolp (setq sym (%cadr form)))
                    (%function sym))
                   ((and (consp sym) (eq (%car sym) 'setf) (consp (%cdr sym)) (null (%cddr sym)))
                    (%function (setf-function-name (%cadr sym))))
                   (t (%make-function nil sym env))))
            ((eq sym 'nfunction)
             (verify-arg-count form 2 2)
             (%make-function (%cadr form) (%caddr form) env))
            ((eq sym 'progn) (progn-in-env (%cdr form) env env))
            ((eq sym 'setq)
             (if (not (%ilogbitp 0 (list-length form)))
               (verify-arg-count form 0 0)) ;Invoke a "Too many args" error.
             (let* ((sym nil)
                    (val nil))
               (while (setq form (%cdr form))
                 (setq sym (require-type (pop form) 'symbol))
                 (multiple-value-bind (expansion expanded)
                                      (macroexpand-1 sym env)
                   (if expanded
                     (setq val (cheap-eval-in-environment `(setf ,expansion ,(%car form)) env))
                     (set sym (setq val (cheap-eval-in-environment (%car form) env))))))
               val))
            ((eq sym 'eval-when)
             (destructuring-bind (when . body) (%cdr form)
               (when (or (memq 'eval when) (memq :execute when)) (progn-in-env body env env))))
            ((eq sym 'if)
             (destructuring-bind (test true &optional false) (%cdr form)
               (cheap-eval-in-environment (if (cheap-eval-in-environment test env) true false) env)))
            ((eq sym 'locally) (progn-in-env (%cdr form) env env))
            ((eq sym 'symbol-macrolet)
	     (multiple-value-bind (body decls) (parse-body (cddr form) env)
	       (progn-in-env body env (augment-environment env :symbol-macro (cadr form) :declare (decl-specs-from-declarations decls)))))
            ((eq sym 'macrolet)
             (let ((temp-env (augment-environment env
                                                  :macro 
                                                  (mapcar #'(lambda (m)
                                                              (destructuring-bind (name arglist &body body) m
                                                                (list name (enclose (parse-macro name arglist body env)
                                                                                    env))))
                                                          (cadr form)))))
               (progn-in-env (cddr form) temp-env temp-env)))
            ((and (symbolp sym) 
                  (compiler-special-form-p sym)
                  (not (functionp (fboundp sym))))
             (if (eq sym 'unwind-protect)
               (destructuring-bind (protected-form . cleanup-forms) (cdr form)
                 (unwind-protect
                   (cheap-eval-in-environment protected-form env)
                   (progn-in-env cleanup-forms env env)))
               (funcall (%make-function nil `(lambda () (progn ,form)) env))))
            ((and (symbolp sym) (macro-function sym env))
             (if (eq sym 'step)
               (let ((*compile-definitions* nil))
                     (cheap-eval-in-environment (macroexpand-1 form env) env))
               (cheap-eval-in-environment (macroexpand-1 form env) env)))
            ((or (symbolp sym)
                 (and (consp sym) (eq (%car sym) 'lambda)))
             (let ((args nil))
               (dolist (elt (%cdr form)) (push (cheap-eval-in-environment elt env) args))
               (apply #'call-check-regs (if (symbolp sym) sym (%make-function nil sym env))
                      (nreverse args))))
            (t (signal-simple-condition 'simple-program-error "Car of ~S is not a function name or lambda-expression." form))))))


(%fhave 'eval #'cheap-eval)



  
(defun call-check-regs (fn &rest args)
  (declare (dynamic-extent args)
           (optimize (debug 3)))        ; don't use any saved registers
  (let ((old-regs (multiple-value-list (get-saved-register-values))))
    (declare (dynamic-extent old-regs))
    (multiple-value-prog1 (apply fn args)
      (let* ((new-regs (multiple-value-list (get-saved-register-values)))
             (new-regs-tail new-regs))
        (declare (dynamic-extent new-regs))
        (unless (dolist (old-reg old-regs t)
                  (unless (eq old-reg (car new-regs-tail))
                    (return nil))
                  (pop new-regs-tail))
          (apply 'error "Registers clobbered applying ~s to ~s~%~@{~a sb: ~s, Was: ~s~%~}"
                 fn args
                 (mapcan 'list
                         (let ((res nil))
                           (dotimes (i (length old-regs))
                             (push (format nil "save~d" i) res))
                           (nreverse res))
                         old-regs
                         new-regs)))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack frame accessors.

; Kinda scant, wouldn't you say ?


;end of L1-readloop.lisp

