;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   Portions Copyright (C) 2001-2004, Clozure Associates
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
      (list *standard-help-argument* *standard-version-argument*))
     (ui-object :initform nil :initarg :ui-object :accessor application-ui-object)))
       
(defclass ui-object ()
    ())

;;; It's intended that this be specialized ...
(defmethod ui-object-do-operation ((u ui-object) operation &rest args)
  (declare (ignore operation args)))


(defun %usage-exit (banner exit-status other-args)
  (with-cstrs ((banner banner)
	       (other-args other-args))
    (ff-call (%kernel-import target::kernel-import-usage-exit)
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

  
;;; Process the "help" and "version" options, report parsing errors.
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

(defmethod application-ui-operation ((a application) operation &rest args)
  (let* ((ui-object (application-ui-object a)))
    (when ui-object
      (apply #'ui-object-do-operation ui-object operation args))))


;;;; specialize this for your application
(defmethod open-application ((self application) startup)
  (declare (ignore startup))
  nil)
  
;;; specialize this for your application
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
	   :allow-multiple nil)
          (make-command-line-argument
           :option-char #\Q
           :long-name "quiet"
           :help-string "if --batch, also suppress printing of heralds, prompts"
           :keyword :quiet
           :may-take-operand nil
           :allow-multiple nil)
          ))))

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
          *quiet-flag* (if *batch-flag*
                         (not (null (assoc :quiet options))))
	  *lisp-startup-parameters*
	  (mapcan #'(lambda (x)
		      (and (member (car x) '(:load :eval :gc-threshold)) (list x)))
		  options))))
	

(defmethod toplevel-function ((a lisp-development-system) init-file)
  (call-next-method)
  (let* ((sr (input-stream-shared-resource *terminal-input*)))
    (make-mcl-listener-process
     "listener"
     *terminal-input*
     *terminal-output*
     #'(lambda () (when sr (setf (shared-resource-primary-owner sr)
				 *initial-process*)))
     :initial-function
     #'(lambda ()
	 (startup-ccl (and *load-lisp-init-file* init-file))
	 (listener-function)
	 nil)
     :close-streams nil))
  (%set-toplevel #'(lambda ()
                     (with-standard-abort-handling nil 
                       (loop
			 (%nanosleep *periodic-task-seconds* *periodic-task-nanoseconds*)
			 (housekeeping)))))
  (toplevel))



(defmethod application-init-file ((app lisp-development-system))
  "home:openmcl-init")


; redefined by hide-listener-support
(defmethod application-error ((a application) condition error-pointer)
  (declare (ignore condition error-pointer))
  (quit))
