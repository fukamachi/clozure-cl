; -*- Mode:Lisp; Package:CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   Portions Copyright (C) 2001 Clozure Associates
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "EVAL"))

(defvar *step-print-level* 4
  "*Print-level* is bound to this when stepper prints forms.")

(defvar *step-print-length* 5
  "*Print-length* is bound to this when stepper prints forms.")

(declaim (special *step-level* ))


(defclass step-ui ()
    ())

(defclass step-tty-ui (step-ui)
    ())

(defmethod step-ui-uses-terminal ((ui step-ui))
  nil)

(defmethod step-ui-uses-terminal ((ui step-tty-ui))
  t					; well, Duh ...
  )

(defmethod step-output-stream ((ui step-tty-ui))
  *debug-io*)

(defmethod step-input-stream ((ui step-tty-ui))
  *debug-io*)

(defmethod step-show-error ((ui step-tty-ui) err)
  (let* ((stream (step-output-stream ui)))
    (step-tab ui)
    (princ "Error >> " stream)
    (format stream "~A" err)))

(defmethod step-prin1 ((ui step-tty-ui) form font &optional prefix)
  (declare (ignore font))
  (let ((*print-level* *step-print-level*)
        (*print-length* *step-print-length*)
        (*print-readably* nil)
        (*print-array* nil)
        (*print-case* :downcase)
	(stream (step-output-stream ui)))
    (when prefix (princ prefix stream))
    (prin1 form stream)
    (force-output stream)))


(defmethod step-show-form ((ui step-ui) form &optional env)
  (declare (ignore env))
  (step-tab ui)
  (step-prin1 ui form 2))

(defmethod step-ask ((ui step-tty-ui))
  (let* ((out (step-output-stream ui))
	 (in (step-input-stream ui)))
    (loop
	(step-tab ui)
	(princ "Step> " out)
      (let* ((form (read in  nil :quit)))
 (case form
	  ((:q :quit) (stream-clear-input in) (return :quit))
	  ((:s :step) (return :step))
	  ((:n :step-over) (return :step-over))
	  ((:g :go :pop) (return :go))
	  ((:e :eval) (return :eval))
	  ((:? :h :help) (return :help)))
	(step-tab ui)
	(princ "(:? for help)" out)))))

(defmethod step-show-help ((ui step-tty-ui))
  (let* ((out (step-output-stream ui)))
    (format out "~& :STEP, :S       - step through evaluation of form")
    (format out "~& :STEP-OVER, :N  - step over evaluation of form")
    (format out "~& :GO, :G         - continue evaluation without stepping")
    (format out "~& :EVAL, :E       - evaluate an expression in current env")
    (format out "~& :QUIT, :Q       - exit from the stepper (returning NIL)")))

(defmethod step-prompt-for-string ((ui step-ui) prompt)
  (get-string-from-user prompt))

(defmethod step-show-values ((ui step-ui) forms)
  (dolist (form forms)
    (step-tab ui)
    (step-prin1 ui form 1)))

;;; Tell the UI that we've finished stepping.
(defmethod step-ui-finish ((ui step-ui))
  )

(defmethod step-tab ((ui step-tty-ui) &aux (n (min *step-level* *trace-max-indent*)))
  (let* ((stream (step-output-stream ui)))
    (fresh-line stream)
    (dotimes (i n)
      (declare (fixnum i))
      (write-char #\Space stream))))

(defvar *step-ui* nil)

(defvar *default-step-ui-class-name* 'step-tty-ui)

; env is an execution environment - get the var alist for munching
; we are missing closed-used list (do we care? dont think so)
(defun evalenv-all-vars (env)
  (let ((vp (evalenv-vp env))
        ;(names (evalenv-names env))
        (values (evalenv-values env))
        (offset (+ 1 (evalenv-maxvp env)))
        (bindings ())
        (newnames ()))
    (while (>= vp 0)
      (let ((name (uvref values (+ offset vp)))
            (val (uvref values vp))
            reffer)
        (when name
          (cond ((eq val %special-marker%)
                 (setq reffer t))
                ((and (consp val)(eq (car val) %closed-marker%))
                 (setq reffer (list '%closure-ref name vp ))))           
          (push (make-symbol-info name vp nil reffer) bindings))
        (push name newnames)
        (setq vp (- vp 1))))
    (values (nreverse bindings)(nreverse newnames))))

; transform an execution environment into a pre-processing environment for eval while stepping
; (closed-used will be  wrong)
; if newmax is given we are gonna use it for running

(defun env-transform (env &optional (newenv (make-evalenv)) newmax)  
  (dotimes (i (1- (uvsize env)))
    (declare (fixnum i))
    (uvset newenv (+ 1 i) (uvref env (+ 1 i))))
  (if (null newmax)
    (multiple-value-bind (newvars newnames)(evalenv-all-vars env)    
      (setf (evalenv-variables newenv) newvars)
      (setf (evalenv-names newenv) newnames))
    (let ((an (make-array (1+ newmax)))
          (av (make-array (1+ newmax))))
      (dotimes (i (1+ (evalenv-vp env)))
        (declare (fixnum i))
        (uvset an i (uvref (evalenv-names env) i))
        (uvset av i (uvref (evalenv-values env) i)))
      (setf (evalenv-names newenv) an)
      (setf (evalenv-values newenv) av)))
  newenv)

;;;;;;;;;;;;;;;;;;;;;
;;; stepper
;;;


(defmacro step (form)
  (let ((*compile-definitions* nil)
        (fn (eval-prep form)))
    `(step-apply-simple ,fn nil)))


; def is a symbol - when called from thing produced by trace-global-def
(defun step-apply-simple (def args)
  ;(if (not (symbolp def))(error "Shouldnt 8"))
  ;(uncompile-for-step-apply def args) ; deal with redefinition (via compile) of stepped thing
  (let ((def (uncompile-for-step-apply def args)))
    (cond ((null *stepping*)
           (let* ((*step-ui* (make-instance *default-step-ui-class-name*))
		  (needs-tty (step-ui-uses-terminal *step-ui*)))
	     (if needs-tty
	       (with-terminal-input
		   (step-apply-simple-internal def args))
	       (step-apply-simple-internal def args))))
          (t (apply def args)))))

(defun step-apply-simple-internal (def args)
  (let ((*evalhook* #'step-evalhook)
	(*applyhook* #'step-applyhook)
	(*step-level* -2)
	(*stepping* t))
    (unwind-protect
	 (catch :ppstep-exit
	   (apply def args))
      (step-ui-finish *step-ui*))))

; we do install the interpreted def this week
(defun uncompile-for-step-apply (thing args)
  (if (or (and (symbolp thing)(fboundp thing))
	  (typep thing 'method)
	  ;;(typep thing 'compiled-lexical-closure)
	  (functionp thing))
    (uncompile-for-stepping thing args)
    thing))

(defun step-evalhook (form env)
  (when (not *stepping*)(return-from step-evalhook (evalhook form nil nil env)))
  (unwind-protect
    (prog ((*step-level* (+ *step-level* 2))
           (count -1)
           (show-form form)
           (last-command nil))
       (declare (ignorable last-command))
      START
      (setq show-form (or (gethash form *source-mapping-table*) form))
      (when (and (consp form) (consp (car form)))
        (case (caar form)
          ((%special-bind %special-declare)
           (return-from step-evalhook (evalhook form nil nil env)))
          (%with-specials
           (setq *step-level* (- *step-level* 2))
           (return-from step-evalhook (evalhook form #'step-evalhook #'step-applyhook env)))
          ;(setq show-form '(progn ***)))
          (%init&bind ; put this in the mapping table at munch time
           ; wont see these when inside with-specials
           (setq show-form `(%init&bind ,(var-name (cadr form))
                                        ,(caddr form)
                                        ,@(if (cadddr form)(var-name (cadddr form))))))
          ((%local-ref %closure-ref %special-ref)
           (setq show-form (cadr form)))))
      (if (not (constantp form))(progn (step-show-form *step-ui* show-form env)))
      EVAL
      (setq count (%i+ count 1))
      (multiple-value-bind 
        (val err)
        (ignore-errors ; wrong for symbol-macros                        
         (when (and (or (symbolp show-form) (constantp form)) (%izerop count))
           (let (info)
             (cond ((and (symbolp show-form)
                         (setq info (ev-symbol-info show-form env))) ; true iff symbol macro
                    (step-prin1 *step-ui* (var-ea info) 1 " = ")
                    ; will its a little funky but at least it doesnt loop
                    (go eval))
                   ((not (constantp form)) 
                    (setq form (evalhook form nil nil env))                   
                    (step-prin1 *step-ui* form 1 " = "))
                   ((consp form) ; quote
                    (setq form (evalhook form nil nil env))))
             (return-from step-evalhook form)))
	 #+later
         (unless (eq last-command :inspect)
           (update-step-inspector env))
         (case (setq last-command (step-ask *step-ui*))
           (:step
            (setq form (multiple-value-list (evalhook form #'step-evalhook #'step-applyhook env)))
            (when *stepping* (step-show-values *step-ui* form))
            (return (values-list form)))
           (:step-over
            (setq form (multiple-value-list (evalhook form nil nil env)))
            (step-show-values *step-ui* form)
            (return (values-list form)))
           (:go
            (setq *stepping* nil *evalhook* nil)
	    (step-ui-finish *step-ui*)
            (return (evalhook form nil nil env)))
           (:quit
            ; maybe this should close the window too
	    (step-ui-finish *step-ui*)
            (throw :step-exit (values)))
	   (:help
	    (step-show-help *step-ui*))
           (:eval
            (catch-cancel
              (let* (;(*compiled-for-evaluation* nil) ; maybe can be T
                     (*evalhook* nil)
                     (*applyhook* nil)
                     (str (step-prompt-for-string *step-ui* "Type a form to evaluate in the current lexical environment:")))
                (when str
                  (let ((form (read-from-string str))
                        (*step-level* (+ *step-level* 2)))
                    (step-tab *step-ui*)
                    (step-prin1 *step-ui* form 2 "Eval: ")
                    (setq form
                          (multiple-value-list
                           (cond ((symbolp form) ; symbol macro?
                                  (find-symbol-value-slow form env))
                                 ((not (consp form)) form)
                                 (t (step-eval-cheat form env)))))
                    (if (and form (null (cdr form)))
                      (step-prin1 *step-ui* (car form) 1 " = ")
                      (step-show-values *step-ui* form)))
                  (progn (step-show-form *step-ui* show-form))))))
	   #+later
           (:inspect (step-inspect env)))
         (go eval))
        (declare (ignore val))
        (when (typep err 'error)
	  (step-show-error *step-ui* err)
          (go start))))
    #+later
    (update-step-inspector nil)))

(defun step-eval-cheat (form env)
  ; try to avoid calling compiler or making a new env for little things like setq
  ; perhaps this is not worth the trouble
  (cond (env
         (let* (disgusting-accessors-constant
                (form (munch-form form (env-transform env %step-evalenv%))))
           (declare (special disgusting-accessors-constant))
           (if (> (evalenv-maxvp %step-evalenv%)(evalenv-maxvp env))
             ; oh foo, it needs more stack than we have - also fails if attempt
             ; to close&setq some new vars (actually fails in any case) - who should bitch?
             (let ((newenv (env-transform env (make-evalenv) (evalenv-maxvp %step-evalenv%))))
               (values (evalhook form nil nil newenv)))
             (values (evalhook form nil nil env)))))
        (t (values (applyhook (eval-prep form) nil nil nil)))))

#|
(defun step-eval-cheat (form env)
  (let ((newenv (env-transform env (make-evalenv))))
    (cheap-eval-in-environment form newenv)))
|#


(defun step-applyhook (fn args)
  (setq fn (uncompile-for-step-apply fn args))
  (applyhook fn args *evalhook* #'step-applyhook)
  )
  
(provide 'step)
