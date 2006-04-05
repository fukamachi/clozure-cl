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

; Returns two values: the arglist & it's functions binding.
; If the second arg is NIL, there was no function binding.
(defun arglist (sym &optional include-bindings)
  (%arglist sym include-bindings))

(defun arglist-string (sym &optional include-bindings)
  (multiple-value-bind (res type)
                       (%arglist-internal sym include-bindings)
    (values
     (if (stringp res)
       res
       (and res (prin1-to-string res)))
     type)))

(defun set-arglist (sym arglist)
  (let ((real-sym (arglist-sym-and-def sym)))
    (when (or real-sym (null sym))
      (if (eq arglist t)
        (remhash real-sym %lambda-lists%)
        (setf (gethash real-sym %lambda-lists%) arglist)))))

(defsetf arglist set-arglist)

; Same as ARGLIST, but has the option of using TEMP-CONS instead of CONS
; to cons up the list.
(defun %arglist (sym &optional include-bindings)
  (multiple-value-bind (res type)
                       (%arglist-internal
                        sym include-bindings)
    (when (stringp res)
      (with-input-from-string (stream res)
        (setq res nil)
        (let ((eof (list nil))
              val errorp)
          (declare (dynamic-extent eof))
          (loop
            (multiple-value-setq (val errorp)
              (ignore-errors (values (read stream nil eof))))
            (when errorp
	      #+help-file ; %HEL temporarily avoiding reference to help file
              (if use-help-file
                (return-from %arglist 
                  (%arglist sym include-bindings temp-cons-p nil)))
              (push '&rest res)
              (push ':unparseable res)
              (return))
            (when (eq val eof)
              (return))
            (push val res))
          (setq res
                (if (and (null (cdr res)) (listp (car res)))
                  (car res)
                  (nreverse res))))))
    (values res type)))

(defun %arglist-internal (sym include-bindings 
                              &aux def type)
  (multiple-value-setq (sym def) (arglist-sym-and-def sym))
  (when (standard-generic-function-p def)
    (let ((methods (%gf-methods def)))
      (if methods
        (setq def (closure-function
                   (find-unencapsulated-definition
                     (%method-function (car methods))))
              type :analysis))))
  (let ((ll (gethash sym %lambda-lists% *eof-value*))
        (conser #'cons)
        (macrop (and (symbolp sym) (eq (macro-function sym) def))))
    (flet ((strip (f) (if (stringp f) f (strip-bindings f include-bindings conser))))
      (declare (dynamic-extent #'strip))
      (cond ((neq ll *eof-value*) (values (strip ll) :declaration))
            ((consp def)
             ;; Presumably (lambda (... arglist) ...)
             (values (strip (cadr def)) :definition))
            ((neq (setq ll (getf (%lfun-info def) 'arglist *eof-value*)) *eof-value*)
             (values ll :definition))
            ((and (not macrop) (setq ll (uncompile-function def)))
             (values (strip (cadr ll)) (or type :definition)))
            ((lfunp def)
             (multiple-value-bind (arglist gotit) 
                                  (unless macrop (arglist-from-map def conser))
               (if gotit
                 (values arglist :analysis)
                 (cond  (macrop (values nil :unknown))
                       (t (values (arglist-from-compiled-def def conser) :analysis))))))
            (t (values nil nil))))))

            

(defun strip-bindings (arglist include-bindings conser)
  (if include-bindings
    arglist
    (macrolet ((push (elt list)
                 `(setq ,list (funcall conser ,elt ,list))))
      (let ((res nil))
        (do ((args arglist (%cdr args)))
            ((not (consp args)) (nreconc res args))
          (let ((arg (car args)))
            (cond ((atom arg)
                   (push arg res))
                  ((atom (car arg))
                   (push (car arg) res))
                  (t (push (caar arg) res)))))))))

(defun arglist-sym-and-def (sym &aux def)
  (cond ((functionp sym)
         (setq def sym
               sym (function-name def))
         (unless (and (symbolp sym) (eq def (fboundp sym)))
           (setq sym nil)))
        ((listp sym)
         (if (eq (car sym) 'setf)
           (setq sym (setf-function-name (cadr sym))
                 def (find-unencapsulated-definition (fboundp sym)))
           (setq sym nil def nil)))
        ((standard-method-p sym)
         (setq def (closure-function 
                    (find-unencapsulated-definition (%method-function sym)))))
        ((and (macro-function sym))
         (setq def (macro-function sym)))
        ((special-operator-p sym)
         nil)
        (t (setq def (find-unencapsulated-definition (fboundp sym)))))
  (values sym (if (standard-generic-function-p def) def (closure-function def))))

(defun arglist-from-map (lfun &optional (conser #'cons))
  (multiple-value-bind (nreq nopt restp nkeys allow-other-keys
                             optinit lexprp
                             ncells nclosed)
                       (function-args lfun)
    (declare (ignore optinit ncells))
    (macrolet ((push (elt list)
                     `(setf ,list (funcall conser ,elt ,list))))
      (when (not lexprp)
        (let ((map (car (function-symbol-map lfun))))
          (when map
            (let ((total (+ nreq nopt (if restp 1 0) (or nkeys 0)))
                  (idx (- (length map) nclosed))
                  (res nil))
              (if (%izerop total)
                (values nil t)
                (progn
                  (dotimes (x nreq)
                    (declare (fixnum x))
                    (push (if (> idx 0) (elt map (decf idx)) (make-arg "ARG" x)) res))
                  (when (neq nopt 0)
                    (push '&optional res)
                    (dotimes (x (the fixnum nopt))
                      (push (if (> idx 0) (elt map (decf idx)) (make-arg "OPT" x)) res)))

                  (when restp
                    (push '&rest res)
                    (when nkeys
                      (when (> idx nkeys) (decf idx nkeys)))
                    (push (if (> idx 0) (elt map (decf idx)) 'the-rest) res))                  (when nkeys
                    (push '&key res)
                    (let ((keyvect (lfun-keyvect lfun)))
                      (dotimes (i (length keyvect))
                        (push (elt keyvect i) res))))
                  (when allow-other-keys
                    (push '&allow-other-keys res))))
              (values (nreverse res) t))))))))

(defvar *req-arg-names*
  #(arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7 arg-8 arg-9))

(defvar *opt-arg-names*
  #(opt-0 opt-1 opt-2 opt-3 opt-4 opt-5 opt-6 opt-7 opt-8 opt-9))


(defun make-arg (prefix count)
  (cond ((and (string= prefix "ARG") (< count (length *req-arg-names*)))
         (svref *req-arg-names* count))
        ((and (string= prefix "OPT") (< count (length *opt-arg-names*)))
         (svref *opt-arg-names* count))
        (t (intern (format nil "~a-~d" prefix count) :CCL))))

(defun arglist-from-compiled-def (lfun &optional (conser #'cons) 
                                       &aux (res nil) argnames)
  (multiple-value-bind (nreq nopt restp nkeys allow-other-keys
                        optinit lexprp
                        ncells nclosed)
          (function-args lfun)
    (declare (ignore optinit ncells nclosed))
    (macrolet ((push (elt list)
                     `(setf ,list (funcall conser ,elt ,list))))
      (flet ((push-various-args (prefix count)
               (dotimes (i (the fixnum count))
                 (push (make-arg prefix i) res))))
        (declare (dynamic-extent #'push-various-args))
	;; Treat &LEXPR like &REST.
	(if lexprp (setq restp t lexprp nil))
        (cond ((and (eq 0 (+ nreq nopt (or nkeys 0))) (not restp))
               nil)
              (t 
               (if argnames
                 (setq res (reverse (butlast argnames (- (length argnames) nreq))))
                 (push-various-args "ARG" nreq))
               (when (> nopt 0)
                 (push '&optional res)
                 (if argnames
                   (setq res (append (reverse (subseq argnames nreq (+ nreq nopt))) res))
                   (push-various-args "OPT" nopt)))
               (when restp
                 (push '&rest res)
                 (if argnames
                   (push (nth (+ nreq nopt) argnames) res)
                   (push 'the-rest res)))
               (when nkeys
                 (push '&key res)
                 (let ((keyvect (lfun-keyvect lfun)))
                   (dotimes (i (length keyvect))
                     (push (elt keyvect i) res))))
               (when allow-other-keys
                 (push '&allow-other-keys res))
               (nreverse res)))))))

; End of arglist.lisp
