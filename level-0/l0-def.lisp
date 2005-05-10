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



; primitives that manipulate function & variable definitions.





(defun functionp (arg)
  "Return true if OBJECT is a FUNCTION, and NIL otherwise."
  (functionp arg))

(defun lfunp (arg)
  (functionp arg))

(defun %proclaim-special (sym &optional initp)
  (let* ((oldbits (%symbol-bits sym)))
    (declare (fixnum oldbits))
    (%symbol-bits sym (bitset $sym_vbit_special oldbits))
    initp))

(setq *lfun-names* (make-hash-table :test 'eq :weak t))

(defun lookup-lfun-name (lfun) 
  (gethash lfun *lfun-names*))


(defun function-name (fun)
  (or (and (functionp fun) (lfun-name fun))
      (if (compiled-function-p (setq fun (closure-function fun)))
        (lfun-name fun)
        (if (and (consp fun) (eq (%car fun) 'lambda))
          (dolist (x (cddr fun))
            (when (and (consp x) (eq (%car x) 'block))
              (return (car (%cdr x)))))))))


(defun bootstrapping-fmakunbound (name)
  (when (consp name)
    (unless (eq (%car name) 'setf)
      (error "Function spec handler not loaded yet"))
    (setq name (setf-function-name (cadr name))))
  (%unfhave name)
  name)

; redefined in sysutils.
(%fhave 'fmakunbound #'bootstrapping-fmakunbound)

(defun bootstrapping-fset (name fn)
  (fmakunbound name)
  (%fhave name fn)
  fn)

;Redefined in sysutils.
(%fhave 'fset #'bootstrapping-fset)

(defun bootstrapping-record-source-file (fn &optional type)
  (declare (ignore fn type))
  nil)

;Redefined in l1-utils.
(%fhave 'record-source-file #'bootstrapping-record-source-file)


(setq *fasload-print* nil)
(setq *save-doc-strings* t)



(%fhave '%defun-encapsulated-maybe ;Redefined in encapsulate
        (qlfun bootstrapping-defun-encapsulated (name fn)
          (declare (ignore name fn))
          nil))

(%fhave 'encapsulated-function-name  ;Redefined in encapsulate - used in l1-io
        (qlfun bootstrapping-encapsulated-function-name (fn)
          (declare (ignore fn))
          nil))

(%fhave '%traced-p  ;Redefined in encapsulate - used in l1-io
        (qlfun bootstrapping-%traced-p (fn)
          (declare (ignore fn))
          nil))

(%fhave '%advised-p  ;Redefined in encapsulate used in l1-io
        (qlfun bootstrapping-%advised-p (fn)
          (declare (ignore fn))
          nil))

(%fhave 'set-function-info (qlfun set-function-info  (name info)
                                  (if (typep info 'string)
                                    (set-documentation name 'function info))
                                  name))

(defun %defun (named-fn &optional info &aux (name (function-name named-fn)))
   (record-source-file name 'function)
   (if (not (%defun-encapsulated-maybe name named-fn))
     (fset name named-fn))
   (set-function-info name info)
   (when *fasload-print* (format t "~&~S~%" name))
   name)

(defun validate-function-name (name)
  (if (symbolp name)
    name
    (if (setf-function-name-p name)
      (setf-function-name (cadr name))
      (report-bad-arg name 'function-name))))

;;;    There are three kinds of things which can go in the function
;;;    cell of a symbol: 1) A function.  2) The thing which is the
;;;    value of %unbound-function%: a 1-element vector whose 0th
;;;    element is a code vector which causes an "undefined function"
;;;    error to be signalled.  3) A macro or special-form definition,
;;;    which is a 2-element vector whose 0th element is a code vector
;;;    which signals a "can't apply macro or special form" error when
;;;    executed and whose 1st element is a macro or special-operator
;;;    name.  It doesn't what type of vector cases 2 and 3 are.  Once
;;;    that's decided, it wouldn't hurt if %FHAVE typechecked its
;;;    second arg.

(defun %fhave (name def)
  (let* ((fname (validate-function-name name)))
    (setf (%svref (%symbol->symptr fname) target::symbol.fcell-cell) def)))

;;; FBOUNDP is true of any symbol whose function-cell contains something other
;;; than %unbound-function%; we expect FBOUNDP to return that something.
(defun fboundp (name)
  "Return true if name has a global function definition."
  (let* ((fname (validate-function-name name))
         (def (%svref (%symbol->symptr fname) ppc32::symbol.fcell-cell)))
    (unless (eq def %unbound-function%)
      def)))

;;; %UNFHAVE doesn't seem to want to deal with SETF names or function specs.
;;; Who does ?

(defun %unfhave (sym)
  (let* ((symptr (%symbol->symptr sym))
         (old (%svref symptr ppc32::symbol.fcell-cell)))
    (setf (%svref symptr ppc32::symbol.fcell-cell) %unbound-function%)
    (not (eq old %unbound-function%))))

; It's guaranteed that lfun-bits is a fixnum.  Might be a 30-bit fixnum ...
(defun lfun-bits (function &optional new)
  (unless (functionp function)
    (setq function (require-type function 'function)))
  (let* ((idx (1- (the fixnum (uvsize function))))
         (old (%svref function idx)))
    (declare (fixnum idx))
    (if new
      (setf (%svref function idx) new))
    old))


; Remember that %DEFUN calls this and that it calls %nth-immediate (defined in
;  the compiler) in the case where its argument isn't compiled-function-p.

(defun closure-function (fun)
  (while (and (functionp fun)  (not (compiled-function-p fun)))
    (setq fun (%svref fun 1))           ; 0 is %closure-code% or something.
    (when (vectorp fun)
      (setq fun (svref fun 0))))
  fun)

(defun lfun-vector-name (fun &optional (new-name nil set-name-p))
  (let* ((bits (lfun-bits fun)))
    (declare (fixnum bits))
    (if (and (logbitp $lfbits-gfn-bit bits)
	     (not (logbitp $lfbits-method-bit bits)))
      (if set-name-p
	(%gf-name fun new-name)
	(%gf-name fun))
      (let* ((has-name-cell (not (logbitp $lfbits-noname-bit bits))))
	(if has-name-cell
	  (let* ((name-idx (- (the fixnum (uvsize fun)) 2))
		 (old-name (%svref fun name-idx)))
	    (declare (fixnum name-idx))
	    (if (and set-name-p (not (eq old-name new-name)))
	      (setf (%svref fun name-idx) new-name))
	    old-name))))))

(defun lfun-name (fun &optional (new-name nil set-name-p))
  (multiple-value-bind (stored-name stored?) (lookup-lfun-name fun)
    (unless stored?
      (setq stored-name (lfun-vector-name fun)))
    (when (and set-name-p (neq new-name stored-name))
      (if (and stored? (eq new-name (lfun-vector-name fun)))
        (remhash fun *lfun-names*)
        (if (logbitp 29 (the fixnum (lfun-bits fun)))   ; no name-cell in function vector.
          (puthash fun *lfun-names* new-name)
          (lfun-vector-name fun new-name))))
    stored-name))



    
(defun %macro-have (symbol macro-function)
  (declare (special %macro-code%))      ; magically set by xloader.
  (%fhave symbol (vector %macro-code% macro-function)))



(defun special-operator-p (symbol)
  "If the symbol globally names a special form, return T, otherwise NIL."
  (let ((def (fboundp symbol)))
    (and (typep def 'simple-vector)
         (not (lfunp (svref def 1))))))

(defun special-form-p (x) (special-operator-p x))

(defun setf-function-name-p (thing)
  (and (consp thing)
       (consp (%cdr thing))
       (null (%cddr thing))
       (eq (%car thing) 'setf)
       (symbolp (%cadr thing))))

(defun macro-function (form &optional env)
  "If SYMBOL names a macro in ENV, returns the expansion function,
   else returns NIL. If ENV is unspecified or NIL, use the global
   environment only."
  (setq form (require-type form 'symbol))
  (when env
    ; A definition-environment isn't a lexical environment, but it can
    ; be an ancestor of one.
    (unless (istruct-typep env 'lexical-environment)
        (report-bad-arg env 'lexical-environment))
      (let ((cell nil))
        (tagbody
          top
          (if (setq cell (%cdr (assq form (lexenv.functions env))))
            (return-from macro-function 
              (if (eq (car cell) 'macro) (%cdr cell))))
          (unless (listp (setq env (lexenv.parent-env env)))
            (go top)))))
      ; Not found in env, look in function cell.
  (%global-macro-function form))

; end of l0-def.lisp
