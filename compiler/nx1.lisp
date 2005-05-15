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

;;; Wimp out, but don't choke on (the (values ...) form)
(defnx1 nx1-the the (&whole call typespec form &environment env)
  (if (or (and (consp typespec) (eq (car typespec) 'values))
          (and (self-evaluating-p form)
               (not (typep form typespec))
               (progn (nx1-whine :type call) t)))
    (setq typespec t))
  (let* ((*nx-form-type* typespec)
         (transformed (nx-transform form env)))
    (if (and (consp transformed)
             (eq (car transformed) 'the))
        (setq transformed form))
    (make-acode
     (%nx1-operator typed-form)
     typespec
     (nx1-transformed-form transformed env))))

(defnx1 nx1-struct-ref struct-ref (&whole whole structure offset)
  (if (not (fixnump (setq offset (nx-get-fixnum offset))))
    (nx1-treat-as-call whole)
    (make-acode (%nx1-operator struct-ref)
                (nx1-form structure)
                (nx1-form offset))))

(defnx1 nx1-struct-set struct-set (&whole whole structure offset newval)
  (if (not (fixnump (setq offset (nx-get-fixnum offset))))
    (nx1-treat-as-call whole)
    (make-acode
     (%nx1-operator struct-set)
     (nx1-form structure)
     (nx1-form offset)
     (nx1-form newval))))

(defnx1 nx1-make-list make-list (&whole whole size &rest keys &environment env)
  (if (and keys 
             (or 
              (neq (list-length keys) 2)
              (neq (nx-transform (%car keys) env) :initial-element)))
    (nx1-treat-as-call whole)
    (make-acode
     (%nx1-operator make-list)
     (nx1-form size)
     (nx1-form (%cadr keys)))))

;;; New semantics: expansion functions are defined in current lexical environment
;;; vice null environment.  May be meaningless ...
(defnx1 nx1-macrolet macrolet (defs &body body)
  (let* ((old-env *nx-lexical-environment*)
         (new-env (new-lexical-environment old-env)))
    (dolist (def defs)
      (destructuring-bind (name arglist &body mbody) def
        (push 
         (cons 
          name
          (cons
           'macro
           (multiple-value-bind (function warnings) (compile-named-function (parse-macro name arglist mbody old-env) name  old-env)
             (setq *nx-warnings* (append *nx-warnings* warnings))
             function)))
         (lexenv.functions new-env))))
    (let* ((*nx-lexical-environment* new-env))
      (with-nx-declarations (pending)
        (multiple-value-bind (body decls) (parse-body body new-env)
          (nx-process-declarations pending decls)
          (nx1-progn-body body))))))

;;; Does SYMBOL-MACROLET allow declarations ?  Yes ...
(defnx1 nx1-symbol-macrolet symbol-macrolet (defs &body forms)
  (let* ((old-env *nx-lexical-environment*))
    (with-nx-declarations (pending)
      (multiple-value-bind (body decls)
                           (parse-body forms old-env nil)
        (nx-process-declarations pending decls)
        (let ((env *nx-lexical-environment*)
              (*nx-bound-vars* *nx-bound-vars*))
          (dolist (def defs)
            (destructuring-bind (sym expansion) def
              (let* ((var (nx-new-var pending sym))
                     (bits (nx-var-bits var)))
                (when (%ilogbitp $vbitspecial bits)
                  (nx-error "SPECIAL declaration applies to symbol macro ~s" sym))
                (nx-set-var-bits var (%ilogior (%ilsl $vbitignoreunused 1) bits))
                (setf (var-ea var) (cons :symbol-macro expansion)))))
          (nx-effect-other-decls pending env)
          (nx1-env-body body old-env))))))

(defnx1 nx1-progn progn (&body args)
  (nx1-progn-body args))

(defnx1 nx1-with-c-frame with-c-frame (var &body body)
  (make-acode (%nx1-operator with-c-frame)
              (nx1-form `(let* ((,var (%current-frame-ptr)))
                          ,@body))))


(defun nx1-progn-body (args)
  (if (null (cdr args))
    (nx1-form (%car args))
    (make-acode (%nx1-operator progn) (nx1-formlist args))))

(defnx1 nx1-unaryop ((%word-to-int) (uvsize)  (%reference-external-entry-point))
        (arg)
  (make-acode
   (%nx1-default-operator) (nx1-form arg)))

(defnx1 nx1-nullaryop ((%current-tcr) (%interrupt-poll) (%current-frame-ptr)) ()
  (make-acode (%nx1-default-operator)))

(defnx1 nx1-fixnum-ref ((%fixnum-ref) (%fixnum-ref-natural)) (base &optional (offset 0))
  (make-acode (%nx1-default-operator)
              (nx1-form base)
              (nx1-form offset)))

(defnx1 nx1-type-unaryop ((typecode) (lisptag) (fulltag))
  (arg)
  (let* ((operator
	  (case *nx-sfname*
	    ((typecode) (%nx1-operator ppc-typecode))
	    ((lisptag) (%nx1-operator ppc-lisptag))
	    (( fulltag) (%nx1-operator ppc-fulltag)))))
    (make-acode
     operator (nx1-form arg))))
        

(defnx1 nx1-code-char ((code-char)) (arg &environment env)
  (make-acode (if (nx-form-typep arg '(unsigned-byte 8) env)
                (%nx1-operator %code-char)
                (%nx1-operator code-char))
              (nx1-form arg)))

(defnx1 nx1-char-code ((char-code)) (arg &environment env)
  (make-acode (if (nx-form-typep arg 'character env)
                (%nx1-operator %char-code)
                (%nx1-operator char-code))
              (nx1-form arg)))

(defnx1 nx1-cXr ((car) (cdr)) (arg &environment env)
  (let* ((op (if (eq *nx-sfname* 'car) (%nx1-operator car) (%nx1-operator cdr)))
         (inline-op (if (eq op (%nx1-operator car)) (%nx1-operator %car) (%nx1-operator %cdr))))
    (make-acode (if (or (nx-inline-car-cdr env) (nx-form-typep arg 'list env))
                  inline-op
                  op)
                (nx1-prefer-areg arg env))))

(defnx1 nx1-rplacX ((rplaca) (rplacd)) (pairform valform &environment env)
  (let* ((op (if (eq *nx-sfname* 'rplaca) (%nx1-operator rplaca) (%nx1-operator rplacd)))
         (inline-op (if (eq op (%nx1-operator rplaca)) (%nx1-operator %rplaca) (%nx1-operator %rplacd))))
    (make-acode (if (or (nx-inline-car-cdr env)
                                 (and (nx-trust-declarations env)
                                      (or (subtypep *nx-form-type* 'cons)
                                          (nx-form-typep pairform 'cons env))))
                  inline-op
                  op)
                (nx1-prefer-areg pairform env)
                (nx1-form valform))))

(defnx1 nx1-set-cXr ((set-car) (set-cdr)) (pairform valform &environment env)
  (let* ((op (if (eq *nx-sfname* 'set-car) (%nx1-operator set-car) (%nx1-operator set-cdr)))
         (inline-op (if (eq op (%nx1-operator set-car)) (%nx1-operator %rplaca) (%nx1-operator %rplacd)))
         (inline-p (or (nx-inline-car-cdr env)
                            (and (nx-trust-declarations env)
                                 (or (subtypep *nx-form-type* 'cons)
                                     (nx-form-typep pairform 'cons env)))))
         (acode (make-acode (if inline-p inline-op op)
                            (nx1-prefer-areg pairform env)
                            (nx1-form valform))))
    (if inline-p
      (make-acode (if (eq op (%nx1-operator set-car)) (%nx1-operator %car) (%nx1-operator %cdr)) acode)
      acode)))

(defun nx1-cc-binaryop (op cc form1 form2)
  (make-acode op (nx1-immediate cc) (nx1-form form1) (nx1-form form2)))

(defnx1 nx1-ccEQ-unaryop ((characterp)  (endp) (consp) (base-char-p)) (arg)
  (make-acode (%nx1-default-operator) (nx1-immediate :EQ) (nx1-form arg)))






(defnx1 nx1-ccEQ-binaryop ( (%ptr-eql) (eq))
        (form1 form2)
  (nx1-cc-binaryop (%nx1-default-operator) :eq form1 form2))


(defnx1 nx1-ccNE-binaryop ((neq))
        (form1 form2)
  (nx1-cc-binaryop (%nx1-default-operator) :ne form1 form2))

(defnx1 nx1-logbitp ((logbitp)) (&whole w bitnum int &environment env)
  (if (and (nx-form-typep bitnum
                          (target-arch-case (:ppc32 '(integer 0 29))
                                            (:ppc64 '(integer 0 60))) env)
           (nx-form-typep int 'fixnum env))
    (nx1-cc-binaryop (%nx1-operator %ilogbitp) :ne bitnum int)
    (make-acode (%nx1-operator logbitp) (nx1-form bitnum) (nx1-form int))))


  
(defnx1 nx1-ccGT-unaryop ((int>0-p)) (arg)
  (make-acode (%nx1-default-operator) (nx1-immediate :gt) (nx1-form arg)))

(defnx1 nx1-macro-unaryop (multiple-value-list) (arg)
  (make-acode
   (%nx1-default-operator) (nx1-form arg)))

(defnx1 nx1-atom ((atom)) (arg)
  (nx1-form `(not (consp ,arg))))

(defnx1 nx1-locally locally (&body forms)
  (with-nx-declarations (pending)
    (let ((env *nx-lexical-environment*))
      (multiple-value-bind (body decls) (parse-body forms env  nil)
        (nx-process-declarations pending decls)
        (nx-effect-other-decls pending env)
         (setq body (nx1-progn-body body))
         (if decls
           (make-acode (%nx1-operator %decls-body) body *nx-new-p2decls*)
           body)))))

(defnx1 nx1-%new-ptr (%new-ptr) (&whole whole size clear-p)
  (make-acode (%nx1-operator %new-ptr) (nx1-form size) (nx1-form clear-p)))

;;; This might also want to look at, e.g., the last form in a progn:
;;;  (not (progn ... x)) => (progn ... (not x)), etc.
(defnx1 nx1-negation ((not) (null)) (arg)
  (if (nx1-negate-form (setq arg (nx1-form arg)))
    arg
    (make-acode (%nx1-operator not) (nx1-immediate :eq) arg)))

(defun nx1-negate-form (form)
  (let* ((subform (nx-untyped-form form)))
    (when (and (acode-p subform) (typep (acode-operator subform) 'fixnum))  
      (let* ((op (acode-operator subform)))
        (declare (fixnum op))
        (when (logbitp operator-cc-invertable-bit op)
          (%rplaca 
           (%cdr (%cadr subform))
           (acode-invert-condition-keyword (%cadr (%cadr subform))))
          t)))))

;;; This is called from pass 1, and therefore shouldn't mess with "puntable bindings"
;;; (assuming, of course, that anyone should ...)
(defun nx-untyped-form (form)
  (while (and (consp form)
              (eq (%car form) (%nx1-operator typed-form)))
    (setq form (%caddr form)))
  form)



(defnx1 nx1-cxxr ((caar) (cadr) (cdar) (cddr)) (form)
  (let* ((op *nx-sfname*))
    (let* ((inner (case op 
                       ((cdar caar) 'car)
                       (t 'cdr)))
              (outer (case op
                       ((cdar cddr) 'cdr)
                       (t 'car))))
         (nx1-form `(,outer (,inner ,form))))))      

(defnx1 nx1-%int-to-ptr ((%int-to-ptr)) (int)
  (make-acode 
   (%nx1-operator %consmacptr%)
   (make-acode (%nx1-operator %immediate-int-to-ptr) 
               (nx1-form int))))

(defnx1 nx1-%ptr-to-int ((%ptr-to-int)) (ptr)
  (make-acode 
   (%nx1-operator %immediate-ptr-to-int)
   (make-acode (%nx1-operator %macptrptr%) 
               (nx1-form ptr))))

(defnx1 nx1-%null-ptr-p ((%null-ptr-p)) (ptr)
  (nx1-form `(%ptr-eql ,ptr (%int-to-ptr 0))))

(defnx1 nx1-binop ( (%ilsl) (%ilsr) (%iasr)
                   (cons) (%temp-cons))
        (arg1 arg2)
  (make-acode (%nx1-default-operator) (nx1-form arg1) (nx1-form arg2)))



(defnx1 nx1-%misc-ref ((%misc-ref)) (v i)
  (make-acode (%nx1-operator uvref) (nx1-form v) (nx1-form i)))




(defnx1 nx1-schar ((schar)) (s i &environment env)
  (make-acode (%nx1-operator %sbchar) (nx1-form s env) (nx1-form i env)))


;;; This has to be ultra-bizarre because %schar is a macro.
;;; %schar shouldn't be a macro.
(defnx1 nx1-%schar ((%schar)) (&whole w arg idx &environment env)
  (let* ((arg (nx-transform arg env))
         (idx (nx-transform idx env))
         (argvar (make-symbol "STRING"))
         (idxvar (make-symbol "INDEX")))
    (nx1-form `(let* ((,argvar ,arg)
                      (,idxvar ,idx))
                 (declare (optimize (speed 3) (safety 0)))
                 (declare (simple-base-string ,argvar))
                 (schar ,argvar ,idxvar)) env)))
        
(defnx1 nx1-%scharcode ((%scharcode)) (arg idx &environment env)
  (make-acode (%nx1-operator %scharcode) (nx1-form arg)(nx1-form idx)))


(defnx1 nx1-svref ((svref) (%svref)) (&environment env v i)
  (make-acode (if (nx-inhibit-safety-checking env)
                (%nx1-operator %svref)
                (%nx1-default-operator))
              (nx1-prefer-areg v env)
              (nx1-form i)))

(defnx1 nx1-%slot-ref ((%slot-ref)) (instance idx)
  (make-acode (%nx1-default-operator)
              (nx1-form instance)
              (nx1-form idx)))


(defnx1 nx1-%err-disp ((%err-disp)) (&rest args)
  (make-acode (%nx1-operator %err-disp)
              (nx1-arglist args)))                       
              
(defnx1 nx1-macro-binop ((nth-value)) (arg1 arg2)
  (make-acode (%nx1-default-operator) (nx1-form arg1) (nx1-form arg2)))

(defnx1 nx1-logior-2 ((logior-2)) (&whole w &environment env arg-1 arg-2)
  (nx-binary-boole-op w 
                      env 
                      arg-1 
                      arg-2 
                      (%nx1-operator %ilogior2)
                      (%nx1-operator logior2)
		      (%nx1-operator %natural-logior)))

(defnx1 nx1-logxor-2 ((logxor-2)) (&whole w &environment env arg-1 arg-2)
  (nx-binary-boole-op w 
                      env 
                      arg-1 
                      arg-2 
                      (%nx1-operator %ilogxor2)
                      (%nx1-operator logxor2)
		      (%nx1-operator %natural-logxor)))

(defnx1 nx1-logand-2 ((logand-2)) (&whole w &environment env arg-1 arg-2)
  (nx-binary-boole-op w 
                      env 
                      arg-1 
                      arg-2 
                      (%nx1-operator %ilogand2)
                      (%nx1-operator logand2)
		      (%nx1-operator %natural-logand)))

(defnx1 nx1-require ((require-simple-vector) (require-simple-string) (require-integer) (require-list)
                     (require-fixnum) (require-real) (require-character) (require-number) (require-symbol))
        (arg)
  (make-acode (%nx1-default-operator) (nx1-form arg)))

(defnx1 nx1-%marker-marker ((%unbound-marker) (%slot-unbound-marker) (%illegal-marker)) ()
  (make-acode (%nx1-default-operator)))

(defnx1 nx1-throw (throw) (tag valuesform)
  (make-acode (%nx1-operator throw) (nx1-form tag) (nx1-form valuesform)))


;;; This is still used in inlining/lambda application.
;;; The tricky parts of handling inlining reasonably have to do with
;;; processing the body (including &optional/&key forms) in the environment
;;; in which the lambda was defined (e.g., macros and symbol-macros.)
;;; (I'm not sure that the traditional MCL/OpenMCL frontend handles
;;; these cases 100% correctly, but it seems difficult to do this
;;;  correctly without being able to jerk around with the environment,
;;; for a variety of reasons.)
;;; A lambda application - ((lambda ()) ...) is applied in the same
;;; environment it's defined in, so the hard case involves inlining
;;; functions whose environment may contain syntactic constructs
;;; not present in the current environment (and which does -not- generally
;;; contain whatever randomness is floating around at the point of
;;; application.)
(defun nx1-destructure (lambda-list bindform cdr-p &whole-allowed-p forms &optional (body-env *nx-lexical-environment*))
  (let* ((old-env body-env)
         (*nx-bound-vars* *nx-bound-vars*)
         (bindform (nx1-form bindform)))
    (if (not (verify-lambda-list lambda-list t &whole-allowed-p))
      (nx-error "Invalid lambda-list ~s" lambda-list)
      (let* ((*nx-lexical-environment* body-env))
        (with-nx-declarations (pending)
          (multiple-value-bind (body decls)
                               (parse-body forms *nx-lexical-environment*)
            (nx-process-declarations pending decls)
            (multiple-value-bind (req opt rest keys auxen whole)
                                 (nx-parse-structured-lambda-list pending lambda-list nil &whole-allowed-p)
              (nx-effect-other-decls pending *nx-lexical-environment*)
              (make-acode
               (%nx1-operator debind)
               nil
               bindform
               req
               opt
               rest
               keys
               auxen
               whole
               (nx1-env-body body old-env)
               *nx-new-p2decls*
               cdr-p))))))))



(defnx1 nx1-%setf-macptr ((%setf-macptr)) (ptr newval)
  (let* ((arg1 (nx1-form ptr))
         (arg2 (nx1-form newval)))
    (if (and (consp arg1) (eq (%car arg1) (%nx1-operator %consmacptr%)))
      ;e.g. (%setf-macptr (%null-ptr) <foo>)
      (make-acode (%nx1-operator %consmacptr%)
                  (make-acode (%nx1-operator progn)
                              (list arg1 (make-acode (%nx1-operator %macptrptr%) arg2))))
      (make-acode (%nx1-operator %setf-macptr) arg1 arg2))))

(defnx1 nx1-%setf-double-float ((%setf-double-float)) (double-node double-val)
  (make-acode (%nx1-operator %setf-double-float) (nx1-form double-node) (nx1-form double-val)))

(defnx1 nx1-%setf-short-float ((%setf-short-float) (%setf-single-float)) (short-node short-val)
  (make-acode (%nx1-operator %setf-short-float) (nx1-form short-node) (nx1-form short-val)))

(defnx1 nx1-%inc-ptr ((%inc-ptr)) (ptr &optional (increment 1))
  (make-acode (%nx1-operator %consmacptr%)
              (make-acode (%nx1-operator %immediate-inc-ptr)
                          (make-acode (%nx1-operator %macptrptr%) (nx1-form ptr))
                          (nx1-form increment))))

(defnx1 nx1-svset ((svset) (%svset)) (&environment env vector index value)
  (make-acode (if (nx-inhibit-safety-checking env)
                (%nx1-operator %svset)
                (%nx1-default-operator))
              (nx1-prefer-areg vector env) (nx1-form index) (nx1-form value)))

(defnx1 nx1-+ ((+-2)) (&whole whole &environment env num1 num2)
  (let* ((f1 (nx1-form num1))
         (f2 (nx1-form num2)))
    (if (and (nx-form-typep num1 'fixnum env) ; (nx-acode-fixnum-type-p f1 env)
             (nx-form-typep num2 'fixnum env)) ;(nx-acode-fixnum-type-p f2 env))
      (let* ((fixadd (make-acode (%nx1-operator %i+) f1 f2))
             (small-enough (target-arch-case
                            (:ppc32 '(signed-byte 28))
                            (:ppc64 '(signed-byte 59)))))
        (if (or (and (nx-acode-form-typep f1 small-enough env)
                     (nx-acode-form-typep f2 small-enough env))
                (and (nx-trust-declarations env)
                     (subtypep *nx-form-type* 'fixnum)))
          fixadd
          (make-acode (%nx1-operator typed-form) 'integer (make-acode (%nx1-operator fixnum-overflow) fixadd))))
      (if (and (nx-form-typep num1 'double-float env)
               (nx-form-typep num2 'double-float env))
        (nx1-form `(%double-float+-2 ,num1 ,num2))
        (if (and (nx-form-typep num1 'short-float env)
                 (nx-form-typep num2 'short-float env))
          (nx1-form `(%short-float+-2 ,num1 ,num2))
	  (if (and (nx-form-typep num1 '(unsigned-byte 32) env)
		   (nx-form-typep num2 '(unsigned-byte 32) env)
		   (subtypep *nx-form-type* '(unsigned-byte 32)))
	    (make-acode (%nx1-operator typed-form)
			'(unsigned-byte 32)
			(make-acode (%nx1-operator %natural+) f1 f2))
	    (make-acode (%nx1-operator typed-form) 'number 
			(make-acode (%nx1-operator add2) f1 f2))))))))
  
(defnx1 nx1-%double-float-x-2 ((%double-float+-2) (%double-float--2) (%double-float*-2) (%double-float/-2 ))
        (f0 f1)
  (make-acode (%nx1-operator typed-form) 'double-float
              (make-acode (%nx1-default-operator) (nx1-form f0) (nx1-form f1))))


(defnx1 nx1-%short-float-x-2 ((%short-float+-2) (%short-float--2) (%short-float*-2) (%short-float/-2 ))
        (f0 f1)
  (make-acode (%nx1-operator typed-form) 'short-float
              (make-acode (%nx1-default-operator) (nx1-form f0) (nx1-form f1))))


(defnx1 nx1-*-2 ((*-2)) (&whole whole &environment env num1 num2)
  (if (nx-binary-fixnum-op-p num1 num2 env)
    (make-acode (%nx1-operator %i*) (nx1-form num1 env) (nx1-form num2 env))
    (if (and (nx-form-typep num1 'double-float env)
             (nx-form-typep num2 'double-float env))
      (nx1-form `(%double-float*-2 ,num1 ,num2))
      (if (and (nx-form-typep num1 'short-float env)
               (nx-form-typep num2 'short-float env))
        (nx1-form `(%short-float*-2 ,num1 ,num2))
        (nx1-treat-as-call whole)))))

(defnx1 nx1-%negate ((%negate)) (&whole whole num &environment env)
  (if (nx-form-typep num 'fixnum env)
    (if (subtypep *nx-form-type* 'fixnum)
      (make-acode (%nx1-operator %%ineg)(nx1-form num))
      (make-acode (%nx1-operator %ineg) (nx1-form num)))
    (make-acode (%nx1-operator minus1) (nx1-form num))))

        
(defnx1 nx1--2 ((--2)) (&whole whole &environment env num0 num1)
  (if (and (nx-form-typep num0 'fixnum env) (nx-form-typep num1 'fixnum env))
    (let* ((f0 (nx1-form num0))
	   (f1 (nx1-form num1))
	   (fixsub (make-acode (%nx1-operator %i-) f0 f1))
	   (small-enough (target-arch-case
                          (:ppc32 '(signed-byte 28))
                          (:ppc64 '(signed-byte 59)))))
      (if (or (and (nx-acode-form-typep f0 small-enough env)
		   (nx-acode-form-typep f1 small-enough env))
	      (and (nx-trust-declarations env)
		   (subtypep *nx-form-type* 'fixnum)))
	fixsub
	(make-acode (%nx1-operator fixnum-overflow) fixsub)))
    (if (and (nx-form-typep num0 'double-float env)
	     (nx-form-typep num1 'double-float env))
      (nx1-form `(%double-float--2 ,num0 ,num1))
      (if (and (nx-form-typep num0 'short-float env)
	       (nx-form-typep num1 'short-float env))
	(nx1-form `(%short-float--2 ,num0 ,num1))
	(if (and (nx-form-typep num0 '(unsigned-byte 32) env)
		 (nx-form-typep num1 '(unsigned-byte 32) env)
		 (nx-trust-declarations env)
		 (subtypep *nx-form-type* '(unsigned-byte 32)))
	  (make-acode (%nx1-operator %natural-)
		      (nx1-form num0)
		      (nx1-form num1))		 
	  (nx1-treat-as-call whole))))))
      
(defnx1 nx1-/-2 ((/-2)) (num0 num1 &environment env)
  (if (and (nx-form-typep num0 'double-float env)
           (nx-form-typep num1 'double-float env))
    (nx1-form `(%double-float/-2 ,num0 ,num1))
    (if (and (nx-form-typep num0 'short-float env)
             (nx-form-typep num1 'short-float env))
      (nx1-form `(%short-float/-2 ,num0 ,num1))
      (make-acode (%nx1-operator %quo2) (nx1-form num0) (nx1-form num1)))))



(defnx1 nx1-numcmp ((<-2) (>-2) (<=-2) (>=-2)) (&whole whole &environment env num1 num2)
  (let* ((op *nx-sfname*)
         (both-fixnums (and (nx-form-typep num1 'fixnum env)
                            (nx-form-typep num2 'fixnum env)))
	 (both-natural (and (nx-form-typep num1 '(unsigned-byte 32) env)
			(nx-form-typep num2 '(unsigned-byte 32) env)))
         (both-double-floats
          (let* ((dfloat-1 (nx-form-typep num1 'double-float env))
                 (dfloat-2 (nx-form-typep num2 'double-float env)))
            (if dfloat-1 
              (or dfloat-2 (if (typep num2 'fixnum) (setq num2 (coerce num2 'double-float))))
              (if dfloat-2 (if (typep num1 'fixnum) (setq num1 (coerce num1 'double-float)))))))
         (both-short-floats
          (let* ((sfloat-1 (nx-form-typep num1 'short-float env))
                 (sfloat-2 (nx-form-typep num2 'short-float env)))
            (if sfloat-1 
              (or sfloat-2 (if (typep num2 'fixnum) (setq num2 (coerce num2 'short-float))))
              (if sfloat-2 (if (typep num1 'fixnum) (setq num1 (coerce num1 'short-float))))))))

    (if (or both-fixnums both-double-floats both-short-floats both-natural)
      (make-acode
       (if both-fixnums
         (%nx1-operator %i<>)
	 (if both-natural
	   (%nx1-operator %natural<>)
	   (if both-double-floats
	     (%nx1-operator double-float-compare)
	     (%nx1-operator short-float-compare))))
       (make-acode
        (%nx1-operator immediate)
        (if (eq op '<-2)
          :LT
          (if (eq op '>=-2)
            :GE
            (if (eq op '<=-2)
              :LE
              :GT))))
       (nx1-form num1)
       (nx1-form num2))
      (nx1-treat-as-call whole))))

(defnx1 nx1-num= ((=-2) (/=-2)) (&whole whole &environment env num1 num2 )
  (let* ((op *nx-sfname*)
	 (2-fixnums (and (nx-form-typep num1 'fixnum env)
			 (nx-form-typep num2 'fixnum env)))
	 (2-naturals (and (nx-form-typep num1 '(unsigned-byte 32) env)
		      (nx-form-typep num2 '(unsigned-byte 32) env)))
         (2-rats (and (nx-form-typep num1 'rational env)
                      (nx-form-typep num2 'rational env)))
         (2-dfloats (let* ((dfloat-1 (nx-form-typep num1 'double-float env))
                           (dfloat-2 (nx-form-typep num2 'double-float env)))
                      (if dfloat-1 
                        (or dfloat-2 (if (typep num2 'fixnum) (setq num2 (coerce num2 'double-float))))
                        (if dfloat-2 (if (typep num1 'fixnum) (setq num1 (coerce num1 'double-float)))))))
         (2-sfloats (let* ((sfloat-1 (nx-form-typep num1 'short-float env))
                           (sfloat-2 (nx-form-typep num2 'short-float env)))
                      (if sfloat-1 
                        (or sfloat-2 (if (typep num2 'fixnum) (setq num2 (coerce num2 'short-float))))
                        (if sfloat-2 (if (typep num1 'fixnum) (setq num1 (coerce num1 'short-float)))))))
         )
    (if (and 2-naturals (not 2-fixnums))
      (make-acode
       (%nx1-operator %natural<>)
       (make-acode
	(%nx1-operator immediate)
	(if (eq op '=-2)
	  :EQ
	  :NE))
       (nx1-form num1)
       (nx1-form num2))
      (if 2-rats
	(let* ((form `(eql ,num1 ,num2))) 
	  (nx1-form (if (eq op '=-2) form `(not ,form))))
	(if (or  2-dfloats 2-sfloats)
	  (make-acode 
	   (if 2-dfloats
             (%nx1-operator double-float-compare)
             (%nx1-operator short-float-compare))
	   (make-acode
	    (%nx1-operator immediate)     
	    (if (eq op '=-2)
	      :EQ
	      :NE))
	   (nx1-form num1)
	   (nx1-form num2))
	  (nx1-treat-as-call whole))))))
             

(defnx1 nx1-uvset ((uvset) (%misc-set)) (vector index value &environment env)
  (make-acode (%nx1-operator uvset)
              (nx1-form vector)
              (nx1-form index)
              (nx1-form value)))

(defnx1 nx1-set-schar ((set-schar)) (&whole w s i v &environment env)
  (make-acode (%nx1-operator %set-sbchar) (nx1-form s) (nx1-form i) (nx1-form v)))



(defnx1 nx1-%set-schar ((%set-schar)) (arg idx char &environment env)
  (let* ((arg (nx-transform arg env))
         (idx (nx-transform idx env))
         (char (nx-transform char env))
         (argvar (make-symbol "ARG"))
         (idxvar (make-symbol "IDX"))
         (charvar (make-symbol "CHAR")))
    (nx1-form `(let* ((,argvar ,arg)
                      (,idxvar ,idx)
                      (,charvar ,char))
                 (declare (optimize (speed 3) (safety 0)))
                 (declare (simple-base-string ,argvar))
                 (setf (schar ,argvar ,idxvar) ,charvar))
              env)))

(defnx1 nx1-%set-scharcode ((%set-scharcode)) (&whole w s i v)
    (make-acode (%nx1-operator %set-scharcode)
                (nx1-form s)
                (nx1-form i)
                (nx1-form v)))
              

(defnx1 nx1-list-vector-values ((list) (vector) (values) (%temp-list)) (&rest args)
  (make-acode (%nx1-default-operator) (nx1-formlist args)))



(defnx1 nx1-%gvector ( (%gvector)) (&rest args)
  (make-acode (%nx1-operator %ppc-gvector) (nx1-arglist args)))

(defnx1 nx1-quote quote (form)
  (nx1-immediate form))

(defnx1 nx1-list* ((list*)) (first &rest rest)
  (make-acode (%nx1-operator list*) (nx1-arglist (cons first rest) 1)))


#|
(defnx1 nx1-append ((append)) (&rest args)
  (make-acode (%nx1-operator append) (nx1-arglist args 2)))


|#

(defnx1 nx1-or or (&whole whole &optional (firstform nil firstform-p) &rest moreforms)
  (if (not firstform-p)
    (nx1-form nil)
    (if (null moreforms)
      (nx1-form firstform)
      (progn
        (make-acode (%nx1-operator or) (nx1-formlist (%cdr whole)))))))

(defun nx1-1d-vref (env arr dim0 &optional uvref-p)
  (let* ((simple-vector-p (nx-form-typep arr 'simple-vector env))
         (string-p (unless simple-vector-p 
                     (if (nx-form-typep arr 'string env)
                       (or (nx-form-typep arr 'simple-string env)
                           (return-from nx1-1d-vref (nx1-form `(char ,arr ,dim0)))))))
         (simple-1d-array-p (unless (or simple-vector-p string-p) 
                              (nx-form-typep arr '(simple-array * (*)) env)))
         
         (array-type (specifier-type  (nx-form-type arr env)))
         (type-keyword (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        array-type)))
    (if type-keyword
      (make-acode (%nx1-operator %typed-uvref) 
                  (nx1-immediate type-keyword)
                  (nx1-form arr)
                  (nx1-form dim0))
      (let* ((op (cond (simple-1d-array-p (%nx1-operator uvref))
                       (string-p (%nx1-operator %sbchar))
                       (simple-vector-p 
                        (if (nx-inhibit-safety-checking env) (%nx1-operator %svref) (%nx1-operator svref)))
                       (uvref-p (%nx1-operator uvref))
                       (t (%nx1-operator %aref1)))))
        (make-acode op (nx1-form arr) (nx1-form dim0))))))
  
(defnx1 nx1-aref ((aref)) (&whole whole &environment env arr &optional (dim0 nil dim0-p)
                                  &rest other-dims)
   (if (and dim0-p (null other-dims))
     (nx1-1d-vref env arr dim0)
     (nx1-treat-as-call whole)))

(defnx1 nx1-uvref ((uvref)) (&environment env arr dim0)
  (nx1-1d-vref env arr dim0 t))

(defnx1 nx1-%aref2 ((%aref2)) (&whole whole &environment env arr i j)
  ;; For now, we only care about the (simple-array double-float (* *)) case.
  (let* ((subtype
	  (cond ((nx-form-typep arr '(simple-array double-float (* *)) env)
		 :double-float-vector)
                ((nx-form-typep arr '(simple-array single-float (* *)) env)
		 :single-float-vector))))
    (if subtype
      (let* ((ctype (specifier-type (nx-form-type arr env)))
	     (dims (array-ctype-dimensions ctype))
	     (dim0 (car dims))
	     (dim1 (cadr dims)))
	(make-acode (%nx1-operator aref2)
		    (nx1-form subtype)
		    (nx1-form arr)
		    (nx1-form i)
		    (nx1-form j)
		    (nx1-form (if (typep dim0 'fixnum) dim0))
		    (nx1-form (if (typep dim1 'fixnum) dim1))))
	(nx1-treat-as-call whole))))

(defun nx1-1d-vset (arr newval dim0 env &optional uvset-p)
  (let* ((simple-vector-p (nx-form-typep arr 'simple-vector env))
         (string-p (unless simple-vector-p 
                     (if (nx-form-typep arr 'string env)
                       (or (nx-form-typep arr 'simple-string env)
                           (return-from nx1-1d-vset (nx1-form `(set-char ,arr ,newval ,dim0)))))))
         (simple-1d-array-p (unless (or simple-vector-p string-p) 
                              (nx-form-typep arr '(simple-array * (*)) env)))
         (array-type (specifier-type  (nx-form-type arr env)))
         (type-keyword (funcall
                        (arch::target-array-type-name-from-ctype-function
                         (backend-target-arch *target-backend*))
                        array-type)))
         (if type-keyword
             (make-acode (%nx1-operator %typed-uvset) 
                         (nx1-immediate type-keyword)
                         (nx1-form arr)
                         (nx1-form newval)
                         (nx1-form dim0))
             (let* ((op (cond (simple-1d-array-p (%nx1-operator uvset))
                              (string-p (%nx1-operator %set-sbchar))
                              (simple-vector-p (if (nx-inhibit-safety-checking env) (%nx1-operator %svset) (%nx1-operator svset)))
			      (uvset-p (%nx1-operator uvset))
                              (t (%nx1-operator aset1)))))
               (if op
                   (make-acode
                    op
                    (nx1-form arr)
                    (nx1-form newval)
                    (nx1-form dim0))
                   (nx1-form `(,(if string-p 'set-schar '%aset1) ,arr ,newval ,dim0)))))))

(defnx1 nx1-aset ((aset)) (&whole whole 
                                  arr newval 
                                  &optional (dim0 nil dim0-p)
                                  &environment env
                                  &rest other-dims)
   (if (and dim0-p (null other-dims))
       (nx1-1d-vset arr newval dim0 env)
       (nx1-treat-as-call whole)))
            
(defnx1 nx1-%aset2 ((%aset2)) (&whole whole &environment env arr i j new)
  (let* ((subtype
	  (cond ((nx-form-typep arr '(simple-array double-float (* *)) env)
		 :double-float-vector)
                ((nx-form-typep arr '(simple-array single-float (* *)) env)
		 :single-float-vector))))
    (if subtype
       (let* ((ctype (specifier-type (nx-form-type arr env)))
	      (dims (array-ctype-dimensions ctype))
	      (dim0 (car dims))
	      (dim1 (cadr dims)))
	(make-acode (%nx1-operator aset2)
		    (nx1-form subtype)
		    (nx1-form arr)
		    (nx1-form i)
		    (nx1-form j)
		    (nx1-form new)
		    (nx1-form dim0)
		    (nx1-form dim1)))
	(nx1-treat-as-call whole))))

(defnx1 nx1-prog1 (prog1 multiple-value-prog1) (save &body args 
                                                     &aux (l (list (nx1-form save))))
  (make-acode 
   (%nx1-default-operator) 
   (dolist (arg args (nreverse l))
     (push (nx1-form arg) l))))

(defnx1 nx1-if if (test true &optional false)
  (if (null true)
    (if (null false)
      (return-from nx1-if (nx1-form `(progn ,test nil)))
      (psetq test `(not ,test) true false false true)))
  (make-acode (%nx1-operator if)  (nx1-form test) (nx1-form true) (nx1-form false)))

(defnx1 nx1-%debug-trap dbg (&optional arg)
  (make-acode (%nx1-operator %debug-trap) (nx1-form arg)))
        
(defnx1 nx1-setq setq (&whole whole &rest args &environment env &aux res)
  (when (%ilogbitp 0 (length args))
    (nx-error "Odd number of forms in ~s ." whole))
  (while args
    (let ((sym (nx-need-var (%car args) nil))
	  (val (%cadr args)))
      (multiple-value-bind (expansion win) (macroexpand-1 sym env)
	(if win
	    (push (nx1-form `(setf ,expansion ,val)) res)
	    (multiple-value-bind (info inherited catchp)
		(nx-lex-info sym)
	      (push
	       (if (eq info :symbol-macro)
		   (progn
		     (nx-set-var-bits catchp
				      (%ilogior
				       (%ilsl $vbitsetq 1)
				       (%ilsl $vbitreffed 1)
				       (nx-var-bits catchp)))
		     (nx1-form `(setf ,inherited ,val)))
		   (let* ((valtype (nx-form-type val env))
			  (declared-type (nx-declared-type sym)))
		     (let ((*nx-form-type* declared-type))
		       (setq val (nx1-typed-form val env)))
		     (if (and info (neq info :special))
			 (progn
			   (nx1-check-assignment sym env)
			   (let ((inittype (var-inittype info)))
			     (if (and inittype (not (subtypep valtype inittype)))
				 (setf (var-inittype info) nil)))
			   (if inherited
			       (nx-set-var-bits info (%ilogior (%ilsl $vbitsetq 1)
							       (%ilsl $vbitnoreg 1) ; I know, I know ... Someday ...
							       (nx-var-bits info)))
			       (nx-set-var-bits info (%ilogior2 (%ilsl $vbitsetq 1) (nx-var-bits info))))
			   (nx-adjust-setq-count info 1 catchp) ; In the hope that that day will come ...
			   (make-acode (%nx1-operator setq-lexical) info val))
			 (make-acode
			  (if (nx1-check-special-ref sym info)
			      (progn
				(nx-record-xref-info :references sym)
				(nx-record-xref-info :sets sym)
			        (if (nx-global-p sym env)
			          (%nx1-operator global-setq)
			          (%nx1-operator setq-special)))
			    (%nx1-operator setq-free)) ; Screw: no object lisp.  Still need setq-free ? For constants ?
			  (nx1-note-vcell-ref sym)
			  val))))
	       res)))
	(setq args (%cddr args)))))
  (make-acode (%nx1-operator progn) (nreverse res)))

;;; See if we're trying to setq something that's currently declared "UNSETTABLE"; whine if so.
;;; If we find a contour in which a "SETTABLE NIL" vdecl for the variable exists, whine.
;;; If we find a contour in which a "SETTABLE T" vdecl for the variable exists. or
;;;    the contour in which the variable's bound, return nil.
;;; Should find something ...
(defun nx1-check-assignment (sym env)
  (loop
    (unless (and env (istruct-typep env 'lexical-environment))
      (return))
    (dolist (decl (lexenv.vdecls env))
      (when (and (eq (car decl) sym)
               (eq (cadr decl) 'settable))
        (unless (cddr decl)
          (nx1-whine :unsettable sym))
        (return-from nx1-check-assignment nil)))
    (let ((vars (lexenv.variables env)))
      (unless (atom vars)
        (dolist (var vars)
          (when (eq (var-name var) sym) (return-from nx1-check-assignment nil)))))
    (setq env (lexenv.parent-env env))))

;;; The cleanup issue is a little vague (ok, it's a -lot- vague) about the environment in
;;; which the load-time form is defined, although it apparently gets "executed in a null
;;; lexical environment".  Ignoring the fact that it's meaningless to talk of executing
;;; something in a lexical environment, we can sort of infer that it must also be defined
;;; in a null lexical environment.

(defnx1 nx1-load-time-value (load-time-value) (&environment env form &optional read-only-p)
  ; Validate the "read-only-p" argument
  (if (and read-only-p (neq read-only-p t)) (require-type read-only-p '(member t nil)))
  ; Then ignore it.
  (if *nx-load-time-eval-token*
    (multiple-value-bind (function warnings)
                         (compile-named-function 
                          `(lambda () ,form) nil nil nil nil nil *nx-load-time-eval-token* (backend-name *target-backend*))
      (setq *nx-warnings* (append *nx-warnings* warnings))
      (nx1-immediate (list *nx-load-time-eval-token* `(funcall ,function))))
    (nx1-immediate (eval form))))

(defnx1 nx1-catch (catch) (operation &body body)
  (make-acode (%nx1-operator catch) (nx1-form operation) (nx1-catch-body body)))

(defnx1 nx1-%badarg ((%badarg)) (badthing right-type)
  (make-acode (%nx1-operator %badarg2) 
              (nx1-form badthing) 
              (nx1-form (or (if (quoted-form-p right-type) (%typespec-id (cadr right-type))) right-type))))

(defnx1 nx1-unwind-protect (unwind-protect) (protected-form &body cleanup-form)
  (if cleanup-form
    (make-acode (%nx1-operator unwind-protect) 
                (nx1-catch-body (list protected-form))
                (nx1-progn-body cleanup-form))
    (nx1-form protected-form)))

(defnx1 nx1-progv progv (symbols values &body body)
  (make-acode (%nx1-operator progv) 
              (nx1-form `(svar-check-symbol-list ,symbols))
              (nx1-form values) 
              (nx1-catch-body body)))

(defun nx1-catch-body (body)
  (let* ((temp (new-lexical-environment *nx-lexical-environment*)))
    (setf (lexenv.variables temp) 'catch)
    (let* ((*nx-lexical-environment* (new-lexical-environment temp)))
      (nx1-progn-body body))))

(defnx1 nx1-disable-interrupts (disable-lisp-interrupts) ()
  (make-acode (%nx1-operator disable-interrupts)))

(defnx1 nx1-restoring-interrupt-level restoring-interrupt-level (var &body body)
  (make-acode (%nx1-operator without-interrupts)
	      (nx1-form var)
	      (nx1-catch-body body)))
			  

(defnx1 nx1-apply ((apply)) (&whole call fn arg &rest args &aux (orig args) (spread-p t))
  (if (null (%car (last (push arg args))))
    (setq spread-p nil args (butlast args)))
  (let ((name (nx1-func-name fn))
        (global nil))
    (if name
      (if (eq (%car fn) 'quote)
        (setq global t name (nx1-form fn))
        (let*  ((afunc (nth-value 1 (nx-lexical-finfo name))))
          (when (and afunc (eq afunc *nx-call-next-method-function*))
            (setq name (if (or arg orig) 
                         '%call-next-method-with-args
                         '%call-next-method)
                         global t
                         args (cons (var-name *nx-next-method-var*) args)))))
      (setq name (nx1-form fn)))
    (nx1-call name args spread-p global)))

(defnx1 nx1-%apply-lexpr ((%apply-lexpr)) (&whole call fn arg &rest args &aux (orig args))
  (push arg args)
  (let ((name (nx1-func-name fn))
        (global nil))
    (if name
      (if (eq (%car fn) 'quote)
        (setq global t name (nx1-form fn))
        (let*  ((afunc (nth-value 1 (nx-lexical-finfo name))))
          (when (and afunc (eq afunc *nx-call-next-method-function*))
            (setq name (if (or arg orig) 
                         '%call-next-method-with-args
                         '%call-next-method)
                  global t
                  args (cons (var-name *nx-next-method-var*) args)))))
      (setq name (nx1-form fn)))
    (nx1-call name args 0 global)))



(defnx1 nx1-%defun %defun (&whole w def &optional (doc nil doc-p) &environment env)
  (declare (ignorable doc doc-p))
  ; Pretty bogus.
  (if (and (consp def)
           (eq (%car def) 'nfunction)
           (consp (%cdr def))
           (symbolp (%cadr def)))
    (note-function-info (%cadr def) nil env))
  (nx1-treat-as-call w))


(defnx1 nx1-function function (arg &aux fn afunc)
  (if (symbolp arg)
    (progn
      (when (macro-function arg *nx-lexical-environment*)
        (nx-error
         "~S can't be used to reference lexically visible macro ~S." 
         'function arg))
      (if (multiple-value-setq (fn afunc) (nx-lexical-finfo arg))
        (progn
          (when afunc 
            (incf (afunc-fn-refcount afunc))
            (when (%ilogbitp $fbitbounddownward (afunc-bits afunc))
              (incf (afunc-fn-downward-refcount afunc))))
          (nx1-symbol (%cddr fn)))
        (progn
          (while (setq fn (assq arg *nx-synonyms*))
            (setq arg (%cdr fn)))
          (nx1-form `(%function ',arg)))))
    (if (and (consp arg) (eq (%car arg) 'setf))
      (nx1-form `(function ,(nx-need-function-name arg)))
      (nx1-ref-inner-function nil arg))))

(defnx1 nx1-nfunction nfunction (name def)
 (nx1-ref-inner-function name def))

(defun nx1-ref-inner-function (name def &optional afunc)
  (setq afunc (nx1-compile-inner-function name def afunc))
  (setf (afunc-fn-refcount afunc) 1)
  (nx1-afunc-ref afunc))

(defun nx1-compile-inner-function (name def 
                                        &optional p (env *nx-lexical-environment*) 
                                        &aux (q *nx-current-function*))
  (unless p (setq p (make-afunc)))
  (setf (afunc-parent p) q)
  (setf (afunc-parent q) *nx-parent-function*)
  (setf (afunc-tags q) *nx-tags*)
  (setf (afunc-blocks q) *nx-blocks*)
  (setf (afunc-inner-functions q) (push p *nx-inner-functions*))
  (setf (lexenv.lambda env) q)
  (nx1-compile-lambda name def p q env *nx-current-compiler-policy* *nx-load-time-eval-token*)) ;returns p.

(defun nx1-afunc-ref (afunc)
  (let ((op (if (afunc-inherited-vars afunc)
              (%nx1-operator closed-function)
              (%nx1-operator simple-function)))
        (ref (afunc-ref-form afunc)))
    (if ref
      (%rplaca ref op) ; returns ref
      (setf (afunc-ref-form afunc)
            (make-acode
             op
             afunc)))))
    
(defnx1 nx1-%function %function (form &aux symbol)
  (let ((sym (nx1-form form)))
    (if (and (eq (car sym) (%nx1-operator immediate))
             (setq symbol (cadr sym))
             (symbolp symbol))
      (progn
        (nx1-call-result-type symbol)   ; misnamed.  Checks for (un-)definedness.
        (make-acode (%nx1-default-operator) symbol))
      (make-acode (%nx1-operator call) (nx1-immediate '%function) (list nil (list sym))))))

(defnx1 nx1-tagbody tagbody (&rest args)
  (let* ((newtags nil)
         (*nx-lexical-environment* (new-lexical-environment *nx-lexical-environment*))
	 (pending (make-pending-declarations))
         (*nx-bound-vars* *nx-bound-vars*)
         (catchvar (nx-new-temp-var pending "tagbody-catch-tag"))
         (indexvar (nx-new-temp-var pending "tagbody-tag-index"))
         (counter (list 0))
         (looplabel (cons nil nil))
         (*nx-tags* *nx-tags*))
    (dolist (form args)
      (when (atom form)
        (if (or (symbolp form) (integerp form))
          (if (assoc form newtags)
            (nx-error "Duplicate tag in TAGBODY: ~S." form)
            (push (list form nil counter catchvar nil nil) newtags))
          (nx-error "Illegal form in TAGBODY: ~S." form))))
    (dolist (tag (setq newtags (nreverse newtags)))
      (push tag *nx-tags*))
    (let* ((body nil))
      (dolist (form args (setq body (nreverse body)))
        (push 
         (if (atom form)
           (let ((info (nx-tag-info form)))
             (%rplaca (%cdr (%cdr (%cdr (%cdr info)))) t)
             (cons (%nx1-operator tag-label) info))
           (nx1-form form))
         body))
      (if (eq 0 (%car counter))
        (make-acode (%nx1-operator local-tagbody) newtags body)
        (progn
          (nx-set-var-bits catchvar (logior (nx-var-bits catchvar)
                                            (%ilsl $vbitdynamicextent 1)))
          (nx-inhibit-register-allocation)   ; There are alternatives ...
          (dolist (tag (reverse newtags))
            (when (%cadr tag)
              (push  
               (nx1-form `(if (eql ,(var-name indexvar) ,(%cadr tag)) (go ,(%car tag))))
               body)))
          (make-acode
           (%nx1-operator let*)
           (list catchvar indexvar)
           (list (make-acode (%nx1-operator cons) *nx-nil* *nx-nil*) *nx-nil*)
           (make-acode
            (%nx1-operator local-tagbody)
            (list looplabel)
            (list
             (cons (%nx1-operator tag-label) looplabel)
             (make-acode
              (%nx1-operator if)
              (make-acode 
               (%nx1-operator setq-lexical)
               indexvar
               (make-acode 
                (%nx1-operator catch)
                (nx1-form (var-name catchvar)) 
                (make-acode
                 (%nx1-operator local-tagbody)
                 newtags
                 body)))
              (make-acode (%nx1-operator local-go) looplabel)
              *nx-nil*)))
           0))))))



(defnx1 nx1-go go (tag)
  (multiple-value-bind (info closed)
                       (nx-tag-info tag)
    (unless info (nx-error "Can't GO to tag ~S." tag))
    (if (not closed)
      (let ((defnbackref (cdr (cdr (cdr (cdr info))))))
        (if (car defnbackref) 
          (rplaca (cdr defnbackref) t))
        (make-acode (%nx1-operator local-go) info))
      (progn

        (make-acode
         (%nx1-operator throw) (nx1-symbol (var-name (cadddr info))) (nx1-form closed))))))




;;; address-expression should return a fixnum; that's our little
;;; secret.  result spec can be NIL, :void, or anything that an
;;; arg-spec can be.  arg-spec can be :double, :single, :address,
;;; :signed-doubleword, :unsigned-doubleword, :signed-fullword,
;;; :unsigned-fullword, :signed-halfword, :unsigned-halfword,
;;; :signed-byte, or :unsigned-byte

(defparameter *arg-spec-keywords*
  '(:double-float :single-float :address :signed-doubleword
    :unsigned-doubleword :signed-fullword :unsigned-fullword
    :signed-halfword :unsigned-halfword :signed-byte :unsigned-byte))


(defnx1 nx1-ff-call ((%ff-call)) (address-expression &rest arg-specs-and-result-spec)
   (nx1-ff-call-internal
    address-expression arg-specs-and-result-spec
    (ecase (backend-name *target-backend*)
      (:linuxppc32 (%nx1-operator eabi-ff-call))
      (:darwinppc32 (%nx1-operator poweropen-ff-call)))))

(defnx1 nx1-syscall ((%syscall)) (idx &rest arg-specs-and-result-spec)
   (nx1-ff-call-internal	
    idx arg-specs-and-result-spec
    (ecase (backend-name *target-backend*)
      (:linuxppc32 (%nx1-operator linux-syscall))
      (:darwinppc32 (%nx1-operator darwin-syscall)))))

(defun nx1-ff-call-internal (address-expression arg-specs-and-result-spec operator )
  (let* ((specs ())
         (vals ())
	 (darwin-target-p (or (eql operator (%nx1-operator darwin-syscall))
			      (eql operator (%nx1-operator poweropen-ff-call))))
	 (monitor (eq (car arg-specs-and-result-spec) :monitor-exception-ports))
         (arg-specs (butlast arg-specs-and-result-spec))
         (result-spec (car (last arg-specs-and-result-spec))))
    (if monitor
      (setq arg-specs (cdr arg-specs)))
    (unless (evenp (length arg-specs))
      (error "odd number of arg-specs"))
    (loop
      (when (null arg-specs) (return))
      (let* ((arg-keyword (pop arg-specs))
	     (value (pop arg-specs)))
        (if (or (memq arg-keyword *arg-spec-keywords*)
		(and darwin-target-p
		     (typep arg-keyword 'unsigned-byte)))
          (progn 
            (push arg-keyword specs)
            (push value vals))
	   (error "Unknown argument spec: ~s" arg-keyword))))
    (unless (or (eq result-spec :void)
		(memq result-spec *arg-spec-keywords*))
      (error "Unknown result spec: ~s" result-spec))
    (make-acode operator
		(nx1-form address-expression)
		(nreverse specs)
		(mapcar #'nx1-form (nreverse vals))
		result-spec
		monitor)))
  
(defnx1 nx1-block block (blockname &body forms)
  (let* ((*nx-blocks* *nx-blocks*)
         (*nx-lexical-environment* (new-lexical-environment *nx-lexical-environment*))
         (*nx-bound-vars* *nx-bound-vars*)
         (tagvar (nx-new-temp-var (make-pending-declarations)))
         (thisblock (cons (setq blockname (nx-need-sym blockname)) tagvar))
         (body nil))
    (push thisblock *nx-blocks*)
    (setq body (nx1-progn-body forms))
    (%rplacd thisblock nil)
    (let ((tagbits (nx-var-bits tagvar)))
      (if (not (%ilogbitp $vbitclosed tagbits))
        (if (neq 0 (%ilogand $vrefmask tagbits))
          (make-acode 
           (%nx1-operator local-block)
           thisblock
           body)
          body)
        (progn
          (nx-set-var-bits tagvar (%ilogior (%ilsl $vbitdynamicextent 1) tagbits))
          (nx-inhibit-register-allocation)   ; Could also set $vbitnoreg in all setqed vars, or keep track better
          (make-acode
           (%nx1-operator local-block)
           thisblock
           (make-acode
            (%nx1-operator let)
            (list tagvar)
            (list (make-acode (%nx1-operator cons) (nx1-form nil) (nx1-form nil)))
            (make-acode
             (%nx1-operator catch)
             (make-acode (%nx1-operator lexical-reference) tagvar)
             body)
            0)))))))

(defnx1 nx1-return-from return-from (blockname &optional value)
  (multiple-value-bind (info closed)
                       (nx-block-info (setq blockname (nx-need-sym blockname)))
    (unless info (nx-error "Can't RETURN-FROM block : ~S." blockname))
    (unless closed (nx-adjust-ref-count (cdr info)))
    (make-acode 
     (if closed
       (%nx1-operator throw)
       (%nx1-operator local-return-from))
     (if closed
       (nx1-symbol (var-name (cdr info)))
       info)
     (nx1-form value))))

(defnx1 nx1-funcall ((funcall)) (func &rest args)
  (let ((name func))
    (if (and (consp name)
             (eq (%car name) 'function)
             (consp (%cdr name))
             (null (%cddr name))
             (or
              (if (symbolp (setq name (%cadr name)))
                (or (not (macro-function name *nx-lexical-environment*))
                    (nx-error "Can't funcall macro function ~s ." name)))
              (and (consp name) 
                   (or (eq (%car name) 'lambda)
                       (setq name (nx-need-function-name name))))))
      (nx1-form (cons name args))  ; This picks up call-next-method evil.
      (nx1-call (nx1-form func) args nil t))))

(defnx1 nx1-multiple-value-call multiple-value-call (value-form &rest args)
  (make-acode (%nx1-default-operator)
              (nx1-form value-form)
              (nx1-formlist args)))

#|
(defun nx1-call-name (fn &aux (name (nx1-func-name fn)))
  (if (and name (or (eq (%car fn) 'quote) (null (nx-lexical-finfo name))))
    (make-acode (%nx1-operator immediate) name)
    (or name (nx1-form fn))))
|#

(defnx1 nx1-compiler-let compiler-let (bindings &body forms)
  (let* ((vars nil)
         (varinits nil))
    (dolist (pair bindings)
      (push (nx-pair-name pair) vars)
      (push (eval (nx-pair-initform pair)) varinits))
   (progv (nreverse vars) (nreverse varinits) (nx1-catch-body forms))))

(defnx1 nx1-fbind fbind (fnspecs &body body &environment old-env)
  (let* ((fnames nil)
         (vars nil)
         (vals nil))
    (dolist (spec fnspecs (setq vals (nreverse vals)))
      (destructuring-bind (fname initform) spec
        (push (setq fname (nx-need-function-name fname)) fnames)
        (push (nx1-form initform) vals)))
    (let* ((new-env (new-lexical-environment old-env))
           (*nx-bound-vars* *nx-bound-vars*)
           (*nx-lexical-environment* new-env)
	   (pending (make-pending-declarations)))
      (dolist (fname fnames)        
        (let ((var (nx-new-var pending (make-symbol (symbol-name fname)))))
          (nx-set-var-bits var (%ilogior (%ilsl $vbitignoreunused 1)
                                         (nx-var-bits var)))
          (let ((afunc (make-afunc)))
            (setf (afunc-bits afunc) (%ilsl $fbitruntimedef 1))
            (setf (afunc-lfun afunc) var)
            (push var vars)
            (push (cons fname (cons 'function (cons afunc (var-name var)))) (lexenv.functions new-env)))))
      (make-acode
       (%nx1-operator let)
       vars
       vals
       (nx1-env-body body old-env)
       *nx-new-p2decls*))))

(defun maybe-warn-about-nx1-alphatizer-binding (funcname)
  (when (and (symbolp funcname)
             (gethash funcname *nx1-alphatizers*))
    (nx1-whine :special-fbinding funcname)))

(defnx1 nx1-flet flet (defs &body forms)
  (with-nx-declarations (pending)
    (let* ((env *nx-lexical-environment*)
           (*nx-lexical-environment* env)
           (*nx-bound-vars* *nx-bound-vars*)
           (new-env (new-lexical-environment env))
           (names nil)
           (funcs nil)
           (pairs nil)
           (fname nil)
           (name nil))
      (multiple-value-bind (body decls) (parse-body forms env nil)
        (nx-process-declarations pending decls)
        (dolist (def defs (setq names (nreverse names) funcs (nreverse funcs)))
          (destructuring-bind (funcname lambda-list &body flet-function-body) def
            (setq fname (nx-need-function-name funcname))
            (maybe-warn-about-nx1-alphatizer-binding funcname)
            (multiple-value-bind (body decls)
                                 (parse-body flet-function-body env)
              (let ((func (make-afunc)))
                (setf (afunc-environment func) env
                      (afunc-lambdaform func) `(lambda ,lambda-list
                                                     ,@decls
                                                     (block ,(if (consp funcname) (%cadr funcname) funcname)
                                                       ,@body)))
                (push func funcs)
                (when (and *nx-next-method-var*
                             (eq funcname 'call-next-method)
                             (null *nx-call-next-method-function*))
                    (setq *nx-call-next-method-function* func))             
                (push (cons funcname func) pairs)
                (if (consp funcname)
                  (setq funcname fname))
                (push (setq name (make-symbol (symbol-name funcname))) names)
                (push (cons funcname (cons 'function (cons func name))) (lexenv.functions new-env))))))
        (let ((vars nil)
              (rvars nil)
              (rfuncs nil))
          (dolist (sym names vars) (push (nx-new-var pending sym) vars))
          (nx-effect-other-decls pending new-env)
          (setq body (let* ((*nx-lexical-environment* new-env))
                       (nx1-dynamic-extent-functions vars new-env)
                       (nx1-env-body body env)))
          (dolist (pair pairs)
            (let ((afunc (cdr pair))
                  (var (pop vars)))
              (when (or (afunc-callers afunc)
                        (neq 0 (afunc-fn-refcount afunc))
                        (neq 0 (afunc-fn-downward-refcount afunc)))
                (push (nx1-compile-inner-function (%car pair)
                                                  (afunc-lambdaform afunc)
                                                  afunc
                                                  (afunc-environment afunc))
                      rfuncs)
                (push var rvars))))
          (nx-reconcile-inherited-vars rfuncs)
          (dolist (f rfuncs) (nx1-afunc-ref f))
          (make-acode
           (%nx1-operator flet)
           rvars
           rfuncs
           body
           *nx-new-p2decls*))))))

(defun nx1-dynamic-extent-functions (vars env)
  (let ((bits nil)
        (varinfo nil))
    (dolist (decl (lexenv.fdecls env))
      (let ((downward-guy (if (eq (cadr decl) 'dynamic-extent) (car decl))))
        (when downward-guy
          (multiple-value-bind (finfo afunc) (nx-lexical-finfo downward-guy)
            (when (and afunc 
                       (not (%ilogbitp $fbitdownward (setq bits (afunc-bits afunc))))
                       (setq varinfo (and (consp (%cdr finfo)) (nx-lex-info (%cddr finfo))))
                       (memq varinfo vars))
              (setf (afunc-bits afunc) 
                    (%ilogior 
                     bits 
                     (%ilsl $fbitdownward 1)
                     (%ilsl $fbitbounddownward 1)))
              (nx-set-var-bits varinfo (%ilogior (%ilsl $vbitdynamicextent 1) (nx-var-bits varinfo))))))))))
          
(defnx1 nx1-labels labels (defs &body forms)
  (with-nx-declarations (pending)
    (let* ((env *nx-lexical-environment*)
           (old-env (lexenv.parent-env env))
           (*nx-bound-vars* *nx-bound-vars*)
           (func nil)
           (funcs nil)
           (funcrefs nil)
           (bodies nil)
           (vars nil)
           (blockname nil)
           (fname nil)
           (name nil))
      (multiple-value-bind (body decls) (parse-body forms env nil)
        (nx-process-declarations pending decls)
        (dolist (def defs (setq funcs (nreverse funcs) bodies (nreverse bodies)))
          (destructuring-bind (funcname lambda-list &body labels-function-body) def
            (maybe-warn-about-nx1-alphatizer-binding funcname)
            (push (setq func (make-afunc)) funcs)
            (setq blockname funcname)
            (setq fname (nx-need-function-name funcname))
            (when (consp funcname)
              (setq blockname (%cadr funcname) funcname fname))
            (let ((var (nx-new-var pending (setq name (make-symbol (symbol-name funcname))))))
              (nx-set-var-bits var (%ilsl $vbitignoreunused 1))
              (push var vars))
            (push func funcrefs)
            (multiple-value-bind (body decls)
                                 (parse-body labels-function-body old-env)
              (push (cons funcname (cons 'function (cons func name))) (lexenv.functions env))
              (let* ((expansion `(lambda ,lambda-list 
                                   ,@decls 
                                   (block ,blockname
                                     ,@body))))
                (setf (afunc-lambdaform func) expansion
                      (afunc-environment func) env)
                (push (cons funcname expansion)
                            bodies)))))
        (nx-effect-other-decls pending env)
        (nx1-dynamic-extent-functions vars env)
        (dolist (def bodies)
          (nx1-compile-inner-function (car def) (cdr def) (setq func (pop funcs))))
        (setq body (nx1-env-body body old-env))
        (nx-reconcile-inherited-vars funcrefs)
        (dolist (f funcrefs) (nx1-afunc-ref f))
        (make-acode
         (%nx1-operator labels)
         (nreverse vars)
         (nreverse funcrefs)
         body
         *nx-new-p2decls*)))))

(defnx1 nx1-put-xxx ((%put-ptr) (%put-long) (%put-full-long)  (%put-ostype) (%put-word) (%put-byte)) 
        (ptr newval &optional (offset 0) &aux (op *nx-sfname*))
  (make-acode
   (%nx1-operator immediate-put-xxx)
   (case op
     (%put-ptr 0)
     (%put-word 2)
     (%put-byte 3)
     (t 1))
   (make-acode (%nx1-operator %macptrptr%) (nx1-form ptr))
   (let ((valform (nx1-form newval)))
     (if (or (eq op '%put-ptr) )
       (make-acode (%nx1-operator %macptrptr%) valform)
       valform))
   (nx1-form offset)))

(defnx1 nx1-set-bit ((%set-bit)) (ptr offset &optional (newval nil newval-p))
  (unless newval-p (setq newval offset offset 0))
  (make-acode
   (%nx1-operator %set-bit)
   (make-acode (%nx1-operator %macptrptr%) (nx1-form ptr))
   (nx1-form offset)
   (nx1-form newval)))
               
(defnx1 nx1-set-xxx ((%set-ptr) (%set-long)  (%set-word) (%set-byte)) 
        (ptr offset &optional (newval nil new-val-p) &aux (op *nx-sfname*))
  (unless new-val-p (setq newval offset offset 0))
  (make-acode
   (%nx1-operator %immediate-set-xxx)
   (case op
     (%set-ptr 0)
     (%set-word 2)
     (%set-byte 3)
     (t 1))
   (make-acode (%nx1-operator %macptrptr%) (nx1-form ptr))
   (nx1-form offset)
   (nx1-form newval)))


(defnx1 nx1-get-bit ((%get-bit)) (ptrform &optional (offset 0))
  (make-acode
   (%nx1-operator typed-form)
   'bit
   (make-acode
    (%nx1-operator %get-bit)
    (make-acode (%nx1-operator %macptrptr%) (nx1-form ptrform))
    (nx1-form offset))))

(defnx1 nx1-get-xxx ((%get-long)  (%get-full-long)  (%get-signed-long)
                     (%get-fixnum) 
                     (%get-word) (%get-unsigned-word)
                     (%get-byte) (%get-unsigned-byte)
                     (%get-signed-word) 
                     (%get-signed-byte) 
                     (%get-unsigned-long))
  (ptrform &optional (offset 0))
  (let* ((sfname *nx-sfname*)
         (flagbits (case sfname
                     ((%get-long %get-full-long  %get-signed-long) 1)
                     (%get-fixnum 33)
		     
                     ((%get-word %get-unsigned-word) 2)
                     (%get-signed-word 6)
                     ((%get-byte %get-unsigned-byte) 3)
                     (%get-signed-byte 7)
                     (%get-unsigned-long 9))))
    (declare (fixnum flagbits))
    (make-acode (%nx1-operator typed-form)
                (case (logand 15 flagbits)
                  (1 (case sfname
                       ((%get-fixnum ) 'fixnum)
                       (t '(signed-byte 32))))
                  (9 '(unsigned-byte 32))
                  (2 '(unsigned-byte 16))
                  (6 '(signed-byte 16))
                  (3 '(unsigned-byte 8))
                  (7 '(signed-byte 8)))
                (make-acode 
                 (%nx1-operator immediate-get-xxx)
                 flagbits
                 (make-acode (%nx1-operator %macptrptr%) (nx1-form ptrform))
                 (nx1-form offset)))))

(defnx1 nx1-%get-ptr ((%get-ptr) ) (ptrform &optional (offset 0))
  (make-acode
   (%nx1-operator %consmacptr%)
   (make-acode
    (%nx1-operator immediate-get-ptr)
    (if (eq *nx-sfname* '%get-ptr) 0 16)
    (make-acode (%nx1-operator %macptrptr%) (nx1-form ptrform))
    (nx1-form offset))))

(defnx1 nx1-%get-float ((%get-single-float)
			(%get-double-float)) (ptrform &optional (offset 0))
  (make-acode
   (%nx1-operator typed-form)
   (if (eq *nx-sfname* '%get-single-float)
     'single-float
     'double-float)
   (make-acode
    (%nx1-default-operator)
    (make-acode (%nx1-operator %macptrptr%) (nx1-form ptrform))
    (nx1-form offset))))

(defnx1 nx1-%set-float ((%set-single-float)
			(%set-double-float)) (ptrform offset &optional (newval nil newval-p))
  (unless newval-p
    (setq newval offset
	  offset 0))
  (make-acode
   (%nx1-operator typed-form)
   (if (eq *nx-sfname* '%set-single-float)
     'single-float
     'double-float)
   (make-acode
    (%nx1-default-operator)
    (make-acode (%nx1-operator %macptrptr%) (nx1-form ptrform))
    (nx1-form offset)
    (nx1-form newval))))

(defnx1 nx1-let let (pairs &body forms &environment old-env)
  (let* ((vars nil)
         (vals nil)
         (varspecs nil))
    (with-nx-declarations (pending)
      (multiple-value-bind (body decls)
                           (parse-body forms *nx-lexical-environment* nil)
        (nx-process-declarations pending decls)
        ; Make sure that the initforms are processed in the outer
        ; environment (in case any declaration handlers side-effected
        ; the environment.)
        (let* ((*nx-lexical-environment* old-env))
          (dolist (pair pairs)
            (push (nx-need-var (nx-pair-name pair)) vars)
            (push (nx1-typed-var-initform pending (car vars) (nx-pair-initform pair)) vals)))
        (let* ((*nx-bound-vars* (append vars *nx-bound-vars*))
               (varbindings (nx1-note-var-bindings
                             (dolist (sym vars varspecs)
                               (push (nx-new-var pending sym) varspecs))
                             (setq vals (nreverse vals))))
               (form 
                (make-acode 
                 (%nx1-operator let)
                 varspecs
                 vals
                 (progn
                   (nx-effect-other-decls pending *nx-lexical-environment*)
                   (nx1-env-body body old-env))
                 *nx-new-p2decls*)))
          (nx1-check-var-bindings varbindings)
          (nx1-punt-bindings varspecs vals)
          form)))))



;((lambda (lambda-list) . body) . args)
(defun nx1-lambda-bind (lambda-list args body &optional (body-environment *nx-lexical-environment*))
  (let* ((old-env body-environment)
         (arg-env *nx-lexical-environment*)
         (arglist nil)
         var-bound-vars
         vars vals vars* vals*)
    ; If the lambda list contains &LEXPR, we can't do it.  Yet.
    (multiple-value-bind (ok req opttail resttail) (verify-lambda-list lambda-list)
      (declare (ignore req opttail))
      (when (and ok (eq (%car resttail) '&lexpr))
        (return-from nx1-lambda-bind (nx1-call (nx1-form `(lambda ,lambda-list ,@body)) args))))
    (let* ((*nx-lexical-environment* body-environment)
           (*nx-bound-vars* *nx-bound-vars*))
      (with-nx-declarations (pending)
        (multiple-value-bind (body decls) (parse-body body *nx-lexical-environment*)
          (nx-process-declarations pending decls)
          (multiple-value-bind (req opt rest keys auxen)
                               (nx-parse-simple-lambda-list pending lambda-list)
            (let* ((*nx-lexical-environment* arg-env))
              (setq arglist (nx1-formlist args)))
            (nx-effect-other-decls pending *nx-lexical-environment*)
            (setq body (nx1-env-body body old-env))
            (while req
              (when (null arglist)
                (nx-error "Not enough args in Lambda form: ~S ~S." lambda-list args))
              (let* ((var (pop req))
                     (val (pop arglist))
                     (binding (nx1-note-var-binding var val)))
                (push var vars)
                (push val vals)
                (when binding (push binding var-bound-vars))))
            (nx1-check-var-bindings var-bound-vars)
            (nx1-punt-bindings vars vals)
            (destructuring-bind (&optional optvars inits spvars) opt
              (while optvars
                (if arglist
                  (progn
                    (push (%car optvars) vars) (push (%car arglist) vals)
                    (when (%car spvars) (push (%car spvars) vars) (push *nx-t* vals)))
                  (progn
                    (push (%car optvars) vars*) (push (%car inits) vals*)
                    (when (%car spvars) (push (%car spvars) vars*) (push *nx-nil* vals*))))
                (setq optvars (%cdr optvars) spvars (%cdr spvars) inits (%cdr inits)
                      arglist (%cdr arglist))))
            (if arglist
              (when (and (not keys) (not rest))
                (nx-error "Extra args in Lambda form: ~S ~S." lambda-list args))
              (when rest
                (push rest vars*) (push *nx-nil* vals*)
                (nx1-punt-bindings (cons rest nil) (cons *nx-nil* nil))
                (setq rest nil)))
            (when keys
              (let* ((punt nil))
                (destructuring-bind (kallowother keyvars spvars inits keyvect) keys
                  (do* ((pairs arglist (%cddr pairs)))
                       ((null pairs))
                    (let* ((keyword (car pairs)))
                      (when (or (not (acode-p keyword))
                                (neq (acode-operator keyword) (%nx1-operator immediate))
                                (eq (%cadr keyword) :allow-other-keys))
                        (return (setq punt t)))))
                  (do* ((nkeys (length keyvect))
                        (keyargs (make-sequence 'vector nkeys :initial-element nil))
                        (argl arglist (%cddr argl))
                        (n 0 (%i+ n 1))
                        idx arg hit)
                       ((null argl)
                        (unless rest
                          (while arglist
                            (push (%cadr arglist) vals)
                            (setq arglist (%cddr arglist))))
                        (dotimes (i (the fixnum nkeys))                      
                          (push (%car keyvars) vars*)
                          (push (or (%svref keyargs i) (%car inits)) vals*)
                          (when (%car spvars)
                            (push (%car spvars) vars*)
                            (push (if (%svref keyargs i) *nx-t* *nx-nil*) vals*))
                          (setq keyvars (%cdr keyvars) inits (%cdr inits) spvars (%cdr spvars)))
                        (setq keys hit))
                    (setq arg (%car argl))
                    (unless (and (not punt)
                                 (%cdr argl))
                      (let ((var (nx-new-temp-var pending)))
                        (when (or (null rest) (%ilogbitp $vbitdynamicextent (nx-var-bits rest)))
                          (nx-set-var-bits var (%ilogior (%ilsl $vbitdynamicextent 1) (nx-var-bits var))))
                        (setq body (make-acode
                                    (%nx1-operator debind)
                                    nil
                                    (make-acode 
                                     (%nx1-operator lexical-reference) var)
                                    nil 
                                    nil 
                                    rest 
                                    keys 
                                    auxen 
                                    nil 
                                    body 
                                    *nx-new-p2decls* 
                                    nil)
                              rest var keys nil auxen nil)
                        (return nil)))
                    (unless (or (setq idx (position (%cadr arg) keyvect))
                                (eq (%cadr arg) :allow-other-keys)
                                (and kallowother (symbolp (%cadr arg))))
                      (nx-error "Invalid keyword in Lambda form: ~S ~S."
                                lambda-list args))
                    (when (and idx (null (%svref keyargs idx)))
                      (setq hit t)
                      (%svset keyargs idx n))))))
            (destructuring-bind (&optional auxvars auxvals) auxen
              (let ((vars!% (nreconc vars* auxvars))
                    (vals!& (nreconc vals* auxvals)))
                (make-acode (%nx1-operator lambda-bind)
                            (append (nreverse vals) arglist)
                            (nreverse vars)
                            rest
                            keys
                            (list vars!% vals!&)
                            body
                            *nx-new-p2decls*)))))))))

(defun nx-inhibit-register-allocation (&optional (why 0))
  (let ((afunc *nx-current-function*))
    (setf (afunc-bits afunc)
          (%ilogior (%ilsl $fbitnoregs 1)
                    why
                    (afunc-bits afunc)))))



(defnx1 nx1-ppc-lap-function (ppc-lap-function) (name bindings &body body)
  (require "PPC-LAP" "ccl:compiler;ppc;ppc-lap")
  (setf (afunc-lfun *nx-current-function*) 
        (%define-ppc-lap-function name `((let ,bindings ,@body))
                                  (dpb (length bindings) $lfbits-numreq 0))))

(defnx1 nx1-sparc-lap-function (sparc-lap-function) (name bindings &body body)
  (declare (ftype (function (t t t t)) %define-sparc-lap-function))
  (require "SPARC-LAP")
  (setf (afunc-lfun *nx-current-function*) 
        (%define-sparc-lap-function name `((let ,bindings ,@body))
				    (dpb (length bindings) $lfbits-numreq 0))))



(defun nx1-env-body (body old-env)
  (do* ((form (nx1-progn-body body))
        (env *nx-lexical-environment* (lexenv.parent-env env)))
       ((or (eq env old-env) (null env)) form)
    (let ((vars (lexenv.variables env)))
      (if (consp vars)
        (dolist (var vars)
          (nx-check-var-usage var))))))

(defnx1 nx1-let* (let* %vreflet) (varspecs &body forms)
  (let* ((vars nil)
         (vals nil)
         (val nil)
         (var-bound-vars nil)
         (*nx-bound-vars* *nx-bound-vars*)
         (old-env *nx-lexical-environment*))
    (with-nx-declarations (pending)
      (multiple-value-bind (body decls)
                           (parse-body forms *nx-lexical-environment* nil)
        (nx-process-declarations pending decls)
        (dolist (pair varspecs)          
          (let* ((sym (nx-need-var (nx-pair-name pair)))
                 (var (progn 
                        (push (setq val (nx1-typed-var-initform pending sym (nx-pair-initform pair))) vals)
                        (nx-new-var pending sym)))
                 (binding (nx1-note-var-binding var val)))
            (when binding (push binding var-bound-vars))
            (push var vars)))
        (nx-effect-other-decls pending *nx-lexical-environment*)
        (let* ((result
                (make-acode 
                 (%nx1-default-operator)
                 (setq vars (nreverse vars))
                 (setq vals (nreverse vals))
                 (nx1-env-body body old-env)
                 *nx-new-p2decls*)))
          (nx1-check-var-bindings var-bound-vars)
          (nx1-punt-bindings vars vals)
          result)))))

(defnx1 nx1-multiple-value-bind multiple-value-bind 
        (varspecs bindform &body forms)
  (if (= (length varspecs) 1)
    (nx1-form `(let* ((,(car varspecs) ,bindform)) ,@forms))
    (let* ((vars nil)
           (*nx-bound-vars* *nx-bound-vars*)
           (old-env *nx-lexical-environment*)
           (mvform (nx1-form bindform)))
      (with-nx-declarations (pending)
        (multiple-value-bind (body decls)
                             (parse-body forms *nx-lexical-environment* nil)
          (nx-process-declarations pending decls)
          (dolist (sym varspecs)
            (push (nx-new-var pending sym t) vars))
          (nx-effect-other-decls pending *nx-lexical-environment*)
          (make-acode 
           (%nx1-operator multiple-value-bind)
           (nreverse vars)
           mvform
           (nx1-env-body body old-env)
           *nx-new-p2decls*))))))


;;; This isn't intended to be user-visible; there isn't a whole lot of 
;;; sanity-checking applied to the subtag.
(defnx1 nx1-%alloc-misc ((%alloc-misc)) (element-count subtag &optional (init nil init-p))
  (if init-p                            ; ensure that "init" is evaluated before miscobj is created.
    (make-acode (%nx1-operator %make-uvector)
                (nx1-form element-count)
                (nx1-form subtag)
                (nx1-form init))
    (make-acode (%nx1-operator %make-uvector)
                (nx1-form element-count)
                (nx1-form subtag))))

(defnx1 nx1-%lisp-word-ref (%lisp-word-ref) (base offset)
  (make-acode (%nx1-operator %lisp-word-ref)
              (nx1-form base)
              (nx1-form offset)))

(defnx1 nx1-ash (ash) (&whole call &environment env num amt)
  (cond ((eq amt 0) (nx1-form num))
        ((and (fixnump amt)
              (< amt 0))
	 (if (nx-form-typep num 'fixnum env)
	   (make-acode (%nx1-operator %iasr)
		       (make-acode (%nx1-operator fixnum)
				   (- amt))
		       (nx1-form num))
	   (if (nx-form-typep num '(unsigned-byte 32) env)
	     (make-acode (%nx1-operator natural-shift-right)
			 (nx1-form num)
			 (make-acode (%nx1-operator fixnum)
				     (- amt)))
	     (nx1-treat-as-call call))))
	((and (fixnump amt)
	      (< amt 32)
	      (nx-form-typep num '(unsigned-byte 32) env)
	      (nx-trust-declarations env)
	      (subtypep *nx-form-type* '(unsigned-byte 32)))
	 (make-acode (%nx1-operator natural-shift-left)
		     (nx1-form num)
		     (nx1-form amt)))
        ((and (fixnump amt)
              (<= 0 amt (integer-length most-positive-fixnum))
              (or (nx-form-typep num `(signed-byte ,(- (1+ (integer-length most-positive-fixnum))  amt)) env)
                  (and (nx-form-typep num 'fixnum env)
                       (nx-trust-declarations env)
                       (subtypep *nx-form-type* 'fixnum))))
         (nx1-form `(%ilsl ,amt ,num)))
        (t (nx1-treat-as-call call))))

    
        
(defun nx-badformat (&rest args)
 (nx-error "Bad argument format in ~S ." args))

(defnx1 nx1-eval-when eval-when (when &body body)
  (nx1-progn-body (if (or (memq 'eval when) (memq :execute when)) body)))

(defnx1 nx1-misplaced (declare) (&rest args)
  (nx-error "~S not expected in ~S." *nx-sfname* (cons *nx-sfname* args)))

