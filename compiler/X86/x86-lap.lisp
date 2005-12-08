;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates and contributors.
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


(in-package "X86")

;;; Intentionally very similar to RISC-LAP, but with some extensions
;;; to deal with alignment and with variable-length and/or span-
;;; dependent instructions.

(defvar *x86-lap-labels* ())
(defvar *x86-lap-instructions* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "DLL-NODE"))


(eval-when (:execute :load-toplevel)
  (defstruct (x86-instruction-element (:include ccl::dll-node))
    address)

  (defstruct (x86-lap-instruction (:include x86-instruction-element)
                                  (:constructor %make-x86-lap-instruction))
    template
    parsed-operands                     ;NIL, or a 1, 2, or 3-element vector
    (flags 0 :type fixnum)
    suffix
    base-opcode                         ;copied from template, maybe modified
    extension-opcode                    ;copied from template, maybe modified
    prefixes                            ;u8 vector or NIL
    (nprefixes 0)
    rex                                 ;the (64-bit) rex prefix
    rm                                  ;a modrm-byte structure (or NIL)
    sib                                 ;a sib-byte structure (or NIL)
    buffer                              ;a (simple-array (unsigned-byte 8) (*))
    buffer-offset                       ;offset in that buffer
    )

  (defstruct (x86-lap-note (:include x86-instruction-element))
    peer
    id)

  (defstruct (x86-lap-note-begin (:include x86-lap-note)))
  (defstruct (x86-lap-note-end (:include x86-lap-note)))
    
  (defstruct (x86-lap-label (:include x86-instruction-element)
                            (:constructor %%make-x86-lap-label (name)))
    name
    refs))

(defmethod print-object ((i x86-lap-instruction) stream)
  (let* ((template (x86-lap-instruction-template i))
         (operands (x86-lap-instruction-parsed-operands i)))
    (print-unreadable-object (i stream :type t)
      (when template
        (format stream "~a" (x86-instruction-template-name template))
        (let* ((suffix (x86-lap-instruction-suffix i)))
          (when suffix
            (format stream "[~c]" suffix)))
        (dotimes (i (length operands))
          (format stream " ~a" (unparse-operand (svref operands i))))))))

(defmethod print-object ((l x86-lap-label) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~a" (x86-lap-label-name l))))

;;; Labels

(defun make-x86-lap-label (name)
  (%%make-x86-lap-label name))

(defun find-x86-lap-label (name)
  (if (typep *x86-lap-labels* 'hash-table)
    (gethash name *x86-lap-labels*)
    (car (member name *x86-lap-labels* :test #'eq :key #'x86-lap-label-name))))

(defun find-or-create-x86-lap-label (name)
  (or (find-x86-lap-label name)
      (make-x86-lap-label name)))


;;; A label can only be emitted once.  Once it's been emitted, its pred/succ
;;; slots will be non-nil.

(defun x86-lap-label-emitted-p (lab)
  (not (null (x86-lap-label-pred lab))))

(defun emit-x86-lap-label (name)
  (let* ((lab (find-x86-lap-label name)))
    (if  lab 
      (when (x86-lap-label-emitted-p lab)
        (error "Label ~s: multiply defined." name))
      (setq lab (make-x86-lap-label name)))
    (ccl::append-dll-node lab *x86-lap-instructions*)))

(defun make-x86-lap-instruction ()
  ;; Might want to us a freelisting scheme: these things become
  ;; garbage quickly.
  (%make-x86-lap-instruction))

;;; Some instructions may be "span-dependent", e.g., come in
;;; different sizes depending on some attribute of their
;;; operands (like branch distance.)  Keep track of the
;;; minimum/maximum possible size as well as the actual
;;; size.  No x86-lap-instruction can have a size of 0 or
;;; a size greater than 15 bytes.
(defconstant x86-lap-instruction-flag-min-size-byte (byte 4 0))
(defun x86-lap-instruction-min-size (x)
  (ldb x86-lap-instruction-flag-min-size-byte (x86-lap-instruction-flags x)))
(defun (setf x86-lap-instruction-min-size) (new x)
  (setf
   (ldb x86-lap-instruction-flag-min-size-byte (x86-lap-instruction-flags x))
   new))

(defconstant x86-lap-instruction-flag-max-size-byte (byte 4 4))
(defun x86-lap-instruction-max-size (x)
  (ldb x86-lap-instruction-flag-max-size-byte (x86-lap-instruction-flags x)))
(defun (setf x86-lap-instruction-max-size) (new x)
  (setf
   (ldb x86-lap-instruction-flag-max-size-byte (x86-lap-instruction-flags x))
   new))

(defconstant x86-lap-instruction-flag-size-byte (byte 4 8))
(defun x86-lap-instruction-size (x)
  (ldb x86-lap-instruction-flag-size-byte (x86-lap-instruction-flags x)))
(defun (setf x86-lap-instruction-size) (new x)
  (setf
   (ldb x86-lap-instruction-flag-size-byte (x86-lap-instruction-flags x))
   new))

;;; "synthetic" instructions are introduced to satisfy alignment constraints.
;;; They can be removed or replaced if those constraints change
(defconstant x86-lap-instruction-flag-synthetic-bit (byte 1 12))
(defun x86-lap-instruction-synthetic-p (x)
  (not (zerop (ldb x86-lap-instruction-flag-synthetic-bit x))))
(defun (setf x86-lap-instruction-synthetic-p) (new x)
  (setf (ldb x86-lap-instruction-flag-synthetic-bit x) (if new 1 0))
  new)

(defstruct code-fragment
  (buffer (make-array 256 :element-type '(unsigned-byte 8)))
  (start 0)
  (current 0))

(defun code-fragment-push-byte (code-fragment byte)
  (let* ((buffer (code-fragment-buffer code-fragment))
         (current (code-fragment-current code-fragment)))
    (declare (fixnum current))
    (if (< current (length buffer))
      (setf (aref buffer current) byte
            (code-fragment-current code-fragment) (the fixnum (1+ current)))
      (let* ((start (code-fragment-start code-fragment))
             (n (- current start))
             (new (make-array 256 :element-type '(unsigned-byte 8))))
        (declare (fixnum start n))
        (dotimes (i n)
          (setf (aref new i) (aref buffer start))
          (incf start))
        (setf (aref new n) byte
              (code-fragment-buffer code-fragment) new
              (code-fragment-start code-fragment) 0
              (code-fragment-current code-fragment) (the fixnum (1+ n)))))
    byte))

(defun code-fragment-push-16 (code-fragment halfword)
  (code-fragment-push-byte code-fragment (ldb (byte 8 0) halfword))
  (code-fragment-push-byte code-fragment (ldb (byte 8 8) halfword))
  halfword)

(defun code-fragment-push-32 (code-fragment word)
  (code-fragment-push-16 code-fragment (ldb (byte 16 0) word))
  (code-fragment-push-16 code-fragment (ldb (byte 16 16) word))
  word)

(defun code-fragment-push-64 (code-fragment doubleword)
  (code-fragment-push-32 code-fragment (ldb (byte 32 0) doubleword))
  (code-fragment-push-32 code-fragment (ldb (byte 32 32) doubleword))
  doubleword)

(defun code-fragment-finish (code-fragment instruction)
  (let* ((start (code-fragment-start code-fragment))
         (current (code-fragment-current code-fragment))
         (buffer (code-fragment-buffer code-fragment))
         (size (- current start)))
    (declare (fixnum start current size))
    (setf (code-fragment-start code-fragment) current
          (x86-lap-instruction-buffer instruction) buffer
          (x86-lap-instruction-buffer-offset instruction) start
          (x86-lap-instruction-size instruction) size)
    size))



(defun lookup-x86-register (regname)
  (gethash (string regname) *x86-registers*))

(defun parse-x86-register-operand (regname)
  (let* ((r (lookup-x86-register regname)))
    (if r
      (make-x86-register-operand :type (logandc2 (reg-entry-reg-type r)
                                                 (encode-operand-type :baseIndex))
                                 :entry r)
      (error "Unknown X86 register ~s" regname))))

;;; It may seem strange to have an expression language in a lisp-based
;;; assembler, since lisp is itself a fairly reasonable expression
;;; language and EVAL is (in this context, at least) an adequate evaluation
;;; mechanism.  This may indeed be overkill, but there are reasons for
;;; wanting something beyond EVAL.
;;; This assumes that any expression that doesn't involve label addresses
;;; will always evaluate to the same value (in "the same" execution context).
;;; Expressions that do involve label references might only be evaluable
;;; after all labels are defined, and the value of such an expression may
;;; change (as label addresses are adjusted.)

;;; A "label address expression" looks like (^ lab), syntactically.  Tree-walk
;;; FORM, and return T if it contains a label address expression.

(defun label-address-expression-p (form)
  (and (consp form)
       (symbolp (car form))
       (string= (car form) "^")
       (consp (cdr form))
       (null (cddr form))))

(defun contains-label-address-expression (form)
  (cond ((label-address-expression-p form) t)
        ((atom form) nil)
        (t (dolist (sub (cdr form))
              (when (contains-label-address-expression sub)
                (return t))))))

(defstruct expression
  )


(defstruct (label-expression (:include expression))
  label)


;;; Represents a constant
(defstruct (constant-expression (:include expression))
  value)

;;; Also support 0, 1, 2, and many args, where at least one of those args
;;; is or contains a label reference.
(defstruct (application-expression (:include expression))
  operator)


(defstruct (unary-expression (:include application-expression))
  operand)


(defstruct (binary-expression (:include application-expression))
  operand0
  operand1)

(defstruct (n-ary-expression (:include application-expression))
  operands)

;;; Looks like a job for DEFMETHOD.
(defun expression-value (exp)
  (etypecase exp
    (constant-expression (constant-expression-value exp))
    (label-expression (x86-lap-label-address (label-expression-label exp)))
    (unary-expression (funcall (unary-expression-operator exp)
                               (expression-value (unary-expression-operand exp))))
    (binary-expression (funcall (binary-expression-operator exp) 
                                (expression-value (binary-expression-operand0 exp))
                                (expression-value (binary-expression-operand1 exp))))
    (n-ary-expression (apply (n-ary-expression-operator exp)
                             (mapcar #'expression-value (n-ary-expression-operands exp))))))

;;; Expression might contain unresolved labels.  Return 0 if so (even
;;; if everything -could- be resolved.)
(defun early-expression-value (expression)
  (etypecase expression
    (constant-expression (constant-expression-value expression))
    (expression 0)))


(defun parse-expression (form)
  (if (label-address-expression-p form)
    (make-label-expression :label (find-or-create-x86-lap-label (cadr form)))
    (if (contains-label-address-expression form)
      (destructuring-bind (op &rest args) form
        (case (length args)
          (1 (make-unary-expression :operator op :operand (parse-expression (car args))))
          (2 (make-binary-expression :operator op :operand0 (parse-expression (car args))
                                     :operand1 (parse-expression (cadr args))))
          (t (make-n-ary-expression :operator op :operands (mapcar #'parse-expression args)))))
      (multiple-value-bind (value condition)
          (eval form)
        (if condition
          (error "~a signaled during assembly-time evaluation of form ~s" condition form)
          (make-constant-expression :value value))))))


`
(defun parse-x86-register-operand (regname)
  (let* ((r (lookup-x86-register regname)))
    (if r
      (make-x86-register-operand :type (logandc2 (reg-entry-reg-type r)
                                                 (encode-operand-type :baseIndex))
                                 :entry r)
      (error "Unknown X86 register ~s" regname))))

(defun parse-x86-label-reference (instruction name)
  (let* ((lab (or (find-x86-lap-label name)
                  (make-x86-lap-label name))))
    (push instruction (x86-lap-label-refs lab))
    (make-x86-label-operand :type (encode-operand-type :label)
                            :label lab)))

(defun check-base-and-index-regs (instruction base index)
  (let* ((regxx (if (and (x86-lap-instruction-prefixes instruction)
                         (not (zerop (aref (x86-lap-instruction-prefixes instruction) addr-prefix))))
                  (encode-operand-type :reg32)
                  (encode-operand-type :reg64))))
    (if (or (and base
                 (not (logtest (reg-entry-reg-type base) regxx))
                 (or (not (eql (reg-entry-reg-type base)
                               (encode-operand-type :baseindex)))
                     index))
            (and index
                 (not (eql (logior regxx (encode-operand-type :baseindex))
                           (logand (reg-entry-reg-type index)
                                   (logior regxx
                                           (encode-operand-type :baseindex)))))))
      (if base
        (if index
          (error "Invalid base/index registers ~s/~s" base index)
          (error "Invalid base register ~s" base))
        (error "Invalid index register ~s" index)))))


;;; Syntax is:
;;; ([seg] [disp] [base] [index] [scale])
;;; A [seg] by itself isn't too meaningful; the same is true
;;; of a few other combinations.
(defun parse-x86-memory-operand (instruction form)
  (flet ((register-operand-p (form)
           (and (consp form)
                (symbolp (car form))
                (string= (car form) '%)
                (destructuring-bind (regname) (cdr form)
                  (or (lookup-x86-register regname)
                      (error "Unknown register ~s" regname))))))
  (let* ((seg nil)
         (disp nil)
         (base nil)
         (index nil)
         (scale nil))
    (do* ((f form (cdr f)))
         ((null f)
          (if (or disp base index)
            (progn
              (check-base-and-index-regs instruction base index)
              (make-x86-memory-operand 
               :type (if (or base index)
                       (encode-operand-type :baseindex)
                       (encode-operand-type :disp))
               :seg seg
               :disp disp
               :base base
               :index index
               :scale scale))
            (error "No displacement, base,  or index in ~s" form)))
      (let* ((head (car f))
             (r (register-operand-p head)))
        (if r
          (if (logtest (reg-entry-reg-type r)
                       (encode-operand-type :sreg2 :sreg3))
            ;; A segment register - if present - must be first
            (if (eq f form)
              (setq seg head)
              (error "Segment register ~s not valid in ~s" form))
            ;; Some other register.  Assume base if this is the
            ;; first gpr.  If we find only one gpr and a significant
            ;; scale factor, make that single gpr be the index.
            (if base
              (if index
                (error "Extra register ~s in memory address ~s" head form)
                (setq index r))
              (setq base r)))
          ;; Not a register, so head is either a displacement or
          ;; a scale factor.
          (if (and (null (cdr f))
                   (or disp base index))
            (let* ((exp (parse-expression head))
                   (val (if (typep exp 'constant-expression)
                          (expression-value exp))))
              (case val
                ((1 2 4 8)
                 (if (and base (not index))
                   (setq index base base nil))
                 (setq scale val))
                (t
                 (error "Invalid scale factor ~s in ~s" head form))))
            (if (not (or disp base index))
              (setq disp (parse-expression head))
              (error "~& not expected in ~s" head form)))))))))

            
          
(defun encode-immediate-type (val)
  (typecase val
    (null (encode-operand-type :imm32))
    ((signed-byte 8) (encode-operand-type :imm8 :imm8s))
    ((signed-byte 32) (encode-operand-type :imm32 :imm32s))
    (t (encode-operand-type :imm64))))

     
    

;;; Operand syntax:
;;; (^ x) -> labelref
;;; (% x) -> register
;;; ($ x) -> immediate
;;; (* x) -> x with :jumpabsolute attribute
;;; (@ x) -> memory operand
;;; x -> shorthand for (@ x)
(defun parse-x86-operand (instruction form)
  (if (consp form)
    (let* ((head (car form)))
      (if (symbolp head)
        (cond ((string= head '$)
               (destructuring-bind (immval) (cdr form)
                 (let* ((expr (parse-expression immval))
                        (value (early-expression-value  expr))
                        (type (encode-immediate-type value)))
                 (make-x86-immediate-operand :type type
                                             :value expr))))
              ((string= head '%)
               (destructuring-bind (reg) (cdr form)
                 (parse-x86-register-operand reg)))
              ((string= head '*)
               (destructuring-bind (subop) (cdr form)
                 (let* ((op (parse-x86-operand subop)))
                   (setf (x86-operand-type op)
                         (logior (encode-operand-type :jumpabsolute)
                                 (x86-operand-type op)))
                   op)))
              ((string= head '@)
               (parse-x86-memory-operand instruction (cdr form)))
              ((string= head '^)
               (destructuring-bind (lab) (cdr form)
                 (parse-x86-label-reference instruction lab)))
              (t (error "unknown X86 operand: ~s" form)))
        (error "unknown X86 operand: ~s" form)))
    ;; Treat an atom as a displacement, which is itself a
    ;; form of memory operand
    (parse-x86-memory-operand instruction `(,form))))

;;; This basically finds a syntactically matching template.
;;; There can still be lots of semantic errors, e.g., "movq %eax, %di"
;;; has both operands the wrong size.
(defun match-template (template parsed-operands suffix)
  (flet ((match (overlap given)
           (and
            (not (zerop (logandc2 overlap (encode-operand-type :jumpabsolute))))
            (= (logand given (encode-operand-type :baseindex :jumpabsolute))
               (logand overlap (encode-operand-type :baseindex :jumpabsolute)))))
         (consistent-register-match (m0 g0 t0 m1 g1 t1)
           (let* ((g0&reg (logand g0 (encode-operand-type :reg)))
                  (g1&reg (logand g1 (encode-operand-type :reg))))
             (or (zerop g0&reg)
                 (zerop g1&reg)
                 (= g0&reg g1&reg)
                 (not
                  (logtest
                   (if (logtest m0 (encode-operand-type :acc))
                     (encode-operand-type :reg)
                     t0)
                   (if (logtest m1 (encode-operand-type :acc))
                     (encode-operand-type :reg)
                     t1)))))))
    (let* ((nops (length parsed-operands))
           (type0 (if (> nops 0) (x86-operand-type (svref parsed-operands 0))))
           (type1 (if (> nops 1) (x86-operand-type (svref parsed-operands 1))))
           (type2 (if (> nops 2) (x86-operand-type (svref parsed-operands 2)))))
      (declare (fixnum nops))
      (let* ((suffix-check
              (case suffix
                (#\b (encode-opcode-modifier :no-bsuf))
                (#\w (encode-opcode-modifier :no-wsuf))
                (#\s (encode-opcode-modifier :no-ssuf))
                (#\l (encode-opcode-modifier :no-lsuf))
                (#\q (encode-opcode-modifier :no-qsuf))
                (#\x (encode-opcode-modifier :no-xsuf))
                (t 0))))
        (when (= nops
                 (the fixnum (x86-instruction-template-operands template)))
          (unless (logtest
                   (x86-instruction-template-opcode-modifier template)
                   suffix-check)
            (or (zerop nops)
                (let* ((template-types
                      (x86-instruction-template-operand-types template))
                     (template-type0 (svref template-types 0))
                     (overlap0
                      (logand type0 template-type0))
                     (match0 (match overlap0 type0)))
                  (if match0
                    (or (= nops 1)
                        ;; 2 or 3 operands.
                        (let* ((template-type1 (svref template-types 1))
                               (overlap1 (logand type1 template-type1))
                               (match1 (match overlap1 type1)))
                          (if (and
                               match1
                               (consistent-register-match
                                overlap0
                                type0
                                template-type0
                                overlap1
                                type1
                                template-type1))
                            (or (= nops 2)
                                ;; 3 operands
                                (let* ((template-type2 (svref template-types 2))
                                       (overlap2 (logand type2 template-type2)))
                                  (and (match overlap2 type2)
                                           (consistent-register-match
                                            overlap1
                                      type1
                                      template-type1
                                      overlap2
                                      type2
                                      template-type2))))))))))))))))

(defun add-prefix (instruction prefix)
  (declare (fixnum prefix))
  (let* ((ret 1)
         (prefixes (or (x86-lap-instruction-prefixes instruction)
                       (setf (x86-lap-instruction-prefixes instruction)
                             (make-array max-prefixes
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0))))
         (q 
          (if (and (>= prefix rex-opcode)
                   (< prefix (+ rex-opcode 16)))
            rex-prefix
            (ecase prefix
              ((#.cs-prefix-opcode #.ds-prefix-opcode #.es-prefix-opcode
                                   #.fs-prefix-opcode #.gs-prefix-opcode
                                   #.ss-prefix-opcode)
               seg-prefix)
              ((#.repne-prefix-opcode #.repe-prefix-opcode)
               (setq ret 2)
               lockrep-prefix)
              (#.lock-prefix-opcode lockrep-prefix)
              (#.fwait-opcode wait-prefix)
              (#.addr-prefix-opcode addr-prefix)
              (#.data-prefix-opcode data-prefix)))))
    (declare (type (simple-array (unsiged-byte 8) (*)) prefixes)
             (fixnum q))
    (unless (zerop (aref prefixes q))
      (error "Duplicate instruction prefix"))
    (incf (x86-lap-instruction-nprefixes instruction))
    (setf (aref prefixes q) prefix)
    ret))

(defun bad-register-operand-for-suffix (instruction i)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (op (svref parsed-operands i))
         (template (x86-lap-instruction-template instruction)))
    (error "register %~a not allowed with ~a~c"
           (reg-entry-reg-name
            (x86-register-operand-entry op))
           (x86-instruction-template-name template)
           (x86-lap-instruction-suffix instruction))))
    
;;; Not sure whether inoutportreg needs special handling, but it's
;;; not clear that we'd be doing I/O in LAP, anyway.
(defun check-byte-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86-operand-type op)))
        (unless (logtest optype (encode-operand-type :reg8))
          (when (logtest optype
                         (encode-operand-type :reg :regmmx :regxmm :sreg2 :sreg3 :control :debug :test :floatreg :floatacc))
            (bad-register-operand-for-suffix instruction i)))))))

(defun check-long-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (template (x86-lap-instruction-template instruction))
         (template-types (x86-instruction-template-operand-types template)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86-operand-type op))
             (template-type (svref template-types i)))
        ;; Reject 8-bit registers, except where the template requres
        ;; them (e.g., movzb)
        (if (or (and (logtest optype (encode-operand-type :reg8))
                     (logtest template-type (encode-operand-type :reg16 :reg32 :acc)))
                (and (logtest optype (encode-operand-type :reg16))
                     (logtest template-type (encode-operand-type :reg32 :acc)))
                (and (logtest optype (encode-operand-type :reg64))
                     (logtest template-type (encode-operand-type :reg32 :acc))))
          (bad-register-operand-for-suffix instruction i))))))

(defun check-qword-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (template (x86-lap-instruction-template instruction))
         (template-types (x86-instruction-template-operand-types template)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86-operand-type op))
             (template-type (svref template-types i)))
        ;; Reject 8-bit registers, except where the template requres
        ;; them (e.g., movzb)
        (if (or (and (logtest optype (encode-operand-type :reg8))
                     (logtest template-type (encode-operand-type :reg16 :reg32 :acc)))
                (and (or
                      (logtest optype (encode-operand-type :reg16))
                      (logtest optype (encode-operand-type :reg32)))
                     (logtest template-type (encode-operand-type :reg32 :acc))))
          (bad-register-operand-for-suffix instruction i))))))

(defun check-word-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (template (x86-lap-instruction-template instruction))
         (template-types (x86-instruction-template-operand-types template)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86-operand-type op))
             (template-type (svref template-types i)))
        ;; Reject 8-bit registers, except where the template requres
        ;; them (e.g., movzb)
        (if (or (and (logtest optype (encode-operand-type :reg8))
                     (logtest template-type (encode-operand-type :reg16 :reg32 :acc)))
                (and (logtest optype (encode-operand-type :reg32))
                     (logtest template-type (encode-operand-type :reg16 :acc))))
          (bad-register-operand-for-suffix instruction i))))))
  
(defun check-suffix (instruction form)
  (let* ((template (x86-lap-instruction-template instruction))
         (parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (suffix (x86-lap-instruction-suffix instruction))
         (template-modifier (x86-instruction-template-opcode-modifier template))
         (has-register-operands (dotimes (i (length parsed-operands))
                                  (if (logtest (encode-operand-type :reg)
                                               (x86-operand-type
                                                (svref parsed-operands i)))
                                    (return t)))))
    (cond ((logtest template-modifier
                    (encode-opcode-modifier :size16 :size32 :size64))
           (cond ((logtest template-modifier (encode-opcode-modifier :size16))
                  (setq suffix word-mnem-suffix))
                 ((logtest template-modifier (encode-opcode-modifier :size32))
                  (setq suffix long-mnem-suffix))
                 ((logtest template-modifier (encode-opcode-modifier :size64))
                  (setq suffix qword-mnem-suffix))))
          (has-register-operands
           ;; If there's no explicit suffix, try to derive it from the
           ;; type of the last (destination) register operand.
           (cond ((null suffix)
                  (do* ((i (1- (length parsed-operands)) (1- i)))
                       ((< i 0))
                    (declare (fixnum i))
                    (let* ((type-i (x86-operand-type (svref parsed-operands i)))
                           (tm-type-i (svref (x86-instruction-template-operand-types template) i)))
                      (when (and (logtest type-i (encode-operand-type :reg))
                               (not (logtest tm-type-i
                                             (encode-operand-type :inoutportreg))))
                        (setq
                         suffix
                         (cond ((logtest type-i (encode-operand-type :reg8))
                                byte-mnem-suffix)
                               ((logtest type-i (encode-operand-type :reg16))
                                word-mnem-suffix)
                               ((logtest type-i (encode-operand-type :reg64))
                                qword-mnem-suffix)
                               (t long-mnem-suffix)))
                        (return)))))
                 ((eql suffix byte-mnem-suffix)
                  (check-byte-reg instruction))
                 ((eql suffix long-mnem-suffix)
                  (check-long-reg instruction))
                 ((eql suffix qword-mnem-suffix)
                  (check-qword-reg instruction))
                 ((eql suffix word-mnem-suffix)
                  (check-word-reg instruction)))))
    (unless (setf (x86-lap-instruction-suffix instruction) suffix)
      (if (logtest template-modifier (encode-opcode-modifier :w))
        (error "No mnemonic suffix specified and can't determine instruction size from operands in ~s" form)))
    ;; Change the opcode based on the operand size given by suffix. We
    ;; don't need to change things for byte insns.
    (when (and suffix (not (eql suffix byte-mnem-suffix)))
      (when (logtest template-modifier (encode-opcode-modifier :w))
        (setf (x86-lap-instruction-base-opcode instruction)
              (logior (x86-lap-instruction-base-opcode instruction)
                      (if (logtest template-modifier (encode-opcode-modifier :shortform))
                        8
                        1))))
    
      ;; Now select between word & dword operations via the operand
      ;; size prefix, except for instructions that will ignore this
      ;; prefix anyway. 
      (if (and (not (eql suffix qword-mnem-suffix))
               (not (eql suffix long-double-mnem-suffix))
               (not (logtest template-modifier
                             (encode-opcode-modifier :ignoresize :floatmf)))
               (or (not (eql suffix long-mnem-suffix))
                   (logtest template-modifier (encode-opcode-modifier :jumpbyte))))
        (add-prefix instruction
                    (if (logtest template-modifier
                                 (encode-opcode-modifier :jumpbyte))
                      data-prefix-opcode
                      addr-prefix-opcode)))
      ;; Set mode64 if appropriate.
      (if (and (eql suffix qword-mnem-suffix)
               (not (logtest template-modifier (encode-opcode-modifier :norex64))))
        (setf (x86-lap-instruction-rex instruction)
              (logior (or (x86-lap-instruction-rex instruction) 0)
                      rex-mode64)))
      ;; Size floating-point instruction
      (if (and (eql suffix long-mnem-suffix)
               (logtest template-modifier (encode-opcode-modifier :floatmf)))
        (setf (x86-lap-instruction-base-opcode instruction)
              (logxor (x86-lap-instruction-base-opcode instruction) 4))))
    instruction))

(defun init-x86-lap-instruction (instruction template parsed-operands suffix)
  (setf (x86-lap-instruction-template instruction) template
        (x86-lap-instruction-parsed-operands instruction) parsed-operands
        (x86-lap-instruction-base-opcode instruction) (x86-instruction-template-base-opcode template)
        (x86-lap-instruction-extension-opcode instruction) (x86-instruction-template-extension-opcode template)
        (x86-lap-instruction-suffix instruction) suffix)
  instruction)

;;; FORM is a list; its car doesn't name a macro or pseudo op.
;;; If we can find a matching instruction template, create a
;;; (skeletal) x86-lap-instruction with that template and these
;;; operands.
(defun parse-x86-instruction (form)
  (let* ((instruction (make-x86-lap-instruction)))
    (labels
        ((parse-x86-instruction-aux (subform expecting-string)
           (if (null subform)
             (error "No non-prefix X86 instruction in ~s" form)
             (destructuring-bind (mnemonic &rest operands) subform
               (multiple-value-bind (templates suffix)
                   (get-x86-instruction-templates-and-suffix mnemonic)
                 (let* ((t0 (car templates))
                        (t0-modifier (x86-instruction-template-opcode-modifier t0)))
                   (if (logtest t0-modifier
                                (encode-opcode-modifier :isprefix))
                     (let* ((prefix (x86-instruction-template-base-opcode t0)))
                       (add-prefix instruction prefix)
                       (parse-x86-instruction-aux
                        operands
                        (or expecting-string
                            (if (or (eql prefix repe-prefix-opcode)
                                    (eql prefix repne-prefix-opcode))
                              (x86-instruction-template-name t0)))))
                                                    
                     (let* ((parsed-operands (if operands
                                               (map
                                                'vector
                                                #'(lambda (f)
                                                    (parse-x86-operand
                                                     instruction f))
                                                operands))))
                       (when (and expecting-string
                                  (not (logtest
                                        (x86-instruction-template-opcode-modifier
                                         (car templates))
                                        (encode-opcode-modifier :isstring))))
                         (error "Expecting string instruction after ~s in ~s"
                                expecting-string form))
                       (dolist (template templates (error "Operands or suffix invalid in ~s" form))
                         (when (match-template template parsed-operands suffix)
                           (init-x86-lap-instruction instruction template parsed-operands suffix)
                           (check-suffix instruction form)
                           (return instruction)))))))))))
      (parse-x86-instruction-aux form nil))))

#|
(defun build-rm-byte (insn)
  (let* ((rm (make-modrm-byte))
         (operands (x86-lap-instruction-parsed-operands insn))
         (num-reg-operands (count-if #'x86-register-operand-p operands)))
    (setf (x86-lap-instruction-rm insn) rm)
    (if (= num-reg-operands 2)
      (let* ((op0 (svref operands 0))
             (type0 (x86-operand-type op0))
             (src (if (logtest type0 (encode-operand-type :reg :regmmx :regxmm
                                                         :sreg2 :sreg3
                                                         :control :debug :test))
                    0
                    1))
             (srcreg (x86-register-operand-entry (svref operands src)))
             (dest (1+ src))
             (destreg (x86-register-operand-entry (svref operands dest))))
        (setf (modrm-byte-mode rm) 3)
        
        ;; One of the register operands will be encoded in the rm-reg
        ;; field, the other in the combined rm-mode and rm-regmem
        ;; fields.  If no form of this instruction supports a memory
        ;; destination operand, then we assume the source operand may
        ;; sometimes be a memory operand and so we need to store the
        ;; destination in the rm-reg field.
        (if (not (logtest (svref (x86-instruction-template-operand-types template) dest)
                          (encode-operand-type :AnyMem)))
          (progn
            (setf (modrm-byte-reg rm) (reg-entry-reg-num destreg)
                  (modrm-byte-regmem rm) (reg-entry-reg-num srcreg))
            (when (logtest RegRex (reg-entry-reg-flags destreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior rex-extx
                            (or (x86-lap-instruction-rex insn) 0))))
            (when (logtest RegRex (reg-entry-reg-flags srcreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior rex-extz
                            (or (x86-lap-instruction-rex insn) 0)))))
          (progn
            (setf (modrm-byte-reg rm) (reg-entry-reg-num srcreg)
                  (modrm-byte-regmem rm) (reg-entry-reg-num destreg))
            (when (logtest RegRex (reg-entry-reg-flags destreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior rex-extz
                            (or (x86-lap-instruction-rex insn) 0))))
            (when (logtest RegRex (reg-entry-reg-flags srcreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior rex-extx
                            (or (x86-lap-instruction-rex insn) 0)))))))
      ;; Not exactly 2 register operands.
      (let* ((memop (dotimes (i (length operands))
                      (let* ((op (svref operands i)))
                        (when (logtest (encode-operand-type :AnyMem)
                                       (x86-operand-type op))
                          (return op))))))
        (if memop
          (let* ((fake-zero-displacement nil)
                 (sib (make-sib-byte)))
            (setf (x86-lap-instruction-sib insn) sib)
            (cond ((null (x86-memory-operand-base memop))
                   (setf (modrm-byte-mode rm) 0)
                   (unless (x86-memory-operand-disp memop)
                     (setq fake-zero-displacement t))
                   (if (null (x86-memory-operand-index memop))
                     (progn
                       (setf (modrm-byte-regmem rm) escape-to-two-byte-addressing
                             (sib-byte-base sib) no-base-register
                             (sib-byte-index sib) no-index)
                       (let* ((prefixes (x86-lap-instruction-prefixes insn)))
                           (setf (x86-memory-operand-type memop)
                                 (if (and prefixes
                                          (not (eql 0 (aref prefixes addr-prefix))))
                                   (encode-operand-type :disp32)
                                   (encode-operand-type :disp32s)))))
                     ;; Index, but no base reg.
                     (let* ((index-reg (x86-register-operand-entry
                                        (x86-memory-operand-index memop))))
                       (setf (sib-byte-index sib) (reg-entry-reg-num index-reg)
                             (sib-byte-base sib) no-base-register
                             (sib-byte-scale sib) (or (x86-memory-operand-scale memop) 1)
                             (modrm-byte-regmem rm) escape-to-two-byte-addressing
                             (x86-memory-operand-type memop) (logior (encode-operand-type :disp32s) (logandc2 (x86-memory-operand-type memop) (encode-operand-type :disp))))
                       (if (logtest (reg-entry-reg-flags index-reg) RegRex)
                         (setf (x86-lap-instruction-rex insn)
                               (logior (or (x86-lap-instruction-rex insn) 0)
                                       rex-exty))))))
                  ;; 64-bit RIP-relative addressing, detected by the
                  ;; fact that the base register has the :baseindex
                  ;; operand type only
                  ((= (x86-register-operand-type
                       (x86-memory-operand-base memop))
                      (encode-operand-type :baseIndex))
                       
            
                              
            
            
                  
     
    
)

|#
                  
(defun x86-generate-instruction-code (frag insn)
  (let* ((template (x86-lap-instruction-template insn))
         (operands (x86-lap-instruction-parsed-operands insn)))
    (when (logtest (encode-opcode-modifier :modrm)
                   (x86-instruction-template-opcode-modifier template))
      (build-rm-byte insn))
    (when (and (= (x86-lap-instruction-base-opcode insn) int-opcode)
                  (= 1 (length operands))
                  (let* ((op0 (svref operands 0)))
                    (and (typep op0 'x86-immediate-operand)
                         (eql 3 (early-expression-value
                                 (x86-immediate-operand-value op0))))))
      (setf operands nil
            (x86-lap-instruction-parsed-operands insn) nil
            (x86-lap-instruction-base-opcode insn) int3-opcode))
    (if (logtest (x86-instruction-template-opcode-modifier template)
                 (encode-opcode-modifier :rex64))
      (setf (x86-lap-instruction-rex insn)
            (logior (or (x86-lap-instruction-rex insn) 0)
                    rex-mode64)))
    (if (x86-lap-instruction-rex insn)
      (add-prefix insn (logior (x86-lap-instruction-rex insn) rex-prefix)))
    (code-fragment-finish frag insn)))

(defun x86-lap-form (form frag addr)
  (if (atom form)
    (let* ((lab (emit-x86-lap-label form)))
      (setf (x86-lap-label-address lab) addr))
    (if (eq (car form) 'progn)
      (dolist (f (cdr form))
        (setq addr (x86-lap-form f frag addr)))
      (let* ((instruction (parse-x86-instruction form)))
        (ccl::append-dll-node instruction *x86-lap-instructions*)
        (setf (x86-lap-instruction-address instruction) addr)
        (incf addr (x86-generate-instruction-code frag instruction))))))

;;; For testing.
(defun x86-lap (forms)
  (let* ((*x86-lap-instructions* (ccl::make-dll-header))
         (*x86-lap-labels* ())
         (frag (make-code-fragment))
         (addr 0))
    (dolist (f forms)
      (setq addr (x86-lap-form f frag addr)))
    (ccl::do-dll-nodes (i *x86-lap-instructions*)
      (format t "~&~s" i))
    *x86-lap-instructions*))

