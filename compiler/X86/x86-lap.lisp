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

(in-package "CCL")

(require "X86-ASM" "ccl:compiler;X86;x86-asm")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "DLL-NODE"))


(eval-when (:execute :load-toplevel)

  (defstruct (x86-lap-note (:include ccl::dll-node))
    peer
    id)

  (defstruct (x86-lap-note-begin (:include x86-lap-note)))
  (defstruct (x86-lap-note-end (:include x86-lap-note)))
    
  (defstruct (x86-lap-label (:constructor %%make-x86-lap-label (name)))
    name
    frag
    offset
    ))


;;; Intentionally very similar to RISC-LAP, but with some extensions
;;; to deal with alignment and with variable-length and/or span-
;;; dependent instructions.

(defvar *x86-lap-labels* ())
(defvar *x86-lap-constants* ())
(defparameter *x86-lap-entry-offset* 15)
(defvar *x86-lap-macros* (make-hash-table :test #'equalp))


(defun x86-lap-macro-function (name)
  (gethash (string name) #|(backend-lap-macros *ppc-backend*)|#
           *x86-lap-macros*))

(defun (setf x86-lap-macro-function) (def name)
  (let* ((s (string name)))
    (when (gethash s x86::*x86-opcode-template-lists*)
      (error "~s already defines an x86 instruction . " name))
    (setf (gethash s *x86-lap-macros* #|(backend-lap-macros *x86-backend*)|#) def)))

(defmacro defx86lapmacro (name arglist &body body)
  `(progn
     (setf (x86-lap-macro-function ',name)
           (nfunction (x86-lap-macro ,name) ,(ccl::parse-macro name arglist body)))
     (record-source-file ',name 'x86-lap)
     ',name))

(defun x86-lap-macroexpand-1 (form)
  (unless (and (consp form) (atom (car form)))
    (values form nil))
  (let* ((expander (x86-lap-macro-function (car form))))
    (if expander
      (values (funcall expander form nil) t)
      (values form nil))))


(defmethod print-object ((l x86-lap-label) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~a" (x86-lap-label-name l))))

;;; Labels

(defun make-x86-lap-label (name)
  (let* ((lab (%%make-x86-lap-label name)))
    (if (typep *x86-lap-labels* 'hash-table)
      (setf (gethash name *x86-lap-labels*) lab)
      (progn
        (push lab *x86-lap-labels*)
        (if (> (length *x86-lap-labels*) 255)
          (let* ((hash (make-hash-table :size 512 :test #'eq)))
            (dolist (l *x86-lap-labels* (setq *x86-lap-labels* hash))
              (setf (gethash (x86-lap-label-name l) hash) l))))))
    lab))

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
  (not (null (x86-lap-label-frag lab))))

(defun emit-x86-lap-label (frag-list name)
  (let* ((lab (find-or-create-x86-lap-label name)))
    (when (x86-lap-label-emitted-p lab)
      (error "Label ~s: multiply defined." name))
    (setf (x86-lap-label-frag lab) (frag-list-current frag-list)
          (x86-lap-label-offset lab) (frag-list-position frag-list))
    lab))





(defstruct reloc
  type                                  ; a keyword
  arg                                   ; a label-operand or an expression, etc.
  frag                                  ; the (redundant) containing frag
  pos                                   ; octet position withing frag
  )

(defstruct (frag (:include ccl::dll-node))
  address
  last-address                          ; address may change during relax
  type                                  ; nil, or (:TYPE &rest args)
  relocs                                ; relocations against this frag
  relax-marker                          ; boolean, flipped during relax
  code-buffer                           ; may be nil, or adjustable vector
  )

(defstruct (frag-list (:include ccl::dll-header)))

;;; ccl::dll-header-last is unit-time
(defun frag-list-current (frag-list)
  (ccl::dll-header-last frag-list))

;;; Add a new (empty) frag to the end of FRAG-LIST and make the new frag
;;; current
(defun new-frag (frag-list)
  (ccl::append-dll-node (make-frag) frag-list))

;;; Make a frag list, and make an empty frag be its current frag.
(defun make-frag-list ()
  (let* ((header (ccl::make-dll-header)))         
    (new-frag header)
    header))

(defun frag-list-current-frag-size (frag-list)
  (length (frag-code-buffer (frag-list-current frag-list))))


;;; Push 1, 2, 4, or 8 bytes onto the frag-list's current-frag's buffer.
;;; (If pushing more than one byte, do so in little-endian order.)
(defun frag-list-push-byte (frag-list b)
  (let* ((frag (frag-list-current frag-list))
         (buf (or (frag-code-buffer frag)
                  (setf (frag-code-buffer frag)
                        (make-array 32 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0
                                    :adjustable t)))))
    (vector-push-extend b buf)
    b))

(defun frag-list-push-16 (frag-list w)
  (let* ((frag (frag-list-current frag-list))
         (buf (or (frag-code-buffer frag)
                  (setf (frag-code-buffer frag)
                        (make-array 32 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0
                                    :adjustable t)))))
    (vector-push-extend (ldb (byte 8 0) w) buf)
    (vector-push-extend (ldb (byte 8 8) w) buf)
    w))

(defun frag-list-push-32 (frag-list w)
  (let* ((frag (frag-list-current frag-list))
         (buf (or (frag-code-buffer frag)
                  (setf (frag-code-buffer frag)
                        (make-array 32 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0
                                    :adjustable t)))))
    (vector-push-extend (ldb (byte 8 0) w) buf)
    (vector-push-extend (ldb (byte 8 8) w) buf)
    (vector-push-extend (ldb (byte 8 16) w) buf)
    (vector-push-extend (ldb (byte 8 24) w) buf)
    w))

(defun frag-list-push-64 (frag-list w)
  (let* ((frag (frag-list-current frag-list))
         (buf (or (frag-code-buffer frag)
                  (setf (frag-code-buffer frag)
                        (make-array 32 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0
                                    :adjustable t)))))
    (vector-push-extend (ldb (byte 8 0) w) buf)
    (vector-push-extend (ldb (byte 8 8) w) buf)
    (vector-push-extend (ldb (byte 8 16) w) buf)
    (vector-push-extend (ldb (byte 8 24) w) buf)
    (vector-push-extend (ldb (byte 8 32) w) buf)
    (vector-push-extend (ldb (byte 8 40) w) buf)
    (vector-push-extend (ldb (byte 8 48) w) buf)
    (vector-push-extend (ldb (byte 8 56) w) buf)
    w))

;;; Returns the length of the current frag
(defun frag-list-position (frag-list)
  (length (frag-code-buffer (frag-list-current frag-list))))


;;; Finish the current frag, marking it as containing a PC-relative
;;; branch to the indicated label, with a one-byte opcode and
;;; one byte of displacement.
(defun finish-frag-for-branch (frag-list opcode label-ref)
  (let* ((frag (frag-list-current frag-list)))
    (frag-list-push-byte frag-list opcode)
    (let* ((pos (frag-list-position frag-list))
           (reloc (make-reloc :type :branch8
                              :arg label-ref
                              :pos pos)))
      (push reloc (frag-relocs frag))
      (frag-list-push-byte frag-list 0)
      (setf (frag-type frag) (list (if (eql opcode #xeb)
                                     :assumed-short-branch
                                     :assumed-short-conditional-branch)
                                   label-ref
                                   pos
                                   reloc))
      (new-frag frag-list))))

;;; Mark the current frag as -ending- with an align directive.
;;; p2align is the power of 2 at which code in the next frag
;;; should be aligned.
;;; Start a new frag.
(defun finish-frag-for-align (frag-list p2align)
  (let* ((frag (frag-list-current frag-list)))
    (setf (frag-type frag) (list :align p2align))
    (new-frag frag-list)))

(defun lookup-x86-register (regname designator)
  (let* ((r (typecase regname
              (symbol (or (gethash (string regname) x86::*x86-registers*)
                          (and (boundp regname)
                               (let* ((val (symbol-value regname)))
                                 (and (typep val 'fixnum)
                                      (>= val 0)
                                      (< val (length x86::*x8664-register-entries*))
                                      (svref x86::*x8664-register-entries* val))))))
              (string (gethash regname x86::*x86-registers*))
              (fixnum (if (and (typep regname 'fixnum)
                                      (>= regname 0)
                                      (< regname (length x86::*x8664-register-entries*)))
                        (svref x86::*x8664-register-entries* regname))))))
                               
    (when r
      (if (eq designator :%)
        r
        (let* ((regtype (x86::reg-entry-reg-type r)))
          (unless (logtest regtype (x86::encode-operand-type :reg8 :reg16 :reg32 :reg64))
            (error "Designator ~a can't be used with register ~a"
                   designator (x86::reg-entry-reg-name r)))
          (case designator
            (:%b (x86::x86-reg8 r))
            (:%w (x86::x86-reg16 r))
            (:%l (x86::x86-reg32 r))
            (:%q (x86::x86-reg64 r))))))))

(defun x86-register-ordinal-or-expression (form)
  (let* ((r (if (typep form 'symbol)
              (lookup-x86-register form :%))))
    (if r
      (x86::reg-entry-ordinal64 r)
      (multiple-value-bind (val condition)
          (ignore-errors (eval form))
        (if condition
          (error "Condition ~a signaled during assembly-time evalation of ~s."
                 condition form)
          val)))))


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

(defstruct x86-lap-expression
  )


(defstruct (label-x86-lap-expression (:include x86-lap-expression))
  label)


;;; Represents a constant
(defstruct (constant-x86-lap-expression (:include x86-lap-expression))
  value)

(setq x86::*lap-constant-0-expression*
      (make-constant-x86-lap-expression :value 0))

;;; Also support 0, 1, 2, and many args, where at least one of those args
;;; is or contains a label reference.
(defstruct (application-x86-lap-expression (:include x86-lap-expression))
  operator)


(defstruct (unary-x86-lap-expression (:include application-x86-lap-expression))
  operand)


(defstruct (binary-x86-lap-expression (:include application-x86-lap-expression))
  operand0
  operand1)

(defstruct (n-ary-x86-lap-expression (:include application-x86-lap-expression))
  operands)

;;; Looks like a job for DEFMETHOD.
(defun x86-lap-expression-value (exp)
  (etypecase exp
    (constant-x86-lap-expression (constant-x86-lap-expression-value exp))
    (label-x86-lap-expression (- (x86-lap-label-address (label-x86-lap-expression-label exp)) *x86-lap-entry-offset*))
    (unary-x86-lap-expression (funcall (unary-x86-lap-expression-operator exp)
                                       (x86-lap-expression-value (unary-x86-lap-expression-operand exp))))
    (binary-x86-lap-expression (funcall (binary-x86-lap-expression-operator exp) 
                                        (x86-lap-expression-value (binary-x86-lap-expression-operand0 exp))
                                        (x86-lap-expression-value (binary-x86-lap-expression-operand1 exp))))
    (n-ary-x86-lap-expression (apply (n-ary-x86-lap-expression-operator exp)
                                     (mapcar #'x86-lap-expression-value (n-ary-x86-lap-expression-operands exp))))))

;;; Expression might contain unresolved labels.  Return nil if so (even
;;; if everything -could- be resolved.)
(defun early-x86-lap-expression-value (expression)
  (etypecase expression
    (constant-x86-lap-expression (constant-x86-lap-expression-value expression))
    (x86-lap-expression nil)))

(defun x86-lap-label-address (lab)
  (let* ((frag (or (x86-lap-label-frag lab)
                   (error "Label ~s was referenced but not defined"
                          (x86-lap-label-name lab)))))
    (+ (frag-address frag)
       (x86-lap-label-offset lab))))

(defun parse-x86-lap-expression (form)
  (when (quoted-form-p form)
    (let* ((val (cadr form)))
      (if (typep val 'fixnum)
        (setq form (ash val 3 #|x8664::fixnumshift|#))
        (let* ((constant-label (or (cdr (assoc val *x86-lap-constants*
                                               :test #'eq))
                                   (let* ((label (make-x86-lap-label
                                                  (gensym)))
                                          (pair (cons val label)))
                                     (push pair *x86-lap-constants*)
                                     label))))
          (setq form `(^ ,(x86-lap-label-name constant-label)))))))
  (if (null form)
    (setq form (arch::target-nil-value (backend-target-arch *target-backend*)))
      (if (eq form t)
        (setq form
              (+ (arch::target-nil-value (backend-target-arch *target-backend*))
                 (arch::target-t-offset  (backend-target-arch *target-backend*))))))

  (if (label-address-expression-p form)
    (make-label-x86-lap-expression :label (find-or-create-x86-lap-label (cadr form)))
    (if (contains-label-address-expression form)
      (destructuring-bind (op &rest args) form
        (case (length args)
          (1 (make-unary-x86-lap-expression :operator op :operand (parse-x86-lap-expression (car args))))
          (2 (make-binary-x86-lap-expression :operator op :operand0 (parse-x86-lap-expression (car args))
                                     :operand1 (parse-x86-lap-expression (cadr args))))
          (t (make-n-ary-x86-lap-expression :operator op :operands (mapcar #'parse-x86-lap-expression args)))))
      (multiple-value-bind (value condition)
          (eval form)
        (if condition
          (error "~a signaled during assembly-time evaluation of form ~s" condition form)
          (make-constant-x86-lap-expression :value value))))))

(defun parse-x86-register-operand (regname designator)
  (let* ((r (lookup-x86-register regname designator)))
    (if r
      (x86::make-x86-register-operand :type (logandc2 (x86::reg-entry-reg-type r)
                                                      (x86::encode-operand-type :baseIndex))
                                 :entry r)
      (error "Unknown X86 register ~s" regname))))

(defun parse-x86-label-reference (name)
  (let* ((lab (find-or-create-x86-lap-label name)))
    (x86::make-x86-label-operand :type (x86::encode-operand-type :label)
                                 :label lab)))



(defun x86-register-designator (form)
  (when (and (consp form)
             (symbolp (car form)))
    (let* ((sym (car form)))
      (cond ((string= sym '%) :%)
            ((string= sym '%b) :%b)
            ((string= sym '%w) :%w)
            ((string= sym '%l) :%l)
            ((string= sym '%q) :%q)))))


;;; Syntax is:
;;; ([seg] [disp] [base] [index] [scale])
;;; A [seg] by itself isn't too meaningful; the same is true
;;; of a few other combinations.
(defun parse-x86-memory-operand (form)
  (flet ((register-operand-p (form)
           (let* ((designator (x86-register-designator form)))
             (when designator
               (destructuring-bind (regname) (cdr form)
                 (or (lookup-x86-register regname designator)
                     (error "Unknown register ~s" regname)))))))
  (let* ((seg nil)
         (disp nil)
         (base nil)
         (index nil)
         (scale nil))
    (do* ((f form (cdr f)))
         ((null f)
          (if (or disp base index)
            (progn
              ;;(check-base-and-index-regs instruction base index)
              (x86::make-x86-memory-operand 
               :type (if (or base index)
                       (if disp
                         (logior (optimize-displacement-type disp)
                                 (x86::encode-operand-type  :baseindex))
                         (x86::encode-operand-type :baseindex))
                       (optimize-displacement-type disp))
               :seg seg
               :disp disp
               :base base
               :index index
               :scale scale))
            (error "No displacement, base,  or index in ~s" form)))
      (let* ((head (car f))
             (r (register-operand-p head)))
        (if r
          (if (logtest (x86::reg-entry-reg-type r)
                       (x86::encode-operand-type :sreg2 :sreg3))
            ;; A segment register - if present - must be first
            (if (eq f form)
              (setq seg (svref x86::*x86-seg-entries* (x86::reg-entry-reg-num r))) 
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
            (let* ((exp (parse-x86-lap-expression head))
                   (val (if (typep exp 'constant-x86-lap-expression)
                          (x86-lap-expression-value exp))))
              (case val
                ((1 2 4 8)
                 (if (and base (not index))
                   (setq index base base nil))
                 (setq scale (1- (integer-length val))))
                (t
                 (error "Invalid scale factor ~s in ~s" head form))))
            (if (not (or disp base index))
              (setq disp (parse-x86-lap-expression head))
              (error "~& not expected in ~s" head form)))))))))

     
    

;;; Operand syntax:
;;; (% x) -> register
;;; ($ x) -> immediate
;;; (* x) -> x with :jumpabsolute attribute
;;; (@ x) -> memory operand
;;; x -> labelref
(defun parse-x86-operand (form)
  (if (consp form)
    (let* ((head (car form))
           (designator nil))
      (if (symbolp head)
        (cond ((string= head '$)
               (destructuring-bind (immval) (cdr form)
                 (let* ((expr (parse-x86-lap-expression immval))
                        (val (early-x86-lap-expression-value expr))
                        (type (if val
                                (smallest-imm-type val)
                                (x86::encode-operand-type :imm32s))))
                   (x86::make-x86-immediate-operand :type type
                                             :value expr))))
              ((setq designator (x86-register-designator form))
               (destructuring-bind (reg) (cdr form)
                 (parse-x86-register-operand reg designator)))
              ((string= head '*)
               (destructuring-bind (subop) (cdr form)
                 (let* ((op (parse-x86-operand subop)))
                   (setf (x86::x86-operand-type op)
                         (logior (x86::encode-operand-type :jumpabsolute)
                                 (x86::x86-operand-type op)))
                   op)))
              ((string= head '@)
               (parse-x86-memory-operand  (cdr form)))
              (t (error "unknown X86 operand: ~s" form)))
        (error "unknown X86 operand: ~s" form)))
    ;; Treat an atom as a label.
    (parse-x86-label-reference form)))





(defun init-x86-instruction (instruction template parsed-operands)
  (setf (x86::x86-instruction-opcode-template instruction) template
        (x86::x86-instruction-base-opcode instruction) (x86::x86-opcode-template-base-opcode template)
        (x86::x86-instruction-modrm-byte instruction) (x86::x86-opcode-template-modrm-byte template)
        (x86::x86-instruction-rex-prefix instruction) (x86::x86-opcode-template-rex-prefix template))
  (let* ((insert-classes (x86::x86-opcode-template-operand-classes template))
         (insert-functions x86::*x86-operand-insert-functions*))
    (dotimes (i (length parsed-operands) instruction)
      (funcall (svref insert-functions (svref insert-classes i))
               instruction
               (pop parsed-operands)))))

(defun reset-x86-instruction (i)
  (setf (x86::x86-instruction-opcode-template i) nil
        (x86::x86-instruction-base-opcode i) nil
        (x86::x86-instruction-rex-prefix i) nil
        (x86::x86-instruction-modrm-byte i) nil
        (x86::x86-instruction-sib-byte i) nil
        (x86::x86-instruction-seg-prefix i) nil
        (x86::x86-instruction-disp i) nil
        (x86::x86-instruction-imm i) nil
        (x86::x86-instruction-extra i) nil)
  i)

(defun smallest-imm-type (val)
  (if (eql val 1)
    (x86::encode-operand-type :Imm1 :Imm8 :Imm8S :Imm16 :Imm32 :Imm32S :Imm64)
    (typecase val
      ((signed-byte 8)
       (x86::encode-operand-type :Imm8S :imm8 :Imm16 :Imm32 :Imm32S :Imm64))
      ((unsigned-byte 8)
       (x86::encode-operand-type  :imm8 :Imm16 :Imm32 :Imm32S :Imm64))
      ((signed-byte 16)
       (x86::encode-operand-type  :Imm16 :Imm32 :Imm32S :Imm64))
      ((unsigned-byte 16)
       (x86::encode-operand-type  :Imm16 :Imm32 :Imm32S :Imm64))
      ((signed-byte 32)
       (x86::encode-operand-type :Imm32 :Imm32S :Imm64))
      ((unsigned-byte 32)
       (x86::encode-operand-type :Imm32 :Imm64))
      (t (x86::encode-operand-type :Imm64)))))

    
(defun x86-optimize-imm (operands suffix)
  (unless suffix
    ;; See if we can determine an implied suffix from operands.
    (do* ((i (1- (length operands)) (1- i)))
         ((< i 0))
      (declare (fixnum i))
      (let* ((op (svref operands i))
             (optype (x86::x86-operand-type op)))
        (when (logtest optype (x86::encode-operand-type :reg))
          (cond ((logtest optype (x86::encode-operand-type :reg8))
                 (setq suffix #\b))
                ((logtest optype (x86::encode-operand-type :reg16))
                 (setq suffix #\w))
                ((logtest optype (x86::encode-operand-type :reg32))
                 (setq suffix #\l))
                ((logtest optype (x86::encode-operand-type :reg64))
                 (setq suffix #\q)))
          (return)))))
  (dotimes (i (length operands))
    (let* ((op (svref operands i))
           (optype (x86::x86-operand-type op)))
      (when (logtest optype (x86::encode-operand-type :imm))
        (let* ((val (x86::x86-immediate-operand-value op)))
          (cond ((typep val 'constant-x86-lap-expression)
                 (case suffix
                   (#\l (setf (x86::x86-operand-type op)
                              (logior optype (x86::encode-operand-type
                                              :imm32 :imm64))))
                   (#\w (setf (x86::x86-operand-type op)
                              (logior optype (x86::encode-operand-type
                                              :imm16 :imm32S  :imm32 :imm64))))
                   (#\b (setf (x86::x86-operand-type op)
                              (logior optype (x86::encode-operand-type
                                              :imm8 :imm16 :imm32S  :imm32 :imm64)))))
                 (setf (x86::x86-operand-type op)
                       (logior (x86::x86-operand-type op)
                               (smallest-imm-type (x86-lap-expression-value val))))
                 (when (eql suffix #\q)
                   (setf (x86::x86-operand-type op)
                         (logandc2 (x86::x86-operand-type op)
                                   (x86::encode-operand-type :imm32)))))
                (t ; immediate value not constant
                 (case suffix
                   (#\q (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm64 :imm32S))))
                   (#\l (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm32))))
                   (#\w (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm16))))
                   (#\b  (setf (x86::x86-operand-type op)
                              (logior optype
                                      (x86::encode-operand-type :imm8))))))))))))

(defun get-x86-opcode-templates (form)
  (let* ((name (string (car form))))
    (or
     (gethash name x86::*x86-opcode-template-lists*)
     ;; Try to determine a suffix, based on the size of the last
     ;; register argument (if any.)  If that can be determined,
     ;; tack it on to the end of NAME and try again.
     (let* ((suffix nil))
       (dolist (arg (cdr form))
         (let* ((designator (x86-register-designator arg)))
           (when designator
             (destructuring-bind (regname) (cdr arg)
               (let* ((reg (lookup-x86-register regname designator)))
                 (when reg
                   (let* ((type (x86::reg-entry-reg-type reg)))
                     (cond ((logtest type (x86::encode-operand-type :reg8))
                            (setq suffix #\b))
                           ((logtest type (x86::encode-operand-type :reg16))
                            (setq suffix #\w))
                           ((logtest type (x86::encode-operand-type :reg32))
                            (setq suffix #\l))
                           ((logtest type (x86::encode-operand-type :reg64))
                            (setq suffix #\q))))))))))
       (when suffix
         (let* ((n (length name))
                (m (1+ n))
                (s (make-string m)))
           (declare (fixnum n m) (dynamic-extent s))
           (dotimes (i n) (setf (schar s i) (char name i)))
           (setf (schar s n) suffix)
           (gethash s x86::*x86-opcode-template-lists*)))))))
         
                
         
     
  
;;; FORM is a list; its car doesn't name a macro or pseudo op.  If we
;;; can find a matching opcode template, initialize the
;;; x86-instruction with that template and these operands.
;;; Note that this doesn't handle "prefix" instructions at all.
;;; Things that would change the operand or address size are
;;; of limited utility, as are REP* prefixes on string instructions
;;; (because of the way that the lisp used %[E|R]DI and %[E|R]SI).
;;; LOCK can be used in the preceding instruction.
(defun parse-x86-instruction (form instruction)
    (let* ((templates (or
                       (get-x86-opcode-templates form)
                       (error "Unknown X86 instruction ~s" form)))
           (operands (cdr form)))
      (let* ((parsed-operands (if operands
                                (mapcar #'parse-x86-operand operands)))
             (operand-types (mapcar #'x86::x86-operand-type parsed-operands))
             (type0 (pop operand-types))
             (type1 (pop operand-types))
             (type2 (car operand-types)))

        ;; (x86-optimize-imm parsed-operands suffix)
        (dolist (template templates (error "Operands or suffix invalid in ~s" form))
          (when (x86::match-template-types template type0 type1 type2)
            (init-x86-instruction instruction template parsed-operands)
            ;(check-suffix instruction form)
            ;(x86-finalize-operand-types instruction)
            (return instruction))))))




#+no
(defun build-rm-byte (insn)
  (let* ((rm (x86::make-modrm-byte))
         (default-seg nil)
         (template (x86-lap-instruction-template insn))
         (operands (x86-lap-instruction-parsed-operands insn))
         (num-reg-operands (count-if #'x86::x86-register-operand-p operands)))
    (setf (x86-lap-instruction-rm insn) rm)
    (if (= num-reg-operands 2)
      (let* ((op0 (svref operands 0))
             (type0 (x86::x86-operand-type op0))
             (src (if (logtest type0 (x86::encode-operand-type :reg :regmmx :regxmm
                                                          :sreg2 :sreg3
                                                          :control :debug :test))
                    0
                    1))
             (srcreg (x86::x86-register-operand-entry (svref operands src)))
             (dest (1+ src))
             (destreg (x86::x86-register-operand-entry (svref operands dest))))
        (setf (x86::modrm-byte-mode rm) 3)
        
        ;; One of the register operands will be encoded in the rm-reg
        ;; field, the other in the combined rm-mode and rm-regmem
        ;; fields.  If no form of this instruction supports a memory
        ;; destination operand, then we assume the source operand may
        ;; sometimes be a memory operand and so we need to store the
        ;; destination in the rm-reg field.
        (if (not (logtest (svref (x86::x86-instruction-template-operand-types template) dest)
                          (x86::encode-operand-type :AnyMem)))
          (progn
            (setf (x86::modrm-byte-reg rm) (x86::reg-entry-reg-num destreg)
                  (x86::modrm-byte-regmem rm) (x86::reg-entry-reg-num srcreg))
            (when (logtest x86::+RegRex+ (x86::reg-entry-reg-flags destreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior x86::+rex-extx+
                            (or (x86-lap-instruction-rex insn) 0))))
            (when (logtest x86::+RegRex+ (x86::reg-entry-reg-flags srcreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior x86::+rex-extz+
                            (or (x86-lap-instruction-rex insn) 0)))))
          (progn
            (setf (x86::modrm-byte-reg rm) (x86::reg-entry-reg-num srcreg)
                  (x86::modrm-byte-regmem rm) (x86::reg-entry-reg-num destreg))
            (when (logtest x86::+RegRex+ (x86::reg-entry-reg-flags destreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior x86::+rex-extz+
                            (or (x86-lap-instruction-rex insn) 0))))
            (when (logtest x86::+RegRex+ (x86::reg-entry-reg-flags srcreg))
              (setf (x86-lap-instruction-rex insn)
                    (logior x86::+rex-extx+
                            (or (x86-lap-instruction-rex insn) 0)))))))
      ;; Not exactly 2 register operands.
      (let* ((memop (dotimes (i (length operands))
                      (let* ((op (svref operands i)))
                        (when (logtest (x86::encode-operand-type :AnyMem)
                                       (x86::x86-operand-type op))
                          (return op))))))
        (when memop
          (setq default-seg x86::*ds-segment-register*)
          (let* ((fake-zero-displacement nil)
                 (sib (x86::make-sib-byte)))
            (setf (x86-lap-instruction-sib insn) sib)
            (cond ((null (x86::x86-memory-operand-base memop))
                   (setf (x86::modrm-byte-mode rm) 0)
                   (unless (x86::x86-memory-operand-disp memop)
                     (setq fake-zero-displacement t))
                   (if (null (x86::x86-memory-operand-index memop))
                     (progn
                       (setf (x86::modrm-byte-regmem rm) x86::+escape-to-two-byte-addressing+
                             (x86::sib-byte-base sib) x86::+no-base-register+
                             (x86::sib-byte-index sib) x86::+no-index-register+
                             (x86::sib-byte-scale sib) 0)
                       (let* ((prefixes (x86-lap-instruction-prefixes insn)))
                         (setf (x86::x86-memory-operand-type memop)
                               (if (and prefixes
                                        (not (eql 0 (aref prefixes x86::+addr-prefix+))))
                                 (x86::encode-operand-type :disp32)
                                 (x86::encode-operand-type :disp32s)))))
                     ;; Index, but no base reg.
                     (let* ((index-reg (x86::x86-memory-operand-index memop)))
                       (setf (x86::sib-byte-index sib) (x86::reg-entry-reg-num index-reg)
                             (x86::sib-byte-base sib) x86::+no-base-register+
                             (x86::sib-byte-scale sib) (or (x86::x86-memory-operand-scale memop) 0)
                             (x86::modrm-byte-regmem rm) x86::+escape-to-two-byte-addressing+
                             (x86::x86-memory-operand-type memop) (logior (x86::encode-operand-type :disp32s) (logandc2 (x86::x86-memory-operand-type memop) (x86::encode-operand-type :disp))))
                       (if (logtest (x86::reg-entry-reg-flags index-reg) x86::+RegRex+)
                         (setf (x86-lap-instruction-rex insn)
                               (logior (or (x86-lap-instruction-rex insn) 0)
                                       x86::+rex-exty+))))))
                  ;; 64-bit RIP-relative addressing, detected by the
                  ;; fact that the base register has the :baseindex
                  ;; operand type only
                  ((= (x86::reg-entry-reg-type
                       (x86::x86-memory-operand-base memop))
                      (x86::encode-operand-type :baseIndex))
                   (setf (x86::modrm-byte-regmem rm) x86::+no-base-register+)
                   (setf (x86::x86-memory-operand-type memop)
                         (logior (x86::encode-operand-type :disp32s)
                                 (logandc2 (x86::x86-memory-operand-type memop)
                                           (x86::encode-operand-type :disp))))
                   (unless (x86::x86-memory-operand-disp memop)
                     (setq fake-zero-displacement t)))
                  (t
                   ;; "real" base register
                   (when (logtest (x86::x86-memory-operand-type memop)
                                  (x86::encode-operand-type :disp))
                     (setf (x86::x86-memory-operand-type memop)
                           (logior (logand (x86::x86-memory-operand-type memop)
                                           (x86::encode-operand-type :disp8))
                                   (let* ((prefixes (x86-lap-instruction-prefixes insn)))
                                     (if (and prefixes
                                              (not (eql 0 (aref prefixes x86::+addr-prefix+))))
                                       (x86::encode-operand-type :disp32)
                                       (x86::encode-operand-type :disp32s))))))
                   (let* ((basereg (x86::x86-memory-operand-base memop))
                          (baseregnum (x86::reg-entry-reg-num basereg)))
                     (setf (x86::modrm-byte-regmem rm) baseregnum)
                     (when (logtest (x86::reg-entry-reg-flags basereg) x86::+RegRex+)
                       (setf (x86-lap-instruction-rex insn)
                             (logior x86::+Rex-extz+
                                     (or (x86-lap-instruction-rex insn) 0))))
                     (setf (x86::sib-byte-base sib) baseregnum)
                     (cond ((= (logand baseregnum 7) x86::+ebp-reg-num+)
                            (unless (x86::x86-memory-operand-disp memop)
                              (setq fake-zero-displacement t
                                    default-seg x86::*ss-segment-register*)
                              (setf (x86::x86-memory-operand-type memop)
                                    (logior (x86::x86-memory-operand-type memop)
                                            (x86::encode-operand-type :disp8)))))
                           ((= baseregnum x86::+esp-reg-num+)
                            (setq default-seg x86::*ss-segment-register*)))
                     (setf (x86::sib-byte-scale sib)
                           (or (or (x86::x86-memory-operand-scale memop) 0)))
                     (if (null (x86::x86-memory-operand-index memop))
                       (progn
                         (setf (x86::sib-byte-index sib) x86::+no-index-register+)
                         (when (x86::x86-memory-operand-scale memop)
                           (setf (x86::modrm-byte-regmem rm) x86::+escape-to-two-byte-addressing+)))
                       (progn
                         (setf (x86::sib-byte-index sib) (x86::reg-entry-reg-num (x86::x86-memory-operand-index memop))
                               (x86::modrm-byte-regmem rm) x86::+escape-to-two-byte-addressing+)
                         (when (logtest (x86::reg-entry-reg-flags (x86::x86-memory-operand-index memop))
                                        x86::+regrex+)
                           (setf (x86-lap-instruction-rex insn)
                                 (logior x86::+rex-exty+
                                         (or (x86-lap-instruction-rex insn) 0))))))
                     (setf (x86::modrm-byte-mode rm) (mode-from-disp-size (x86::x86-memory-operand-type memop))))))
            (when fake-zero-displacement
              (setf (x86::x86-memory-operand-disp memop)
                    (make-constant-x86-lap-expression :value 0)
                    (x86::x86-operand-type memop)
                    (logior (x86::x86-operand-type memop) (x86::encode-operand-type :disp8) )))))
        (let* ((regop (dotimes (i (length operands))
                        (let* ((op (svref operands i)))
                          (when (logtest (x86::encode-operand-type :Reg :RegMMX
                                                              :RegXMM
                                                              :SReg2 :SReg3
                                                              :Control :Debug
                                                              :Test)
                                         (x86::x86-operand-type op))
                            (return op)))))
               (regentry (if regop (x86::x86-register-operand-entry regop))))
          (when regop
            (cond ((x86-lap-instruction-extension-opcode insn)
                   (setf (x86::modrm-byte-regmem rm) (x86::reg-entry-reg-num regentry))
                   (when (logtest (x86::reg-entry-reg-flags regentry) x86::+RegRex+)
                     (setf (x86-lap-instruction-rex insn)
                           (logior x86::+rex-extz+
                                   (or (x86-lap-instruction-rex insn) 0)))))
                  (t
                   (setf (x86::modrm-byte-reg rm) (x86::reg-entry-reg-num regentry))
                   (when (logtest (x86::reg-entry-reg-flags regentry) x86::+RegRex+)
                     (setf (x86-lap-instruction-rex insn)
                           (logior x86::+rex-extx+
                                   (or (x86-lap-instruction-rex insn) 0))))))
            (unless memop
              (setf (x86::modrm-byte-mode rm) x86::+regmem-field-has-reg+))))))
    (let* ((extop (x86-lap-instruction-extension-opcode insn)))
      (when extop
        (setf (x86::modrm-byte-reg rm) extop)))
    default-seg))
              

(defun optimize-displacement-type (disp)
  (if disp
    (let* ((value (early-x86-lap-expression-value disp)))
      (if value
        (if (typep value '(signed-byte 8))
          (x86::encode-operand-type :disp8 :disp32 :disp32s :disp64)
          (if (typep value '(signed-byte 32))
            (x86::encode-operand-type :disp32s :disp64)
            (if (typep value '(unsigned-byte 32))
              (x86::encode-operand-type :disp32 :disp64)
              (x86::encode-operand-type :disp64))))
        (x86::encode-operand-type :disp32s :disp64)))
    0))

(defun optimize-displacements (operands)
  (dotimes (i (length operands))
    (let* ((op (svref operands i)))
      (when (typep op 'x86::x86-memory-operand)
        (let* ((disp (x86::x86-memory-operand-disp op))
               (val (if disp (early-x86-lap-expression-value disp))))
          (if (typep val '(signed-byte 32))
            (setf (x86::x86-operand-type op)
                  (logior (x86::x86-operand-type op) (x86::encode-operand-type :disp32s))))
          (if (typep val '(unsigned-byte 32))
            (setf (x86::x86-operand-type op)
                  (logior (x86::x86-operand-type op) (x86::encode-operand-type :disp32))))
          (if (and (logtest (x86::x86-operand-type op)
                            (x86::encode-operand-type :disp32 :disp32S :disp16))
                   (typep val '(signed-byte 8)))
            (setf (x86::x86-operand-type op)
                  (logior (x86::x86-operand-type op) (x86::encode-operand-type :disp8)))))))))

(defun x86-output-branch (frag-list insn)
  (dolist (b (x86::x86-opcode-template-prefixes
              (x86::x86-instruction-opcode-template insn)))
    (when (or (= b x86::+data-prefix-opcode+)
              (= b x86::+cs-prefix-opcode+)
              (= b x86::+ds-prefix-opcode+))
      (frag-list-push-byte frag-list b)))
  (finish-frag-for-branch frag-list
                          (x86::x86-instruction-base-opcode insn)
                          (x86::x86-instruction-extra insn)))


(defun x86-generate-instruction-code (frag-list insn)
  (let* ((template (x86::x86-instruction-opcode-template insn))
         (opcode-modifier (x86::x86-opcode-template-flags template))
         (prefixes (x86::x86-opcode-template-prefixes template)))
    (let* ((explicit-seg-prefix (x86::x86-instruction-seg-prefix insn)))
      (when explicit-seg-prefix
        (push explicit-seg-prefix prefixes)))
      (cond
        ((logtest (x86::encode-opcode-modifier :jump) opcode-modifier)
         ;; a variable-length pc-relative branch, possibly preceded
         ;; by prefixes (used for branch prediction, mostly.)
         (x86-output-branch frag-list insn))
          (t
           (let* ((base-opcode (x86::x86-instruction-base-opcode insn)))
             (declare (fixnum base-opcode))
             (dolist (b prefixes)
               (frag-list-push-byte frag-list b))
             (let* ((rex-bits (logand #xf
                                      (or (x86::x86-instruction-rex-prefix insn)
                                          0))))
               (declare (fixnum rex-bits))
               (unless (= 0 rex-bits)
                 (frag-list-push-byte frag-list (logior #x40 rex-bits))))
             (when (logtest base-opcode #xff00)
               (frag-list-push-byte frag-list (ldb (byte 8 8) base-opcode)))
             (frag-list-push-byte frag-list (ldb (byte 8 0) base-opcode)))
           (let* ((modrm (x86::x86-instruction-modrm-byte insn)))
             (when modrm
               (frag-list-push-byte frag-list modrm)
               (let* ((sib (x86::x86-instruction-sib-byte insn)))
                 (when sib
                   (frag-list-push-byte frag-list sib)))))
           (let* ((disp (x86::x86-instruction-disp insn)))
             (when disp
               (let* ((optype (x86::x86-instruction-extra insn))
                      (val (early-x86-lap-expression-value disp)))
                 (if (null val)
                   ;; We can do better job here, but (for now)
                   ;; generate a 32-bit relocation
                   (let* ((frag (frag-list-current frag-list))
                          (pos (frag-list-position frag-list)))
                     (push (make-reloc :type :expr32
                                       :arg disp
                                       :frag frag
                                       :pos pos)
                           (frag-relocs frag))
                     (frag-list-push-32 frag-list 0))
                   (if (logtest optype (x86::encode-operand-type :disp8))
                     (frag-list-push-byte frag-list (logand val #xff))
                     (if (logtest optype (x86::encode-operand-type :disp32 :disp32s))
                       (frag-list-push-32 frag-list val)
                       (frag-list-push-64 frag-list val)))))))
           ;; Emit immediate operand(s).
           (let* ((op (x86::x86-instruction-imm insn)))
             (when op
               (let* ((optype (x86::x86-operand-type op))
                      (expr (x86::x86-immediate-operand-value op))
                      (val (early-x86-lap-expression-value expr)))
                 (if (null val)
                   (let* ((frag (frag-list-current frag-list))
                          (pos (frag-list-position frag-list))
                          (size 4)
                          (reloctype :expr32))
                     (when (logtest optype
                                    (x86::encode-operand-type
                                     :imm8 :imm8S :imm16 :imm64))
                       (setq size 2 reloctype :expr16)
                       (if (logtest optype (x86::encode-operand-type
                                            :imm8 :imm8s))
                         (setq size 1 reloctype :expr8)
                         (if (logtest optype (x86::encode-operand-type :imm64))
                           (setq size 8 reloctype :expr64))))
                     (push (make-reloc :type reloctype
                                       :arg expr
                                       :frag frag
                                       :pos pos)
                           (frag-relocs frag))
                     (dotimes (b size)
                       (frag-list-push-byte frag-list 0)))
                   (if (logtest optype (x86::encode-operand-type :imm8 :imm8s))
                     (frag-list-push-byte frag-list (logand val #xff))
                     (if (logtest optype (x86::encode-operand-type :imm16))
                       (frag-list-push-16 frag-list (logand val #xffff))
                       (if (logtest optype (x86::encode-operand-type :imm64))
                         (frag-list-push-64 frag-list val)
                         (frag-list-push-32 frag-list val))))))))))))

(defun x86-lap-directive (frag-list directive arg)
  (if (eq directive :tra)
    (progn
      (finish-frag-for-align frag-list 3)
      (x86-lap-directive frag-list :long `(- 15 (^ ,arg)))
      (emit-x86-lap-label frag-list arg))
    (let* ((exp (parse-x86-lap-expression arg))
           (constantp (constant-x86-lap-expression-p exp)))
      (if constantp
        (let* ((val (x86-lap-expression-value exp)))
          (ecase directive
            (:byte (frag-list-push-byte frag-list val))
            (:short (frag-list-push-16 frag-list val))
            (:long (frag-list-push-32 frag-list val))
            (:quad (frag-list-push-64 frag-list val))
            (:align (finish-frag-for-align frag-list val))))
        (let* ((pos (frag-list-position frag-list))
               (frag (frag-list-current frag-list))
               (reloctype nil))
          (ecase directive
            (:byte (frag-list-push-byte frag-list 0)
                   (setq reloctype :expr8))
            (:short (frag-list-push-16 frag-list 0)
                    (setq reloctype :expr16))
            (:long (frag-list-push-32 frag-list 0)
                   (setq reloctype :expr32))
            (:quad (frag-list-push-64 frag-list 0)
                   (setq reloctype :expr64))
            (:align (error ":align expression ~s not constant" arg)))
          (when reloctype
            (push
             (make-reloc :type reloctype
                         :arg exp
                         :pos pos
                         :frag frag)
             (frag-relocs frag)))))
      nil)))
                       
         

(defun x86-lap-form (form frag-list instruction)
  (if (and form (symbolp form))
    (emit-x86-lap-label frag-list form)
    (if (or (atom form) (not (symbolp (car form))))
      (error "Unknown X86-LAP form ~s ." form)
      (multiple-value-bind (expansion expanded)
          (x86-lap-macroexpand-1 form)
        (if expanded
          (x86-lap-form expansion frag-list instruction)
          (if (typep (car form) 'keyword)
            (destructuring-bind (op arg) form
              (x86-lap-directive frag-list op arg))
            (case (car form)
              (progn
                (dolist (f (cdr form))
                  (x86-lap-form f frag-list instruction)))
              (let
                  (destructuring-bind (equates &body body)
                      (cdr form)
                    (x86-lap-equate-form equates frag-list instruction body)))
              (t
               (reset-x86-instruction instruction)
               (parse-x86-instruction form instruction)
               (x86-generate-instruction-code frag-list instruction)))))))))

(defun relax-frag-list (frag-list)
  ;; First, assign tentative addresses to all frags, assuming that
  ;; span-dependent instructions have short displacements.
  (let* ((address 8))
    (declare (fixnum address))
    (ccl::do-dll-nodes (frag frag-list)
      (setf (frag-address frag) address)
      (incf address (length (frag-code-buffer frag)))
      (case (car (frag-type frag))
        (:align
         (let* ((bits (cadr (frag-type frag)))
                (mask (1- (ash 1 bits))))
           (setq address (logandc2 (+ address mask) mask))))
        ((:assumed-short-branch :assumed-short-conditional-branch)))))
  ;; Repeatedly "stretch" frags containing span-dependent instructions
  ;; until nothing's stretched.  It may take several iterations to
  ;; converge; is convergence guaranteed ?
  (loop
    (let* ((stretch 0)                    ;cumulative growth in frag sizes
           (stretched nil))               ;any change on this pass ?
      (ccl::do-dll-nodes (frag frag-list)
        (let* ((growth 0)
               (fragtype (frag-type frag))
               (was-address (frag-address frag))
               (address (incf (frag-address frag) stretch)))
          (setf (frag-relax-marker frag) (not (frag-relax-marker frag)))
          (case (car fragtype)
            (:align
             (let* ((bits (cadr fragtype))
                    (mask (1- (ash 1 bits)))
                    (oldoff (logandc2 (+ was-address mask) mask))
                    (newoff (logandc2 (+ address mask) mask)))
               (setq growth (- newoff oldoff))))
            ;; If we discover - on any iteration - that a short
            ;; branch doesn't fit, we change the type (and the reloc)
            ;; destructively to a wide branch indicator and will
            ;; never change our minds about that, so we only have
            ;; to look here at conditional branches that may still
            ;; be able to use a 1-byte displacement.
            ((:assumed-short-branch :assumed-short-conditional-branch)
             (destructuring-bind (label-op pos reloc) (cdr (frag-type frag))
               (declare (fixnum pos))
               (let* ((label (x86::x86-label-operand-label label-op))
                      (label-address (x86-lap-label-address label))
                      (branch-pos (+ address (1+ pos)))
                      (buffer (frag-code-buffer frag))
                      (diff (- branch-pos label-address)))
                 (unless (typep diff '(signed-byte 8))
                   (cond ((eq (car fragtype) :assumed-short-branch)
                          ;; replace the opcode byte
                          (setf (aref buffer (the fixnum (1- pos)))
                                      x86::+jump-pc-relative+)
                          (vector-push-extend 0 buffer)
                          (vector-push-extend 0 buffer)
                          (vector-push-extend 0 buffer)
                          (setf (reloc-type reloc) :branch32)
                          (setf (car fragtype) :long-branch)
                          (setq growth 3))
                         (t
                          ;; Conditional branch: must change
                          ;; 1-byte opcode to 2 bytes, add 4-byte
                          ;; displacement
                          (let* ((old-opcode (aref buffer (1- pos))))
                            (setf (aref buffer (1- pos)) #x0f
                                  (aref buffer pos) (+ old-opcode #x10))
                            (vector-push-extend 0 buffer)
                            (vector-push-extend 0 buffer)
                            (vector-push-extend 0 buffer)
                            (vector-push-extend 0 buffer)
                            (setf (reloc-type reloc) :branch32
                                  (reloc-pos reloc) (1+ pos))
                            (setf (car fragtype) :long-conditional-branch
                                  (caddr fragtype) (1+ pos))
                            (setq growth 4)))))))))
          (unless (eql 0 growth)
            (incf stretch growth)
            (setq stretched t))))
      (unless stretched (return)))))

(defun apply-relocs (frag-list)
  (flet ((emit-byte (buffer pos b)
           (setf (aref buffer pos) (logand b #xff))))
    (flet ((emit-short (buffer pos s)
             (setf (aref buffer pos) (ldb (byte 8 0) s)
                   (aref buffer (1+ pos)) (ldb (byte 8 8) s))))
      (flet ((emit-long (buffer pos l)
               (emit-short buffer pos (ldb (byte 16 0) l))
               (emit-short buffer (+ pos 2) (ldb (byte 16 16) l))))
        (flet ((emit-quad (buffer pos q)
                 (emit-long buffer pos (ldb (byte 32 0) q))
                 (emit-long buffer (+ pos 4) (ldb (byte 32 32) q))))
          (do-dll-nodes (frag frag-list)
            (let* ((buffer (frag-code-buffer frag))
                   (address (frag-address frag)))
              (dolist (reloc (frag-relocs frag))
                (let* ((pos (reloc-pos reloc))
                       (arg (reloc-arg reloc)))
                  (ecase (reloc-type reloc)
                    (:branch8 (let* ((label (x86::x86-label-operand-label arg))
                                     (target (x86-lap-label-address label))
                                     (refpos (+ address (1+ pos))))
                                (emit-byte buffer pos (- target refpos))))
                    (:branch32 (let* ((label (x86::x86-label-operand-label arg))
                                     (target (x86-lap-label-address label))
                                     (refpos (+ address pos 4)))
                                (emit-long buffer pos (- target refpos))))
                    (:expr8 (emit-byte buffer pos  (x86-lap-expression-value arg)))
                    (:expr16 (emit-short buffer pos (x86-lap-expression-value arg)))
                    (:expr32 (emit-long buffer pos (x86-lap-expression-value arg)))
                    (:expr64 (emit-quad buffer pos (x86-lap-expression-value  arg)))))))))))))
                             

(defun fill-for-alignment (frag-list)
  (ccl::do-dll-nodes (frag frag-list)
    (let* ((next (ccl::dll-node-succ frag)))
      (unless (eq next frag-list)
        (let* ((addr (frag-address frag))
               (buffer (frag-code-buffer frag))
               (nextaddr (frag-address next))
               (pad (- nextaddr (+ addr (length buffer)))))
          (unless (eql 0 pad)
            (setq buffer (or buffer (setf (frag-code-buffer frag)
                                          (make-array pad
                                                      :element-type '(unsigned-byte 8)
                                                      :fill-pointer 0))))
            (dotimes (i pad) (vector-push-extend #xcc buffer))))))))

(defun show-frag-bytes (frag-list)
  (ccl::do-dll-nodes (frag frag-list)
    (let* ((buffer (frag-code-buffer frag)))
      (format t "~& frag at #x~x" (frag-address frag))
      (dotimes (i (length buffer))
        (unless (logtest 15 i)
          (format t "~&"))
        (format t "~2,'0x " (aref buffer i))))))

(defun x86-lap-equate-form (eqlist fraglist instruction  body) 
  (let* ((symbols (mapcar #'(lambda (x)
                              (let* ((name (car x)))
                                (or
                                 (and name 
                                      (symbolp name)
                                      (not (constant-symbol-p name))
                                      name)
                                 (error 
                                  "~S is not a bindable symbol name ." name))))
                          eqlist))
         (values (mapcar #'(lambda (x) (x86-register-ordinal-or-expression
                                        (cadr x)))
                         eqlist)))
    (progv symbols values
      (dolist (form body)
        (x86-lap-form form fraglist instruction)))))          
                
#-x86-target
(defun cross-create-x86-function (name frag-list constants bits)
  (let* ((constants-vector (%alloc-misc (+ (length constants)
                                           (if name 3 2))
                                        target::subtag-xfunction)))
    (unless name (setq bits (logior bits (ash -1 $lfbits-noname-bit))))
    (let* ((last (1- (uvsize constants-vector))))
      (declare (fixnum last))
      (setf (uvref constants-vector last) bits)
      (when name
        (setf (uvref constants-vector (decf last)) name))
      (dolist (c constants)
        (setf (uvref constants-vector (decf last)) c))
      (let* ((nbytes 0))
        (do-dll-nodes (frag frag-list)
          (incf nbytes (length (frag-code-buffer frag))))
        (let* ((code-vector (make-array nbytes
                                        :element-type '(unsigned-byte 8)))
               (target-offset 0))
          (declare (fixnum target-offset))
          (setf (uvref constants-vector 0) code-vector)
          (do-dll-nodes (frag frag-list)
            (let* ((buffer (frag-code-buffer frag))
                   (length (length buffer)))
              (declare (fixnum length))
              (when buffer
                (multiple-value-bind (data offset)
                    (array-data-and-offset buffer)
                  (%copy-ivector-to-ivector data
                                            offset
                                            code-vector
                                            target-offset
                                            length)
                  (incf target-offset length)))))
          constants-vector)))))
      
(defun %define-x86-lap-function (name forms function-creator &optional (bits 0))
  (let* ((*x86-lap-labels* ())
         (*x86-lap-constants* ())
         (end-code-tag (gensym))
         (instruction (x86::make-x86-instruction))
         (frag-list (make-frag-list)))
    (make-x86-lap-label end-code-tag)
    (x86-lap-directive frag-list :long `(ash (+ (- (^ ,end-code-tag ) 8)
                                              *x86-lap-entry-offset*) -3))
    (x86-lap-directive frag-list :short 0)
    (x86-lap-directive frag-list :byte 0)
    (dolist (f forms)
      (x86-lap-form f frag-list instruction))
    (x86-lap-directive frag-list :align 3)
    (emit-x86-lap-label frag-list end-code-tag)
    (dolist (c *x86-lap-constants*)
      (emit-x86-lap-label frag-list (x86-lap-label-name (cdr c)))
      (x86-lap-directive frag-list :quad 0))
    (when name
      (x86-lap-directive frag-list :quad 0))
    ;; room for lfun-bits
    (x86-lap-directive frag-list :quad 0)
    (relax-frag-list frag-list)
    (apply-relocs frag-list)
    (fill-for-alignment frag-list)
    ;;(show-frag-bytes frag-list)
    (funcall function-creator name frag-list (mapcar #'car *x86-lap-constants*) bits)))


(defmacro defx86lapfunction (&environment env name arglist &body body
                             &aux doc)
  (if (not (endp body))
      (and (stringp (car body))
           (cdr body)
           (setq doc (car body))
           (setq body (cdr body))))
  `(progn
     (eval-when (:compile-toplevel)
       (note-function-info ',name t ,env))
     #-x86-target
     (progn
       (eval-when (:load-toplevel)
         (%defun (nfunction ,name (lambda (&lap 0) (x86-lap-function ,name ,arglist ,@body))) ,doc))
       (eval-when (:execute)
         (%define-x86-lap-function ',name '((let ,arglist ,@body)) #'cross-create-x86-function)))
     #+x86-target	; just shorthand for defun
     (%defun (nfunction ,name (lambda (&lap 0) (x86-lap-function ,name ,arglist ,@body))) ,doc)))
