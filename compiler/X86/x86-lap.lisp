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


  (defstruct (x86-lap-instruction (:include ccl::dll-node)
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
    )

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
(defvar *x86-lap-instructions* ())
(defvar *x86-lap-constants* ())
(defvar *x86-lap-macros* (make-hash-table :test #'equalp))


(defun x86-lap-macro-function (name)
  (gethash (string name) #|(backend-lap-macros *ppc-backend*)|#
           *x86-lap-macros*))

(defun (setf x86-lap-macro-function) (def name)
  (let* ((s (string name)))
    (when (gethash s x86::*x86-instruction-template-lists*)
      (error "~s already defines an x86 instruction . " name))
    (setf (gethash s *x86-lap-macros* #|(backend-lap-macros *ppc-backend*)|#) def)))

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

(defmethod print-object ((i x86-lap-instruction) stream)
  (let* ((template (x86-lap-instruction-template i))
         (operands (x86-lap-instruction-parsed-operands i)))
    (print-unreadable-object (i stream :type t)
      (when template
        (format stream "~a" (x86::x86-instruction-template-name template))
        (let* ((suffix (x86-lap-instruction-suffix i)))
          (when suffix
            (format stream "[~c]" suffix)))
        (dotimes (i (length operands))
          (format stream " ~a" (x86::unparse-operand (svref operands i))))))))

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

;;; Expression might contain unresolved labels.  Return nil if so (even
;;; if everything -could- be resolved.)
(defun early-expression-value (expression)
  (etypecase expression
    (constant-expression (constant-expression-value expression))
    (expression nil)))

(defun x86-lap-label-address (lab)
  (let* ((frag (or (x86-lap-label-frag lab)
                   (error "Label ~s was referenced but not defined"
                          (x86-lap-label-name lab)))))
    (+ (frag-address frag)
       (x86-lap-label-offset lab))))

(defun parse-expression (form)
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
          (setq form `(- (^ ,(x86-lap-label-name constant-label)) (^ @entry)))))))
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

(defun check-base-and-index-regs (instruction base index)
  (let* ((regxx (if (and (x86-lap-instruction-prefixes instruction)
                         (not (zerop (aref (x86-lap-instruction-prefixes instruction) x86::+addr-prefix+))))
                  (x86::encode-operand-type :reg32)
                  (x86::encode-operand-type :reg64))))
    (if (or (and base
                 (not (logtest (x86::reg-entry-reg-type base) regxx))
                 (or (not (eql (x86::reg-entry-reg-type base)
                               (x86::encode-operand-type :baseindex)))
                     index))
            (and index
                 (not (eql (logior regxx (x86::encode-operand-type :baseindex))
                           (logand (x86::reg-entry-reg-type index)
                                   (logior regxx
                                           (x86::encode-operand-type :baseindex)))))))
      (if base
        (if index
          (error "Invalid base/index registers ~s/~s" base index)
          (error "Invalid base register ~s" base))
        (error "Invalid index register ~s" index)))))

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
(defun parse-x86-memory-operand (instruction form)
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
              (check-base-and-index-regs instruction base index)
              (x86::make-x86-memory-operand 
               :type (if (or base index)
                       (if disp
                         (x86::encode-operand-type :disp64 :disp32 :disp32S :baseindex)
                         (x86::encode-operand-type :baseindex))
                       (x86::encode-operand-type :disp64 :disp32 :disp32S))
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
            (let* ((exp (parse-expression head))
                   (val (if (typep exp 'constant-expression)
                          (expression-value exp))))
              (case val
                ((1 2 4 8)
                 (if (and base (not index))
                   (setq index base base nil))
                 (setq scale (1- (integer-length val))))
                (t
                 (error "Invalid scale factor ~s in ~s" head form))))
            (if (not (or disp base index))
              (setq disp (parse-expression head))
              (error "~& not expected in ~s" head form)))))))))

     
    

;;; Operand syntax:
;;; (^ x) -> labelref
;;; (% x) -> register
;;; ($ x) -> immediate
;;; (* x) -> x with :jumpabsolute attribute
;;; (@ x) -> memory operand
;;; x -> shorthand for (@ x)
(defun parse-x86-operand (instruction form)
  (if (consp form)
    (let* ((head (car form))
           (designator nil))
      (if (symbolp head)
        (cond ((string= head '$)
               (destructuring-bind (immval) (cdr form)
                 (let* ((expr (parse-expression immval)))
                   (x86::make-x86-immediate-operand :type (x86::encode-operand-type :imm64)
                                             :value expr))))
              ((setq designator (x86-register-designator form))
               (destructuring-bind (reg) (cdr form)
                 (parse-x86-register-operand reg designator)))
              ((string= head '*)
               (destructuring-bind (subop) (cdr form)
                 (let* ((op (parse-x86-operand instruction subop)))
                   (setf (x86::x86-operand-type op)
                         (logior (x86::encode-operand-type :jumpabsolute)
                                 (x86::x86-operand-type op)))
                   op)))
              ((string= head '@)
               (parse-x86-memory-operand instruction (cdr form)))
              ((string= head '^)
               (destructuring-bind (lab) (cdr form)
                 (parse-x86-label-reference lab)))
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
            (not (zerop (logandc2 overlap (x86::encode-operand-type :jumpabsolute))))
            (= (logand given (x86::encode-operand-type :baseindex :jumpabsolute))
               (logand overlap (x86::encode-operand-type :baseindex :jumpabsolute)))))
         (consistent-register-match (m0 g0 t0 m1 g1 t1)
           (let* ((g0&reg (logand g0 (x86::encode-operand-type :reg)))
                  (g1&reg (logand g1 (x86::encode-operand-type :reg))))
             (or (zerop g0&reg)
                 (zerop g1&reg)
                 (= g0&reg g1&reg)
                 (not
                  (logtest
                   (if (logtest m0 (x86::encode-operand-type :acc))
                     (x86::encode-operand-type :reg)
                     t0)
                   (if (logtest m1 (x86::encode-operand-type :acc))
                     (x86::encode-operand-type :reg)
                     t1)))))))
    (let* ((nops (length parsed-operands))
           (type0 (if (> nops 0) (x86::x86-operand-type (svref parsed-operands 0))))
           (type1 (if (> nops 1) (x86::x86-operand-type (svref parsed-operands 1))))
           (type2 (if (> nops 2) (x86::x86-operand-type (svref parsed-operands 2)))))
      (declare (fixnum nops))
      (let* ((suffix-check
              (case suffix
                (#\b (x86::encode-opcode-modifier :no-bsuf))
                (#\w (x86::encode-opcode-modifier :no-wsuf))
                (#\s (x86::encode-opcode-modifier :no-ssuf))
                (#\l (x86::encode-opcode-modifier :no-lsuf))
                (#\q (x86::encode-opcode-modifier :no-qsuf))
                (#\x (x86::encode-opcode-modifier :no-xsuf))
                (t 0))))
        (when (= nops
                 (the fixnum (x86::x86-instruction-template-operands template)))
          (unless (logtest
                   (x86::x86-instruction-template-opcode-modifier template)
                   suffix-check)
            (or (zerop nops)
                (let* ((template-types
                      (x86::x86-instruction-template-operand-types template))
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
                             (make-array x86::+max-prefixes+
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0))))
         (q 
          (if (and (>= prefix x86::+rex-opcode+)
                   (< prefix (+ x86::+rex-opcode+ 16)))
            x86::+rex-prefix+
            (ecase prefix
              ((#.x86::+cs-prefix-opcode+ #.x86::+ds-prefix-opcode+ #.x86::+es-prefix-opcode+
                                     #.x86::+fs-prefix-opcode+ #.x86::+gs-prefix-opcode+
                                     #.x86::+ss-prefix-opcode+)
               x86::+seg-prefix+)
              ((#.x86::+repne-prefix-opcode+ #.x86::+repe-prefix-opcode+)
               (setq ret 2)
               x86::+lockrep-prefix+)
              (#.x86::+lock-prefix-opcode+ x86::+lockrep-prefix+)
              (#.x86::+fwait-opcode+ x86::+wait-prefix+)
              (#.x86::+addr-prefix-opcode+ x86::+addr-prefix+)
              (#.x86::+data-prefix-opcode+ x86::+data-prefix+)))))
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
           (x86::reg-entry-reg-name
            (x86::x86-register-operand-entry op))
           (x86::x86-instruction-template-name template)
           (x86-lap-instruction-suffix instruction))))
    
;;; Not sure whether inoutportreg needs special handling, but it's
;;; not clear that we'd be doing I/O in LAP, anyway.
(defun check-byte-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86::x86-operand-type op)))
        (unless (logtest optype (x86::encode-operand-type :reg8))
          (when (logtest optype
                         (x86::encode-operand-type :reg :regmmx :regxmm :sreg2 :sreg3 :control :debug :test :floatreg :floatacc))
            (bad-register-operand-for-suffix instruction i)))))))

(defun check-long-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (template (x86-lap-instruction-template instruction))
         (template-types (x86::x86-instruction-template-operand-types template)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86::x86-operand-type op))
             (template-type (svref template-types i)))
        ;; Reject 8-bit registers, except where the template requres
        ;; them (e.g., movzb)
        (if (or (and (logtest optype (x86::encode-operand-type :reg8))
                     (logtest template-type (x86::encode-operand-type :reg16 :reg32 :acc)))
                (and (logtest optype (x86::encode-operand-type :reg16))
                     (logtest template-type (x86::encode-operand-type :reg32 :acc)))
                (and (logtest optype (x86::encode-operand-type :reg64))
                     (logtest template-type (x86::encode-operand-type :reg32 :acc))))
          (bad-register-operand-for-suffix instruction i))))))

(defun check-qword-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (template (x86-lap-instruction-template instruction))
         (template-types (x86::x86-instruction-template-operand-types template)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86::x86-operand-type op))
             (template-type (svref template-types i)))
        ;; Reject 8-bit registers, except where the template requres
        ;; them (e.g., movzb)
        (if (or (and (logtest optype (x86::encode-operand-type :reg8))
                     (logtest template-type (x86::encode-operand-type :reg16 :reg32 :acc)))
                (and (or
                      (logtest optype (x86::encode-operand-type :reg16))
                      (logtest optype (x86::encode-operand-type :reg32)))
                     (logtest template-type (x86::encode-operand-type :reg32 :acc))))
          (bad-register-operand-for-suffix instruction i))))))

(defun check-word-reg (instruction)
  (let* ((parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (template (x86-lap-instruction-template instruction))
         (template-types (x86::x86-instruction-template-operand-types template)))
    (dotimes (i (length parsed-operands))
      (let* ((op (svref parsed-operands i))
             (optype (x86::x86-operand-type op))
             (template-type (svref template-types i)))
        ;; Reject 8-bit registers, except where the template requres
        ;; them (e.g., movzb)
        (if (or (and (logtest optype (x86::encode-operand-type :reg8))
                     (logtest template-type (x86::encode-operand-type :reg16 :reg32 :acc)))
                (and (logtest optype (x86::encode-operand-type :reg32))
                     (logtest template-type (x86::encode-operand-type :reg16 :acc))))
          (bad-register-operand-for-suffix instruction i))))))
  
(defun check-suffix (instruction form)
  (let* ((template (x86-lap-instruction-template instruction))
         (parsed-operands (x86-lap-instruction-parsed-operands instruction))
         (suffix (x86-lap-instruction-suffix instruction))
         (template-modifier (x86::x86-instruction-template-opcode-modifier template))
         (has-register-operands (dotimes (i (length parsed-operands))
                                  (if (logtest (x86::encode-operand-type :reg)
                                               (x86::x86-operand-type
                                                (svref parsed-operands i)))
                                    (return t)))))
    (cond ((logtest template-modifier
                    (x86::encode-opcode-modifier :size16 :size32 :size64))
           (cond ((logtest template-modifier (x86::encode-opcode-modifier :size16))
                  (setq suffix x86::+word-mnem-suffix+))
                 ((logtest template-modifier (x86::encode-opcode-modifier :size32))
                  (setq suffix x86::+long-mnem-suffix+))
                 ((logtest template-modifier (x86::encode-opcode-modifier :size64))
                  (setq suffix x86::+qword-mnem-suffix+))))
          (has-register-operands
           ;; If there's no explicit suffix, try to derive it from the
           ;; type of the last (destination) register operand.
           (cond ((null suffix)
                  (do* ((i (1- (length parsed-operands)) (1- i)))
                       ((< i 0))
                    (declare (fixnum i))
                    (let* ((type-i (x86::x86-operand-type (svref parsed-operands i)))
                           (tm-type-i (svref (x86::x86-instruction-template-operand-types template) i)))
                      (when (and (logtest type-i (x86::encode-operand-type :reg))
                               (not (logtest tm-type-i
                                             (x86::encode-operand-type :inoutportreg))))
                        (setq
                         suffix
                         (cond ((logtest type-i (x86::encode-operand-type :reg8))
                                x86::+byte-mnem-suffix+)
                               ((logtest type-i (x86::encode-operand-type :reg16))
                                x86::+word-mnem-suffix+)
                               ((logtest type-i (x86::encode-operand-type :reg64))
                                x86::+qword-mnem-suffix+)
                               (t x86::+long-mnem-suffix+)))
                        (return)))))
                 ((eql suffix x86::+byte-mnem-suffix+)
                  (check-byte-reg instruction))
                 ((eql suffix x86::+long-mnem-suffix+)
                  (check-long-reg instruction))
                 ((eql suffix x86::+qword-mnem-suffix+)
                  (check-qword-reg instruction))
                 ((eql suffix x86::+word-mnem-suffix+)
                  (check-word-reg instruction)))))
    (unless (setf (x86-lap-instruction-suffix instruction) suffix)
      (if (logtest template-modifier (x86::encode-opcode-modifier :w))
        (error "No mnemonic suffix specified and can't determine instruction size from operands in ~s" form)))
    ;; Change the opcode based on the operand size given by suffix. We
    ;; don't need to change things for byte insns.
    (when (and suffix (not (eql suffix x86::+byte-mnem-suffix+)))
      (when (logtest template-modifier (x86::encode-opcode-modifier :w))
        (setf (x86-lap-instruction-base-opcode instruction)
              (logior (x86-lap-instruction-base-opcode instruction)
                      (if (logtest template-modifier (x86::encode-opcode-modifier :shortform))
                        8
                        1))))
    
      ;; Now select between word & dword operations via the operand
      ;; size prefix, except for instructions that will ignore this
      ;; prefix anyway. 
      (if (and (not (eql suffix x86::+qword-mnem-suffix+))
               (not (eql suffix x86::+long-double-mnem-suffix+))
               (not (logtest template-modifier
                             (x86::encode-opcode-modifier :ignoresize :floatmf)))
               (or (not (eql suffix x86::+long-mnem-suffix+))
                   (logtest template-modifier (x86::encode-opcode-modifier :jumpbyte))))
        (add-prefix instruction
                    (if (logtest template-modifier
                                 (x86::encode-opcode-modifier :jumpbyte))
                      x86::+addr-prefix-opcode+
                      x86::+data-prefix-opcode+)))
      ;; Set mode64 if appropriate.
      (if (and (eql suffix x86::+qword-mnem-suffix+)
               (not (logtest template-modifier (x86::encode-opcode-modifier :norex64))))
        (setf (x86-lap-instruction-rex instruction)
              (logior (or (x86-lap-instruction-rex instruction) 0)
                      x86::+rex-mode64+)))
      ;; Size floating-point instruction
      (if (and (eql suffix x86::+long-mnem-suffix+)
               (logtest template-modifier (x86::encode-opcode-modifier :floatmf)))
        (setf (x86-lap-instruction-base-opcode instruction)
              (logxor (x86-lap-instruction-base-opcode instruction) 4))))
    instruction))

(defun init-x86-lap-instruction (instruction template parsed-operands suffix)
  (setf (x86-lap-instruction-template instruction) template
        (x86-lap-instruction-parsed-operands instruction) parsed-operands
        (x86-lap-instruction-base-opcode instruction) (x86::x86-instruction-template-base-opcode template)
        (x86-lap-instruction-extension-opcode instruction) (x86::x86-instruction-template-extension-opcode template)
        (x86-lap-instruction-suffix instruction) suffix)
  instruction)

(defun reset-x86-instruction (i)
  (setf (x86-lap-instruction-template i) nil
        (x86-lap-instruction-parsed-operands i) nil
        (x86-lap-instruction-flags i) nil
        (x86-lap-instruction-base-opcode i) nil
        (x86-lap-instruction-extension-opcode i) nil
        (x86-lap-instruction-prefixes i) nil
        (x86-lap-instruction-nprefixes i) 0
        (x86-lap-instruction-rex i) nil
        (x86-lap-instruction-rm i) nil
        (x86-lap-instruction-sib i) nil)
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
       
(defun x86-finalize-operand-types (inst)
  (let* ((template (x86-lap-instruction-template inst))
         (operands (x86-lap-instruction-parsed-operands inst))
         (suffix (x86-lap-instruction-suffix inst))
         (template-types (x86::x86-instruction-template-operand-types
                          template))
         (prefixes (x86-lap-instruction-prefixes inst))
         (noperands (length operands)))
    (declare (fixnum noperands))
    (when (> noperands 0)
      (let* ((op0 (svref operands 0))
             (op-type0 (x86::x86-operand-type op0))
             (template-type0 (svref template-types 0))
             (overlap0 (logand op-type0 template-type0)))
        (when (and (logtest overlap0 (x86::encode-operand-type
                                      :imm8 :imm8s :imm16 :imm32 :imm32s :imm64))
                   (not (eql (logcount overlap0) 1)))
          (if suffix
            (setq overlap0
                  (logand overlap0
                          (case suffix
                            (#\b (x86::encode-operand-type :imm8 :imm8s))
                            (#\w (x86::encode-operand-type :imm16))
                            (#\q (x86::encode-operand-type :imm64 :imm32s))
                            (t (x86::encode-operand-type :imm32)))))
            (if (or (= overlap0 (x86::encode-operand-type :imm16 :imm32S :imm32))
                    (= overlap0 (x86::encode-operand-type :imm16 :imm32))
                    (= overlap0 (x86::encode-operand-type :imm16 :imm32S)))
              (setq overlap0 (if (and prefixes (not (eql (aref prefixes
                                                               x86::+data-prefix+)
                                                         0)))
                               (x86::encode-operand-type :imm16)
                               (x86::encode-operand-type :imm32s)))))
          (unless (= (logcount overlap0) 1)
            (error "Can't determine size of immediate operand in ~s"
                   inst)))
        ;; Do this regardless of whether or not op was immediate
        (setf (x86::x86-operand-type op0) overlap0))
      (when (> noperands 1)
        (let* ((op1 (svref operands 1))
               (op-type1 (x86::x86-operand-type op1))
               (template-type1 (svref template-types 1))
               (overlap1 (logand op-type1 template-type1)))
          (when (and (logtest overlap1 (x86::encode-operand-type
                                        :imm8 :imm8s :imm16 :imm32 :imm32s :imm64))
                     (not (eql (logcount overlap1) 1)))
            (if suffix
              (setq overlap1
                    (logand overlap1
                            (case suffix
                              (#\b (x86::encode-operand-type :imm8 :imm8s))
                              (#\w (x86::encode-operand-type :imm16))
                              (#\q (x86::encode-operand-type :imm64 :imm32s))
                              (t (x86::encode-operand-type :imm32)))))
              (if (or (= overlap1 (x86::encode-operand-type :imm16 :imm32S :imm32))
                      (= overlap1 (x86::encode-operand-type :imm16 :imm32))
                      (= overlap1 (x86::encode-operand-type :imm16 :imm32S)))
                (setq overlap1 (if (and prefixes (not (eql (aref prefixes
                                                                 x86::+data-prefix+)
                                                           0)))
                                 (x86::encode-operand-type :imm16)
                                 (x86::encode-operand-type :imm32s)))))
            (unless (= (logcount overlap1) 1)
              (error "Can't determine size of immediate operand in ~s"
                     inst)))
          (setf (x86::x86-operand-type op1) overlap1))
        (when (= noperands 3)
          (let* ((op2 (svref operands 2))
                 (op-type2 (x86::x86-operand-type op2))
                 (template-type2 (svref template-types 2)))
            ;; third operand can't be an immediate
            (setf (x86::x86-operand-type op2)
                  (logand op-type2 template-type2))))))))
        
          
          
          
        
    
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
          (cond ((typep val 'constant-expression)
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
                               (smallest-imm-type (expression-value val))))
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
  
;;; FORM is a list; its car doesn't name a macro or pseudo op.
;;; If we can find a matching instruction template, create a
;;; (skeletal) x86-lap-instruction with that template and these
;;; operands.
(defun parse-x86-instruction (form instruction)
  (labels
      ((parse-x86-instruction-aux (subform expecting-string)
         (if (null subform)
           (error "No non-prefix X86 instruction in ~s" form)
           (destructuring-bind (mnemonic &rest operands) subform
             (multiple-value-bind (templates suffix)
                 (x86::get-x86-instruction-templates-and-suffix mnemonic)
               (let* ((t0 (car templates))
                      (t0-modifier (x86::x86-instruction-template-opcode-modifier t0)))
                 (if (logtest t0-modifier
                              (x86::encode-opcode-modifier :isprefix))
                   (let* ((prefix (x86::x86-instruction-template-base-opcode t0)))
                     (add-prefix instruction prefix)
                     (parse-x86-instruction-aux
                      operands
                      (or expecting-string
                          (if (or (eql prefix x86::+repe-prefix-opcode+)
                                  (eql prefix x86::+repne-prefix-opcode+))
                            (x86::x86-instruction-template-name t0)))))
                                                    
                   (let* ((parsed-operands (if operands
                                             (map
                                              'vector
                                              #'(lambda (f)
                                                  (parse-x86-operand
                                                   instruction f))
                                              operands))))
                     (when (and expecting-string
                                (not (logtest
                                      (x86::x86-instruction-template-opcode-modifier
                                       (car templates))
                                      (x86::encode-opcode-modifier :isstring))))
                       (error "Expecting string instruction after ~s in ~s"
                              expecting-string form))
                     (x86-optimize-imm parsed-operands suffix)
                     (dolist (template templates (error "Operands or suffix invalid in ~s" form))
                       (when (match-template template parsed-operands suffix)
                         (init-x86-lap-instruction instruction template parsed-operands suffix)
                         (check-suffix instruction form)
                         (x86-finalize-operand-types instruction)
                         (return instruction)))))))))))
    (parse-x86-instruction-aux form nil)))

(defun mode-from-disp-size (type)
  (cond ((logtest type (x86::encode-operand-type :disp8)) 1)
        ((logtest type (x86::encode-operand-type :disp16 :disp32 :disp32S)) 2)
        (t 0)))

(defun x86-process-instruction-shortform (instruction)
  (let* ((operands (x86-lap-instruction-parsed-operands instruction))
         (op0 (svref operands 0))
         (op1 (svref operands 1))
         (reg (if (logtest (x86::encode-operand-type :reg)
                           (x86::x86-operand-type op0))
                (x86::x86-register-operand-entry op0)
                (x86::x86-register-operand-entry op1))))
    (setf (x86-lap-instruction-base-opcode instruction)
          (logior (x86-lap-instruction-base-opcode instruction)
                  (x86::reg-entry-reg-num reg)))
    (when (logtest (x86::reg-entry-reg-flags reg) x86::+regrex+)
      (setf (x86-lap-instruction-rex instruction)
            (logior (or (x86-lap-instruction-rex instruction) 0)
                    x86::+rex-extz+)))))

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
                                         (or (x86-lap-instruction-rex insn) 0)))))))))
            (setf (x86::modrm-byte-mode rm) (mode-from-disp-size (x86::x86-memory-operand-type memop)))
            (when fake-zero-displacement
              (setf (x86::x86-memory-operand-disp memop)
                    (make-constant-expression :value 0)
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
              

(defun optimize-displacements (operands)
  (dotimes (i (length operands))
    (let* ((op (svref operands i)))
      (when (typep op 'x86::x86-memory-operand)
        (let* ((disp (x86::x86-memory-operand-disp op))
               (val (if disp (early-expression-value disp))))
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
  (let* ((prefixes (x86-lap-instruction-prefixes insn)))
    (flet ((maybe-output-prefix (index val)
             (when prefixes
               (let* ((c (aref prefixes index)))
                 (unless (zerop c)
                   (frag-list-push-byte frag-list (or val c)))))))
      (maybe-output-prefix x86::+DATA-PREFIX+ x86::+data-prefix-opcode+)
      (when (and prefixes (or (= (aref prefixes x86::+seg-prefix+)
                                 x86::+cs-prefix-opcode+)
                              (= (aref prefixes x86::+seg-prefix+)
                                 x86::+ds-prefix-opcode+)))
        (maybe-output-prefix x86::+seg-prefix+ nil))
      (maybe-output-prefix x86::+rex-prefix+ nil)
      (finish-frag-for-branch frag-list
                              (x86-lap-instruction-base-opcode insn)
                              (svref (x86-lap-instruction-parsed-operands insn)
                                     0)))))


(defun x86-generate-instruction-code (frag-list insn)
  (let* ((template (x86-lap-instruction-template insn))
         (operands (x86-lap-instruction-parsed-operands insn))
         (opcode-modifier (x86::x86-instruction-template-opcode-modifier template))
         (default-seg nil))
    (optimize-displacements operands)
    (when (logtest (x86::encode-opcode-modifier :shortform) opcode-modifier)
      (x86-process-instruction-shortform insn))
    (when (logtest (x86::encode-opcode-modifier :modrm) opcode-modifier)
      (setq default-seg (build-rm-byte insn)))
    (when (and (= (x86-lap-instruction-base-opcode insn) x86::+int-opcode+)
               (= 1 (length operands))
               (let* ((op0 (svref operands 0)))
                 (and (typep op0 'x86::x86-immediate-operand)
                      (eql 3 (early-expression-value
                              (x86::x86-immediate-operand-value op0))))))
      (setf operands nil
            (x86-lap-instruction-parsed-operands insn) nil
            (x86-lap-instruction-base-opcode insn) x86::+int3-opcode+))
    (if (logtest (x86::x86-instruction-template-opcode-modifier template)
                 (x86::encode-opcode-modifier :rex64))
      (setf (x86-lap-instruction-rex insn)
            (logior (or (x86-lap-instruction-rex insn) 0)
                    x86::+rex-mode64+)))
    (let* ((explicit-seg (dotimes (i (length operands))
                           (let* ((op (svref operands i)))
                             (when (typep op 'x86::x86-memory-operand)
                               (let* ((seg (x86::x86-memory-operand-seg op)))
                                 (if seg (return seg))))))))
      (if (and explicit-seg (not (eq explicit-seg default-seg)))
        (add-prefix insn (x86::seg-entry-seg-prefix explicit-seg))))
    (if (x86-lap-instruction-rex insn)
      (add-prefix insn (logior (x86-lap-instruction-rex insn) x86::+rex-opcode+)))
    (let* ((prefixes (x86-lap-instruction-prefixes insn)))
      (cond
        ((logtest (x86::encode-opcode-modifier :jump) opcode-modifier)
         ;; a variable-length pc-relative branch, possibly preceded
         ;; by prefixes (used for branch prediction, mostly.)
         (x86-output-branch frag-list insn))
          (t
           (let* ((base-opcode (x86-lap-instruction-base-opcode insn)))
             (declare (fixnum base-opcode))
             (when (logtest base-opcode #xff0000)
               (add-prefix insn (ldb (byte 8 16) base-opcode)))
             (dotimes (i (length prefixes))
               (let* ((b (aref prefixes i)))
                 (declare (type (unsigned-byte 8) b))
                 (unless (zerop b)
                   (frag-list-push-byte frag-list b))))
             (when (logtest base-opcode #xff00)
               (frag-list-push-byte frag-list (ldb (byte 8 8) base-opcode)))
             (frag-list-push-byte frag-list (ldb (byte 8 0) base-opcode)))
           (let* ((modrm (x86-lap-instruction-rm insn)))
             (when modrm
               (frag-list-push-byte frag-list
                                    (dpb (x86::modrm-byte-mode modrm)
                                         (byte 2 6)
                                         (dpb (x86::modrm-byte-reg modrm)
                                              (byte 3 3)
                                              (ldb (byte 3 0)
                                                   (x86::modrm-byte-regmem modrm)))))
               (let* ((sib (x86-lap-instruction-sib insn)))
                 (when (and (= (x86::modrm-byte-regmem modrm) x86::+escape-to-two-byte-addressing+)
                            (not (= (x86::modrm-byte-mode modrm) 3))
                            sib)
                   (frag-list-push-byte frag-list
                                        (dpb (x86::sib-byte-scale sib)
                                             (byte 2 6)
                                             (dpb (x86::sib-byte-index sib)
                                                  (byte 3 3)
                                                  (ldb (byte 3 0)
                                                       (x86::sib-byte-base sib)))))))))
           (dotimes (i (length operands))
             (let* ((op (svref operands i))
                    (optype (x86::x86-operand-type op)))
               (when (logtest optype (x86::encode-operand-type :disp))
                 (let* ((disp (x86::x86-memory-operand-disp op))
                        (val (early-expression-value disp)))
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
                       (if (logtest optype (x86::encode-operand-type :disp64))
                         (frag-list-push-64 frag-list val)
                         (frag-list-push-32 frag-list val))))))))
           ;; Emit immediate operand(s).
           (dotimes (i (length operands))
             (let* ((op (svref operands i))
                    (optype (x86::x86-operand-type op)))
               (when (logtest optype (x86::encode-operand-type :imm))
                 (let* ((expr (x86::x86-immediate-operand-value op))
                        (val (early-expression-value expr)))
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
                           (frag-list-push-32 frag-list val))))))))))))))

(defun x86-lap-directive (frag-list directive arg)
  (if (eq directive :tra)
    (progn
      (finish-frag-for-align frag-list 3)
      (x86-lap-directive frag-list :long `(- 15 (^ ,arg)))
      (emit-x86-lap-label frag-list arg))
    (let* ((exp (parse-expression arg))
           (constantp (constant-expression-p exp)))
      (if constantp
        (let* ((val (expression-value exp)))
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
                      (label-frag (or (x86-lap-label-frag label)
                                      (progn (describe label)
                                             (error "Label ~s was referenced but not defined" (x86-lap-label-name label)))))
                      (label-address (+ (frag-address label-frag)
                                        (x86-lap-label-offset label)))
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
           (setf (aref buffer pos) b)))
    (flet ((emit-short (buffer pos s)
             (setf (aref buffer pos) (ldb (byte 8 0) s)
                   (aref buffer (1+ pos)) (ldb (byte 8 8) s))))
      (flet ((emit-long (buffer pos l)
               (emit-short buffer pos (ldb (byte 16 0) l))
               (emit-short buffer (+ pos 2) (ldb (byte 16 16) l))))
        (flet ((emit-quad (buffer pos q)
                 (emit-long buffer pos (ldb (byte 32 0) q))
                 (emit-long buffer (+ pos 4) (ldb (byte 32 32) q))))
          (ccl::do-dll-nodes (frag frag-list)
            (let* ((buffer (frag-code-buffer frag))
                   (address (frag-address frag)))
              (dolist (reloc (frag-relocs frag))
                (let* ((pos (reloc-pos reloc))
                       (arg (reloc-arg reloc)))
                  (ecase (reloc-type reloc)
                    (:branch8 (let* ((label (x86::x86-label-operand-label arg))
                                     (target (+ (frag-address (x86-lap-label-frag label))
                                                (x86-lap-label-offset label)))
                                     (refpos (+ address (1+ pos))))
                                (emit-byte buffer pos (- target refpos))))
                    (:branch32 (let* ((label (x86::x86-label-operand-label arg))
                                     (target (+ (frag-address (x86-lap-label-frag label))
                                                (x86-lap-label-offset label)))
                                     (refpos (+ address pos 4)))
                                (emit-long buffer pos (- target refpos))))
                    (:expr8 (emit-byte buffer pos  (expression-value arg)))
                    (:expr16 (emit-short buffer pos (expression-value arg)))
                    (:expr32 (emit-long buffer pos (expression-value arg)))
                    (:expr64 (emit-quad buffer pos (expression-value arg)))))))))))))
                             

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
  (let* ((*x86-lap-instructions* (ccl::make-dll-header))
         (*x86-lap-labels* ())
         (*x86-lap-constants* ())
         (end-code-tag (gensym))
         (instruction (make-x86-lap-instruction))
         (frag-list (make-frag-list)))
    (make-x86-lap-label end-code-tag)
    (make-x86-lap-label '@entry)    
    (x86-lap-directive frag-list :long `(ash (- (^ ,end-code-tag ) 8) -3))
    (x86-lap-directive frag-list :short 0)
    (x86-lap-directive frag-list :short 0)
    (emit-x86-lap-label frag-list '@entry)
    (dolist (f forms)
      (x86-lap-form f frag-list instruction))
    (x86-lap-directive frag-list :align 3)
    (emit-x86-lap-label frag-list end-code-tag)
    (dolist (c *x86-lap-constants*)
      (emit-x86-lap-label frag-list (cdr c))
      (x86-lap-directive frag-list :quad 0))
    (when name
      (x86-lap-directive frag-list :quad 0))
    ;; room for lfun-bits
    (x86-lap-directive frag-list :quad 0)
    (relax-frag-list frag-list)
    (apply-relocs frag-list)
    (fill-for-alignment frag-list)
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