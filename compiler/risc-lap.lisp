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

;; Lap data structures & some generic code (at least for RISC backends.)

(in-package "CCL")

(defvar *lap-labels* ())
(defvar *lap-instructions* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARCH")
  (require "DLL-NODE")
  (require "SUBPRIMS"))

(eval-when (:execute :load-toplevel)
  (defstruct (instruction-element (:include dll-node))
    address)

  (defstruct (lap-instruction (:include instruction-element)
                                  (:constructor %make-lap-instruction (opcode)))
    opcode
    parsed-operands
    )

  (defstruct (lap-note (:include instruction-element))
    peer
    id)

  (defstruct (lap-note-begin (:include lap-note)))
  (defstruct (lap-note-end (:include lap-note)))
    
  (defstruct (lap-label (:include instruction-element)
                            (:constructor %%make-lap-label (name)))
    name
    refs))

(def-standard-initial-binding *lap-label-freelist* (make-dll-node-freelist))
(def-standard-initial-binding *lap-instruction-freelist* (make-dll-node-freelist))

(def-standard-initial-binding *operand-vector-freelist* (%cons-pool))

(defconstant lap-operand-vector-size #+ppc-target 5)

(defun alloc-lap-operand-vector (&optional (size lap-operand-vector-size))
  (declare (fixnum size))
  (if (eql size lap-operand-vector-size)
    (without-interrupts 
     (let* ((freelist  *operand-vector-freelist*)
            (v (pool.data freelist)))
       (if v
         (progn
           (setf (pool.data freelist) 
                 (svref v 0))
           (%init-misc nil v)
           v)
         (make-array lap-operand-vector-size  :initial-element nil))))
    (make-array size :initial-element nil)))

(defun free-lap-operand-vector (v)
  (when (= (length v) lap-operand-vector-size)
    (without-interrupts 
     (setf (svref v 0) (pool.data *operand-vector-freelist*)
           (pool.data *operand-vector-freelist*) nil))))

(defun %make-lap-label (name)
  (let* ((lab (alloc-dll-node *lap-label-freelist*)))
    (if lab
      (progn
        (setf (lap-label-address lab) nil
              (lap-label-refs lab) nil
              (lap-label-name lab) name)
        lab)
      (%%make-lap-label name))))

(defun make-lap-instruction (opcode)
  (let* ((insn (alloc-dll-node *lap-instruction-freelist*)))
    (if (typep insn 'lap-instruction)
      (progn
        (setf (lap-instruction-address insn) nil
              (lap-instruction-parsed-operands insn) nil
              (lap-instruction-opcode insn) opcode)
        insn)
      (%make-lap-instruction opcode))))

(defmacro do-lap-labels ((lab &optional result) &body body)
  (let* ((thunk-name (gensym))
         (k (gensym))
         (xlab (gensym)))
    `(flet ((,thunk-name (,lab) ,@body))
      (if (listp *lap-labels*)
        (dolist (,xlab *lap-labels*)
          (,thunk-name ,xlab))
        (maphash #'(lambda (,k ,xlab)
                     (declare (ignore ,k))
                     (,thunk-name ,xlab))
                 *lap-labels*))
      ,result)))

(defun make-lap-label (name)
  (let* ((lab (%make-lap-label name)))
    (if (typep *lap-labels* 'hash-table)
      (setf (gethash name *lap-labels*) lab)
      (progn
        (push lab *lap-labels*)
        (if (> (length *lap-labels*) 255)
          (let* ((hash (make-hash-table :size 512 :test #'eq)))
            (dolist (l *lap-labels* (setq *lap-labels* hash))
              (setf (gethash (lap-label-name l) hash) l))))))
    lab))

(defun find-lap-label (name)
  (if (typep *lap-labels* 'hash-table)
    (gethash name *lap-labels*)
    (car (member name *lap-labels* :test #'eq :key #'lap-label-name))))

(defun lap-note-label-reference (labx insn)
  '(unless (and labx (symbolp labx))
    (error "Label names must be symbols; otherwise, all hell might break loose."))
  (let* ((lab (or (find-lap-label labx)
                  (make-lap-label labx))))
    (push insn (lap-label-refs lab))
    lab))

;;; A label can only be emitted once.  Once it's been emitted, its pred/succ
;;; slots will be non-nil.

(defun lap-label-emitted-p (lab)
  (not (null (lap-label-pred lab))))


(defun emit-lap-label (name)
  (let* ((lab (find-lap-label name)))
    (if  lab 
      (when (lap-label-emitted-p lab)
        (error "Label ~s: multiply defined." name))
      (setq lab (make-lap-label name)))
    (append-dll-node lab *lap-instructions*)))

(defun emit-lap-note (note)
  (append-dll-node note *lap-instructions*))

(provide "RISC-LAP")

