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

(defvar *lap-labels* ())
(defvar *lap-instructions* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "DLL-NODE"))


(eval-when (:execute :load-toplevel)
  (defstruct (instruction-element (:include ccl::dll-node))
    address)

  (defstruct (lap-instruction (:include instruction-element)
                                  (:constructor %make-lap-instruction (opcode)))
    template
    parsed-operands                     ;NIL, or a 1, 2, or 3-element vector
    size                                ;size in bytes, if known
    min-size                            ;minimum size in bytes
    max-size                            ;maximum size in bytes
    (prefixes 0)                        ;bitmap of prefixes
    
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