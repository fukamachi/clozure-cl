;;;-*- Mode: Lisp; Package: (PPC32 :use CL) -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates
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

(next-nx-defops)
(defvar *x862-specials* nil)
(let* ((newsize (%i+ (next-nx-num-ops) 10))
       (old *x862-specials*)
       (oldsize (length old)))
  (declare (fixnum newsize oldsize))
  (unless (>= oldsize newsize)
    (let* ((v (make-array newsize :initial-element nil)))
      (dotimes (i oldsize (setq *x862-specials* v))
        (setf (svref v i) (svref old i))))))

#+x8632-target
(require "X8632-BACKEND")
#+x8664-target
(require "X8664-BACKEND")

(defparameter *x86-backend*
  #+x8632-target *x8632-backend*
  #+x8664-target *x8664-backend*)

	      
(defun fixup-x86-backend (&rest args)
  #+x8632-target (apply #'fixup-x8632-backend args)
  #+x8664-target (apply #'fixup-x8664-backend args))

  