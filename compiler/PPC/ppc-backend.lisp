;;;-*- Mode: Lisp; Package: (PPC32 :use CL) -*-
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

(next-nx-defops)
(defvar *ppc2-specials* nil)
(let* ((newsize (%i+ (next-nx-num-ops) 10))
       (old *ppc2-specials*)
       (oldsize (length old)))
  (declare (fixnum newsize oldsize))
  (unless (>= oldsize newsize)
    (let* ((v (make-array newsize :initial-element nil)))
      (dotimes (i oldsize (setq *ppc2-specials* v))
        (setf (svref v i) (svref old i))))))

#+ppc32-target
(require "PPC32-BACKEND")
#+ppc64-target
(require "PPC64-BACKEND")

(defparameter *ppc-backend*
  #+ppc32-target *ppc32-backend*
  #+ppc64-target *ppc64-backend*)

	      
  