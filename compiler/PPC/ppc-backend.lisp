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
(in-package "CCL")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "PPCENV"))

(defvar *ppc-vinsn-templates* (make-hash-table :test #'eq))

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


(defun lookup-ppc-opcode (name)
  (gethash (string name) ppc32::*ppc-opcode-numbers*))

(defvar *known-ppc-backends* ())


#+linuxppc-target
(defvar *linuxppc-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc32::ppc-macro-function
		:lap-opcodes ppc32::*ppc-opcodes*
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc-vinsn-templates*
		:p2-template-hash-name '*ppc-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :eabi-target :linux-target :linuxppc-target)
		:target-fasl-pathname (make-pathname :type "pfsl")
		:target-architecture 1
		:target-os #+linux-target :linuxppc
		:name :linuxppc
		:target-arch-name :ppc
		:target-foreign-type-data nil)
  )


#+darwinppc-target
(defvar *darwinppc-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc32::ppc-macro-function
		:lap-opcodes ppc32::*ppc-opcodes*
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc-vinsn-templates*
		:p2-template-hash-name '*ppc-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :darwin-target :darwinppc-target)
		:target-fasl-pathname (make-pathname :type "dfsl")
		:target-architecture 3
		:target-os :darwinppc
		:name :darwinppc
		:target-arch-name :ppc
		:target-foreign-type-data nil)
  )

#+linuxppc-target
(pushnew *linuxppc-backend* *known-ppc-backends* :key #'backend-name)


#+darwinppc-target
(pushnew *darwinppc-backend* *known-ppc-backends* :key #'backend-name)

(defvar *ppc-backend* (car *known-ppc-backends*))

(defun fixup-ppc-backend ()
  (dolist (b *known-ppc-backends*)
    (setf (backend-lap-opcodes b) ppc32::*ppc-opcodes*
	  (backend-p2-dispatch b) *ppc2-specials*
	  (backend-p2-vinsn-templates b)  *ppc-vinsn-templates*)))



(fixup-ppc-backend)

#+ppc-target
(setq *host-backend* *ppc-backend* *target-backend* *ppc-backend*)

(pushnew *ppc-backend* *known-backends* :key #'backend-name)

(require "PPC-VINSNS")
(provide "PPC-BACKEND")
