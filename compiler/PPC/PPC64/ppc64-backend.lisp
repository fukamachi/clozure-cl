;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2004, Clozure Associates
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

(defvar *ppc64-vinsn-templates* (make-hash-table :test #'eq))

(defvar *known-ppc64-backends* ())


#+linuxppc-target
(defvar *linuxppc64-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc64-vinsn-templates*
		:p2-template-hash-name '*ppc64-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :eabi-target :linux-target :linuxppc-target :ppc64-target)
		:target-fasl-pathname (make-pathname :type "pfsl64")
		:target-architecture (logior 1 64)
		:target-os :linuxppc
		:name :linuxppc64
		:target-arch-name :ppc64
		:target-foreign-type-data nil
		:target-lisp-node-size 8
                :target-nil-value ppc64::nil-value
                :target-fixnum-shift ppc64::fixnum-shift
                :target-most-positive-fixnum (1- (ash 1 (1- (- 64 ppc64::fixnumshift))))
                :target-nbits-in-word 64
                :target-ntagbits 4
                :target-nlisptagbits 3
                )
  
  )


#+darwinppc-target
(defvar *darwinppc64-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc64-vinsn-templates*
		:p2-template-hash-name '*ppc64-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :darwin-target :darwinppc-target :ppc64-target)
		:target-fasl-pathname (make-pathname :type "dfsl64")
		:target-architecture (logior 2 64)
		:target-os :darwinppc
		:name :darwinppc64
		:target-arch-name :ppc64
		:target-foreign-type-data nil
		:target-lisp-node-size 8
                :target-nil-value ppc64::nil-value
                :target-fixnum-shift ppc64::fixnum-shift
                :target-most-positive-fixnum (1- (ash 1 (1- (- 64 ppc64::fixnumshift))))
                :target-nbits-in-word 64
                :target-ntagbits 4
                :target-nlisptagbits 3))

#+linuxppc-target
(pushnew *linuxppc64-backend* *known-ppc64-backends* :key #'backend-name)


#+darwinppc-target
(pushnew *darwinppc64-backend* *known-ppc64-backends* :key #'backend-name)

(defvar *ppc64-backend* (car *known-ppc64-backends*))

(defun fixup-ppc64-backend ()
  (dolist (b *known-ppc64-backends*)
    (setf (backend-lap-opcodes b) ppc::*ppc-opcodes*
	  (backend-p2-dispatch b) *ppc2-specials*
	  (backend-p2-vinsn-templates b)  *ppc64-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-ppc64-backend)

#+ppc64-target
(setq *host-backend* *ppc64-backend* *target-backend* *ppc64-backend*)

(pushnew *ppc64-backend* *known-backends* :key #'backend-name)

(provide "PPC64-BACKEND")
