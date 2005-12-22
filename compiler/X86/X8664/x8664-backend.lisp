;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
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
  (require "X8664ENV"))

(defvar *x8664-vinsn-templates* (make-hash-table :test #'eq))



(defvar *known-x8664-backends* ())


#+(or linuxx86-target (not x86-target))
(defvar *linuxx8664-backend*
  (make-backend #| :lookup-opcode #'lookup-x86-opcode |#
                #| :lookup-macro #'x86::x86-macro-function |#
                #| :lap-opcodes x86::*x86-opcodes* |#
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :linux-target :linuxx86-target :x8664-target
                  :little-endian-target)
		:target-fasl-pathname (make-pathname :type "lx64fsl")
		:target-architecture (logior 5 64)
		:target-os :linuxx86
		:name :linuxx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*
                ))


#+darwinx86-target
(defvar *darwinx8664-backend*
  (make-backend #| :lookup-opcode #'lookup-x86-opcode |#
		#| :lookup-macro #'x86::x86-macro-function |#
		#| :lap-opcodes x86::*x86-opcodes* |#
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :darwin-target :darwinx86-target :x8664-target
                  :little-endian-target)
		:target-fasl-pathname (make-pathname :type "d64fsl")
		:target-architecture (logior 7 64)
		:target-os :darwinx86
		:name :darwinx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*))

#+(or linuxx86-target (not x86-target))
(pushnew *linuxx8664-backend* *known-x8664-backends* :key #'backend-name)


#+darwinx86-target
(pushnew *darwinx8664-backend* *known-x8664-backends* :key #'backend-name)

(defvar *x8664-backend* (car *known-x8664-backends*))

(defun fixup-x8664-backend ()
  (dolist (b *known-x8664-backends*)
    (setf #| (backend-lap-opcodes b) x86::*x86-opcodes* |#
	  (backend-p2-dispatch b) *x862-specials*
	  (backend-p2-vinsn-templates b)  *x8664-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-x8664-backend)

#+x8664-target
(setq *host-backend* *x8664-backend* *target-backend* *x8664-backend*)

#-x8664-target
(unless (backend-target-foreign-type-data *x8664-backend*)
  (let* ((ftd (make-ftd
               :interface-db-directory
               #+darwinx86-target "ccl:darwin-x86-headers64;"
               #+(or linuxx86-target (not x86-target)) "ccl:x86-headers64;"
               :interface-package-name
               #+darwinx86-target "X86-DARWIN64"
               #+(or linuxx86-target (not x86-target)) "X86-LINUX64"
               :attributes
               #+darwinx86-target
               '(:signed-char t
                 :struct-by-value t
                 :prepend-underscores t
                 :bits-per-word  64)
               #+(or linuxx86-target (not x86-target))
               '(:bits-per-word  64))))
    (install-standard-foreign-types ftd)
    (use-interface-dir :libc ftd)
    (setf (backend-target-foreign-type-data *x8664-backend*) ftd)))

(pushnew *x8664-backend* *known-backends* :key #'backend-name)

#+x8664-target
(require "X8664-VINSNS")

(provide "X8664-BACKEND")
