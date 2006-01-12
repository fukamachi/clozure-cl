;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2004, 2005 Clozure Associates
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
                :define-vinsn 'define-ppc-vinsn
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc64-vinsn-templates*
		:p2-template-hash-name '*ppc64-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :poweropen-target :linux-target :linuxppc-target :ppc64-target)
		:target-fasl-pathname (make-pathname :type "p64fsl")
		:target-platform (logior platform-cpu-ppc
                                         platform-os-linux
                                         platform-word-size-64)
		:target-os :linuxppc
		:name :linuxppc64
		:target-arch-name :ppc64
		:target-foreign-type-data nil
                :target-arch ppc64::*ppc64-target-arch*
                ))


#+darwinppc-target
(defvar *darwinppc64-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
                :define-vinsn 'define-ppc-vinsn
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc64-vinsn-templates*
		:p2-template-hash-name '*ppc64-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :darwin-target :darwinppc-target :ppc64-target)
		:target-fasl-pathname (make-pathname :type "d64fsl")
		:target-platform (logior platform-cpu-ppc
                                         platform-os-darwin
                                         platform-word-size-64)
		:target-os :darwinppc
		:name :darwinppc64
		:target-arch-name :ppc64
		:target-foreign-type-data nil
                :target-arch ppc64::*ppc64-target-arch*))

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
#-ppc64-target
(unless (backend-target-foreign-type-data *ppc64-backend*)
  (let* ((ftd (make-ftd
               :interface-db-directory
               #+darwinppc-target "ccl:darwin-headers64;"
               #+linuxppc-target "ccl:headers64;"
               :interface-package-name
               #+darwinppc-target "DARWIN64"
               #+linuxppc-target "LINUX64"
               :attributes
               #+darwinppc-target
               '(:signed-char t
                 :struct-by-value t
                 :prepend-underscores t
                 :bits-per-word  64)
               #+linuxppc-target
               '(:bits-per-word  64))))
    (install-standard-foreign-types ftd)
    (use-interface-dir :libc ftd)
    (setf (backend-target-foreign-type-data *ppc64-backend*) ftd)))
  
(pushnew *ppc64-backend* *known-backends* :key #'backend-name)

#+ppc64-target
(require "PPC64-VINSNS")

(provide "PPC64-BACKEND")
