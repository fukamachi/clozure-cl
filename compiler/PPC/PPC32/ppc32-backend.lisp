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
  (require "PPCENV")
  (require "PPC32-ARCH"))

(defvar *ppc32-vinsn-templates* (make-hash-table :test #'eq))

(defparameter *ppc32-target-uvector-subtags*
  `((:simple-vector . ,ppc32::subtag-simple-vector)
    (:bit-vector . ,ppc32::subtag-bit-vector)
    (:simple-string . ,ppc32::subtag-simple-base-string)
    (:u8-vector . ,ppc32::subtag-u8-vector)
    (:s8-vector . ,ppc32::subtag-s8-vector)
    (:u16-vector . ,ppc32::subtag-u16-vector)
    (:s16-vector . ,ppc32::subtag-s16-vector)
    (:u32-vector . ,ppc32::subtag-u32-vector)
    (:s32-vector . ,ppc32::subtag-s32-vector)))

     





(defvar *known-ppc32-backends* ())


#+linuxppc-target
(defvar *linuxppc32-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc32-vinsn-templates*
		:p2-template-hash-name '*ppc32-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :eabi-target :linux-target :linuxppc-target :ppc32-target)
		:target-fasl-pathname (make-pathname :type "pfsl")
		:target-architecture 1
		:target-os :linuxppc
		:name :linuxppc32
		:target-arch-name :ppc32
		:target-foreign-type-data nil
		:target-lisp-node-size 4
                :target-nil-value ppc32::nil-value
                :target-fixnum-shift ppc32::fixnumshift
                :target-most-positive-fixnum (1- (ash 1 (1- (- 32 ppc32::fixnumshift))))
                :target-word-shift 2
                :target-misc-data-offset ppc32::misc-data-offset
                :target-misc-dfloat-offset ppc32::misc-dfloat-offset
                :target-nbits-in-word 32
                :target-ntagbits 3
                :target-nlisptagbits 2
                :target-uvector-subtags *ppc32-target-uvector-subtags*))


#+darwinppc-target
(defvar *darwinppc32-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc32-vinsn-templates*
		:p2-template-hash-name '*ppc32-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :darwin-target :darwinppc-target :ppc32-target)
		:target-fasl-pathname (make-pathname :type "dfsl")
		:target-architecture 3
		:target-os :darwinppc
		:name :darwinppc32
		:target-arch-name :ppc32
		:target-foreign-type-data nil
		:target-lisp-node-size 4
                :target-nil-value ppc32::nil-value
                :target-fixnum-shift ppc32::fixnumshift
                :target-most-positive-fixnum (1- (ash 1 (1- (- 32 ppc32::fixnumshift))))
                :target-word-shift 2
                :target-misc-data-offset ppc32::misc-data-offset
                :target-misc-dfloat-offset ppc32::misc-dfloat-offset
                :target-nbits-in-word 32
                :target-ntagbits 3
                :target-nlisptagbits 2
                :target-uvector-subtags *ppc32-target-uvector-subtags*))

#+linuxppc-target
(pushnew *linuxppc32-backend* *known-ppc32-backends* :key #'backend-name)


#+darwinppc-target
(pushnew *darwinppc32-backend* *known-ppc32-backends* :key #'backend-name)

(defvar *ppc32-backend* (car *known-ppc32-backends*))

(defun fixup-ppc32-backend ()
  (dolist (b *known-ppc32-backends*)
    (setf (backend-lap-opcodes b) ppc::*ppc-opcodes*
	  (backend-p2-dispatch b) *ppc2-specials*
	  (backend-p2-vinsn-templates b)  *ppc32-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-ppc32-backend)

#+ppc32-target
(setq *host-backend* *ppc32-backend* *target-backend* *ppc32-backend*)

(pushnew *ppc32-backend* *known-backends* :key #'backend-name)

(require "PPC32-VINSNS")
(provide "PPC32-BACKEND")
