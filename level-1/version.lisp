;;;-*-Mode: LISP; Package: CCL -*-
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

(defparameter *openmcl-major-version* 0)
(defparameter *openmcl-minor-version* 14)
(defparameter *openmcl-revision* 4)
(defparameter *openmcl-suffix* "pre-050821a")
(defparameter *openmcl-dev-level*
  #+ppc64-target "Alpha" #-ppc64-target "Beta")

(defparameter *openmcl-version* (format nil "(~A: ~~A) ~d.~d~@[.~d~]~@[-~a~]"
                                        *openmcl-dev-level*
					*openmcl-major-version*
					*openmcl-minor-version*
					(unless (zerop *openmcl-revision*)
					  *openmcl-revision*)
					*openmcl-suffix*))


; end
