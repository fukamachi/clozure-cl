;;; -*- Mode: Lisp; Package: CCL -*-
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

#+allow-in-package
(in-package "CCL")

(defun pdbg (string)
  (%string-to-stderr string)
  (%string-to-stderr #.(string  #-macos-target  #\LineFeed #+macos-target #\Return)))

; set a debugger breakpoint on this dude
(defun breaker (&rest rest)
  (declare (ignore rest))
  nil)
