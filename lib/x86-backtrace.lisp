;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2006 Clozure Associates and contributors
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


;;; Returns two values:
;;;  [nil, nil] if it can be reliably determined that function uses no registers at PC
;;;  [mask, saved-location]  if it can be reliably determined that the registers specified by "mask"
;;;      were saved at "saved-location" in the function's stack frame
;;;  [mask, nil] if registers in "mask" MAY have been saved, but we don't know how to restore them
;;;      (perhaps because the "at-pc" argument wasn't specified.


(defun registers-used-by (function &optional at-pc)
  (multiple-value-bind (mask stack-location rpc)
      (%function-register-usage function)
    (if (null mask)
      (values nil nil)
      (values mask (if (and at-pc rpc (> at-pc rpc)) stack-location)))))
