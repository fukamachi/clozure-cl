;;; x86-trap-support
;;;
;;;   Copyright (C) 2005-2006 Clozure Associates and contributors
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

;;; Most/all of the callbacks we'd get from the kernel will be in
;;; response to UUOs, and most/all of them should come here.

;;;
(defparameter xcmain nil)

(defun handle-gc-hooks ()
  (let ((bits *gc-event-status-bits*))
    (declare (fixnum bits))
    (cond ((logbitp $gc-postgc-pending-bit bits)
           (setq *gc-event-status-bits*
                 (logand (lognot (ash 1 $gc-postgc-pending-bit))
                         bits))
           (let ((f *post-gc-hook*))
             (when (functionp f) (funcall f)))))))