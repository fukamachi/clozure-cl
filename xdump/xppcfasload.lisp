;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   Portions copyright (C) 2001-2003 Clozure Associates
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

(eval-when (:compile-toplevel :execute)
  (require "FASLENV" "ccl:xdump;faslenv")
  (require "PPC-LAP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "XFASLOAD" "ccl:xdump;xfasload"))

; This is used to pass the value of lisp_nil into the
; kernel.  The data section will look like:
;  dc.l (ppc32::make-vheader (+ 1024 3) ppc32::subtag-u32-vector)
;  dc.l import_ref_to "set_nil_and_start"
;  dc.l code_address, lisp_nil
;  ds.l 1024  ; lisp_globals
;   ds.b fulltag_nil
; lisp_nil:      ; our TOC anchor
; ...
; which explains what (- (+ ppc32::fulltag-nil 4096 12)) is all about.
(defparameter *ppc-application-entry-function-code*
  (uvref (%define-ppc-lap-function nil '((bctr)))
         0))

(defparameter *ppc-macro-apply-code*
  (uvref
   (%define-ppc-lap-function  ()
                              '((mflr loc-pc)
                                (bla .SPheap-rest-arg)
                                (mtlr loc-pc)
                                (vpop arg_z)
                                (mr arg_y fname)
                                (li arg_x '#.$xnotfun)
                                (set-nargs 3)
                                (ba .SPksignalerr)))
   0
   ))



(defparameter *ppc-closure-trampoline-code*
  (uvref
   (%define-ppc-lap-function  ()
                              '((ba .SPcall-closure)))
   0
   ))




; For now, do this with a UUO so that the kernel can catch it.
(defparameter *ppc-udf-code*
  (uvref (%define-ppc-lap-function nil '((uuo_interr #.ppc32::error-udf-call 0))) 0))





(defparameter *ppc-excised-code*
  (uvref (%define-ppc-lap-function nil '((uuo_interr #.ppc32::error-excised-function-call 0))) 0))

(defparameter *ppc-xload-backend*
  (make-backend-xload-info
   :name (backend-name *host-backend*)
   :application-entry-code *ppc-application-entry-function-code*
   :macro-apply-code *ppc-macro-apply-code*
   :closure-trampoline-code *ppc-closure-trampoline-code*
   :udf-code *ppc-udf-code*
   :excised-code *ppc-excised-code*
   :default-image-name
   #+linuxppc-target "ccl:ccl;ppc-boot"
   #+darwinppc-target "ccl:ccl;ppc-boot.image"
   :default-startup-file-name
   #+linuxppc-target "level-1.pfsl"
   #+darwinppc-target "level-1.dfsl"
   :relativize-subprims-hook 'false
   :prepend-subprims-hook 'false
   :subdir "ccl:level-0;PPC;"
   :compiler-target-name
   #+linuxppc-target :linuxppc
   #+darwinppc-target :darwinppc
))

(add-xload-backend *ppc-xload-backend*)

#+ppc-target
(progn
(setq *xload-default-backend* *ppc-xload-backend*)

(defun Xcompile-directory (dir &optional force)
  (target-xcompile-directory (backend-name *host-backend*) dir  force))

(defun Xcompile-level-0 (&optional force)
  (target-xcompile-level-0 (backend-name *host-backend*) force))

(defun xload-level-0 (&optional (recompile t))
  (target-xload-level-0 (backend-name *host-backend*) recompile))

(defun xload-bootstrap (&optional (recompile t))
  (let* ((bootstrap-backend (copy-backend-xload-info *ppc-xload-backend*)))
    (setf (backend-xload-info-name bootstrap-backend) :ppc-bootstrap
	  (backend-xload-info-default-image-name bootstrap-backend) "ccl:ccl;runtime-boot"
	  (backend-xload-info-default-startup-file-name bootstrap-backend) "runtime.pfsl")
    (add-xload-backend bootstrap-backend)
    (target-xload-level-0 :ppc-bootstrap recompile)))

)
