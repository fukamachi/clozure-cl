;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Opensourced MCL.
;;;
;;;   Opensourced MCL is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Lesser General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2.1 of the License, or (at your option) any later version.
;;;
;;;   Opensourced MCL is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;   Lesser General Public License for more details.
;;;
;;;   You should have received a copy of the GNU Lesser General Public
;;;   License along with this library; if not, write to the Free Software
;;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;
(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "FASLENV" "ccl:xdump;faslenv")
  (require "SPARC-LAP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "XFASLOAD" "ccl:xdump;xfasload"))

;;  The whole "application entry code" idea is vestigal MacOS
;;  stuff: a saved heap image was an application (under MacOS),
;;  and the the entry point called back into the kernel (which
;;  was a MacOS shared library.)
;;  It's unlikely that we're going to do that anywhere but MacOS,
;;  but the xloader wants to set all of this up (before writing
;;  the heap image as a Mac PEF-format application ....)
(defparameter *sparc-application-entry-function-code*
  (uvref (%define-sparc-lap-function nil '((retl) (nop)))
         0))

(defparameter *sparc-macro-apply-code*
  (uvref
   (%define-sparc-lap-function
    ()
    '((mov '#.$xnotfun %arg_y)
      (ld (%nfn (+ arch::misc-data-offset (ash 1 arch::word-shift))) %arg_z)
      (jump-subprim .SPksignalerr)
        (set-nargs 2)))
   0
   ))



(defparameter *sparc-closure-trampoline-code*
  (uvref
   (%define-sparc-lap-function  ()
                              '((jump-subprim .SPcall-closure)
				(nop)))
   0
   ))




; For now, do this with a UUO so that the kernel can catch it.
(defparameter *sparc-udf-code*
  (uvref (%define-sparc-lap-function nil '((uuo_interr #.arch::error-udf-call %rzero))) 0))




(defparameter *sparc-excised-code*
   (uvref (%define-sparc-lap-function nil '((uuo_interr #.arch::error-excised-function-call %rzero))) 0))

(defparameter *sparc-xload-backend*
  (make-backend-xload-info
   :name :SPARC
   :application-entry-code *sparc-application-entry-function-code*
   :macro-apply-code *sparc-macro-apply-code*
   :closure-trampoline-code *sparc-closure-trampoline-code*
   :udf-code *sparc-udf-code*
   :excised-code *sparc-excised-code*
   :default-image-name "ccl:ccl;sparc-boot"
   :default-startup-file-name "level-1.sfsl"
   :relativize-subprims-hook 'false
   :prepend-subprims-hook 'false
   :subdir "ccl:level-0;SPARC;"
   :compiler-target-name :SPARC
))

(add-xload-backend *sparc-xload-backend*)

#+sparc-target
(progn
(setq *xload-default-backend* *sparc-xload-backend*)

(defun Xcompile-directory (dir &optional force)
  (target-xcompile-directory :sparc dir  force))

(defun Xcompile-level-0 (&optional force)
  (target-xcompile-level-0 :sparc force))

(defun xload-level-0 (&optional (recompile t))
  (target-xload-level-0 :sparc recompile))

(defun xload-bootstrap (&optional (recompile t))
  (let* ((bootstrap-backend (copy-backend-xload-info *sparc-xload-backend*)))
    (setf (backend-xload-info-name bootstrap-backend) :sparc-bootstrap
	  (backend-xload-info-default-image-name bootstrap-backend) "ccl:ccl;sparc-runtime-boot"
	  (backend-xload-info-default-startup-file-name bootstrap-backend) "runtime.sfsl")
    (add-xload-backend bootstrap-backend)
    (target-xload-level-0 :sparc-bootstrap recompile)))

)
