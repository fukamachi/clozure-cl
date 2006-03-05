;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   Portions copyright (C) 2001-2006 Clozure Associates
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
  (require "X86-LAP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "XFASLOAD" "ccl:xdump;xfasload"))

(defun xload-x8664-lap-code (instructions)
  (let* ((f (%define-x86-lap-function nil instructions)))
    (if (= (typecode f) target::subtag-xfunction)
      (uvref f 0)
      f)))

(defparameter *x8664-macro-apply-code*
  (xload-x8664-lap-code
   '((movzwl (% nargs) (% imm0.l))
     (subq ($ (ash $numx8664argregs x8664::word-shift)) (% imm0))
     (jle @simple)
     (movq (% rbp) (@ (% rsp) (% imm0)))
     (leaq (@ (% rsp) (% imm0)) (% rbp))
     (movq (% ra0) (@ 8 (% rbp)))
     (jmp @did-frame)
     @simple
     (pushq (% ra0))
     (pushq (% rbp))
     (movq (% rsp) (% rbp))
     @did-frame
     (call-subprim .SPheap-rest-arg)
     (pop (% arg_z))
     (movq (% fname) (% arg_y))
     (movq ($ '$xnotfun) (% arg_x))
     (set-nargs 3)
     (jmp-subprim .SPksignalerr))))


(defun x8664-fixup-macro-apply-code ()
  *x8664-macro-apply-code*)


(defparameter *x8664-closure-trampoline-code*
  (xload-x8664-lap-code '((jmp-subprim  .SPcall-closure))))



;;; For now, do this with a UUO so that the kernel can catch it.
(defparameter *x8664-udf-code*
  (xload-x8664-lap-code '((uuo-error-udf-call))))


(defparameter *x8664-linux-xload-backend*
  (make-backend-xload-info
   :name  :linuxx8664
   :macro-apply-code-function 'x8664-fixup-macro-apply-code
   :closure-trampoline-code *x8664-closure-trampoline-code*
   :udf-code *x8664-udf-code*
   :default-image-name "ccl:ccl;x86-boot64"
   :default-startup-file-name "level-1.lx64fsl"
   :subdirs '("ccl:level-0;X86;X8664;" "ccl:level-0;X86;")
   :compiler-target-name :linuxx8664
   :image-base-address #x100000000))

(add-xload-backend *x8664-linux-xload-backend*)



#+x8664-target
(progn
  #+linux-target
  (setq *xload-default-backend* *x8664-linux-xload-backend*))




