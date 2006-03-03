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


(defun xload-x86-lap-word (instruction-form)
  (uvref (uvref (compile nil
                         `(lambda (&lap 0)
                           (x86-lap-function () ((?? 0))
                            ,instruction-form)))
                  0) #+x8632-target 0 #+x8664-target 1))



(defparameter *x8664-macro-apply-code*
  (let* ((code
          (%define-x86-lap-function
           nil
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
             (jmp-subprim .SPksignalerr)))))
    (if (= (typecode code) target::subtag-xfunction)
      (uvref code 0)
      code)))

(defun x86-fixup-macro-apply-code ()
  (let* ((codev *x86-macro-apply-code*))
    (setf (uvref codev 5)
          (logior (logand #xffff00000 (uvref *x86-macro-apply-code* 5))
                  (target-arch-case
                   (:x8632 (ash $xnotfun x8632::fixnumshift))
                   (:x8664 (ash $xnotfun x8664::fixnumshift)))))
    codev))


(defparameter *x86-closure-trampoline-code*
  (let* ((code '((ba .SPcall-closure))))
    (make-array (length code)
                :element-type '(unsigned-byte 32)
                :initial-contents
                (mapcar #'xload-ppc-lap-word code))))


;;; For now, do this with a UUO so that the kernel can catch it.
(defparameter *ppc-udf-code*
  (let* ((code '((uuo_interr #.arch::error-udf-call 0))))
    (make-array (length code)
                :element-type '(unsigned-byte 32)
                :initial-contents
                (mapcar #'xload-ppc-lap-word code))))




(defparameter *x8664-linux-xload-backend*
  (make-backend-xload-info
   :name  :linuxx8664
   :macro-apply-code-function 'x86-fixup-macro-apply-code
   :closure-trampoline-code *x86-closure-trampoline-code*
   :udf-code *x86-udf-code*
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




