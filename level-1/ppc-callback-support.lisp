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

;;; ppc-callback-support.lisp
;;;
;;; Support for PPC callbacks

;;;sp-callback (sp-eabi-callback for eabi) receives a
;;;callback index in r12; this is an index into the
;;;%pascal-functions% vector (so called because, under 68K MacOS, most
;;;interesting foreign functions were defined in Pascal and followed
;;;Pascal calling conventions.)  Then it funcalls #'%pascal-functions%
;;;with two args, the %pascal-functions% index and a pointer to the
;;;stack frame containing the arguments (tagged as a fixnum).
;;; %pascal-functions% puts the return value in param0 in the stack frame
;;; (which is where its argument pointer was pointing.)

#+ppc-target
(defppclapfunction %get-object ((macptr arg_y) (offset arg_z))
  (check-nargs 2)
  (trap-unless-typecode= arg_y arch::subtag-macptr)
  (macptr-ptr imm0 arg_y)
  (trap-unless-lisptag= arg_z arch::tag-fixnum imm1)
  (unbox-fixnum imm1 arg_z)
  (lwzx arg_z imm0 imm1)
  (blr))

;; It would be awfully nice if (setf (%get-long macptr offset)
;;                                   (ash (the fixnum value) ppc::fixnumshift))
;; would do this inline.
#+ppc-target
(defppclapfunction %set-object ((macptr arg_x) (offset arg_y) (value arg_z))
  (check-nargs 3)
  (trap-unless-typecode= arg_x arch::subtag-macptr)
  (macptr-ptr imm0 arg_x)
  (trap-unless-lisptag= arg_y arch::tag-fixnum imm1)
  (unbox-fixnum imm1 arg_y)
  (stwx arg_z imm0 imm1)
  (blr))


;; This is machine-dependent (it conses up a piece of "trampoline" code
;; which calls a subprim in the lisp kernel.)
(defun make-callback-trampoline (index &optional monitor-exception-ports)
  (declare (ignorable monitor-exception-ports))
  (macrolet ((ppc-lap-word (instruction-form)
               (uvref (uvref (compile nil `(lambda (&lap 0) (ppc-lap-function () ((?? 0)) ,instruction-form))) 0) 0)))
    (let* ((subprim
	    #+eabi-target
	     #.(subprim-name->offset '.SPeabi-callback)
	     #-eabi-target
	     (if monitor-exception-ports
	       #.(subprim-name->offset '.SPcallbackX)
	       #.(subprim-name->offset '.SPcallback)))
           (p (malloc 20)))
      (setf (%get-long p 0) (ppc-lap-word (lis 12 ??))
	    (%get-word p 2) (ash subprim -16)
	    (%get-long p 4) (ppc-lap-word (ori 12 12 ??))
	    (%get-word p 6) (logand #xffff subprim)
            (%get-long p 8) (ppc-lap-word (mtctr 12))
            (%get-long p 12) (logior index (ppc-lap-word (li 11 ??)))   ; unboxed index
	    (%get-word p 14) index
            (%get-long p 16) (ppc-lap-word (bctr)))
      (ff-call (%kernel-import #.arch::kernel-import-makedataexecutable) 
               :address p 
               :unsigned-fullword 20
               :void)
      p)))










; This is called by .SPcallback



; moved to "lib;dumplisp" as restore-pascal-functions

