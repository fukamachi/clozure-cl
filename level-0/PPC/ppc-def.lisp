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


;;; Do an FF-CALL to MakeDataExecutable so that the data cache gets flushed.
;;; If the GC moves this function while we're trying to flush the cache,
;;; it'll flush the cache: no harm done in that case.
(defppclapfunction %make-code-executable ((codev arg_z))
  (let ((len temp2)
	(word-offset imm0))
    (save-lisp-context)
    (getvheader word-offset codev)
    (header-length len word-offset)
    ;; This is going to have to pass a lisp object to a foreign function.
    ;; If we ever have a preemptive scheduler, we'd better hope that
    ;; WITHOUT-INTERRUPTS refuses to run lisp code from a callback.
    (stwu sp (- (+ #+linuxppc-target ppc::eabi-c-frame.minsize
		   #+darwinppc-target ppc::c-frame.minsize ppc::lisp-frame.size)) sp)	; make an FFI frame.
    (la imm0 arch::misc-data-offset codev)
    (stw imm0 #+linuxppc-target ppc::eabi-c-frame.param0 #+darwinppc-target ppc::c-frame.param0  sp)
    (stw len #+linuxppc-target ppc::eabi-c-frame.param1 #+darwinppc-target ppc::c-frame.param1 sp)
    (ref-global imm3 kernel-imports)
    (lwz arg_z arch::kernel-import-MakeDataExecutable imm3)
    (bla #+linuxppc-target .SPeabi-ff-call #+darwinppc-target .SPffcall)
    (li arg_z ppc::nil-value)
    (restore-full-lisp-context)
    (blr)))

; end of ppc-def.lisp
