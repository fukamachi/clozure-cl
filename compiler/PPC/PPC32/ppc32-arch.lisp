;;;-*- Mode: Lisp; Package: (PPC32 :use CL) -*-
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


;; This file matches "ccl:pmcl;constants.h" & "ccl:pmcl;constants.s"

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARCH"))


(in-package "PPC32")



;; PPC-32 stuff and tags.
(eval-when (:compile-toplevel :load-toplevel :execute)


;;; Lisp registers.
(eval-when (:compile-toplevel :execute)
  (defmacro defregs (&body regs)
    `(progn
       (ccl::defenum () ,@regs)
       (defparameter *gpr-register-names* ,(coerce (mapcar #'string regs) 'vector))))
  (defmacro deffpregs (&body regs)
    `(progn
       (ccl::defenum () ,@regs)
       (defparameter *fpr-register-names* ,(coerce (mapcar #'string regs) 'vector))))
  (defmacro defvregs (&body regs)
    `(progn
      (ccl::defenum () ,@regs)
      (defparameter *vector-register-names* ,(coerce (mapcar #'string regs) 'vector))
      )))

(defregs
    rzero				; Always contains 0; not as handy as it sounds.
    sp					; The control stack.  Aligned on 16-byte boundary.
  rcontext				; thread context
  imm0                                  ; Unboxed, volatile registers.
  imm1 
  imm2 
  imm3 
  imm4
  imm5
  allocptr
  allocbase
  nargs                                 ; Volatile.  SHOULDN'T be used for tag extraction. (TWI handler confusion.)
  tsp                                   ; Temp-stack pointer.
  vsp                                   ; Value stack pointer; grows towards 0.
  loc-pc                                ; for return PC only.
  fn					; Current function (constants vector).
  temp4                                 ; Boxed, volatile registers.  Some
					; may be defined on function entry.
  temp3
  temp2 
  temp1 
  temp0 
  arg_x                                 ; Next-to-next-to-last function arg.
  arg_y                                 ; Next-to-last function argument.
  arg_z                                 ; Last function argument.
  save7                                 ; Boxed, nonvolatile registers.
  save6
  save5
  save4 
  save3 
  save2 
  save1 
  save0
  )

(defconstant nil-value #x00002015)

(deffpregs 
  fp0
  fp1
  fp2
  fp3
  fp4
  fp5
  fp6
  fp7
  fp8
  fp9
  fp10
  fp11
  fp12
  fp13
  fp14
  fp15
  fp16
  fp17
  fp18
  fp19
  fp20
  fp21
  fp22
  fp23
  fp24
  fp25
  fp26
  fp27
  fp28
  fp29
  fp30
  fp31)

(defvregs
  vr0					; General temp vector register
  vr1					; Most-significant quadword when word-aligning
  vr2					; Least-significant quadword when word-aligning
  vr3					; Operand A resulting from word-aligning
  vr4					; Operand B resulting from word-aligning
  vr5					; Result from operations on A and B
  vr6
  vr7
  vr8
  vr9
  vr10
  vr11
  vr12
  vr13
  vr14
  vr15
  vr16
  vr17
  vr18
  vr19
  ;;By convention, registers after this point are considered non-volatile. Callee should save.
  vr20
  vr21
  vr22
  vr23
  vr24
  vr25
  vr26
  vr27					; Permutation control register A for loads
  vr28					; Permutation control register B for stores
  vr29					; mask register
  vr30					; All zeros
  vr31					; All ones
  )


(defconstant fname temp3)

;;; Calling sequence may pass additional arguments in temp registers.
;;; "nfn" (new function) is always passed; it's the new value of "fn".
(defconstant nfn temp2)

;;; CLOS may pass the context for, e.g.., CALL-NEXT-METHOD in 
;;;; the "next-method-context" register.
(defconstant next-method-context temp1)


;;; It's handy to have 0.0 in an fpr.
(defconstant fp-zero fp31)

; Also handy to have #x4330000080000000 in an fpr, for s32->float conversion.
(defconstant fp-s32conv fp30)




(defconstant max-64-bit-constant-index (ash (+ #x7fff ppc32::misc-dfloat-offset) -3))
(defconstant max-32-bit-constant-index (ash (+ #x7fff ppc32::misc-data-offset) -2))
(defconstant max-16-bit-constant-index (ash (+ #x7fff ppc32::misc-data-offset) -1))
(defconstant max-8-bit-constant-index (+ #x7fff ppc32::misc-data-offset))
(defconstant max-1-bit-constant-index (ash (+ #x7fff ppc32::misc-data-offset) 5))


; The objects themselves look something like this:


;; For the eabi port: mark this stack frame as Lisp's (since EABI
;; foreign frames can be the same size as a lisp frame.)


(ppc32::define-storage-layout lisp-frame 0
  backlink
  savefn
  savelr
  savevsp
)

(ppc32::define-storage-layout c-frame 0
  backlink
  crsave
  savelr
  unused-1
  unused-2
  savetoc
  param0
  param1
  param2
  param3
  param4
  param5
  param6
  param7
)

(defconstant c-frame.minsize c-frame.size)

;; .SPeabi-ff-call "shrinks" this frame after loading the GPRs.
(ppc32::define-storage-layout eabi-c-frame 0
  backlink
  savelr
  param0
  param1
  param2
  param3
  param4
  param5
  param6
  param7
)

(defconstant eabi-c-frame.minsize eabi-c-frame.size)






(ccl::defenum (:prefix "PPC-" :suffix "-BIT")
  lt
  gt
  eq
  so
)


(ccl::defenum (:prefix "FPSCR-" :suffix "-BIT")
  fx
  fex
  vx
  ox
  ux
  zx
  xx
  vxsnan
  vxisi
  vxidi
  vxzdz
  vximz
  vxvc
  fr
  fi
  fprfc
  fl
  fg
  fe
  fu
  nil
  vxsoft
  vxsqrt
  vxcvi
  ve
  oe
  ue
  ze
  xe
  ni
  rn0
  rn1
)

(defconstant yield-syscall
  #+darwinppc-target -60
  #+linuxppc-target #$__NR_sched_yield)
)

(provide "PPC32-ARCH")
