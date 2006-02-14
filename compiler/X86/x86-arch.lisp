;;;-*- Mode: Lisp; Package: (PPC32 :use CL) -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates and contributors.
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

(defpackage "X86"
  (:use "CL"))

(in-package "X86")

;;; Kernel globals are allocated "below" nil.  This list (used to map
;;; symbolic names to rnil-relative offsets) must (of course) exactly
;;; match the kernel's notion of where things are.
;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" & the
;;; lisp_globals record in "ccl:lisp-kernel;constants.s"
(defparameter *x86-kernel-globals*
  '(get-tcr				; callback to obtain (real) tcr
    tcr-count
    interrupt-signal			; used by PROCESS-INTERRUPT
    kernel-imports                      ; some things we need to have imported for us.
    tcr-lock
    emulator-registers                  ; Where the 68K registers are kept.
    appmain                             ; application's (c-runtime) main() function
    subprims-base                       ; start of dynamic subprims jump table
    ret1valaddr                         ; magic multiple-values return address.
    tcr-key                             ; tsd key for thread's tcr
    area-lock                           ; serialize access to gc
    exception-lock			; serialize exception handling
    deleted-static-pairs                ; hash-consing
    default-allocation-quantum          ; log2_heap_segment_size, as a fixnum.
    intflag				; interrupt-pending flag
    gc-inhibit-count                    ; for gc locking
    refbits                             ; oldspace refbits
    oldspace-dnode-count                ; number of dnodes in dynamic space that are older than
                                        ; youngest generation
    altivec-present                     ; non-zero if cpu supports AltiVec 
    fwdnum                              ; fixnum: GC "forwarder" call count.
    gc-count                            ; fixnum: GC call count.
    gcable-pointers                     ; linked-list of weak macptrs.
    heap-start                          ; start of lisp heap
    heap-end                            ; end of lisp heap
    statically-linked                   ; true if the lisp kernel is statically linked
    bad-current-vs                      ; current value-stack area
    bad-current-ts                      ; current temp-stack area
    bad-funcall                         ; pseudo-target for funcall
    all-areas                           ; doubly-linked area list
    lexpr-return                        ; multiple-value lexpr return address
    lexpr-return1v                      ; single-value lexpr return address
    in-gc                               ; non-zero when GC-ish thing active
    metering-info                       ; kernel metering structure
    doh-head                            ; creole
    short-float-zero                    ; low half of 1.0d0
    double-float-one                    ; high half of 1.0d0
    ffi-exception                       ; ffi fpscr[fex] bit
    exception-saved-registers           ; saved registers from exception frame
    oldest-ephemeral                    ; doublenode address of oldest ephemeral object or 0
    tenured-area                        ; the tenured_area.
    errno                               ; address of C lib errno
    argv                                ; address of C lib argv
    host-platform                       ; 0 on MacOS, 1 on PPC Linux, 2 on VxWorks ...
    batch-flag				; non-zero if --batch specified
    BAD-fpscr-save			; lisp's fpscr when in FFI-land
    BAD-fpscr-save-high  		; high word of FP reg used to save FPSCR
    image-name				; current image name
    initial-tcr                         ; initial thread's context record
    ))

;;; The order here matches "ccl:lisp-kernel;lisp_globals.h" and the nrs record
;;; in "ccl:lisp-kernel;constants.s".
(defparameter *x86-nilreg-relative-symbols*
  '(t
    nil
    ccl::%err-disp
    ccl::cmain
    eval
    ccl::apply-evaluated-function
    error    
    ccl::%defun
    ccl::%defvar
    ccl::%defconstant
    ccl::%macro
    ccl::%kernel-restart
    *package*
    ccl::*total-bytes-freed*
    :allow-other-keys    
    ccl::%toplevel-catch%
    ccl::%toplevel-function%
    ccl::%pascal-functions%    
    ccl::*all-metered-functions*
    ccl::*total-gc-microseconds*
    ccl::%builtin-functions%
    ccl::%unbound-function%
    ccl::%init-misc
    ccl::%macro-code%
    ccl::%closure-code%
    ccl::%new-gcable-ptr
    ccl::*gc-event-status-bits*
    ccl::*post-gc-hook*
    ccl::%handlers%
    ccl::%all-packages%
    ccl::*keyword-package* 
    ccl::%finalization-alist%
    ccl::%foreign-thread-control
    ))

(ccl::defenum (:prefix "AREA-")
  void                                  ; list header
  cstack                                ; a control stack
  vstack                                ; a value stack
  tstack                                ; (dynamic-extent) temp stack
  readonly                              ; readonly section
  managed-static                        ; growable static area
  static                                ; static data in application
  dynamic                               ; dynmaic (heap) data in application
)

;;; areas are sorted such that (in the "succ" direction) codes are >=.
;;; If you think that you're looking for a stack (instead of a heap), look
;;; in the "pred" direction from the all-areas header.
(defconstant max-stack-area-code area-tstack)
(defconstant min-heap-area-code area-readonly)

;;; mxcsr bits.  (Unlike the convention used on the PPC, bit 0 is the
;;; least significant bit of the containing byte/word.)

(ccl::defenum (:prefix "MXCSR-" :suffix "-BIT")
  ie                                    ;invalid exception
  de                                    ;denormal exception
  ze                                    ;divide-by-zero exception
  oe                                    ;overflow exception
  ue                                    ;underflow exception
  pe                                    ;precision exception
  daz                                   ;denorms-are-zeros (not-IEEE)
  im                                    ;invalid masked
  dm                                    ;denormals masked
  zm                                    ;divide-by-zero masked
  om                                    ;overflow masked
  um                                    ;underflow masked
  pm                                    ;precision masked
  rc0                                   ;rounding control bit 0
  rc1                                   ;rounding control bit 1
  fz                                    ;flush-to-zero (not-IEEE)
)

(defconstant mxcsr-status-byte (byte 6 0))
(defconstant mxcsr-control-bit-shift 7)
(defconstant mxcsr-control-byte (byte 6 mxcsr-control-bit-shift))
(defconstant mxcsr-control-and-rounding-byte (byte 8 mxcsr-control-bit-shift))

(defconstant mxcsr-write-mask (dpb -1 mxcsr-status-byte
                                   (dpb -1 mxcsr-control-and-rounding-byte 0)))

;;; Condition bitfields, used in jcc, cmovcc, setcc.
(defconstant x86-o-bits #x0)
(defconstant x86-no-bit #x1)
(defconstant x86-b-bits #x2)
(defconstant x86-ae-bits #x3)
(defconstant x86-e-bits #x4)
(defconstant x86-ne-bits #x5)
(defconstant x86-be-bits #x6)
(defconstant x86-a-bits #x7)
(defconstant x86-s-bits #x8)
(defconstant x86-ns-bits #x9)
(defconstant x86-pe-bits #xa)
(defconstant x86-po-bits #xb)
(defconstant x86-l-bits #xc)
(defconstant x86-ge-bits #xd)
(defconstant x86-le-bits #xe)
(defconstant x86-g-bits #xf)



(provide "X86-ARCH")
