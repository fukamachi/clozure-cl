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
(defpackage "PPC32"
  (:nicknames "ARCH32" "ARCH" "PPC")
  (:use "CL"))

(in-package "PPC32")



;; PPC-32 stuff and tags.
(eval-when (:compile-toplevel :load-toplevel :execute)









; The objects themselves look something like this:




  


(defconstant tcr-flag-bit-foreign 0)
(defconstant tcr-flag-bit-awaiting-preset 1)










)

(defmacro make-vheader (element-count subtag)
  `(logior ,subtag (ash ,element-count 8)))

(defmacro ppc-fixnum (val)
  `(ash ,val fixnum-shift))

(defmacro unbox-ppc-fixnum (f)
  `(ash ,f (- fixnum-shift)))


; Kernel globals are allocated "below" nil.  This list (used to map symbolic names to
; rnil-relative offsets) must (of course) exactly match the kernel's notion of where 
; things are.
; The order here matches "ccl:pmcl;lisp_globals.h" & the lisp_globals record
; in "ccl:pmcl;constants.s"
(defparameter *ppc-kernel-globals*
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
    gc-lock                             ; serialize access to gc
    exception-lock			; serialize exception handling
    go-tag-counter        		; counter for (immediate) go tag
    block-tag-counter                   ; counter for (immediate) block tag
    intflag				; interrupt-pending flag
    gc-inhibit-count                    ; for gc locking
    os-trap-call                        ; callostrapunivesalproc's descriptor
    tb-trap-call                        ; CallUniversalProc's descriptor
    qd-globals                          ; (untagged) pointer to QD globals.
    fwdnum                              ; fixnum: GC "forwarder" call count.
    gc-count                            ; fixnum: GC call count.
    gcable-pointers                     ; linked-list of weak macptrs.
    heap-start                          ; start of lisp heap
    heap-end                            ; end of lisp heap
    bad-current-cs                      ; current control-stack area
    bad-current-vs                      ; current value-stack area
    bad-current-ts                      ; current temp-stack area
    bad-cs-overflow-limit               ; limit for control-stack overflow check
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
    oldest-ephemeral                    ; doubleword address of oldest ephemeral object or 0
    tenured-area                        ; the tenured_area.
    errno                               ; address of C lib errno
    argv                                ; address of C lib argv
    host-platform                       ; 0 on MacOS, 1 on PPC Linux, 2 on VxWorks ...
    batch-flag				; non-zero if --batch specified
    fpscr-save				; lisp's fpscr when in FFI-land
    fpscr-save-high			; high word of FP reg used to save FPSCR
    image-name				; current image name
    initial-tcr                         ; initial thread's context record
    ))

(defun %kernel-global (sym)
  (let* ((pos (position sym *ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-nil (* (1+ pos) 4)))
      (error "Unknown kernel global : ~s ." sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym *ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-nil (* (1+ pos) 4)))
      (error "Unknown kernel global : ~s ." sym))))


; The kernel imports things that are defined in various other libraries for us.
; The objects in question are generally fixnum-tagged; the entries in the
; "kernel-imports" vector are 4 bytes apart.
(ccl::defenum (:prefix "KERNEL-IMPORT-" :start 0 :step 4)
  fd-setsize-bytes
  do-fd-set
  do-fd-clr
  do-fd-is-set
  do-fd-zero
  MakeDataExecutable
  GetSharedLibrary
  FindSymbol
  malloc
  free
  allocate_tstack
  allocate_vstack
  register_cstack
  condemn-area
  metering-control
  restore-soft-stack-limit
  egc-control
  lisp-bug
  NewThread
  YieldToThread
  DisposeThread
  ThreadCurrentStackSpace
  usage-exit
  save-fp-context
  restore-fp-context
  put-altivec-registers
  get-altivec-registers
  new-semaphore
  wait-on-semaphore
  signal-semaphore
  destroy-semaphore
  new-recursive-lock
  lock-recursive-lock
  unlock-recursive-lock
  destroy-recursive-lock
  suspend-other-threads
  resume-other-threads
  suspend-tcr
  resume-tcr
  rwlock-new
  rwlock-destroy
  rwlock-rlock
  rwlock-wlock
  rwlock-unlock
  recursive-lock-trylock
  foreign-name-and-offset
)

(defmacro define-header (name element-count subtag)
  `(defconstant ,name (logior (ash ,element-count num-subtag-bits) ,subtag)))

(define-header single-float-header single-float.element-count subtag-single-float)
(define-header double-float-header double-float.element-count subtag-double-float)
(define-header one-digit-bignum-header 1 subtag-bignum)
(define-header two-digit-bignum-header 2 subtag-bignum)
(define-header symbol-header symbol.element-count subtag-symbol)
(define-header value-cell-header value-cell.element-count subtag-value-cell)
(define-header macptr-header macptr.element-count subtag-macptr)


;; Error numbers, as used in UU0s and such.
;; These match "ccl:pmcl;errors.h" & "ccl:pmcl;errors.s"

(defconstant error-reg-regnum 0)        ; "real" error number is in RB field of UU0.
                                        ; Currently only used for :errchk in emulated traps
                                        ; The errchk macro should expand into a check-trap-error vinsn, too.
(defconstant error-udf 1)               ; Undefined function (reported by symbol-function)
(defconstant error-udf-call 2)          ; Attempt to call undefined function
(defconstant error-throw-tag-missing 3)
(defconstant error-alloc-failed 4)      ; can't allocate (largish) vector
(defconstant error-stack-overflow 5)    ; some stack overflowed.
(defconstant error-excised-function-call 6)     ; excised function was called.
(defconstant error-too-many-values 7)   ; too many values returned
(defconstant error-cant-take-car 8)
(defconstant error-cant-take-cdr 9)
(defconstant error-cant-call 17)        ; Attempt to funcall something that is not a symbol or function.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant error-type-error 64)
)

(defconstant error-fpu-exception-double 1024)   ; FPU exception, binary double-float op
(defconstant error-fpu-exception-single 1025)

(defconstant error-memory-full 2048)

;; These are now supposed to match (mod 64) the %type-error-typespecs%
;; array that %err-disp looks at.
(ccl::defenum (:start  error-type-error :prefix "ERROR-OBJECT-NOT-")
  array
  bignum
  fixnum
  character
  integer
  list
  number
  sequence
  simple-string
  simple-vector
  string
  symbol
  macptr
  real
  cons
  unsigned-byte
  radix
  float  
  rational
  ratio
  short-float
  double-float
  complex
  vector
  simple-base-string
  function
  unsigned-byte-16
  unsigned-byte-8
  unsigned-byte-32
  signed-byte-32
  signed-byte-16
  signed-byte-8
  base-char
  bit
  unsigned-byte-24
  )

; The order here matches "ccl:pmcl;lisp_globals.h" and the nrs record
; in "ccl:pmcl;constants.s".
(defparameter *ppc-nilreg-relative-symbols*
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

(defmacro nrs-offset (name)
  (let* ((pos (position name *ppc-nilreg-relative-symbols* :test #'eq)))
    (if pos (+ t-offset (* pos symbol.size)))))

(defun builtin-function-name-offset (name)
  (and name (position name ccl::%builtin-functions% :test #'eq)))

(ccl::defenum ()
  storage-class-lisp                    ; General lisp objects
  storage-class-imm                     ; Fixnums, chars, NIL: not relocatable
  storage-class-wordptr                 ; "Raw" (fixnum-tagged) pointers to stack,etc
  storage-class-u8                      ; Unsigned, untagged, 8-bit objects
  storage-class-s8                      ; Signed, untagged, 8-bit objects
  storage-class-u16                     ; Unsigned, untagged, 16-bit objects
  storage-class-s16                     ; Signed, untagged, 16-bit objects
  storage-class-u32                     ; Unsigned, untagged, 8-bit objects
  storage-class-s32                     ; Signed, untagged, 8-bit objects
  storage-class-address                 ; "raw" (untagged) 32-bit addresses.
  storage-class-single-float            ; 32-bit single-float objects
  storage-class-double-float            ; 64-bit double-float objects
  storage-class-pc                      ; pointer to/into code vector
  storage-class-locative                ; pointer to/into node-misc object
  storage-class-crf                     ; condition register field
  storage-class-crbit                   ; condition register bit: 0-31
  storage-class-crfbit                  ; bit within condition register field : 0-3
)

;; For assembly/disassembly, at least on RISC platforms.
(defstruct opcode 
  (name (error "Opcode name must be present") :type (or string symbol))
  (opcode 0 :type (unsigned-byte 32))
  (majorop 0 :type (unsigned-byte 6))
  (mask #xffffffff :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 32))
  (operands () :type list)
  (min-args 0 :type (unsigned-byte 3))
  (max-args 0 :type (unsigned-byte 3))
  (op-high 0 :type (unsigned-byte 16))
  (op-low 0 :type (unsigned-byte 16))
  (mask-high #xffff :type (unsigned-byte 16))
  (mask-low #xffff :type (unsigned-byte 16))
  (vinsn-operands () :type list)
  (min-vinsn-args 0 :type fixnum)
  (max-vinsn-args 0 :type fixnum))

(defmethod print-object ((p opcode) stream)
  (declare (ignore depth))
  (print-unreadable-object (p stream :type t) 
    (format stream "~a" (string (opcode-name p)))))

(defmethod make-load-form ((p opcode) &optional env)
  (make-load-form-saving-slots p :environment env))

(defstruct operand
  (index 0 :type unsigned-byte)
  (width 0 :type (mod 32))
  (offset 0 :type (mod 32))
  (insert-function nil :type (or null symbol function))
  (extract-function 'nil :type (or symbol function))
  (flags 0 :type fixnum))

(defmethod make-load-form ((o operand) &optional env)
  (make-load-form-saving-slots o :environment env))

(defconstant operand-optional 27)
(defconstant operand-fake 28)

(defconstant reservation-discharge #x1004)

(ccl::provide "ARCH")
