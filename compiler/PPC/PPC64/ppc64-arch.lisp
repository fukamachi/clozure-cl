;;;-*- Mode: Lisp; Package: (PPC64 :use CL) -*-
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

(defpackage "PPC64"
  (:use "CL")
  #+ppc64-target
  (:nicknames "TARGET"))


(in-package "PPC64")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant nbits-in-word 64)
(defconstant least-significant-bit 63)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 4)
(defconstant nlisptagbits 3)
(defconstant nfixnumtagbits 3)          ; See ?
(defconstant num-subtag-bits 8)         ; tag part of header is 8 bits wide
(defconstant fixnumshift nfixnumtagbits)
(defconstant fixnum-shift fixnumshift)          ; A pet name for it.
(defconstant fulltagmask (1- (ash 1 ntagbits)))         ; Only needed by GC/very low-level code
(defconstant full-tag-mask fulltagmask)
(defconstant tagmask (1- (ash 1 nlisptagbits)))
(defconstant tag-mask tagmask)
(defconstant fixnummask (1- (ash 1 nfixnumtagbits)))
(defconstant fixnum-mask fixnummask)
(defconstant subtag-mask (1- (ash 1 num-subtag-bits)))
(defconstant ncharcodebits 32)
(defconstant charcode-shift (- nbits-in-word ncharcodebits))
(defconstant word-shift 3)
(defconstant word-size-in-bytes 8)
(defconstant target-most-negative-fixnum (ash -1 (1- (- nbits-in-word nfixnumtagbits))))
(defconstant target-most-positive-fixnum (1- (ash 1 (1- (- nbits-in-word nfixnumtagbits)))))
(defmacro define-subtag (name tag value)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name) (logior ,tag (ash ,value ntagbits))))

;; PPC64 stuff and tags.

;; There are several ways to look at the 4 tag bits of any object or
;; header.  Looking at the low 2 bits, we can classify things as
;; follows (I'm not sure if we'd ever want to do this) :
;;
;;  #b00   a "primary" object: fixnum, cons, uvector
;;  #b01   an immediate
;;  #b10   the header on an immediate uvector
;;  #b11   the header on a node (pointer-containing) uvector
;;
;;  Note that the ppc64's LD and STD instructions require that the low
;;  two bits of the constant displacement be #b00.  If we want to use constant
;;  offsets to access CONS and UVECTOR fields, we're pretty much obligated
;;  to ensure that CONS and UVECTOR have tags that also end in #b00, and
;;  fixnum addition and subtraction work better when fixnum tags are all 0.
;;  We generally have to look at all 4 tag bits before we really know what
;;  class of "potentially primary" object we're looking at.
;;  If we look at 3 tag bits, we can see:
;;
;;  #b000  fixnum
;;  #b001  immediate
;;  #b010  immedate-header
;;  #b011  node-header
;;  #b100  CONS or UVECTOR
;;  #b101  immediate
;;  #b110  immediate-header
;;  #b111  node-header
;;

(defconstant tag-fixnum 0)

;;  Note how we're already winding up with lots of header and immediate
;;  "classes".  That might actually be useful.
;;
;;  When we move to 4 bits, we wind up (obviously) with 4 tags of the form
;;  #bxx00.  There are two partitionings that make (some) sense: we can either
;;  use 2 of these for (even and odd) fixnums, or we can give NIL a tag
;;  that's congruent (mod 16) with CONS.  There seem to be a lot of tradeoffs
;;  involved, but it ultimately seems best to be able to treat 64-bit
;;  aligned addresses as fixnums: we don't want the VSP to look like a
;;  vector.   That basically requires that NIL really be a symbol (good
;;  bye, nilsym) and that we ensure that there are NILs where its CAR and
;;  CDR would be (-4, 4 bytes from the tagged pointer.)  That means that
;;  CONS is 4 and UVECTOR is 12, and we have even more immediate/header types.

(defconstant fulltag-even-fixnum    #b0000)
(defconstant fulltag-imm-0          #b0001)
(defconstant fulltag-immheader-0    #b0010)
(defconstant fulltag-nodeheader-0   #b0011)
(defconstant fulltag-cons           #b0100)
(defconstant fulltag-imm-1          #b0101)
(defconstant fulltag-immheader-1    #b0110)
(defconstant fulltag-nodeheader-1   #b0111)
(defconstant fulltag-odd-fixnum     #b1000)
(defconstant fulltag-imm-2          #b1001)
(defconstant fulltag-immheader-2    #b1010)
(defconstant fulltag-nodeheader-2   #b1011)
(defconstant fulltag-misc           #b1100)
(defconstant fulltag-imm-3          #b1101)
(defconstant fulltag-immheader-3    #b1110)
(defconstant fulltag-nodeheader-3   #b1111)


;; The general algorithm for determining the (primary) type of an
;; object is something like:
;; (clrldi tag node 60)
;; (cmpwi tag fulltag-misc)
;; (clrldi tag tag 61
;; (bne @done)
;; (lbz tag misc-subtag-offset node)
;; @done
;;
;; That's good enough to identify FIXNUM, "generally immediate", cons,
;; or a header tag from a UVECTOR.  In some cases, we may need to hold
;; on to the full 4-bit tag.
;; In no specific order:
;; - it's important to be able to quickly recognize fixnums; that's
;;    simple
;; - it's important to be able to quickly recognize lists (for CAR/CDR)
;;   and somewhat important to be able to quickly recognize conses.
;;   Also simple, though we have to special-case NIL.
;; - it's desirable to be able to do VECTORP, ARRAYP, and specific-array-type-
;;   p.  We need at least 12 immediate CL vector types (SIGNED/UNSIGNED-BYTE
;;   8/16/32/64, SINGLE-FLOAT, DOUBLE-FLOAT, BIT, and at least one CHARACTER;
;;   we need SIMPLE-ARRAY, VECTOR-HEADER, and ARRAY-HEADER as node
;;   array types.  That's suspciciously close to 16
;; - it's desirable to be able (in FUNCALL) to quickly recognize
;;   functions/symbols/other, and probably desirable to trap on other.
;;   Pretty much have to do a memory reference and at least one comparison
;;   here.
;; - it's sometimes desirable to recognize numbers and distinct numeric
;;   types (other than FIXNUM) quickly.
;; - The GC (especially) needs to be able to determine the size of
;;   ivectors (ivector elements) fairly cheaply.  Most ivectors are CL
;;   arrays, but code-vectors are fairly common (and have 32-bit elements,
;;   naturally.)
;; - We have a fairly large number of non-array gvector types, and it's
;;   always desirable to have room for expansion.
;; - we basically have 8 classes of header subtags, each of which has
;;   16 possible values.  If we stole the high bit of the subtag to
;;   indicate CL-array-ness, we'd still have 6 bits to encode non-CL
;;   array types.  

(defconstant cl-array-subtag-mask #x80)
(defmacro define-cl-array-subtag (name tag value)
  `(defconstant ,(ccl::form-symbol "SUBTAG-" name)
    (logior cl-array-subtag-mask (logior ,tag (ash ,value ntagbits)))))

(define-cl-array-subtag arrayH  fulltag-nodeheader-1 0)
(define-cl-array-subtag vectorH fulltag-nodeheader-2 0)
(define-cl-array-subtag simple-vector fulltag-nodeheader-3 0)

;;  bits:                         64             32       16    8     1
;;  CL-array ivector types    DOUBLE-FLOAT     SINGLE    s16   CHAR  BIT
;;                               s64             s32     u16    s8
;;                               u64             u32            u8
;;  Other ivector types       MACPTR           CODE-VECTOR
;;                            DEAD-MACPTR     XCODE-VECTOR
;;                            BIGNUM
;;                            DOUBLE-FLOAT
;; There might possibly be ivectors with 128-bit (VMX/AltiVec) elements
;; someday, and there might be multiple character sizes (16/32 bits).
;; That sort of suggests that we use the four immheader classes to
;; encode the ivector size (64, 32, 8, other) and make BIT an easily-
;; detected case of OTHER.

(defconstant ivector-class-64-bit fulltag-immheader-3)
(defconstant ivector-class-32-bit fulltag-immheader-2)
(defconstant ivector-class-other-bit fulltag-immheader-1)
(defconstant ivector-class-8-bit fulltag-immheader-0)

(define-cl-array-subtag s64-vector ivector-class-64-bit 1)
(define-cl-array-subtag u64-vector ivector-class-64-bit 2)
(define-cl-array-subtag double-float-vector ivector-class-64-bit 4)
(define-cl-array-subtag s32-vector ivector-class-32-bit 1)
(define-cl-array-subtag u32-vector ivector-class-32-bit 2)
(define-cl-array-subtag single-float-vector ivector-class-32-bit 3)
(define-cl-array-subtag s16-vector ivector-class-other-bit 1)
(define-cl-array-subtag u16-vector ivector-class-other-bit 2)
(define-cl-array-subtag bit-vector ivector-class-other-bit 7)
(define-cl-array-subtag s8-vector ivector-class-8-bit 1)
(define-cl-array-subtag u8-vector ivector-class-8-bit 2)
(define-cl-array-subtag simple-base-string ivector-class-8-bit 5)

;; There's some room for expansion in non-array ivector space.
(define-subtag bignum ivector-class-64-bit 0)
(define-subtag macptr ivector-class-64-bit 1)
(define-subtag dead-macptr ivector-class-64-bit 2)
(define-subtag double-float ivector-class-64-bit 3)

(define-subtag code-vector ivector-class-32-bit 0)
(define-subtag xcode-vector ivector-class-32-bit 1)

;; Size doesn't matter for non-CL-array gvectors; I can't think of a good
;; reason to classify them in any particular way.  Let's put funcallable
;; things in the first slice by themselves, though it's not clear that
;; that helps FUNCALL much.
(defconstant gvector-funcallable fulltag-nodeheader-0)
(define-subtag function gvector-funcallable 0)
(define-subtag symbol gvector-funcallable 1)

(define-subtag catch-frame fulltag-nodeheader-1 0)
(define-subtag lisp-thread fulltag-nodeheader-1 1)
(define-subtag lock fulltag-nodeheader-1 2)
(define-subtag hash-vector fulltag-nodeheader-1 3)
(define-subtag pool fulltag-nodeheader-1 4)
(define-subtag weak fulltag-nodeheader-1 5)
(define-subtag package fulltag-nodeheader-1 6)
(define-subtag svar fulltag-nodeheader-1 7)

(define-subtag slot-vector 0 fulltag-nodeheader-2)
(define-subtag instance 1 fulltag-nodeheader-2)
(define-subtag struct 2  fulltag-nodeheader-2)
(define-subtag istruct 3  fulltag-nodeheader-2)
(define-subtag value-cell 4  fulltag-nodeheader-2)
(define-subtag xfunction 5 fulltag-nodeheader-2)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "PPC-ARCH")
  (defmacro define-storage-layout (name origin &rest cells)
  `(progn
     (ccl::defenum (:start ,origin :step 8)
       ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell)) cells))
     (defconstant ,(ccl::form-symbol name ".SIZE") ,(* (length cells)
						       8))))
 
(defmacro define-lisp-object (name tagname &rest cells)
  `(define-storage-layout ,name ,(- (symbol-value tagname)) ,@cells))



(defmacro define-fixedsized-object (name &rest non-header-cells)
  `(progn
     (define-lisp-object ,name fulltag-misc header ,@non-header-cells)
     (ccl::defenum ()
       ,@(mapcar #'(lambda (cell) (ccl::form-symbol name "." cell "-CELL")) non-header-cells))
     (defconstant ,(ccl::form-symbol name ".ELEMENT-COUNT") ,(length non-header-cells))))







(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-subtag-offset (+ misc-header-offset 7 ))
(defconstant misc-data-offset (+ misc-header-offset 8))
(defconstant misc-dfloat-offset (+ misc-header-offset 8))



(define-subtag single-float fulltag-imm-0 0)

(define-subtag character fulltag-imm-2 0)

(define-subtag unbound fulltag-imm-3 0)
(defconstant unbound-marker subtag-unbound)
(defconstant undefined unbound-marker)
(define-subtag slot-unbound fulltag-imm-3 1)
(defconstant slot-unbound-marker subtag-slot-unbound)
(define-subtag illegal fulltag-imm-3 2)
(defconstant illegal-marker subtag-illegal)

(define-subtag no-thread-local-binding fulltag-imm-3 3)
(define-subtag forward-marker fulltag-imm-3 15)


(defconstant max-64-bit-constant-index (ash (+ #x7fff ppc64::misc-dfloat-offset) -3))
(defconstant max-32-bit-constant-index (ash (+ #x7fff ppc64::misc-data-offset) -2))
(defconstant max-16-bit-constant-index (ash (+ #x7fff ppc64::misc-data-offset) -1))
(defconstant max-8-bit-constant-index (+ #x7fff ppc64::misc-data-offset))
(defconstant max-1-bit-constant-index (ash (+ #x7fff ppc64::misc-data-offset) 5))


; The objects themselves look something like this:

; Order of CAR and CDR doesn't seem to matter much - there aren't
; too many tricks to be played with predecrement/preincrement addressing.
; Keep them in the confusing MCL 3.0 order, to avoid confusion.
(define-lisp-object cons fulltag-cons 
  cdr 
  car)


(define-fixedsized-object ratio
  numer
  denom)

(define-fixedsized-object double-float
  value)

(define-fixedsized-object complex
  realpart
  imagpart
)


; There are two kinds of macptr; use the length field of the header if you
; need to distinguish between them
(define-fixedsized-object macptr
  address
  domain
  type
)

(define-fixedsized-object xmacptr
  address
  domain
  type
  flags
  link
)

; Catch frames go on the tstack; they point to a minimal lisp-frame
; on the cstack.  (The catch/unwind-protect PC is on the cstack, where
; the GC expects to find it.)
(define-fixedsized-object catch-frame
  catch-tag                             ; #<unbound> -> unwind-protect, else catch
  link                                  ; tagged pointer to next older catch frame
  mvflag                                ; 0 if single-value, 1 if uwp or multiple-value
  csp                                   ; pointer to control stack
  db-link                               ; value of dynamic-binding link on thread entry.
  save-save7                            ; saved registers
  save-save6
  save-save5
  save-save4
  save-save3
  save-save2
  save-save1
  save-save0
  xframe                                ; exception-frame link
  tsp-segment                           ; mostly padding, for now.
)

(define-fixedsized-object lock
  _value                                ;finalizable pointer to kernel object
  kind                                  ; '0 = recursive-lock, '1 = rwlock
  writer				;tcr of owning thread or 0
  name
  )

(define-fixedsized-object lisp-thread
  tcr
  name
  cs-size
  vs-size
  ts-size
  initial-function.args
  interrupt-functions
  interrupt-lock
  startup-function
  state
  state-change-lock
)

(define-fixedsized-object symbol
  pname
  vcell
  fcell
  package-plist
  flags
)

(defconstant t-offset (- symbol.size))




(define-fixedsized-object vectorH
  logsize                               ; fillpointer if it has one, physsize otherwise
  physsize                              ; total size of (possibly displaced) data vector
  data-vector                           ; object this header describes
  displacement                          ; true displacement or 0
  flags                                 ; has-fill-pointer,displaced-to,adjustable bits; subtype of underlying simple vector.
)

(define-lisp-object arrayH fulltag-misc
  header                                ; subtag = subtag-arrayH
  rank                                  ; NEVER 1
  physsize                              ; total size of (possibly displaced) data vector
  data-vector                           ; object this header describes
  displacement                          ; true displacement or 0  
  flags                                 ; has-fill-pointer,displaced-to,adjustable bits; subtype of underlying simple vector.
 ;; Dimensions follow
)

(defconstant arrayH.rank-cell 0)
(defconstant arrayH.physsize-cell 1)
(defconstant arrayH.data-vector-cell 2)
(defconstant arrayH.displacement-cell 3)
(defconstant arrayH.flags-cell 4)
(defconstant arrayH.dim0-cell 5)

(defconstant arrayH.flags-cell-bits-byte (byte 8 0))
(defconstant arrayH.flags-cell-subtag-byte (byte 8 8))


(define-fixedsized-object value-cell
  value)

(define-fixedsized-object svar
  symbol
  idx)

;;; The kernel uses these (rather generically named) structures
;;; to keep track of various memory regions it (or the lisp) is
;;; interested in.


(define-storage-layout area 0
  pred                                  ; pointer to preceding area in DLL
  succ                                  ; pointer to next area in DLL
  low                                   ; low bound on area addresses
  high                                  ; high bound on area addresses.
  active                                ; low limit on stacks, high limit on heaps
  softlimit                             ; overflow bound
  hardlimit                             ; another one
  code                                  ; an area-code; see below
  markbits                              ; bit vector for GC
  ndwords                               ; "active" size of dynamic area or stack
  older                                 ; in EGC sense
  younger                               ; also for EGC
  h                                     ; Handle or null pointer
  softprot                              ; protected_area structure pointer
  hardprot                              ; another one.
  owner                                 ; fragment (library) which "owns" the area
  refbits                               ; bitvector for intergenerational refernces
  threshold                             ; for egc
  gc-count                              ; generational gc count.
)

(ccl::defenum (:prefix "AREA-")
  void                                  ; list header
  cstack                                ; a control stack
  vstack                                ; a value stack
  tstack                                ; (dynamic-extent) temp stack
  readonly                              ; readonly section
  staticlib                             ; static data in library
  static                                ; static data in application
  dynamic                               ; dynmaic (heap) data in application
)


; areas are sorted such that (in the "succ" direction) codes are >=.
; If you think that you're looking for a stack (instead of a heap), look
; in the "pred" direction from the all-areas header.
(defconstant max-stack-area-code area-tstack)
(defconstant min-heap-area-code area-readonly)


(define-storage-layout protected-area 0
  next
  start                                 ; first byte (page-aligned) that might be protected
  end                                   ; last byte (page-aligned) that could be protected
  nprot                                 ; Might be 0
  protsize                              ; number of bytes to protect
  why)

(defconstant tcr-bias #x7000)

(define-storage-layout tcr (- tcr-bias)
  prev					; in doubly-linked list 
  next					; in doubly-linked list
  single-float-convert			; per-thread scratch space.
  lisp-fpscr-high
  db-link				; special binding chain head 
  catch-top				; top catch frame 
  save-vsp				; VSP when in foreign code 
  save-tsp				; TSP when in foreign code 
  cs-area				; cstack area pointer 
  vs-area				; vstack area pointer 
  ts-area				; tstack area pointer 
  cs-limit				; cstack overflow limit
  total-bytes-allocated-high
  total-bytes-allocated-low
  interrupt-level			; fixnum
  interrupt-pending			; fixnum
  xframe				; exception frame linked list
  errno-loc				; thread-private, maybe
  ffi-exception				; fpscr bits from ff-call.
  osid					; OS thread id 
  valence				; odd when in foreign code 
  foreign-exception-status
  native-thread-info
  native-thread-id
  last-allocptr
  save-allocptr
  save-allocbase
  reset-completion
  activate
  suspend-count
  suspend-context
  pending-exception-context
  suspend				; semaphore for suspension notify 
  resume				; sempahore for resumption notify
  flags					; foreign, being reset, ...
  gc-context
  suspend-total
  suspend-total-on-exception-entry
  tlb-limit
  tlb-pointer
  shutdown-count
)

(defconstant tcr.lisp-fpscr-low (+ tcr.lisp-fpscr-high 4))

(define-storage-layout lockptr 0
  avail
  owner
  count
  signal
  waiting
  malloced-ptr)

;; For the eabi port: mark this stack frame as Lisp's (since EABI
;; foreign frames can be the same size as a lisp frame.)


(ppc64::define-storage-layout lisp-frame 0
  backlink
  savefn
  savelr
  savevsp
)

(ppc64::define-storage-layout c-frame 0
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

(defmacro define-header (name element-count subtag)
  `(defconstant ,name (logior (ash ,element-count num-subtag-bits) ,subtag)))

(define-header double-float-header double-float.element-count subtag-double-float)
(define-header one-digit-bignum-header 1 subtag-bignum)
(define-header two-digit-bignum-header 2 subtag-bignum)
(define-header symbol-header symbol.element-count subtag-symbol)
(define-header value-cell-header value-cell.element-count subtag-value-cell)
(define-header macptr-header macptr.element-count subtag-macptr)


(defconstant yield-syscall
  #+darwinppc-target -60
  #+linuxppc-target #$__NR_sched_yield)
)
)






(defun %kernel-global (sym)
  (let* ((pos (position sym ppc::*ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-misc (* (1+ pos) word-size-in-bytes)))
      (error "Unknown kernel global : ~s ." sym))))

(defmacro kernel-global (sym)
  (let* ((pos (position sym ppc::*ppc-kernel-globals* :test #'string=)))
    (if pos
      (- (+ fulltag-misc (* (1+ pos) word-size-in-bytes)))
      (error "Unknown kernel global : ~s ." sym))))

; The kernel imports things that are defined in various other libraries for us.
; The objects in question are generally fixnum-tagged; the entries in the
; "kernel-imports" vector are 8 bytes apart.
(ccl::defenum (:prefix "KERNEL-IMPORT-" :start 0 :step word-size-in-bytes)
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

(defmacro nrs-offset (name)
  (let* ((pos (position name ppc::*ppc-nilreg-relative-symbols* :test #'eq)))
    (if pos (* (1+ pos) symbol.size))))

(defconstant nil-value (+ #x2000 symbol.size fulltag-misc))


(defconstant reservation-discharge #x1008)

(defparameter *ppc64-target-uvector-subtags*
  `((:simple-vector . ,subtag-simple-vector)
    (:bit-vector . ,subtag-bit-vector)
    (:simple-string . ,subtag-simple-base-string)
    (:u8-vector . ,subtag-u8-vector)
    (:s8-vector . ,subtag-s8-vector)
    (:u16-vector . ,subtag-u16-vector)
    (:s16-vector . ,subtag-s16-vector)
    (:u32-vector . ,subtag-u32-vector)
    (:s32-vector . ,subtag-s32-vector)
    (:u64-vector . ,subtag-u64-vector)
    (:s64-vector . ,subtag-s64-vector)
    (:single-float-vector . ,subtag-single-float-vector)
    (:double-float-vector . ,subtag-double-float-vector)))

(defparameter *ppc64-target-arch*
  (arch::make-target-arch :name :ppc64
                          :lisp-node-size 8
                          :nil-value nil-value
                          :fixnum-shift fixnumshift
                          :most-positive-fixnum (1- (ash 1 (1- (- 64 fixnumshift))))
                          :most-negative-fixnum (- (ash 1 (1- (- 64 fixnumshift))))
                          :misc-data-offset misc-data-offset
                          :misc-dfloat-offset misc-dfloat-offset
                          :nbits-in-word 64
                          :ntagbits 4
                          :nlisptagbits 3
                          :uvector-subtags *ppc64-target-uvector-subtags*
                          :max-64-bit-constant-index max-64-bit-constant-index
                          :max-32-bit-constant-index max-32-bit-constant-index
                          :max-16-bit-constant-index max-16-bit-constant-index
                          :max-8-bit-constant-index max-8-bit-constant-index
                          :max-1-bit-constant-index max-1-bit-constant-index
                          :word-shift 3
                          ))

(provide "PPC64-ARCH")
