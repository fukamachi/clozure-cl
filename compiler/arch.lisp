;;;-*- Mode: Lisp; Package: (ARCH :use CL) -*-
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
(defpackage "ARCH"
  (:use "CL"))

(in-package "ARCH")

; Some of these macros were stolen from CMUCL.  Sort of ...

(defmacro iterate (name binds &body body)
  "Iterate Name ({(Var Initial-Value)}*) Declaration* Form*
  This is syntactic sugar for Labels.  It creates a local function Name with
  the specified Vars as its arguments and the Declarations and Forms as its
  body.  This function is then called with the Initial-Values, and the result
  of the call is return from the macro."
  (dolist (x binds)
    (unless (and (listp x)
                 (= (length x) 2))
      (error "Malformed iterate variable spec: ~S." x)))

  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create a Let* which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let* around the result of the
  evaluation of Body.  Within the body, each Var is bound to the corresponding
  temporary variable."
  (iterate frob
           ((specs specs)
            (body body))
    (if (null specs)
        `(progn ,@body)
        (let ((spec (first specs)))
          (when (/= (length spec) 2)
            (error "Malformed Once-Only binding spec: ~S." spec))
          (let ((name (first spec))
                (exp-temp (gensym)))
            `(let ((,exp-temp ,(second spec))
                   (,name (gensym)))
               `(let ((,,name ,,exp-temp))
                  ,,(frob (rest specs) body))))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun form-symbol (first &rest others)
  (intern (apply #'concatenate 'simple-base-string (string first) (mapcar #'string others))))
)


;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)


(defun collect-normal-expander (n-value fun forms)
  `(progn
     ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
     ,n-value))

;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
       ,@(mapcar #'(lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond (,n-tail
                               (setf (cdr ,n-tail) ,n-res)
                               (setq ,n-tail ,n-res))
                              (t
                               (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                 forms)
       ,n-value)))
)

;;;
;;;    The ultimate collection macro...
;;;
(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."
  
  
  (let ((macros ())
        (binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
        (error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
            (name (first spec))
            (default (second spec))
            (kind (or (third spec) 'collect)))
        
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
          (let ((n-tail (gensym)))
            (if default
              (push `(,n-tail (last ,n-value)) binds)
              (push n-tail binds))
            (push `(,name (&rest args)
                          (collect-list-expander ',n-value ',n-tail args))
                  macros))
          (push `(,name (&rest args)
                        (collect-normal-expander ',n-value ',kind args))
                macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))


;;; DEFENUM -- Internal Interface.
;;;
(defmacro defenum ((&key (prefix "") (suffix "") (start 0) (step 1))
                   &rest identifiers)
  (let ((results nil)
        (index 0)
        (start (eval start))
        (step (eval step)))
    (dolist (id identifiers)
      (multiple-value-bind
        (root docs)
        (if (consp id)
          (values (car id) (cdr id))
          (values id nil))
        (push `(defconstant ,(intern (concatenate 'simple-base-string
                                                  (string prefix)
                                                  (string root)
                                                  (string suffix)))
                 ,(+ start (* step index))
                 ,@docs)
              results))
      (incf index))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(nreverse results))))


(defmacro define-storage-layout (name origin &rest cells)
  `(progn
     (defenum (:start ,origin :step 4)
       ,@(mapcar #'(lambda (cell) (form-symbol name "." cell)) cells))
     (defconstant ,(form-symbol name ".SIZE") ,(* (length cells) 4))))
 
(defmacro define-lisp-object (name tagname &rest cells)
  `(define-storage-layout ,name ,(- (symbol-value tagname)) ,@cells))

(defmacro define-subtag (name tag subtag)
  `(defconstant ,(form-symbol "SUBTAG-" name) (logior ,tag (ash ,subtag ntagbits))))


(defmacro define-imm-subtag (name subtag)
  `(define-subtag ,name fulltag-immheader ,subtag))

(defmacro define-node-subtag (name subtag)
  `(define-subtag ,name fulltag-nodeheader ,subtag))

(defmacro define-fixedsized-object (name  &rest non-header-cells)
  `(progn
     (define-lisp-object ,name fulltag-misc header ,@non-header-cells)
     (defenum ()
       ,@(mapcar #'(lambda (cell) (form-symbol name "." cell "-CELL")) non-header-cells))
     (defconstant ,(form-symbol name ".ELEMENT-COUNT") ,(length non-header-cells))))



;; PPC-32 stuff and tags.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant nbits-in-word 32)
(defconstant least-significant-bit 31)
(defconstant nbits-in-byte 8)
(defconstant ntagbits 3)                ; But non-header objects only use 2
(defconstant nlisptagbits 2)
(defconstant nfixnumtagbits 2)          ; See ?
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
(defconstant ncharcodebits 16)
(defconstant charcode-shift (- nbits-in-word ncharcodebits))
(defconstant word-shift 2)


;; Tags.
;; There are two-bit tags and three-bit tags.
;; A FULLTAG is the value of the low three bits of a tagged object.
;; A TAG is the value of the low two bits of a tagged object.
;; A TYPECODE is either a TAG or the value of a "tag-misc" object's header-byte.

;; There are 4 primary TAG values.  Any object which lisp can "see" can be classified 
;; by its TAG.  (Some headers have FULLTAGS that are congruent modulo 4 with the
;; TAGS of other objects, but lisp can't "see" headers.)
(defenum ()
  tag-fixnum                            ; All fixnums, whether odd or even
  tag-list                              ; Conses and NIL
  tag-misc                              ; Heap-consed objects other than lists: vectors, symbols, functions, floats ...
  tag-imm                               ; Immediate-objects: characters, UNBOUND, other markers.
)

;; And there are 8 FULLTAG values.  Note that NIL has its own FULLTAG (congruent mod 4 to tag-list),
;; that FULLTAG-MISC is > 4 (so that code-vector entry-points can be branched to, since the low
;; two bits of the PC are ignored) and that both FULLTAG-MISC and FULLTAG-IMM have header fulltags
;; that share the same TAG.
;; Things that walk memory (and the stack) have to be careful to look at the FULLTAG of each
;; object that they see.
(defenum ()
  fulltag-even-fixnum                   ; I suppose EVENP/ODDP might care; nothing else does.
  fulltag-cons                          ; a real (non-null) cons.  Shares TAG with fulltag-nil.
  fulltag-nodeheader                    ; Header of heap-allocated object that contains lisp-object pointers
  fulltag-imm                           ; a "real" immediate object.  Shares TAG with fulltag-immheader.
  fulltag-odd-fixnum                    ; 
  fulltag-nil                           ; NIL and nothing but.  (Note that there's still a hidden NILSYM.)
  fulltag-misc                          ; Pointer "real" tag-misc object.  Shares TAG with fulltag-nodeheader.
  fulltag-immheader                     ; Header of heap-allocated object that contains unboxed data.
)



; Order of CAR and CDR doesn't seem to matter much - there aren't
; too many tricks to be played with predecrement/preincrement addressing.
; Keep them in the confusing MCL 3.0 order, to avoid confusion.
(define-lisp-object cons tag-list 
  cdr 
  car)


(defconstant misc-header-offset (- fulltag-misc))
(defconstant misc-subtag-offset (+ misc-header-offset 3))
(defconstant misc-data-offset (+ misc-header-offset 4))
(defconstant misc-dfloat-offset (+ misc-header-offset 8))



; T is almost adjacent to NIL: since NIL is a misaligned CONS, it spans
; two doublewords.  The arithmetic difference between T and NIL is
; such that the least-significant bit and exactly one other bit is
; set in the result.

(defconstant t-offset (+ 8 (- 8 fulltag-nil) fulltag-misc))
(assert (and (logbitp 0 t-offset) (= (logcount t-offset) 2)))


; The order in which various header values are defined is significant in several ways:
; 1) Numeric subtags precede non-numeric ones; there are further orderings among numeric subtags.
; 2) All subtags which denote CL arrays are preceded by those that don't,
;    with a further ordering which requires that (< header-arrayH header-vectorH ,@all-other-CL-vector-types)
; 3) The element-size of ivectors is determined by the ordering of ivector subtags.
; 4) All subtags are >= fulltag-immheader .


; Numeric subtags.
(define-imm-subtag bignum 0)
(defconstant min-numeric-subtag subtag-bignum)
(define-node-subtag ratio 1)
(defconstant max-rational-subtag subtag-ratio)

(define-imm-subtag single-float 1)          ; "SINGLE" float, aka short-float in the new order.
(define-imm-subtag double-float 2)
(defconstant min-float-subtag subtag-single-float)
(defconstant max-float-subtag subtag-double-float)
(defconstant max-real-subtag subtag-double-float)

(define-node-subtag complex 3)
(defconstant max-numeric-subtag subtag-complex)

; CL array types.  There are more immediate types than node types; all CL array subtags must be > than
; all non-CL-array subtags.  So we start by defining the immediate subtags in decreasing order, starting
; with that subtag whose element size isn't an integral number of bits and ending with those whose
; element size - like all non-CL-array fulltag-immheader types - is 32 bits.
(define-imm-subtag bit-vector 31)
(define-imm-subtag double-float-vector 30)
(define-imm-subtag s16-vector 29)
(define-imm-subtag u16-vector 28)
(define-imm-subtag simple-general-string 27)
(defconstant min-16-bit-ivector-subtag subtag-simple-general-string)
(defconstant max-16-bit-ivector-subtag subtag-s16-vector)
(defconstant max-string-subtag subtag-simple-general-string)

(define-imm-subtag simple-base-string 26)
(define-imm-subtag s8-vector 25)
(define-imm-subtag u8-vector 24)
(defconstant min-8-bit-ivector-subtag subtag-u8-vector)
(defconstant max-8-bit-ivector-subtag subtag-simple-base-string)
(defconstant min-string-subtag subtag-simple-base-string)

(define-imm-subtag s32-vector 23)
(define-imm-subtag u32-vector 22)
(define-imm-subtag single-float-vector 21)
(defconstant max-32-bit-ivector-subtag subtag-s32-vector)
(defconstant min-cl-ivector-subtag subtag-single-float-vector)

(define-node-subtag vectorH 21)
(define-node-subtag arrayH 20)
(assert (< subtag-arrayH subtag-vectorH min-cl-ivector-subtag))
(define-node-subtag simple-vector 22)   ; Only one such subtag
(assert (< subtag-arrayH subtag-vectorH subtag-simple-vector))
(defconstant min-vector-subtag subtag-vectorH)
(defconstant min-array-subtag subtag-arrayH)

; So, we get the remaining subtags (n: (n > max-numeric-subtag) & (n < min-array-subtag))
; for various immediate/node object types.

(define-imm-subtag macptr 3)
(defconstant min-non-numeric-imm-subtag subtag-macptr)
(assert (> min-non-numeric-imm-subtag max-numeric-subtag))
(define-imm-subtag dead-macptr 4)
(define-imm-subtag code-vector 5)
(define-imm-subtag creole-object 6)
(define-imm-subtag xcode-vector 7)  ; code-vector for cross-development

(defconstant max-non-array-imm-subtag (logior (ash 19 ntagbits) fulltag-immheader))

(define-node-subtag catch-frame 4)
(defconstant min-non-numeric-node-subtag subtag-catch-frame)
(assert (> min-non-numeric-node-subtag max-numeric-subtag))
(define-node-subtag function 5)
(define-node-subtag lisp-thread 6)
(define-node-subtag symbol 7)
(define-node-subtag lock 8)
(define-node-subtag hash-vector 9)
(define-node-subtag pool 10)
(define-node-subtag weak 11)
(define-node-subtag package 12)
(define-node-subtag slot-vector 13)
(define-node-subtag instance 14)
(define-node-subtag struct 15)
(define-node-subtag istruct 16)
(define-node-subtag value-cell 17)
(define-node-subtag xfunction 18)       ; Function for cross-development
(define-node-subtag svar 19)
(defconstant max-non-array-node-subtag (logior (ash 19 ntagbits) fulltag-nodeheader))


; The objects themselves look something like this:

(define-fixedsized-object ratio
  numer
  denom)

(define-fixedsized-object single-float
  value)

(define-fixedsized-object double-float
  pad
  value
  val-low)

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


(defconstant nilsym-offset (+ t-offset symbol.size))


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
;;; The gc-area record definition in "ccl:interfaces;mcl-records.lisp"
;;; matches this.
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

(define-storage-layout tcr 0
  prev					; in doubly-linked list 
  next					; in doubly-linked list 
  lisp-fpscr-high
  lisp-fpscr-low
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
)

(defconstant tcr-flag-bit-foreign 0)
(defconstant tcr-flag-bit-awaiting-preset 1)

(define-storage-layout lockptr 0
  avail
  owner
  count
  signal
  waiting
  malloced-ptr)



(defenum (:prefix "AREA-")
  void                                  ; list header
  cstack                                ; a control stack
  vstack                                ; a value stack
  tstack                                ; (dynamic-extent) temp stack
  readonly                              ; readonly section
  staticlib                             ; static data in library
  static                                ; static data in application
  dynamic                               ; dynmaic (heap) data in application
)

(define-storage-layout protected-area 0
  next
  start                                 ; first byte (page-aligned) that might be protected
  end                                   ; last byte (page-aligned) that could be protected
  nprot                                 ; Might be 0
  protsize                              ; number of bytes to protect
  why)

; areas are sorted such that (in the "succ" direction) codes are >=.
; If you think that you're looking for a stack (instead of a heap), look
; in the "pred" direction from the all-areas header.
(defconstant max-stack-area-code area-tstack)
(defconstant min-heap-area-code area-readonly)

(define-subtag unbound fulltag-imm 6)
(defconstant unbound-marker subtag-unbound)
(defconstant undefined unbound-marker)

(define-subtag character fulltag-imm 9)
(define-subtag vsp-protect fulltag-imm 7)
(define-subtag slot-unbound fulltag-imm 10)
(defconstant slot-unbound-marker subtag-slot-unbound)
(define-subtag illegal fulltag-imm 11)
(defconstant illegal-marker subtag-illegal)
(define-subtag go-tag fulltag-imm 12)
(define-subtag block-tag fulltag-imm 24)
(define-subtag no-thread-local-binding fulltag-imm 30)
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
(defenum (:prefix "KERNEL-IMPORT-" :start 0 :step 4)
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
(defenum (:start  error-type-error :prefix "ERROR-OBJECT-NOT-")
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

(defenum ()
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
