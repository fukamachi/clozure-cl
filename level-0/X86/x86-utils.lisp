; -*- Mode: Lisp; Package: CCL; -*-
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


(in-package "CCL")

(defx86lapfunction %address-of ((arg arg_z))
  ;; %address-of a fixnum is a fixnum, just for spite.
  ;; %address-of anything else is the address of that thing as an integer.
  (testb ($ x8664::fixnummask) (%b arg))
  (je @done)
  (movq (% arg) (% imm0))
  (jmp-subprim .SPmakeu64)
  @done
  (single-value-return))

;;; "areas" are fixnum-tagged and, for the most part, so are their
;;; contents.

;;; The nilreg-relative global all-areas is a doubly-linked-list header
;;; that describes nothing.  Its successor describes the current/active
;;; dynamic heap.  Return a fixnum which "points to" that area, after
;;; ensuring that the "active" pointers associated with the current thread's
;;; stacks are correct.



(defx86lapfunction %normalize-areas ()
  (let ((address temp0)
        (temp temp1))

    ; update active pointer for tsp area.
    (movq (@ (% rcontext) x8664::tcr.ts-area) (% address))
    (movq (@ (% rcontext) x8664::tcr.save-tsp) (% temp))
    (movq (% temp) (@ x8664::area.active (% address)))
    
    ;; Update active pointer for vsp area.
    (movq (@ (% rcontext) x8664::tcr.vs-area) (% address))
    (movq (% rsp) (@ x8664::area.active (% address)))

    (ref-global all-areas arg_z)
    (movq (@ x8664::area.succ (% arg_z)) (% arg_z))

    (single-value-return)))

(defx86lapfunction %active-dynamic-area ()
  (ref-global all-areas arg_z)
  (movq (@ x8664::area.succ (% arg_z)) (% arg_z))
  (single-value-return))

  
(defx86lapfunction %object-in-stack-area-p ((object arg_y) (area arg_z))
  (movq (@ x8664::area.active (% area)) (% imm0))
  (movq (@ x8664::area.high (% area)) (% imm1))
  (rcmp (% object) (% imm0))
  (movq ($ nil) (% arg_z))
  (movq ($ t) (% imm0))
  (jb @done)
  (rcmp (% object) (% imm1))
  (cmovbq (% imm0) (% arg_z))
  @done
  (single-value-return))

(defx86lapfunction %object-in-heap-area-p ((object arg_y) (area arg_z))
  (rcmp (% object) (@ x8664::area.low (% area)))
  (setae (%b imm0))
  (rcmp (% object) (@ x8664::area.low (% area)))
  (setb (%b imm1))
  (andb (% imm1.b) (% imm0.b))
  (andl ($ x8664::t-offset) (%l imm0))
  (lea (@ x8664::nil-value (% imm0)) (% arg_z))
  (single-value-return))


(defx86lapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (limit save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm0))
    (save-lisp-context)
    (vpush fun)
    (vpush obj)
    (vpush limit)
    (mr fun f)
    (ld limit ppc64::area.active a)
    (ld obj ppc64::area.low a)
    (b @test)
    @loop
    (ld header 0 obj)
    (extract-lowtag tag header)
    (cmpri cr0 tag ppc64::lowtag-immheader)
    (cmpri cr1 tag ppc64::lowtag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la arg_z ppc64::fulltag-cons obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (la obj ppc64::cons.size obj)
    (b @test)
    @misc
    (la arg_z ppc64::fulltag-misc obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (ldr header 0 obj)
    (extract-lowtag tag header)
    (extract-fulltag subtag header)
    (cmpri cr1 tag ppc64::lowtag-nodeheader)
    (extract-lowbyte tag header)
    (cmpri cr2 subtag ppc64::ivector-class-64-bit)
    (cmpri cr3 subtag ppc64::ivector-class-8-bit)
    (cmpri cr4 subtag ppc64::ivector-class-32-bit)
    (cmpri cr5 tag ppc64::subtag-bit-vector)
    (header-size elements header)
    (sldi bytes elements 3)
    (beq cr1 @bump)
    (beq cr2 @bump)
    (mr bytes elements)
    (beq cr3 @bump)
    (sldi bytes elements 2)
    (beq cr4 @bump)
    (sldi bytes elements 1)
    (bne cr5 @bump)
    (la elements 7 elements)
    (srdi bytes elements 3)
    @bump
    (la bytes (+ 8 15) bytes)
    (clrrdi bytes bytes 4)
    (add obj obj bytes)
    @test
    (cmpld :cr0 obj limit)
    (blt cr0 @loop)
    (vpop limit)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (single-value-return)))

;;; This walks the active "dynamic" area.  Objects might be moving around
;;; while we're doing this, so we have to be a lot more careful than we 
;;; are when walking a static area.
;;; There's the vague notion that we can't take an interrupt when
;;; "initptr" doesn't equal "freeptr", though what kind of hooks into a
;;; preemptive scheduler we'd need to enforce this is unclear.  We use
;;; initptr as an untagged pointer here (and set it to freeptr when we've
;;; got a tagged pointer to the current object.)
;;; There are a couple of approaches to termination:
;;;  a) Allocate a "sentinel" cons, and terminate when we run into it.
;;;  b) Check the area limit (which is changing if we're consing) and
;;;     terminate when we hit it.
;;; (b) loses if the function conses.  (a) conses.  I can't think of anything
;;; better than (a).
;;; This, of course, assumes that any GC we're doing does in-place compaction
;;; (or at least preserves the relative order of objects in the heap.)



(defx86lapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (sentinel save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm4))
    (save-lisp-context)
    (:regsave sentinel 0)
    (vpush fun)
    (vpush obj)
    (vpush sentinel)
    (ref-global imm0 tenured-area)
    (cmpdi cr0 imm0 0)
    (lwi allocbase #x8000)
    (sldi allocbase allocbase 32)
    (subi allocbase allocbase 16)
    (la allocptr (- ppc64::fulltag-cons ppc64::cons.size) allocptr)
    (tdlt allocptr allocbase)
    (mr sentinel allocptr)
    (clrrdi allocptr allocptr ppc64::ntagbits)
    (mr fun f)
    (if :ne
      (mr a imm0))    
    (ld imm5 ppc64::area.low a)
    @loop
    (ld header 0 imm5)
    (extract-lowtag tag header)
    (cmpdi cr0 tag ppc64::lowtag-immheader)
    (cmpdi cr1 tag ppc64::lowtag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la obj ppc64::fulltag-cons imm5)
    (cmpd cr0 obj sentinel)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (beq cr0 @done)
    (bla .SPfuncall)
    (la imm5 (- ppc64::cons.size ppc64::fulltag-cons) obj)
    (b @loop)
    @misc
    (la obj ppc64::fulltag-misc imm5)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (getvheader header obj)
    (extract-lowtag tag header)    
    (extract-fulltag subtag header)
    (cmpdi cr1 tag ppc64::lowtag-nodeheader)
    (extract-lowbyte tag header)
    (cmpri cr2 subtag ppc64::ivector-class-64-bit)
    (cmpri cr3 subtag ppc64::ivector-class-8-bit)
    (cmpri cr4 subtag ppc64::ivector-class-32-bit)
    (cmpri cr5 tag ppc64::subtag-bit-vector)
    (header-size elements header)
    (sldi bytes elements 3)
    (beq cr1 @bump)
    (beq cr2 @bump)
    (mr bytes elements)
    (beq cr3 @bump)
    (sldi bytes elements 2)
    (beq cr4 @bump)
    (sldi bytes elements 1)
    (bne cr5 @bump)
    (la elements 7 elements)
    (srdi bytes elements 3)
    @bump
    (la bytes (+ 8 15) bytes)
    (clrrdi bytes bytes 4)
    (subi imm5 obj ppc64::fulltag-misc)
    (add imm5 imm5 bytes)
    (b @loop)
    @done
    (li arg_z nil)
    (vpop sentinel)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (single-value-return)))

(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))



(defx86lapfunction %class-of-instance ((i arg_z))
  (svref arg_z instance.class-wrapper i)
  (svref arg_z %wrapper-class arg_z)
  (single-value-return))

(defx86lapfunction class-of ((x arg_z))
  (check-nargs 1)
  (extract-fulltag imm0 x)
  (cmpri imm0 x8664::fulltag-misc)
  (beq @misc)
  (extract-lowbyte imm0 x)
  (b @done)
  @misc
  (extract-subtag imm0 x)
  @done
  (slri imm0 imm0 x8664::word-shift)
  (ldr temp1 '*class-table* nfn)
  (addi imm0 imm0 x8664::misc-data-offset)
  (ldr temp1 x8664::symbol.vcell temp1)
  (ldrx temp0 temp1 imm0) ; get entry from table
  (cmpri cr0 temp0 nil)
  (beq @bad)
  ;; functionp?
  (extract-typecode imm1 temp0)
  (cmpri imm1 x8664::subtag-function)
  (bne @ret)  ; not function - return entry
  ;; else jump to the fn
  (mr nfn temp0)
  (ldr temp0 x8664::misc-data-offset temp0)
  (SET-NARGS 1)
  (mtctr temp0)
  (bctr)
  @bad
  (ldr fname 'no-class-error nfn)
  (ba .spjmpsym)
  @ret
  (mr arg_z temp0)  ; return frob from table
  (single-value-return))

(defx86lapfunction full-gccount ()
  (ref-global tenured-area arg_z)
  (testq (% arg_z) (% arg_z))
  (cmoveq (@ (+ x8664::nil-value (x8664::%kernel-global 'gc-count))) (% arg_z))
  (cmovneq (@ x8664::area.gc-count (% arg_z)) (% arg_z))
  (single-value-return))


(defx86lapfunction gc ()
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-gc) (% imm0))
  (uuo-gc-trap)
  (movq ($ nil) (% arg_z)
  (single-value-return))


(defx86lapfunction egc ((arg arg_z))
  "Enable the EGC if arg is non-nil, disables the EGC otherwise. Return
the previous enabled status. Although this function is thread-safe (in
the sense that calls to it are serialized), it doesn't make a whole lot
of sense to be turning the EGC on and off from multiple threads ..."
  (check-nargs 1)
  (clrq imm1)
  (cmp-reg-to-nil arg)
  (setne (% imm1.b))
  (movq ($ arch::gc-trap-function-egc-control) (% imm0))
  (uuo-gc-trap)
  (single-value-return))




(defx86lapfunction %configure-egc ((e0size arg_x)
				   (e1size arg_y)
				   (e2size arg_z))
  (check-nargs 3)
  (movq ($ arch::gc-trap-function-configure-egc) (% imm0))
  (uuo-gc-trap)
  (single-value-return))

(defx86lapfunction purify ()
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-purify) (% imm0))
  (uuo-gc-trap)
  (movq ($ nil) (% arg_z))
  (single-value-return))


(defx86lapfunction impurify ()
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-impurify) (% imm0))
  (uuo-gc-trap)
  (movq ($ nil) (% arg_z))
  (single-value-return))


(defx86lapfunction lisp-heap-gc-threshold ()
  "Return the value of the kernel variable that specifies the amount
of free space to leave in the heap after full GC."
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-get-lisp-heap-threshold) (% imm0))
  (uuo-gc-trap)
  #+x8632-target
  (jump-subprim .SPmakeu32)
  #+x8664-target
  (jump-subprim .SPmakeu64))

(defx86lapfunction set-lisp-heap-gc-threshold ((new arg_z))
  "Set the value of the kernel variable that specifies the amount of free
space to leave in the heap after full GC to new-value, which should be a
non-negative fixnum. Returns the value of that kernel variable (which may
be somewhat larger than what was specified)."
  (check-nargs 1)
  (mflr loc-pc)
  #+ppc32-target
  (bla .SPgetu32)
  #+ppc64-target
  (bla .SPgetu64)
  (mtlr loc-pc)
  (mr imm1 imm0)
  (li imm0 arch::gc-trap-function-set-lisp-heap-threshold)
  (trlgei allocptr 0)
  #+ppc32-target
  (ba .SPmakeu32)
  #+ppc64-target
  (ba .SPmakeu64))


(defx86lapfunction use-lisp-heap-gc-threshold ()
  "Try to grow or shrink lisp's heap space, so that the free space is(approximately) equal to the current heap threshold. Return NIL"
  (check-nargs 0) 
  (li imm0 arch::gc-trap-function-use-lisp-heap-threshold)
  (trlgei allocptr 0)
  (li arg_z nil)
  (single-value-return))



  


;;; offset is a fixnum, one of the x8664::kernel-import-xxx constants.
;;; Returns that kernel import, a fixnum.
(defx86lapfunction %kernel-import ((offset arg_z))
  (ref-global imm0 kernel-imports)
  (unbox-fixnum imm1 arg_z)
  (ldrx arg_z imm0 imm1)
  (single-value-return))

(defx86lapfunction %get-unboxed-ptr ((macptr arg_z))
  (macptr-ptr imm0 arg_z)
  (ldr arg_z 0 imm0)
  (single-value-return))


(defx86lapfunction %revive-macptr ((p arg_z))
  (li imm0 x8664::subtag-macptr)
  (stb imm0 x8664::misc-subtag-offset p)
  (single-value-return))

(defx86lapfunction %macptr-type ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svref imm0 x8664::macptr.type-cell p)
  (box-fixnum arg_z imm0)
  (single-value-return))
  
(defx86lapfunction %macptr-domain ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svref imm0 x8664::macptr.domain-cell p)
  (box-fixnum arg_z imm0)
  (single-value-return))

(defx86lapfunction %set-macptr-type ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svset imm1 x8664::macptr.type-cell p)
  (single-value-return))

(defx86lapfunction %set-macptr-domain ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svset imm1 x8664::macptr.domain-cell p)
  (single-value-return))

; end
