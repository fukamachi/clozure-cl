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


#+allow-in-package
(in-package "CCL")

(defppclapfunction %address-of ((arg arg_z))
  ; %address-of a fixnum is a fixnum, just for spite.
  ; %address-of anything else is the address of that thing as an integer.
  (clrlwi. imm0 arg (- 32 ppc32::nlisptagbits))
  (beqlr cr0)
  (mr imm0 arg_z)
  ; set cr0_eq if result fits in a fixnum
  (clrrwi. imm1 imm0 (- ppc32::least-significant-bit ppc32::nfixnumtagbits))
  (box-fixnum arg_z imm0)               ; assume it did
  (beqlr+ cr0)                          ; else arg_z tagged ok, but missing bits
  (ba .SPmakeu32)         ; put all bits in bignum.
)

#|
These things were always a bad idea.  Let's hope that nothing uses them.

; ??? is this too dangerous for PPC ?
(defppclapfunction %coerce-to-pointer ((arg arg_z))
  ; returns a (possibly invalid !) pointer iff its
  ; argument is a fixnum. 
  ; Screw: should accept an integer.
  (clrlwi. imm0 arg (- 32 ppc32::nlisptagbits))
  (bne @end)
  (unbox-fixnum arg_z arg)
  @end
  (blr))

; ??? is this too dangerous for PPC ?
(defppclapfunction %scale-pointer ((ptr arg_y) (offset arg_z))
  ; adds the unboxed fixnum (someday integer) offset to ptr, returning
  ; a new, probably invalid, pointer.
  (unbox-fixnum imm0 offset)
  (add arg_z ptr imm0)
  (blr))

; ??? is this too dangerous for PPC ?
(defppclapfunction %extract-pointer ((ptr arg_y) (offset arg_z))
  ; adds the unboxed fixnum (someday integer) offset and ptr, returning
  ; the contents of the addressed location.
  (unbox-fixnum imm0 offset)
  (add imm1 ptr imm0)
  (lwzx arg_z imm0 ptr)
  (blr))

|#

;;; "areas" are fixnum-tagged and, for the most part, so are their
;;; contents.

;;; The nilreg-relative global all-areas is a doubly-linked-list header
;;; that describes nothing.  Its successor describes the current/active
;;; dynamic heap.  Return a fixnum which "points to" that area, after
;;; ensuring that the "active" pointers associated with the current thread's
;;; stacks are correct.



(defppclapfunction %normalize-areas ()
  (let ((address imm0)
        (temp imm2))

    ; update active pointer for tsp area.
    (lwz address ppc32::tcr.ts-area rcontext)
    (stw tsp ppc32::area.active address)
    
    ;; Update active pointer for vsp area.
    (lwz address ppc32::tcr.vs-area rcontext)
    (stw vsp ppc32::area.active address)
    
    ; Update active pointer for SP area
    (lwz arg_z ppc32::tcr.cs-area rcontext)
    (stw sp ppc32::area.active arg_z)


    (ref-global arg_z all-areas)
    (lwz arg_z ppc32::area.succ arg_z)

    (blr)))

(defppclapfunction %active-dynamic-area ()
  (ref-global arg_z all-areas)
  (lwz arg_z ppc32::area.succ arg_z)
  (blr))

  
(defppclapfunction %object-in-stack-area-p ((object arg_y) (area arg_z))
  (lwz imm0 ppc32::area.active area)
  (cmplw cr0 object imm0)
  (lwz imm1 ppc32::area.high area)
  (cmplw cr1 object imm1)
  (li arg_z ppc32::nil-value)
  (bltlr cr0)
  (bgelr cr1)
  (la arg_z ppc32::t-offset arg_z)
  (blr))

(defppclapfunction %object-in-heap-area-p ((object arg_y) (area arg_z))
  (lwz imm0 ppc32::area.low area)
  (cmplw cr0 object imm0)
  (lwz imm1 ppc32::area.active area)
  (cmplw cr1 object imm1)
  (li arg_z ppc32::nil-value)
  (bltlr cr0)
  (bgelr cr1)
  (la arg_z ppc32::t-offset arg_z)
  (blr))



(defppclapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (limit save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm0))
    (save-lisp-context)
    (:regsave limit 0)
    (vpush fun)
    (vpush obj)
    (vpush limit)
    (mr fun f)
    (lwz limit ppc32::area.active a)
    (lwz obj ppc32::area.low a)
    (b @test)
    @loop
    (lwz header 0 obj)
    (extract-fulltag tag header)
    (cmpwi cr0 tag ppc32::fulltag-immheader)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la arg_z ppc32::fulltag-cons obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (la obj ppc32::cons.size obj)
    (b @test)
    @misc
    (la arg_z ppc32::fulltag-misc obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (lwz header 0 obj)
    (extract-fulltag tag header)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (clrlwi subtag header (- 32 ppc32::num-subtag-bits))
    (cmpwi cr2 subtag ppc32::max-32-bit-ivector-subtag)
    (cmpwi cr3 subtag ppc32::max-8-bit-ivector-subtag)
    (cmpwi cr4 subtag ppc32::max-16-bit-ivector-subtag)
    (cmpwi cr5 subtag ppc32::subtag-double-float-vector)
    (header-size elements header)
    (slwi bytes elements 2)
    (beq cr1 @bump)
    (ble cr2 @bump)
    (mr bytes elements)
    (ble cr3 @bump)
    (slwi bytes elements 1)
    (ble cr4 @bump)
    (slwi bytes elements 3)
    (beq cr5 @bump)
    (la elements 7 elements)
    (srwi bytes elements 3)
    @bump
    (la bytes (+ 4 7) bytes)
    (clrrwi bytes bytes 3)
    (add obj obj bytes)
    @test
    (cmplw :cr0 obj limit)
    (blt cr0 @loop)
    (vpop limit)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (blr)))

; This walks the active "dynamic" area.  Objects might be moving around
; while we're doing this, so we have to be a lot more careful than we 
; are when walking a static area.
; There's the vague notion that we can't take an interrupt when
; "initptr" doesn't equal "freeptr", though what kind of hooks into a
; preemptive scheduler we'd need to enforce this is unclear.  We use
; initptr as an untagged pointer here (and set it to freeptr when we've
; got a tagged pointer to the current object.)
; There are a couple of approaches to termination:
;  a) Allocate a "sentinel" cons, and terminate when we run into it.
;  b) Check the area limit (which is changing if we're consing) and
;     terminate when we hit it.
; (b) loses if the function conses.  (a) conses.  I can't think of anything
; better than (a).
; This, of course, assumes that any GC we're doing does in-place compaction
; (or at least preserves the relative order of objects in the heap.)


(defppclapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
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
    (cmpwi cr0 imm0 0)
    (li allocbase #xfff8)
    (la allocptr (- ppc32::fulltag-cons ppc32::cons.size) allocptr)
    (twllt allocptr allocbase)
    (mr sentinel allocptr)
    (clrrwi allocptr allocptr ppc32::ntagbits)
    (mr fun f)
    (if :ne
      (mr a imm0))    
    (lwz imm5 ppc32::area.low a)
    @loop
    (lwz header 0 imm5)
    (extract-fulltag tag header)
    (cmpwi cr0 tag ppc32::fulltag-immheader)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la obj ppc32::fulltag-cons imm5)
    (cmpw cr0 obj sentinel)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (beq cr0 @done)
    (bla .SPfuncall)
    (la imm5 (- ppc32::cons.size ppc32::fulltag-cons) obj)
    (b @loop)
    @misc
    (la obj ppc32::fulltag-misc imm5)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (getvheader header obj)
    (extract-fulltag tag header)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (cmpwi cr7 tag ppc32::fulltag-immheader)
    (clrlwi subtag header (- 32 ppc32::num-subtag-bits))
    (cmpwi cr2 subtag ppc32::max-32-bit-ivector-subtag)
    (cmpwi cr3 subtag ppc32::max-8-bit-ivector-subtag)
    (cmpwi cr4 subtag ppc32::max-16-bit-ivector-subtag)
    (cmpwi cr5 subtag ppc32::subtag-double-float-vector)
    (header-size elements header)
    (slwi bytes elements 2)
    (beq cr1 @bump)
    (if (:cr7 :ne)
      (twle 0 0))
    (ble cr2 @bump)
    (mr bytes elements)
    (ble cr3 @bump)
    (slwi bytes elements 1)
    (ble cr4 @bump)
    (slwi bytes elements 3)
    (beq cr5 @bump)
    (la elements 7 elements)
    (srwi bytes elements 3)
    @bump
    (la bytes (+ 4 7) bytes)
    (clrrwi bytes bytes 3)
    (subi imm5 obj ppc32::fulltag-misc)
    (add imm5 imm5 bytes)
    (cmpw cr0 imm5  sentinel)
    (blt cr0 @loop)
    (uuo_interr 0 0)
    (b @loop)
    @done
    (li arg_z ppc32::nil-value)
    (vpop sentinel)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (blr)))

(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))



(defppclapfunction %class-of-instance ((i arg_z))
  (svref arg_z instance.class-wrapper i)
  (svref arg_z %wrapper-class arg_z)
  (blr))

(defppclapfunction class-of ((x arg_z))
  (check-nargs 1)
  (extract-fulltag imm0 x)  ; low8bits-of from here to done
  (cmpwi cr0 imm0 ppc32::fulltag-misc)
  (beq cr0 @misc)
  (clrlslwi imm0 x 24 ppc32::fixnumshift)   ; clear left 24 bits, box assume = make byte index 
  (b @done)
  @misc
  (extract-subtag imm0 x)
  (box-fixnum imm0 imm0)  
  @done
  (addi imm0 imm0 ppc32::misc-data-offset)
  (lwz temp1 '*class-table* nfn)
  (lwz temp1 ppc32::symbol.vcell temp1)
  (lwzx temp0 temp1 imm0) ; get entry from table
  (cmpwi cr0 temp0 ppc32::nil-value)
  (beq @bad)
  ; functionp?
  (extract-typecode imm1 temp0)
  (cmpwi imm1 ppc32::subtag-function)
  (bne @ret)  ; not function - return entry
  ; else jump to the fn
  ;(lwz temp0 ppc32::function.codevector temp0) ; like jump_nfn asm macro
  (mr nfn temp0)
  (lwz temp0 ppc32::misc-data-offset temp0) ; get the ffing codevector
  (SET-NARGS 1) ; maybe not needed
  (mtctr temp0)
  (bctr)
  @bad
  (lwz fname 'no-class-error nfn)
  (ba .spjmpsym)
  @ret
  (mr arg_z temp0)  ; return frob from table
  (blr))

(defppclapfunction full-gccount ()
  (ref-global arg_z tenured-area)
  (cmpwi cr0 arg_z 0)
  (if :eq
    (ref-global arg_z gc-count)
    (lwz arg_z ppc32::area.gc-count arg_z))
  (blr))


(defppclapfunction gc ()
  (check-nargs 0)
  (li imm0 0)
  (twlgei allocptr 0)
  (li arg_z ppc32::nil-value)
  (blr))


(defppclapfunction egc ((arg arg_z))
  (check-nargs 1)
  (subi imm1 arg nil)
  (li imm0 32)
  (twlgei allocptr 0)
  (blr))



(defppclapfunction %configure-egc ((e0size arg_x)
				   (e1size arg_y)
				   (e2size arg_z))
  (check-nargs 3)
  (li imm0 64)
  (twlgei allocptr 0)
  (blr))

(defppclapfunction purify ()
  (li imm0 1)
  (twlgei allocptr 0)
  (li arg_z nil)
  (blr))


(defppclapfunction impurify ()
  (li imm0 2)
  (twlgei allocptr 0)
  (li arg_z nil)
  (blr))

(defppclapfunction lisp-heap-gc-threshold ()
  (check-nargs 0)
  (li imm0 16)
  (twlgei allocptr 0)
  (blr))

(defppclapfunction set-lisp-heap-gc-threshold ((new arg_z))
  (check-nargs 1)
  (li imm0 17)
  (unbox-fixnum imm1 arg_z)
  (twlgei allocptr 0)
  (blr))

(defppclapfunction use-lisp-heap-gc-threshold ()
  (check-nargs 0)
  (li imm0 18)
  (twlgei allocptr 0)
  (li arg_z nil)
  (blr))

;;; Returns two fixnums: low, high
(defppclapfunction macptr-to-fixnums ((macptr arg_z))
  (check-nargs 1)
  (trap-unless-typecode= macptr ppc32::subtag-macptr)
  (lwz imm0 ppc32::macptr.address macptr)
  (rlwinm imm1 imm0 2 14 29)
  (vpush imm1)
  (rlwinm imm1 imm0 18 14 29)
  (vpush imm1)
  (set-nargs 2)
  (la temp0 8 vsp)
  (ba .SPvalues))

;;; This does something much like what COMPOSE-DIGIT does (in the
;;; PPC/CMU-bignum code), only we HOPE that compose-digit disappears
;;; REAL SOON
(defppclapfunction %compose-unsigned-fullword ((high arg_y) (low arg_z))
  (rlwinm imm0 low (- 32 ppc32::fixnumshift) 16 31)
  (rlwimi imm0 high (- 16 ppc32::fixnumshift) 0 15)
  ;; Now have an unsigned fullword in imm0.  Box it.
  (clrrwi. imm1 imm0 (- ppc32::least-significant-bit ppc32::nfixnumtagbits))
  (box-fixnum arg_z imm0)             ; assume no high bits set.
  (beqlr+)
  (ba .SPmakeu32))

(defppclapfunction %compose-signed-fixnum ((high arg_y) (low arg_z))
  (rlwinm imm0 low (- 32 ppc32::fixnumshift) 16 31)
  (rlwimi imm0 high (- 16 ppc32::fixnumshift) 0 15)
  ;; Now have an unsigned fullword in imm0.  Box it.
  (box-fixnum arg_z imm0)
  (blr))


;;; offset is a fixnum, one of the ppc32::kernel-import-xxx.
;;; Returns that kernel import, a fixnum.
(defppclapfunction %kernel-import ((offset arg_z))
  (ref-global imm0 kernel-imports)
  (unbox-fixnum imm1 arg_z)
  (lwzx arg_z imm0 imm1)
  (blr))

(defppclapfunction %get-unboxed-ptr ((macptr arg_z))
  (macptr-ptr imm0 arg_z)
  (lwz arg_z 0 imm0)
  (blr))


(defppclapfunction %revive-macptr ((p arg_z))
  (li imm0 ppc32::subtag-macptr)
  (stb imm0 ppc32::misc-subtag-offset p)
  (blr))

(defppclapfunction %macptr-type ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p ppc32::subtag-macptr)
  (svref imm0 ppc32::macptr.type-cell p)
  (box-fixnum arg_z imm0)
  (blr))
  
(defppclapfunction %macptr-domain ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p ppc32::subtag-macptr)
  (svref imm0 ppc32::macptr.domain-cell p)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %set-macptr-type ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-typecode= p ppc32::subtag-macptr)
  (svset imm1 ppc32::macptr.type-cell p)
  (blr))

(defppclapfunction %set-macptr-domain ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-typecode= p ppc32::subtag-macptr)
  (svset imm1 ppc32::macptr.domain-cell p)
  (blr))

; end
