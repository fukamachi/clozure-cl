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
  (clrlwi. imm0 arg (- 32 arch::nlisptagbits))
  (beqlr cr0)
  (mr imm0 arg_z)
  ; set cr0_eq if result fits in a fixnum
  (clrrwi. imm1 imm0 (- arch::least-significant-bit arch::nfixnumtagbits))
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
  (clrlwi. imm0 arg (- 32 arch::nlisptagbits))
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
    (lwz address arch::tcr.ts-area rcontext)
    (stw tsp arch::area.active address)
    
    ;; Update active pointer for vsp area.
    (lwz address arch::tcr.vs-area rcontext)
    (stw vsp arch::area.active address)
    
    ; Update active pointer for SP area
    (lwz arg_z arch::tcr.cs-area rcontext)
    (stw sp arch::area.active arg_z)


    (ref-global arg_z all-areas)
    (lwz arg_z arch::area.succ arg_z)

    (blr)))

(defppclapfunction %active-dynamic-area ()
  (ref-global arg_z all-areas)
  (lwz arg_z arch::area.succ arg_z)
  (blr))

  
(defppclapfunction %object-in-stack-area-p ((object arg_y) (area arg_z))
  (lwz imm0 arch::area.active area)
  (cmplw cr0 object imm0)
  (lwz imm1 arch::area.high area)
  (cmplw cr1 object imm1)
  (li arg_z ppc::nil-value)
  (bltlr cr0)
  (bgelr cr1)
  (la arg_z arch::t-offset arg_z)
  (blr))

(defppclapfunction %object-in-heap-area-p ((object arg_y) (area arg_z))
  (lwz imm0 arch::area.low area)
  (cmplw cr0 object imm0)
  (lwz imm1 arch::area.active area)
  (cmplw cr1 object imm1)
  (li arg_z ppc::nil-value)
  (bltlr cr0)
  (bgelr cr1)
  (la arg_z arch::t-offset arg_z)
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
    (lwz limit arch::area.active a)
    (lwz obj arch::area.low a)
    (b @test)
    @loop
    (lwz header 0 obj)
    (extract-fulltag tag header)
    (cmpwi cr0 tag arch::fulltag-immheader)
    (cmpwi cr1 tag arch::fulltag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la arg_z arch::fulltag-cons obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (la obj arch::cons.size obj)
    (b @test)
    @misc
    (la arg_z arch::fulltag-misc obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (lwz header 0 obj)
    (extract-fulltag tag header)
    (cmpwi cr1 tag arch::fulltag-nodeheader)
    (clrlwi subtag header (- 32 arch::num-subtag-bits))
    (cmpwi cr2 subtag arch::max-32-bit-ivector-subtag)
    (cmpwi cr3 subtag arch::max-8-bit-ivector-subtag)
    (cmpwi cr4 subtag arch::max-16-bit-ivector-subtag)
    (cmpwi cr5 subtag arch::subtag-double-float-vector)
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
    (la allocptr (- arch::fulltag-cons arch::cons.size) allocptr)
    (twllt allocptr allocbase)
    (mr sentinel allocptr)
    (clrrwi allocptr allocptr arch::ntagbits)
    (mr fun f)
    (if :ne
      (mr a imm0))    
    (lwz imm5 arch::area.low a)
    @loop
    (lwz header 0 imm5)
    (extract-fulltag tag header)
    (cmpwi cr0 tag arch::fulltag-immheader)
    (cmpwi cr1 tag arch::fulltag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la obj arch::fulltag-cons imm5)
    (cmpw cr0 obj sentinel)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (beq cr0 @done)
    (bla .SPfuncall)
    (la imm5 (- arch::cons.size arch::fulltag-cons) obj)
    (b @loop)
    @misc
    (la obj arch::fulltag-misc imm5)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (getvheader header obj)
    (extract-fulltag tag header)
    (cmpwi cr1 tag arch::fulltag-nodeheader)
    (cmpwi cr7 tag arch::fulltag-immheader)
    (clrlwi subtag header (- 32 arch::num-subtag-bits))
    (cmpwi cr2 subtag arch::max-32-bit-ivector-subtag)
    (cmpwi cr3 subtag arch::max-8-bit-ivector-subtag)
    (cmpwi cr4 subtag arch::max-16-bit-ivector-subtag)
    (cmpwi cr5 subtag arch::subtag-double-float-vector)
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
    (subi imm5 obj arch::fulltag-misc)
    (add imm5 imm5 bytes)
    (cmpw cr0 imm5  sentinel)
    (blt cr0 @loop)
    (uuo_interr 0 0)
    (b @loop)
    @done
    (li arg_z ppc::nil-value)
    (vpop sentinel)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (blr)))

(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))








    


  

; end
