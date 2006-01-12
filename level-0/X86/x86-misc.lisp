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

;;; level-0;x86;x86-misc.lisp


(in-package "CCL")

;;; Copy N bytes from pointer src, starting at byte offset src-offset,
;;; to ivector dest, starting at offset dest-offset.
;;; It's fine to leave this in lap.
;;; Depending on alignment, it might make sense to move more than
;;; a byte at a time.
;;; Does no arg checking of any kind.  Really.

(defx86lapfunction %copy-ptr-to-ivector ((src (* 1 x8664::node-size) )
                                         (src-byte-offset 0) 
                                         (dest arg_x)
                                         (dest-byte-offset arg_y)
                                         (nbytes arg_z))
  (let ((src-reg imm0)
        (src-byteptr imm1)
        (src-node-reg temp0)
        (dest-byteptr imm2)
        (val imm3)
        (node-temp temp1))
    (cmpri cr0 nbytes 0)
    (ldr src-node-reg src vsp)
    (macptr-ptr src-reg src-node-reg)
    (ldr src-byteptr src-byte-offset vsp)
    (unbox-fixnum src-byteptr src-byteptr)
    (unbox-fixnum dest-byteptr dest-byte-offset)
    (la dest-byteptr x8664::misc-data-offset dest-byteptr)
    (b @test)
    @loop
    (subi nbytes nbytes '1)
    (cmpri cr0 nbytes '0)
    (lbzx val src-reg src-byteptr)
    (la src-byteptr 1 src-byteptr)
    (stbx val dest dest-byteptr)
    (la dest-byteptr 1 dest-byteptr)
    @test
    (bne cr0 @loop)
    (mr arg_z dest)
    (la vsp '2 vsp)
    (single-value-return)))

(defx86lapfunction %copy-ivector-to-ptr ((src (* 1 x8664::node-size))
                                         (src-byte-offset 0) 
                                         (dest arg_x)
                                         (dest-byte-offset arg_y)
                                         (nbytes arg_z))
  (ldr temp0 src vsp)
  (cmpri cr0 nbytes 0)
  (ldr imm0 src-byte-offset vsp)
  (unbox-fixnum imm0 imm0)
  (la imm0 x8664::misc-data-offset imm0)
  (unbox-fixnum imm2 dest-byte-offset)
  (ldr imm1 x8664::macptr.address dest)
  (b @test)
  @loop
  (subi nbytes nbytes '1)
  (cmpri cr0 nbytes 0)
  (lbzx imm3 temp0 imm0)
  (addi imm0 imm0 1)
  (stbx imm3 imm1 imm2)
  (addi imm2 imm2 1)
  @test
  (bne cr0 @loop)
  (mr arg_z dest)
  (la vsp '2 vsp)
  (single-value-return))

#+x8632-target
(defx86lapfunction %copy-ivector-to-ivector ((src 4) 
                                             (src-byte-offset 0) 
                                             (dest arg_x)
                                             (dest-byte-offset arg_y)
                                             (nbytes arg_z))
  (lwz temp0 src vsp)
  (cmpwi cr0 nbytes 0)
  (cmpw cr2 temp0 dest)   ; source and dest same?
  (rlwinm imm3 nbytes 0 (- 30 x8664::fixnum-shift) 31)  
  (lwz imm0 src-byte-offset vsp)
  (rlwinm imm1 imm0 0 (- 30 x8664::fixnum-shift) 31)
  (or imm3 imm3 imm1)
  (unbox-fixnum imm0 imm0)
  (la imm0 x8664::misc-data-offset imm0)
  (unbox-fixnum imm2 dest-byte-offset)
  (rlwimi imm1 imm2 0 30 31)
  (or imm3 imm3 imm1)
  (cmpwi cr1 imm3 0)  ; is everybody multiple of 4?
  (la imm2 x8664::misc-data-offset imm2)
  (beq cr2 @SisD)   ; source and dest same
  @fwd
  (beq :cr1 @wtest)
  (b @test)

  @loop
  (subi nbytes nbytes '1)
  (cmpwi cr0 nbytes 0)
  (lbzx imm3 temp0 imm0)
  (addi imm0 imm0 1)
  (stbx imm3 dest imm2)
  (addi imm2 imm2 1)
  @test
  (bne cr0 @loop)
  (mr arg_z dest)
  (la vsp 8 vsp)
  (single-value-return)

  @words      ; source and dest different - words 
  (subi nbytes nbytes '4)  
  (cmpwi cr0 nbytes 0)
  (lwzx imm3 temp0 imm0)
  (addi imm0 imm0 4)
  (stwx imm3 dest imm2)
  (addi imm2 imm2 4)
  @wtest
  (bgt cr0 @words)
  @done
  (mr arg_z dest)
  (la vsp 8 vsp)
  (single-value-return)

  @SisD
  (cmpw cr2 imm0 imm2) ; cmp src and dest
  (bgt cr2 @fwd)
  ;(B @bwd) 
  

  ; Copy backwards when src & dest are the same and we're sliding down
  @bwd ; ok
  (unbox-fixnum imm3 nbytes)
  (add imm0 imm0 imm3)
  (add imm2 imm2 imm3)
  (b @test2)
  @loop2
  (subi nbytes nbytes '1)
  (cmpwi cr0 nbytes 0)
  (subi imm0 imm0 1)
  (lbzx imm3 temp0 imm0)
  (subi imm2 imm2 1)
  (stbx imm3 dest imm2)
  @test2
  (bne cr0 @loop2)
  (b @done))

#+x8664-target
(defx86lapfunction %copy-ivector-to-ivector ((src-offset 8) 
                                             (src-byte-offset-offset 0) 
                                             (dest arg_x)
                                             (dest-byte-offset arg_y)
                                             (nbytes arg_z))
  (let ((src temp0)
        (src-byte-offset imm0))
    (subi nbytes nbytes '1)
    (ld src-byte-offset src-byte-offset-offset vsp)
    (cmpdi nbytes 0 )
    (ld src src-offset vsp)
    (la vsp '2 vsp)
    (cmpd cr1 src dest)
    (cmpdi cr2 src-byte-offset dest-byte-offset)
    (unbox-fixnum src-byte-offset src-byte-offset)
    (unbox-fixnum imm1 dest-byte-offset)
    (la imm0 x8664::misc-data-offset src-byte-offset)
    (la imm1 x8664::misc-data-offset imm1)
    (bne cr1 @test)
    ;; Maybe overlap, or maybe nothing to do.
    (beq cr2 @done)                       ; same vectors, same offsets
    (blt cr2 @back)                       ; copy backwards, avoid overlap
    (b @test)
    @loop
    (subi nbytes nbytes '1)
    (lbzx imm3 src imm0)
    (cmpdi nbytes 0)
    (addi imm0 imm0 1)
    (stbx imm3 dest imm1)
    (addi imm1 imm1 1)
    @test
    (bge @loop)
    @done
    (mr arg_z dest)
    (single-value-return)
    @back
    ;; nbytes was predecremented above
    (unbox-fixnum imm2 nbytes)
    (add imm0 imm2 imm0)
    (add imm1 imm2 imm1)
    (b @back-test)
    @back-loop
    (subi nbytes nbytes '1)
    (lbzx imm3 src imm0)
    (cmpdi nbytes 0)
    (subi imm0 imm0 1)
    (stbx imm3 dest imm1)
    (subi imm1 imm1 1)
    @back-test
    (bge @back-loop)
    (mr arg_z dest)
    (single-value-return)))
  

(defx86lapfunction %copy-gvector-to-gvector ((src (* 1 x8664::node-size))
					     (src-element 0)
					     (dest arg_x)
					     (dest-element arg_y)
					     (nelements arg_z))
  (subi nelements nelements '1)
  (cmpri nelements 0)
  (ldr imm0 src-element vsp)
  (ldr temp0 src vsp)
  (la vsp '2 vsp)
  (cmpr cr1 temp0 dest)
  (cmpri cr2 src-element dest-element)
  (la imm0 x8664::misc-data-offset imm0)
  (la imm1 x8664::misc-data-offset dest-element)
  (bne cr1 @test)
  ;; Maybe overlap, or maybe nothing to do.
  (beq cr2 @done)                       ; same vectors, same offsets
  (blt cr2 @back)                       ; copy backwards, avoid overlap
  (b @test)
  @loop
  (subi nelements nelements '1)
  (cmpri nelements 0)
  (ldrx temp1 temp0 imm0)
  (addi imm0 imm0 '1)
  (strx temp1 dest imm1)
  (addi imm1 imm1 '1)
  @test
  (bge @loop)
  @done
  (mr arg_z dest)
  (single-value-return)
  @back
  ;; We decremented NELEMENTS by 1 above.
  (add imm1 nelements imm1)
  (add imm0 nelements imm0)
  (b @back-test)
  @back-loop
  (subi nelements nelements '1)
  (cmpri nelements 0)
  (ldrx temp1 temp0 imm0)
  (subi imm0 imm0 '1)
  (strx temp1 dest imm1)
  (subi imm1 imm1 '1)
  @back-test
  (bge @back-loop)
  (mr arg_z dest)
  (single-value-return))
  
  



#+x8632-target
(defx86lapfunction %heap-bytes-allocated ()
  (lwz imm2 x8664::tcr.last-allocptr x8632::rcontext)
  (cmpwi cr1 imm2 0)
  (cmpwi allocptr -8)			;void_allocptr
  (lwz imm0 x8664::tcr.total-bytes-allocated-high x8632::rcontext)
  (lwz imm1 x8664::tcr.total-bytes-allocated-low x8632::rcontext)
  (sub imm2 imm2 allocptr)
  (beq cr1 @go)
  (beq @go)
  (addc imm1 imm1 imm2)
  (addze imm0 imm0)
  @go
  (ba .SPmakeu64))


(defx86lapfunction %heap-bytes-allocated ()
  (movq (@ (% rcontext) x8664::tcr.last-allocptr) (% temp0))
  (movq (@ (% rcontext) x8664::tcr.save-allocptr) (% temp1))
  (movq (@ (% rcontext) x8664::tcr.total-bytes-allocated) (% imm0))
  (movq (% temp0) (% temp2))
  (subq (% temp1) (% temp0))
  (testq (% temp2) (% temp2))
  (jz @go)
  (add (% temp0) (% imm0))
  @go
  (jump-subprim .SPmakeu64))


(defx86lapfunction values ()
  (vpush-argregs)
  (add temp0 nargs vsp)
  (ba .SPvalues))

;;; It would be nice if (%setf-macptr macptr (ash (the fixnum value)
;;; ash::fixnumshift)) would do this inline.

(defx86lapfunction %setf-macptr-to-object ((macptr arg_y) (object arg_z))
  (check-nargs 2)
  (trap-unless-typecode= arg_y x8664::subtag-macptr)
  (movq (% arg_z) (@ x8664::macptr.address (% arg_y)))
  (single-value-return))

(defx86lapfunction %fixnum-from-macptr ((macptr arg_z))
  (check-nargs 1)
  (trap-unless-typecode= arg_z x8664::subtag-macptr)
  (movq (@ x8664::macptr.address (% arg_z)) (% imm0))
  (trap-unless-lisptag= imm0 x8664::tag-fixnum imm1)
  (movq (% imm0) (% arg_z))
  (single-value-return))


(defx86lapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (macptr-ptr ptr imm1)
  (unbox-fixnum imm0 offset)
  (movq (@ (% imm1) (% imm0)) (% imm0))
  (jmp-subprim .SPmakeu64))


(defx86lapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (macptr-ptr ptr imm1)
  (unbox-fixnum imm0 offset)
  (movq (@ (% imm1) (% imm0)) (% imm0))
  (jmp-subprim .SPmakes64))




(defx86lapfunction %%set-unsigned-longlong ((ptr arg_x)
                                            (offset arg_y)
                                            (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (bla .SPgetu64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (stdx imm0 imm3 imm2)
  (ba .SPpopj))


#+x8664-target
(defx86lapfunction %%set-signed-longlong ((ptr arg_x)
                                          (offset arg_y)
                                          (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (bla .SPgets64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (stdx imm0 imm3 imm2)
  (ba .SPpopj))

(defx86lapfunction interrupt-level ()
  (ldr arg_z x8664::tcr.tlb-pointer x8664::rcontext)
  (ldr arg_z x8664::interrupt-level-binding-index arg_z)
  (single-value-return))


(defx86lapfunction disable-lisp-interrupts ()
  (li imm0 '-1)
  (ldr imm1 x8664::tcr.tlb-pointer x8664::rcontext)
  (ldr arg_z x8664::interrupt-level-binding-index imm1)
  (str imm0 x8664::interrupt-level-binding-index imm1)
  (single-value-return))

(defx86lapfunction set-interrupt-level ((new arg_z))
  (ldr imm1 x8664::tcr.tlb-pointer x8664::rcontext)
  (trap-unless-lisptag= new x8664::tag-fixnum imm0)
  (str new x8664::interrupt-level-binding-index imm1)
  (single-value-return))

;;; If we're restoring the interrupt level to 0 and an interrupt
;;; was pending, restore the level to 1 and zero the pending status.
(defx86lapfunction restore-interrupt-level ((old arg_z))
  (cmpri :cr1 old 0)
  (ldr imm0 x8664::tcr.interrupt-pending x8664::rcontext)
  (ldr imm1 x8664::tcr.tlb-pointer x8664::rcontext)
  (cmpri :cr0 imm0 0)
  (bne :cr1 @store)
  (beq :cr0 @store)
  (str rzero x8664::tcr.interrupt-pending x8664::rcontext)
  (li old '1)
  @store
  (str old x8664::interrupt-level-binding-index imm1)
  (single-value-return))



(defx86lapfunction %current-tcr ()
  (movq (@ (% rcontext) x8664::tcr.linear-end) (% arg_z))
  (single-value-return))

(defx86lapfunction %tcr-toplevel-function ((tcr arg_z))
  (check-nargs 1)
  (cmpr tcr x8664::rcontext)
  (mr imm0 vsp)
  (ldr temp0 x8664::tcr.vs-area tcr)
  (ldr imm1 x8664::area.high temp0)
  (beq @room)
  (ldr imm0 x8664::area.active temp0)
  @room
  (cmpr imm1 imm0)
  (li arg_z nil)
  (beqlr)
  (ldr arg_z (- x8664::node-size) imm1)
  (single-value-return))

(defx86lapfunction %set-tcr-toplevel-function ((tcr arg_y) (fun arg_z))
  (check-nargs 2)
  (cmpr tcr x8664::rcontext)
  (mr imm0 vsp)
  (ldr temp0 x8664::tcr.vs-area tcr)
  (ldr imm1 x8664::area.high temp0)
  (beq @check-room)
  (ldr imm0 x8664::area.active temp0)
  @check-room
  (cmpr imm1 imm0)
  (push rzero imm1)
  (bne @have-room)
  (str imm1 x8664::area.active temp0)
  (str imm1 x8664::tcr.save-vsp tcr)
  @have-room
  (str fun 0 imm1)
  (single-value-return))

;;; This needs to be done out-of-line, to handle EGC memoization.
(defx86lapfunction %store-node-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (ba .SPstore-node-conditional))

(defx86lapfunction %store-immediate-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (vpop temp0)
  (unbox-fixnum imm0 temp0)
  (let ((current temp1))
    @again
    (lrarx current object imm0)
    (cmpr current old)
    (bne @lose)
    (strcx. new object imm0)
    (bne @again)
    (isync)
    (li arg_z (+ x8664::t-offset x8664::nil-value))
    (single-value-return)
    @lose
    (li imm0 x8664::reservation-discharge)
    (strcx. rzero rzero imm0)
    (li arg_z nil)
    (single-value-return)))

(defx86lapfunction set-%gcable-macptrs% ((ptr x8664::arg_z))
  (li imm0 (+ x8664::nil-value (x8664::kernel-global gcable-pointers)))
  @again
  (lrarx arg_y rzero imm0)
  (str arg_y x8664::xmacptr.link ptr)
  (strcx. ptr rzero imm0)
  (bne @again)
  (isync)
  (single-value-return))

;;; Atomically increment or decrement the gc-inhibit-count kernel-global
;;; (It's decremented if it's currently negative, incremented otherwise.)
(defx86lapfunction %lock-gc-lock ()
  @again
  (movq (@ (+ x8664::nil-value (x8664::kernel-global gc-inhibit-count))) (% rax))
  (lea (@ '-1 (% rax)) (% temp0))
  (lea (@ '1 (% rax)) (% arg_z))
  (testq (% rax) (% rax))
  (cmovsq (% temp0) (% arg_z))
  (lock)
  (cmpxchgq (% temp1) (@ (+ x8664::nil-value (x8664::kernel-global gc-inhibit-count))))
  (jnz @again)
  (single-value-return))

;;; Atomically decrement or increment the gc-inhibit-count kernel-global
;;; (It's incremented if it's currently negative, incremented otherwise.)
;;; If it's incremented from -1 to 0, try to GC (maybe just a little.)
(defx86lapfunction %unlock-gc-lock ()
;;  (sync)
  (li imm0 (+ x8664::nil-value (x8664::kernel-global gc-inhibit-count)))
  @again
  (lrarx arg_y rzero imm0)
  (cmpri cr1 arg_y -1)
  (subi arg_z arg_y '1)
  (bgt cr1 @store)
  (addi arg_z arg_y '1)
  @store
  (strcx. arg_z rzero imm0)
  (bne @again)
  (bnelr cr1)
  ;; The GC tried to run while it was inhibited.  Unless something else
  ;; has just inhibited it, it should be possible to GC now.
  (mov ($ arch::gc-trap-function-immediate-gc) (% imm0))
  (uuo-gc-trap)
  (single-value-return))

;;; Return true iff we were able to increment a non-negative
;;; lock._value
(defx86lapfunction %try-read-lock-rwlock ((lock arg_z))
  (check-nargs 1)
  (li imm1 x8664::lock._value)
  @try
  (lrarx imm0 lock imm1)
  (cmpri imm0 0)
  (blt @fail)				; locked for writing
  (addi imm0 imm0 '1)
  (strcx. imm0 lock imm1)
  (bne @try)                            ; lost reservation, try again
  (isync)
  (single-value-return)                                 ; return the lock
@fail
  (li imm0 x8664::reservation-discharge)
  (strcx. rzero rzero imm0)
  (li arg_z nil)
  (single-value-return))



(defx86lapfunction unlock-rwlock ((lock arg_z))
  (ldr imm2 x8664::lock._value lock)
  (cmpri imm2 0)
  (li imm1 x8664::lock._value)
  (ble @unlock-write)
  @unlock-read
  (lrarx imm0 lock imm1)
  (subi imm0 imm0 '1)
  (strcx. imm0 lock imm1)
  (bne @unlock-read)
  (isync)
  (single-value-return)
  @unlock-write
  ;;; If we aren't the writer, return NIL.
  ;;; If we are and the value's about to go to 0, clear the writer field.
  (ldr imm0 x8664::lock.writer lock)
  (cmpr imm0 x8664::rcontext)
  (ldrx imm0 lock imm1)
  (cmpri cr1 imm0 '-1)
  (addi imm0 imm0 '1)
  (bne @fail)
  (bne cr1 @noclear)
  (str rzero x8664::lock.writer lock)
  @noclear
  (str imm0 x8664::lock._value lock)
  (single-value-return)
  @fail
  (li arg_z nil)
  (single-value-return))

(defx86lapfunction %atomic-incf-node ((by arg_x) (node arg_y) (disp arg_z))
  (check-nargs 3)
  (unbox-fixnum imm1 disp)
  @again
  (lrarx arg_z node imm1)
  (add arg_z arg_z by)
  (strcx. arg_z node imm1)
  (bne- @again)
  (isync)
  (single-value-return))

(defx86lapfunction %atomic-incf-ptr ((ptr arg_z))
  (macptr-ptr ptr imm1)
  (movq (% rbp) (% temp0))
  @again
  (movq (@ (% imm1)) (% rax))
  (lea (@ 1 (% rax)) (% rbp))
  (lock)
  (cmpxchgq (% rbp) (@ (% imm1)))
  (jnz @again)
  (box-fixnum rbp arg_z)
  (movq (% temp0) (% rbp))
  (single-value-return))

(defx86lapfunction %atomic-incf-ptr-by ((ptr arg_y) (by arg_z))
  (macptr-ptr ptr imm1)
  (movq (% rbp) (% temp0))
  (unbox-fixnum imm2 by)
  @again
  (movq (@ (% imm1)) (% rax))
  (unbox-fixnum by rbp)
  (add (% rax) (% rbp))
  (lock)
  (cmpxchgq %rbp (@ (% imm1)))
  (jnz @again)
  (box-fixnum rbp arg_z)
  (movq (% temp0) (% rbp))
  (single-value-return))


(defx86lapfunction %atomic-decf-ptr ((ptr arg_z))
  (macptr-ptr ptr imm1)
  (movq (% rbp) (% temp0))
  @again
  (movq (@ (% imm1)) (% rax))
  (lea (@ -1 (% rax)) (% rbp))
  (lock)
  (cmpxchgq (% rbp) (@ (% imm1)))
  (jnz @again)
  (box-fixnum rbp arg_z)
  (movq (% temp0) (% rbp))
  (single-value-return))

(defx86lapfunction %atomic-decf-ptr-if-positive ((ptr arg_z))
  (macptr-ptr ptr imm1)
  (movq (% rbp) (% temp0))
  @again
  (movq (@ (% imm1)) (% rax))
  (testq (% rax) (% rax))
  (lea (@ -1 (% rax)) (% rbp))
  (jz @done)
  (lock)
  (cmpxchgq (% rbp) (@ (% imm1)))
  (jnz @again)
  (box-fixnum rbp arg_z)
  (movq (% temp0) (% rbp))
  (single-value-return)
  @done
  (movq (% temp0) (% rbp))
  (box-fixnum rax arg_z)
  (single-value-return))


(defx86lapfunction %atomic-swap-ptr ((ptr arg_y) (newval arg_z))
  (macptr-ptr arg_y imm1)
  (unbox-fixnum newval imm0)
  (lock)
  (xchgq (% imm0) (@ (% imm1)))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; Try to store the fixnum NEWVAL at PTR, if and only if the old value
;;; was equal to OLDVAL.  Return the old value
(defx86lapfunction %ptr-store-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (macptr-ptr imm0 ptr)
  (unbox-fixnum imm1 expected-oldval)
  (unbox-fixnum imm2 newval)
  @again
  (lrarx imm3 0 imm0)
  (cmpr imm3 imm1)
  (bne- @done)
  (strcx. imm2 0 imm0)
  (bne- @again)
  (isync)
  (box-fixnum arg_z imm3)
  (single-value-return)
  @done
  (li imm0 x8664::reservation-discharge)
  (box-fixnum arg_z imm3)
  (strcx. rzero 0 imm0)
  (single-value-return))


(defx86lapfunction %macptr->dead-macptr ((macptr arg_z))
  (check-nargs 1)
  (movb ($ x8664::subtag-dead-macptr) (@ x8664::misc-subtag-offset (% macptr)))
  (single-value-return))

#+are-you-kidding
(defx86lapfunction %%apply-in-frame ((catch-count imm0) (srv temp0) (tsp-count imm0) (db-link imm0)
                                     (parent arg_x) (function arg_y) (arglist arg_z))
  (check-nargs 7)

  ; Throw through catch-count catch frames
  (lwz imm0 12 vsp)                      ; catch-count
  (vpush parent)
  (vpush function)
  (vpush arglist)
  (bla .SPnthrowvalues)

  ; Pop tsp-count TSP frames
  (lwz tsp-count 16 vsp)
  (cmpi cr0 tsp-count 0)
  (b @test)
@loop
  (subi tsp-count tsp-count '1)
  (cmpi cr0 tsp-count 0)
  (lwz tsp 0 tsp)
@test
  (bne cr0 @loop)

  ; Pop dynamic bindings until we get to db-link
  (lwz imm0 12 vsp)                     ; db-link
  (lwz imm1 x8664::tcr.db-link x8664::rcontext)
  (cmp cr0 imm0 imm1)
  (beq cr0 @restore-regs)               ; .SPunbind-to expects there to be something to do
  (bla .SPunbind-to)

@restore-regs
  ; restore the saved registers from srv
  (lwz srv 20 vsp)
@get0
  (svref imm0 1 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @get1)
  (lwz save0 0 imm0)
@get1
  (svref imm0 2 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @get2)
  (lwz save1 0 imm0)
@get2
  (svref imm0 3 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @get3)
  (lwz save2 0 imm0)
@get3
  (svref imm0 4 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @get4)
  (lwz save3 0 imm0)
@get4
  (svref imm0 5 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @get5)
  (lwz save4 0 imm0)
@get5
  (svref imm0 6 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @get6)
  (lwz save5 0 imm0)
@get6
  (svref imm0 7 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @get7)
  (lwz save6 0 imm0)
@get7
  (svref imm0 8 srv)
  (cmpwi cr0 imm0 x8664::nil-value)
  (beq @got)
  (lwz save7 0 imm0)
@got

  (vpop arg_z)                          ; arglist
  (vpop temp0)                          ; function
  (vpop parent)                         ; parent
  (extract-lisptag imm0 parent)
  (cmpi cr0 imm0 x8664::tag-fixnum)
  (if (:cr0 :ne)
    ; Parent is a fake-stack-frame. Make it real
    (progn
      (svref sp %fake-stack-frame.sp parent)
      (stwu sp (- x8664::lisp-frame.size) sp)
      (svref fn %fake-stack-frame.fn parent)
      (stw fn x8664::lisp-frame.savefn sp)
      (svref temp1 %fake-stack-frame.vsp parent)
      (stw temp1 x8664::lisp-frame.savevsp sp)
      (svref temp1 %fake-stack-frame.lr parent)
      (extract-lisptag imm0 temp1)
      (cmpi cr0 imm0 x8664::tag-fixnum)
      (if (:cr0 :ne)
        ;; must be a macptr encoding the actual link register
        (macptr-ptr loc-pc temp1)
        ;; Fixnum is offset from start of function vector
        (progn
          (svref temp2 0 fn)        ; function vector
          (unbox-fixnum temp1 temp1)
          (add loc-pc temp2 temp1)))
      (stw loc-pc x8664::lisp-frame.savelr sp))
    ;; Parent is a real stack frame
    (mr sp parent))
  (set-nargs 0)
  (bla .SPspreadargz)
  (ba .SPtfuncallgen))



  
(defx86lapfunction %%save-application ((flags arg_y) (fd arg_z))
  (unbox-fixnum flags imm0)
  (orq ($ arch::gc-trap-function-save-application) (% imm0))
  (unbox-fixnum fd imm1)
  (uuo-gc-trap)
  (single-value-return))



(defx86lapfunction %misc-address-fixnum ((misc-object arg_z))
  (check-nargs 1)
  (lea (@ x8664::misc-data-offset (% misc-object)) (% arg_z))
  (single-value-return))


(defx86lapfunction fudge-heap-pointer ((ptr arg_x) (subtype arg_y) (len arg_z))
  (check-nargs 3)
  (macptr-ptr imm1 ptr) ; address in macptr
  (addi imm0 imm1 17)     ; 2 for delta + 15 for alignment
  (clrrdi imm0 imm0 4)   ; Clear low four bits to align
  (subf imm1 imm1 imm0)  ; imm1 = delta
  (sth imm1 -2 imm0)     ; save delta halfword
  (unbox-fixnum imm1 subtype)  ; subtype at low end of imm1
  (sldi imm2 len (- x8664::num-subtag-bits x8664::fixnum-shift))
  (or imm1 imm2 imm1)
  (std imm1 0 imm0)       ; store subtype & length
  (addi arg_z imm0 x8664::fulltag-misc) ; tag it, return it
  (single-value-return))

(defx86lapfunction %%make-disposable ((ptr arg_y) (vector arg_z))
  (check-nargs 2)
  (lea (@ (- x8664::fulltag-misc) (% vector)) (% imm0)) ; imm0 is addr = vect less tag
  (movzwq (@ -2 (% imm0)) (% imm1))     ; get delta
  (subq (% imm1) (% imm0))              ; vector addr (less tag)  - delta is orig addr
  (movq (% imm0) (@ x8664::macptr.address (% ptr)))
  (single-value-return))


(defx86lapfunction %vect-data-to-macptr ((vect arg_y) (ptr arg_z))
  (lea (@ x8664::misc-data-offset (% vect)) (% imm0))
  (movq (% imm0) (@ x8664::macptr.address (% ptr)))
  (single-value-return))

(defx86lapfunction get-saved-register-values ()
  (movq (% rsp) (% temp0))
  (push (% save0))
  (push (% save1))
  (push (% save2))
  (push (% save3))
  (set-nargs 4)
  (jump-subprim .SPvalues))


(defx86lapfunction %current-db-link ()
  (movq (@ (% rcontext) x8664::tcr.db-link) (% arg_z))
  (single-value-return))

(defx86lapfunction %no-thread-local-binding-marker ()
  (movq ($ x8664::subtag-no-thread-local-binding) (% arg_z))
  (single-value-return))


(defx86lapfunction break-event-pending-p ()
  (xorq (% imm0) (% imm0))
  (ref-global x8664::intflag arg_z)
  (set-global imm0 x8664::intflag)
  (movq ($ t) (% imm0))
  (testq (% arg_z) (% arg_z))
  (movq ($ nil) (% arg_z))
  (cmovneq (% imm0) (% arg_z))
  (single-value-return))

; end of x86-misc.lisp
