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

;;; level-0;ppc;ppc-misc.lisp


;(in-package "CCL")

(eval-when (:execute :compile-toplevel)
  (defppclapmacro get-arg (dest arg)
    `(lwz ,dest ,arg vsp))

  (defppclapmacro bignum-ref (dest src index)
    `(lwz ,dest (+ ppc32::misc-data-offset (ash ,index 2)) ,src))

  (defppclapmacro get-hv (h v pt)
    (let ((lbl-got (gensym)))
      `(progn
         ;(eq-if-fixnum 0 ,h ,pt)
         (clrlwi. ,h ,pt (- ppc32::nbits-in-word ppc32::nlisptagbits))
         (unbox-fixnum ,h ,pt)
         (beq+ cr0 ,lbl-got)
         ; Should probably branch around a uuo_interr ppc32::error-object-not-signed-byte-32
         (trap-unless-typecode= ,pt ppc32::subtag-bignum ,h)
         (bignum-ref ,h ,pt 0)
         ,lbl-got                       ; now "h" has (signed-byte 32): vvvvhhhh
         (srawi ,v ,h 16)
         (extsh ,h ,h))))


  )


; Copy N bytes from pointer src, starting at byte offset src-offset,
; to ivector dest, starting at offset dest-offset.
; It's fine to leave this in lap.
; Depending on alignment, it might make sense to move more than
; a byte at a time.
; Does no arg checking of any kind.  Really.

(defppclapfunction %copy-ptr-to-ivector ((src 4) 
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
    (cmpwi cr0 nbytes 0)
    (get-arg src-node-reg src)
    (lwz src-reg ppc32::macptr.address src-node-reg)
    (get-arg src-byteptr src-byte-offset)
    (unbox-fixnum src-byteptr src-byteptr)
    (unbox-fixnum dest-byteptr dest-byte-offset)
    (la dest-byteptr ppc32::misc-data-offset dest-byteptr)
    (b @test)
    @loop
    (subi nbytes nbytes '1)
    (cmpwi cr0 nbytes '0)
    (lbzx val src-reg src-byteptr)
    (la src-byteptr 1 src-byteptr)
    (stbx val dest dest-byteptr)
    (la dest-byteptr 1 dest-byteptr)
    @test
    (bne cr0 @loop)
    (mr arg_z dest)
    (la vsp 8 vsp)
    (blr)))

; %copy-ivector-to-ptr - from hello.lisp:
(defppclapfunction %copy-ivector-to-ptr ((src 4) 
                                         (src-byte-offset 0) 
                                         (dest arg_x)
                                         (dest-byte-offset arg_y)
                                         (nbytes arg_z))
  (lwz temp0 src vsp)
  (cmpwi cr0 nbytes 0)
  (lwz imm0 src-byte-offset vsp)
  (unbox-fixnum imm0 imm0)
  (la imm0 ppc32::misc-data-offset imm0)
  (unbox-fixnum imm2 dest-byte-offset)
  (lwz imm1 ppc32::macptr.address dest)
  (b @test)
  @loop
  (subi nbytes nbytes '1)
  (cmpwi cr0 nbytes 0)
  (lbzx imm3 temp0 imm0)
  (addi imm0 imm0 1)
  (stbx imm3 imm1 imm2)
  (addi imm2 imm2 1)
  @test
  (bne cr0 @loop)
  (mr arg_z dest)
  (la vsp 8 vsp)
  (blr))

(defppclapfunction %copy-ivector-to-ivector ((src 4) 
                                             (src-byte-offset 0) 
                                             (dest arg_x)
                                             (dest-byte-offset arg_y)
                                             (nbytes arg_z))
  (lwz temp0 src vsp)
  (cmpwi cr0 nbytes 0)
  (cmpw cr2 temp0 dest)   ; source and dest same?
  (rlwinm imm3 nbytes 0 (- 30 ppc32::fixnum-shift) 31)  
  (lwz imm0 src-byte-offset vsp)
  (rlwinm imm1 imm0 0 (- 30 ppc32::fixnum-shift) 31)
  (or imm3 imm3 imm1)
  (unbox-fixnum imm0 imm0)
  (la imm0 ppc32::misc-data-offset imm0)
  (unbox-fixnum imm2 dest-byte-offset)
  (rlwimi imm1 imm2 0 30 31)
  (or imm3 imm3 imm1)
  (cmpwi cr1 imm3 0)  ; is everybody multiple of 4?
  (la imm2 ppc32::misc-data-offset imm2)
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
  (blr)

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
  (blr)

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



; value will be in save7 = r24
(defppclapfunction %dbg ((arg arg_z))    ; (&optional arg)
  (check-nargs 0 1)                      ; optional
  (save-lisp-context)
  (vpush save7)
  (cmpw cr0 nargs rzero)
  (if (:cr0 :eq)
    (li arg_z ppc32::nil-value))
  (mr save7 arg)
  (set-nargs 0)
  (call-symbol Debugger)               ; can't (easily) call "traps" inline
  (vpop save7)
  (restore-full-lisp-context)
  (blr))

(defvar *debugger-slep* nil)

(eval-when (:compile-toplevel :execute)
  (declaim (type t *debugger-slep*)))







(defppclapfunction %heap-bytes-allocated ()
  (lwz imm2 ppc32::tcr.last-allocptr rcontext)
  (cmpwi cr1 imm2 0)
  (cmpwi allocptr -8)			;void_allocptr
  (lwz imm0 ppc32::tcr.total-bytes-allocated-high rcontext)
  (lwz imm1 ppc32::tcr.total-bytes-allocated-low rcontext)
  (sub imm2 imm2 allocptr)
  (beq cr1 @go)
  (beq @go)
  (addc imm1 imm1 imm2)
  (addze imm0 imm0)
  @go
  (ba .SPmakeu64))














(defppclapfunction values ()
  (vpush-argregs)
  (add temp0 nargs vsp)
  (ba .SPvalues))

;; It would be nice if (%setf-macptr macptr (ash (the fixnum value) ash::fixnumshift))
;; would do this inline.
#+ppc-target
(defppclapfunction %setf-macptr-to-object ((macptr arg_y) (object arg_z))
  (check-nargs 2)
  (trap-unless-typecode= arg_y ppc32::subtag-macptr)
  (stw arg_z ppc32::macptr.address arg_y)
  (blr))

(defppclapfunction %fixnum-from-macptr ((macptr arg_z))
  (check-nargs 1)
  (trap-unless-typecode= arg_z ppc32::subtag-macptr)
  (lwz imm0 ppc32::macptr.address arg_z)
  (trap-unless-lisptag= imm0 ppc32::tag-fixnum imm1)
  (mr arg_z imm0)
  (blr))

(defppclapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (add imm2 imm2 imm1)
  (lwz imm0 0 imm2)
  (lwz imm1 4 imm2)
  (ba .SPmakeu64))

(defppclapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (add imm2 imm2 imm1)
  (lwz imm0 0 imm2)
  (lwz imm1 4 imm2)
  (ba .SPmakes64))

(defppclapfunction %%set-unsigned-longlong ((ptr arg_x)
					      (offset arg_y)
					      (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (bla .SPgetu64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (add imm2 imm3 imm2)
  (stw imm0 0 imm2)
  (stw imm1 4 imm2)
  (ba .SPpopj))

(defppclapfunction %%set-signed-longlong ((ptr arg_x)
					    (offset arg_y)
					    (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (bla .SPgets64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (add imm2 imm3 imm2)
  (stw imm0 0 imm2)
  (stw imm1 4 imm2)
  (ba .SPpopj))

(defppclapfunction interrupt-level ()
  (lwz arg_z ppc32::tcr.interrupt-level rcontext)
  (blr))


(defppclapfunction disable-lisp-interrupts ()
  (li imm0 '-1)
  (lwz arg_z ppc32::tcr.interrupt-level rcontext)
  (stw imm0 ppc32::tcr.interrupt-level rcontext)
  (blr))

(defppclapfunction set-interrupt-level ((new arg_z))
  (trap-unless-lisptag= new ppc32::tag-fixnum imm0)
  (stw new ppc32::tcr.interrupt-level rcontext)
  (blr))

;;; If we're restoring the interrupt level to 0 and an interrupt
;;; was pending, restore the level to 1 and zero the pending status.
(defppclapfunction restore-interrupt-level ((old arg_z))
  (cmpwi :cr1 old 0)
  (lwz imm0 ppc32::tcr.interrupt-pending rcontext)
  (cmpwi :cr0 imm0 0)
  (bne :cr1 @store)
  (beq :cr0 @store)
  (stw rzero ppc32::tcr.interrupt-pending rcontext)
  (li old '1)
  @store
  (stw old ppc32::tcr.interrupt-level rcontext)
  (blr))

(defppclapfunction %interrupt-poll ()
  (check-nargs 0)
  (event-poll)
  (li arg_z nil)
  (blr))

(defppclapfunction %current-tcr ()
  (mr arg_z rcontext)
  (blr))

(defppclapfunction %tcr-toplevel-function ((tcr arg_z))
  (check-nargs 1)
  (cmpw tcr rcontext)
  (mr imm0 vsp)
  (lwz temp0 ppc32::tcr.vs-area tcr)
  (lwz imm1 ppc32::area.high temp0)
  (beq @room)
  (lwz imm0 ppc32::area.active temp0)
  @room
  (cmpw imm1 imm0)
  (li arg_z nil)
  (beqlr)
  (lwz arg_z -4 imm1)
  (blr))

(defppclapfunction %set-tcr-toplevel-function ((tcr arg_y) (fun arg_z))
  (check-nargs 2)
  (cmpw tcr rcontext)
  (mr imm0 vsp)
  (lwz temp0 ppc32::tcr.vs-area tcr)
  (lwz imm1 ppc32::area.high temp0)
  (beq @check-room)
  (lwz imm0 ppc32::area.active temp0)
  @check-room
  (cmpw imm1 imm0)
  (stwu rzero -4 imm1)
  (bne @have-room)
  (stw imm1 ppc32::area.active temp0)
  (stw imm1 ppc32::tcr.save-vsp tcr)
  @have-room
  (stw fun 0 imm1)
  (blr))

;;; Yield the CPU, via a platform-specific syscall.
;;; On both platforms, this bashes r3 (only) and we have to bash r0
;;; to select the syscall.
(defppclapfunction yield ()
  (li 0 ppc32::yield-syscall)
  (sc)
  ;;; There might be some funky return conventions; set r0 back to 0
  ;;; early and often.
  (li 0 0)
  (li 0 0)
  (li 0 0)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %store-node-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (vpop temp0)
  (unbox-fixnum imm0 temp0)
  (let ((current temp1))
    @again
    (lwarx current object imm0)
    (cmpw current old)
    (bne @lose)
    (stwcx. new object imm0)
    (bne @again)
    (isync)
    (la arg_z (+ ppc32::t-offset ppc32::nil-value) 0)
    (blr)
    @lose
    (li imm0 ppc32::reservation-discharge)
    (stwcx. rzero rzero imm0)
    (li arg_z nil)
    (blr)))

(defppclapfunction set-%gcable-macptrs% ((ptr ppc32::arg_z))
  (li imm0 (+ ppc32::nil-value (ppc32::kernel-global gcable-pointers)))
  @again
  (lwarx arg_y rzero imm0)
  (stw arg_y ppc32::xmacptr.link ptr)
  (stwcx. ptr rzero imm0)
  (bne @again)
  (isync)
  (blr))

;;; Atomically increment the gc-inhibit-count kernel-global
(defppclapfunction %lock-gc-lock ()
  (li imm0 (+ ppc32::nil-value (ppc32::kernel-global gc-inhibit-count)))
  @again
  (lwarx arg_z rzero imm0)
  (addi arg_z arg_z '1)
  (stwcx. arg_z rzero imm0)
  (bne @again)
  (isync)
  (blr))

(defppclapfunction %unlock-gc-lock ()
  (li imm0 (+ ppc32::nil-value (ppc32::kernel-global gc-inhibit-count)))
  @again
  (lwarx arg_z rzero imm0)
  (subi arg_z arg_z '1)
  (stwcx. arg_z rzero imm0)
  (bne @again)
  (isync)
  (blr))

(defppclapfunction read-lock-rwlock ((lock arg_z))
  (check-nargs 1)
  (b @try)
  @loop
  (li imm0 ppc32::reservation-discharge)
  (stwcx. rzero rzero imm0)
  (event-poll)
  (li 0 ppc32::yield-syscall)
  (sc)
  (li 0 0)
  (li 0 0)
  (li 0 0)  
  @try
  (li imm1 ppc32::lock._value)
  (lwarx imm0 lock imm1)
  (cmpwi imm0 0)
  (blt @loop)				; locked for writing
  (addi imm0 imm0 '1)
  (stwcx. imm0 lock imm1)
  (bne @try)
  (isync)
  (blr))




(defppclapfunction write-lock-rwlock ((lock arg_z))
  (check-nargs 1)
  ;; If it's already locked by us, just decrement the count.
  (lwz imm0 ppc32::lock.writer lock)
  (cmpw imm0 rcontext)
  (bne @try)
  (lwz imm0 ppc32::lock._value lock)
  (subi imm0 imm0 '1)
  (stw imm0 ppc32::lock._value lock)
  (blr)
  @loop
  (li imm0 ppc32::reservation-discharge)
  (stwcx. rzero rzero imm0)
  (event-poll)
  (li 0 ppc32::yield-syscall)
  (sc)
  (li 0 0)
  (li 0 0)
  (li 0 0)
  @try
  (li imm1 ppc32::lock._value)
  (lwarx imm0 lock imm1)
  (cmpwi imm0 0)
  (bne @loop)				; locked by other thread
  (subi imm0 imm0 '1)
  (stwcx. imm0 lock imm1)
  (bne @try)
  (isync)
  (stw rcontext ppc32::lock.writer lock)
  (blr))



(defppclapfunction unlock-rwlock ((lock arg_z))
  (lwz imm2 ppc32::lock._value lock)
  (cmpwi imm2 0)
  (li imm1 ppc32::lock._value)
  (ble @unlock-write)
  @unlock-read
  (lwarx imm0 lock imm1)
  (subi imm0 imm0 '1)
  (stwcx. imm0 lock imm1)
  (bne @unlock-read)
  (isync)
  (blr)
  @unlock-write
  ;;; If we aren't the writer, return NIL.
  ;;; If we are and the value's about to go to 0, clear the writer field.
  (lwz imm0 ppc32::lock.writer lock)
  (cmpw imm0 rcontext)
  (lwzx imm0 lock imm1)
  (cmpwi cr1 imm0 '-1)
  (addi imm0 imm0 '1)
  (bne @fail)
  (bne cr1 @noclear)
  (stw rzero ppc32::lock.writer lock)
  @noclear
  (stw imm0 ppc32::lock._value lock)
  (blr)
  @fail
  (li arg_z nil)
  (blr))

(defppclapfunction %atomic-incf-node ((by arg_x) (node arg_y) (disp arg_z))
  (check-nargs 3)
  (unbox-fixnum imm1 disp)
  @again
  (lwarx arg_z node imm1)
  (add arg_z arg_z by)
  (stwcx. arg_z node imm1)
  (bne- @again)
  (isync)
  (blr))

(defppclapfunction %atomic-incf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (lwarx imm0 0 imm1)
  (addi imm0 imm0 1)
  (stwcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %atomic-incf-ptr-by ((ptr arg_y) (by arg_z))
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 by)
  @again
  (lwarx imm0 0 imm1)
  (add imm0 imm0 imm2)
  (stwcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %atomic-decf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (lwarx imm0 0 imm1)
  (subi imm0 imm0 1)
  (stwcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %atomic-decf-ptr-if-positive ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (lwarx imm0 0 imm1)
  (cmpwi cr1 imm0 0)
  (subi imm0 imm0 1)
  (beq @done)
  (stwcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr)
  @done
  (li imm1 ppc32::reservation-discharge)
  (box-fixnum arg_z imm0)
  (stwcx. rzero rzero imm1)
  (blr))

(defppclapfunction %atomic-swap-ptr ((ptr arg_y) (newval arg_z))
  (sync)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 arg_z)
  @again
  (lwarx imm0 0 imm1)
  (stwcx. imm2 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

;;; Try to store the fixnum NEWVAL at PTR, if and only if the old value
;;; was equal to OLDVAL.  Return the old value
(defppclapfunction %ptr-store-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (macptr-ptr imm0 ptr)
  (unbox-fixnum imm1 expected-oldval)
  (unbox-fixnum imm2 newval)
  @again
  (lwarx imm3 0 imm0)
  (cmpw imm3 imm1)
  (bne- @done)
  (stwcx. imm2 0 imm0)
  (bne- @again)
  (isync)
  (box-fixnum arg_z imm3)
  (blr)
  @done
  (li imm0 ppc32::reservation-discharge)
  (box-fixnum arg_z imm3)
  (stwcx. rzero 0 imm0)
  (blr))


  
  
  

; end of ppc-misc.lisp
