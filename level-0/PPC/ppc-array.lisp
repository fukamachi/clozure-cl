;;;-*- Mode: Lisp; Package: CCL -*-
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


(eval-when (:compile-toplevel :execute)
  (require "PPC-ARCH" "ccl:compiler;ppc;ppc-arch")
  (require "PPC-LAPMACROS" "ccl:compiler;ppc;ppc-lapmacros"))


; Users of this shouldn't make assumptions about return value.


; Assumptions made by %init-misc
(eval-when (:compile-toplevel :execute)
  (assert (and (< ppc32::max-32-bit-ivector-subtag
                  ppc32::max-8-bit-ivector-subtag
                  ppc32::max-16-bit-ivector-subtag)
               (eql ppc32::max-32-bit-ivector-subtag ppc32::subtag-s32-vector)
               (eql ppc32::max-16-bit-ivector-subtag ppc32::subtag-s16-vector)
               (eql ppc32::max-8-bit-ivector-subtag ppc32::subtag-simple-base-string))))

(defppclapfunction %init-misc ((val arg_y)
                               (miscobj arg_z))
  (getvheader imm0 miscobj)
  (header-size imm3 imm0)
  (cmpwi cr3 imm3 0)
  (extract-fulltag imm1 imm0)
  (cmpwi cr0 imm1 ppc32::fulltag-nodeheader)
  (extract-lowbyte imm2 imm0)
  (beqlr cr3)                           ; Silly 0-length case
  (li imm4 ppc32::misc-data-offset)
  (bne cr0 @imm)
  ; Node vector.  Don't need to memoize, since initial value is
  ; older than vector.
  @node-loop
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stwx val miscobj imm4)
  (la imm4 4 imm4)
  (bne cr0 @node-loop)
  (blr)
  @imm
  (cmpwi cr0 imm2 ppc32::subtag-double-float-vector)
  (cmpwi cr1 imm2 ppc32::max-32-bit-ivector-subtag)
  (cmpwi cr2 imm2 ppc32::max-8-bit-ivector-subtag)
  (cmpwi cr3 imm2 ppc32::max-16-bit-ivector-subtag)
  (extract-typecode imm0 val :CR6)		; don't clobber CR0
  (cmpwi cr7 imm0 ppc32::tag-fixnum)
  (beq cr0 @dfloat)
  (ble cr1 @32)
  (ble cr2 @8)
  (ble cr3 @16)
  ; Bit vector.
  (cmplwi cr0 val '1)
  (la imm3 31 imm3)
  (srwi imm3 imm3 5)
  (unbox-fixnum imm0 val)
  (neg imm0 imm0)
  (ble+ cr0 @set-32)
  @bad
  (li arg_x '#.$xnotelt)
  (save-lisp-context)
  (set-nargs 3)
  (call-symbol %err-disp)
  @dfloat
  (cmpwi cr0 imm0 ppc32::subtag-double-float)
  (li imm4 ppc32::misc-dfloat-offset)
  (bne- cr0 @bad)
  (lfd fp0 ppc32::double-float.value val)
  @dfloat-loop
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stfdx fp0 miscobj imm4)
  (la imm4 8 imm4)
  (bne cr0 @dfloat-loop)
  (blr)
  @32
  (cmpwi cr0 imm2 ppc32::subtag-single-float-vector)
  (cmpwi cr2 imm0 ppc32::subtag-bignum)
  (beq cr1 @s32)                     ; ppc32::max-32-bit-ivector-subtag
  (bne cr0 @u32)
  ;@sfloat
  (cmpwi cr0 imm0 ppc32::subtag-single-float)
  (bne- cr0 @bad)
  (lwz imm0 ppc32::single-float.value val)
  (b @set-32)
  @s32
  (unbox-fixnum imm0 val)
  (beq+ cr7 @set-32)
  (bne- cr2 @bad)
  (getvheader imm0 val)
  (cmpwi cr0 imm0 (logior (ash 1 ppc32::num-subtag-bits) ppc32::subtag-bignum))
  (lwz imm0 ppc32::misc-data-offset val)
  (beq+ cr0 @set-32)
  (b @bad)
  @u32
  (extract-unsigned-byte-bits. imm0 val 30)
  (unbox-fixnum imm0 val)
  (beq cr0 @set-32)
  (bne- cr2 @bad)
  ; a one-digit bignum is ok if that digit is positive.
  ; a two-digit bignum is ok if the sign-digit is 0.
  (getvheader imm0 val)
  (cmpwi cr2 imm0 (logior (ash 2 ppc32::num-subtag-bits) ppc32::subtag-bignum))
  (lwz imm0 ppc32::misc-data-offset val)
  (cmpwi cr3 imm0 0)
  (bgt- cr2 @bad)                       ; more than two digits.
  (beq cr2 @two-digits)
  (bgt+ cr3 @set-32)
  (b @bad)
  @two-digits
  (lwz imm1 (+ 4 ppc32::misc-data-offset) val)
  (cmpwi cr0 imm1 0)
  (bne- cr0 @bad)
  (b @set-32)
  @16
  (cmpwi cr0 imm2 ppc32::subtag-u16-vector)
  (la imm3 1 imm3)
  (srwi imm3 imm3 1)
  (beq cr3 @s16)                        ; ppc32::max-16-bit-ivector-subtag
  (bne cr0 @char16)
  (extract-unsigned-byte-bits. imm0 val 16)
  (unbox-fixnum imm0 val)
  (beq+ cr0 @set-16)
  (b @bad)
  @s16
  (slwi imm0 val (- 32 (+ 16 ppc32::fixnumshift)))
  (srawi imm0 imm0 (- 32 (+ 16 ppc32::fixnumshift)))
  (cmpw cr0 imm0 val)
  (unbox-fixnum imm0 val)
  (bne- cr7 @bad)
  (beq+ cr0 @set-16)
  (b @bad)
  @char16
  (clrlwi imm0 val 24)
  (cmpwi cr0 imm0 ppc32::subtag-character)
  (srwi imm0 val ppc32::charcode-shift)
  (beq+ cr0 @set-16)
  (b @bad)
  @8
  (cmpwi cr0 imm0 ppc32::subtag-s8-vector)
  (la imm3 3 imm3)
  (srwi imm3 imm3 2)
  (beq cr2 @char8)                      ; ppc32::max-8-bit-ivector-subtag
  (beq cr0 @s8)
  (extract-unsigned-byte-bits. imm0 val 8)
  (unbox-fixnum imm0 val)
  (beq+ cr0 @set-8)
  (b @bad)
  @s8
  (slwi imm0 val (- 32 (+ 8 ppc32::fixnumshift)))
  (srawi imm0 imm0 (- 32 (+ 8 ppc32::fixnumshift)))
  (cmpw cr0 imm0 val)
  (unbox-fixnum imm0 val)
  (bne- cr7 @bad)
  (beq+ cr0 @set-8)
  (b @bad)
  @char8
  (unbox-base-char imm0 val cr0)   ; this type checks val
  @set-8                                ; propagate low 8 bits into low 16
  (rlwimi imm0 imm0 8 (- 32 16) (- 31 8))
  @set-16                               ; propagate low 16 bits into high 16
  (rlwimi imm0 imm0 16 0 (- 31 16))
  @set-32
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (stwx imm0 miscobj imm4)
  (la imm4 4 imm4)
  (bne cr0 @set-32)
  (blr))

; Make a new vector of size newsize whose subtag matches that of oldv-arg.
; Blast the contents of the old vector into the new one as quickly as
; possible; leave remaining elements of new vector undefined (0).
; Return new-vector.
(defppclapfunction %extend-vector ((start-arg arg_x) (oldv-arg arg_y) (newsize arg_z))
  (let ((oldv save0)
        (oldsize save1)
        (oldsubtag save2)
        (start-offset save3))
    (save-lisp-context)
    (:regsave save3 0)
    (vpush save0)
    (vpush save1)
    (vpush save2)
    (vpush save3)
    (mr oldv oldv-arg)
    (mr start-offset start-arg)
    (getvheader imm0 oldv)
    (header-length oldsize imm0)
    (header-subtag[fixnum] oldsubtag imm0)
    (mr arg_y newsize)
    (mr arg_z oldsubtag)
    (bla .SPmisc-alloc)
    (extrwi imm0 oldsubtag ppc32::ntagbits (- 32 (+  ppc32::fixnumshift ppc32::ntagbits)))
    (cmpwi cr0 oldsize 0)
    (cmpwi cr1 imm0 ppc32::fulltag-nodeheader)
    (cmpwi cr2 oldsubtag '#.ppc32::max-32-bit-ivector-subtag)
    (la imm1 ppc32::misc-data-offset start-offset)
    (li imm3 ppc32::misc-data-offset)
    (beq cr0 @done)
    (bne cr1 @imm)
    ; copy nodes.  New vector is "new", so no memoization required.
    @node-loop
    (cmpwi cr0 oldsize '1)
    (lwzx temp0 oldv imm1)
    (addi imm1 imm1 4)
    (subi oldsize oldsize '1)
    (stwx temp0 arg_z imm3)
    (addi imm3 imm3 4)
    (bne cr0 @node-loop)
    ;Restore registers.  New vector's been in arg_z all this time.
    @done
    (lwz save3 0 vsp)
    (lwz save2 4 vsp)
    (lwz save1 8 vsp)
    (lwz save0 12 vsp)
    (restore-full-lisp-context)
    (blr)
    @imm
    (unbox-fixnum imm2 oldsize)
    (unbox-fixnum imm3 start-offset)
    (li imm1 ppc32::misc-data-offset)
    (la imm4 ppc32::misc-data-offset start-offset)
    (cmpwi cr1 oldsubtag '#.ppc32::max-8-bit-ivector-subtag)
    (cmpwi cr0 oldsubtag '#.ppc32::max-16-bit-ivector-subtag)
    (ble cr2 @fullword-loop)
    (cmpwi cr2 oldsubtag '#.ppc32::subtag-bit-vector)
    (ble cr1 @8-bit)
    (ble cr0 @16-bit)
    (beq cr2 @1-bit)
    ; 64-bit (double-float) vectors.  There's a different
    ; initial offset, but we're always word-aligned, so that
    ; part's easy.
    (li imm1 ppc32::misc-dfloat-offset)   ; scaled destination pointer
    (slwi imm2 imm2 1)                  ; twice as many fullwords
    (slwi imm3 imm3 3)                  ; convert dword count to byte offset
    (la imm4 ppc32::misc-dfloat-offset imm3)      ; scaled source pointer
    (b @fullword-loop)
    ; The bitvector case is hard if START-OFFSET isn't on an 8-bit boundary,
    ;  and can be turned into the 8-bit case otherwise.
    ; The 8-bit case is hard if START-OFFSET isn't on a 16-bit boundary, 
    ;  and can be turned into the 16-bit case otherwise.
    ; The 16-bit case is hard if START-OFFSET isn't on a 32-bit boundary, 
    ;  and can be turned into the 32-bit case otherwise.
    ; Hmm.
    @1-bit
    (clrlwi. imm0 imm3 (- 32 3))
    (bne- cr0 @hard-1-bit)
    (srwi imm3 imm3 3)                  ; bit offset to byte offset
    (addi imm2 imm2 7)
    (srwi imm2 imm2 3)                  ; bit count to byte count
    @8-bit
    ; If the byte offset's even, copy half as many halfwords
    (clrlwi. imm0 imm3 (- 32 1))
    (bne- cr0 @hard-8-bit)
    (addi imm2 imm2 1)
    (srwi imm2 imm2 1)                  ; byte count to halfword count
    (srwi imm3 imm3 1)                  ; byte offset to halfword offset
    @16-bit
    ; If the halfword offset's even, copy half as many fullwords
    (clrlwi. imm0 imm3 (- 32 1))
    (bne- cr0 @hard-16-bit)
    (addi imm2 imm2 1)
    (srwi imm2 imm2 1)                  ; halfword count to fullword count
    (li imm1 ppc32::misc-data-offset)   
    @fullword-loop
    (cmpwi cr0 imm2 1)
    (lwzx imm0 oldv imm4)
    (addi imm4 imm4 4)
    (subi imm2 imm2 1)
    (stwx imm0 arg_z imm1)
    (addi imm1 imm1 4)
    (bne cr0 @fullword-loop)
    (b @done)
    ;;; This can just do a uvref/uvset loop.  Cases that can
    ;;; cons (x32, double-float) have already been dealt with.
    @hard-1-bit
    @hard-8-bit
    @hard-16-bit
    (let ((newv save3)
          (outi save4))
      (vpush save3)
      (vpush save4)
      (mr newv arg_z)
      (li outi 0)
      @hard-loop
      (mr arg_y oldv)
      (mr arg_z start-offset)
      (bla .SPmisc-ref)
      (mr arg_x newv)
      (mr arg_y outi)
      (bla .SPmisc-set)
      (la outi '1 outi)
      (cmpw cr0 outi oldsize)
      (la start-offset '1 start-offset)
      (bne @hard-loop)
      (mr arg_z newv)
      (vpop save4)
      (vpop save3)
      (b @done))))



  



;; argument is a vector header or an array header.  Or else.
(defppclapfunction %array-header-data-and-offset ((a arg_z))
  (let ((offset arg_y)
        (disp arg_x)
        (temp temp0))
    (li offset 0)
    (mr temp a)
    @loop
    (lwz a ppc32::arrayH.data-vector temp)
    (lbz imm0 ppc32::misc-subtag-offset a)
    (cmpwi cr0 imm0 ppc32::subtag-vectorH)
    (lwz disp ppc32::arrayH.displacement temp)
    (mr temp a)
    (add offset offset disp)
    (ble cr0 @loop)
    (vpush a)
    (vpush offset)
    (set-nargs 2)
    (la temp0 8 vsp)
    (ba .SPvalues)))

(defun ppc-subtag-bytes (subtag element-count)
  (subtag-bytes subtag element-count))
