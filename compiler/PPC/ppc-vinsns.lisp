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


(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "PPC-BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "PPCENV"))

(defmacro define-ppc-vinsn (vinsn-name (results args &optional temps) &body body)
  (%define-vinsn *ppc-backend* vinsn-name results args temps body))


; Index "scaling" and constant-offset misc-ref vinsns.

(define-ppc-vinsn scale-32bit-misc-index (((dest :u32))
                                          ((idx :imm)      ; A fixnum
                                           )
                                          ())
  (addi dest idx arch::misc-data-offset))

(define-ppc-vinsn scale-16bit-misc-index (((dest :u32))
                                          ((idx :imm)      ; A fixnum
                                           )
                                          ())
  (srwi dest idx 1)
  (addi dest dest arch::misc-data-offset))

(define-ppc-vinsn scale-8bit-misc-index (((dest :u32))
                                         ((idx :imm)      ; A fixnum
                                          )
                                         ())
  (srwi dest idx 2)
  (addi dest dest arch::misc-data-offset))

(define-ppc-vinsn scale-64bit-misc-index (((dest :u32))
                                          ((idx :imm)      ; A fixnum
                                           )
                                          ())
  (slwi dest idx 1)
  (addi dest dest arch::misc-dfloat-offset))

(define-ppc-vinsn scale-1bit-misc-index (((word-index :u32)
                                          (bitnum :u8))     ; (unsigned-byte 5)
                                         ((idx :imm)      ; A fixnum
                                          )
                                         )
  ; Logically, we want to:
  ; 1) Unbox the index by shifting it right 2 bits.
  ; 2) Shift (1) right 5 bits
  ; 3) Scale (2) by shifting it left 2 bits.
  ; We get to do all of this with one instruction
  (rlwinm word-index idx (- arch::nbits-in-word 5) 5 (- arch::least-significant-bit arch::fixnum-shift))
  (addi word-index word-index arch::misc-data-offset)     ; Hmmm. Also one instruction, but less impressive somehow.
  (extrwi bitnum idx 5 (- arch::nbits-in-word (+ arch::fixnum-shift 5))))



(define-ppc-vinsn misc-ref-u32  (((dest :u32))
                                 ((v :lisp)
                                  (scaled-idx :u32))
                                 ())
  (lwzx dest v scaled-idx))

(define-ppc-vinsn misc-ref-c-u32  (((dest :u32))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  (lwz dest (:apply + arch::misc-data-offset (:apply ash idx 2)) v))

(define-ppc-vinsn misc-ref-s32 (((dest :s32))
                                ((v :lisp)
                                 (scaled-idx :u32))
                                ())
  (lwzx dest v scaled-idx))

(define-ppc-vinsn misc-ref-c-s32  (((dest :s32))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  (lwz dest (:apply + arch::misc-data-offset (:apply ash idx 2)) v))


(define-ppc-vinsn misc-set-c-u32 (()
                                  ((val :u32)
                                   (v :lisp)
                                   (idx :u32const)))
  (stw val (:apply + arch::misc-data-offset (:apply ash idx 2)) v))

(define-ppc-vinsn misc-set-u32 (()
                                ((val :u32)
                                 (v :lisp)
                                 (scaled-idx :u32)))
  (stwx val v scaled-idx))

                              
(define-ppc-vinsn misc-ref-single-float  (((dest :single-float))
                                          ((v :lisp)
                                           (scaled-idx :u32))
                                          ())
  (lfsx dest v scaled-idx))

(define-ppc-vinsn misc-ref-c-single-float  (((dest :single-float))
                                            ((v :lisp)
                                             (idx :u32const))
                                            ())
  (lfs dest (:apply + arch::misc-data-offset (:apply ash idx 2)) v))

(define-ppc-vinsn misc-ref-double-float  (((dest :double-float))
                                          ((v :lisp)
                                           (scaled-idx :u32))
                                          ())
  (lfdx dest v scaled-idx))


(define-ppc-vinsn misc-ref-c-double-float  (((dest :double-float))
                                            ((v :lisp)
                                             (idx :u32const))
                                            ())
  (lfd dest (:apply + arch::misc-dfloat-offset (:apply ash idx 3)) v))

(define-ppc-vinsn misc-set-c-double-float (((val :double-float))
                                           ((v :lisp)
                                            (idx :u32const)))
  (stfd val (:apply + arch::misc-dfloat-offset (:apply ash idx 3)) v))

(define-ppc-vinsn misc-set-double-float (()
                                         ((val :double-float)
                                          (v :lisp)
                                          (scaled-idx :u32)))
  (stfdx val v scaled-idx))

(define-ppc-vinsn misc-set-c-single-float (((val :single-float))
                                           ((v :lisp)
                                            (idx :u32const)))
  (stfs val (:apply + arch::misc-data-offset (:apply ash idx 2)) v))

(define-ppc-vinsn misc-set-single-float (()
                                         ((val :single-float)
                                          (v :lisp)
                                          (scaled-idx :u32)))
  (stfsx val v scaled-idx))


(define-ppc-vinsn misc-ref-u16  (((dest :u16))
                                 ((v :lisp)
                                  (scaled-idx :u32))
                                 ())
  (lhzx dest v scaled-idx))

(define-ppc-vinsn misc-ref-c-u16  (((dest :u16))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  (lhz dest (:apply + arch::misc-data-offset (:apply ash idx 1)) v))

(define-ppc-vinsn misc-set-c-u16  (((val :u16))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  (sth val (:apply + arch::misc-data-offset (:apply ash idx 1)) v))

(define-ppc-vinsn misc-set-u16 (((val :u16))
                                ((v :lisp)
                                 (scaled-idx :s32)))
  (sthx val v scaled-idx))

(define-ppc-vinsn misc-ref-s16  (((dest :s16))
                                 ((v :lisp)
                                  (scaled-idx :u32))
                                 ())
  (lhax dest v scaled-idx))

(define-ppc-vinsn misc-ref-c-s16  (((dest :s16))
                                   ((v :lisp)
                                    (idx :u32const))
                                   ())
  (lha dest (:apply + arch::misc-data-offset (:apply ash idx 1)) v))

(define-ppc-vinsn misc-ref-u8  (((dest :u8))
                                ((v :lisp)
                                 (scaled-idx :u32))
                                ())
  (lbzx dest v scaled-idx))

(define-ppc-vinsn misc-ref-c-u8  (((dest :u8))
                                  ((v :lisp)
                                   (idx :u32const))
                                  ())
  (lbz dest (:apply + arch::misc-data-offset idx) v))

(define-ppc-vinsn misc-set-c-u8  (((val :u8))
                                  ((v :lisp)
                                   (idx :u32const))
                                  ())
  (stb val (:apply + arch::misc-data-offset idx) v))

(define-ppc-vinsn misc-set-u8  (((val :u8))
                                ((v :lisp)
                                 (scaled-idx :u32))
                                ())
  (stbx val v scaled-idx))

(define-ppc-vinsn misc-ref-s8  (((dest :s8))
                                ((v :lisp)
                                 (scaled-idx :u32))
                                ())
  (lbzx dest v scaled-idx)
  (extsb dest dest))

(define-ppc-vinsn misc-ref-c-s8  (((dest :s8))
                                  ((v :lisp)
                                   (idx :u32const))
                                  ())
  (lbz dest (:apply + arch::misc-data-offset idx) v)
  (extsb dest dest))


(define-ppc-vinsn misc-ref-c-bit (((dest :u8))
                                  ((v :lisp)
                                   (idx :u32const))
                                  ())
  (lwz dest (:apply + arch::misc-data-offset (:apply ash idx -5)) v)
  (rlwinm dest dest (:apply 1+ (:apply logand idx #x1f)) 31 31))

(define-ppc-vinsn misc-ref-c-bit[fixnum] (((dest :imm))
                                          ((v :lisp)
                                           (idx :u32const))
                                          ((temp :u32)))
                  (lwz temp (:apply + arch::misc-data-offset (:apply ash idx -5)) v)
                  (rlwinm dest 
                          temp
                          (:apply + 1 arch::fixnumshift (:apply logand idx #x1f)) 
                          (- arch::least-significant-bit arch::fixnumshift)
                          (- arch::least-significant-bit arch::fixnumshift)))


(define-ppc-vinsn misc-ref-node  (((dest :lisp))
                                  ((v :lisp)
                                   (scaled-idx :s32))
                                  ())
  (lwzx dest v scaled-idx))

(define-ppc-vinsn misc-set-node (()
                                 ((val :lisp)
                                  (v :lisp)
                                  (scaled-idx :s32))
                                 ())
  (stwx val v scaled-idx))




(define-ppc-vinsn misc-ref-c-node (((dest :lisp))
                                   ((v :lisp)
                                    (idx :s16const))
                                   ())
  (lwz dest (:apply + arch::misc-data-offset (:apply ash idx 2)) v))

(define-ppc-vinsn misc-set-c-node (()
                                   ((val :lisp)
                                    (v :lisp)
                                    (idx :s16const))
                                   ())
  (stw val (:apply + arch::misc-data-offset (:apply ash idx 2)) v))


(define-ppc-vinsn misc-element-count[fixnum] (((dest :imm))
                                              ((v :lisp))
                                              ((temp :u32)))
  (lwz temp arch::misc-header-offset v)
  (rlwinm dest 
          temp 
          (- arch::nbits-in-word (- arch::num-subtag-bits arch::fixnumshift))
          (- arch::num-subtag-bits arch::fixnumshift) 
          (- arch::least-significant-bit arch::fixnumshift)))

(define-ppc-vinsn check-misc-bound (()
                                    ((idx :imm)
                                     (v :lisp))
                                    ((temp :u32)))
  (lwz temp arch::misc-header-offset v)
  (rlwinm temp 
          temp 
          (- arch::nbits-in-word (- arch::num-subtag-bits arch::fixnumshift))
          (- arch::num-subtag-bits arch::fixnumshift) 
          (- arch::least-significant-bit arch::fixnumshift))
  (twlge idx temp))

(define-ppc-vinsn 2d-unscaled-index (((dest :u32))
                                     ((array :lisp)
                                      (i :imm)
                                      (j :imm)
                                      (dim1 :u32)))
  (mullw dest i dim1)
  (add dest dest j))



(define-ppc-vinsn 2d-32-scaled-index (((dest :u32))
                                      ((array :lisp)
                                       (i :imm)
                                       (j :imm)
                                       (dim1 :u32)))
  (mullw dest i dim1)
  (add dest dest j)
  (la dest arch::misc-data-offset dest))

(define-ppc-vinsn 2d-dim1 (((dest :u32))
                           ((header :lisp)))
  (lwz dest (+ arch::misc-data-offset (* 4 (1+ arch::arrayH.dim0-cell))) header)
  (srawi dest dest arch::fixnumshift))

;; Return dim1 (unboxed)
(define-ppc-vinsn check-2d-bound (((dim :u32))
                                  ((i :imm)
                                   (j :imm)
                                   (header :lisp)))
  (lwz dim (+ arch::misc-data-offset (* 4 arch::arrayH.dim0-cell)) header)
  (twlge i dim)
  (lwz dim (+ arch::misc-data-offset (* 4 (1+ arch::arrayH.dim0-cell))) header)
  (twlge j dim)
  (srawi dim dim arch::fixnumshift))

(define-ppc-vinsn array-data-vector-ref (((dest :lisp))
                                         ((header :lisp)))
  (lwz dest arch::arrayH.data-vector header))
  

(define-ppc-vinsn check-arrayH-rank (()
                                     ((header :lisp)
                                      (expected :u32const))
                                     ((rank :imm)))
  (lwz rank arch::arrayH.rank header)
  (twi 27 rank (:apply ash expected arch::fixnumshift)))

(define-ppc-vinsn check-arrayH-flags (()
                                      ((header :lisp)
                                       (expected :u16const))
                                      ((flags :imm)
                                       (xreg :u32)))
  (lis xreg (:apply ldb (byte 16 16) (:apply ash expected arch::fixnumshift)))
  (ori xreg xreg (:apply ldb (byte 16 0) (:apply ash expected arch::fixnumshift)))
  (lwz flags arch::arrayH.flags header)
  (tw 27 flags xreg))

  
(define-ppc-vinsn misc-element-count[u32] (((dest :u32))
                                           ((v :lisp))
                                           ())
  (lwz dest arch::misc-header-offset v)
  (srwi dest dest arch::num-subtag-bits))

(define-ppc-vinsn misc-subtag[fixnum] (((dest :imm))
                                       ((v :lisp))
                                       ((temp :u32)))
  (lbz temp arch::misc-subtag-offset v)
  (slwi dest temp arch::fixnumshift))

(define-ppc-vinsn misc-subtag[u32] (((dest :u32))
                                    ((v :lisp))
                                    ())
  (lbz dest arch::misc-subtag-offset v))

(define-ppc-vinsn header->subtag[u32] (((dest :u32))
                                       ((header :u32))
                                       ())
  (clrlwi dest header (- arch::nbits-in-word arch::num-subtag-bits)))

(define-ppc-vinsn header->subtag[fixnum] (((dest :imm))
                                          ((header :u32))
                                          ())
  (rlwinm dest 
          header 
          arch::fixnumshift 
          (- arch::nbits-in-word (+ arch::nfixnumtagbits arch::num-subtag-bits)) 
          (- arch::least-significant-bit arch::nfixnumtagbits)))

(define-ppc-vinsn header->element-count[u32] (((dest :u32))
                                              ((header :u32))
                                              ())
  (srwi dest header arch::num-subtag-bits))

  
(define-ppc-vinsn node-slot-ref  (((dest :lisp))
                                  ((node :lisp)
                                   (cellno :u32const)))
  (lwz dest (:apply + arch::misc-data-offset (:apply ash cellno 2)) node))



(define-ppc-vinsn  %slot-ref (((dest :lisp))
                              ((instance (:lisp (:ne dest)))
                               (index :lisp))
                              ((scaled :u32)))
  (la scaled arch::misc-data-offset index)
  (lwzx dest instance scaled)
  (tweqi dest arch::slot-unbound-marker))


; Untagged memory reference & assignment.

(define-ppc-vinsn mem-ref-c-fullword (((dest :u32))
                                      ((src :address)
                                       (index :s16const)))
  (lwz dest index src))

(define-ppc-vinsn mem-ref-fullword (((dest :u32))
                                    ((src :address)
                                     (index :s32)))
  (lwzx dest src index))

(define-ppc-vinsn mem-ref-c-u16 (((dest :u16))
                                 ((src :address)
                                  (index :s16const)))
  (lhz dest index src))

(define-ppc-vinsn mem-ref-u16 (((dest :u16))
                               ((src :address)
                                (index :s32)))
  (lhzx dest src index))


(define-ppc-vinsn mem-ref-c-s16 (((dest :s16))
                                 ((src :address)
                                  (index :s16const)))
  (lha dest src index))

(define-ppc-vinsn mem-ref-s16 (((dest :s16))
                               ((src :address)
                                (index :s32)))
  (lhax dest src index))

(define-ppc-vinsn mem-ref-c-u8 (((dest :u8))
                                ((src :address)
                                 (index :s16const)))
  (lbz dest index src))

(define-ppc-vinsn mem-ref-u8 (((dest :u8))
                              ((src :address)
                               (index :s32)))
  (lbzx dest src index))

(define-ppc-vinsn mem-ref-c-s8 (((dest :s8))
                                ((src :address)
                                 (index :s16const)))
  (lbz dest index src)
  (extsb dest dest))

(define-ppc-vinsn mem-ref-s8 (((dest :s8))
                              ((src :address)
                               (index :s32)))
  (lbzx dest src index)
  (extsb dest dest))

(define-ppc-vinsn mem-ref-c-bit (((dest :u8))
                                 ((src :address)
                                  (byte-index :s16const)
                                  (bit-shift :u8const)))
  (lbz dest byte-index src)
  (rlwinm dest dest bit-shift 31 31))

(define-ppc-vinsn mem-ref-c-bit[fixnum] (((dest :lisp))
                                         ((src :address)
                                          (byte-index :s16const)
                                          (bit-shift :u8const))
                                         ((byteval :u8)))
  (lbz byteval byte-index src)
  (rlwinm dest byteval bit-shift 29 29))

(define-ppc-vinsn mem-ref-bit (((dest :u8))
                               ((src :address)
                                (bit-index :lisp))
                               ((byte-index :s16)
                                (bit-shift :u8)))
  (srwi byte-index bit-index (+ arch::fixnumshift 3))
  (extrwi bit-shift bit-index 3 27)
  (addi bit-shift bit-shift 29)
  (lbzx dest src byte-index)
  (rlwnm dest dest bit-shift 31 31))


(define-ppc-vinsn mem-ref-bit[fixnum] (((dest :lisp))
                                       ((src :address)
                                        (bit-index :lisp))
                                       ((byte-index :s16)
                                        (bit-shift :u8)))
  (srwi byte-index bit-index (+ arch::fixnumshift 3))
  (extrwi bit-shift bit-index 3 27)
  (addi bit-shift bit-shift 27)
  (lbzx byte-index src byte-index)
  (rlwnm dest
         byte-index
         bit-shift
         (- arch::least-significant-bit arch::fixnum-shift)
         (- arch::least-significant-bit arch::fixnum-shift)))

(define-ppc-vinsn mem-ref-c-double-float (((dest :double-float))
                                          ((src :address)
                                           (index :s16const)))
  (lfd dest index src))

(define-ppc-vinsn mem-ref-double-float (((dest :double-float))
                                        ((src :address)
                                         (index :s32)))
  (lfdx dest src index))

(define-ppc-vinsn mem-set-c-double-float (()
                                          ((val :double-float)
                                           (src :address)
                                           (index :s16const)))
  (stfd val index src))

(define-ppc-vinsn mem-set-double-float (()
                                        ((val :double-float)
                                         (src :address)
                                         (index :s32)))
  (stfdx val src index))

(define-ppc-vinsn mem-ref-c-single-float (((dest :single-float))
                                          ((src :address)
                                           (index :s16const)))
  (lfs dest index src))

(define-ppc-vinsn mem-ref-single-float (((dest :single-float))
                                        ((src :address)
                                         (index :s32)))
  (lfsx dest src index))

(define-ppc-vinsn mem-set-c-single-float (()
                                          ((val :single-float)
                                           (src :address)
                                           (index :s16const)))
  (stfs val index src))

(define-ppc-vinsn mem-set-single-float (()
                                        ((val :single-float)
                                         (src :address)
                                         (index :s32)))
  (stfsx val src index))

                                           
(define-ppc-vinsn mem-set-c-fullword (()
                                      ((val :u32)
                                       (src :address)
                                       (index :s16const)))
  (stw val index src))

(define-ppc-vinsn mem-set-fullword (()
                                    ((val :u32)
                                     (src :address)
                                     (index :s32)))
  (stwx val src index))

(define-ppc-vinsn mem-set-c-halfword (()
                                      ((val :u16)
                                       (src :address)
                                       (index :s16const)))
  (sth val index src))

(define-ppc-vinsn mem-set-halfword (()
                                    ((val :u16)
                                     (src :address)
                                     (index :s32)))
  (sthx val src index))

(define-ppc-vinsn mem-set-c-byte (()
                                  ((val :u16)
                                   (src :address)
                                   (index :s16const)))
  (stb val index src))

(define-ppc-vinsn mem-set-byte (()
                                ((val :u8)
                                 (src :address)
                                 (index :s32)))
  (stbx val src index))

(define-ppc-vinsn mem-set-c-bit-0 (()
                                   ((src :address)
                                    (byte-index :s16const)
                                    (mask-begin :u8const)
                                    (mask-end :u8const))
                                   ((val :u8)))
  (lbz val byte-index src)
  (rlwinm val val 0 mask-begin mask-end)
  (stb val byte-index src))

(define-ppc-vinsn mem-set-c-bit-1 (()
                                   ((src :address)
                                    (byte-index :s16const)
                                    (mask :u8const))
                                   ((val :u8)))
  (lbz val byte-index src)
  (ori val val mask)
  (stb val byte-index src))

(define-ppc-vinsn mem-set-c-bit (()
                                 ((src :address)
                                  (byte-index :s16const)
                                  (bit-index :u8const)
                                  (val :imm))
                                 ((byteval :u8)))
  (lbz byteval byte-index src)
  (rlwimi byteval val (:apply logand 31 (:apply - 29 bit-index)) bit-index bit-index)
  (stb byteval byte-index src))

;;; Hey, they should be happy that it even works.  Who cares how big it is or how
;;; long it takes ...
(define-ppc-vinsn mem-set-bit (()
                               ((src :address)
                                (bit-index :lisp)
                                (val :lisp))
                               ((bit-shift :u32)
                                (mask :u32)
                                (byte-index :u32)
                                (crf :crf)))
  (cmplwi crf val (ash 1 arch::fixnumshift))
  (extrwi bit-shift bit-index 3 27)
  (li mask #x80)
  (srw mask mask bit-shift)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it
  (srwi bit-shift bit-index (+ 3 arch::fixnumshift))
  (lbzx bit-shift src bit-shift)
  (beq crf :set)
  (andc mask bit-shift mask)
  (b :done)
  :set
  (or mask bit-shift mask)
  :done
  (srwi bit-shift bit-index (+ 3 arch::fixnumshift))
  (stbx mask src bit-shift))
     
; Tag and subtag extraction, comparison, checking, trapping ...

(define-ppc-vinsn extract-tag (((tag :u8)) 
                               ((object :lisp)) 
                               ())
  (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits)))

(define-ppc-vinsn extract-tag[fixnum] (((tag :imm))
                                       ((object :lisp)))
  (rlwinm tag 
          object 
          arch::fixnum-shift 
          (- arch::nbits-in-word 
             (+ arch::nlisptagbits arch::fixnum-shift)) 
          (- arch::least-significant-bit arch::fixnum-shift)))

(define-ppc-vinsn extract-fulltag (((tag :u8))
                                   ((object :lisp))
                                   ())
  (clrlwi tag object (- arch::nbits-in-word arch::ntagbits)))


(define-ppc-vinsn extract-fulltag[fixnum] (((tag :imm))
                                           ((object :lisp)))
  (rlwinm tag 
          object 
          arch::fixnum-shift 
          (- arch::nbits-in-word 
             (+ arch::ntagbits arch::fixnum-shift)) 
          (- arch::least-significant-bit arch::fixnum-shift)))

(define-ppc-vinsn extract-typecode (((code :u8))
                                    ((object :lisp))
                                    ((crf :crf)))
  (clrlwi code object (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crf code arch::tag-misc)
  (bne crf :not-misc)
  (lbz code arch::misc-subtag-offset object)
  :not-misc)

(define-ppc-vinsn extract-typecode[fixnum] (((code :imm))
                                            ((object (:lisp (:ne code))))
                                            ((crf :crf) (subtag :u8)))
  (rlwinm code 
          object 
          arch::fixnum-shift 
          (- arch::nbits-in-word 
             (+ arch::nlisptagbits arch::fixnum-shift)) 
          (- arch::least-significant-bit arch::fixnum-shift))
  (cmpwi crf code (ash arch::tag-misc arch::fixnum-shift))
  (bne crf :not-misc)
  (lbz subtag arch::misc-subtag-offset object)
  (slwi code subtag arch::fixnum-shift)
  :not-misc)


(define-ppc-vinsn require-fixnum (()
                                  ((object :lisp))
                                  ((crf0 (:crf 0))
                                   (tag :u8)))
  :again
  (clrlwi. tag object (- arch::nbits-in-word arch::nlisptagbits))
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-fixnum object)
  (b :again)
  :got-it)

(define-ppc-vinsn require-integer (()
                                   ((object :lisp))
                                   ((crf0 (:crf 0))
                                    (tag :u8)))
  :again
  (clrlwi. tag object (- arch::nbits-in-word arch::nlisptagbits))
  (beq+ crf0 :got-it)
  (cmpwi crf0 tag arch::tag-misc)
  (bne crf0 :no-got)
  (lbz tag arch::misc-subtag-offset object)
  (cmpwi crf0 tag arch::subtag-bignum)
  (beq+ crf0 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-integer object)
  (b :again)
  :got-it)

(define-ppc-vinsn require-simple-vector (()
                                         ((object :lisp))
                                         ((tag :u8)
                                          (crf :crf)))
  :again
  (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crf tag arch::tag-misc)
  (bne crf :no-got)
  (lbz tag arch::misc-subtag-offset object)
  (cmpwi crf tag arch::subtag-simple-vector)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-simple-vector object)
  (b :again)
  :got-it)

(define-ppc-vinsn require-simple-string (()
                                         ((object :lisp))
                                         ((tag :u8)
                                          (crf :crf)
                                          (crf2 :crf)))
  :again
  (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crf tag arch::tag-misc)
  (bne crf :no-got)
  (lbz tag arch::misc-subtag-offset object)
  (cmpwi crf tag arch::subtag-simple-base-string)
  (cmpwi crf2 tag arch::subtag-simple-general-string)
  (beq+ crf :got-it)
  (beq+ crf2 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-simple-string object)
  (b :again)
  :got-it)

  
(define-ppc-vinsn require-real (()
                                ((object :lisp))
                                ((crf0 (:crf 0))
                                 (tag :u8)))
  :again
  (clrlwi. tag object (- arch::nbits-in-word arch::nlisptagbits))
  (beq+ crf0 :got-it)
  (cmpwi crf0 tag arch::tag-misc)
  (bne crf0 :no-got)
  (lbz tag arch::misc-subtag-offset object)
  (cmplwi crf0 tag arch::max-real-subtag)
  (ble+ crf0 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-real object)
  (b :again)
  :got-it)

(define-ppc-vinsn require-number (()
                                  ((object :lisp))
                                  ((crf0 (:crf 0))
                                   (tag :u8)))
  :again
  (clrlwi. tag object (- arch::nbits-in-word arch::nlisptagbits))
  (beq+ crf0 :got-it)
  (cmpwi crf0 tag arch::tag-misc)
  (bne crf0 :no-got)
  (lbz tag arch::misc-subtag-offset object)
  (cmplwi crf0 tag arch::max-numeric-subtag)
  (ble+ crf0 :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-number object)
  (b :again)
  :got-it)


(define-ppc-vinsn require-list (()
                                ((object :lisp))
                                ((tag :u8)
                                 (crf :crf)))
  :again
  (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crf tag arch::tag-list)
  (beq+ crf :got-it)
  (uuo_intcerr arch::error-object-not-list object)
  (b :again)
  :got-it)

(define-ppc-vinsn require-symbol (()
                                  ((object :lisp))
                                  ((tag :u8)
                                   (crf :crf)))
  :again
  (cmpwi crf object ppc::nil-value)
  (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits))
  (beq crf :got-it)
  (cmpwi crf tag arch::tag-misc)
  (bne crf :no-got)
  (lbz tag arch::misc-subtag-offset object)
  (cmpwi crf tag arch::subtag-symbol)
  (beq+ crf :got-it)
  :no-got
  (uuo_intcerr arch::error-object-not-symbol object)
  (b :again)
  :got-it)

(define-ppc-vinsn require-character (()
                                     ((object :lisp))
                                     ((tag :u8)
                                      (crf :crf)))
  :again
  (clrlwi tag object (- arch::nbits-in-word arch::num-subtag-bits))
  (cmpwi crf tag arch::subtag-character)
  (beq+ crf :got-it)
  (uuo_intcerr arch::error-object-not-character object)
  (b :again)
  :got-it)


(define-ppc-vinsn require-u8 (()
                              ((object :lisp))
                              ((crf0 (:crf 0))
                               (tag :u32)))
  :again
  ; The bottom arch::fixnumshift bits and the top (- 32 (+ arch::fixnumshift 8)) must all be zero.
  (rlwinm. tag object 0 (- arch::nbits-in-word arch::fixnumshift) (- arch::least-significant-bit (+ arch::fixnumshift 8)))
  (beq+ crf0 :got-it)
  (uuo_intcerr arch::error-object-not-unsigned-byte-8 object)
  (b :again)
  :got-it)

(define-ppc-vinsn box-fixnum (((dest :imm))
                              ((src :s32)))
  (slwi dest src arch::fixnumshift))

(define-ppc-vinsn fixnum->s32 (((dest :s32))
                               ((src :imm)))
  (srawi dest src arch::fixnumshift))

(define-ppc-vinsn fixnum->u32 (((dest :u32))
                               ((src :imm)))
  (srwi dest src arch::fixnumshift))

; An object is of type (UNSIGNED-BYTE 32) iff
;  a) it's of type (UNSIGNED-BYTE 30) (e.g., an unsigned fixnum)
;  b) it's a bignum of length 1 and the 0'th digit is positive
;  c) it's a bignum of length 2 and the sign-digit is 0.

(define-ppc-vinsn unbox-u32 (((dest :u32))
                             ((src :lisp))
                             ((crf0 (:crf 0))
                              (crf1 :crf)))
  (rlwinm. dest src 0 (- arch::nbits-in-word arch::fixnumshift) 0)
  (srwi dest src arch::fixnumshift)
  (beq+ crf0 :got-it)
  (clrlwi dest src (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crf0 dest arch::tag-misc)
  (bne- crf0 :bad)
  (lwz dest arch::misc-header-offset src)
  (cmpwi crf1 dest arch::two-digit-bignum-header)
  (cmpwi crf0 dest arch::one-digit-bignum-header)
  (lwz dest arch::misc-data-offset src)
  (beq crf1 :two)
  (bne crf0 :bad)
  (cmpwi crf0 dest 0)
  (bgt+ crf0 :got-it)
  :bad
  (uuo_interr arch::error-object-not-unsigned-byte-32 src)
  :two
  (lwz dest (+ arch::misc-data-offset 4) src)
  (cmpwi crf0 dest 0)
  (bne- crf0 :bad)
  (lwz dest arch::misc-data-offset src)
  :got-it)

; an object is of type (SIGNED-BYTE 32) iff
; a) it's a fixnum
; b) it's a bignum with exactly one digit.

(define-ppc-vinsn unbox-s32 (((dest :s32))
                             ((src :lisp))
                             ((crfx :crf)
                              (crfy :crf)
                              (tag :u32)))
  (clrlwi tag src (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crfx tag arch::tag-fixnum)
  (cmpwi crfy tag arch::tag-misc)
  (srawi dest src arch::fixnumshift)
  (beq+ crfx :got-it)
  (bne- crfy :bad)
  (lwz tag arch::misc-header-offset src)
  (cmpwi crfx tag arch::one-digit-bignum-header)
  (lwz dest arch::misc-data-offset src)
  (beq+ crfx :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-32 src)
  :got-it)

; For the sake of argument, "dest" is u32.
; Return dest if src is either (signed-byte 32) or (unsigned-byte 32).
; Say that it's not (signed-byte 32) if neither.
(define-ppc-vinsn unbox-x32 (((dest :u32))
                             ((src :lisp))
                             ((crfx :crf)
                              (crfy :crf)
                              (tag :u32)))
  (clrlwi tag src (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crfx tag arch::tag-fixnum)
  (cmpwi crfy tag arch::tag-misc)
  (srawi dest src arch::fixnumshift)
  (beq+ crfx :got-it)
  (bne- crfy :bad)
  (lwz tag arch::misc-header-offset src)
  (cmpwi crfx tag (logior (ash 1 arch::num-subtag-bits) arch::subtag-bignum))
  (cmpwi crfy tag (logior (ash 2 arch::num-subtag-bits) arch::subtag-bignum))
  (lwz dest arch::misc-data-offset src)
  (beq crfx :got-it)
  (lwz tag (+ 4 arch::misc-data-offset) src)
  (cmpwi crfx tag 0)
  (bne crfy :bad)
  (beq+ crfx :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-32 src)
  :got-it)

(define-ppc-vinsn unbox-u16 (((dest :u16))
                             ((src :lisp))
                             ((crf0 (:crf 0))))
  ; The bottom arch::fixnumshift bits and the top (- 31 (+ arch::fixnumshift 16)) must all be zero.
  (rlwinm. dest src 0 (- arch::nbits-in-word arch::fixnumshift) (- arch::least-significant-bit (+ arch::fixnumshift 16)))
  (rlwinm dest src (- 32 arch::fixnumshift) 16 31)
  (beq+ crf0 :got-it)
  (uuo_interr arch::error-object-not-unsigned-byte-16 src)
  :got-it)

(define-ppc-vinsn unbox-s16 (((dest :s16))
                             ((src :lisp))
                             ((crf :crf)))
  (slwi dest src (- 16 arch::fixnumshift))
  (srawi dest dest (- 16 arch::fixnumshift))
  (cmpw crf dest src)
  (clrlwi dest src (- arch::nbits-in-word arch::nlisptagbits))
  (bne- crf :bad)
  (cmpwi crf dest arch::tag-fixnum)
  (srawi dest src arch::fixnumshift)
  (beq+ crf :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-16 src)
  :got-it)

  
  
(define-ppc-vinsn unbox-u8 (((dest :u8))
                            ((src :lisp))
                            ((crf0 (:crf 0))))
  ; The bottom arch::fixnumshift bits and the top (- 31 (+ arch::fixnumshift 8)) must all be zero.
  (rlwinm. dest src 0 (- arch::nbits-in-word arch::fixnumshift) (- arch::least-significant-bit (+ arch::fixnumshift 8)))
  (rlwinm dest src (- 32 arch::fixnumshift) 24 31)
  (beq+ crf0 :got-it)
  (uuo_interr arch::error-object-not-unsigned-byte-8 src)
  :got-it)

(define-ppc-vinsn unbox-s8 (((dest :s8))
                            ((src :lisp))
                            ((crf :crf)))
  (slwi dest src (- arch::nbits-in-word (+ 8 arch::fixnumshift)))
  (srawi dest dest (- arch::nbits-in-word (+ 8 arch::fixnumshift)))
  (cmpw crf dest src)
  (clrlwi dest src (- arch::nbits-in-word arch::nlisptagbits))
  (bne- crf :bad)
  (cmpwi crf dest arch::tag-fixnum)
  (srawi dest src arch::fixnumshift)
  (beq+ crf :got-it)
  :bad
  (uuo_interr arch::error-object-not-signed-byte-16 src)
  :got-it)

(define-ppc-vinsn unbox-base-char (((dest :u32))
                                   ((src :lisp))
                                   ((crf :crf)))
  (rlwinm dest src 8 16 31)
  (cmpwi crf dest (ash arch::subtag-character 8))
  (srwi dest src arch::charcode-shift)
  (beq+ crf :got-it)
  (uuo_interr arch::error-object-not-base-char src)
  :got-it)

(define-ppc-vinsn unbox-character (((dest :u32))
                                   ((src :lisp))
                                   ((crf :crf)))
  (clrlwi dest src 24)
  (cmpwi crf dest arch::subtag-character)
  (srwi dest src arch::charcode-shift)
  (beq+ crf :got-it)
  (uuo_interr arch::error-object-not-character src)
  :got-it)

(define-ppc-vinsn unbox-bit (((dest :u32))
                             ((src :lisp))
                             ((crf :crf)))
  (cmplwi crf src (ash 1 arch::fixnumshift))
  (srawi dest src arch::fixnumshift)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it)

(define-ppc-vinsn unbox-bit-bit0 (((dest :u32))
                                  ((src :lisp))
                                  ((crf :crf)))
  (cmplwi crf src (ash 1 arch::fixnumshift))
  (rlwinm dest src (- 32 (1+ arch::fixnumshift)) 0 0)
  (ble+ crf :got-it)
  (uuo_interr arch::error-object-not-bit src)
  :got-it)

(define-ppc-vinsn fixnum->fpr (((dest :double-float))
                               ((src :lisp))
                               ((imm :s32)))
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfd ppc::fp-s32conv 8 ppc::tsp)
  (srawi imm src arch::fixnumshift)
  (xoris imm imm #x8000)
  (stw imm 12 ppc::tsp)
  (lfd dest 8 ppc::tsp)
  (lwz ppc::tsp 0 ppc::tsp)
  (fsub dest dest ppc::fp-s32conv))


(define-ppc-vinsn shift-right-variable-word (((dest :u32))
                                             ((src :u32)
                                              (sh :u32)))
  (srw dest src sh))

(define-ppc-vinsn u32logandc2 (((dest :u32))
                               ((x :u32)
                                (y :u32)))
  (andc dest x y))

(define-ppc-vinsn u32logior (((dest :u32))
                             ((x :u32)
                              (y :u32)))
  (or dest x y))

(define-ppc-vinsn rotate-left-variable-word (((dest :u32))
                                             ((src :u32)
                                              (rot :u32)))
  (rlwnm dest src rot 0 31))

(define-ppc-vinsn complement-shift-count (((dest :u32))
                                          ((src :u32)))
  (subfic dest src 32))

(define-ppc-vinsn extract-lowbyte (((dest :u32))
                                   ((src :lisp)))
  (clrlwi dest src (- arch::nbits-in-word arch::num-subtag-bits)))

; Set DEST to the difference between the low byte of SRC and BYTEVAL.
(define-ppc-vinsn extract-compare-lowbyte (((dest :u32))
                                           ((src :lisp)
                                            (byteval :u8const)))
  (clrlwi dest src (- arch::nbits-in-word arch::num-subtag-bits))
  (subi dest dest byteval))


; Set the "EQ" bit in condition-register field CRF if object is
; a fixnum.  Leave the object's tag in TAG.
; This is a little easier if CRF is CR0.
(define-ppc-vinsn eq-if-fixnum (((crf :crf)
                                 (tag :u8))
                                ((object :lisp))
                                ())
  ((:eq crf 0)
   (clrlwi. tag object (- arch::nbits-in-word arch::nlisptagbits)))
  ((:not (:eq crf 0))
   (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits))
   (cmpwi crf tag arch::tag-fixnum)))

(define-ppc-vinsn trap-unless-tag= (()
                                    ((object :lisp)
                                     (tagval :u16const))
                                    ((tag :u8)))
  (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits))
  (twnei tag tagval))

(define-ppc-vinsn trap-unless-fulltag= (()
                                        ((object :lisp)
                                         (tagval :u16const))
                                        ((tag :u8)))
  (clrlwi tag object (- arch::nbits-in-word arch::ntagbits))
  (twnei tag tagval))

(define-ppc-vinsn trap-unless-lowbyte= (()
                                        ((object :lisp)
                                         (tagval :u16const))
                                        ((tag :u8)))
  (clrlwi tag object (- arch::nbits-in-word 8))
  (twnei tag tagval))

(define-ppc-vinsn trap-unless-typecode= (()
                                         ((object :lisp)
                                          (tagval :u16const))
                                         ((tag :u8)
                                          (crf :crf)))
  (clrlwi tag object (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crf tag arch::tag-misc)
  (bne crf :do-trap)
  (lbz tag arch::misc-subtag-offset object)
  :do-trap
  (twnei tag tagval))
  
(define-ppc-vinsn subtract-constant (((dest :imm))
                                     ((src :imm)
                                      (const :s16const)))
  (subi dest src const))

(define-ppc-vinsn trap-unless-numeric-type (()
                                            ((object :lisp)
                                             (maxtype :u16const))
                                            ((crf0 (:crf 0))
                                             (tag :u8)
                                             (crfX :crf)))
  (clrlwi. tag object (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi tag arch::tag-misc)
  (beq+ crf0 :fixnum)
  (bne crfX :scale-tag)
  (lbz tag arch::misc-subtag-offset object)
  :scale-tag
  (subi tag tag arch::min-numeric-subtag)
  (twlgti tag (:apply - maxtype arch::min-numeric-subtag))
  :fixnum)


;; Bit-extraction & boolean operations

(eval-when (:compile-toplevel :execute)
  (assert (= arch::t-offset #b10001)))         ; PPC-bits 31 and 27 set

;; For some mind-numbing reason, IBM decided to call the most significant
;; bit in a 32-bit word "bit 0" and the least significant bit "bit 31"
;; (this despite the fact that it's essentially a big-endian architecture
;; (it was exclusively big-endian when this decision was made.))
;; We'll probably be least confused if we consistently use this backwards
;; bit ordering (letting things that have a "sane" bit-number worry about
;; it at compile-time or run-time (subtracting the "sane" bit number from
;; 31.))

(define-ppc-vinsn extract-variable-bit (((dest :u8))
                                        ((src :u32)
                                         (bitnum :u8))
                                        ())
  (rotlw dest src bitnum)
  (extrwi dest dest 1 0))


(define-ppc-vinsn extract-variable-bit[fixnum] (((dest :imm))
                                                ((src :u32)
                                                 (bitnum :u8))
                                                ((temp :u32)))
  (rotlw temp src bitnum)
  (rlwinm dest
          temp 
          (1+ arch::fixnumshift) 
          (- arch::least-significant-bit arch::fixnumshift)
          (- arch::least-significant-bit arch::fixnumshift)))


;; Sometimes we try to extract a single bit from some source register
;; into a destination bit (typically 31, sometimes fixnum bit 0 = 29).
;; If we copy bit 0 (whoops, I mean "bit 31") to bit 4 (aka 27) in a
;; given register, we get a value that's either 17 (the arithmetic difference
;; between T and NIL) or 0.

(define-ppc-vinsn bit31->truth (((dest :lisp)
                                 (bits :u32))
                                ((bits :u32))
                                ())
  (rlwimi bits bits (- arch::least-significant-bit 27) 27 27)    ; bits = 0000...X000X
  (addi dest bits ppc::nil-value))

(define-ppc-vinsn invert-bit31 (((bits :u32))
                                ((bits :u32))
                                ())
  (xori bits bits 1))

                           

;; Some of the obscure-looking instruction sequences - which map some relation
;; to PPC bit 31 of some register - were found by the GNU SuperOptimizer.
;; Some of them use extended-precision instructions (which may cause interlocks
;; on some superscalar PPCs, if I remember correctly.)  In general, sequences
;; that GSO found that -don't- do extended precision are longer and/or use
;; more temporaries.
;; On the 604, the penalty for using an instruction that uses the CA bit is
;; "at least" one cycle: it can't complete execution until all "older" instructions
;; have.  That's not horrible, especially given that the alternative is usually
;; to use more instructions (and, more importantly, more temporaries) to avoid
;; using extended-precision.


(define-ppc-vinsn eq0->bit31 (((bits :u32))
                              ((src (t (:ne bits)))))
  (cntlzw bits src)
  (srwi bits bits 5))                  ; bits = 0000...000X

(define-ppc-vinsn ne0->bit31 (((bits :u32))
                              ((src (t (:ne bits)))))
  (cntlzw bits src)
  (slw bits src bits)
  (srwi bits bits 31))                ; bits = 0000...000X

(define-ppc-vinsn lt0->bit31 (((bits :u32))
                              ((src (t (:ne bits)))))
  (srwi bits src 31))                   ; bits = 0000...000X


(define-ppc-vinsn ge0->bit31 (((bits :u32))
                              ((src (t (:ne bits)))))
  (srwi bits src 31)       
  (xori bits bits 1))                   ; bits = 0000...000X


(define-ppc-vinsn le0->bit31 (((bits :u32))
                              ((src (t (:ne bits)))))
  (neg bits src)
  (orc bits bits src)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc-vinsn gt0->bit31 (((bits :u32))
                              ((src (t (:ne bits)))))
  (subi bits src 1)       
  (nor bits bits src)
  (srwi bits bits 31))                  ; bits = 0000...000X

(define-ppc-vinsn ne->bit31 (((bits :u32))
                             ((x t)
                              (y t))
                             ((temp :u32)))
  (subf temp x y)
  (cntlzw bits temp)
  (slw bits temp bits)
  (srwi bits bits 31))                ; bits = 0000...000X

(define-ppc-vinsn fulltag->bit31 (((bits :u32))
                                  ((lispobj :lisp)
                                   (tagval :u8const))
                                  ())
  (clrlwi bits lispobj (- arch::nbits-in-word arch::ntagbits))
  (subi bits bits tagval)
  (cntlzw bits bits)
  (srwi bits bits 5))


(define-ppc-vinsn eq->bit31 (((bits :u32))
                             ((x t)
                              (y t)))
  (subf bits x y)
  (cntlzw bits bits)
  (srwi bits bits 5))                  ; bits = 0000...000X

(define-ppc-vinsn eqnil->bit31 (((bits :u32))
                                ((x t)))
  (subi bits x ppc::nil-value)
  (cntlzw bits bits)
  (srwi bits bits 5))

(define-ppc-vinsn ne->bit31 (((bits :u32))
                             ((x t)
                              (y t)))
  (subf bits x y)
  (cntlzw bits bits)
  (srwi bits bits 5)
  (xori bits bits 1))

(define-ppc-vinsn nenil->bit31 (((bits :u32))
                                ((x t)))
  (subi bits x ppc::nil-value)
  (cntlzw bits bits)
  (srwi bits bits 5)
  (xori bits bits 1))

(define-ppc-vinsn lt->bit31 (((bits :u32))
                             ((x (t (:ne bits)))
                              (y (t (:ne bits)))))

  (xor bits x y)
  (srawi bits bits 31)
  (or bits bits x)
  (subf bits y bits)
  (srwi bits bits 31))              ; bits = 0000...000X

(define-ppc-vinsn ltu->bit31 (((bits :u32))
                              ((x :u32)
                               (y :u32)))
  (subfc bits y x)
  (subfe bits bits bits)
  (neg bits bits))

(define-ppc-vinsn le->bit31 (((bits :u32))
                             ((x (t (:ne bits)))
                              (y (t (:ne bits)))))

  (xor bits x y)
  (srawi bits bits 31)
  (nor bits bits y)
  (add bits bits x)
  (srwi bits bits 31))              ; bits = 0000...000X

(define-ppc-vinsn leu->bit31  (((bits :u32))
                               ((x :u32)
                                (y :u32)))
  (subfc bits x y)
  (addze bits ppc::rzero))

(define-ppc-vinsn gt->bit31 (((bits :u32))
                             ((x (t (:ne bits)))
                              (y (t (:ne bits)))))

  (eqv bits x y)
  (srawi bits bits 31)
  (and bits bits x)
  (subf bits bits y)
  (srwi bits bits 31))              ; bits = 0000...000X

(define-ppc-vinsn gtu->bit31 (((bits :u32))
                               ((x :u32)
                                (y :u32)))
  (subfc bits x y)
  (subfe bits bits bits)
  (neg bits bits))

(define-ppc-vinsn ge->bit31 (((bits :u32))
                             ((x (t (:ne bits)))
                              (y (t (:ne bits)))))
  (eqv bits x y)
  (srawi bits bits 31)
  (andc bits bits x)
  (add bits bits y)
  (srwi bits bits 31))              ; bits = 0000...000X

(define-ppc-vinsn geu->bit31 (((bits :u32))
                               ((x :u32)
                                (y :u32)))
  (subfc bits y x)
  (addze bits ppc::rzero))


; there are big-time latencies associated with MFCR on more heavily
; pipelined processors; that implies that we should avoid this like
; the plague.
; GSO can't find anything much quicker for LT or GT, even though
; MFCR takes three cycles and waits for previous instructions to complete.
; Of course, using a CR field costs us something as well.
(define-ppc-vinsn crbit->bit31 (((bits :u32))
                                ((crf :crf)
                                 (bitnum :crbit))
                                ())
  (mfcr bits)                           ; Suffer.
  (rlwinm bits bits (:apply + 1  bitnum (:apply ash crf 2)) 31 31))    ; bits = 0000...000X


(define-ppc-vinsn compare (((crf :crf))
                           ((arg0 t)
                            (arg1 t))
                           ())
  (cmpw crf arg0 arg1))

(define-ppc-vinsn compare-to-nil (((crf :crf))
                                  ((arg0 t)))
  (cmpwi crf arg0 ppc::nil-value))

(define-ppc-vinsn compare-logical (((crf :crf))
                                   ((arg0 t)
                                    (arg1 t))
                                   ())
  (cmplw crf arg0 arg1))

(define-ppc-vinsn double-float-compare (((crf :crf))
                                        ((arg0 :double-float)
                                         (arg1 :double-float))
                                        ())
  (fcmpo crf arg0 arg1))
              

(define-ppc-vinsn double-float+-2 (((result :double-float))
                                   ((x :double-float)
                                    (y :double-float))
                                   ((crf (:crf 4))))
  (fadd result x y))

(define-ppc-vinsn double-float--2 (((result :double-float))
                                   ((x :double-float)
                                    (y :double-float))
                                   ((crf (:crf 4))))
  (fsub result x y))

(define-ppc-vinsn double-float*-2 (((result :double-float))
                                   ((x :double-float)
                                    (y :double-float))
                                   ((crf (:crf 4))))
  (fmul result x y))

(define-ppc-vinsn double-float/-2 (((result :double-float))
                                   ((x :double-float)
                                    (y :double-float))
                                   ((crf (:crf 4))))
  (fdiv result x y))

(define-ppc-vinsn single-float+-2 (((result :single-float))
                                   ((x :single-float)
                                    (y :single-float))
                                   ((crf (:crf 4))))
  (fadds result x y))

(define-ppc-vinsn single-float--2 (((result :single-float))
                                   ((x :single-float)
                                    (y :single-float))
                                   ((crf (:crf 4))))
  (fsubs result x y))

(define-ppc-vinsn single-float*-2 (((result :single-float))
                                   ((x :single-float)
                                    (y :single-float))
                                   ((crf (:crf 4))))
  (fmuls result x y))

(define-ppc-vinsn single-float/-2 (((result :single-float))
                                   ((x :single-float)
                                    (y :single-float))
                                   ((crf (:crf 4))))
  (fdivs result x y))





(define-ppc-vinsn compare-unsigned (((crf :crf))
                                    ((arg0 :imm)
                                     (arg1 :imm))
                                    ())
  (cmplw crf arg0 arg1))

(define-ppc-vinsn compare-signed-s16const (((crf :crf))
                                           ((arg0 :imm)
                                            (imm :s16const))
                                           ())
  (cmpwi crf arg0 imm))

(define-ppc-vinsn compare-unsigned-u16const (((crf :crf))
                                             ((arg0 :u32)
                                              (imm :u16const))
                                             ())
  (cmplwi crf arg0 imm))



;; Extract a constant bit (0-31) from src; make it be bit 31 of dest.
;; Bitnum is treated mod 32.
(define-ppc-vinsn extract-constant-ppc-bit (((dest :u32))
                                            ((src :imm)
                                             (bitnum :u16const))
                                            ())
  (rlwinm dest src (:apply + 1 bitnum) 31 31))


(define-ppc-vinsn set-constant-ppc-bit-to-variable-value (((dest :u32))
                                                          ((src :u32)
                                                           (bitval :u32) ; 0 or 1
                                                           (bitnum :u8const)))
  (rlwimi dest bitval (:apply - 31 bitnum) bitnum bitnum))

(define-ppc-vinsn set-constant-ppc-bit-to-1 (((dest :u32))
                                             ((src :u32)
                                              (bitnum :u8const)))
  ((:pred < bitnum 16)
   (oris dest src (:apply ash #x8000 (:apply - bitnum))))
  ((:pred >= bitnum 16)
   (ori dest src (:apply ash #x8000 (:apply - (:apply - bitnum 16))))))

(define-ppc-vinsn set-constant-ppc-bit-to-0 (((dest :u32))
                                             ((src :u32)
                                              (bitnum :u8const)))
  (rlwinm dest src 0 (:apply logand #x1f (:apply 1+ bitnum)) (:apply logand #x1f (:apply 1- bitnum))))

  
(define-ppc-vinsn insert-bit-0 (((dest :u32))
                                ((src :u32)
                                 (val :u32)))
  (rlwimi dest val 0 0 0))
  
; The bit number is boxed and wants to think of the least-significant bit as 0.
; Imagine that.
; To turn the boxed, lsb-0 bitnumber into an unboxed, msb-0 rotate count,
; we (conceptually) unbox it, add arch::fixnumshift to it, subtract it from
; 31, and add one.  This can also be done as "unbox and subtract from 28",
; I think ...
; Actually, it'd be "unbox, then subtract from 30".
(define-ppc-vinsn extract-variable-non-insane-bit (((dest :u32))
                                                   ((src :imm)
                                                    (bit :imm))
                                                   ((temp :u32)))
  (srwi temp bit arch::fixnumshift)
  (subfic temp temp (- 32 arch::fixnumshift))
  (rlwnm dest src temp 31 31))
                                               
; Operations on lists and cons cells

(define-ppc-vinsn %cdr (((dest :lisp))
                        ((src :lisp)))
  (lwz dest arch::cons.cdr src))

(define-ppc-vinsn %car (((dest :lisp))
                        ((src :lisp)))
  (lwz dest arch::cons.car src))

(define-ppc-vinsn %set-car (()
                            ((cell :lisp)
                             (new :lisp)))
  (stw new arch::cons.car cell))

(define-ppc-vinsn %set-cdr (()
                            ((cell :lisp)
                             (new :lisp)))
  (stw new arch::cons.cdr cell))

(define-ppc-vinsn load-adl (()
                            ((n :u32const)))
  (lis ppc::nargs (:apply ldb (byte 16 16) n))
  (ori ppc::nargs ppc::nargs (:apply ldb (byte 16 0) n)))
                            
(define-ppc-vinsn set-nargs (()
                             ((n :s16const)))
  (li ppc::nargs (:apply ash n arch::word-shift)))

(define-ppc-vinsn scale-nargs (()
                               ((nfixed :s16const)))
  ((:pred > nfixed 0)
   (la ppc::nargs (:apply - (:apply ash nfixed arch::word-shift)) ppc::nargs)))
                           


(define-ppc-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (stwu reg -4 ppc::vsp))

(define-ppc-vinsn (vpush-register-arg :push :node :vsp :outgoing-argument)
    (()
     ((reg :lisp)))
  (stwu reg -4 ppc::vsp))

(define-ppc-vinsn (vpop-register :pop :node :vsp)
    (((dest :lisp))
     ())
  (lwz dest 0 ppc::vsp)
  (la ppc::vsp 4 ppc::vsp))


(define-ppc-vinsn copy-node-gpr (((dest :lisp))
                                 ((src :lisp)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src)))

(define-ppc-vinsn copy-gpr (((dest t))
                            ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (mr dest src)))


(define-ppc-vinsn copy-fpr (((dest t))
                            ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (fmr dest src)))

(define-ppc-vinsn vcell-ref (((dest :lisp))
                             ((vcell :lisp)))
  (lwz dest arch::misc-data-offset vcell))

(define-ppc-vinsn vcell-set (()
                             ((vcell :lisp)
                              (value :lisp)))
  (stw value arch::misc-data-offset vcell))


(define-ppc-vinsn make-vcell (((dest :lisp))
                              ((closed (:lisp :ne dest)))
                              ((header :u32)))
  (li header arch::value-cell-header)
  (la ppc::allocptr (- arch::fulltag-misc arch::value-cell.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header arch::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stw closed arch::value-cell.value dest))

(define-ppc-vinsn make-tsp-vcell (((dest :lisp))
                                  ((closed :lisp))
                                  ((header :u32)))
  (li header arch::value-cell-header)
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfd ppc::fp-zero 8 ppc::tsp)
  (stw ppc::rzero 4 ppc::tsp)
  (stw header (+ 8 arch::fulltag-misc arch::value-cell.header) ppc::tsp)
  (stw closed (+ 8 arch::fulltag-misc arch::value-cell.value) ppc::tsp)
  (la dest (+ 8 arch::fulltag-misc) ppc::tsp))

(define-ppc-vinsn make-tsp-cons (((dest :lisp))
                                 ((car :lisp) (cdr :lisp))
                                 ())
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfd ppc::fp-zero 8 ppc::tsp)
  (stw ppc::rzero 4 ppc::tsp)
  (stw car (+ 8 arch::fulltag-cons arch::cons.car) ppc::tsp)
  (stw cdr (+ 8 arch::fulltag-cons arch::cons.cdr) ppc::tsp)
  (la dest (+ 8 arch::fulltag-cons) ppc::tsp))


(define-ppc-vinsn %closure-code% (((dest :lisp))
                                  ())
  (lwz dest (+ arch::symbol.vcell (arch::nrs-offset %closure-code%) ppc::nil-value) 0))


(define-ppc-vinsn (call-subprim :call :subprim-call) (()
                                                      ((spno :s32const)))
  (bla spno))

(define-ppc-vinsn (jump-subprim :jumpLR) (()
                                          ((spno :s32const)))
  (ba spno))

; Same as "call-subprim", but gives us a place to 
; track args, results, etc.
(define-ppc-vinsn (call-subprim-0 :call :subprim-call) (((dest t))
                                                        ((spno :s32const)))
  (bla spno))

(define-ppc-vinsn (call-subprim-1 :call :subprim-call) (((dest t))
                                                        ((spno :s32const)
                                                         (z t)))
  (bla spno))
  
(define-ppc-vinsn (call-subprim-2 :call :subprim-call) (((dest t))
                                                        ((spno :s32const)
                                                         (y t)
                                                         (z t)))
  (bla spno))

(define-ppc-vinsn (call-subprim-3 :call :subprim-call) (((dest t))
                                                        ((spno :s32const)
                                                         (x t)
                                                         (y t)
                                                         (z t)))
  (bla spno))

(define-ppc-vinsn event-poll (()
                              ())
  (lwz ppc::nargs arch::tcr.interrupt-level ppc::rcontext)
  (twgti ppc::nargs 0))

                         
; Unconditional (pc-relative) branch
(define-ppc-vinsn (jump :jump)
    (()
     ((label :label)))
  (b label))

(define-ppc-vinsn (call-label :call) (()
                                      ((label :label)))
  (bl label))

; just like JUMP, only (implicitly) asserts that the following 
; code is somehow reachable.
(define-ppc-vinsn (non-barrier-jump :xref) (()
                                            ((label :label)))
  (b label))


(define-ppc-vinsn (cbranch-true :branch) (()
                                          ((label :label)
                                           (crf :crf)
                                           (crbit :u8const)))
  (bt (:apply + crf crbit) label))

(define-ppc-vinsn (cbranch-false :branch) (()
                                           ((label :label)
                                            (crf :crf)
                                            (crbit :u8const)))
  (bf (:apply + crf crbit) label))

(define-ppc-vinsn check-trap-error (()
                                    ())
  (beq+ 0 :no-error)
  (uuo_interr arch::error-reg-regnum ppc::arg_z)
  :no-error)


(define-ppc-vinsn lisp-word-ref (((dest t))
                                 ((base t)
                                  (offset t)))
  (lwzx dest base offset))

(define-ppc-vinsn lisp-word-ref-c (((dest t))
                                   ((base t)
                                    (offset :s16const)))
  (lwz dest offset base))

  

;; Load an unsigned, 32-bit constant into a destination register.
(define-ppc-vinsn (lwi :constant-ref) (((dest :imm))
                                       ((intval :u32const))
                                       ())
  ((:or (:pred = (:apply ash intval -15) #x1ffff)
        (:pred = (:apply ash intval -15) #x0))
   (li dest (:apply %word-to-int (:apply logand #xffff intval))))
  ((:not                                ; that's :else to you, bub.
    (:or (:pred = (:apply ash intval -15) #x1ffff)
         (:pred = (:apply ash intval -15) #x0)))
   ((:pred = (:apply ash intval -15) 1)
    (ori dest ppc::rzero (:apply logand intval #xffff)))
   ((:not (:pred = (:apply ash intval -15) 1))
    (lis dest (:apply ash intval -16))
    ((:not (:pred = 0 (:apply logand intval #xffff)))
     (ori dest dest (:apply logand intval #xffff))))))

; Exactly the same thing, but take a signed integer value
(define-ppc-vinsn lwi-s32 (((dest :imm))
                           ((intval :s32const))
                           ())
  ((:or (:pred = (:apply ash intval -15) -1)
        (:pred = (:apply ash intval -15) #x0))
   (li dest (:apply %word-to-int (:apply logand #xffff intval))))
  ((:not                                ; that's :else to you, bub.
    (:or (:pred = (:apply ash intval -15) -1)
         (:pred = (:apply ash intval -15) #x0)))
   ((:pred = (:apply ash intval -15) 1)
    (ori dest ppc::rzero (:apply logand intval #xffff)))
   ((:not (:pred = (:apply ash intval -15) 1))
    (lis dest (:apply ash intval -16))
    ((:not (:pred = 0 (:apply logand intval #xffff)))
     (ori dest dest (:apply logand intval #xffff))))))

(define-ppc-vinsn discard-temp-frame (()
                                      ())
  (lwz ppc::tsp 0 ppc::tsp))


;;; Somewhere, deep inside the "OS_X_PPC_RuntimeConventions.pdf"
;;; document, they bother to document the fact that SP should
;;; maintain 16-byte alignment on OSX.  (The example prologue
;;; code in that document incorrectly assumes 8-byte alignment.
;;; Or something.  It's wrong in a number of other ways.)
;;; The caller always has to reserve a 24-byte linkage area
;;; (large chunks of which are unused).
(define-ppc-vinsn alloc-c-frame (()
                                 ((n-c-args :u16const)))
  ;; Always reserve space for at least 8 args and space for a lisp
  ;; frame (for the kernel) underneath it.
  ;; Do a stack-probe ...  Or maybe not
  ;;(lwz ppc::nargs (arch::kernel-global cs-overflow-limit) ppc::rnil)
  ;;(twllt ppc::sp ppc::nargs)
  ;; Zero the c-frame's savelr field, not that the GC cares ..
  ((:pred <= n-c-args 10)
   (stwu ppc::sp (- (+ 8 ppc::c-frame.size ppc::lisp-frame.size)) ppc::sp))
  ((:pred > n-c-args 10)
   ;; A normal C frame has room for 10 args (when padded out to
   ;; 16-byte alignment. Add enough double words to accomodate the
   ;; remaining args, in multiples of 4.
   (stwu ppc::sp (:apply - (:apply +
                                   8
                                   (+ ppc::c-frame.size ppc::lisp-frame.size)
                                   (:apply ash
                                           (:apply logand
                                                   (lognot 3)
                                                   (:apply
                                                    +
                                                    3
                                                    (:apply - n-c-args 10)))
                                           2)))
         ppc::sp))
  (stw ppc::rzero ppc::c-frame.savelr ppc::sp))

(define-ppc-vinsn alloc-eabi-c-frame (()
                                      ((n-c-args :u16const)))
                                        ; Always reserve space for at least 8 args and space for a lisp
                                        ; frame (for the kernel) underneath it.  Store NIL inthe c-frame's
                                        ; savelr field, so that the kernel doesn't mistake this for a lisp
                                        ; frame.
  ((:pred <= n-c-args 8)
   (stwu ppc::sp (- (+ ppc::eabi-c-frame.size ppc::lisp-frame.size)) ppc::sp))
  ((:pred > n-c-args 8)
                                        ; A normal C frame has room for 8 args. Add enough double words to accomodate the remaining args
   (stwu ppc::sp (:apply - (:apply + 
                                   (+ ppc::eabi-c-frame.size ppc::lisp-frame.size)
                                   (:apply ash
                                           (:apply logand
                                                   (lognot 1)
                                                   (:apply
                                                    1+
                                                    (:apply - n-c-args 8)))
                                           2)))
         ppc::sp))
  (stw ppc::sp ppc::eabi-c-frame.savelr ppc::sp))

; We should rarely have to do this.  It's easier to just generate code
; to do the memory reference than it would be to keep track of the size
; of each frame.
(define-ppc-vinsn discard-c-frame (()
                                   ())
  (lwz ppc::sp 0 ppc::sp))




(define-ppc-vinsn set-c-arg (()
                             ((argval :u32)
                              (argnum :u16const)))
  (stw argval (:apply + ppc::c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn set-single-c-arg (()
                                    ((argval :single-float)
                                     (argnum :u16const)))
  (stfs argval (:apply + ppc::c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn set-double-c-arg (()
                                    ((argval :double-float)
                                     (argnum :u16const)))
  (stfd argval (:apply + ppc::c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn reload-single-c-arg (((argval :single-float))
                                       ((argnum :u16const)))
  (lfs argval (:apply + ppc::c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn reload-double-c-arg (((argval :double-float))
                                       ((argnum :u16const)))
  (lfd argval (:apply + ppc::c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn set-eabi-c-arg (()
                                  ((argval :u32)
                                   (argnum :u16const)))
  (stw argval (:apply + ppc::eabi-c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn set-single-eabi-c-arg (()
                                         ((argval :single-float)
                                          (argnum :u16const)))
  (stfs argval (:apply + ppc::eabi-c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn set-double-eabi-c-arg (()
                                         ((argval :double-float)
                                          (argnum :u16const)))
  (stfd argval (:apply + ppc::eabi-c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn reload-single-eabi-c-arg (((argval :single-float))
                                            ((argnum :u16const)))
  (lfs argval (:apply + ppc::eabi-c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn reload-double-eabi-c-arg (((argval :double-float))
                                            ((argnum :u16const)))
  (lfd argval (:apply + ppc::eabi-c-frame.param0 (:apply ash argnum arch::word-shift)) ppc::sp))

(define-ppc-vinsn (load-nil :constant-ref) (((dest t))
                                            ())
  (li dest ppc::nil-value))

(define-ppc-vinsn (load-t :constant-ref) (((dest t))
                                          ())
  (li dest (+ arch::t-offset ppc::nil-value)))

(define-ppc-vinsn set-eq-bit (((dest :crf))
                              ())
  (cror (:apply + ppc::ppc-eq-bit dest)
        (:apply + ppc::ppc-eq-bit dest)
        (:apply + ppc::ppc-eq-bit dest)))

(define-ppc-vinsn (ref-constant :constant-ref) (((dest :lisp))
                                                ((src :s16const)))
  (lwz dest (:apply + arch::misc-data-offset (:apply ash (:apply 1+ src) 2)) ppc::fn))

(define-ppc-vinsn ref-indexed-constant (((dest :lisp))
                                        ((idxreg :s32)))
  (lwzx dest ppc::fn idxreg))


(define-ppc-vinsn cons (((dest :lisp))
                        ((newcar :lisp)
                         (newcdr :lisp)))
  (la ppc::allocptr (- arch::fulltag-cons arch::cons.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw newcdr arch::cons.cdr ppc::allocptr)
  (stw newcar arch::cons.car ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits))



;; subtag had better be a PPC-NODE-SUBTAG of some sort!
(define-ppc-vinsn %ppc-gvector (((dest :lisp))
                                ((Rheader :u32) 
                                 (nbytes :u32const))
                                ((immtemp0 :u32)
                                 (nodetemp :lisp)
                                 (crf :crf)))
  (la ppc::allocptr (:apply - arch::fulltag-misc
                            (:apply logand (lognot 7)
                                    (:apply + (+ 7 4) nbytes)))
      ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw Rheader arch::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  ((:not (:pred = nbytes 0))
   (li immtemp0 (:apply + arch::misc-data-offset nbytes))
   :loop
   (subi immtemp0 immtemp0 4)
   (cmpwi crf immtemp0 arch::misc-data-offset)
   (lwz nodetemp 0 ppc::vsp)
   (la ppc::vsp 4 ppc::vsp)
   (stwx nodetemp dest immtemp0)
   (bne crf :loop)))

;; allocate a small (phys size <= 32K bytes) misc obj of known size/subtag
(define-ppc-vinsn %alloc-misc-fixed (((dest :lisp))
                                     ((Rheader :u32)
                                      (nbytes :u32const)))
  (la ppc::allocptr (:apply - arch::fulltag-misc
                            (:apply logand (lognot 7)
                                    (:apply + (+ 7 4) nbytes)))
      ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw Rheader arch::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits))

(define-ppc-vinsn vstack-discard (()
                                  ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   (la ppc::vsp (:apply ash nwords arch::word-shift) ppc::vsp)))


(define-ppc-vinsn lcell-load (((dest :lisp))
                              ((cell :lcell)
                               (top :lcell)))
  (lwz dest (:apply - 
                    (:apply - (:apply calc-lcell-depth top) 4)
                    (:apply calc-lcell-offset cell)) ppc::vsp))

(define-ppc-vinsn vframe-load (((dest :lisp))
                               ((frame-offset :u16const)
                                (cur-vsp :u16const)))
  (lwz dest (:apply - (:apply - cur-vsp 4) frame-offset) ppc::vsp))

(define-ppc-vinsn lcell-store (()
                               ((src :lisp)
                                (cell :lcell)
                                (top :lcell)))
  (stw src (:apply - 
                   (:apply - (:apply calc-lcell-depth top) 4)
                   (:apply calc-lcell-offset cell)) ppc::vsp))

(define-ppc-vinsn vframe-store (()
                                ((src :lisp)
                                 (frame-offset :u16const)
                                 (cur-vsp :u16const)))
  (stw src (:apply - (:apply - cur-vsp 4) frame-offset) ppc::vsp))

(define-ppc-vinsn load-vframe-address (((dest :imm))
                                       ((offset :s16const)))
  (la dest offset ppc::vsp))

(define-ppc-vinsn copy-lexpr-argument (()
                                       ()
                                       ((temp :lisp)))
  (lwzx temp ppc::vsp ppc::nargs)
  (stwu temp -4 ppc::vsp))

; Boxing/unboxing of integers.

; Treat the low 8 bits of VAL as an unsigned integer; set RESULT to the equivalent fixnum.
(define-ppc-vinsn u8->fixnum (((result :imm)) 
                              ((val :u8)) 
                              ())
  (rlwinm result val arch::fixnumshift (- arch::nbits-in-word (+ 8 arch::fixnumshift)) (- arch::least-significant-bit arch::fixnumshift)))

; Treat the low 8 bits of VAL as a signed integer; set RESULT to the equivalent fixnum.
(define-ppc-vinsn s8->fixnum (((result :imm)) 
                              ((val :s8)) 
                              ())
  (extlwi result val 8 (- arch::nbits-in-word 8))
  (srawi result result (- (- arch::nbits-in-word 8) arch::fixnumshift)))


; Treat the low 16 bits of VAL as an unsigned integer; set RESULT to the equivalent fixnum.
(define-ppc-vinsn u16->fixnum (((result :imm)) 
                               ((val :u16)) 
                               ())
  (rlwinm result val arch::fixnumshift (- arch::nbits-in-word (+ 16 arch::fixnumshift)) (- arch::least-significant-bit arch::fixnumshift)))

; Treat the low 16 bits of VAL as a signed integer; set RESULT to the equivalent fixnum.
(define-ppc-vinsn s16->fixnum (((result :imm)) 
                               ((val :s16)) 
                               ())
  (extlwi result val 16 (- arch::nbits-in-word 16))
  (srawi result result (- (- arch::nbits-in-word 16) arch::fixnumshift)))

(define-ppc-vinsn fixnum->s16 (((result :s16))
                               ((src :imm)))
  (srawi result src arch::fixnumshift))

; A signed 32-bit untagged value can be at worst a 1-digit bignum.
; There should be something very much like this that takes a stack-consed
; bignum result ...
(define-ppc-vinsn s32->integer (((result :lisp))
                                ((src :s32))
                                ((crf (:crf 0)) ; a casualty
                                 (temp :s32)))        
  (addo temp src src)
  (addo. result temp temp)
  (bns+ :done)
  (mtxer ppc::rzero)
  (li temp arch::one-digit-bignum-header)
  (la ppc::allocptr (- arch::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw temp arch::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stw src arch::misc-data-offset result)
  :done)


; An unsigned 32-bit untagged value can be either a 1 or a 2-digit bignum.
(define-ppc-vinsn u32->integer (((result :lisp))
                                ((src :u32))
                                ((crf (:crf 0)) ; a casualty
                                 (temp :s32)
				 (size :u32)))
  (clrrwi. temp src (- arch::least-significant-bit arch::nfixnumtagbits))
  (slwi result src arch::fixnumshift)
  (beq+ crf :done)
  (cmpwi src 0)
  (li temp arch::one-digit-bignum-header)
  (li size (- 8 arch::fulltag-misc))
  (bgt :common)
  (li temp arch::two-digit-bignum-header)
  (li size (- 16 arch::fulltag-misc))
  :common
  (sub ppc::allocptr ppc::allocptr size)
  (twllt ppc::allocptr ppc::allocbase)
  (stw temp arch::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stw src arch::misc-data-offset result)
  :done)

(define-ppc-vinsn u16->u32 (((dest :u32))
                            ((src :u16)))
  (clrlwi dest src 16))

(define-ppc-vinsn u8->u32 (((dest :u32))
                           ((src :u8)))
  (clrlwi dest src 24))


(define-ppc-vinsn s16->s32 (((dest :s32))
                            ((src :s16)))
  (extsh dest src))

(define-ppc-vinsn s8->s32 (((dest :s32))
                           ((src :s8)))
  (extsb dest src))


; ... of floats ...

; Heap-cons a double-float to store contents of FPREG.  Hope that we don't do
; this blindly.
(define-ppc-vinsn double->heap (((result :lisp)) ; tagged as a double-float
                                ((fpreg :double-float)) 
                                ((header-temp :u32)))
  (li header-temp (arch::make-vheader arch::double-float.element-count arch::subtag-double-float))
  (la ppc::allocptr (- arch::fulltag-misc arch::double-float.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header-temp arch::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stfd fpreg arch::double-float.value result)  )


; This is about as bad as heap-consing a double-float.  (In terms of verbosity).
; Wouldn't kill us to do either/both out-of-line, but need to make visible to
; compiler so unnecessary heap-consing can be elided.
(define-ppc-vinsn single->heap (((result :lisp)) ; tagged as a single-float
                                ((fpreg :single-float))
                                ((header-temp :u32)))
  (li header-temp (arch::make-vheader arch::single-float.element-count arch::subtag-single-float))
  (la ppc::allocptr (- arch::fulltag-misc arch::single-float.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header-temp arch::misc-header-offset ppc::allocptr)
  (mr result ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stfs fpreg arch::single-float.value result))


; "dest" is preallocated, presumably on a stack somewhere.
(define-ppc-vinsn store-double (()
                                ((dest :lisp)
                                 (source :double-float))
                                ())
  (stfd source arch::double-float.value dest))

(define-ppc-vinsn get-double (((target :double-float))
                              ((source :lisp))
                              ())
  (lfd target arch::double-float.value source))

;;; Extract a double-float value, typechecking in the process.
;;; IWBNI we could simply call the "trap-unless-typecode=" vinsn here,
;;; instead of replicating it ..

(define-ppc-vinsn get-double? (((target :double-float))
                               ((source :lisp))
                               ((tag :u8)
                                (crf :crf)))
  (clrlwi tag source (- arch::nbits-in-word arch::nlisptagbits))
  (cmpwi crf tag arch::tag-misc)
  (bne crf :do-trap)
  (lbz tag arch::misc-subtag-offset source)
  :do-trap
  (twnei tag arch::subtag-double-float)
  (lfd target arch::double-float.value source))
  

(define-ppc-vinsn store-single (()
                                ((dest :lisp)
                                 (source :single-float))
                                ())
  (stfs source arch::single-float.value dest))

(define-ppc-vinsn get-single (((target :single-float))
                              ((source :lisp))
                              ())
  (lfs target arch::single-float.value source))

; ... of characters ...
(define-ppc-vinsn charcode->u16 (((dest :u16))
                                 ((src :imm))
                                 ())
  (srwi dest src arch::charcode-shift))

(define-ppc-vinsn character->fixnum (((dest :lisp))
                                     ((src :lisp))
                                     ())
  (rlwinm dest
          src
          (- arch::nbits-in-word (- arch::charcode-shift arch::fixnumshift))
          (- arch::nbits-in-word (+ arch::charcode-shift arch::fixnumshift)) 
          (- arch::least-significant-bit arch::fixnumshift)))

(define-ppc-vinsn character->code (((dest :u32))
                                   ((src :lisp)))
  (rlwinm dest src arch::charcode-shift arch::charcode-shift arch::least-significant-bit))

(define-ppc-vinsn charcode->fixnum (((dest :lisp))
                                    ((src :imm))
                                    ())
  (rlwinm dest 
          src 
          (+ arch::charcode-shift arch::fixnumshift)  
          (- arch::nbits-in-word (+ arch::charcode-shift arch::fixnumshift))  
          (- arch::least-significant-bit arch::fixnumshift)))

(define-ppc-vinsn fixnum->char (((dest :lisp))
                                ((src :imm))
                                ())
  (rlwinm dest src (- arch::charcode-shift arch::fixnumshift) 8 (1- arch::charcode-shift))
  (addi dest dest arch::subtag-character))

(define-ppc-vinsn u8->char (((dest :lisp))
                            ((src :u8))
                            ())
  (rlwinm dest src arch::charcode-shift 8 (1- arch::charcode-shift))
  (addi dest dest arch::subtag-character))

;; ... Macptrs ...

(define-ppc-vinsn deref-macptr (((addr :address))
                                ((src :lisp))
                                ())
  (lwz addr arch::macptr.address src))

(define-ppc-vinsn set-macptr-address (()
                                      ((addr :address)
                                       (src :lisp))
                                      ())
  (stw addr arch::macptr.address src))


(define-ppc-vinsn macptr->heap (((dest :lisp))
                                ((address :address))
                                ((header :u32)))
  (li header (logior (ash arch::macptr.element-count arch::num-subtag-bits) arch::subtag-macptr))
  (la ppc::allocptr (- arch::fulltag-misc arch::macptr.size) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header arch::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stw address arch::macptr.address dest))

(define-ppc-vinsn macptr->stack (((dest :lisp))
                                 ((address :address))
                                 ((header :u32)))
  (li header arch::macptr-header)
  (stwu ppc::tsp (- (+ 8 arch::macptr.size)) ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stw header (+ 8 arch::fulltag-misc arch::macptr.header) ppc::tsp)
  (stw address (+ 8 arch::fulltag-misc arch::macptr.address) ppc::tsp)
  (la dest (+ 8 arch::fulltag-misc) ppc::tsp))

  
(define-ppc-vinsn adjust-stack-register (()
                                         ((reg t)
                                          (amount :s16const)))
  (la reg amount reg))

(define-ppc-vinsn adjust-vsp (()
                              ((amount :s16const)))
  (la ppc::vsp amount ppc::vsp))

;; Arithmetic on fixnums & unboxed numbers

(define-ppc-vinsn u32-lognot (((dest :u32))
                              ((src :u32))
                              ())
  (not dest src))

(define-ppc-vinsn fixnum-lognot (((dest :imm))
                                 ((src :imm))
                                 ((temp :u32)))
  (not temp src)
  (clrrwi dest temp arch::nfixnumtagbits))


(define-ppc-vinsn negate-fixnum-overflow-inline (((dest :lisp))
                                                 ((src :imm))
                                                 ((unboxed :s32)
                                                  (header :u32)))
  (nego. dest src)
  (bns+ :done)
  (mtxer ppc::rzero)
  (srawi unboxed dest arch::fixnumshift)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 arch::fixnumshift))))
  (li header arch::one-digit-bignum-header)
  (la ppc::allocptr (- arch::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header arch::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stw unboxed arch::misc-data-offset dest)
  :done)

(define-ppc-vinsn negate-fixnum-overflow-ool (()
                                              ((src :imm))
                                              )
  (nego. ppc::arg_z src)
  (bsola- .SPfix-overflow)
  :done)
  
                                                  
                                       
(define-ppc-vinsn negate-fixnum-no-ovf (((dest :lisp))
                                        ((src :imm)))
  
  (neg dest src))
  

(define-ppc-vinsn logior-high (((dest :imm))
                               ((src :imm)
                                (high :u16const)))
  (oris dest src high))

(define-ppc-vinsn logior-low (((dest :imm))
                              ((src :imm)
                               (low :u16const)))
  (ori dest src low))

                           
                           
(define-ppc-vinsn %logior2 (((dest :imm))
                            ((x :imm)
                             (y :imm))
                            ())
  (or dest x y))

(define-ppc-vinsn logand-high (((dest :imm))
                               ((src :imm)
                                (high :u16const))
                               ((crf0 (:crf 0))))
  (andis. dest src high))

(define-ppc-vinsn logand-low (((dest :imm))
                              ((src :imm)
                               (low :u16const))
                              ((crf0 (:crf 0))))
  (andi. dest src low))


(define-ppc-vinsn %logand2 (((dest :imm))
                            ((x :imm)
                             (y :imm))
                            ())
  (and dest x y))

(define-ppc-vinsn logxor-high (((dest :imm))
                               ((src :imm)
                                (high :u16const)))
  (xoris dest src high))

(define-ppc-vinsn logxor-low (((dest :imm))
                              ((src :imm)
                               (low :u16const)))
  (xori dest src low))

                           

(define-ppc-vinsn %logxor2 (((dest :imm))
                            ((x :imm)
                             (y :imm))
                            ())
  (xor dest x y))

(define-ppc-vinsn %ilsl (((dest :imm))
                         ((count :imm)
                          (src :imm))
                         ((temp :u32)
                          (crx :crf)))
  (cmpwi crx count (ash 31 arch::fixnumshift))
  (srwi temp count arch::fixnumshift)
  (slw dest src temp)
  (ble+ crx :foo)
  (li dest 0)
  :foo)

(define-ppc-vinsn %ilsl-c (((dest :imm))
                           ((count :u8const)
                            (src :imm)))
                                        ; Hard to use ppcmacroinstructions that expand into expressions involving variables.
  (rlwinm dest src count 0 (:apply - arch::least-significant-bit count)))


(define-ppc-vinsn %ilsr-c (((dest :imm))
                           ((count :u8const)
                            (src :imm)))
  (rlwinm dest src (:apply - arch::nbits-in-word count) count (- arch::least-significant-bit
                                                                 arch::fixnumshift)))



; 68k did the right thing for counts < 64 - fixnumshift but not if greater
; so load-byte fails in 3.0 also


(define-ppc-vinsn %iasr (((dest :imm))
                         ((count :imm)
                          (src :imm))
                         ((temp :s32)
                          (crx :crf)))
  (cmpwi crx count (ash 31 arch::fixnumshift))
  (srawi temp count arch::fixnumshift)
  (sraw temp src temp)
  (ble+ crx :foo)
  (srawi temp src 31)
  :foo
  (clrrwi dest temp arch::fixnumshift))

(define-ppc-vinsn %iasr-c (((dest :imm))
                           ((count :u8const)
                            (src :imm))
                           ((temp :s32)))
  (srawi temp src count)
  (clrrwi dest temp arch::fixnumshift))

(define-ppc-vinsn %ilsr (((dest :imm))
                         ((count :imm)
                          (src :imm))
                         ((temp :s32)
                          (crx :crf)))
  (cmpwi crx count (ash 31 arch::fixnumshift))
  (srwi temp count arch::fixnumshift)
  (srw temp src temp)
  (clrrwi dest temp arch::fixnumshift)
  (ble+ crx :foo)
  (li dest 0)
  :foo  
  )

(define-ppc-vinsn %ilsr-c (((dest :imm))
                           ((count :u8const)
                            (src :imm))
                           ((temp :s32)))
  (rlwinm temp src (:apply - 32 count) count 31)
  (clrrwi dest temp arch::fixnumshift))

(define-ppc-vinsn u32-shift-left (((dest :u32))
                                  ((src :u32)
                                   (count :u8const)))
  (rlwinm dest src count 0 (:apply - 31 count)))

(define-ppc-vinsn u32-shift-right (((dest :u32))
                                   ((src :u32)
                                    (count :u8const)))
  (rlwinm dest src (:apply - 32 count) count 31))

(define-ppc-vinsn sign-extend-halfword (((dest :imm))
                                        ((src :imm)))
  (slwi dest src (- 16 arch::fixnumshift))
  (srawi dest dest (- 16 arch::fixnumshift)))

(define-ppc-vinsn s32-highword (((dest :imm))
                                ((src :s32))
                                ((temp :s32)))
  (srawi temp src 16)
  (slwi dest temp arch::fixnumshift))

                            

(define-ppc-vinsn fixnum-add (((dest t))
                              ((x t)
                               (y t)))
  (add dest x y))


(define-ppc-vinsn fixnum-add-overflow-ool (()
                                           ((x :imm)
                                            (y :imm))
                                           ((cr0 (:crf 0))))
  (addo. ppc::arg_z x y)
  (bsola- .SPfix-overflow))

(define-ppc-vinsn fixnum-add-overflow-inline (((dest :lisp))
                                              ((x :imm)
                                               (y :imm))
                                              ((cr0 (:crf 0))
                                               (unboxed :s32)
                                               (header :u32)))
  (addo. dest x y)
  (bns+ cr0 :done)
  (mtxer ppc::rzero)
  (srawi unboxed dest arch::fixnumshift)
  (li header arch::one-digit-bignum-header)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 arch::fixnumshift))))
  (la ppc::allocptr (- arch::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header arch::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stw unboxed arch::misc-data-offset dest)
  :done)
  

  

;  (setq dest (- x y))
(define-ppc-vinsn fixnum-sub (((dest t))
                              ((x t)
                               (y t)))
  (subf dest y x))

(define-ppc-vinsn fixnum-sub-from-constant (((dest :imm))
                                            ((x :s16const)
                                             (y :imm)))
  (subfic dest y (:apply ash x arch::fixnumshift)))




(define-ppc-vinsn fixnum-sub-overflow-ool (()
                                           ((x :imm)
                                            (y :imm)))
  (subo. ppc::arg_z x y)
  (bsola- .SPfix-overflow))

(define-ppc-vinsn fixnum-sub-overflow-inline (((dest :lisp))
                                              ((x :imm)
                                               (y :imm))
                                              ((cr0 (:crf 0))
                                               (unboxed :s32)
                                               (header :u32)))
  (subo. dest x y)
  (bns+ cr0 :done)
  (mtxer ppc::rzero)
  (srawi unboxed dest arch::fixnumshift)
  (li header arch::one-digit-bignum-header)
  (xoris unboxed unboxed (logand #xffff (ash #xffff (- 32 16 arch::fixnumshift))))
  (la ppc::allocptr (- arch::fulltag-misc 8) ppc::allocptr)
  (twllt ppc::allocptr ppc::allocbase)
  (stw header arch::misc-header-offset ppc::allocptr)
  (mr dest ppc::allocptr)
  (clrrwi ppc::allocptr ppc::allocptr arch::ntagbits)
  (stw unboxed arch::misc-data-offset dest)
  :done)

; This is, of course, also "subtract-immediate."
(define-ppc-vinsn add-immediate (((dest t))
                                 ((src t)
                                  (upper :u32const)
                                  (lower :u32const)))
  ((:not (:pred = upper 0))
   (addis dest src upper)
   ((:not (:pred = lower 0))
    (addi dest dest lower)))
  ((:and (:pred = upper 0) (:not (:pred = lower 0)))
   (addi dest src lower)))

;This must unbox one reg, but hard to tell which is better.
;(The one with the smaller absolute value might be)
(define-ppc-vinsn multiply-fixnums (((dest :imm))
                                    ((a :imm)
                                     (b :imm))
                                    ((unboxed :s32)))
  (srawi unboxed b arch::fixnumshift)
  (mullw dest a unboxed))

(define-ppc-vinsn multiply-immediate (((dest :imm))
                                      ((boxed :imm)
                                       (const :s16const)))
  (mulli dest boxed const))

; Mask out the code field of a base character; the result
; should be EXACTLY = to subtag-base-char
(define-ppc-vinsn mask-base-char (((dest :u32))
                                  ((src :imm)))
  (rlwinm dest src 0 (1+ (- arch::least-significant-bit arch::charcode-shift)) (1- (- arch::nbits-in-word (+ arch::charcode-shift 8)))))

                             
;; Boundp, fboundp stuff.
(define-ppc-vinsn (svar-ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (bla .SPsvar-specrefcheck))

(define-ppc-vinsn (%svar-ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (bla .SPsvar-specref))

(define-ppc-vinsn (svar-setq-special :call :subprim-call)
    (()
     ((sym :lisp)
      (val :lisp)))
  (bla .SPsvar-specset))


(define-ppc-vinsn symbol-function (((val :lisp))
                                   ((sym (:lisp (:ne val))))
                                   ((crf :crf)
                                    (tag :u32)))
  (lwz val arch::symbol.fcell sym)
  (clrlwi tag val (- 32 arch::nlisptagbits))
  (cmpwi crf tag arch::tag-misc)
  (bne- crf :bad)
  (lbz tag arch::misc-subtag-offset val)
  (cmpwi crf tag arch::subtag-function)
  (beq+ crf :good)
  :bad 
  (uuo_interr arch::error-udf sym)
  :good)

(define-ppc-vinsn (temp-push-unboxed-word :push :word :tsp)
    (()
     ((w :u32)))
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stw w 8 ppc::tsp))

(define-ppc-vinsn (temp-pop-unboxed-word :pop :word :tsp)
    (((w :u32))
     ())
  (lwz w 8 ppc::tsp)
  (lwz ppc::tsp 0 ppc::tsp))

(define-ppc-vinsn (temp-push-double-float :push :doubleword :tsp)
    (((d :double-float))
     ())
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfd d 8 ppc::tsp))

(define-ppc-vinsn (temp-pop-double-float :pop :doubleword :tsp)
    (()
     ((d :double-float)))
  (lfd d 8 ppc::tsp)
  (lwz ppc::tsp 0 ppc::tsp))

(define-ppc-vinsn (temp-push-single-float :push :word :tsp)
    (((s :single-float))
     ())
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stfs s 8 ppc::tsp))

(define-ppc-vinsn (temp-pop-single-float :pop :word :tsp)
    (()
     ((s :single-float)))
  (lfs s 8 ppc::tsp)
  (lwz ppc::tsp 0 ppc::tsp))


(define-ppc-vinsn (save-nvrs-individually :push :node :vsp :multiple)
    (()
     ((first :u8const)))
  (stwu ppc::save0 -4 ppc::vsp)
  ((:pred <= first ppc::save1)
   (stwu ppc::save1 -4 ppc::vsp)
   ((:pred <= first ppc::save2)
    (stwu ppc::save2 -4 ppc::vsp)
    ((:pred <= first ppc::save3)
     (stwu ppc::save3 -4 ppc::vsp)
    ((:pred <= first ppc::save4)
     (stwu ppc::save4 -4 ppc::vsp)
    ((:pred <= first ppc::save5)
     (stwu ppc::save5 -4 ppc::vsp)
    ((:pred <= first ppc::save6)
     (stwu ppc::save6 -4 ppc::vsp)
     ((:pred = first ppc::save7)
      (stwu ppc::save7 -4 ppc::vsp)))))))))

(define-ppc-vinsn (save-nvrs :push :node :vsp :multiple)
              (()
               ((first :u8const)))
  ((:pred <= first ppc::save3)
   (subi ppc::vsp ppc::vsp (:apply * 4 (:apply - 32 first)))
   (stmw first 0 ppc::vsp))
  ((:pred >= first ppc::save2)
   (stwu ppc::save0 -4 ppc::vsp)
   ((:pred <= first ppc::save1)
    (stwu ppc::save1 -4 ppc::vsp)
    ((:pred = first ppc::save2)
     (stwu ppc::save2 -4 ppc::vsp)))))


(define-ppc-vinsn (restore-nvrs :pop :node :vsp :multiple)
    (()
     ((firstreg :u8const)
      (basereg :imm)
      (offset :s16const)))
  ((:pred <= firstreg ppc::save3)
   (lmw firstreg offset basereg))
  ((:pred = firstreg ppc::save2)
   (lwz ppc::save2 offset basereg)
   (lwz ppc::save1 (:apply + offset 4) basereg)
   (lwz ppc::save0 (:apply + offset 8) basereg))
  ((:pred = firstreg ppc::save1)
   (lwz ppc::save1 offset basereg)
   (lwz ppc::save0 (:apply + offset 4) basereg))
  ((:pred = firstreg ppc::save0)
   (lwz ppc::save0 offset basereg)))

(define-ppc-vinsn %current-frame-ptr (((dest :imm))
                                    ())
  (mr dest ppc::sp))

(define-ppc-vinsn %current-tcr (((dest :imm))
                                    ())
  (mr dest ppc::rcontext))

(define-ppc-vinsn (svar-dpayback :call :subprim-call) (()
                                                  ((n :s16const))
                                                  ((temp (:u32 #.ppc::imm0))))
  ((:pred > n 1)
   (li temp n)
   (bla .SPsvar-unbind-n))
  ((:pred = n 1)
   (bla .SPsvar-unbind)))

(define-ppc-vinsn zero-double-float-register 
    (((dest :double-float))
     ())
  (fmr dest ppc::fp-zero))

(define-ppc-vinsn zero-single-float-register 
    (((dest :single-float))
     ())
  (fmr dest ppc::fp-zero))

(define-ppc-vinsn load-double-float-constant
    (((dest :double-float))
     ((high t)
      (low t)))
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stw high 8 ppc::tsp)
  (stw low  12 ppc::tsp)
  (lfd dest 8 ppc::tsp)
  (lwz ppc::tsp 0 ppc::tsp))

(define-ppc-vinsn load-single-float-constant
    (((dest :single-float))
     ((src t)))
  (stwu ppc::tsp -16 ppc::tsp)
  (stw ppc::tsp 4 ppc::tsp)
  (stw src 12 ppc::tsp)
  (lfs dest 12 ppc::tsp)
  (lwz ppc::tsp 0 ppc::tsp))

(define-ppc-vinsn load-indexed-node (((node :lisp))
                                     ((base :lisp)
                                      (offset :s16const)))
  (lwz node offset base))

(define-ppc-vinsn recover-saved-vsp (((dest :imm))
                                     ())
  (lwz dest ppc::lisp-frame.savevsp ppc::sp))


(define-ppc-vinsn check-exact-nargs (()
                                     ((n :u16const)))
  (twnei ppc::nargs (:apply ash n 2)))

(define-ppc-vinsn check-min-nargs (()
                                   ((min :u16const)))
  (twllti ppc::nargs (:apply ash min 2)))

(define-ppc-vinsn check-max-nargs (()
                                   ((max :u16const)))
  (twlgti ppc::nargs (:apply ash max 2)))

; Save context and establish FN.  The current VSP is the the
; same as the caller's, e.g., no arguments were vpushed.
(define-ppc-vinsn save-lisp-context-vsp (()
                                         ()
                                         ((imm :u32)))
  (stwu ppc::sp (- ppc::lisp-frame.size) ppc::sp)
  (stw ppc::fn ppc::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc::lisp-frame.savelr ppc::sp)
  (stw ppc::vsp ppc::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (lwz imm arch::tcr.cs-limit ppc::rcontext)
  (twllt ppc::sp imm))

; Do the same thing via a subprim call.
(define-ppc-vinsn (save-lisp-context-vsp-ool :call :subprim-call)
    (()
     ()
     ((imm (:u32 #.ppc::imm0))))
  (bla .SPsavecontextvsp))

(define-ppc-vinsn save-lisp-context-offset (()
                                            ((nbytes-vpushed :u16const))
                                            ((imm :u32)))
  (la imm nbytes-vpushed ppc::vsp)
  (stwu ppc::sp (- ppc::lisp-frame.size) ppc::sp)
  (stw ppc::fn ppc::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc::lisp-frame.savelr ppc::sp)
  (stw imm ppc::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (lwz imm arch::tcr.cs-limit ppc::rcontext)
  (twllt ppc::sp imm))

(define-ppc-vinsn save-lisp-context-offset-ool (()
                                                ((nbytes-vpushed :u16const))
                                                ((imm (:u32 #.ppc::imm0))))
  (li imm nbytes-vpushed)
  (bla .SPsavecontext0))


(define-ppc-vinsn save-lisp-context-lexpr (()
                                           ()
                                           ((imm :u32)))
  (stwu ppc::sp (- ppc::lisp-frame.size) ppc::sp)
  (stw ppc::rzero ppc::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc::lisp-frame.savelr ppc::sp)
  (stw ppc::vsp ppc::lisp-frame.savevsp ppc::sp)
  (mr ppc::fn ppc::nfn)
  ;; Do a stack-probe ...
  (lwz imm arch::tcr.cs-limit ppc::rcontext)
  (twllt ppc::sp imm))
  
(define-ppc-vinsn save-cleanup-context (()
                                        ())
  ;; SP was this deep just a second ago, so no need to do a stack-probe.
  (mflr ppc::loc-pc)
  (stwu ppc::sp (- ppc::lisp-frame.size) ppc::sp)
  (stw ppc::rzero ppc::lisp-frame.savefn ppc::sp)
  (stw ppc::loc-pc ppc::lisp-frame.savelr ppc::sp)
  (stw ppc::vsp ppc::lisp-frame.savevsp ppc::sp))

;; Vpush the argument registers.  We got at least "min-fixed" args;
;; that knowledge may help us generate better code.
(define-ppc-vinsn (save-lexpr-argregs :call :subprim-call)
    (()
     ((min-fixed :u16const))
     ((crfx :crf)
      (crfy :crf)
      (entry-vsp (:u32 #.ppc::imm0))
      (arg-temp :u32)))
  ((:pred >= min-fixed $numppcargregs)
   (stwu ppc::arg_x -4 ppc::vsp)
   (stwu ppc::arg_y -4 ppc::vsp)
   (stwu ppc::arg_z -4 ppc::vsp))
  ((:pred = min-fixed 2)                ; at least 2 args
   (cmplwi crfx ppc::nargs (ash 2 arch::word-shift))
   (beq crfx :yz2)                      ; skip arg_x if exactly 2
   (stwu ppc::arg_x -4 ppc::vsp)
   :yz2
   (stwu ppc::arg_y -4 ppc::vsp)
   (stwu ppc::arg_z -4 ppc::vsp))
  ((:pred = min-fixed 1)                ; at least one arg
   (cmplwi crfx ppc::nargs (ash 2 arch::word-shift))
   (blt crfx :z1)                       ; branch if exactly one
   (beq crfx :yz1)                      ; branch if exactly two
   (stwu ppc::arg_x -4 ppc::vsp)
   :yz1
   (stwu ppc::arg_y -4 ppc::vsp)
   :z1
   (stwu ppc::arg_z -4 ppc::vsp))
  ((:pred = min-fixed 0)
   (cmplwi crfx ppc::nargs (ash 2 arch::word-shift))
   (cmplwi crfy ppc::nargs 0)
   (beq crfx :yz0)                      ; exactly two
   (beq crfy :none)                     ; exactly zero
   (blt crfx :z0)                       ; one
                                        ; Three or more ...
   (stwu ppc::arg_x -4 ppc::vsp)
   :yz0
   (stwu ppc::arg_y -4 ppc::vsp)
   :z0
   (stwu ppc::arg_z -4 ppc::vsp)
   :none
   )
  ((:pred = min-fixed 0)
   (stwu ppc::nargs -4 ppc::vsp))
  ((:not (:pred = min-fixed 0))
   (subi arg-temp ppc::nargs (:apply ash min-fixed arch::word-shift))
   (stwu arg-temp -4 ppc::vsp))
  (add entry-vsp ppc::vsp ppc::nargs)
  (la entry-vsp 4 entry-vsp)
  (bla .SPlexpr-entry))


(define-ppc-vinsn (jump-return-pc :jumpLR)
    (()
     ())
  (blr))

(define-ppc-vinsn (restore-full-lisp-context :lispcontext :pop :csp :lrRestore)
    (()
     ())
  (lwz ppc::loc-pc ppc::lisp-frame.savelr ppc::sp)
  (lwz ppc::vsp ppc::lisp-frame.savevsp ppc::sp)
  (lwz ppc::fn ppc::lisp-frame.savefn ppc::sp)
  (mtlr ppc::loc-pc)
  (la ppc::sp ppc::lisp-frame.size ppc::sp))

(define-ppc-vinsn (restore-full-lisp-context-ool :lispcontext :pop :csp :lrRestore)
    (()
     ())
  (bla .SPrestorecontext)
  (mtlr ppc::loc-pc))

(define-ppc-vinsn (popj :lispcontext :pop :csp :lrRestore :jumpLR)
    (() 
     ())
  (ba .SPpopj))

; Exiting from an UNWIND-PROTECT cleanup is similar to
; (and a little simpler than) returning from a function.
(define-ppc-vinsn restore-cleanup-context (()
                                           ())
  (lwz ppc::loc-pc ppc::lisp-frame.savelr ppc::sp)
  (mtlr ppc::loc-pc)
  (la ppc::sp ppc::lisp-frame.size ppc::sp))



(define-ppc-vinsn default-1-arg (()
                                 ((min :u16const))
                                 ((crf :crf)))
  (cmplwi crf ppc::nargs (:apply ash min 2))
  (bne crf :done)
  ((:pred >= min 3)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 2)
   (mr ppc::arg_x ppc::arg_y))
  ((:pred >= min 1)
   (mr ppc::arg_y ppc::arg_z))
  (li ppc::arg_z ppc::nil-value)
  :done)

(define-ppc-vinsn default-2-args (()
                                  ((min :u16const))
                                  ((crf :crf)))
  (cmplwi crf ppc::nargs (:apply ash (:apply 1+ min) 2))
  (bgt crf :done)
  (beq crf :one)
                                        ; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (stwu ppc::arg_x -4 ppc::vsp))   
  ((:pred >= min 2)
   (stwu ppc::arg_y -4 ppc::vsp))
  ((:pred >= min 1)
   (mr ppc::arg_x ppc::arg_z))
  (li ppc::arg_y ppc::nil-value)
  (b :last)
  :one
                                        ; We got min+1 args: arg_y was supplied, arg_z defaults to nil.
  ((:pred >= min 2)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 1)
   (mr ppc::arg_x ppc::arg_y))
  (mr ppc::arg_y ppc::arg_z)
  :last
  (li ppc::arg_z ppc::nil-value)
  :done)

(define-ppc-vinsn default-3-args (()
                                  ((min :u16const))
                                  ((crfx :crf)
                                   (crfy :crf)))
  (cmplwi crfx ppc::nargs (:apply ash (:apply + 2 min) 2))
  (cmplwi crfy ppc::nargs (:apply ash min 2))
  (bgt crfx :done)
  (beq crfx :two)
  (beq crfy :none)
                                        ; The first (of three) &optional args was supplied.
  ((:pred >= min 2)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 1)
   (stwu ppc::arg_y -4 ppc::vsp))
  (mr ppc::arg_x ppc::arg_z)
  (b :last-2)
  :two
                                        ; The first two (of three) &optional args were supplied.
  ((:pred >= min 1)
   (stwu ppc::arg_x -4 ppc::vsp))
  (mr ppc::arg_x ppc::arg_y)
  (mr ppc::arg_y ppc::arg_z)
  (b :last-1)
                                        ; None of the three &optional args was provided.
  :none
  ((:pred >= min 3)
   (stwu ppc::arg_x -4 ppc::vsp))
  ((:pred >= min 2)
   (stwu ppc::arg_y -4 ppc::vsp))
  ((:pred >= min 1)
   (stwu ppc::arg_z -4 ppc::vsp))
  (li ppc::arg_x ppc::nil-value)
  :last-2
  (li ppc::arg_y ppc::nil-value)
  :last-1
  (li ppc::arg_z ppc::nil-value)
  :done)

(define-ppc-vinsn save-lr (()
                           ())
  (mflr ppc::loc-pc))

;; "n" is the sum of the number of required args + 
;; the number of &optionals.  
(define-ppc-vinsn (default-optionals :call :subprim-call) (()
                                                           ((n :u16const)))
  (li ppc::imm0 (:apply ash n 2))
  (bla .SPdefault-optional-args))

; fname contains a known symbol
(define-ppc-vinsn (call-known-symbol :call) (((result (:lisp ppc::arg_z)))
                                             ())
  (lwz ppc::nfn arch::symbol.fcell ppc::fname)
  (lwz ppc::temp0 arch::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctrl))

(define-ppc-vinsn (jump-known-symbol :jumplr) (()
                                               ())
  (lwz ppc::nfn arch::symbol.fcell ppc::fname)
  (lwz ppc::temp0 arch::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctr))

(define-ppc-vinsn (call-known-function :call) (()
                                               ())
  (lwz ppc::temp0 arch::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctrl))

(define-ppc-vinsn (jump-known-function :jumplr) (()
                                                 ())
  (lwz ppc::temp0 arch::misc-data-offset ppc::nfn)
  (mtctr ppc::temp0)
  (bctr))

(define-ppc-vinsn %schar (((char :imm))
                          ((str :lisp)
                           (idx :imm))
                          ((imm :u32)
                           (cr0 (:crf 0))))
  (srwi imm idx arch::fixnumshift)
  (addi imm imm arch::misc-data-offset)
  (lbzx imm str imm)
  (rlwinm imm imm arch::charcode-shift 8 (1- arch::charcode-shift))
  (addi char imm arch::subtag-character))

(define-ppc-vinsn %set-schar (()
                              ((str :lisp)
                               (idx :imm)
                               (char :imm))
                              ((imm :u32)
                               (imm1 :u32)
                               (cr0 (:crf 0))))
  (srwi imm idx arch::fixnumshift)
  (addi imm imm arch::misc-data-offset)
  (srwi imm1 char arch::charcode-shift)
  (stbx imm1 str imm)
  )

(define-ppc-vinsn %set-scharcode (()
                                  ((str :lisp)
                                   (idx :imm)
                                   (code :imm))
                                  ((imm :u32)
                                   (imm1 :u32)
                                   (cr0 (:crf 0))))
  (srwi imm idx arch::fixnumshift)
  (addi imm imm arch::misc-data-offset)
  (srwi imm1 code arch::fixnumshift)
  (stbx imm1 str imm)
  )


(define-ppc-vinsn %scharcode (((code :imm))
                              ((str :lisp)
                               (idx :imm))
                              ((imm :u32)
                               (cr0 (:crf 0))))
  (srwi imm idx arch::fixnumshift)
  (addi imm imm arch::misc-data-offset)
  (lbzx imm str imm)
  (slwi code imm arch::fixnumshift))

; Clobbers LR
(define-ppc-vinsn (%debug-trap :call :subprim-call) (()
                                                     ())
  (bla .SPbreakpoint)
  )


(define-ppc-vinsn eep.address (((dest t))
                               ((src (:lisp (:ne dest )))))
  (lwz dest (+ (ash 1 2) arch::misc-data-offset) src)
  (tweqi dest ppc::nil-value))
                 
(define-ppc-vinsn %u32+ (((dest :u32))
                         ((x :u32) (y :u32)))
  (add dest x y))

(define-ppc-vinsn %u32+-c (((dest :u32))
                           ((x :u32) (y :u16const)))
  (addi dest x y))

(define-ppc-vinsn %u32- (((dest :u32))
                         ((x :u32) (y :u32)))
  (sub dest x y))

(define-ppc-vinsn %u32--c (((dest :u32))
                           ((x :u32) (y :u16const)))
  (subi dest x y))

(define-ppc-vinsn %u32-logior (((dest :u32))
                               ((x :u32) (y :u32)))
  (or dest x y))

(define-ppc-vinsn %u32-logior-c (((dest :u32))
                                 ((x :u32) (high :u16const) (low :u16const)))
  ((:not (:pred = high 0))
   (oris dest x high))
  ((:not (:pred = low 0))
   (ori dest x low)))

(define-ppc-vinsn %u32-logxor (((dest :u32))
                               ((x :u32) (y :u32)))
  (xor dest x y))

(define-ppc-vinsn %u32-logxor-c (((dest :u32))
                                 ((x :u32) (high :u16const) (low :u16const)))
  ((:not (:pred = high 0))
   (xoris dest x high))
  ((:not (:pred = low 0))
   (xori dest x low)))

(define-ppc-vinsn %u32-logand (((dest :u32))
                               ((x :u32) (y :u32)))
  (and dest x y))

(define-ppc-vinsn %u32-logand-high-c (((dest :u32))
                                      ((x :u32) (high :u16const))
                                      ((cr0 (:crf 0))))
  (andis. dest x high))

(define-ppc-vinsn %u32-logand-low-c (((dest :u32))
                                     ((x :u32) (low :u16const))
                                     ((cr0 (:crf 0))))
  (andi. dest x low))

(define-ppc-vinsn %u32-logand-mask-c (((dest :u32))
                                      ((x :u32)
                                       (start :u8const)
                                       (end :u8const)))
  (rlwinm dest x 0 start end))

(define-ppc-vinsn disable-interrupts (((dest :lisp))
                                      ()
                                      ((temp :imm)))
  (li temp -4)
  (lwz dest arch::tcr.interrupt-level ppc::rcontext)
  (stw temp arch::tcr.interrupt-level ppc::rcontext))

;;; Subprim calls.  Done this way for the benefit of VINSN-OPTIMIZE.
(defmacro define-ppc-subprim-call-vinsn ((name &rest other-attrs) spno)
  `(define-ppc-vinsn (,name :call :subprim-call ,@other-attrs) (() ())
    (bla ,spno)))

(defmacro define-ppc-subprim-jump-vinsn ((name &rest other-attrs) spno)
  `(define-ppc-vinsn (,name :jump :jumpLR ,@other-attrs) (() ())
    (ba ,spno)))

(define-ppc-subprim-jump-vinsn (restore-interrupt-level) .SPrestoreintlevel)

(define-ppc-subprim-call-vinsn (save-values) .SPsave-values)

(define-ppc-subprim-call-vinsn (recover-values)  .SPrecover-values)

(define-ppc-subprim-call-vinsn (add-values) .SPadd-values)

(define-ppc-subprim-jump-vinsn (jump-known-symbol-ool) .SPjmpsym)

(define-ppc-subprim-call-vinsn (call-known-symbol-ool)  .SPjmpsym)

(define-ppc-subprim-call-vinsn (pass-multiple-values)  .SPmvpass)

(define-ppc-subprim-call-vinsn (pass-multiple-values-symbol) .SPmvpasssym)

(define-ppc-subprim-jump-vinsn (tail-call-sym-gen) .SPtcallsymgen)

(define-ppc-subprim-jump-vinsn (tail-call-fn-gen) .SPtcallnfngen)

(define-ppc-subprim-jump-vinsn (tail-call-sym-slide) .SPtcallsymslide)

(define-ppc-subprim-jump-vinsn (tail-call-fn-slide) .SPtcallnfnslide)

(define-ppc-subprim-jump-vinsn (tail-call-sym-vsp) .SPtcallsymvsp)

(define-ppc-subprim-jump-vinsn (tail-call-fn-vsp) .SPtcallnfnvsp)

(define-ppc-subprim-call-vinsn (funcall)  .SPfuncall)

(define-ppc-subprim-jump-vinsn (tail-funcall-gen) .SPtfuncallgen)

(define-ppc-subprim-jump-vinsn (tail-funcall-slide) .SPtfuncallslide)

(define-ppc-subprim-jump-vinsn (tail-funcall-vsp) .SPtfuncallvsp)

(define-ppc-subprim-call-vinsn (spread-lexpr)  .SPspread-lexpr-z)

(define-ppc-subprim-call-vinsn (spread-list)  .SPspreadargz)

(define-ppc-subprim-call-vinsn (pop-argument-registers)  .SPvpopargregs)

(define-ppc-subprim-call-vinsn (getxlong)  .SPgetXlong)

(define-ppc-subprim-call-vinsn (stack-cons-list)  .SPstkconslist)

(define-ppc-subprim-call-vinsn (list) .SPconslist)

(define-ppc-subprim-call-vinsn (stack-cons-list*)  .SPstkconslist-star)

(define-ppc-subprim-call-vinsn (list*) .SPconslist-star)

(define-ppc-subprim-call-vinsn (make-stack-block)  .SPmakestackblock)

(define-ppc-subprim-call-vinsn (make-stack-block0)  .Spmakestackblock0)

(define-ppc-subprim-call-vinsn (make-stack-list)  .Spmakestacklist)

(define-ppc-subprim-call-vinsn (make-stack-vector)  .SPmkstackv)

(define-ppc-subprim-call-vinsn (make-stack-gvector)  .SPstkgvector)

(define-ppc-subprim-call-vinsn (stack-misc-alloc)  .SPstack-misc-alloc)

(define-ppc-subprim-call-vinsn (stack-misc-alloc-init)  .SPstack-misc-alloc-init)

(define-ppc-subprim-call-vinsn (svar-bind-nil)  .SPsvar-bind-nil)

(define-ppc-subprim-call-vinsn (svar-bind-self)  .SPsvar-bind-self)

(define-ppc-subprim-call-vinsn (svar-bind-self-boundp-check)  .SPsvar-bind-self-boundp-check)

(define-ppc-subprim-call-vinsn (svar-bind)  .SPsvar-bind)

(define-ppc-subprim-jump-vinsn (nvalret :jumpLR) .SPnvalret)

(define-ppc-subprim-call-vinsn (nthrowvalues) .SPnthrowvalues)

(define-ppc-subprim-call-vinsn (nthrow1value) .SPnthrow1value)

(define-ppc-subprim-call-vinsn (slide-values) .SPmvslide)

(define-ppc-subprim-call-vinsn (macro-bind) .SPmacro-bind)

(define-ppc-subprim-call-vinsn (destructuring-bind-inner) .SPdestructuring-bind-inner)

(define-ppc-subprim-call-vinsn (destructuring-bind) .SPdestructuring-bind)

(define-ppc-subprim-call-vinsn (simple-keywords) .SPsimple-keywords)

(define-ppc-subprim-call-vinsn (keyword-args) .SPkeyword-args)

(define-ppc-subprim-call-vinsn (keyword-bind) .SPkeyword-bind)

(define-ppc-subprim-call-vinsn (stack-rest-arg) .SPstack-rest-arg)

(define-ppc-subprim-call-vinsn (req-stack-rest-arg) .SPreq-stack-rest-arg)

(define-ppc-subprim-call-vinsn (stack-cons-rest-arg) .SPstack-cons-rest-arg)

(define-ppc-subprim-call-vinsn (heap-rest-arg) .SPheap-rest-arg)

(define-ppc-subprim-call-vinsn (req-heap-rest-arg) .SPreq-heap-rest-arg)

(define-ppc-subprim-call-vinsn (heap-cons-rest-arg) .SPheap-cons-rest-arg)

(define-ppc-subprim-call-vinsn (opt-supplied-p) .SPopt-supplied-p)

(define-ppc-subprim-call-vinsn (gvector) .SPgvector)

(define-ppc-vinsn (nth-value :call :subprim-call) (((result :lisp))
                                                   ())
  (bla .SPnthvalue))

(define-ppc-subprim-call-vinsn (fitvals) .SPfitvals)

(define-ppc-subprim-call-vinsn (misc-alloc) .SPmisc-alloc)

(define-ppc-subprim-call-vinsn (misc-alloc-init) .SPmisc-alloc-init)

(define-ppc-subprim-call-vinsn (integer-sign) .SPinteger-sign)

;;; Even though it's implemented by calling a subprim, THROW is really
;;; a JUMP (to a possibly unknown destination).  If the destination's
;;; really known, it should probably be inlined (stack-cleanup, value
;;; transfer & jump ...)
(define-ppc-vinsn (throw :jump :jump-unknown) (()
                                               ())
  (bla .SPthrow))

(define-ppc-subprim-call-vinsn (mkcatchmv) .SPmkcatchmv)

(define-ppc-subprim-call-vinsn (mkcatch1v) .SPmkcatch1v)

(define-ppc-subprim-call-vinsn (setqsym) .SPsvar-setqsym)

(define-ppc-subprim-call-vinsn (ksignalerr) .SPksignalerr)

(define-ppc-subprim-call-vinsn (subtag-misc-ref) .SPsubtag-misc-ref)

(define-ppc-subprim-call-vinsn (subtag-misc-set) .SPsubtag-misc-set)

(define-ppc-subprim-call-vinsn (mkunwind) .SPmkunwind)

(define-ppc-subprim-call-vinsn (progvsave) .SPsvar-progvsave)

(define-ppc-subprim-jump-vinsn (progvrestore) .SPsvar-progvrestore)

(define-ppc-subprim-call-vinsn (syscall) .SPsyscall)

(define-ppc-subprim-call-vinsn (newblocktag) .SPnewblocktag)

(define-ppc-subprim-call-vinsn (newgotag) .SPnewgotag)

(define-ppc-subprim-call-vinsn (misc-ref) .SPmisc-ref)

(define-ppc-subprim-call-vinsn (misc-set) .SPmisc-set)

(define-ppc-subprim-call-vinsn (gets64) .SPgets64)

(define-ppc-subprim-call-vinsn (getu64) .SPgetu64)

(define-ppc-subprim-call-vinsn (makeu64) .SPmakeu64)

(define-ppc-subprim-call-vinsn (makes64) .SPmakes64)

(define-ppc-vinsn (darwin-syscall :call :subprim-call) (()
                                                        ())
  (stw ppc::rzero ppc::c-frame.crsave ppc::sp)
  (bla .SPdarwin-syscall))

(define-ppc-vinsn (darwin-syscall-s64 :call :subprim-call) (()
                                                            ())
  (stw ppc::sp ppc::c-frame.crsave ppc::sp)
  (bla .SPdarwin-syscall))

(define-ppc-subprim-call-vinsn (eabi-ff-call) .SPeabi-ff-call)

(define-ppc-subprim-call-vinsn (poweropen-ff-call) .SPffcall)

(define-ppc-subprim-call-vinsn (poweropen-ff-callX) .SPffcallX)



;;; In case ppc::*ppc-opcodes* was changed since this file was compiled.
(queue-fixup
 (fixup-vinsn-templates *ppc-vinsn-templates* ppc::*ppc-opcode-numbers*))

(ccl::provide "PPC-VINSNS")
