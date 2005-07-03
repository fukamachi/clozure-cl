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


;;; Do an FF-CALL to MakeDataExecutable so that the data cache gets flushed.
;;; If the GC moves this function while we're trying to flush the cache,
;;; it'll flush the cache: no harm done in that case.
(defppclapfunction %make-code-executable ((codev arg_z))
  (let ((len imm2)
	(word-offset imm0))
    (save-lisp-context)
    (getvheader word-offset codev)
    (header-size len word-offset)
    ;; The idea is that if we GC here, no harm is done (since the GC
    ;; will do any necessary cache-flushing.)  The idea may be
    ;; incorrect: if we pass an address that's not mapped anymore,
    ;; could we fault ?
    (stru sp (- (+ #+linuxppc-target ppc32::eabi-c-frame.minsize
		   #+darwinppc-target target::c-frame.minsize target::lisp-frame.size)) sp)	; make an FFI frame.
    (la imm0 target::misc-data-offset codev)
    (slri len len 2)
    (str imm0 #+linuxppc-target ppc32::eabi-c-frame.param0 #+darwinppc-target target::c-frame.param0  sp)
    (str len #+linuxppc-target ppc32::eabi-c-frame.param1 #+darwinppc-target target::c-frame.param1 sp)
    (ref-global imm3 kernel-imports)
    (ldr arg_z target::kernel-import-MakeDataExecutable imm3)
    (bla #+linuxppc-target .SPeabi-ff-call #+darwinppc-target .SPffcall)
    (li arg_z nil)
    (restore-full-lisp-context)
    (blr)))

(defppclapfunction %get-kernel-global-from-offset ((offset arg_z))
  (check-nargs 1)
  (unbox-fixnum imm0 offset)
  (addi imm0 imm0 target::nil-value)
  (ldr arg_z 0 imm0)
  (blr))

(defppclapfunction %set-kernel-global-from-offset ((offset arg_y) (new-value arg_z))
  (check-nargs 2)
  (unbox-fixnum imm0 offset)
  (addi imm0 imm0 target::nil-value)
  (str new-value 0 imm0)
  (blr))



(defppclapfunction %get-kernel-global-ptr-from-offset ((offset arg_y)
						       (ptr arg_z))
  (check-nargs 2)
  (unbox-fixnum imm0 offset)
  (addi imm0 imm0 target::nil-value)
  (ldr imm0 0 imm0)
  (str imm0 target::macptr.address ptr)
  (blr))




(defppclapfunction %fixnum-ref ((fixnum arg_y) #| &optional |# (offset arg_z))
  (cmpri cr0 nargs '1)
  (check-nargs 1 2)
  (bne cr0 @2-args)
  (mr fixnum offset)
  (li offset 0)
  @2-args
  (unbox-fixnum imm0 offset)
  (ldrx arg_z imm0 fixnum)
  (blr))


#+ppc32-target
(defppclapfunction %fixnum-ref-natural ((fixnum arg_y) #| &optional |# (offset arg_z))
  (cmpri cr0 nargs '1)
  (check-nargs 1 2)
  (bne cr0 @2-args)
  (mr fixnum offset)
  (li offset 0)
  @2-args
  (unbox-fixnum imm0 offset)
  (lwzx imm0 imm0 fixnum)
  (ba .SPmakeu32))

#+ppc64-target
(defppclapfunction %fixnum-ref-natural ((fixnum arg_y) #| &optional |# (offset arg_z))
  (cmpdi cr0 nargs '1)
  (check-nargs 1 2)
  (bne cr0 @2-args)
  (mr fixnum offset)
  (li offset 0)
  @2-args
  (unbox-fixnum imm0 offset)
  (ldx imm0 imm0 fixnum)
  (ba .SPmakeu64))

(defppclapfunction %fixnum-set ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (cmpi cr0 nargs '2)
  (check-nargs 2 3)
  (bne cr0 @3-args)
  (mr fixnum offset)
  (li offset 0)
  @3-args
  (unbox-fixnum imm0 offset)
  (strx new-value imm0 fixnum)
  (mr arg_z new-value)
  (blr))

#+ppc32-target
(defppclapfunction %fixnum-set-u32 ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (cmpwi cr0 nargs '2)
  (check-nargs 2 3)
  (bne cr0 @3-args)
  (mr fixnum offset)
  (li offset 0)
  @3-args
  (unbox-fixnum imm0 offset)
  (extract-typecode imm1 new-value)
  (cmpwi cr0 imm1 ppc32::tag-fixnum)
  (cmpwi cr1 imm1 ppc32::subtag-bignum)
  (srwi imm2 new-value ppc32::fixnumshift)
  (beq cr0 @store)
  (beq cr1 @bignum)
  @notu32
  (uuo_interr arch::error-object-not-unsigned-byte-32 new-value)
  @bignum
  (getvheader imm0 new-value)
  (cmpwi cr1 imm0 ppc32::one-digit-bignum-header)
  (cmpwi cr2 imm0 ppc32::two-digit-bignum-header)
  (lwz imm2 ppc32::misc-data-offset new-value)
  (cmpwi cr0 imm2 0)
  (beq cr1 @one)
  (bne cr2 @notu32)
  (lwz imm1 (+ 4 ppc32::misc-data-offset) new-value)
  (cmpwi cr1 imm1 0)
  (bgt cr0 @notu32)
  (beq cr1 @store)
  (b @notu32)
  @one
  (blt cr0 @notu32)
  @store
  (stwx imm2 imm0 fixnum)
  (mr arg_z new-value)
  (blr))

#+ppc64-target
(defppclapfunction %fixnum-set-u32 ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (cmpdi cr0 nargs '2)
  (check-nargs 2 3)
  (bne cr0 @3-args)
  (mr fixnum offset)
  (li offset 0)
  @3-args
  (unbox-fixnum imm0 offset)
  (extract-typecode imm1 new-value)
  (cmpdi cr0 imm1 ppc64::tag-fixnum)
  (cmpdi cr1 imm1 ppc64::subtag-bignum)
  (srdi imm2 new-value ppc64::fixnumshift)
  (beq cr0 @store)
  (beq cr1 @bignum)
  @notu64
  (uuo_interr arch::error-object-not-unsigned-byte-64 new-value)
  @bignum
  (ld imm2 ppc64::misc-data-offset new-value)
  (getvheader imm0 new-value)
  (cmpdi cr1 imm0 ppc64::two-digit-bignum-header)
  (rotldi imm2 imm2 32)
  (cmpdi cr2 imm0 ppc64::three-digit-bignum-header)
  (cmpdi cr0 imm2 0)
  (beq cr1 @two)
  (bne cr2 @notu64)
  (lwz imm1 (+ 8 ppc64::misc-data-offset) new-value)
  (cmpwi cr1 imm1 0)
  (bgt cr0 @notu64)
  (beq cr1 @store)
  (b @notu64)
  @two
  (blt cr0 @notu64)
  @store
  (stdx imm2 imm0 fixnum)
  (mr arg_z new-value)
  (blr))


(defppclapfunction %current-frame-ptr ()
  (check-nargs 0)
  (mr arg_z sp)
  (blr))

(defppclapfunction %current-vsp ()
  (check-nargs 0)
  (mr arg_z vsp)
  (blr))




(defppclapfunction %set-current-vsp ((new-vsp arg_z))
  (check-nargs 1)
  (mr vsp new-vsp)
  (blr))

(defppclapfunction %current-tsp ()
  (check-nargs 0)
  (mr arg_z tsp)
  (blr))



(defppclapfunction %set-current-tsp ((new-tsp arg_z))
  (check-nargs 1)
  (mr tsp new-tsp)
  (blr))

(defppclapfunction %%frame-backlink ((p arg_z))
  (check-nargs 1)
  (ldr arg_z target::lisp-frame.backlink arg_z)
  (blr))





(defppclapfunction %%frame-savefn ((p arg_z))
  (check-nargs 1)
  (ldr arg_z target::lisp-frame.savefn arg_z)
  (blr))

(defppclapfunction %cfp-lfun ((p arg_z))
  (ldr arg_y target::lisp-frame.savefn p)
  (extract-typecode imm0 arg_y)
  (cmpri imm0 target::subtag-function)
  (ldr loc-pc target::lisp-frame.savelr p)
  (bne @no)
  (ldr arg_x target::misc-data-offset arg_y)
  (sub imm1 loc-pc arg_x)
  (la imm1 (- target::misc-data-offset) imm1)
  (getvheader imm0 arg_x)
  (header-length imm0 imm0)
  (cmplr imm1 imm0)
  (box-fixnum imm1 imm1)
  (bge @no)
  (vpush arg_y)
  (vpush imm1)
  @go
  (set-nargs 2)
  (la temp0 '2 vsp)
  (ba .SPvalues)
  @no
  (li imm0 nil)
  (vpush imm0)
  (vpush imm0)
  (b @go))




(defppclapfunction %%frame-savevsp ((p arg_z))
  (check-nargs 1)
  (ldr arg_z target::lisp-frame.savevsp arg_z)
  (blr))





#+ppc32-target
(eval-when (:compile-toplevel :execute)
  (assert (eql ppc32::t-offset #x11)))

(defppclapfunction %uvector-data-fixnum ((uv arg_z))
  (check-nargs 1)
  (trap-unless-fulltag= arg_z target::fulltag-misc)
  (la arg_z target::misc-data-offset arg_z)
  (blr))

(defppclapfunction %catch-top ((tcr arg_z))
  (check-nargs 1)
  (ldr arg_z target::tcr.catch-top tcr)
  (cmpri cr0 arg_z 0)
  (bne @ret)
  (li arg_z nil)
 @ret
  (blr))

(defppclapfunction %catch-tsp ((catch arg_z))
  (check-nargs 1)
  (la arg_z (- (+ target::fulltag-misc
                                 (ash 1 (1+ target::word-shift)))) arg_z)
  (blr))



;;; Same as %address-of, but doesn't cons any bignums
;;; It also left shift fixnums just like everything else.
(defppclapfunction %fixnum-address-of ((x arg_z))
  (check-nargs 1)
  (box-fixnum arg_z x)
  (blr))



(defppclapfunction %save-standard-binding-list ((bindings arg_z))
  (ldr imm0 target::tcr.vs-area target::rcontext)
  (ldr imm1 target::area.high imm0)
  (push bindings imm1)
  (blr))

(defppclapfunction %saved-bindings-address ()
  (ldr imm0 target::tcr.vs-area target::rcontext)
  (ldr imm1 target::area.high imm0)
  (la arg_z (- target::node-size) imm1)
  (blr))

(defppclapfunction %code-vector-pc ((code-vector arg_y) (pcptr arg_z))
  (macptr-ptr imm0 pcptr)
  (ldr loc-pc 0 imm0)
  (sub imm0 loc-pc code-vector)
  (subi imm0 imm0 target::misc-data-offset)
  (getvheader imm1 code-vector)
  (header-size imm1 imm1)
  (slri imm1 imm1 2)
  (cmplr imm0 imm1)
  (li arg_z nil)
  (bgelr)
  (box-fixnum arg_z imm0)
  (blr))

;;; FF-call, in LAP.
#+eabi-target
(progn
(defppclapfunction %%ff-call ((fploads 8)
                              (single-offset 4)
                              (double-offset 0)
                              (framesize arg_x) ;always even, negative, includes frame overhead
                              (buf arg_y)
                              (entry arg_z))
  (check-nargs 6)
  (la imm0 12 vsp)
  (save-lisp-context imm0)
  (stwux sp sp framesize)
  (stw sp 4 sp)
  (macptr-ptr imm2 buf)
  (mr imm1 imm2)
  (la imm3 ppc32::eabi-c-frame.param0 sp)
  (li imm0 0)
  (lwz temp1 single-offset vsp)
  (lwz temp2 double-offset vsp)
  @copy
  (addi imm0 imm0 8)
  (cmpw imm0 temp1)
  (lfd fp0 0 imm2)
  (la imm2 8 imm2)
  (stfd fp0 0 imm3)
  (la imm3 8 imm3)
  (blt @copy)
  ;; We've copied the gpr-save area and the "other" arg words.
  ;; Sadly, we may still need to load up to 8 FPRs, and we have
  ;; to use some pretty ugly code to do so.
  (add temp1 temp1 imm1)
  (add temp2 temp2 imm1)
  (lwz temp0 fploads vsp)
  @load-fp1
  (lbz imm0 (+ ppc32::misc-data-offset 0) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp1-double)
  (lfs fp1 0 temp1)
  (la temp1 4 temp1)
  (b @load-fp2)
  @load-fp1-double
  (lfd fp1 0 temp2)
  (la temp2 8 temp2)
  @load-fp2
  (lbz imm0 (+ ppc32::misc-data-offset 1) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp2-double)
  (lfs fp2 0 temp1)
  (la temp1 4 temp1)
  (b @load-fp3)
  @load-fp2-double
  (lfd fp2 0 temp2)
  (la temp2 8 temp2)
  @load-fp3
  (lbz imm0 (+ ppc32::misc-data-offset 2) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp3-double)
  (lfs fp3 0 temp1)
  (la temp1 4 temp1)
  (b @load-fp4)
  @load-fp3-double
  (lfd fp3 0 temp2)
  (la temp2 8 temp2)
  @load-fp4
  (lbz imm0 (+ ppc32::misc-data-offset 3) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp4-double)
  (lfs fp4 0 temp1)
  (la temp1 4 temp1)
  (b @load-fp5)
  @load-fp4-double
  (lfd fp4 0 temp2)
  (la temp2 8 temp2)
  @load-fp5
  (lbz imm0 (+ ppc32::misc-data-offset 4) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp5-double)
  (lfs fp5 0 temp1)
  (la temp1 4 temp1)
  (b @load-fp6)
  @load-fp5-double
  (lfd fp5 0 temp2)
  (la temp2 8 temp2)
  @load-fp6
  (lbz imm0 (+ ppc32::misc-data-offset 5) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp6-double)
  (lfs fp6 0 temp1)
  (la temp1 4 temp1)
  (b @load-fp7)
  @load-fp6-double
  (lfd fp6 0 temp2)
  (la temp2 8 temp2)
  @load-fp7
  (lbz imm0 (+ ppc32::misc-data-offset 6) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp7-double)
  (lfs fp7 0 temp1)
  (la temp1 4 temp1)
  (b @load-fp8)
  @load-fp7-double
  (lfd fp7 0 temp2)
  (la temp2 8 temp2)
  @load-fp8
  (lbz imm0 (+ ppc32::misc-data-offset 0) temp0)
  (cmpwi imm0 1)
  (blt @loaded)
  (bne @load-fp8-double)
  (lfs fp8 0 temp1)
  (b @loaded)
  @load-fp8-double
  (lfd fp8 0 temp2)
  @loaded
  (vpush buf)
  (bla .SPeabi-ff-call)
  (vpop buf)
  (macptr-ptr imm2 buf)
  (stw imm0 0 imm2)
  (stw imm1 4 imm2)
  (stfs fp1 8 imm2)
  (stfd fp1 16 imm2)
  (restore-full-lisp-context)
  (li arg_z ppc32::nil-value)
  (blr))
  

(defun %ff-call (entry &rest specs-and-vals)
  (declare (dynamic-extent specs-and-vals))
  (let* ((len (length specs-and-vals))
         (other-offset 8)
         (single-float-offset 8)
         (double-float-offset 0)
         (nsingle-floats 0)
         (ndouble-floats 0)
         (nother-words 0)
         (nfpr-args 0)
         (ngpr-args 0))
    (declare (fixnum len  other-offset single-float-offset double-float-offset
                     nsingle-floats ndouble-floats nother-words nfpr-args ngpr-args))
    (unless (oddp len)
      (error "Length of ~s is even.  Missing result ?" specs-and-vals))

      (let* ((result-spec (or (car (last specs-and-vals)) :void))
             (nargs (ash (the fixnum (1- len)) -1))
             (fpr-reloads (make-array 8 :element-type '(unsigned-byte 8))))
        (declare (fixnum nargs) (dynamic-extent fpr-reloads))
        (do* ((i 0 (1+ i))
              (specs specs-and-vals (cddr specs))
              (spec (car specs) (car specs)))
             ((= i nargs))
          (declare (fixnum i))
          (ecase spec
            (:double-float (incf nfpr-args)
                           (if (<= nfpr-args 8)
                             (incf ndouble-floats)
                             (progn
                               (if (oddp nother-words)
                                 (incf nother-words))
                               (incf nother-words 2))))
            (:single-float (incf nfpr-args)
                           (if (<= nfpr-args 8)
                             (incf nsingle-floats)
                             (incf nother-words)))
	    ((:signed-doubleword :unsigned-doubleword)
	     (if (oddp ngpr-args)
	       (incf ngpr-args))
	     (incf ngpr-args 2)
	     (when (> ngpr-args 8)
	       (if (oddp nother-words)
		 (incf nother-words))
	       (incf nother-words 2)))
            ((:signed-byte :unsigned-byte :signed-halfword :unsigned-halfword
                           :signed-fullword :unsigned-fullword :address)
	     (incf ngpr-args)
             (if (> ngpr-args 8)
               (incf nother-words)))))
        (let* ((single-words (+ 8 nother-words nsingle-floats))
               (total-words (if (zerop ndouble-floats)
                              single-words
                              (+ (the fixnum (+ ndouble-floats ndouble-floats))
                                 (the fixnum (logand (lognot 1)
                                                     (the fixnum (1+ single-words))))))))
          (declare (fixnum total-words single-words))
          (%stack-block
           ((buf (ash total-words 2)))
           (setq single-float-offset (+ other-offset nother-words))
           (setq double-float-offset
            (logand (lognot 1)
                    (the fixnum (1+ (the fixnum (+ single-float-offset nsingle-floats))))))
           ;;; Make another pass through the arg/value pairs, evaluating each arg into
           ;;; the buffer.
           (do* ((i 0 (1+ i))
                 (specs specs-and-vals (cddr specs))
                 (spec (car specs) (car specs))
                 (val (cadr specs) (cadr specs))
                 (ngpr 0)
                 (nfpr 0)
                 (gpr-byte-offset 0)
                 (other-byte-offset (ash other-offset 2))
                 (single-byte-offset (ash single-float-offset 2))
                 (double-byte-offset (ash double-float-offset 2)))
                ((= i nargs))
             (declare (fixnum i gpr-byte-offset single-byte-offset double-byte-offset
                              ngpr nfpr))
             (case spec
               (:double-float
                (cond ((< nfpr 8)
                       (setf (uvref fpr-reloads nfpr) 2
                             (%get-double-float buf double-byte-offset) val
                             double-byte-offset (+ double-byte-offset 8)))
                      (t
                       (setq other-byte-offset (logand (lognot 7)
                                                       (the fixnum (+ other-byte-offset 4))))
                       (setf (%get-double-float buf other-byte-offset) val)
                       (setq other-byte-offset (+ other-byte-offset 8))))
                (incf nfpr))
               (:single-float
                (cond ((< nfpr 8)
                       (setf (uvref fpr-reloads nfpr) 1
                             (%get-single-float buf single-byte-offset) val
                             single-byte-offset (+ single-byte-offset 4)))
                             
                      (t
                       (setf (%get-single-float buf other-byte-offset) val
                             other-byte-offset (+ other-byte-offset 4))))
                (incf nfpr))
               (:address
                (cond ((< ngpr 8)
                       (setf (%get-ptr buf gpr-byte-offset) val
                             gpr-byte-offset (+ gpr-byte-offset 4)))
                      (t
                       (setf (%get-ptr buf other-byte-offset) val
                             other-byte-offset (+ other-byte-offset 4))))
                (incf ngpr))
	       ((:signed-doubleword :unsigned-doubleword)
		(when (oddp ngpr)
		  (incf ngpr)
		  (incf gpr-byte-offset 4))
		(cond ((< ngpr 8)
		       (if (eq spec :signed-doubleword)
			 (setf (%get-signed-long-long buf gpr-byte-offset) val)
			 (setf (%get-unsigned-long-long buf gpr-byte-offset) val))
		       (incf gpr-byte-offset 8))
		      (t
		       (when (logtest other-byte-offset 7)
			 (incf other-byte-offset 4))
		       (if (eq spec :signed-doubleword)
			 (setf (%get-signed-long-long buf other-byte-offset) val)
			 (setf (%get-unsigned-long-long buf other-byte-offset) val))
		       (incf other-byte-offset 8)))
		(incf ngpr 2))
               (t
                (cond ((< ngpr 8)
                       (setf (%get-long buf gpr-byte-offset) val
                             gpr-byte-offset (+ gpr-byte-offset 4)))
                      (t
                       (setf (%get-long buf other-byte-offset) val
                             other-byte-offset (+ other-byte-offset 4))))
                (incf ngpr))))
           (%%ff-call fpr-reloads
                      single-float-offset
                      double-float-offset
                      (the fixnum (-
                                   (ash (the fixnum
                                          (+ 6
                                             (the fixnum (logand
                                                          (lognot 1)
                                                          (the fixnum (1+ total-words))))))
                                        2)))
                      buf
                      entry)
          (ecase result-spec
            (:void nil)
            (:single-float (%get-single-float buf 8))
            (:double-float (%get-double-float buf 16))
            (:address (%get-ptr buf))
	    (:signed-doubleword (%get-signed-long-long buf 0))
	    (:unsigned-doubleword (%get-unsigned-long-long buf 0))
            (:signed-fullword (%get-signed-long buf))
            (:unsigned-fullword (%get-unsigned-long buf))
            (:signed-halfword (%get-signed-word buf 2))
            (:unsigned-halfword (%get-unsigned-word buf 2))
            (:signed-byte (%get-signed-byte buf 3))
            (:unsigned-byte (%get-unsigned-byte buf 3))))))))
)





;;; In the PowerOpen ABI, all arguments are passed in a contiguous
;;; block.  The first 13 (!) FP args are passed in FP regs; doubleword
;;; arguments are aligned on word boundaries.
#+poweropen-target
(progn
(defun %ff-call (entry &rest specs-and-vals)
  (declare (dynamic-extent specs-and-vals))
  (let* ((len (length specs-and-vals))
	 (total-words 0)
	 (monitor (eq (car specs-and-vals) :monitor-exception-ports)))
    (declare (fixnum len total-words))
    (when monitor
      (decf len)
      (setq specs-and-vals (cdr specs-and-vals)))
    (unless (oddp len)
      (error "Length of ~s is even.  Missing result ?" specs-and-vals))
      (let* ((result-spec (or (car (last specs-and-vals)) :void))
             (nargs (ash (the fixnum (1- len)) -1))
             (fpr-reload-sizes (make-array 13 :element-type '(unsigned-byte 8)))
	     (fpr-reload-offsets (make-array 13 :element-type '(unsigned-byte 16))))
        (declare (fixnum nargs) (dynamic-extent fpr-reload-sizes fpr-reload-offsets))
        (do* ((i 0 (1+ i))
              (specs specs-and-vals (cddr specs))
              (spec (car specs) (car specs)))
             ((= i nargs))
          (declare (fixnum i))
          (case spec
	    ((:double-float :signed-doubleword :unsigned-doubleword)
	     (incf total-words 2))
	    ((:single-float :signed-byte :unsigned-byte :signed-halfword
			    :unsigned-halfword :signed-fullword
			    :unsigned-fullword :address)
	     (incf total-words))
	    (t (if (typep spec 'unsigned-byte)
		 (incf total-words spec)
		 (error "Invalid argument spec ~s" spec)))))
	(%stack-block ((buf (ash (logand (lognot 1) (1+ (max 6  total-words))) 2)))
	  (do* ((i 0 (1+ i))
		(fpr 0)
		(offset 0 (+ offset 4))
		(specs specs-and-vals (cddr specs))
		(spec (car specs) (car specs))
		(val (cadr specs) (cadr specs)))
	       ((= i nargs))
	    (declare (fixnum i offset fpr))
	    (case spec
	      (:double-float
	       (when (< fpr 13)
		 (setf (uvref fpr-reload-sizes fpr) 2
		       (uvref fpr-reload-offsets fpr) offset))
	       (incf fpr)
	       (setf (%get-double-float buf offset) val)
	       (incf offset 4))
	      (:single-float
	       (when (< fpr 13)
		 (setf (uvref fpr-reload-sizes fpr) 1
		       (uvref fpr-reload-offsets fpr) offset))
	       (incf fpr)
	       (setf (%get-single-float buf offset) val))
	      (:signed-doubleword
	       (setf (%get-signed-long-long buf offset) val)
	       (incf offset 4))
	      (:unsigned-doubleword
	       (setf (%get-unsigned-long-long buf offset) val)
	       (incf offset 4))
	      (:address
	       (setf (%get-ptr buf offset) val))
	      (t
	       (if (typep spec 'unsigned-byte)
		 (dotimes (i spec (decf offset 4))
		   (setf (%get-ptr buf offset)
			 (%get-ptr val (* i 4)))
		   (incf offset 4))
		 (setf (%get-long buf offset) val)))))
	  (let* ((frame-size (if (<= total-words 8)
			       (ash
				   (+ ppc32::c-frame.size ppc32::lisp-frame.size)
				   -2)
			       (+
				   (ash
				    (+ ppc32::c-frame.size ppc32::lisp-frame.size)
				    -2)
				   (logand (lognot 1)
						(1+ (- total-words 8)))))))

	    (%%ff-call
	     monitor
	     fpr-reload-sizes
	     fpr-reload-offsets
	     (- (logandc2 (+ frame-size 3) 3))
	     total-words
	     buf
	     entry))
	  (ecase result-spec
            (:void nil)
            (:single-float (%get-single-float buf 8))
            (:double-float (%get-double-float buf 16))
            (:address (%get-ptr buf))
	    (:signed-doubleword (%get-signed-long-long buf 0))
	    (:unsigned-doubleword (%get-unsigned-long-long buf 0))
            (:signed-fullword (%get-signed-long buf))
            (:unsigned-fullword (%get-unsigned-long buf))
            (:signed-halfword (%get-signed-word buf 2))
            (:unsigned-halfword (%get-unsigned-word buf 2))
            (:signed-byte (%get-signed-byte buf 3))
            (:unsigned-byte (%get-unsigned-byte buf 3)))))))

(defppclapfunction %%ff-call ((monitor-exception-ports 12)
			      (reload-sizes 8)
			      (reload-offsets 4)
			      (frame-size 0)			     
			      (total-words arg_x)
			      (buf arg_y)
			      (entry arg_z))
  (check-nargs 7)
  (la imm0 16 vsp)
  (save-lisp-context imm0)
  (lwz imm0 frame-size vsp)
  (stwux sp sp imm0)
  (stw sp ppc32::c-frame.savelr sp)
  (lwz imm2 monitor-exception-ports vsp)
  (cmpwi cr1 imm2 nil)
  (macptr-ptr imm2 buf)
  (mr imm1 imm2)
  (la imm3 ppc32::c-frame.param0 sp)
  (li temp1 0)
  @copy
  (addi temp1 temp1 '1)
  (cmpw temp1 total-words)
  (lwz imm0 0 imm2)
  (la imm2 4 imm2)
  (stw imm0 0 imm3)
  (la imm3 4 imm3)
  (blt @copy)
  (lwz temp0 reload-sizes vsp)
  (lwz temp1 reload-offsets vsp)
  @load-fp1
  (lbz imm0 (+ ppc32::misc-data-offset 0) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 0) temp1)
  (blt @loaded)
  (bne @load-fp1-double)
  (lfsx fp1 imm1 imm2)
  (b @load-fp2)
  @load-fp1-double
  (lfdx fp1 imm1 imm2)

  @load-fp2
  (lbz imm0 (+ ppc32::misc-data-offset 1) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 2) temp1)
  (blt @loaded)
  (bne @load-fp2-double)
  (lfsx fp2 imm1 imm2)
  (b @load-fp3)
  @load-fp2-double
  (lfdx fp2 imm1 imm2)

  @load-fp3
  (lbz imm0 (+ ppc32::misc-data-offset 2) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 4) temp1)
  (blt @loaded)
  (bne @load-fp3-double)
  (lfsx fp3 imm1 imm2)
  (b @load-fp4)
  @load-fp3-double
  (lfdx fp3 imm1 imm2)

  @load-fp4
  (lbz imm0 (+ ppc32::misc-data-offset 3) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 6) temp1)
  (blt @loaded)
  (bne @load-fp4-double)
  (lfsx fp4 imm1 imm2)
  (b @load-fp5)
  @load-fp4-double
  (lfdx fp4 imm1 imm2)

  @load-fp5
  (lbz imm0 (+ ppc32::misc-data-offset 4) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 8) temp1)
  (blt @loaded)
  (bne @load-fp5-double)
  (lfsx fp5 imm1 imm2)
  (b @load-fp6)
  @load-fp5-double
  (lfdx fp5 imm1 imm2)

   @load-fp6
  (lbz imm0 (+ ppc32::misc-data-offset 5) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 10) temp1)
  (blt @loaded)
  (bne @load-fp1-double)
  (lfsx fp6 imm1 imm2)
  (b @load-fp7)
  @load-fp6-double
  (lfdx fp6 imm1 imm2)

   @load-fp7
  (lbz imm0 (+ ppc32::misc-data-offset 6) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 12) temp1)
  (blt @loaded)
  (bne @load-fp1-double)
  (lfsx fp7 imm1 imm2)
  (b @load-fp8)
  @load-fp7-double
  (lfdx fp7 imm1 imm2)

  @load-fp8
  (lbz imm0 (+ ppc32::misc-data-offset 7) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 14) temp1)
  (blt @loaded)
  (bne @load-fp8-double)
  (lfsx fp8 imm1 imm2)
  (b @load-fp9)
  @load-fp8-double
  (lfdx fp8 imm1 imm2)

  @load-fp9
  (lbz imm0 (+ ppc32::misc-data-offset 8) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 16) temp1)
  (blt @loaded)
  (bne @load-fp9-double)
  (lfsx fp9 imm1 imm2)
  (b @load-fp10)
  @load-fp9-double
  (lfdx fp9 imm1 imm2)

  @load-fp10
  (lbz imm0 (+ ppc32::misc-data-offset 9) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 18) temp1)
  (blt @loaded)
  (bne @load-fp10-double)
  (lfsx fp10 imm1 imm2)
  (b @load-fp11)
  @load-fp10-double
  (lfdx fp10 imm1 imm2)

  @load-fp11
  (lbz imm0 (+ ppc32::misc-data-offset 10) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 20) temp1)
  (blt @loaded)
  (bne @load-fp11-double)
  (lfsx fp11 imm1 imm2)
  (b @load-fp12)
  @load-fp11-double
  (lfdx fp11 imm1 imm2)

  @load-fp12
  (lbz imm0 (+ ppc32::misc-data-offset 11) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 22) temp1)
  (blt @loaded)
  (bne @load-fp12-double)
  (lfsx fp12 imm1 imm2)
  (b @load-fp13)
  @load-fp12-double
  (lfdx fp12 imm1 imm2)

  @load-fp13
  (lbz imm0 (+ ppc32::misc-data-offset 12) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ ppc32::misc-data-offset 24) temp1)
  (blt @loaded)
  (bne @load-fp13-double)
  (lfsx fp13 imm1 imm2)
  (b @loaded)
  @load-fp13-double
  (lfdx fp13 imm1 imm2)
  @loaded
  (vpush buf)
  (bne cr1 @callX)
  (bla .SPffcall)
  (b @called)
  @callX
  (bla .SPffcallX)
  @called
  (vpop buf)
  (macptr-ptr imm2 buf)
  (stw imm0 0 imm2)
  (stw imm1 4 imm2)
  (stfs fp1 8 imm2)
  (stfd fp1 16 imm2)
  (restore-full-lisp-context)
  (li arg_z ppc32::nil-value)
  (blr))

  

  )




(defppclapfunction %get-object ((macptr arg_y) (offset arg_z))
  (check-nargs 2)
  (trap-unless-typecode= arg_y target::subtag-macptr)
  (macptr-ptr imm0 arg_y)
  (trap-unless-lisptag= arg_z target::tag-fixnum imm1)
  (unbox-fixnum imm1 arg_z)
  (ldrx arg_z imm0 imm1)
  (blr))


(defppclapfunction %set-object ((macptr arg_x) (offset arg_y) (value arg_z))
  (check-nargs 3)
  (trap-unless-typecode= arg_x target::subtag-macptr)
  (macptr-ptr imm0 arg_x)
  (trap-unless-lisptag= arg_y target::tag-fixnum imm1)
  (unbox-fixnum imm1 arg_y)
  (strx arg_z imm0 imm1)
  (blr))


(defppclapfunction %apply-lexpr-with-method-context ((magic arg_x)
                                                     (function arg_y)
                                                     (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in ppc::next-method-context (= ppc::temp1).
  ;; Put function in ppc::nfn (= ppc::temp2).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves ppc::nfn/ppc::next-method-context.
  ;; Jump to the function in ppc::nfn.
  (mr ppc::next-method-context magic)
  (mr ppc::nfn function)
  (set-nargs 0)
  (mflr loc-pc)
  (bla .SPspread-lexpr-z)
  (mtlr loc-pc)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))


(defppclapfunction %apply-with-method-context ((magic arg_x)
                                               (function arg_y)
                                               (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in ppc::next-method-context (= ppc::temp1).
  ;; Put function in ppc::nfn (= ppc::temp2).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves ppc::nfn/ppc::next-method-context.
  ;; Jump to the function in ppc::nfn.
  (mr ppc::next-method-context magic)
  (mr ppc::nfn function)
  (set-nargs 0)
  (mflr loc-pc)
  (bla .SPspreadargZ)
  (mtlr loc-pc)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))


;;; is a winner - saves ~15%
(defppclapfunction gag-one-arg ((arg arg_z))
  (check-nargs 1)  
  (svref arg_y gf.dispatch-table nfn) ; mention dt first
  (set-nargs 2)
  (svref nfn gf.dcode nfn)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))


(defppclapfunction gag-two-arg ((arg0 arg_y) (arg1 arg_z))
  (check-nargs 2)  
  (svref arg_x gf.dispatch-table nfn) ; mention dt first
  (set-nargs 3)
  (svref nfn gf.dcode nfn)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))

(defparameter *cm-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (ppc-lap-function 
      gag 
      ()
      (mflr loc-pc)
      (vpush-argregs)
      (vpush nargs)
      (add imm0 vsp nargs)
      (la imm0 target::node-size imm0)                  ; caller's vsp
      (bla .SPlexpr-entry)
      (mtlr loc-pc)                     ; return to kernel
      (mr arg_z vsp)                    ; lexpr
      (svref arg_y combined-method.thing nfn) ; thing
      (set-nargs 2)
      (svref nfn combined-method.dcode nfn) ; dcode function
      (ldr temp0 target::misc-data-offset nfn)
      (mtctr temp0)
      (bctr)))))


(defppclapfunction %apply-lexpr-tail-wise ((method arg_y) (args arg_z))
  ;; This assumes
  ;; a) that "args" is a lexpr made via the .SPlexpr-entry mechanism
  ;; b) That the LR on entry to this function points to the lexpr-cleanup
  ;;    code that .SPlexpr-entry set up
  ;; c) That there weren't any required args to the lexpr, e.g. that
  ;;    (%lexpr-ref args (%lexpr-count args) 0) was the first arg to the gf.
  ;; The lexpr-cleanup code will be EQ to either (lisp-global ret1valaddr)
  ;; or (lisp-global lexpr-return1v).  In the former case, discard a frame
  ;; from the cstack (multiple-value tossing).  Restore FN and LR from
  ;; the first frame that .SPlexpr-entry pushed, restore vsp from (+
  ;; args node-size), pop the argregs, and jump to the function.
  (mflr loc-pc)
  (ref-global imm0 ret1valaddr)
  (cmpr cr2 loc-pc imm0)
  (ldr nargs 0 args)
  (mr imm5 nargs)
  (cmpri cr0 nargs 0)
  (cmpri cr1 nargs '2)
  (mr nfn arg_y)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (if (:cr2 :eq)
    (la sp target::lisp-frame.size sp))
  (ldr loc-pc target::lisp-frame.savelr sp)
  (ldr fn target::lisp-frame.savefn sp)
  (ldr imm0 target::lisp-frame.savevsp sp)
  (sub vsp imm0 nargs)
  (mtlr loc-pc)
  (la sp target::lisp-frame.size sp)
  (beqctr)
  (vpop arg_z)
  (bltctr cr1)
  (vpop arg_y)
  (beqctr cr1)
  (vpop arg_x)
  (bctr))


;;; end of ppc-def.lisp
