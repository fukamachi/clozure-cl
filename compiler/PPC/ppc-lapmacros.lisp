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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "PPC-LAP"))

(defppclapmacro dbg (&optional save-lr?)
  (if save-lr?
    `(progn
       (mflr loc-pc)
       (stw imm0 -40 sp) ; better than clobbering imm0
       (bla .SPbreakpoint)
       (lwz imm0 -40 sp)
       (mtlr loc-pc))
    `(bla .SPbreakpoint)))

(defppclapmacro lwi (dest n)
  (setq n (logand n #xffffffff))
  (let* ((mask #xffff8000)
         (masked (logand n mask))
         (high (ash n -16))
         (low (logand #xffff n)))
    (if (or (= 0 masked) (= mask masked))
      `(li ,dest ,low)
      (if (= low 0)
        `(lis ,dest ,high)
        `(progn
           (lis ,dest ,high)
           (ori ,dest ,dest ,low))))))

(defppclapmacro set-nargs (n)
  (check-type n (unsigned-byte 13))
  `(li nargs ',n))

(defppclapmacro check-nargs (min &optional (max min))
  (if (eq max min)
    `(twnei nargs ',min)
    (if (null max)
      (unless (= min 0)
        `(twllti nargs ',min))
      (if (= min 0)
        `(twlgti nargs ',max)
        `(progn
           (twllti nargs ',min)
           (twlgti nargs ',max))))))

;; Event-polling involves checking to see if the value of the current
;; thread's interrupt-level is > 0.  For now, use nargs; this may
;; change to "any register BUT nargs".  (Note that most number-of-args
;; traps use unsigned comparisons.)
(defppclapmacro event-poll ()
  '(progn
     (lwz nargs ppc32::tcr.interrupt-level rcontext)
     (twgti nargs 0)))

;; The general idea is that storing zero N bytes beyond the end of
;; a stack pointer will generate a write-protection violation if
;; a stack overflow is pending.  "N" should be somewhere around
;; the page size.  If a function uses more than N bytes of stack,
;; it should poll every time it's pushed around N bytes ...

(defppclapmacro stack-probe (stackpointer &optional (n -4096))
  `(stw rzero ,n ,stackpointer))

(defppclapmacro vstack-probe (&optional (n -4096))
  `(stack-probe vsp ,n))

(defppclapmacro tstack-probe (&optional (n -4096))
  `(stack-probe tsp ,n))



; There's no "else"; learn to say "(progn ...)".
; Note also that the condition is a CR bit specification (or a "negated" one).
; Whatever affected that bit (hopefully) happened earlier in the pipeline.
(defppclapmacro if (test then &optional (else nil else-p))
  "If Predicate Then [Else]
  If Predicate evaluates to non-null, evaluate Then and returns its values,
  otherwise evaluate Else and return its values. Else defaults to NIL."
  (multiple-value-bind (bitform negated) (ppc-lap-parse-test test)
    (let* ((false-label (gensym)))
      (if (not else-p)
      `(progn
         (,(if negated 'bt 'bf) ,bitform ,false-label)
         ,then
         ,false-label)
      (let* ((cont-label (gensym)))
        `(progn
          (,(if negated 'bt 'bf) ,bitform ,false-label)
          ,then
          (b ,cont-label)
          ,false-label
          ,else
          ,cont-label))))))

(defppclapmacro save-pc ()
  `(mflr loc-pc))

; This needs to be done if we aren't a leaf function (e.g., if we clobber our
; return address or need to reference any constants.  Note that it's not
; atomic wrt a preemptive scheduler, but we need to pretend that it will be.)
; The VSP to be saved is the value of the VSP before any of this function's
; arguments were vpushed by its caller; that's not the same as the VSP register
; if any non-register arguments were received, but is usually easy to compute.

(defppclapmacro save-lisp-context (&optional (vsp 'vsp) (save-pc t))
  `(progn
    ,@(if save-pc 
	  '((save-pc)))
    (stwu sp (- ppc32::lisp-frame.size) sp)
    (stw fn ppc32::lisp-frame.savefn sp)
    (stw loc-pc ppc32::lisp-frame.savelr sp)
    (stw ,vsp ppc32::lisp-frame.savevsp sp)
    (mr fn nfn)))

;;; There are a few cases to deal with when restoring: whether or not
;;; to restore the vsp, whether we need to saved LR back in the LR or
;;; whether it only needs to get as far as loc-pc, etc.  This fully
;;; restores everything (letting the caller specify some register
;;; other than the VSP, if that's useful.)  Note that, since FN gets
;;; restored, it's no longer possible to use it to address the current
;;; function's constants.
(defppclapmacro restore-full-lisp-context (&optional (vsp 'vsp))
  `(progn
    (lwz loc-pc ppc32::lisp-frame.savelr sp)
    (lwz ,vsp ppc32::lisp-frame.savevsp sp)
    (mr new-vsp ,vsp)
    (mtlr loc-pc)
    (lwz fn ppc32::lisp-frame.savefn sp)
    (la sp ppc32::lisp-frame.size sp)))

(defppclapmacro restore-pc ()
  `(mtlr loc-pc))

(defppclapmacro push (src stack)
  `(stwu ,src -4 ,stack))

(defppclapmacro vpush (src)
  `(progn
    (push ,src vsp)
    (mr new-vsp vsp)))

; You typically don't want to do this to pop a single register (it's better to
; do a sequence of loads, and then adjust the stack pointer.)

(defppclapmacro pop (dest stack)
  `(progn
     (lwz ,dest 0 ,stack)
     (la ,stack 4 ,stack)))

(defppclapmacro vpop (dest)
  `(progn
    (pop ,dest vsp)
    (mr new-vsp vsp)))

(defppclapmacro %cdr (dest node)
  `(lwz ,dest ppc32::cons.cdr ,node))

(defppclapmacro %car (dest node)
  `(lwz ,dest ppc32::cons.car ,node))

(defppclapmacro extract-lisptag (dest node)
  `(clrlwi ,dest ,node (- 32 ppc32::nlisptagbits)))

(defppclapmacro extract-fulltag (dest node)
  `(clrlwi ,dest ,node (- 32 ppc32::ntagbits)))

(defppclapmacro extract-subtag (dest node)
  `(lbz ,dest ppc32::misc-subtag-offset ,node))

(defppclapmacro extract-typecode (dest node &optional (crf :cr0))
  `(progn
     (extract-lisptag ,dest ,node)
     (cmpwi ,crf ,dest ppc32::tag-misc)
     (if (,crf :eq)
       (extract-subtag ,dest ,node))))

(defppclapmacro trap-unless-lisptag= (node tag &optional (immreg ppc::imm0))
  `(progn
     (extract-lisptag ,immreg ,node)
     (twnei ,immreg ,tag)))

(defppclapmacro trap-unless-fulltag= (node tag &optional (immreg ppc::imm0))
  `(progn
     (extract-fulltag ,immreg ,node)
     (twnei ,immreg ,tag)))


(defppclapmacro trap-unless-typecode= (node tag &optional (immreg ppc::imm0) (crf :cr0))
  `(progn
     (extract-typecode ,immreg ,node ,crf)
     (twnei ,immreg ,tag)))


(defppclapmacro load-constant (dest constant)
  `(lwz ,dest ',constant fn))

;; This is about as hard on the pipeline as anything I can think of.
(defppclapmacro call-symbol (function-name)
  `(progn
     (load-constant fname ,function-name)
     (lwz nfn ppc32::symbol.fcell fname)
     (lwz loc-pc ppc32::misc-data-offset nfn)
     (mtctr loc-pc)
     (bctrl)))

(defppclapmacro sp-call-symbol (function-name)
  `(progn
     (load-constant fname ,function-name)
     (bla .SPjmpsym)))

(defppclapmacro getvheader (dest src)
  `(lwz ,dest ppc32::misc-header-offset ,src))

;; "Size" is unboxed element-count.
(defppclapmacro header-size (dest vheader)
  `(srwi ,dest ,vheader ppc32::num-subtag-bits))

;; "Length" is fixnum element-count.
(defppclapmacro header-length (dest vheader)
  `(rlwinm ,dest 
           ,vheader 
           (- ppc32::nbits-in-word (- ppc32::num-subtag-bits ppc32::nfixnumtagbits))
           (- ppc32::num-subtag-bits ppc32::nfixnumtagbits)
           (- ppc32::least-significant-bit ppc32::nfixnumtagbits)))

(defppclapmacro header-subtag[fixnum] (dest vheader)
  `(rlwinm ,dest
           ,vheader
           ppc32::fixnumshift
           (- ppc32::nbits-in-word (+ ppc32::num-subtag-bits ppc32::nfixnumtagbits))
           (- ppc32::least-significant-bit ppc32::nfixnumtagbits)))


(defppclapmacro vector-size (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-size ,dest ,vheader)))

(defppclapmacro vector-length (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-length ,dest ,vheader)))


;; Reference a 32-bit miscobj entry at a variable index.
;; Make the caller explicitly designate a scratch register
;; to use for the scaled index.

(defppclapmacro vref32 (dest miscobj index scaled-idx)
  `(progn
     (la ,scaled-idx ppc32::misc-data-offset ,index)
     (lwzx ,dest ,miscobj ,scaled-idx)))

;; The simple (no-memoization) case.
(defppclapmacro vset32 (src miscobj index scaled-idx)
  `(progn
     (la ,scaled-idx ppc32::misc-data-offset ,index)
     (stwx ,src ,miscobj ,scaled-idx)))

(defppclapmacro extract-lowbyte (dest src)
  `(clrlwi ,dest ,src (- 32 8)))

(defppclapmacro unbox-fixnum (dest src)
  `(srawi ,dest ,src ppc32::fixnumshift))

(defppclapmacro box-fixnum (dest src)
  `(slwi ,dest ,src ppc32::fixnumshift))



; If crf is specified, type checks src
(defppclapmacro unbox-base-char (dest src &optional crf)
  (if (null crf)
    `(srwi ,dest ,src ppc32::charcode-shift)
    (let ((label (gensym)))
      `(progn
         (rlwinm ,dest ,src 8 16 31)
         (cmpwi ,crf ,dest (ash ppc32::subtag-character 8))
         (srwi ,dest ,src ppc32::charcode-shift)
         (beq+ ,crf ,label)
         (uuo_interr arch::error-object-not-base-char ,src)
         ,label))))

; If crf is specified, type checks src
(defppclapmacro unbox-character (dest src &optional crf)
  (if (null crf)
    `(srwi ,dest ,src ppc32::charcode-shift)
    (let ((label (gensym)))
      `(progn
         (clrlwi ,dest ,src 24)
         (cmpwi ,crf ,dest ppc32::subtag-character)
         (srwi ,dest ,src ppc32::charcode-shift)
         (beq+ ,crf ,label)
         (uuo_interr arch::error-object-not-character ,src)
         ,label))))

(defppclapmacro box-character (dest src)
  `(progn
     (li ,dest ppc32::subtag-character)
     (rlwimi ,dest ,src 16 0 15)))

(defppclapmacro ref-global (reg sym)
  (let* ((offset (ppc32::%kernel-global sym)))
    `(lwz ,reg (+ ,offset ppc32::nil-value) 0)))

(defppclapmacro set-global (reg sym)
  (let* ((offset (ppc32::%kernel-global sym)))
    `(stw ,reg (+ ,offset ppc32::nil-value) 0)))

; Set "dest" to those bits in "src" that are other than those
; that would be set if "src" is a fixnum and of type (unsigned-byte "width").
; If no bits are set in "dest", then "src" is indeed of type (unsigned-byte "width").
(defppclapmacro extract-unsigned-byte-bits (dest src width)
  `(rlwinm ,dest ,src 0 (- 32 ppc32::fixnumshift) (- 31 (+ ,width ppc32::fixnumshift))))

; As above, but set (:CR0 :EQ) according to the result.
(defppclapmacro extract-unsigned-byte-bits. (dest src width)
  `(rlwinm. ,dest ,src 0 (- 32 ppc32::fixnumshift) (- 31 (+ ,width ppc32::fixnumshift))))


;;; from/blame slh:

; setpred depends on this
(eval-when (:compile-toplevel :execute :load-toplevel)
  (assert (= ppc32::t-offset #x11)))

(defppclapmacro setpred (dest crf cc-bit &optional (temp 'imm0))
  (let ((shift (+ (* (position crf '(:cr0 :cr1 :cr2 :cr3 :cr4 :cr5 :cr6 :cr7)) 4)
                  (position cc-bit '(:lt :gt :eq :so))
                  1)))
    `(progn
       (mfcr ,temp)
       (rlwinm ,temp ,temp ,shift 31 31)    ; get  1 bit
       (rlwimi ,temp ,temp      4 27 27)    ; get 16 bit
       (addi ,dest ,temp ppc32::nil-value))))

; You generally don't want to have to say "mfcr": it crosses functional
; units and forces synchronization (all preceding insns must complete,
; no subsequent insns may start.)
; There are often algebraic ways of computing ppc32::t-offset:

(defppclapmacro eq0->boolean (dest src temp)
  `(progn
     (cntlzw ,temp ,src)                ; 32 leading zeros if (= rx ry)
     (srwi ,temp ,temp 5)               ; temp = (rx == ry), C-wise
     (rlwimi ,temp ,temp 4 27 27)       ; temp = ppc32::t-offset or 0
     (addi ,dest ,temp ppc32::nil-value))) ; dest = (eq rx ry), lisp-wise

(defppclapmacro eq->boolean (dest rx ry temp)
  `(progn
     (sub ,temp ,rx ,ry)
     (eq0->boolean ,dest ,temp ,temp)))

;;; Src has high 31 bits clear.  Map low bit to T/NIL.
(defppclapmacro bit0->boolean (dest src temp)
  `(progn
    (rlwimi ,temp ,src 4 27 27)
    (addi ,dest ,temp ppc32::nil-value)))

(defppclapmacro repeat (n inst)
  (let* ((insts ()))
    (dotimes (i n `(progn ,@(nreverse insts)))
      (push inst insts))))

(defppclapmacro get-single-float (dest node)
  `(lfs ,dest ppc32::single-float.value ,node))

(defppclapmacro get-double-float (dest node)
  `(lfd ,dest ppc32::double-float.value ,node))

(defppclapmacro put-single-float (src node)
  `(stfs ,src ppc32::single-float.value ,node))

(defppclapmacro put-double-float (src node)
  `(stfd ,src ppc32::double-float.value ,node))

(defppclapmacro clear-fpu-exceptions ()
  `(mtfsf #xfc #.ppc::fp-zero))

(defppclapmacro get-boxed-sign (dest src crf)
  `(progn
     (load-constant ,dest 1)               ; assume positive
     (mtcrf ,crf ,src)
     (if (,crf :lt)
       (load-constant ,dest -1))))

; from ppc-bignum.lisp
(defppclapmacro digit-h (dest src)
  `(rlwinm ,dest ,src (+ 16 ppc32::fixnumshift) (- 16 ppc32::fixnumshift) (- 31 ppc32::fixnumshift)))

; from ppc-bignum.lisp
(defppclapmacro digit-l (dest src)
  `(clrlslwi ,dest ,src 16 ppc32::fixnumshift))
  
; from ppc-bignum.lisp
(defppclapmacro compose-digit (dest high low)
  `(progn
     (rlwinm ,dest ,low (- ppc32::nbits-in-word ppc32::fixnumshift) 16 31)
     (rlwimi ,dest ,high (- 16 ppc32::fixnumshift) 0 15)))

#+ppc32-target
(defppclapmacro macptr-ptr (dest macptr)
  `(lwz ,dest ppc32::macptr.address ,macptr))

(defppclapmacro svref (dest index vector)
  `(lwz ,dest (+ (* 4 ,index) ppc32::misc-data-offset) ,vector))

; This evals its args in the wrong order.
; Can't imagine any code will care.
(defppclapmacro svset (new-value index vector)
  `(stw ,new-value (+ (* 4 ,index) ppc32::misc-data-offset) ,vector))

(defppclapmacro vpush-argregs ()
  (let* ((none (gensym))
         (two (gensym))
         (one (gensym)))
  `(progn
     (cmpwi cr1 nargs '2)
     (cmpwi cr0 nargs 0)
     (beq cr1 ,two)
     (beq cr0 ,none)
     (blt cr1 ,one)
     (vpush arg_x)
     ,two
     (vpush arg_y)
     ,one
     (vpush arg_z)
     ,none)))


; Set FP-reg to 0.0 . Using (fsub fp-reg fp-reg fpreg)
; doesn't work if fp-reg contains a NaN.

(defppclapmacro zero-fp-reg (fp-reg)
  (let* ((offset (ppc32::kernel-global short-float-zero)))
    `(lfs ,fp-reg (+ ,offset ppc32::nil-value) 0)))




; Functions to access exception frames






;;; Saving and restoring AltiVec registers.

;;; Note that under the EABI (to which PPCLinux conforms), the OS
;;; doesn't attach any special significance to the value of the VRSAVE
;;; register (spr 256).  Under some other ABIs, VRSAVE is a bitmask
;;; which indicates which vector registers are live at context switch
;;; time.  These macros contain code to maintain VRSAVE when the
;;; variable *ALTIVEC-LAPMACROS-MAINTAIN-VRSAVE-P* is true at
;;; macroexpand time; that variable is initialized to true if and only
;;; if :EABI-TARGET is not on *FEATURES*.  Making this behavior
;;; optional is supposed to help make code which uses these macros
;;; easier to port to other platforms.

;;; From what I can tell, a function that takes incoming arguments in
;;; vector registers (vr2 ... vr13) (and doesn't use any other vector
;;; registers) doesn't need to assert that it uses any vector
;;; registers (even on platforms that maintain VRSAVE.)  A function
;;; that uses vector registers that were not incoming arguments has to
;;; assert that it uses those registers on platforms that maintain
;;; VRSAVE.  On all platforms, a function that uses any non-volatile
;;; vector registers (vr20 ... vr31) has to assert that it uses these
;;; registers and save and restore the caller's value of these registers
;;; around that usage.

(defparameter *altivec-lapmacros-maintain-vrsave-p*
  #-eabi-target t
  #+eabi-target nil)

(defun %vr-register-mask (reglist)
  (let* ((mask 0))
    (dolist (reg reglist mask)
      (let* ((regval (ppc-vector-register-name-or-expression reg)))
        (unless (typep regval '(mod 32))
          (error "Bad AltiVec register - ~s" reg))
        (setq mask (logior mask (ash #x80000000 (- regval))))))))



;;; Build a frame on the temp stack large enough to hold N 128-bit vector
;;; registers and the saved value of the VRSAVE spr.  That frame will look
;;; like:
;;; #x??????I0   backpointer to previous tstack frame
;;; #x??????I4   non-zero marker: frame doesn't contain tagged lisp data
;;; #x??????I8   saved VRSAVE
;;; #x??????IC   pad word for alignment
;;; #x??????J0   first saved vector register
;;; #x??????K0   second saved vector register
;;;   ...
;;; #x??????X0   last saved vector register
;;; #x??????Y0   (possibly) 8 bytes wasted for alignment.
;;; #x????????   UNKNOWN; not necessarily the previous tstack frame
;;;
;;;  Use the specified immediate register to build the frame.
;;;  Save the caller's VRSAVE in the frame.

(defppclapmacro %build-vrsave-frame (n tempreg)
  (if (or (> n 0) *altivec-lapmacros-maintain-vrsave-p*)
    (if (zerop n)
      ;; Just make room for vrsave; no need to align to 16-byte boundary.
      `(progn
	(stwu tsp -16 tsp)
	(stw tsp 4 tsp))
      `(progn
	(la ,tempreg ,(- (ash (1+ n) 4)) ppc32::tsp)
	(clrrwi ,tempreg ,tempreg 4)	; align to 16-byte boundary
	(sub ,tempreg ,tempreg ppc32::tsp) ; calculate (aligned) frame size.
	(stwux ppc32::tsp ppc32::tsp ,tempreg)
	(stw ppc32::tsp 4 ppc32::tsp)))	; non-zero: non-lisp
    `(progn)))

;;; Save the current value of the VRSAVE spr in the newly-created
;;; tstack frame.

(defppclapmacro %save-vrsave (tempreg)
  (if *altivec-lapmacros-maintain-vrsave-p*
    `(progn
      (mfspr ,tempreg 256)		; SPR 256 = vrsave
      (stw ,tempreg 8 tsp))
    `(progn)))



;;; When this is expanded, "tempreg" should contain the caller's vrsave.
(defppclapmacro %update-vrsave (tempreg mask)
  (let* ((mask-high (ldb (byte 16 16) mask))
         (mask-low (ldb (byte 16 0) mask)))
    `(progn
       ,@(unless (zerop mask-high) `((oris ,tempreg ,tempreg ,mask-high)))
       ,@(unless (zerop mask-low) `((ori ,tempreg ,tempreg ,mask-low)))
       (mtspr 256 ,tempreg))))

;;; Save each of the vector regs in "nvrs" into the current tstack 
;;; frame, starting at offset 16
(defppclapmacro %save-vector-regs (nvrs tempreg)
  (let* ((insts ()))
    (do* ((offset 16 (+ 16 offset))
          (regs nvrs (cdr regs)))
         ((null regs) `(progn ,@(nreverse insts)))
      (declare (fixnum offset))
      (push `(la ,tempreg ,offset ppc32::tsp) insts)
      (push `(stvx ,(car regs) ppc32::rzero ,tempreg) insts))))


;;; Pretty much the same idea, only we restore VRSAVE first and
;;; discard the tstack frame after we've reloaded the vector regs.
(defppclapmacro %restore-vector-regs (nvrs tempreg)
  (let* ((loads ()))
    (do* ((offset 16 (+ 16 offset))
          (regs nvrs (cdr regs)))
         ((null regs) `(progn
			,@ (when *altivec-lapmacros-maintain-vrsave-p*
			     `((progn
				 (lwz ,tempreg 8 ppc32::tsp)
				 (mtspr 256 ,tempreg))))
			,@(nreverse loads)
			(lwz ppc32::tsp 0 ppc32::tsp)))
      (declare (fixnum offset))
      (push `(la ,tempreg ,offset ppc32::tsp) loads)
      (push `(lvx ,(car regs) ppc32::rzero ,tempreg) loads))))


(defun %extract-non-volatile-vector-registers (vector-reg-list)
  (let* ((nvrs ()))
    (dolist (reg vector-reg-list (nreverse nvrs))
      (let* ((regval (ppc-vector-register-name-or-expression reg)))
        (unless (typep regval '(mod 32))
          (error "Bad AltiVec register - ~s" reg))
        (when (>= regval 20)
          (pushnew regval nvrs))))))


;;; One could imagine something more elaborate:
;;; 1) Binding a global bitmask that represents the assembly-time notion
;;;    of VRSAVE's contents; #'ppc-vector-register-name-or-expression
;;;    could then warn if a vector register wasn't marked as active.
;;;    Maybe a good idea, but PPC-LAP would have to bind that special
;;;    variable to 0 to make things reentrant.
;;; 2) Binding a user-specified variable to the list of NVRs that need
;;;    to be restored, so that it'd be more convenient to insert one's
;;;    own calls to %RESTORE-VECTOR-REGS at appropriate points.
;;; Ad infinitum.  As is, this allows one to execute a "flat" body of code
;;;   that's bracketed by the stuff needed to keep VRSAVE in sync and
;;;   to save and restore any non-volatile vector registers specified.
;;;   That body of code is "flat" in the sense that it doesn't return,
;;;   tail-call, establish a catch or unwind-protect frame, etc.
;;;   It -can- contain lisp or foreign function calls.

(defppclapmacro %with-altivec-registers ((&key (immreg 'ppc::imm0)) reglist &body body)
  (let* ((mask (%vr-register-mask reglist))
         (nvrs (%extract-non-volatile-vector-registers reglist))
         (num-nvrs (length nvrs)))
    (if (or *altivec-lapmacros-maintain-vrsave-p* nvrs)
      `(progn
	(%build-vrsave-frame ,num-nvrs ,immreg)
	(%save-vrsave ,immreg)
	,@ (if *altivec-lapmacros-maintain-vrsave-p*
	     `((%update-vrsave ,immreg ,mask)))
	(%save-vector-regs ,nvrs ,immreg)
	(progn ,@body)
	(%restore-vector-regs ,nvrs ,immreg))
      `(progn ,@body))))


(defppclapmacro with-altivec-registers (reglist &body body)
  `(%with-altivec-registers () ,reglist ,@body))


;;; Create an aligned buffer on the temp stack, large enough for N vector
;;; registers.  Make base be a pointer to this buffer (base can be
;;; any available GPR, since the buffer will be fixnum-tagged.) N should
;;; be a constant.
;;; The intent here is that the register 'base' can be used in subsequent
;;; stvx/lvx instructions.  Any vector registers involved in such instructions
;;; must have their corresponding bits saved in VRSAVE on platforms where
;;; that matters.

(defppclapmacro allocate-vector-buffer (base n)
  `(progn
    (stwux tsp (- (ash (1+ ,n) 4)))	; allocate a frame on temp stack
    (stw tsp 4 tsp)			; temp frame contains immediate data
    (la ,base (+ 8 8) tsp)		; skip header, round up
    (clrrwi ,base ,base 4)))		; align (round down)

;;; Execute the specified body of code; on entry to that body, BASE
;;; will point to the lowest address of a vector-aligned buffer with
;;; room for N vector registers.  On exit, the buffer will be
;;; deallocated.  The body should preserve the value of BASE as long
;;; as it needs to reference the buffer.

(defppclapmacro with-vector-buffer (base n &body body)
  `(progn
    (allocate-vector-buffer ,base ,n)
    (progn
      (progn ,@body)
      (unlink tsp))))

#|

;;; This is just intended to test the macros; I can't test whether or not the code works.

(defppclapfunction load-array ((n arg_z))
  (check-nargs 1)
  (with-altivec-registers (vr1 vr2 vr3 vr27) ; Clobbers imm0
    (li imm0 ppc32::misc-data-offset)
    (lvx vr1 arg_z imm0)		; load MSQ
    (lvsl vr27 arg_z imm0)		; set the permute vector
    (addi imm0 imm0 16)			; address of LSQ
    (lvx vr2 arg_z imm0)		; load LSQ
    (vperm vr3 vr1 vr2 vr27)		; aligned result appears in VR3
    (dbg t))				; Look at result in some debugger
  (blr))
|#

;;; see "Optimizing PowerPC Code" p. 156
;;; Note that the constant #x4330000080000000 is now in fp-s32conv

(defppclapmacro int-to-freg (int freg imm)
  `(let ((temp 8)
	 (temp.h 8)
	 (temp.l 12))
    (stwu tsp -16 tsp)
    (stw tsp 4 tsp)
    (stfd ppc::fp-s32conv temp tsp)
    (unbox-fixnum ,imm ,int)
    (xoris ,imm ,imm #x8000)       ; invert sign of unboxed fixnum
    (stw ,imm temp.l tsp)
    (lfd ,freg temp tsp)
    (lwz tsp 0 tsp)
    (fsub ,freg ,freg ppc::fp-s32conv)))




(ccl::provide "PPC-LAPMACROS")

; end of ppc-lapmacros.lisp
