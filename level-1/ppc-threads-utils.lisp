;;;-*-Mode: LISP; Package: ccl -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Opensourced MCL.
;;;
;;;   Opensourced MCL is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Lesser General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2.1 of the License, or (at your option) any later version.
;;;
;;;   Opensourced MCL is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;   Lesser General Public License for more details.
;;;
;;;   You should have received a copy of the GNU Lesser General Public
;;;   License along with this library; if not, write to the Free Software
;;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;

; low-level support for PPC threads and stack-backtrace printing

(in-package :ccl)



(defppclapfunction %get-kernel-global-from-offset ((offset arg_z))
  (check-nargs 1)
  (unbox-fixnum imm0 offset)
  (addi imm0 imm0 ppc::nil-value)
  (lwz arg_z 0 imm0)
  (blr))

(defppclapfunction %set-kernel-global-from-offset ((offset arg_y) (new-value arg_z))
  (check-nargs 2)
  (unbox-fixnum imm0 offset)
  (addi imm0 imm0 ppc::nil-value)
  (stw new-value 0 imm0)
  (blr))

(defppclapfunction %get-kernel-global-ptr-from-offset ((offset arg_y)
						       (ptr arg_z))
  (check-nargs 2)
  (unbox-fixnum imm0 offset)
  (addi imm0 imm0 ppc::nil-value)
  (lwz imm0 0 imm0)
  (stw imm0 arch::macptr.address ptr)
  (blr))


(defppclapfunction %fixnum-ref ((fixnum arg_y) #| &optional |# (offset arg_z))
  (cmpi cr0 nargs '1)
  (check-nargs 1 2)
  (bne cr0 @2-args)
  (mr fixnum offset)
  (li offset 0)
  @2-args
  (unbox-fixnum imm0 offset)
  (lwzx arg_z imm0 fixnum)
  (blr))

(defppclapfunction %fixnum-ref-u32 ((fixnum arg_y) #| &optional |# (offset arg_z))
  (cmpi cr0 nargs '1)
  (check-nargs 1 2)
  (bne cr0 @2-args)
  (mr fixnum offset)
  (li offset 0)
  @2-args
  (unbox-fixnum imm0 offset)
  (lwzx imm0 imm0 fixnum)
  (ba .SPmakeu32))


(defppclapfunction %fixnum-set ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (cmpi cr0 nargs '2)
  (check-nargs 2 3)
  (bne cr0 @3-args)
  (mr fixnum offset)
  (li offset 0)
  @3-args
  (unbox-fixnum imm0 offset)
  (stwx new-value imm0 fixnum)
  (mr arg_z new-value)
  (blr))

(defppclapfunction %fixnum-set-u32 ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (cmpwi cr0 nargs '2)
  (check-nargs 2 3)
  (bne cr0 @3-args)
  (mr fixnum offset)
  (li offset 0)
  @3-args
  (unbox-fixnum imm0 offset)
  (extract-typecode imm1 new-value)
  (cmpwi cr0 imm1 arch::tag-fixnum)
  (cmpwi cr1 imm1 arch::subtag-bignum)
  (srwi imm2 new-value arch::fixnumshift)
  (beq cr0 @store)
  (beq cr1 @bignum)
  @notu32
  (uuo_interr arch::error-object-not-unsigned-byte-32 new-value)
  @bignum
  (getvheader imm0 new-value)
  (cmpwi cr1 imm0 arch::one-digit-bignum-header)
  (cmpwi cr2 imm0 arch::two-digit-bignum-header)
  (lwz imm2 arch::misc-data-offset new-value)
  (cmpwi cr0 imm2 0)
  (beq cr1 @one)
  (bne cr2 @notu32)
  (lwz imm1 (+ 4 arch::misc-data-offset) new-value)
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

;;; Sure would be nice to have &optional in defppclapfunction arglists
;;; Sure would be nice not to do this at runtime.

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref)))))

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref-u32
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set-u32
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set)))))

(defun %fixnum-ref-macptr (fixnum &optional (offset 0))
  (%int-to-ptr (%fixnum-ref-u32 fixnum offset)))

(defun %fixnum-set-macptr (fixnum offset &optional (newval offset newval-p))
  (%fixnum-set-u32 fixnum (if newval-p offset 0) (%ptr-to-int newval))
  newval)
  
				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defsetf interrupt-level set-interrupt-level)



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

    
    
(defun %frame-backlink (p &optional (tcr (%current-tcr)))
  (cond ((fake-stack-frame-p p)
         (%fake-stack-frame.next-sp p))
        ((fixnump p)
         (let ((backlink (%%frame-backlink p))
               (fake-frame (symbol-value-in-tcr '*fake-stack-frames* tcr)))
           (loop
             (when (null fake-frame) (return backlink))
             (when (eq backlink (%fake-stack-frame.sp fake-frame))
               (return fake-frame))
             (setq fake-frame (%fake-stack-frame.link fake-frame)))))
        (t (error "~s is not a valid stack frame" p))))

(defppclapfunction %%frame-backlink ((p arg_z))
  (check-nargs 1)
  (lwz arg_z ppc::lisp-frame.backlink arg_z)
  (rlwinm imm0 arg_z 30 0 0)            ; Bit 1 -> sign bit
  (srawi imm0 imm0 31)                  ; copy sign bit to rest of word
  (andc arg_z arg_z imm0)               ; arg_z = 0 if bit 1 was set
  (rlwinm arg_z arg_z 0 0 29)           ; clear low two bits
  (blr))



(defppclapfunction %%frame-savefn ((p arg_z))
  (check-nargs 1)
  (lwz arg_z ppc::lisp-frame.savefn arg_z)
  (blr))

(defppclapfunction %cfp-lfun ((p arg_z))
  (lwz arg_y ppc::lisp-frame.savefn p)
  (extract-typecode imm0 arg_y)
  (cmpwi imm0 arch::subtag-function)
  (lwz loc-pc ppc::lisp-frame.savelr p)
  (bne @no)
  (lwz arg_x arch::misc-data-offset arg_y)
  (sub imm1 loc-pc arg_x)
  (la imm1 (- arch::misc-data-offset) imm1)
  (getvheader imm0 arg_x)
  (header-length imm0 imm0)
  (cmplw imm1 imm0)
  (box-fixnum imm1 imm1)
  (bge @no)
  (vpush arg_y)
  (vpush imm1)
  @go
  (set-nargs 2)
  (la temp0 8 vsp)
  (ba .SPvalues)
  @no
  (li imm0 nil)
  (vpush imm0)
  (vpush imm0)
  (b @go))


(defppclapfunction %%frame-savevsp ((p arg_z))
  (check-nargs 1)
  (lwz imm0 ppc::lisp-frame.savevsp arg_z)
  (rlwinm imm0 imm0 0 0 30)             ; clear lsb
  (mr arg_z imm0)
  (blr))



(eval-when (:compile-toplevel :execute)
  (assert (eql arch::t-offset #x11)))

(defppclapfunction %uvector-data-fixnum ((uv arg_z))
  (check-nargs 1)
  (trap-unless-fulltag= arg_z arch::fulltag-misc)
  (la arg_z arch::misc-data-offset arg_z)
  (blr))



(defun lisp-frame-p (p tcr)
  (or (fake-stack-frame-p p)
      (locally (declare (fixnum p))
        (let ((next-frame (%frame-backlink p tcr)))
          (when (fake-stack-frame-p next-frame)
            (setq next-frame (%fake-stack-frame.sp next-frame)))
          (locally (declare (fixnum next-frame))
            (if (bottom-of-stack-p next-frame tcr)
              (values nil t)
              (and
               (eql (ash ppc::lisp-frame.size (- arch::fixnum-shift))
                    (the fixnum (- next-frame p)))
               ;; EABI C functions keep their saved LRs where we save FN or 0
               ;; The saved LR of such a function would be fixnum-tagged and never 0.
               (let* ((fn (%fixnum-ref p ppc::lisp-frame.savefn)))
                 (or (eql fn 0) (typep fn 'function))))))))))

(defppclapfunction %catch-top ((tcr arg_z))
  (check-nargs 1)
  (cmp cr0 tcr rcontext)
  (bne cr0 @not-current)

  ; tcr = current-tcr
  (lwz arg_z arch::tcr.catch-top rcontext)
  (cmpwi cr0 arg_z 0)
  (bne @ret)
  (li arg_z ppc::nil-value)
 @ret
  (blr)

@not-current
  (lwz imm0 arch::tcr.ts-area tcr)
  (lwz imm0 arch::area.active imm0)
  (la arg_z (+ 8 arch::fulltag-misc) imm0)
  (blr))









; Same as %address-of, but doesn't cons any bignums
; It also left shift fixnums just like everything else.
(defppclapfunction %fixnum-address-of ((x arg_z))
  (check-nargs 1)
  (box-fixnum arg_z x)
  (blr))

(defppclapfunction %sub-timevals ((result arg_x) (a arg_y) (b arg_z))
  (let ((pa imm0)
        (pb imm1)
        (1m imm2)
        (seconds imm3)
        (micros imm4)
        (temp imm2))
    (macptr-ptr pa a)
    (macptr-ptr pb b)
    (lwz temp 4 pb)
    (lwz micros 4 pa)
    (subf micros temp micros)
    (cmpwi micros 0)
    (lwz temp 0 pb)
    (lwz seconds 0 pa)
    (subf seconds temp seconds)
    (lwi 1m 1000000)
    (macptr-ptr pa result)
    (bge @store)
    (add micros 1m micros)
    (la seconds -1 seconds)
    @store
    (stw seconds 0 pa)
    (stw micros 4 pa)
    (mr arg_z result)
    (blr)))


;;; A callback to handle foreign thread preparation, initialization,
;;; and termination.
;;; "preparation" involves telling the kernel to reserve space for
;;; some initial thread-specific special bindings.  The kernel
;;; needs to reserve this space on the foreign thread's vstack;
;;; it needs us to tell it how much space to reserve (enough
;;; for bindings of *current-thread*, *current-process*, and
;;; the default initial bindings of *PACKAGE*, etc.)
;;;
;;; "initialization" involves making those special bindings in
;;; the vstack space reserved by the kernel, and setting the
;;; values of *current-thread* and *current-process* to newly
;;; created values.
;;;
;;; "termination" involves removing the current thread and
;;; current process from the global thread/process lists.
;;; "preparation" and "initialization" happen when the foreign
;;; thread first tries to call lisp code.  "termination" happens
;;; via the pthread thread-local-storage cleanup mechanism.
(defcallback %foreign-thread-control (:without-interrupts t :int param :int)
  (declare (fixnum param))
  (cond ((< param 0) (%foreign-thread-prepare))
	((= param 0) (%foreign-thread-initialize) 0)
	(t (%foreign-thread-terminate) 0)))

(defppclapfunction %save-standard-binding-list ((bindings arg_z))
  (lwz imm0 arch::tcr.vs-area rcontext)
  (lwz imm1 arch::area.high imm0)
  (push bindings imm1)
  (blr))

(defppclapfunction %saved-bindings-address ()
  (lwz imm0 arch::tcr.vs-area rcontext)
  (lwz imm1 arch::area.high imm0)
  (la arg_z -4 imm1)
  (blr))

(defun %foreign-thread-prepare ()
  (let* ((initial-bindings (standard-initial-bindings)))
    (%save-standard-binding-list initial-bindings)
    (* 3 (+ 2 (length initial-bindings)))))


(defun %foreign-thread-initialize ()
  ;; Recover the initial-bindings alist.
  (let* ((bsp (%saved-bindings-address))
	 (initial-bindings (%fixnum-ref bsp )))
    (declare (fixnum bsp))
    (flet ((save-binding (value symbol prev)
	     (setf (%fixnum-ref bsp -4) value
		   (%fixnum-ref bsp -8) symbol
		   (%fixnum-ref bsp -12) prev
		   bsp (- bsp 3))))
      (save-binding nil '*current-lisp-thread* 0)
      (save-binding nil '*current-process* bsp)
      (dolist (pair initial-bindings)
	(save-binding (funcall (cdr pair)) (car pair) bsp))
      (setf (%fixnum-ref (%current-tcr) arch::tcr.db-link) bsp)
      ;; Ensure that pending unwind-protects (for WITHOUT-INTERRUPTS
      ;; on the callback) don't try to unwind the binding stack beyond
      ;; where it was just set.
      (let* ((top-catch (%fixnum-ref (%current-tcr) arch::tcr.catch-top)))
        (unless (eql 0 top-catch)
          (setf (%fixnum-ref top-catch arch::catch-frame.db-link) bsp)))))
  (let* ((thread (new-lisp-thread-from-tcr (%current-tcr) nil)))
    (setq *current-lisp-thread* thread
	  *current-process*
	  (make-process "foreign" :thread thread))
    (setf (%process-whostate *current-process*) "Foreign thread callback")))
    
;;; Remove the foreign thread's lisp-thread and lisp process from
;;; the global lists.
(defun %foreign-thread-terminate ()
  (let* ((proc *current-process*))
    (when proc (remove-from-all-processes proc))))
