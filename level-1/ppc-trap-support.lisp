;;; ppc-trap-support
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
;; Copyright 1995 Digitool, Inc. The 'tool rules!
;;
;; Support for PPC traps, this includes the event-poll trap
;; and all the twxxx traps for type checks & arg count checks.



(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")

  (defparameter *ppc-instruction-fields*
    `((:opcode . ,(byte 6 26))
      (:rt . ,(byte 5 21))
      (:to . ,(byte 5 21))
      (:ra . ,(byte 5 16))
      (:rb . ,(byte 5 11))
      (:d . ,(byte 16 0))
      (:mb . ,(byte 5 6))
      (:me . ,(byte 5 1))
      (:x-minor . ,(byte 10 1))
      (:fulltag . ,(byte arch::ntagbits 0))
      (:lisptag . ,(byte arch::nlisptagbits 0))))
  
  (defun ppc-instruction-field (field-name)
    (or (cdr (assoc field-name *ppc-instruction-fields*))
	(error "Unknown PPC instruction field: ~s" field-name)))
  
  (defun ppc-instruction-field-mask (field-spec)
    (let* ((name (if (atom field-spec) field-spec (car field-spec)))
	   (value (if (atom field-spec) -1 (cadr field-spec))))
      (dpb value (ppc-instruction-field name) 0)))
  

  (defmacro with-xp-registers-and-gpr-offset ((xp register-number) (registers offset) &body body)
    `(with-macptrs ((,registers #+linuxppc-target (pref ,xp :ucontext.uc_mcontext.regs)
		                #+darwinppc-target (pref ,xp :ucontext.uc_mcontext.ss)))
      (let ((,offset (xp-gpr-offset ,register-number)))
	,@body)))

  (defmacro RA-field (instr)
    `(ldb (byte 5 16) ,instr))

  (defmacro RB-field (instr)
    `(ldb (byte 5 11) ,instr))

  (defmacro D-field (instr)
    `(ldb (byte 16 0) ,instr))

  (defmacro RS-field (instr)
    `(ldb (byte 5 21) ,instr))
  
  (defmacro lisp-reg-p (reg)
    `(>= ,reg ppc::fn))
  
  (defmacro ppc-lap-word (instruction-form)
    (uvref (uvref (compile nil
                           `(lambda (&lap 0)
			     (ppc-lap-function () ((?? 0))
			      ,instruction-form)))
		  
                  0) 0))
  
  (defmacro ppc-instruction-mask (&rest fields)
    `(logior ,@(mapcar #'ppc-instruction-field-mask (cons :opcode fields))))
  
  )  



; The #.'s are necessary below since (get-field-offset ..) expands into
; (values n type), and the compiler is not smart enough to notice that
; it's only going to use n, a constant fixnum.
(defun xp-gpr-offset (register-number)
  (unless (and (fixnump register-number)
               (<= -2 (the fixnum register-number))
               (< (the fixnum register-number) 48))
    (setq register-number (require-type register-number '(integer -2 48))))
  (the fixnum 
    (* (the fixnum #+linuxppc-target register-number
	           #+darwinppc-target (+ register-number 2)) 4)))



(defun xp-gpr-lisp (xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-object registers offset))))

(defun (setf xp-gpr-lisp) (value xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (%set-object registers offset value)))

(defun xp-gpr-signed-long (xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-signed-long registers offset))))

(defun xp-gpr-macptr (xp register-number)
  (with-xp-registers-and-gpr-offset (xp register-number) (registers offset)
    (values (%get-ptr registers offset))))

(defun xp-argument-list (xp)
  (let ((nargs (xp-gpr-lisp xp ppc::nargs))     ; tagged as a fixnum (how convenient)
        (arg-x (xp-gpr-lisp xp ppc::arg_x))
        (arg-y (xp-gpr-lisp xp ppc::arg_y))
        (arg-z (xp-gpr-lisp xp ppc::arg_z)))
    (cond ((eql nargs 0) nil)
          ((eql nargs 1) (list arg-z))
          ((eql nargs 2) (list arg-y arg-z))
          (t (let ((args (list arg-x arg-y arg-z)))
               (if (eql nargs 3)
                 args
                 (let ((vsp (xp-gpr-macptr xp ppc::vsp)))
                   (dotimes (i (- nargs 3))
                     (push (%get-object vsp (* i 4)) args))
                   args)))))))
    
(defun xp-fpscr-info (xp)
  (let* ((fpscr #+linuxppc-target (%get-unsigned-long (pref xp :ucontext.uc_mcontext.regs) (ash #$PT_FPSCR 2))
		#+darwinppc-target (pref xp :ucontext.uc_mcontext.fs.fpscr)))
    (values (ldb (byte 24 8) fpscr) (ldb (byte 8 0) fpscr))))

#+linuxppc-target
(defun xp-double-float (xp fpr)
  (%get-double-float (pref xp :ucontext.uc_mcontext.regs) (+ (ash #$PT_FPR0 2)  (ash fpr 3))))

#+darwinppc-target
(defun xp-double-float (xp fpr)
  (%get-double-float (pref xp :ucontext.uc_mcontext.fs) (ash fpr 3)))


(defparameter *trap-lookup-tries* 5)



(defun %scan-for-instr (mask opcode fn pc-index tries)
  (let ((code-vector (and fn (uvref fn 0)))
        (offset 0))
    (declare (fixnum offset))
    (flet ((get-instr ()
             (if code-vector
               (let ((index (+ pc-index offset)))
                 (when (< index 0) (return-from %scan-for-instr nil))
                 (uvref code-vector index))
               (%get-long pc-index (the fixnum (* 4 offset))))))
      (declare (dynamic-extent #'get-instr))
      (dotimes (i tries)
        (decf offset)
        (let ((instr (get-instr)))
          (when (match-instr instr mask opcode)
            (return instr))
          (when (codevec-header-p instr)
            (return nil)))))))



(defvar *error-reentry-count* 0)

(defun funcall-with-error-reentry-detection (thunk)
  (let* ((count *error-reentry-count*)
         (*error-reentry-count* (1+ count)))
    (cond ((eql count 0) (funcall thunk))
          ((eql count 1) (error "Error reporting error"))
          (t (bug "Error reporting error")))))

(defppclapfunction %code-vector-pc ((code-vector arg_y) (pcptr arg_z))
  (macptr-ptr imm0 pcptr)
  (lwz loc-pc 0 imm0)
  (sub imm0 loc-pc code-vector)
  (subi imm0 imm0 arch::misc-data-offset)
  (getvheader imm1 code-vector)
  (header-size imm1 imm1)
  (slwi imm1 imm1 2)
  (cmplw imm0 imm1)
  (li arg_z nil)
  (bgelr)
  (box-fixnum arg_z imm0)
  (blr))

(defun return-address-offset (xp fn machine-state-offset)
  (with-macptrs ((regs (pref xp #+linuxppc-target :ucontext.uc_mcontext.regs
			        #+darwinppc-target :ucontext.uc_mcontext)))
    (if (functionp fn)
      (or (%code-vector-pc (uvref fn 0) (%inc-ptr regs machine-state-offset))
           (%get-ptr regs machine-state-offset))
      (%get-ptr regs machine-state-offset))))

(defconstant lr-offset-in-register-context
  #+linuxppc-target (ash #$PT_LNK 2)
  #+darwinppc-target (+ (get-field-offset :mcontext.ss)
			(get-field-offset :ppc_thread_state.lr)))

(defconstant pc-offset-in-register-context
  #+linuxppc-target (ash #$PT_NIP 2)
  #+darwinppc-target (+ (get-field-offset :mcontext.ss)
			(get-field-offset :ppc_thread_state.srr0)))

; When a trap happens, we may have not yet created control
; stack frames for the functions containing PC & LR.
; If that is the case, we add fake-stack-frame's to *fake-stack-frames*
; There are 4 cases:
;
; PC in FN
;   Push 1 stack frame: PC/FN
;   This might miss one recursive call, but it won't miss any variables
; PC in NFN
;   Push 2 stack frames:
;   1) PC/NFN/VSP
;   2) LR/FN/VSP
;   This might think some of NFN's variables are part of FN's stack frame,
;   but that's the best we can do.
; LR in FN
;   Push 1 stack frame: LR/FN
; None of the above
;   Push no new stack frames
;
; The backtrace support functions in "ccl:l1;ppc-stack-groups.lisp" know how
; to find the fake stack frames and handle them as arguments.
(defun funcall-with-xp-stack-frames (xp trap-function thunk)
  (cond ((null trap-function)
         ; Maybe inside a subprim from a lisp function
         (let* ((fn (xp-gpr-lisp xp ppc::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context)))
           (if (fixnump lr)
             (let* ((sp (xp-gpr-lisp xp ppc::sp))
                    (vsp (xp-gpr-lisp xp ppc::vsp))
                    (frame (%cons-fake-stack-frame sp sp fn lr vsp *fake-stack-frames*))
                    (*fake-stack-frames* frame))
               (declare (dynamic-extent frame))
               (funcall thunk frame))
             (funcall thunk (xp-gpr-lisp xp ppc::sp)))))
        ((eq trap-function (xp-gpr-lisp xp ppc::fn))
         (let* ((sp (xp-gpr-lisp xp ppc::sp))
                (fn trap-function)
                (lr (return-address-offset
                     xp fn pc-offset-in-register-context))
                (vsp (xp-gpr-lisp xp ppc::vsp))
                (frame (%cons-fake-stack-frame sp sp fn lr vsp *fake-stack-frames*))
                (*fake-stack-frames* frame))
           (declare (dynamic-extent frame))
           (funcall thunk frame)))
        ((eq trap-function (xp-gpr-lisp xp ppc::nfn))
         (let* ((sp (xp-gpr-lisp xp ppc::sp))
                (fn (xp-gpr-lisp xp ppc::fn))
                (lr (return-address-offset
                     xp fn lr-offset-in-register-context))
                (vsp (xp-gpr-lisp xp ppc::vsp))
                (lr-frame (%cons-fake-stack-frame sp sp fn lr vsp))
                (pc-fn trap-function)
                (pc-lr (return-address-offset
                        xp pc-fn pc-offset-in-register-context))
                (pc-frame (%cons-fake-stack-frame sp lr-frame pc-fn pc-lr vsp *fake-stack-frames*))
                (*fake-stack-frames* pc-frame))
           (declare (dynamic-extent lr-frame pc-frame))
           (funcall thunk pc-frame)))
        (t (funcall thunk (xp-gpr-lisp xp ppc::sp)))))



;; Enter here from handle-trap in "lisp-exceptions.c".
;; xp is a pointer to an ExceptionInformationPowerPC record.
;; the-trap is the trap instruction that got us here.
;; fn-reg is either fn, nfn or 0. If it is fn or nfn, then
;; the trap occcurred in that register's code vector.
;; If it is 0, then the trap occurred somewhere else.
;; pc-index is either the index in fn-reg's code vector
;; or, if fn-reg is 0, the address of the PC at the trap instruction.
;; This code parallels the trap decoding code in
;; "lisp-exceptions.c" that runs if (symbol-value 'cmain)
;; is not a macptr.
;; Some of these could probably call %err-disp instead of error,
;; but I was too lazy to look them up.

(defcallback cmain (:without-interrupts t
					:address xp 
					:unsigned-fullword fn-reg 
					:address pc-or-index 
					:unsigned-fullword the-trap
					:signed-fullword 
					#+proxy-scheduler trap-level
					#-proxy-scheduler ignore-0
					:signed-fullword ignore-1)
  (declare (ignore ignore-1 #-proxy-scheduler ignore-0))
  ;; twgti nargs,0
  ;; time for event polling.
  ;; This happens a lot so we test for it first.
  (let ((fn (unless (eql fn-reg 0) (xp-gpr-lisp xp fn-reg))))
    (with-xp-stack-frames (xp fn frame-ptr)
      (if (eql the-trap (ppc-lap-word (twgti nargs 0)))
        (unwind-protect
          (progn
            ;(setq *interrupt-level* 0)
            (cmain))
	  (let* ((catch-top (%catch-top (%current-tcr)))
		 (csp (catch-frame-sp catch-top))
		 (vsp (%%frame-savevsp csp))
		 (topword (%fixnum-ref vsp)))
	    (declare (fixnum topword))
	    (unless (>= topword 0)
	      (setf (%fixnum-ref vsp) 0))))
        (with-error-reentry-detection
          (let ((pc-index (if (eql fn-reg 0) pc-or-index (%ptr-to-int pc-or-index)))
                instr ra temp rs condition)
            (cond
             ;; tweqi RA nil-value - resolve-eep
	      ((and (match-instr the-trap
				 (ppc-instruction-mask  :opcode :to :d)
				 (ppc-lap-word (tweqi ?? ppc::nil-value)))
		    (setq instr (scan-for-instr
				 (ppc-instruction-mask :opcode :d)
				 (ppc-lap-word (lwz ??
						    (+ 4 arch::misc-data-offset)
						    ??))
                                               fn pc-index)))
	       (let* ((eep (xp-gpr-lisp xp (RA-field instr))))
		 (resolve-eep eep)
		 (setf (xp-gpr-lisp xp (RA-field the-trap))
		       (eep.address eep))))
             ;; twnei RA,N; RA = nargs
             ;; nargs check, no optional or rest involved
	      ((match-instr the-trap
                           (ppc-instruction-mask :opcode :rt :ra)
                           (ppc-lap-word (twnei nargs ??)))
              (%error (if (< (xp-GPR-signed-long xp ppc::nargs) (D-field the-trap))
                        'too-few-arguments
                        'too-many-arguments )
                      (list :nargs (ash (xp-GPR-signed-long xp ppc::nargs)
					(- arch::fixnumshift))
			    :fn  fn)
                      frame-ptr))
             
             ;; twnei RA,N; RA != nargs, N = fulltag_node/immheader
             ;; type check; look for "lbz rt-imm,-3(ra-node)"
             ((and (or (match-instr the-trap
                                    (ppc-instruction-mask :opcode :rt :fulltag)
                                    (ppc-lap-word (twnei ?? arch::fulltag-nodeheader)))
                       (match-instr the-trap
                                    (ppc-instruction-mask :opcode :rt :fulltag)
                                    (ppc-lap-word (twnei ?? arch::fulltag-immheader))))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lbz ?? arch::misc-subtag-offset ??))
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (let* ((typecode (D-field the-trap))
                     (type-tag (logand typecode arch::fulltagmask))
                     (type-name (svref (if (eql type-tag arch::fulltag-nodeheader)
                                         *nodeheader-types*
                                         *immheader-types*)
                                       (ldb (byte (- arch::num-subtag-bits arch::ntagbits) arch::ntagbits) typecode))))
                (%error (make-condition 'type-error
                                        :format-control (%rsc-string $XWRONGTYPE)
                                        :datum (xp-GPR-lisp xp ra)
                                        :expected-type type-name)
                        nil
                        frame-ptr)))

             ;; twnei RA,N; RA != nargs, N = subtag_character
             ;; type check; look for "clrlwi rs-node,ra-imm,24" = "rlwinm rs,ra,0,24,31"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :rt :d)
                                (ppc-lap-word (twnei ?? arch::subtag-character)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :rb :mb :me)
                                               (ppc-lap-word (rlwinm ?? ?? 0 24 31))
                                               fn pc-index))
                   (lisp-reg-p (setq rs (RS-field instr))))
              (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type 'character)
                        nil
                        frame-ptr))

             ;; twnei RA,N; RA != nargs, N != fulltag_node/immheader
             ;; type check; look for "clrlwi rs-node,ra-imm,29/30" = "rlwinm rs,ra,0,29/30,31"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :rt)                                
                                (ppc-lap-word (twnei ?? ??)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :rb (:mb 28) :me)
                                               (ppc-lap-word (rlwinm ?? ?? 0 28 31))                                               
                                               fn pc-index))
                   (or (eql (- 32 arch::ntagbits) (setq temp (ldb #.(ppc-instruction-field :mb) instr)))
                       (eql (- 32 arch::nlisptagbits) temp))
                   (lisp-reg-p (setq rs (RS-field instr))))
              (let* ((tag (logand the-trap arch::tagmask))
                     (type-name 
                      (case tag
                        (#.arch::tag-fixnum 'fixnum)
                        (#.arch::tag-list (if (eql temp (- 32 arch::ntagbits)) 'cons 'list))
                        (#.arch::tag-misc 'uvector)
                        (#.arch::tag-imm 'immediate))))                                      
                (%error (make-condition 'type-error
                                        :datum (xp-GPR-lisp xp rs)
                                        :expected-type type-name)
                        nil
                        frame-ptr)))
             
             ;; twnei RA,N; RA != nargs, N = subtag_character
             ;; type check; look for "clrlwi rs-node,ra-imm,24" = "rlwinm rs,ra,0,24,31"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :rt :d)
                                (ppc-lap-word (twnei ?? arch::subtag-character)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :rb :mb :me)
                                               (ppc-lap-word (rlwinm ?? ?? 0 24 31))
                                               fn pc-index))
                   (lisp-reg-p (setq rs (RS-field instr))))
              (%error (make-condition 'type-error
                                      :datum (xp-GPR-lisp xp rs)
                                      :expected-type 'character)
                      nil
                      frame-ptr))
             
             ;; twlgti RA,N; RA = nargs (xy = 01)
             ;; twllti RA,N; RA = nargs (xy = 10)
             ;; nargs check, optional or rest involved
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode (:to #x1c) :ra)
                                (ppc-lap-word (twi ?? ppc::nargs ??)))
                   (or (eql #b01 (setq temp (ldb #.(ppc-instruction-field :to) the-trap)))
	               (eql #b10 temp)))
              (%error (if (eql temp #b10)
                        'too-few-arguments
                        'too-many-arguments)
                      (list :nargs (ash (xp-GPR-signed-long xp ppc::nargs)
					(- arch::fixnumshift))
			    :fn  fn)
                      frame-ptr))
             
             ;; tweqi RA,N; N = unbound
             ;; symeval boundp check; look for "lwz RA,symbol.vcell(nodereg)"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :rt :d)                                
                                (ppc-lap-word (tweqi ?? arch::unbound-marker)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lwz ?? arch::symbol.vcell ??))                                               
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (setf (xp-GPR-lisp xp (RA-field the-trap))
                    (%kernel-restart-internal $xvunbnd (list (xp-GPR-lisp xp ra)) frame-ptr)))
	     ;; tweqi RA,N: n = (%slot-unbound-marker)
	     ;; slot-unbound trap.  Look for preceding "lwzx RA,rx,ry".
	     ;; rx = slots-vector, ry = scaled index in slots vector.
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :rt :d)
				(ppc-lap-word (tweqi ?? arch::slot-unbound-marker)))
		   (setq instr (scan-for-instr (ppc-instruction-mask
						:opcode :rt  :x-minor)
					       (dpb
						(RA-field the-trap)
						(byte 5 21)
						(ppc-lap-word
						 (lwzx ?? ?? ??)))
					       fn pc-index)))
              ;; %SLOT-UNBOUND-TRAP will decode the arguments further, then call
              ;; the generic function SLOT-UNBOUND.  That might return a value; if
              ;; so, set the value of the register that caused the trap to that
              ;; value.
              (setf (xp-gpr-lisp xp (ra-field the-trap))
                    (%slot-unbound-trap (xp-gpr-lisp xp (RA-field instr))
                                        (ash (- (xp-gpr-signed-long xp (RB-field instr))
                                                arch::misc-data-offset)
                                             (- arch::word-shift))
                                        frame-ptr)))
             ;; twlge RA,RB
             ;; vector bounds check; look for "lwz immreg, misc_header_offset(nodereg)"
             ((and (match-instr the-trap
                                (ppc-instruction-mask :opcode :to :x-minor)                                
                                (ppc-lap-word (twlge 0 0)))
                   (setq instr (scan-for-instr (ppc-instruction-mask :opcode #|:d|#)
                                               (ppc-lap-word (lwz ?? ?? #|arch::misc-header-offset|# ??))
                                               fn pc-index))
                   (lisp-reg-p (setq ra (RA-field instr))))
              (%error (%rsc-string $xarroob)
                      (list (xp-GPR-lisp xp (RA-field the-trap))
                            (xp-GPR-lisp xp ra))
                      frame-ptr))
             ;; twi 27 ra d - array header rank check
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to)
				(ppc-lap-word (twi 27 ?? ??)))
		   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lwz ?? arch::arrayH.rank ??))
                                               fn pc-index))
		   (lisp-reg-p (setq ra (RA-field instr))))
	      (%error (%rsc-string $xndims)
		      (list (xp-gpr-lisp xp ra)
			    (ash (ldb (byte 16 0) the-trap) (- arch::fixnumshift)))
		      frame-ptr))
	     ;; tw 27 ra rb - array flags check
	     ((and (match-instr the-trap
				(ppc-instruction-mask :opcode :to :x-minor)
				(ppc-lap-word (tw 27 ?? ??)))
		   (setq instr (scan-for-instr (ppc-instruction-mask :opcode :d)
                                               (ppc-lap-word (lwz ?? arch::arrayH.flags ??))
                                               fn pc-index))
		   (lisp-reg-p (setq ra (RA-field instr)))
		   (let* ((expected (xp-gpr-lisp xp (RB-field the-trap)))
			  (expected-subtype (ldb
					     arch::arrayH.flags-cell-subtag-byte
					     expected))
			  (expect-simple (=
					  (ldb arch::arrayH.flags-cell-bits-byte
					       expected)
					  (ash 1 $arh_simple_bit)))
			  (type-name
			   (case expected-subtype
			     (#.arch::subtag-double-float-vector 'double-float))))

		     (and type-name expect-simple
			  (setq condition
				(make-condition 'type-error
						:datum (xp-gpr-lisp xp ra)
						:expected-type
						`(simple-array ,type-name))))))
	      (%error condition nil frame-ptr))
			       
             ;; Unknown trap
             (t (%error "Unknown trap: #x~x~%xp: ~s, fn: ~s, pc: #x~x"
                        (list the-trap xp fn (ash pc-index arch::fixnumshift))
                        frame-ptr)))))))))

#+ppc-target
(defun handle-gc-hooks ()
  (let ((bits *gc-event-status-bits*))
    (declare (fixnum bits))
    (cond ((logbitp $gc-postgc-pending-bit bits)
           (setq *gc-event-status-bits*
                 (logand (lognot (ash 1 $gc-postgc-pending-bit))
                         bits))
           (let ((f *post-gc-hook*))
             (when (functionp f) (funcall f)))))))

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
  (la imm3 ppc::eabi-c-frame.param0 sp)
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
  (lbz imm0 (+ arch::misc-data-offset 0) temp0)
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
  (lbz imm0 (+ arch::misc-data-offset 1) temp0)
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
  (lbz imm0 (+ arch::misc-data-offset 2) temp0)
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
  (lbz imm0 (+ arch::misc-data-offset 3) temp0)
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
  (lbz imm0 (+ arch::misc-data-offset 4) temp0)
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
  (lbz imm0 (+ arch::misc-data-offset 5) temp0)
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
  (lbz imm0 (+ arch::misc-data-offset 6) temp0)
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
  (lbz imm0 (+ arch::misc-data-offset 0) temp0)
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
  (li arg_z ppc::nil-value)
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
				   (+ ppc::c-frame.size ppc::lisp-frame.size)
				   -2)
			       (+
				   (ash
				    (+ ppc::c-frame.size ppc::lisp-frame.size)
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
  (stw sp ppc::c-frame.savelr sp)
  (lwz imm2 monitor-exception-ports vsp)
  (cmpwi cr1 imm2 nil)
  (macptr-ptr imm2 buf)
  (mr imm1 imm2)
  (la imm3 ppc::c-frame.param0 sp)
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
  (lbz imm0 (+ arch::misc-data-offset 0) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 0) temp1)
  (blt @loaded)
  (bne @load-fp1-double)
  (lfsx fp1 imm1 imm2)
  (b @load-fp2)
  @load-fp1-double
  (lfdx fp1 imm1 imm2)

  @load-fp2
  (lbz imm0 (+ arch::misc-data-offset 1) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 2) temp1)
  (blt @loaded)
  (bne @load-fp2-double)
  (lfsx fp2 imm1 imm2)
  (b @load-fp3)
  @load-fp2-double
  (lfdx fp2 imm1 imm2)

  @load-fp3
  (lbz imm0 (+ arch::misc-data-offset 2) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 4) temp1)
  (blt @loaded)
  (bne @load-fp3-double)
  (lfsx fp3 imm1 imm2)
  (b @load-fp4)
  @load-fp3-double
  (lfdx fp3 imm1 imm2)

  @load-fp4
  (lbz imm0 (+ arch::misc-data-offset 3) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 6) temp1)
  (blt @loaded)
  (bne @load-fp4-double)
  (lfsx fp4 imm1 imm2)
  (b @load-fp5)
  @load-fp4-double
  (lfdx fp4 imm1 imm2)

  @load-fp5
  (lbz imm0 (+ arch::misc-data-offset 4) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 8) temp1)
  (blt @loaded)
  (bne @load-fp5-double)
  (lfsx fp5 imm1 imm2)
  (b @load-fp6)
  @load-fp5-double
  (lfdx fp5 imm1 imm2)

   @load-fp6
  (lbz imm0 (+ arch::misc-data-offset 5) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 10) temp1)
  (blt @loaded)
  (bne @load-fp1-double)
  (lfsx fp6 imm1 imm2)
  (b @load-fp7)
  @load-fp6-double
  (lfdx fp6 imm1 imm2)

   @load-fp7
  (lbz imm0 (+ arch::misc-data-offset 6) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 12) temp1)
  (blt @loaded)
  (bne @load-fp1-double)
  (lfsx fp7 imm1 imm2)
  (b @load-fp8)
  @load-fp7-double
  (lfdx fp7 imm1 imm2)

  @load-fp8
  (lbz imm0 (+ arch::misc-data-offset 7) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 14) temp1)
  (blt @loaded)
  (bne @load-fp8-double)
  (lfsx fp8 imm1 imm2)
  (b @load-fp9)
  @load-fp8-double
  (lfdx fp8 imm1 imm2)

  @load-fp9
  (lbz imm0 (+ arch::misc-data-offset 8) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 16) temp1)
  (blt @loaded)
  (bne @load-fp9-double)
  (lfsx fp9 imm1 imm2)
  (b @load-fp10)
  @load-fp9-double
  (lfdx fp9 imm1 imm2)

  @load-fp10
  (lbz imm0 (+ arch::misc-data-offset 9) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 18) temp1)
  (blt @loaded)
  (bne @load-fp10-double)
  (lfsx fp10 imm1 imm2)
  (b @load-fp11)
  @load-fp10-double
  (lfdx fp10 imm1 imm2)

  @load-fp11
  (lbz imm0 (+ arch::misc-data-offset 10) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 20) temp1)
  (blt @loaded)
  (bne @load-fp11-double)
  (lfsx fp11 imm1 imm2)
  (b @load-fp12)
  @load-fp11-double
  (lfdx fp11 imm1 imm2)

  @load-fp12
  (lbz imm0 (+ arch::misc-data-offset 11) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 22) temp1)
  (blt @loaded)
  (bne @load-fp12-double)
  (lfsx fp12 imm1 imm2)
  (b @load-fp13)
  @load-fp12-double
  (lfdx fp12 imm1 imm2)

  @load-fp13
  (lbz imm0 (+ arch::misc-data-offset 12) temp0)
  (cmpwi imm0 1)
  (lhz imm2 (+ arch::misc-data-offset 24) temp1)
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
  (li arg_z ppc::nil-value)
  (blr))

  

  )
