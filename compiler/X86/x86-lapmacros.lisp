;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates and contributors.
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

;;; Comparisons make more sense if arg order is "dest, src", instead
;;; of the gas/ATT arg order.

(defx86lapmacro rcmp (src dest)
  `(cmp ,dest ,src))

(defx86lapmacro clrq (reg)
  `(xorq (% ,reg) (% ,reg)))

(defx86lapmacro set-nargs (n)
  (if (eql n 0)
    `(xorw (% nargs) (% nargs))
    `(movw ($ ',n) (% nargs))))

(defx86lapmacro check-nargs (min &optional (max min))
  (let* ((ok (gensym)))
    (if (= max min)
      `(progn
        (rcmp (% nargs) ($ ',min))
        (je.pt ,ok)
        (uuo-error-wrong-number-of-args)
        ,ok)
      (if (null max)
        (unless (zerop min)
          `(progn
            (rcmp (% nargs) ($ ',min))
            (jae.pt  ,ok)
            (uuo-error-too-few-args)
            ,ok))
        (if (zerop min)
          `(progn
            (rcmp (% nargs) ($ ',max))
            (jb.pt  ,ok)
            (uuo-error-too-many-args)
            ,ok)
          (let* ((sofar (gensym)))
            `(progn
              (rcmp (% nargs) ($ ',min))
              (jae.pt  ,sofar)
              (uuo-error-too-few-args)
              ,sofar
              (rcmp (% nargs) ($ ',max))
              (jb.pt  ,ok)
              (uuo-error-too-many-args)
              ,ok)))))))



(defx86lapmacro extract-lisptag (node dest)
  `(progn
    (movb ($ x8664::tagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-fulltag (node dest)
  `(progn
    (movb ($ x8664::fulltagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-subtag (node dest)
  `(movb (@ x8664::misc-subtag-offset (% ,node)) (%b ,dest)))

(defx86lapmacro extract-typecode (node dest)
  ;;; In general, these things are only defined to affect the low
  ;;; byte of the destination register.  This can also affect
  ;;; the #xff00 byte.
  `(progn
    (extract-lisptag ,node ,dest)
    (rcmp (%b ,dest) ($ x8664::tag-misc))
    (cmovew (@  x8664::misc-subtag-offset (% ,node)) (%w ,dest))))

(defx86lapmacro trap-unless-typecode= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
    `(progn
      (extract-typecode ,node ,immreg)
      (cmpb ($ ,tag) (%b ,immreg))
      (je.pt ,ok)
      (uuo-error-reg-not-tag (% ,node) ($ ,tag))
      ,ok)))

(defx86lapmacro trap-unless-fulltag= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
    `(progn
      (extract-fulltag ,node ,immreg)
      (cmpb ($ ,tag) (%b ,immreg))
      (je.pt ,ok)
      (uuo-error-reg-not-tag (% ,node) ($ ,tag))
      ,ok)))

(defx86lapmacro trap-unless-lisptag= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
    `(progn
      (extract-lisptag ,node ,immreg)
      (cmpb ($ ,tag) (%b ,immreg))
      (je.pt ,ok)
      (uuo-error-reg-not-tag (% ,node) ($ ,tag))
      ,ok)))

;;; On x8664, NIL has its own tag, so no other lisp object can
;;; have the same low byte as NIL.  (That probably won't be
;;; true on x8632.)
(defx86lapmacro cmp-reg-to-nil (reg)
  `(cmpb ($ (logand #xff x8664::nil-value)) (%b ,reg)))


(defx86lapmacro unbox-fixnum (src dest)
  `(progn
    (mov (% ,src) (% ,dest))
    (sar ($ x8664::fixnumshift) (% ,dest))))

(defx86lapmacro box-fixnum (src dest)
  `(lea (@ (% ,src) 8) (% ,dest)))

;;; stores the 32-bit value in low 32 bits of xmm reg dest.
;;; It seems to be faster to go through memory than it would
;;; be to use MOVD and PSRLQ.
(defx86lapmacro get-single-float (node dest)
  `(progn
    (movq (% ,node) (@ (% rcontext) x8664::tcr.single-float-convert))
    (movss (@ (% rcontext) x8664::tcr.single-float-convert.value) (% ,dest))))


(defx86lapmacro put-single-float (src node)
  `(progn
    (movss (% ,src) (@ (% rcontext) x8664::tcr.single-float-convert.value))
    (movq (@ (% rcontext) x8664::tcr.single-float-convert) (% ,node))))

(defx86lapmacro get-double-float (src fpreg)
  `(movsd (@ x8664::double-float.value (% ,src)) (% ,fpreg)))

(defx86lapmacro put-double-float (fpreg dest)
  `(movsd (% ,fpreg) (@ x8664::double-float.value (% ,dest))))
  

  
(defx86lapmacro getvheader (src dest)
  `(movq (@ x8664::misc-header-offset (% ,src)) (% ,dest)))

;;; "Size" is unboxed element-count.  vheader and dest should
;;; both be immediate registers
(defx86lapmacro header-size (vheader dest)
  `(progn
    (mov (% ,vheader) (% ,dest))
    (shr ($ x8664::num-subtag-bits) (% ,dest))))


;;; "Length" is fixnum element-count.
(defx86lapmacro header-length (vheader dest)
  `(progn
    (movq ($ (lognot 255)) (% ,dest))
    (andq (% ,vheader) (% ,dest))
    (shr ($ (- x8664::num-subtag-bits x8664::fixnumshift)) (% ,dest))))

(defx86lapmacro header-subtag[fixnum] (vheader dest)
  `(progn
    (lea (@ (% ,vheader) 8) (% ,dest))
    (andl ($ '255) (%l ,dest))))

(defx86lapmacro vector-size (vector vheader dest)
  `(progn
    (getvheader ,vector ,vheader)
    (header-size ,vheader ,dest)))

(defx86lapmacro vector-length (vector vheader dest)
  `(progn
    (getvheader ,vector ,vheader)
    (header-length ,vheader ,dest)))  

(defx86lapmacro int-to-double (int temp double)
  `(progn
    (unbox-fixnum  ,int ,temp)
    (cvtsi2sdq (% ,temp) (% ,double))))

(defx86lapmacro int-to-single (int temp single)
  `(progn
    (unbox-fixnum ,int ,temp)
    (cvtsi2sdq (% ,temp) (% ,single))))

(defx86lapmacro ref-global (global reg)
  `(movq (@ (+ x8664::nil-value ,(x8664::%kernel-global global))) (% ,reg)))

(defx86lapmacro set-global (reg global)
  `(movq (% ,reg) (@ (+ x8664::nil-value ,(x8664::%kernel-global global)))))

(defx86lapmacro macptr-ptr (src dest)
  `(movq (@ x8664::macptr.address (% ,src)) (% ,dest)))

;;; CODE is unboxed char-code (in low 8 bits); CHAR needs to be boxed.
(defx86lapmacro box-character (code char)
  `(progn
    (box-fixnum ,code ,char)
    (shl ($ (- x8664::charcode-shift x8664::fixnumshift)) (% ,char))
    (movb ($ x8664::subtag-character) (%b ,char))))

  
;;; index is a constant
(defx86lapmacro svref (vector index dest)
  `(movq (@ (+ x8664::misc-data-offset (* ,index 8)) (% ,vector)) (% ,dest)))

;;; Index is still a constant
(defx86lapmacro svset (vector index new)
  `(movq (% ,new) (@ (+ x8664::misc-data-offset (* ,index 8)) (% ,vector))))

(defx86lapmacro int-to-double (int temp double)
  `(progn
    (unbox-fixnum (% ,int) (% ,temp))
    (cvtsi2sdq (% ,temp) (% ,double))))

(defx86lapmacro int-to-double (int temp double)
  `(progn
    (unbox-fixnum (% ,int) (% ,temp))
    (cvtsi2ssq (% ,temp) (% ,double))))

;;; Frames, function entry and exit.


;;; Simple frame, since the caller didn't reserve space for it.
(defx86lapmacro save-simple-frame ()
  `(progn
    (pushq (% ra0))
    (pushq (% rbp))
    (movq (% rsp) (% rbp))))

(defx86lapmacro restore-simple-frame ()
  `(progn
    (leave)
    (popq (% ra0))))


;;; Caller pushed zeros to reserve space for stack frame before
;;; pushing args.  We have to discard it before returning.
(defx86lapmacro discard-reserved-frame ()
  `(add ($ '2) (% rsp)))

;;; Return to caller.  (% RA0) should contain a tagged return
;;; address inside the caller's (% FN).
(defx86lapmacro single-value-return ()
  `(jmp (% ra0)))

(defx86lapmacro recover-fn-from-ra0 (here)
  `(leaq (@ (- (:^ ,here)) (% ra0)) (% fn)))

;;; Using *x8664-backend* here is wrong but expedient.
(defun x86-subprim-offset (name)
  (let* ((info (find name (arch::target-subprims-table (backend-target-arch *x8664-backend*)) :test #'string-equal :key #'subprimitive-info-name))
         (offset (when info 
                   (subprimitive-info-offset info))))
    (or offset      
      (error "Unknown subprim: ~s" name))))

(defx86lapmacro jmp-subprim (name)
  `(jmp (* (@ ,(x86-subprim-offset name)))))

(defx86lapmacro call-subprim (name)
  (let* ((label (gensym)))
    `(progn
      (leaq (@ (:^ ,label) (% fn)) (% ra0))
      (jmp-subprim ,name)
      (:tra ,label)
      (recover-fn-from-ra0 ,label))))
     
(defx86lapmacro %car (src dest)
  `(movq (@ x8664::cons.car (% ,src)) (% ,dest)))

(defx86lapmacro %cdr (src dest)
  `(movq (@ x8664::cons.cdr (% ,src)) (% ,dest)))

(defx86lapmacro stack-probe ()
  (let* ((ok (gensym)))
    `(progn
      (rcmp (% rsp) (@ (% rcontext) x8664::tcr.cs-limit))
      (jae.pt ,ok)
      (uuo-stack-overflow)
      ,ok)))

(defx86lapmacro load-constant (constant dest &optional (fn 'fn))
  `(movq (@ ',constant (% ,fn)) (% ,dest)))

;;; call symbol named NAME, setting nargs to NARGS.  Do the TRA
;;; hair.   Args should already be in arg regs, and we expect
;;; to return a single value.
(defx86lapmacro call-symbol (name nargs)
  (let* ((return (gensym)))
    `(progn
      (load-constant ,name fname)
      (set-nargs ,nargs)
      (lea (@ (:^ ,return) (% fn)) (% ra0))
      (movq (@ x8664::symbol.fcell (% fname)) (% fn))
      (jmp (* (% fn)))
      (:tra ,return)
      (recover-fn-from-ra0 ,return))))

;;;  tail call the function named by NAME with nargs NARGS.  %FN is
;;;  the caller, which will be in %FN on entry to the callee.  For the
;;;  couple of instructions where neither %RA0 or %FN point to the
;;;  current function, ensure that %XFN does; this is necessary to
;;;  prevent the current function from being GCed halfway through
;;;  those couple of instructions.
(defx86lapmacro jump-symbol (name nargs)
  `(progn
    (load-constant ,name fname)
    (movq (% fn) (% xfn))
    (movq (@ x8664::symbol.fcell (% fname)) (% fn))
    (set-nargs ,nargs)
    (jmp (% fn))))

