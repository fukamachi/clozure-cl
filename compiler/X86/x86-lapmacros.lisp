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


(defx86lapmacro set-nargs (n)
  (if (eql n 0)
    `(xorw (% nargs) (% nargs))
    `(movw ($ ',n) (% nargs))))

(defx86lapmacro check-nargs (min &optional (max min))
  (let* ((ok (gensym)))
    (if (= max min)
      `(progn
        (cmpw ($ ',min) (% nargs))
        (je+ (^ ,ok))
        (uuo-error-wrong-number-of-args)
        ,ok)
      (if (null max)
        (unless (zerop min)
          `(progn
            (cmpw ($ ',min) (% nargs))
            (jae+ (^ ,ok))
            (uuo-error-too-few-args)
            ,ok))
        (if (zerop min)
          `(progn
            (cmpw ($ ',max) (% nargs))
            (jb+ (^ ,ok))
            (uuo-error-too-many-args)
            ,ok)
          (let* ((sofar (gensym)))
            `(progn
              (cmpw ($ ',min) (% nargs))
              (jae+ (^ ,sofar))
              (uuo-error-too-few-args)
              ,sofar
              (cmpw ($ ',max) (% nargs))
              (jb+ ( ^ ,ok))
              (uuo-error-too-many-args)
              ,ok)))))))


(defx86lapmacro save-pc ()
  `(xchg (% fn) (% nfn)))
    
(defx86lapmacro save-lisp-context ()
  `(progn
    (xchg (% fn) (% nfn))
    (pushq (% nfn))
    (pushq (% rbp))
    (movq (% rsp) (% rbp))))


(defx86lapmacro extract-lisptag (node dest)
  `(progn
    (movb ($ x8664::tagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-fulltag (node dest)
  `(progn
    (movb ($ x8664::fulltagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-subtag (node dest)
  `(movb (@ x8664::misc-subtag-offset (%q ,node)) (%b ,dest)))

(defx86lapmacro extract-typecode (node dest)
  ;;; In general, these things are only defined to affect the low
  ;;; byte of the destination register.  This can also affect
  ;;; the #xff00 byte.
  `(progn
    (extract-lisptag ,node ,dest)
    (cmpb ($ x8664::tag-misc) (%b ,dest))
    (cmovew (@  x8664::misc-subtag-offset (%q ,node)) (%w ,dest))))

(defx86lapmacro unbox-fixnum (src dest)
  `(progn
    (mov (% ,src) (% ,dest))
    (shr ($ x8664::fixnumshift) (% ,dest))))

(defx86lapmacro box-fixnum (src dest)
  `(lea (@ (% ,src) 8) (% ,dest)))

;;; stores the 32-bit value in low 32 bits of xmm reg dest
(defx86lapmacro get-single-float (node dest)
  `(progn
    (movd (% ,node) (% ,dest))          ; value now in bits 32:63 of dest
    (psrlq ($ 32) (% ,dest))))          ; one way to get value to bits 0:31

;;; Note that this modifies src.
(defx86lapmacro put-single-float (src node)
  `(progn
    (psllq ($ 32) (% ,src))
    (movd (% ,src) (% ,node))           ; dest now tagged as a fixnum
    (movb ($ x8664::subtag-single-float) (%b ,node)) ; fix that
    ))

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
    (unbox-fixnum (% ,int) (% ,temp))
    (cvtsi2sdq (% ,temp) (% ,double))))

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

;;; no frame to deal with.
(defx86lapmacro simple-function-entry ()
  `(xchg (% nfn) (% fn)))

;;; Caller pushed zeros to reserve space for stack frame before
;;; pushing args.  We have to discard it before returning.
(defx86lapmacro discard-reserved-frame ()
  `(add ($ '2) (% rsp)))

;;; Return to caller.  (% NFN) should contain a tagged return
;;; address inside the caller's (% FN).
(defx86lapmacro single-value-return ()
  `(jmp (* (% nfn))))

