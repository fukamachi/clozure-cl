;;;-*- Mode: Lisp; Package: (X86 :use CL) -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates and contributors.
;;;   This file is part of OpenMCL.
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License   known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict  the preamble takes precedence.
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(defmacro define-x8664-vinsn (vinsn-name (results args &optional temps) &body body)
  (%define-vinsn *x8664-backend* vinsn-name results args temps body))



(define-x8664-vinsn scale-32bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (movq (:%q idx) (:%q dest))
  (shrq (:$1 1) (:%q dest)))

(define-x8664-vinsn scale-16bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (movq (:%q idx) (:%q dest))
  (shrq (:$ub 2) (:%q dest)))

(define-x8664-vinsn scale-8bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (movq (:%q idx) (:%q dest))
  (shrq (:$ub 3) (:%q dest)))


(define-x8664-vinsn misc-ref-u64  (((dest :u64))
                                  ((v :lisp)
                                   (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-node  (((dest :lisp))
                                    ((v :lisp)
                                     (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-u8 (((dest :u8))
                                 ((v :lisp)
                                  (scaled-idx :s64)))
  (movzbq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))


(define-x8664-vinsn misc-ref-s64  (((dest :s64))
                                  ((v :lisp)
                                   (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))



(define-x8664-vinsn misc-ref-c-u64  (((dest :u64))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-ref-c-s64  (((dest :s64))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn misc-set-u64 (()
                                  ((val :u64)
                                   (v :lisp)
                                   (scaled-idx :u64)))
  (movq (:%q val) (:@ (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-u64 (()
				    ((val :u64)
				     (v :lisp)
				     (idx :u32const)))
  (movq (:%q val) (:@ idx (:%q v))))

(define-x8664-vinsn misc-set-s64 (()
                                  ((val :s64)
                                   (v :lisp)
                                   (scaled-idx :imm)))
  (movq (:%q val) (:@ x8664::misc-data-offset  (:%q v) (:%q scaled-idx))))


(define-x8664-vinsn misc-set-c-s64 (()
				    ((val :s64)
				     (v :lisp)
				     (idx :s32const)))
  (movq (:%q val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))

(define-x8664-vinsn (call-known-symbol :call) (((result (:lisp x8664::arg_z)))
					       ())
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (movq (:@ x8664::symbol.fcell (:% x8664::fname)) (:%q x8664::fn))
  (jmp (:%q x8664::fn))
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:% x8664::ra0)) (:%q x8664::fn)))

;;; It's neccessary to use x8664:xfn to reference the current function
;;; when neither x8664::fn nor x8664::ra0 are doing so.
(define-x8664-vinsn (jump-known-symbol :jumplr) (()
                                                 ())
  (movq (:%q x8664::fn) (:%q x8664::xfn))
  (movq (:@ x8664::symbol.fcell (:% x8664::fname)) (:%q x8664::fn))
  (jmp (:%q x8664::fn)))

(define-x8664-vinsn set-nargs (()
			       ((n :s16const)))
  (movw (:$w (:apply ash n x8664::word-shift)) (:%w x8664::nargs )))

(define-x8664-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  (cmpw (:$w (:apply ash n x8664::word-shift)) (:%w x8664::nargs))
  (jz.pt :ok)
  (uuo-error-wrong-number-of-args)
  :ok)

(define-x8664-vinsn check-min-nargs (()
                                       ((n :u16const)))
  (cmpw (:$w (:apply ash n x8664::word-shift)) (:%w x8664::nargs))
  (jae.pt :ok)
  (uuo-error-too-few-args)
  :ok)

(define-x8664-vinsn check-max-nargs (()
                                       ((n :u16const)))
  (cmpw (:$w (:apply ash n x8664::word-shift)) (:%w x8664::nargs))
  (jb.pt :ok)
  (uuo-error-too-many-args)
  :ok)

(define-x8664-vinsn build-or-init-frame (()
                                         ()
                                         ((temp :u64)))
  (movzwq (:%w x8664::nargs) (:%q temp))
  (subq (:$b (ash $numx8664argregs x8664::word-shift)) (:%q temp))
  (jle :simple)
  (leaq (:@ (:%q x8664::rsp) (:%q temp)) (:%q temp))
  (movq (:%q x8664::rbp) (:@ (:%q temp)))
  (movq (:%q temp) (:%q x8664::rbp))
  (movq (:% x8664::ra0) (:@ 8 (:%q x8664::rbp)))
  (jmp :done)
  :simple
  (pushq (:%q x8664::ra0))
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp))
  :done)


(define-x8664-vinsn default-1-arg (()
                                   ((min :u16const)))
  (cmpw (:$w (:apply ash min x8664::word-shift)) (:%w x8664::nargs))
  (je :done)
  ((:pred >= min 3)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 2)
   (movq (:%q x8664::arg_y) (:%q x8664::arg_x)))
  ((:pred >= min 1)
   (movq (:%q x8664::arg_z) (:%q x8664::arg_y)))
  (movq (:$l x8664::nil-value) (:%q x8664::arg_z))
  :done)


(define-x8664-vinsn default-2-args (()
				    ((min :u16const)))
  (cmpw (:$w (:apply ash (:apply 1+ min) x8664::word-shift)) (:%w x8664::nargs ))
  (jb :done)
  (je :one)
  ;; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (pushq (:%q x8664::arg_x)))   
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_y)))
  ((:pred >= min 1)
   (movq (:%q x8664::arg_z) (:%q x8664::arg_x)))
  (movq (:$l x8664::nil-value) (:%q x8664::arg_y))
  (jmp :last)
  :one
  ;; We got min+1 args: arg_y was supplied, arg_z defaults to nil.
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 1)
   (movq (:%q x8664::arg_y) (:%q x8664::arg_x)))
  (movq (:%q x8664::arg_z) (:%q x8664::arg_y))
  :last
  (movq (:$l x8664::nil-value) (:%q x8664::arg_z))
  :done)
  

(define-x8664-vinsn save-lisp-context-no-stack-args (()
                                                     ())
  (pushq (:%q x8664::ra0))
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp)))

(define-x8664-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (pushq (:% reg)))

(define-x8664-vinsn vframe-load (((dest :lisp))
				 ((frame-offset :u16const)
				  (cur-vsp :u16const)))
  (movq (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))        (:%q dest)))

(define-x8664-vinsn (popj :lispcontext :pop :csp :lrRestore :jumpLR)
    (() 
     ())
  (leave)
  (popq (:%q x8664::ra0))
  (jmp (:%q x8664::ra0)))

(define-x8664-vinsn (restore-full-lisp-context :lispcontext :pop :vsp :lrRestore)
    (()
     ())
  (leave)
  (popq (:%q x8664::ra0)))

(define-x8664-vinsn compare-to-nil (((crf :crf))
                                    ((arg0 t)))
  (cmpb (:$b #.(logand x8664::nil-value #xff)) (:%b arg0)))


(define-x8664-vinsn ref-constant (((dest :lisp))
                                  ((lab :label)))
  (movq (:@ (:^ lab) (:%q x8664::fn)) (:%q dest)))

  
(define-x8664-vinsn (jump :jump)
    (()
     ((label :label)))
  (jmp label))

(define-x8664-vinsn (cbranch-true :branch) (()
					    ((label :label)
					     (crbit :u8const)))
  (jcc (:$ub crbit) label))

(define-x8664-vinsn (cbranch-false :branch) (()
					     ((label :label)
					      (crbit :u8const)))
  (jcc (:$ub (:apply logxor 1 crbit)) label))


(define-x8664-vinsn (lri :constant-ref) (((dest :imm))
                                         ((intval :s64const))
                                         ())
  ((:pred = intval 0)
   (xorq (:%q dest) (:%q dest)))
  ((:and (:pred /= intval 0)
         (:pred >= intval  -2147483648)
         (:pred <= intval 2147483647))
   (movq (:$l intval) (:%q dest)))
  ((:or (:pred < intval  -2147483648)
        (:pred > intval 2147483647))
   (movq (:$q (:apply logand #xffffffff intval)) (:%q dest))))

(define-x8664-vinsn trap-unless-list (()
				      ((object :lisp))
				      ((tag :u8)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-list) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-list (:%q object))
  :ok)

(define-x8664-vinsn trap-unless-fixnum (()
                                        ((object :lisp))
                                        ((tag :u8)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-fixnum (:%q object))
  :ok)

(define-x8664-vinsn trap-unless-typecode= (()
					   ((object :lisp)
					    (tagval :u16const))
					   ((tag :u8)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (cmovew (:@ x8664::misc-subtag-offset (:%q object)) (:%w tag))
  (cmpb (:$b tagval) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q object) (:$ub tagval))
  :ok)


(define-x8664-vinsn check-misc-bound (()
				      ((idx :imm)
				       (v :lisp))
				      ((temp :u64)))
  (movq (:%q idx) (:%q temp))
  (shlq (:$ub (- x8664::num-subtag-bits x8664::fixnumshift)) (:%q temp))
  (cmpq (:@ x8664::misc-header-offset (:%q v)) (:%q temp))
  (jb.pt :ok)
  (uuo-error-vector-bounds (:%q idx) (:%q v))
  :ok)



(define-x8664-vinsn %cdr (((dest :lisp))
			  ((src :lisp)))
  (movq (:@ x8664::cons.cdr (:%q src)) (:%q dest)))

(define-x8664-vinsn %car (((dest :lisp))
			  ((src :lisp)))
  (movq (:@ x8664::cons.car (:%q src)) (:%q dest)))



(define-x8664-vinsn u8->char (((dest :lisp))
			      ((src :u8))
			      ())
  (leaq (:@ (:%q src) x8664::fixnumone) (:%q dest))
  (shlq (:$ub (- x8664::charcode-shift x8664::fixnum-shift)) (:%q dest))
  (movb (:$b x8664::subtag-character) (:%b dest)))