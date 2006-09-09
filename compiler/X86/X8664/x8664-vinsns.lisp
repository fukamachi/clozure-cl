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

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "X8664-BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "X8664ENV"))

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

(define-x8664-vinsn misc-ref-double-float  (((dest :double-float))
                                            ((v :lisp)
                                             (scaled-idx :imm)))
  (movsd (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-c-double-float  (((dest :double-float))
                                              ((v :lisp)
                                             (idx :s32const)))
  (movsd (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%xmm dest)))


(define-x8664-vinsn misc-ref-node  (((dest :lisp))
                                    ((v :lisp)
                                     (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn (push-misc-ref-node :push :node :vsp)  (()
                                                            ((v :lisp)
                                                             (scaled-idx :imm)))
  (pushq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-node (()
				   ((val :lisp)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movq (:%q val) (:@ x8664::misc-data-offset (:%q  v) (:%q unscaled-idx))))


(define-x8664-vinsn misc-set-double-float (()
				   ((val :double-float)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movsd (:%xmm val) (:@ x8664::misc-data-offset (:%q  v) (:%q unscaled-idx))))

(define-x8664-vinsn misc-ref-u8 (((dest :u8))
                                 ((v :lisp)
                                  (scaled-idx :s64)))
  (movzbl (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%l dest)))

(define-x8664-vinsn misc-ref-s8 (((dest :s8))
                                 ((v :lisp)
                                  (scaled-idx :s64)))
  (movsbq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-u16 (((dest :u16))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movzwl (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%l dest)))

(define-x8664-vinsn misc-ref-u32 (((dest :u32))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movl (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%l dest)))


(define-x8664-vinsn misc-ref-single-float (((dest :single-float))
                                           ((v :lisp)
                                            (scaled-idx :s64)))
  (movss(:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-s32 (((dest :s32))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movslq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-s16 (((dest :s16))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movswq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-s64  (((dest :s64))
                                  ((v :lisp)
                                   (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))


(define-x8664-vinsn misc-ref-c-node  (((dest :lisp))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn (push-misc-ref-c-node :push :node :vsp)
    (()
     ((v :lisp)
      (idx :u32const)) ; sic
     ())
  (pushq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v))))

(define-x8664-vinsn misc-ref-c-u64  (((dest :u64))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn misc-ref-c-s64  (((dest :s64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn misc-ref-c-u32  (((dest :u32))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2)) (:%q v)) (:%l dest)))

(define-x8664-vinsn misc-ref-c-s32  (((dest :s32))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movslq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-ref-c-single-float  (((dest :single-float))
                                              ((v :lisp)
                                               (idx :s32const)) ; sic
                                              ())
  (movss (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-c-u8  (((dest :u64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movzbl (:@ (:apply + x8664::misc-data-offset idx) (:%q v)) (:%l dest)))

(define-x8664-vinsn misc-ref-c-s8  (((dest :s64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movsbq (:@ (:apply + x8664::misc-data-offset idx) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-set-u64 (()
                                  ((val :u64)
                                   (v :lisp)
                                   (idx :u64)))
  (movq (:%q val) (:@ x8664::misc-data-offset (:%q v) (:%q idx))))

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


(define-x8664-vinsn misc-set-c-node (()
				    ((val :lisp)
				     (v :lisp)
				     (idx :s32const)))
  (movq (:%q val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))

(define-x8664-vinsn set-closure-forward-reference (()
                                                   ((val :lisp)
                                                    (closure :lisp)
                                                    (idx :s32const)))
  (movq (:%q val) (:@ (:apply + x8664::misc-function-offset (:apply ash idx x8664::word-shift)) (:%q closure))))


(define-x8664-vinsn misc-set-c-double-float (()
				    ((val :double-float)
				     (v :lisp)
				     (idx :s32const)))
  (movsd (:%xmm val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))

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
  ((:pred = n 0)
   (xorw (:%w x8664::nargs ) (:%w x8664::nargs )))
  ((:not (:pred = n 0))
   (movw (:$w (:apply ash n x8664::word-shift)) (:%w x8664::nargs ))))

(define-x8664-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  ((:pred = n 0)
   (testw (:%w x8664::nargs) (:%w x8664::nargs)))
  ((:not (:pred = n 0))
   (cmpw (:$w (:apply ash n x8664::word-shift)) (:%w x8664::nargs)))
  (jz.pt :ok)
  (uuo-error-wrong-number-of-args)
  :ok)

(define-x8664-vinsn check-min-nargs (()
                                       ((n :u16const)))
  (rcmpw (:%w x8664::nargs) (:$w (:apply ash n x8664::word-shift)))
  (jae.pt :ok)
  (uuo-error-too-few-args)
  :ok)

(define-x8664-vinsn check-max-nargs (()
                                       ((n :u16const)))
  (rcmpw (:%w x8664::nargs) (:$w (:apply ash n x8664::word-shift)))
  (jbe.pt :ok)
  (uuo-error-too-many-args)
  :ok)


(define-x8664-vinsn default-1-arg (()
                                   ((min :u16const)))
  (rcmpw (:%w x8664::nargs) (:$w (:apply ash min x8664::word-shift)))
  (jne :done)
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
  (rcmpw (:%w x8664::nargs ) (:$w (:apply ash (:apply 1+ min) x8664::word-shift)))
  (ja :done)
  (je :one)
  ;; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (pushq (:%q x8664::arg_x)))   
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_y)))
  ((:pred >= min 1)
   (movq (:%q x8664::arg_z) (:%q x8664::arg_x)))
  (movl (:$l x8664::nil-value) (:%l x8664::arg_y))
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

(define-x8664-vinsn default-3-args (()
				    ((min :u16const)))
  (rcmpw (:%w x8664::nargs ) (:$w (:apply ash (:apply + 2 min) x8664::word-shift)))
  (ja :done)
  (je :two)
  (rcmpw (:%w x8664::nargs ) (:$w (:apply ash min x8664::word-shift)))
  (je :none)
  ;; The first (of three) &optional args was supplied.
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 1)
   (pushq (:%q x8664::arg_y)))
  (movq (:%q x8664::arg_z) (:%q x8664::arg_x))
  (jmp :last-2)
  :two
  ;; The first two (of three) &optional args were supplied.
  ((:pred >= min 1)
   (pushq (:%q x8664::arg_x)))
  (movq (:%q x8664::arg_y) (:%q x8664::arg_x))
  (movq (:%q x8664::arg_z) (:%q x8664::arg_y))
  (jmp :last-1)
  ;; None of the three &optional args was provided.
  :none
  ((:pred >= min 3)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_y)))
  ((:pred >= min 1)
   (pushq (:%q x8664::arg_z)))
  (movl (:$l x8664::nil-value) (:%l x8664::arg_x))
  :last-2
  (movl (:$l x8664::nil-value) (:%l x8664::arg_y))
  :last-1
  (movl (:$l x8664::nil-value) (:%l x8664::arg_z))
  :done)


(define-x8664-vinsn default-optionals (()
                                       ((n :u16const))
                                       ((temp :u64)))
  (rcmpw (:%w x8664::nargs) (:$w (:apply ash n x8664::word-shift)))
  (movw (:%w x8664::nargs) (:%w temp))
  (jae :done)
  :loop
  (addw (:$w x8664::fixnumone) (:%w temp))
  (cmpw (:$w (:apply ash n x8664::word-shift)) (:%w temp))
  (pushq (:$l x8664::nil-value))
  (jne :loop)
  :done)
  

(define-x8664-vinsn save-lisp-context-no-stack-args (()
                                                     ())
  (pushq (:%q x8664::ra0))
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp)))


(define-x8664-vinsn save-lisp-context-offset (()
					      ((nbytes-pushed :s32const))
					      ((temp :u64)))
  (movq (:%q x8664::rbp) (:@ nbytes-pushed (:%q x8664::rsp)))
  (leaq (:@ nbytes-pushed (:%q x8664::rsp)) (:%q x8664::rbp))
  (movq (:% x8664::ra0) (:@ 8 (:%q x8664::rbp))))

(define-x8664-vinsn save-lisp-context-variable-arg-count (()
                                                          ()
                                                          ((temp :u64)))
  (movzwl (:%w x8664::nargs) (:%l temp))
  (subq (:$b (* $numx8664argregs x8664::node-size)) (:%q temp))
  (jle :push)
  (movq (:%q x8664::rbp) (:@ (:%q x8664::rsp) (:%q temp)))
  (leaq (:@ (:%q x8664::rsp) (:%q temp)) (:%q x8664::rbp))
  (movq (:% x8664::ra0) (:@ 8 (:%q x8664::rbp)))
  (jmp :done)
  :push
  (pushq (:%q x8664::ra0))
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp))
  :done)

;;; We know that some args were pushed, but don't know how many were
;;; passed.
(define-x8664-vinsn save-lisp-context-in-frame (()
                                                ()
                                                ((temp :u64)))
  (movzwl (:%w x8664::nargs) (:%l temp))
  (subq (:$b (* $numx8664argregs x8664::node-size)) (:%q temp))
  (movq (:%q x8664::rbp) (:@ (:%q x8664::rsp) (:%q temp)))
  (leaq (:@ (:%q x8664::rsp) (:%q temp)) (:%q x8664::rbp))
  (movq (:% x8664::ra0) (:@ 8 (:%q x8664::rbp))))


(define-x8664-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (pushq (:% reg)))

(define-x8664-vinsn (vpush-fixnum :push :node :vsp)
    (()
     ((const :s32const)))
  ((:and  (:pred < const 128) (:pred > const 127))
   (pushq (:$b const)))
  ((:not (:and  (:pred < const 128) (:pred > const 127)))
   (pushq (:$l const))))



(define-x8664-vinsn vframe-load (((dest :lisp))
				 ((frame-offset :u16const)
				  (cur-vsp :u16const)))
  (movq (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp)) (:%q dest)))


(define-x8664-vinsn lcell-load (((dest :lisp))
				((cell :lcell)
				 (top :lcell)))
  (movq (:@ (:apply - (:apply + (:apply calc-lcell-offset cell) x8664::word-size-in-bytes)) (:%q x8664::rbp)) (:%q dest)))

(define-x8664-vinsn (vframe-push :push :node :vsp)
    (()
     ((frame-offset :u16const)
      (cur-vsp :u16const)))
  (pushq (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))

(define-x8664-vinsn vframe-store (()
				 ((src :lisp)
                                  (frame-offset :u16const)
				  (cur-vsp :u16const)))
  (movq (:%q src) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))

(define-x8664-vinsn lcell-store (()
				 ((src :lisp)
				  (cell :lcell)
				  (top :lcell)))
  (movq (:%q src) (:@ (:apply - (:apply + (:apply calc-lcell-offset cell) x8664::word-size-in-bytes)) (:%q x8664::rbp))))
        
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

(define-x8664-vinsn compare-to-nil (()
                                    ((arg0 t)))
  (cmpb (:$b x8664::fulltag-nil) (:%b arg0)))


(define-x8664-vinsn ref-constant (((dest :lisp))
                                  ((lab :label)))
  (movq (:@ (:^ lab) (:%q x8664::fn)) (:%q dest)))

(define-x8664-vinsn (vpush-constant :push :node :vsp) (()
                                                       ((lab :label)))
  (pushq (:@ (:^ lab) (:%q x8664::fn))))

  
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
   (xorl (:%l dest) (:%l dest)))
  ((:and (:pred /= intval 0)
         (:pred >= intval  -2147483648)
         (:pred <= intval 2147483647))
   (movq (:$l intval) (:%q dest)))
  ((:or (:pred < intval  -2147483648)
        (:pred > intval 2147483647))
   (movq (:$q (:apply logand #xffffffffffffffff intval)) (:%q dest))))


(define-x8664-vinsn trap-unless-bit (()
                                     ((value :lisp)))
                                     
  (testq (:$l (lognot x8664::fixnumone)) (:%q value))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q value) (:$ub arch::error-object-not-bit))
  :ok
  )

(define-x8664-vinsn trap-unless-list (()
				      ((object :lisp))
				      ((tag :u8)))
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-list) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-list (:%q object))
  :ok)

(define-x8664-vinsn trap-unless-cons (()
				      ((object :lisp))
				      ((tag :u8)))
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::fulltagmask) (:%b tag))
  (cmpb (:$b x8664::fulltag-cons) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::fulltag-cons))
  :ok)

(define-x8664-vinsn trap-unless-uvector (()
                                         ((object :lisp))
                                         ((tag :u8)))
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jz.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::tag-misc))
  :ok)
  
(define-x8664-vinsn trap-unless-single-float (()
                                              ((object :lisp)))
  (cmpb (:$b x8664::tag-single-float) (:%b object))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::tag-single-float))
  :ok)

(define-x8664-vinsn trap-unless-character (()
                                              ((object :lisp)))
  (cmpb (:$b x8664::subtag-character) (:%b object))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-character))
  :ok)

(define-x8664-vinsn trap-unless-fixnum (()
                                        ((object :lisp))
                                        ())
  (testb (:$b x8664::tagmask) (:%b object))
  (je.pt :ok)
  (uuo-error-reg-not-fixnum (:%q object))
  :ok)

(define-x8664-vinsn set-flags-from-lisptag (()
                                            ((reg :lisp)))
  (testb (:$b x8664::tagmask) (:%b reg)))
                                            

(define-x8664-vinsn trap-unless-typecode= (()
					   ((object :lisp)
					    (tagval :u16const))
					   ((tag :u8)))
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q object)) (:%b tag))
  :have-tag
  (cmpb (:$b tagval) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub tagval))
  :ok)

(define-x8664-vinsn trap-unless-double-float (()
                                              ((object :lisp))
                                              ((tag :u8)))
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q object)) (:%b tag))
  :have-tag
  (cmpb (:$b x8664::subtag-double-float) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-double-float))
  :ok)

(define-x8664-vinsn trap-unless-macptr (()
                                        ((object :lisp))
                                        ((tag :u8)))
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q object)) (:%b tag))
  :have-tag
  (cmpb (:$b x8664::subtag-macptr) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-macptr))
  :ok)


(define-x8664-vinsn check-misc-bound (()
				      ((idx :imm)
				       (v :lisp))
				      ((temp :u64)))
  (movq (:@ x8664::misc-header-offset (:%q v)) (:%q temp))
  (shrq (:$ub x8664::num-subtag-bits) (:%q temp))
  (shlq (:$ub x8664::word-shift) (:%q temp))
  (rcmpq (:%q idx) (:%q temp))
  (jb.pt :ok)
  (uuo-error-vector-bounds (:%q idx) (:%q v))
  :ok)



(define-x8664-vinsn %cdr (((dest :lisp))
			  ((src :lisp)))
  (movq (:@ x8664::cons.cdr (:%q src)) (:%q dest)))

(define-x8664-vinsn (%vpush-cdr :push :node :vsp)
    (()
     ((src :lisp)))
  (pushq (:@ x8664::cons.cdr (:%q src))))

(define-x8664-vinsn %car (((dest :lisp))
			  ((src :lisp)))
  (movq (:@ x8664::cons.car (:%q src)) (:%q dest)))

(define-x8664-vinsn (%vpush-car :push :node :vsp)
    (()
     ((src :lisp)))
  (pushq (:@ x8664::cons.car (:%q src))))


(define-x8664-vinsn u32->char (((dest :lisp)
                               (src :u8))
			      ((src :u8))
			      ())
  (shll (:$ub x8664::charcode-shift) (:%l src))
  (leaq  (:@ x8664::subtag-character (:%q src)) (:%q dest)))


(define-x8664-vinsn (load-nil :constant-ref) (((dest t))
					      ())
  (movl (:$l x8664::nil-value) (:%l dest)))


(define-x8664-vinsn (load-t :constant-ref) (((dest t))
					    ())
  (movl(:$l x8664::t-value) (:%l dest)))


(define-x8664-vinsn extract-tag (((tag :u8))
                                 ((object :lisp)))
  (movzbl (:%b object) (:%l tag))
  (andb (:$b x8664::tagmask) (:%b tag)))

(define-x8664-vinsn extract-tag-fixnum (((tag :imm))
					((object :lisp)))
  (leal (:@ (:%q object) 8) (:%l tag))
  (andl (:$b (ash x8664::tagmask x8664::fixnumshift)) (:%l tag)))

(define-x8664-vinsn extract-fulltag (((tag :u8))
                                 ((object :lisp)))
  (movzbl (:%b object) (:%l tag))
  (andb (:$b x8664::fulltagmask) (:%b tag)))

(define-x8664-vinsn extract-fulltag-fixnum (((tag :imm))
                                            ((object :lisp)))
  (leal (:@ (:%q object) 8) (:%l tag))
  (andl (:$b (ash x8664::fulltagmask x8664::fixnumshift)) (:%l tag)))

(define-x8664-vinsn extract-typecode (((tag :u32))
                                      ((object :lisp)))
  (movzbl (:%b object) (:%l tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q object)) (:%b tag))
  :have-tag)

(define-x8664-vinsn extract-typecode-fixnum (((tag :imm))
                                             ((object :lisp))
                                             ((temp :u32)))
  (movzbl (:%b object) (:%l temp))
  (andb (:$b x8664::tagmask) (:%b temp))
  (cmpb (:$b x8664::tag-misc) (:%b temp))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q object)) (:%b temp))
  :have-tag
  (leal (:@ (:%q temp) 8) (:%l tag)))


(define-x8664-vinsn compare-reg-to-zero (()
                                         ((reg :imm)))
  (testq (:%q reg) (:%q reg)))

(define-x8664-vinsn compare-u8-reg-to-zero (()
                                            ((reg :u8)))
  (testb (:%b reg) (:%b reg)))

(define-x8664-vinsn cr-bit->boolean (((dest :lisp))
                                     ((crbit :u8const))
                                     ((temp :u32)))
  (movl (:$l x8664::t-value) (:%l temp))
  (leaq (:@ (- x8664::t-offset) (:%q temp)) (:%q dest))
  (cmovccl (:$ub crbit) (:%l temp) (:%l dest)))



(define-x8664-vinsn compare-s32-constant (()
                                            ((val :imm)
                                             (const :s32const)))
  ((:or  (:pred < const -128) (:pred > const 127))
   (rcmpq (:%q val) (:$l const)))
  ((:not (:or  (:pred < const -128) (:pred > const 127)))
   (rcmpq (:%q val) (:$b const))))

(define-x8664-vinsn compare-u31-constant (()
                                          ((val :u64)
                                           (const :u32const)))
  ((:pred > const 127)
   (rcmpq (:%q val) (:$l const)))
  ((:not (:pred > const 127))
   (rcmpq (:%q val) (:$b const))))

(define-x8664-vinsn compare-u8-constant (()
                                         ((val :u8)
                                          (const :u8const)))
  #|
  ((:pred logbitp 7 const)
   (movzbl (:%b val) (:%l val))
   (rcmpw (:%w val) (:$w const)))
  ((:not (:pred logbitp 7 const))
   (rcmpb (:%b val) (:$b const)))
  ||#
  (rcmpb (:%b val) (:$b const))
  )


(define-x8664-vinsn cons (((dest :lisp))
                          ((car :lisp)
                           (cdr :lisp)))
  (subq (:$b (- x8664::cons.size x8664::fulltag-cons)) (:@ (:%seg :rcontext) x8664::tcr.save-allocptr))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-allocptr) (:%q x8664::allocptr))
  (rcmpq (:%q x8664::allocptr) (:@ (:%seg :rcontext) x8664::tcr.save-allocbase))
  (jg :no-trap)
  (uuo-alloc)
  :no-trap
  (andb (:$b (lognot x8664::fulltagmask)) (:@ (:%seg :rcontext) x8664::tcr.save-allocptr))
  (movq (:%q car) (:@ x8664::cons.car (:%q x8664::allocptr)))
  (movq (:%q cdr) (:@ x8664::cons.cdr (:%q x8664::allocptr)))
  (movq (:%q x8664::allocptr) (:%q dest)))

(define-x8664-vinsn unbox-u8 (((dest :u8))
			      ((src :lisp)))
  (movq (:$l (lognot (ash #xff x8664::fixnumshift))) (:%q dest))
  (andq (:% src) (:% dest))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-8))
  :ok
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn %unbox-u8 (((dest :u8))
			      ((src :lisp)))
  (movl (:%l src) (:%l dest))
  (shrl (:$ub x8664::fixnumshift) (:%l dest))
  (movzbl (:%b dest) (:%l dest)))

(define-x8664-vinsn unbox-s8 (((dest :s8))
			      ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (shlq (:$ub (- x8664::nbits-in-word (+ 8 x8664::fixnumshift))) (:%q dest))
  (sarq (:$ub (- x8664::nbits-in-word (+ 8 x8664::fixnumshift))) (:%q dest))
  (cmpq (:%q src) (:%q dest))
  (jne.pn :bad)
  (testb (:$b x8664::fixnummask) (:%b dest))
  (jne.pn :bad)
  (sarq (:$ub x8664::fixnumshift) (:%q dest))
  (jmp :got-it)
  :bad
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-8))
  :got-it)

(define-x8664-vinsn unbox-u16 (((dest :u16))
			      ((src :lisp)))
  (testq (:$l (lognot (ash #xffff x8664::fixnumshift))) (:% src))
  (movq (:%q src) (:%q dest))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-16))
  :ok
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-s16 (((dest :s16))
			      ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (shlq (:$ub (- x8664::nbits-in-word (+ 16 x8664::fixnumshift))) (:%q dest))
  (sarq (:$ub (- x8664::nbits-in-word (+ 16 x8664::fixnumshift))) (:%q dest))
  (cmpq (:%q src) (:%q dest))
  (jne.pn :bad)
  (testb (:$b x8664::fixnummask) (:%b dest))
  (je.pt :got-it)
  :bad
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-16))
  :got-it
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-u32 (((dest :u32))
			      ((src :lisp)))
  (movq (:$q (lognot (ash #xffffffff x8664::fixnumshift))) (:%q dest))
  (testq (:% src) (:% dest))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-32))
  :ok
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-s32 (((dest :s32))
                               ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (shlq (:$ub (- x8664::nbits-in-word (+ 32 x8664::fixnumshift))) (:%q dest))
  (sarq (:$ub (- x8664::nbits-in-word (+ 32 x8664::fixnumshift))) (:%q dest))
  (cmpq (:%q src) (:%q dest))
  (jne.pn :bad)
  (testb (:$b x8664::fixnummask) (:%b dest))
  (je.pt :got-it)
  :bad
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-32))
  :got-it
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-u64 (((dest :u64))
                               ((src :lisp)))
  (movq (:$q (lognot (ash x8664::target-most-positive-fixnum x8664::fixnumshift))) (:%q dest))
  (testq (:%q dest) (:%q src))
  (movq (:%q src) (:%q dest))
  (jnz :maybe-bignum)
  (sarq (:$ub x8664::fixnumshift) (:%q dest))
  (jmp :done)
  :maybe-bignum
  (andb (:$b x8664::tagmask) (:%b dest))
  (cmpb (:$b x8664::tag-misc) (:%b dest))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q src)) (:%b dest))
  :have-tag
  (cmpb (:$b x8664::subtag-bignum) (:%b dest))
  (jne :bad)
  (movq (:@ x8664::misc-header-offset (:%q src)) (:%q dest))
  (cmpq (:$l x8664::three-digit-bignum-header) (:%q dest))
  (je :three)
  (cmpq (:$l x8664::two-digit-bignum-header) (:%q dest))
  (jne :bad)
  (movq (:@ x8664::misc-data-offset (:%q src)) (:%q dest))
  (testq (:%q dest) (:%q dest))
  (jns :done)
  :bad
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-64))
  :three
  (movl (:@ (+ 8 x8664::misc-data-offset) (:%q src)) (:%l dest))
  (testl (:%l dest) (:%l dest))
  (movq (:@ x8664::misc-data-offset (:%q src)) (:%q dest))
  (jne :bad)
  :done)

(define-x8664-vinsn unbox-s64 (((dest :s64))
                               ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest))
  ;; Was it a fixnum ?
  (testb (:$b x8664::fixnummask) (:%b src))
  (je :done)
  ;; May be a 2-digit bignum
  (movb (:%b src) (:%b dest))
  (andb (:$b x8664::tagmask) (:%b dest))
  (cmpb (:$b x8664::tag-misc) (:%b dest))
  (jne :bad)
  (cmpq (:$l x8664::two-digit-bignum-header) (:@ x8664::misc-header-offset (:%q src)))
  (movq (:@ x8664::misc-data-offset (:%q src)) (:%q dest))
  (je :done)
  :bad
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-64))
  :done)

(define-x8664-vinsn (jump-subprim :jumpLR) (()
					    ((spno :s32const)))
  (jmp (:@ spno)))

(define-x8664-vinsn call-subprim (()
                                  ((spno :s32const)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ spno))
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:% x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn fixnum-subtract-from (((dest t)
                                           (y t))
                                          ((y t)
                                           (x t)))
  (subq (:%q y) (:%q x)))

(define-x8664-vinsn %logand-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (andq (:$b const) (:%q val)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (andq (:$l const) (:%q val))))

(define-x8664-vinsn %logior-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (orq (:$b const) (:%q val)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (orq (:$l const) (:%q val))))

(define-x8664-vinsn %logxor-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (xorq (:$b const) (:%q val)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (xorq (:$l const) (:%q val))))

(define-x8664-vinsn character->fixnum (((dest :lisp))
				       ((src :lisp))
				       ())
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movq (:%q src) (:%q dest)))
  (shrq (:$ub (- x8664::charcode-shift x8664::fixnumshift)) (:%q dest)))

(define-x8664-vinsn compare (()
                             ((x t)
                              (y t)))
  (rcmpq (:%q x) (:%q y)))

(define-x8664-vinsn negate-fixnum (((val :lisp))
                                   ((val :imm)))
  (negq (:% val)))

;;; This handles the 1-bit overflow from addition/subtraction/unary negation
(define-x8664-vinsn set-bigits-and-header-for-fixnum-overflow
    (()
     ((val :lisp)
      (no-overflow
       :label))
     ((header (:u64 #.x8664::imm0))
      (scaled-size (:u64 #.x8664::imm1))))
  (jno.pt no-overflow)
  (movq (:%q val) (:%q scaled-size))
  (sarq (:$ub x8664::fixnumshift) (:%q scaled-size))
  (movq (:$q #xe000000000000000) (:%q header))
  (xorq (:%q header) (:%q scaled-size))
  (movd (:%q scaled-size) (:%mmx x8664::mm0))
  (movq (:$l x8664::two-digit-bignum-header) (:%q header))
  (movq (:$l (- 16 x8664::fulltag-misc)) (:%q scaled-size)))

(define-x8664-vinsn %set-z-flag-if-s64-fits-in-fixnum (((dest :imm))
                                                       ((src :s64))
                                                       ((temp :s64)))
  (movq (:%q src) (:%q temp))
  (shlq (:$ub x8664::fixnumshift) (:%q temp))
  (movq (:%q temp) (:%q dest))          ; tagged as a fixnum
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (cmpq (:%q src) (:%q temp)))

(define-x8664-vinsn %set-z-flag-if-u64-fits-in-fixnum (((dest :imm))
                                                       ((src :u64))
                                                       ((temp :u64)))
  (movq (:%q src) (:%q temp))
  (shlq (:$ub (1+ x8664::fixnumshift)) (:%q temp))
  (movq (:%q temp) (:%q dest))          ; tagged as an even fixnum
  (shrq (:$ub (1+ x8664::fixnumshift)) (:%q temp))
  (shrq (:%q dest))
  (cmpq (:%q src) (:%q temp))
  :done)


(define-x8664-vinsn setup-bignum-alloc-for-s64-overflow (()
                                                         ((src :s64)))
  (movd (:%q src) (:%mmx x8664::mm0))
  (movl (:$l x8664::two-digit-bignum-header) (:%l x8664::imm0.l))
  (movl (:$l (- 16 x8664::fulltag-misc)) (:%l x8664::imm1.l)))


;;; If the sign bit is set in SRC, need to make a 3-digit bignum
;;; that requires 32 bytes of aligned memory
(define-x8664-vinsn setup-bignum-alloc-for-u64-overflow (()
                                                         ((src :s64)))
  (testq (:%q src) (:%q src))
  (movd (:%q src) (:%mmx x8664::mm0))
  (movl (:$l x8664::two-digit-bignum-header) (:%l x8664::imm0.l))
  (movl (:$l (- 16 x8664::fulltag-misc)) (:%l x8664::imm1.l))
  (jns :done)
  (movl (:$l x8664::three-digit-bignum-header) (:%l x8664::imm0.l))
  (movl (:$l (- 32 x8664::fulltag-misc)) (:%l x8664::imm1.l))
  :done)
  
  

(define-x8664-vinsn %allocate-uvector (((dest :lisp))
                                       ()
                                       ((header (:u64 #.x8664::imm0))
                                        (freeptr (:lisp #.x8664::allocptr))))
  (subq (:%q x8664::imm1) (:@ (:%seg :rcontext) x8664::tcr.save-allocptr))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-allocptr) (:%q freeptr))
  (rcmpq (:%q freeptr) (:@ (:%seg :rcontext) x8664::tcr.save-allocbase))
  (jg :no-trap)
  (uuo-alloc)
  :no-trap
  (movq (:%q header) (:@ x8664::misc-header-offset (:%q freeptr)))
  (andb (:$b (lognot x8664::fulltagmask)) (:@ (:%seg :rcontext) x8664::tcr.save-allocptr))
  ((:not (:pred = freeptr
                (:apply %hard-regspec-value dest)))
   (movq (:%q freeptr) (:%q dest))))

(define-x8664-vinsn set-bigits-after-fixnum-overflow (()
                                                      ((bignum :lisp)))
  (movq (:%mmx x8664::mm0) (:@ x8664::misc-data-offset (:%q bignum))))
  
                                                       
(define-x8664-vinsn box-fixnum (((dest :imm))
                                ((src :s8)))
  (imulq (:$b x8664::fixnumone) (:%q src)(:%q dest)))


(define-x8664-vinsn fix-fixnum-overflow-ool (((val :lisp))
                                             ((val :lisp))
                                             ((unboxed (:s64 #.x8664::imm1))
                                              (header (:u64 #.x8664::imm0))))
  (jno.pt :done)
  ((:not (:pred = x8664::arg_z
                (:apply %hard-regspec-value val)))
   (movq (:%q val) (:%q x8664::arg_z)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPfix-overflow))
  (:align 3)
  (:long (:^ :back))
  :back
  ;; We don't lose FN while consing the bignum.
  ((:not (:pred = x8664::arg_z
                (:apply %hard-regspec-value val)))
   (movq (:%q x8664::arg_z) (:%q val)))
  :done)

(define-x8664-vinsn fix-fixnum-overflow-ool-and-branch (((val :lisp))
                                                        ((val :lisp)
                                                         (lab :label))
                                                        ((unboxed (:s64 #.x8664::imm1))
                                                         (header (:u64 #.x8664::imm0))))
  (jno.pt lab)
  ((:not (:pred = x8664::arg_z
                (:apply %hard-regspec-value val)))
   (movq (:%q val) (:%q x8664::arg_z)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPfix-overflow))
  (:align 3)
  (:long (:^ :back))
  :back
  ;; We don't lose FN while consing the bignum.
  ((:not (:pred = x8664::arg_z
                (:apply %hard-regspec-value val)))
   (movq (:%q x8664::arg_z) (:%q val)))
  (jmp lab))

(define-x8664-vinsn add-constant (((dest :imm))
                                  ((dest :imm)
                                   (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (addq (:$b const) (:%q dest)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (addq (:$l const) (:%q dest))))

(define-x8664-vinsn add-constant3 (((dest :imm))
                                   ((src :imm)
                                    (const :s32const)))
  ((:pred = (:apply %hard-regspec-value dest)
          (:apply %hard-regspec-value src))
   ((:and (:pred >= const -128) (:pred <= const 127))
    (addq (:$b const) (:%q dest)))
   ((:not (:and (:pred >= const -128) (:pred <= const 127)))
    (addq (:$l const) (:%q dest))))
  ((:not (:pred = (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (leaq (:@ const (:%q src)) (:%q dest))))

  

(define-x8664-vinsn fixnum-add2  (((dest :imm))
                                  ((dest :imm)
                                   (other :imm)))
  (addq (:%q other) (:%q dest)))

(define-x8664-vinsn fixnum-sub2  (((dest :imm))
                                  ((x :imm)
                                   (y :imm))
                                  ((temp :imm)))
  (movq (:%q x) (:%q temp))
  (subq (:%q y) (:%q temp))
  (movq (:%q temp) (:%q dest)))



(define-x8664-vinsn fixnum-add3 (((dest :imm))
                                 ((x :imm)
                                  (y :imm)))
  
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (addq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (addq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (leaq (:@ (:%q x) (:%q y)) (:%q dest)))))
   
(define-x8664-vinsn copy-gpr (((dest t))
			      ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movq (:%q src) (:%q dest))))

(define-x8664-vinsn (vpop-register :pop :node :vsp)
    (((dest :lisp))
     ())
  (popq (:%q dest)))

                                           
(define-x8664-vinsn (push-argregs :push :node :vsp) (()
                                                      ())
  (testw (:%w x8664::nargs) (:%w x8664::nargs))
  (jz :done)
  (rcmpw (:%w x8664::nargs) (:$w (* 2 x8664::node-size)))
  (jb :one)
  (je :two)
  (pushq (:%q x8664::arg_x))
  :two
  (pushq (:%q x8664::arg_y))
  :one
  (pushq (:%q x8664::arg_z))
  :done)

(define-x8664-vinsn (push-max-argregs :push :node :vsp) (()
                                                         ((max :u32const)))
  ((:pred >= max 3)
   (testw (:%w x8664::nargs) (:%w x8664::nargs))
   (jz :done)
   (rcmpw (:%w x8664::nargs) (:$w (* 2 x8664::node-size)))
   (jb :one)
   (je :two)
   (pushq (:%q x8664::arg_x))
   :two
   (pushq (:%q x8664::arg_y))
   :one
   (pushq (:%q x8664::arg_z))
   :done)
  ((:pred = max 2)
   (rcmpw (:%w x8664::nargs) (:$w (* 1 x8664::node-size)))
   (jb :done)
   (je :one)
   (pushq (:%q x8664::arg_y))
   :one
   (pushq (:%q x8664::arg_z))
   :done)
  ((:pred = max 1)
   (testw (:%w x8664::nargs) (:%w x8664::nargs))
   (je :done)
   (pushq (:%q x8664::arg_z))
   :done))

(define-x8664-vinsn (call-label :call) (()
					((label :label)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp label)
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:% x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn double-float-compare (()
					  ((arg0 :double-float)
					   (arg1 :double-float)))
  (comisd (:%xmm arg1) (:%xmm arg0)))

(define-x8664-vinsn single-float-compare (()
					  ((arg0 :single-float)
					   (arg1 :single-float)))
  (comiss (:%xmm arg1) (:%xmm arg0)))
              

(define-x8664-vinsn double-float+-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (addsd (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (addsd (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movsd (:%xmm x) (:%xmm result))
   (addsd (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8664-vinsn double-float--2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movsd (:%xmm x) (:%xmm result)))
  (subsd (:%xmm y) (:%xmm result)))

(define-x8664-vinsn double-float*-2 (((result :single-float))
				     ((x :single-float)
                                      (y :single-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (mulsd (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (mulsd (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movsd (:%xmm x) (:%xmm result))
   (mulsd (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8664-vinsn double-float/-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movsd (:%xmm x) (:%xmm result)))
  (divsd (:%xmm y) (:%xmm result)))

(define-x8664-vinsn single-float+-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (addss (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (addss (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movss (:%xmm x) (:%xmm result))
   (addss (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8664-vinsn single-float--2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movss (:%xmm x) (:%xmm result)))
  (subss (:%xmm y) (:%xmm result)))

(define-x8664-vinsn single-float*-2 (((result :single-float))
				     ((x :single-float)
                                      (y :single-float)))
    ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (mulss (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (mulss (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movss (:%xmm x) (:%xmm result))
   (mulss (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8664-vinsn single-float/-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movss (:%xmm x) (:%xmm result)))
  (divss (:%xmm y) (:%xmm result)))

(define-x8664-vinsn get-single (((result :single-float))
                                ((source :lisp)))
  (movd (:%q source) (:%xmm result))
  (psrlq (:$ub 32) (:%xmm result)))

(define-x8664-vinsn get-double (((result :double-float))
                                ((source :lisp)))
  (movsd (:@  x8664::double-float.value (:%q source)) (:%xmm result)))

;;; Extract a double-float value, typechecking in the process.
;;; IWBNI we could simply call the "trap-unless-typecode=" vinsn here,
;;; instead of replicating it ..

(define-x8664-vinsn get-double? (((target :double-float))
				 ((source :lisp))
				 ((tag :u8)))
  (movb (:%b source) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q source)) (:%b tag))
  :have-tag
  (cmpb (:$b x8664::subtag-double-float) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q source) (:$ub x8664::subtag-double-float))
  :ok
  (movsd (:@  x8664::double-float.value (:%q source)) (:%xmm target)))

(define-x8664-vinsn single->node (((result :lisp)
                                   (source :single-float))
                                  ((source :single-float)))
  (psllq (:$ub 32) (:%xmm source))
  (movd (:%xmm source) (:%q result))
  (movb (:$b x8664::tag-single-float) (:%b result)))

(define-x8664-vinsn copy-double-float (((dest :double-float))
                                       ((src :double-float)))
  (movsd (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn copy-single-float (((dest :single-float))
                                       ((src :single-float)))
  (movss (:%xmm src) (:%xmm dest)))


(define-x8664-vinsn copy-single-to-double (((dest :double-float))
                                           ((src :single-float)))
  (cvtss2sd (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn copy-double-to-single (((dest :single-float))
                                           ((src :double-float)))
  (cvtsd2ss (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn u8->fixnum (((result :imm)) 
				((val :u8)) 
				())
  (leaq (:@ (:%q val) 8) (:%q result)))

(define-x8664-vinsn fitvals (()
                             ((n :u16const))
                             ((imm :u16)))
  ((:pred = n 0)
   (xorq (:%q imm) (:%q imm)))
  ((:not (:pred = n 0))
   (movw (:$w (:apply ash n x8664::fixnumshift)) (:%w imm)))
  (subw (:%w x8664::nargs) (:%w imm))
  (jae :push-more)
  (movswq (:%w imm) (:%q imm))
  (subq (:%q imm) (:%q x8664::rsp))
  (jmp :done)
  :push-loop
  (pushq (:$l x8664::nil-value))
  (addw (:$b x8664::node-size) (:%w x8664::nargs))
  (subw (:$b x8664::node-size) (:%w imm))
  :push-more
  (jne :push-loop)
  :done)
  
(define-x8664-vinsn (nvalret :jumpLR) (()
                                       ())
  
  (jmp (:@ .SPnvalret)))


(define-x8664-vinsn lisp-word-ref (((dest t))
				   ((base t)
				    (offset t)))
  (movq (:@ (:%q base) (:%q offset)) (:%q  dest)))


(define-x8664-vinsn lisp-word-ref-c (((dest t))
				     ((base t)
				      (offset :s32const)))
  ((:pred = offset 0)
   (movq (:@ (:%q base)) (:%q dest)))
  ((:not (:pred = offset 0))
   (movq (:@ offset (:%q base)) (:%q dest))))


(define-x8664-vinsn start-mv-call (()
                                   ((label :label)))
  (leaq (:@ (:^ label) (:%q x8664::fn)) (:%q x8664::ra0))
  (pushq (:%q x8664::ra0)))


(define-x8664-vinsn emit-aligned-label (()
                                        ((label :label)))
  (:align 3)
  (:long (:^ label)))

;;; %ra0 is pointing into %fn, so no need to copy %fn here.
(define-x8664-vinsn pass-multiple-values-symbol (()
                                                 ())
  (movq (:@ x8664::symbol.fcell (:% x8664::fname)) (:%q x8664::fn))
  (movq (:@ (+ x8664::nil-value (x8664::%kernel-global 'x86::ret1valaddr)))
        (:%q x8664::ra0))
  (jmp (:%q x8664::fn)))

;;; It'd be good to have a variant that deals with a known function
;;; as well as this. 
(define-x8664-vinsn pass-multiple-values (()
                                          ()
                                          ((tag :u8)))
  (movb (:%b x8664::temp0) (:%b tag))
  (andb (:$b x8664::fulltagmask) (:%b tag))
  (cmpb (:$b x8664::fulltag-symbol) (:%b tag))
  (cmovgq (:%q x8664::temp0) (:%q x8664::fn))
  (jl :bad)
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::fn))
  (movq (:@ (+ x8664::nil-value (x8664::%kernel-global 'x86::ret1valaddr)))
        (:%q x8664::ra0))
  (jmp (:%q x8664::fn))
  :bad
  (uuo-error-not-callable)
  ;; If we don't do this (and leave %fn as a TRA into itself), reporting
  ;; the error is likely a little harder.  Tough.
  ;; (leaq (@ (:apply - (:^ :bad)) (:%q x8664::rn)) (:%q x8664::fn))
)



(define-x8664-vinsn reserve-outgoing-frame (()
                                            ())
  (pushq (:$b x8664::reserved-frame-marker))
  (pushq (:$b x8664::reserved-frame-marker)))


(define-x8664-vinsn (call-known-function :call) (()
						 ())
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (movq (:%q x8664::temp0) (:%q x8664::fn))
  (jmp (:%q x8664::fn))
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:% x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn (jump-known-function :jumplr) (()
                                                   ())
  (Movq (:%q x8664::fn) (:%q x8664::xfn))
  (movq (:%q x8664::temp0)  (:%q x8664::fn))
  (jmp (:%q x8664::fn)))

(define-x8664-vinsn list (()
                          ())
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPconslist))
  (:align 3)
  (:long (:^ :back))
  :back)


(define-x8664-vinsn make-tsp-cons (((dest :lisp))
				   ((car :lisp) (cdr :lisp))
				   ((temp :imm)))
  (subq (:$b (+ x8664::cons.size x8664::dnode-size)) (:@ (:%seg :rcontext) x8664::tcr.next-tsp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.next-tsp) (:%q temp))
  (movapd (:%xmm x8664::fpzero) (:@ (:%q temp)))
  (movapd (:%xmm x8664::fpzero) (:@ 16 (:%q temp)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-tsp) (:%mmx x8664::stack-temp))
  (movq (:%mmx x8664::stack-temp) (:@ (:%q temp)))
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.save-tsp))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-cons) (:%q temp)) (:%q temp))
  (movq (:%q car) (:@ x8664::cons.car (:%q temp)))
  (movq (:%q cdr) (:@ x8664::cons.cdr (:%q temp)))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn make-fixed-stack-gvector (((dest :lisp))
                                              ((aligned-size :u32const)
                                               (header :s32const))
                                              ((tempa :imm)
                                               (tempb :imm)))
  ((:and (:pred >= (:apply + aligned-size x8664::dnode-size) -128)
         (:pred <= (:apply + aligned-size x8664::dnode-size) 127))
   (subq (:$b (:apply + aligned-size x8664::dnode-size))
         (:@ (:%seg :rcontext) x8664::tcr.next-tsp)))
  ((:not (:and (:pred >= (:apply + aligned-size x8664::dnode-size) -128)
               (:pred <= (:apply + aligned-size x8664::dnode-size) 127)))
   (subq (:$l (:apply + aligned-size x8664::dnode-size))
         (:@ (:%seg :rcontext) x8664::tcr.next-tsp)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-tsp) (:%q tempb))
  (movq (:@ (:%seg :rcontext) x8664::tcr.next-tsp) (:%q tempa))
  (movd (:%q tempb) (:%mmx x8664::stack-temp))
  :loop
  (movapd (:%xmm x8664::fpzero) (:@ -16 (:%q tempb)))
  (subq (:$b x8664::dnode-size) (:%q tempb))
  (cmpq (:%q tempa) (:%q tempb))
  (jnz :loop)
  (movq (:%mmx x8664::stack-temp) (:@ (:%q tempa)))
  (movq (:%q tempa) (:@ (:%seg :rcontext) x8664::tcr.save-tsp))
  (movl (:$l header) (:@ x8664::dnode-size (:%q tempa)))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-misc) (:%q tempa)) (:%q dest)))


(define-x8664-vinsn discard-temp-frame (()
					()
                                        ((temp :imm)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-tsp) (:%q temp))
  (movq (:@ (:%q temp)) (:%q temp))
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.save-tsp))
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.next-tsp))
  )

(define-x8664-vinsn discard-c-frame (()
                                     ()
                                     ((temp :imm)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q temp))
  (movq (:@ (:%q temp)) (:%q temp))
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp)))

  
(define-x8664-vinsn vstack-discard (()
				    ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   ((:pred < nwords 16)
    (addq (:$b (:apply ash nwords x8664::word-shift)) (:%q x8664::rsp)))
   ((:not (:pred < nwords 16))
    (addq (:$l (:apply ash nwords x8664::word-shift)) (:%q x8664::rsp)))))


(defmacro define-x8664-subprim-call-vinsn ((name &rest other-attrs) spno &optional (recover-fn nil))
  `(define-x8664-vinsn (,name :call :subprim-call ,@other-attrs) (() ())
    (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
    (jmp (:@ ,spno))
    (:align 3)
    (:long (:^ :back))
    :back
    ,@(if recover-fn
          `((leaq (:@ (:apply - (:^ :back)) (:%q x8664::ra0)) (:%q x8664::fn))))))

(defmacro define-x8664-subprim-jump-vinsn ((name &rest other-attrs) spno)
  `(define-x8664-vinsn (,name :jump :jumpLR ,@other-attrs) (() ())
    (jmp (:@ ,spno))))

(define-x8664-vinsn (nthrowvalues :call :subprim-call) (()
                                                        ((lab :label)))
  (leaq (:@ (:^ lab) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPnthrowvalues)))

(define-x8664-vinsn (nthrow1value :call :subprim-call) (()
                                                        ((lab :label)))
  (leaq (:@ (:^ lab) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPnthrow1value)))


(define-x8664-subprim-call-vinsn (bind-interrupt-level-0) .SPbind-interrupt-level-0)

(define-x8664-subprim-call-vinsn (bind-interrupt-level-m1) .SPbind-interrupt-level-m1)

(define-x8664-subprim-call-vinsn (bind-interrupt-level) .SPbind-interrupt-level)

(define-x8664-subprim-call-vinsn (unbind-interrupt-level) .SPunbind-interrupt-level)

(define-x8664-vinsn (jump-return-pc :jumpLR)
    (()
     ())
  (jmp (:%q x8664::ra0)))

(define-x8664-vinsn (mkcatchmv :call :subprim-call) (()
                                                     ((lab :label)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (leaq (:@ (:^ lab)  (:%q x8664::fn)) (:%q x8664::xfn))
  (jmp (:@ .SPmkcatchmv))
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:%q x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn (mkcatch1v :call :subprim-call) (()
                                                     ((lab :label)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (leaq (:@ (:^ lab)  (:%q x8664::fn)) (:%q x8664::xfn))
  (jmp (:@ .SPmkcatch1v))
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:%q x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn (mkunwind :call :subprim-call) (()
                                                     ((protform-lab :label)
                                                      (cleanup-lab :label)))
  (leaq (:@ (:^ protform-lab) (:%q x8664::fn)) (:%q x8664::ra0))
  (leaq (:@ (:^ cleanup-lab)  (:%q x8664::fn)) (:%q x8664::xfn))
  (jmp (:@ .SPmkunwind)))


(define-x8664-subprim-call-vinsn (gvector) .SPgvector)

(define-x8664-subprim-call-vinsn (getu64) .SPgetu64)

;;; Call something callable and obtain the single value that it
;;; returns.
(define-x8664-vinsn funcall (()
                             ()
                             ((tag :u8)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (movb (:%b x8664::temp0) (:%b tag))
  (andb (:$b x8664::fulltagmask) (:%b tag))
  (cmpb (:$b x8664::fulltag-symbol) (:%b tag))
  (cmovgq (:%q x8664::temp0) (:%q x8664::fn))
  (jl :bad)
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::fn))
  (jmp (:%q x8664::fn))
  :bad
  (uuo-error-not-callable)
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:%q x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn tail-funcall (()
                                  ()
                                  ((tag :u8)))
  (movq (:%q x8664::fn) (:%q x8664::xfn))
  (movb (:%b x8664::temp0) (:%b tag))
  (andb (:$b x8664::fulltagmask) (:%b tag))
  (cmpb (:$b x8664::fulltag-symbol) (:%b tag))
  (cmovgq (:%q x8664::temp0) (:%q x8664::fn))
  (jl :bad)
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::fn))
  (jmp (:%q x8664::fn))
  :bad
  (uuo-error-not-callable))
                             
  

(define-x8664-vinsn init-closure (()
                                  ((closure :lisp)))
  (movb (:$b 3) (:@ x8664::misc-data-offset (:%q closure))) ; code word count
  (movb (:$b -1) (:@ (+ x8664::misc-data-offset 7) (:%q closure))) ; 1st byte of jmp
  (movl (:$l (:apply logior #x2524 (:apply ash .SPcall-closure 16))) (:@ (+ x8664::misc-data-offset 8) (:%q closure))) ; rest of jmp instruction, low two bytes of subprim address
  ((:not (:pred = 0 (:apply ash .SPcall-closure -16)))
   (movb (:$b (:apply ash .SPcall-closure -16)) (:@ (+ x8664::misc-data-offset 12) (:%q closure))))
  (movb (:$b x8664::function-boundary-marker) (:@ (+ x8664::misc-data-offset 16)  (:%q closure))))


(define-x8664-vinsn finalize-closure (((closure :lisp))
                                      ((closure :lisp)))
  (addq (:$b (- x8664::fulltag-function x8664::fulltag-misc)) (:%q closure)))


(define-x8664-vinsn (ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPspecrefcheck))
  (:align 3)
  (:long (:^ :back))
  :back)

(define-x8664-vinsn ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  (movq (:@ x8664::symbol.binding-index (:%q src)) (:%q idx))
  (rcmpq (:%q idx) (:@ (:%seg :rcontext) x8664::tcr.tlb-limit))
  (movq (:@ (:%seg :rcontext) x8664::tcr.tlb-pointer) (:%q table))
  (jae :symbol)
  (movq (:@ (:%q table) (:%q idx)) (:%q dest))
  (cmpb (:$b x8664::subtag-no-thread-local-binding) (:%b dest))
  (jne :test)
  :symbol
  (movq (:@ x8664::symbol.vcell (:%q src)) (:%q dest))
  :test
  (cmpb (:$b x8664::unbound-marker) (:%b dest))
  (jne.pt :done)
  (uuo-error-unbound (:%q src))
  :done)


(define-x8664-vinsn (%ref-symbol-value :call :subprim-call)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPspecref))
  (:align 3)
  (:long (:^ :back))
  :back)

(define-x8664-vinsn %ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  (movq (:@ x8664::symbol.binding-index (:%q src)) (:%q idx))
  (rcmpq (:%q idx) (:@ (:%seg :rcontext) x8664::tcr.tlb-limit))
  (movq (:@ (:%seg :rcontext) x8664::tcr.tlb-pointer) (:%q table))
  (jae :symbol)
  (movq (:@ (:%q table) (:%q idx)) (:%q dest))
  (cmpb (:$b x8664::subtag-no-thread-local-binding) (:%b dest))
  (jne :done)
  :symbol
  (movq (:@ x8664::symbol.vcell (:%q src)) (:%q dest))
  :done)

(define-x8664-vinsn ref-interrupt-level (((dest :imm))
                                         ()
                                         ((temp :u64)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.tlb-pointer) (:%q temp))
  (movq (:@ x8664::INTERRUPT-LEVEL-BINDING-INDEX (:%q temp)) (:%q dest)))

(define-x8664-vinsn save-cleanup-context (()
                                          ((lab :label)))
  (leaq (:@ (:apply - (:^ lab)) (:%q x8664::xfn)) (:%q x8664::fn))
  )



(define-x8664-vinsn setup-double-float-allocation (()
                                                   ())
  (movl (:$l (arch::make-vheader x8664::double-float.element-count x8664::subtag-double-float)) (:%l x8664::imm0.l))
  (movl (:$l (- x8664::double-float.size x8664::fulltag-misc)) (:%l x8664::imm1.l)))

(define-x8664-vinsn set-double-float-value (()
                                            ((node :lisp)
                                             (val :double-float)))
  (movsd (:%xmm val) (:@ x8664::double-float.value (:%q node))))

(define-x8664-vinsn word-index-and-bitnum-from-index (((word-index :u64)
                                                       (bitnum :u8))
                                                      ((index :imm)))
  (movq (:%q index) (:%q word-index))
  (shrq (:$ub x8664::fixnumshift) (:%q word-index))
  (movl (:$l 63) (:%l bitnum))
  (andl (:%l word-index) (:%l bitnum))
  (shrq (:$ub 6) (:%q word-index)))

(define-x8664-vinsn ref-bit-vector-fixnum (((dest :imm)
                                            (bitnum :u8))
                                           ((bitnum :u8)
                                            (bitvector :lisp)
                                            (word-index :u64)))
  (btq (:%q bitnum) (:@ x8664::misc-data-offset (:%q bitvector) (:%q word-index) 8))
  (setb (:%b bitnum))
  (negb (:%b bitnum))
  (andl (:$l x8664::fixnumone) (:%l bitnum))
  (movl (:%l bitnum) (:%l dest)))
                                            
                                                      
(define-x8664-vinsn misc-ref-c-bit-fixnum (((dest :imm))
                                           ((src :lisp)
                                            (idx :u64const))
                                           ((temp :u8)))
  (btq (:$ub (:apply logand 63 idx))
       (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src)))
  (setb (:%b temp))
  (negb (:%b temp))
  (andl (:$l x8664::fixnumone) (:%l temp))
  (movl (:%l temp) (:%l dest)))

(define-x8664-vinsn deref-macptr (((addr :address))
				  ((src :lisp))
				  ())
  (movq (:@ x8664::macptr.address (:%q src)) (:%q addr)))

(define-x8664-vinsn (temp-push-unboxed-word :push :word :csp)
    (()
     ((w :u64)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%mmx x8664::stack-temp))  
  (subq (:$b 16) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))  
  (movq (:%mmx x8664::stack-temp) (:@ (:%q x8664::ra0)))
  (movq (:%q w) (:@ 8 (:%q x8664::ra0))))


(define-x8664-vinsn (temp-push-node :push :word :tsp)
        (()
         ((w :lisp))
         ((temp :imm)))
  (subq (:$b (* 2 x8664::dnode-size)) (:@ (:%seg :rcontext) x8664::tcr.next-tsp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-tsp) (:%mmx x8664::stack-temp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.next-tsp) (:%q temp))
  (movapd (:%xmm x8664::fpzero) (:@ (:%q temp)))
  (movapd (:%xmm x8664::fpzero) (:@ 16 (:%q temp)))
  (movq (:%mmx x8664::stack-temp) (:@ (:%q temp)))
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.save-tsp))
  (movq (:%q w) (:@ x8664::dnode-size (:%q temp))))

(define-x8664-vinsn (temp-push-double-float :push :word :csp)
    (()
     ((f :double-float)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%mmx x8664::stack-temp))  
  (subq (:$b 16) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))  
  (movq (:%mmx x8664::stack-temp) (:@ (:%q x8664::ra0)))
  (movsd (:%xmm f) (:@ 8 (:%q x8664::ra0))))


(define-x8664-vinsn (vpush-single-float :push :word :vsp)
    (()
     ((f :single-float)))
  (pushq (:$b x8664::tag-single-float))
  (movss (:%xmm f) (:@ 4 (:%q x8664::rsp))))

(define-x8664-vinsn (vpop-single-float :pop :word :vsp)
    (()
     ((f :single-float)))
  (movss (:@ 4 (:%q x8664::rsp)) (:%xmm f))
  (addq (:$b x8664::node-size) (:%q x8664::rsp)))

(define-x8664-vinsn (temp-pop-unboxed-word :pop :word :csp)
    (((w :u64))
     ())
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movq (:@ 8 (:%q x8664::ra0)) (:%q w))
  (addq (:$b 16) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp)))


(define-x8664-vinsn (temp-pop-node :pop :word :tsp)
        (((w :lisp))
         ()
         ((temp :imm)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-tsp) (:%q temp))
  (movq (:@ x8664::dnode-size (:%q temp)) (:%q w))
  (movq (:@ (:%q temp)) (:%q temp))
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.save-tsp))  
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.next-tsp)))

(define-x8664-vinsn (temp-pop-double-float :pop :word :csp)
    (((f :double-float))
     ())
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movsd (:@ 8 (:%q x8664::ra0)) (:%xmm f))
  (addq (:$b 16) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp)))



(define-x8664-vinsn macptr->stack (((dest :lisp))
                                   ((ptr :address)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%mmx x8664::stack-temp))
  (subq (:$b (+ 16 x8664::macptr.size)) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movq (:%mmx x8664::stack-temp) (:@ (:%q x8664::ra0)))
  (leaq (:@ (+ 16 x8664::fulltag-misc) (:%q  x8664::ra0)) (:%q dest))
  (movq (:$l x8664::macptr-header) (:@ x8664::macptr.header (:%q dest)))
  (movq (:%q ptr) (:@ x8664::macptr.address (:%q dest)))
  (movapd (:%xmm x8664::fpzero)  (:@ x8664::macptr.domain (:%q dest))))

(define-x8664-vinsn fixnum->signed-natural (((dest :s64))
                                            ((src :imm)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn mem-set-double-float (()
					  ((val :double-float)
					   (src :address)
					   (index :s64)))
  (movsd (:%xmm val) (:@ (:%q src) (:%q  index))))

(define-x8664-vinsn mem-set-single-float (()
					  ((val :single-float)
					   (src :address)
					   (index :s64)))
  (movss (:%xmm val) (:@ (:%q src) (:%q  index))))



(define-x8664-vinsn mem-set-c-doubleword (()
                                          ((val :u64)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movq (:%q val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movq (:%q val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-fullword (()
                                          ((val :u32)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movl (:%l val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movl (:%l val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-halfword (()
                                          ((val :u16)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movw (:%w val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movw (:%w val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-byte (()
                                          ((val :u8)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movb (:%b val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movb (:%b val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-doubleword (()
                                                   ((val :s32const)
                                                    (dest :address)
                                                    (offset :s32const)))
  ((:pred = offset 0)
   (movq (:$l val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movq (:$l val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-fullword (()
                                                 ((val :s32const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movl (:$l val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movl (:$l val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-halfword (()
                                                 ((val :s16const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movw (:$w val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movw (:$w val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-byte (()
                                                 ((val :s8const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movb (:$b val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movb (:$b val) (:@ offset (:%q dest)))))


(define-x8664-vinsn mem-set-constant-doubleword (()
                                                   ((val :s32const)
                                                    (dest :address)
                                                    (offset :s64)))
   (movq (:$l val) (:@ (:%q dest) (:%q offset))))



(define-x8664-vinsn mem-ref-natural (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn setup-macptr-allocation (()
                                             ((src :address)))
  (movd (:%q src) (:%mmx x8664::mm0))
  (movl (:$l x8664::macptr-header) (:%l x8664::imm0.l))
  (movl (:$l (- x8664::macptr.size x8664::fulltag-misc)) (:%l x8664::imm1.l)))

(define-x8664-vinsn %set-new-macptr-value (()
                                           ((ptr :lisp)))
  (movq (:%mmx x8664::mm0) (:@ x8664::macptr.address (:%q ptr))))

(define-x8664-vinsn mem-ref-c-fullword (((dest :u32))
					((src :address)
					 (index :s32const)))
  ((:pred = index 0)
   (movl (:@ (:%q src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movl (:@ index (:%q src)) (:%l dest))))

(define-x8664-vinsn mem-ref-c-signed-fullword (((dest :s32))
                                               ((src :address)
                                                (index :s32const)))
  ((:pred = index 0)
   (movslq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movslq (:@ index (:%q src)) (:%q dest))))


(define-x8664-vinsn mem-ref-c-single-float (((dest :single-float))
                                           ((src :address)
                                            (index :s32const)))
  ((:pred = index 0)
   (movss (:@ (:%q src)) (:%xmm dest)))
  ((:not (:pred = index 0))
   (movss (:@ index (:%q src)) (:%xmm dest))))

(define-x8664-vinsn mem-set-c-single-float (()
					    ((val :single-float)
					     (src :address)
					     (index :s16const)))
  ((:pred = index 0)
   (movss (:%xmm val) (:@ (:%q src))))
  ((:not (:pred = index 0))
   (movss (:%xmm val) (:@ index (:%q src)))))

(define-x8664-vinsn mem-ref-c-doubleword (((dest :u64))
                                          ((src :address)
                                           (index :s32const)))
  ((:pred = index 0)
   (movq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-c-signed-doubleword (((dest :s64))
                                                 ((src :address)
                                                  (index :s32const)))
  ((:pred = index 0)
   (movq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-c-natural (((dest :u64))
                                       ((src :address)
                                        (index :s32const)))
  ((:pred = index 0)
   (movq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-c-double-float (((dest :double-float))
                                            ((src :address)
                                             (index :s32const)))
  ((:pred = index 0)
   (movsd (:@ (:%q src)) (:%xmm dest)))
  ((:not (:pred = index 0))
   (movsd (:@ index (:%q src)) (:%xmm dest))))

(define-x8664-vinsn mem-set-c-double-float (()
					    ((val :double-float)
					     (src :address)
					     (index :s16const)))
  ((:pred = index 0)
   (movsd (:%xmm val) (:@ (:%q src))))
  ((:not (:pred = index 0))
   (movsd (:%xmm val) (:@ index (:%q src)))))

(define-x8664-vinsn mem-ref-fullword (((dest :u32))
				      ((src :address)
				       (index :s64)))
  (movl (:@ (:%q src) (:%q index)) (:%l dest)))

(define-x8664-vinsn mem-ref-signed-fullword (((dest :s32))
                                             ((src :address)
                                              (index :s64)))
  (movslq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-doubleword (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-natural (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-signed-doubleword (((dest :s64))
                                               ((src :address)
                                                (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-u16 (((dest :u16))
				   ((src :address)
				    (index :s32const)))
  ((:pred = index 0)  
   (movzwq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movzwq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-u16 (((dest :u16))
				 ((src :address)
				  (index :s64)))
  (movzwq (:@ (:%q src) (:%q index)) (:%q dest)))


(define-x8664-vinsn mem-ref-c-s16 (((dest :s16))
				   ((src :address)
				    (index :s32const)))
  ((:pred = index 0)
   (movswq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movswq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-s16 (((dest :s16))
				 ((src :address)
				  (index :s32)))
  (movswq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-u8 (((dest :u8))
				  ((src :address)
				   (index :s16const)))
  ((:pred = index 0)
   (movzbq (:@  (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movzbq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-u8 (((dest :u8))
				((src :address)
				 (index :s32)))
  (movzbq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-s8 (((dest :s8))
				  ((src :address)
				   (index :s16const)))
  ((:pred = index 0)
   (movsbq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movsbq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-s8 (((dest :s8))
				((src :address)
				 (index :s32)))
  (movsbq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-set-constant-doubleword (()
                                                 ((val :s32const)
                                                  (ptr :address)
                                                  (offset :s64)))
  (movq (:$l val) (:@ (:%q ptr) (:%q offset))))

(define-x8664-vinsn mem-set-constant-fullword (()
                                               ((val :s32const)
                                                (ptr :address)
                                                (offset :s64)))
  (movl (:$l val) (:@ (:%q ptr) (:%q offset))))


(define-x8664-vinsn mem-set-constant-halfword (()
                                               ((val :s16const)
                                                (ptr :address)
                                                (offset :s64)))
  (movw (:$w val) (:@ (:%q ptr) (:%q offset))))

(define-x8664-vinsn mem-set-constant-byte (()
                                           ((val :s8const)
                                            (ptr :address)
                                            (offset :s64)))
  (movb (:$b val) (:@ (:%q ptr) (:%q offset))))

(define-x8664-vinsn misc-set-c-u8  (((val :u8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (movb (:%b val) (:@ (:apply + x8664::misc-data-offset idx) (:%q v))))

(define-x8664-vinsn misc-set-u8  (((val :u8))
				  ((v :lisp)
				   (scaled-idx :s64))
				  ())
  (movb (:%b val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-u8  (((val :u8))
                                    ((v :lisp)
                                     (idx :s32const))
                                    ())
  (movb (:%b val) (:@ (:apply + x8664::misc-data-offset idx) (:%q v))))

(define-x8664-vinsn misc-set-u8  (()
				  ((val :u8)
                                   (v :lisp)
				   (scaled-idx :s64))
				  ())
  (movb (:%b val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-u16  (()
                                    ((val :u16)
                                     (v :lisp)
                                     (idx :s32const))
                                    ())
  (movw (:%w val) (:@ (:apply + x8664::misc-data-offset (:apply * 2 idx)) (:%q v))))


(define-x8664-vinsn misc-set-u16  (()
                                   ((val :u16)
                                    (v :lisp)
                                    (scaled-idx :s64))
                                   ())
  (movw (:%w val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-u32  (()
				     ((val :u32)
                                      (v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:%l val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2)) (:%q v))))

(define-x8664-vinsn misc-set-u32  (()
                                   ((val :u32)
                                    (v :lisp)
                                    (scaled-idx :s64))
                                   ())
  (movl (:%l val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn %iasr (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :s64)
                            (shiftcount (:s64 #.x8664::rcx))))
  (movq (:%q count) (:%q temp))
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (xorl (:%l shiftcount) (:%l shiftcount))
  (rcmpq (:%q temp) (:$l 63))
  (cmovbw (:%w temp) (:%w shiftcount))
  (movq (:%q src) (:%q temp))
  (jae :shift-max)
  (sarq (:%shift x8664::cl) (:%q temp))
  (xorb (:%b x8664::cl) (:%b x8664::cl))
  (jmp :done)
  :shift-max
  (sarq (:$ub 63) (:%q temp))
  :done
  (andb (:$b (lognot x8664::fixnummask)) (:%b temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %ilsr (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :s64)
                            (shiftcount (:s64 #.x8664::rcx))))
  (movq (:%q count) (:%q temp))
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (xorl (:%l shiftcount) (:%l shiftcount))
  (rcmpq (:%q temp) (:$l 63))
  (cmovbw (:%w temp) (:%w shiftcount))
  (movq (:%q src) (:%q temp))
  (jae :shift-max)
  (shrq (:%shift x8664::cl) (:%q temp))
  (xorb (:%b x8664::cl) (:%b x8664::cl))
  (jmp :done)
  :shift-max
  (shrq (:$ub 63) (:%q temp))
  :done
  (andb (:$b (lognot x8664::fixnummask)) (:%b temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %iasr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
			     ((temp :s64)))
  (movq (:%q src) (:%q temp))
  (sarq (:$ub count) (:%q temp))
  (andb (:$b (lognot x8664::fixnummask)) (:%b temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %ilsr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
			     ((temp :s64)))
  (movq (:%q src) (:%q temp))
  (shrq (:$ub count) (:%q temp))
  (andb (:$b (lognot x8664::fixnummask)) (:%b temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %ilsl (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :s64)
                            (shiftcount (:s64 #.x8664::rcx))))
  (movq (:%q count) (:%q temp))
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (xorl (:%l shiftcount) (:%l shiftcount))
  (rcmpq (:%q temp) (:$l 63))
  (cmovbw (:%w temp) (:%w shiftcount))
  (movq (:%q src) (:%q temp))
  (jae :shift-max)
  (shlq (:%shift x8664::cl) (:%q temp))
  (xorb (:%b x8664::cl) (:%b x8664::cl))
  (jmp :done)
  :shift-max
  (xorq (:%q temp) (:%q temp))
  :done
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %ilsl-c (((dest :imm))
			     ((count :u8const)
			      (src :imm)))
  ((:not (:pred =
                (:apply %hard-regspec-value src)
                (:apply %hard-regspec-value dest)))
   (movq (:%q src) (:%q dest)))
  (shlq (:$ub count) (:%q dest)))

;;; In safe code, something else has ensured that the value is of type
;;; BIT.
(define-x8664-vinsn set-variable-bit-to-variable-value (()
                                                        ((vec :lisp)
                                                         (word-index :s64)
                                                         (bitnum :u8)
                                                         (value :lisp)))
  (testb (:%b value) (:%b value))
  (je :clr)
  (btsq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8))
  (jmp :done)
  :clr
  (btrq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8))
  :done)

(define-x8664-vinsn set-variable-bit-to-zero (()
                                              ((vec :lisp)
                                               (word-index :s64)
                                               (bitnum :u8)))
  (btrq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8)))

(define-x8664-vinsn set-variable-bit-to-one (()
                                              ((vec :lisp)
                                               (word-index :s64)
                                               (bitnum :u8)))
  (btsq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8)))

(define-x8664-vinsn set-constant-bit-to-zero (()
                                              ((src :lisp)
                                               (idx :u64const)))
  (btrq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src))))

(define-x8664-vinsn set-constant-bit-to-one (()
                                             ((src :lisp)
                                              (idx :u64const)))
  (btsq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src))))

(define-x8664-vinsn set-constant-bit-to-variable-value (()
                                                        ((src :lisp)
                                                         (idx :u64const)
                                                         (value :lisp)))
  (testb (:%b value) (:%b value))
  (je :clr)
  (btsq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src)))
  (jmp :done)
  :clr
  (btrq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src)))
  :done)


(define-x8664-vinsn require-fixnum (()
                                    ((object :lisp)))
  :again
  (testb (:$b x8664::fixnummask) (:%b object))
  (je.pt :got-it)
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-fixnum))
  (jmp :again)
  :got-it)

(define-x8664-vinsn require-integer (()
                                     ((object :lisp))
                                     ((tag :u8)))
  :again
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::fixnummask) (:%b tag))
  (je.pt :got-it)
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-bignum) (:@ x8664::misc-subtag-offset (:%q object)))
  (je :got-it)
  :bad
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-integer))
  (jmp :again)
  :got-it)

(define-x8664-vinsn require-simple-vector (()
                                           ((object :lisp))
                                           ((tag :u8)))
  :again
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::fixnummask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-simple-vector) (:@ x8664::misc-subtag-offset (:%q object)))
  (je :got-it)
  :bad
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-simple-vector))
  (jmp :again)
  :got-it)

(define-x8664-vinsn require-simple-string (()
                                           ((object :lisp))
                                           ((tag :u8)))
  :again
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::fixnummask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-simple-base-string) (:@ x8664::misc-subtag-offset (:%q object)))
  (je :got-it)
  :bad
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-simple-string))
  (jmp :again)
  :got-it)
                                    
(define-x8664-vinsn require-real (()
                                    ((object :lisp))
                                    ((tag :u8)
                                     (mask :u64)))
  (movq (:$q (logior (ash 1 x8664::tag-fixnum)
                     (ash 1 x8664::tag-single-float)
                     (ash 1 x8664::subtag-double-float)
                     (ash 1 x8664::subtag-bignum)
                     (ash 1 x8664::subtag-ratio)))
        (:%q mask))
  :again
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q object)) (:%b tag))
  :have-tag
  (rcmpb (:%b tag) (:$b 64))
  (jae :bad)
  (btq (:%q tag) (:%q mask))
  (jb.pt :good)
  :bad
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-real))
  (jmp :again)
  :good)

(define-x8664-vinsn require-number (()
                                    ((object :lisp))
                                    ((tag :u8)
                                     (mask :u64)))
  (movq (:$q (logior (ash 1 x8664::tag-fixnum)
                     (ash 1 x8664::tag-single-float)
                     (ash 1 x8664::subtag-double-float)
                     (ash 1 x8664::subtag-bignum)
                     (ash 1 x8664::subtag-ratio)
                     (ash 1 x8664::subtag-complex)))
        (:%q mask))
  :again
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q object)) (:%b tag))
  :have-tag
  (rcmpb (:%b tag) (:$b 64))
  ;;(movzbl (:%b tag) (:%l tag))
  (jae :bad)
  (btq (:%q tag) (:%q mask))
  (jb.pt :good)
  :bad
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-number))
  (jmp :again)
  :good)

(define-x8664-vinsn require-list (()
                                  ((object :lisp))
                                  ((tag :u8)))
  :again
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-list) (:%b tag))
  (je :good)
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-list))
  (jmp :again)
  :good)

(define-x8664-vinsn require-symbol (()
                                    ((object :lisp))
                                    ((tag :u8)))
  :again
  (cmpb (:$b x8664::fulltag-nil) (:%b object))
  (je :good)
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-symbol) (:%b tag))
  (je :good)
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-symbol))
  (jmp :again)
  :good)

(define-x8664-vinsn require-character (()
				((object :lisp)))
  :again
  (cmpb (:$b x8664::subtag-character) (:%b object))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-character))
  (jmp :again)
  :ok)

(define-x8664-vinsn require-u8 (()
				((object :lisp))
				((tag :u32)))
  :again
  (movq (:$l (lognot (ash #xff x8664::fixnumshift))) (:%q tag))
  (andq (:% object) (:% tag))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-unsigned-byte-8))
  (jmp :again)
  :ok)

(define-x8664-vinsn require-char-code (()
                                       ((object :lisp))
                                       ((tag :u32)))
  :again
  (testb (:$b x8664::fixnummask) (:%b object))
  (jne.pn :bad)
  (cmpq (:$l (ash #x110000 x8664::fixnumshift)) (:%q object))
  (jb.pt :ok)
  :bad
  (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-mod-char-code-limit))
  (jmp :again)
  :ok)





(define-x8664-vinsn mask-base-char (((dest :u8))
                                    ((src :lisp)))
  (movzbl (:%b src) (:%l dest))) 

(define-x8664-vinsn single-float-bits (((dest :u32))
                                       ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (shrq (:$ub 32) (:%q dest)))

(define-x8664-vinsn zero-double-float-register (((dest :double-float))
                                                ())
  (movsd (:%xmm x8664::fpzero) (:%xmm dest)))

(define-x8664-vinsn zero-single-float-register (((dest :single-float))
                                                ())
  (movss (:%xmm x8664::fpzero) (:%xmm dest)))

(define-x8664-subprim-call-vinsn (heap-rest-arg) .SPheap-rest-arg)
(define-x8664-subprim-call-vinsn (stack-rest-arg) .SPstack-rest-arg)
(define-x8664-subprim-call-vinsn (req-stack-rest-arg) .SPreq-stack-rest-arg)

(define-x8664-subprim-call-vinsn (stack-misc-alloc) .SPstack-misc-alloc)

(define-x8664-vinsn misc-element-count-fixnum (((dest :imm))
                                               ((src :lisp))
                                               ((temp :u64)))
  (movq (:@ x8664::misc-header-offset (:%q src)) (:%q temp))
  (movb (:$b 0) (:%b temp))
  (movq (:%q temp) (:%q dest))
  (shrq (:$ub (- x8664::num-subtag-bits x8664::fixnumshift)) (:%q dest)))

(define-x8664-vinsn %logior2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (orq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (orq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movq (:%q x) (:%q dest))
    (orq (:%q y) (:%q dest)))))

(define-x8664-vinsn %logand2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (andq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (andq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movq (:%q x) (:%q dest))
    (andq (:%q y) (:%q dest)))))

(define-x8664-vinsn %logxor2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (xorq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (xorq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movq (:%q x) (:%q dest))
    (xorq (:%q y) (:%q dest)))))

(define-x8664-subprim-call-vinsn (integer-sign) .SPinteger-sign)

(define-x8664-vinsn vcell-ref (((dest :lisp))
			       ((vcell :lisp)))
  (movq (:@ x8664::misc-data-offset (:%q vcell)) (:%q dest)))

;;; The only current use of this - calling .SPgvset - doesn't need
;;; to recover %fn from %ra0.
(define-x8664-vinsn (call-subprim-3 :call :subprim-call) (((dest t))
							  ((spno :s32const)
							   (x t)
							   (y t)
							   (z t)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ spno))
  (:align 3)
  (:long (:^ :back))
  :back)

(define-x8664-vinsn setup-vcell-allocation (()
                                            ())
  (movl (:$l x8664::value-cell-header) (:%l x8664::imm0))
  (movl (:$l (- x8664::value-cell.size x8664::fulltag-misc)) (:%l x8664::imm1)))

(define-x8664-vinsn %init-vcell (()
                                 ((vcell :lisp)
                                  (closed :lisp)))
  (movq (:%q closed) (:@ x8664::value-cell.value (:%q vcell))))

(define-x8664-subprim-call-vinsn (progvsave) .SPprogvsave)

(define-x8664-subprim-jump-vinsn (progvrestore) .SPprogvrestore)

(define-x8664-subprim-call-vinsn (simple-keywords) .SPsimple-keywords)

(define-x8664-subprim-call-vinsn (keyword-args) .SPkeyword-args)

(define-x8664-subprim-call-vinsn (keyword-bind) .SPkeyword-bind)

(define-x8664-vinsn scale-nargs (()
				 ((nfixed :s16const)))
  ((:pred > nfixed 0)
   (addw (:$w (:apply - (:apply ash nfixed x8664::word-shift))) (:%w x8664::nargs))))

(define-x8664-vinsn opt-supplied-p (()
                                    ())
  (xorl (:%l x8664::imm1) (:%l x8664::imm1))
  (movl (:$l x8664::t-value) (:%l x8664::arg_y))
  :loop
  (rcmpw (:%w x8664::imm1) (:%w x8664::nargs))
  (movl (:$l x8664::nil-value) (:%l x8664::arg_z))
  (cmovll (:%l x8664::arg_y) (:%l  x8664::arg_z))
  (addl (:$b x8664::node-size) (:%l x8664::imm1))
  (cmpl (:%l x8664::imm1) (:%l x8664::imm0))
  (pushq (:%q x8664::arg_z))
  (jne :loop))

(define-x8664-vinsn one-opt-supplied-p (()
                                        ()
                                        ((temp :u64)))
  (testw (:%w x8664::nargs) (:%w x8664::nargs))
  (setne (:%b temp))
  (negb (:%b temp))
  (andl (:$b x8664::t-offset) (:%l temp))
  (addl (:$l x8664::nil-value) (:%l temp))
  (pushq (:%q temp)))

(define-x8664-vinsn two-opt-supplied-p (()
                                        ()
                                        ((temp0 :u64)
                                         (temp1 :u64)))
  (rcmpw (:%w x8664::nargs) (:$w x8664::node-size))
  (setae (:%b temp0))
  (seta (:%b temp1))
  (negb (:%b temp0))
  (negb (:%b temp1))
  (andl (:$b x8664::t-offset) (:%l temp0))
  (andl (:$b x8664::t-offset) (:%l temp1))
  (addl (:$l x8664::nil-value) (:%l temp0))
  (addl (:$l x8664::nil-value) (:%l temp1))
  (pushq (:%q temp0))
  (pushq (:%q temp1)))


(define-x8664-vinsn set-c-flag-if-constant-logbitp (()
                                                    ((bit :u8const)
                                                     (int :imm)))
  (btq (:$ub bit) (:%q int)))

(define-x8664-vinsn set-c-flag-if-variable-logbitp (()
                                                    ((bit :imm)
                                                     (int :imm))
                                                    ((temp0 :u8)
                                                     (temp1 :u8)))
  (movl (:$l 63) (:%l temp1))
  (movq (:%q bit) (:%q temp0))
  (sarq (:$ub x8664::fixnumshift) (:%q temp0))
  (addq (:$b x8664::fixnumshift) (:%q temp0))
  (rcmpq (:%q temp0) (:%q temp1))
  (cmoval (:%l temp1) (:%l temp0))
  (btq (:%q temp0) (:%q int)))

(define-x8664-vinsn multiply-immediate (((dest :imm))
                                        ((src :imm)
                                         (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (imulq (:$b const) (:%q src) (:%q dest)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (imulq (:$l const) (:%q src) (:%q dest))))

(define-x8664-vinsn multiply-fixnums (((dest :imm))
                                      ((x :imm)
                                       (y :imm))
                                      ((unboxed :s64)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (movq (:%q y) (:%q unboxed))
   (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
   (imulq (:%q unboxed) (:%q dest)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value x)
                      (:apply %hard-regspec-value dest)))
         (:pred =
                (:apply %hard-regspec-value y)
                (:apply %hard-regspec-value dest)))
   (movq (:%q x) (:%q unboxed))
   (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
   (imulq (:%q unboxed) (:%q dest)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value x)
                      (:apply %hard-regspec-value dest)))
         (:not (:pred =
                      (:apply %hard-regspec-value y)
                      (:apply %hard-regspec-value dest))))
   (movq (:%q y) (:%q dest))
   (movq (:%q x) (:%q unboxed))
   (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
   (imulq (:%q unboxed) (:%q dest))))

   
(define-x8664-vinsn save-lexpr-argregs (()
                                        ((min-fixed :u16const)))
  ((:pred >= min-fixed $numx8664argregs)
   (pushq (:%q x8664::arg_x))
   (pushq (:%q x8664::arg_y))
   (pushq (:%q x8664::arg_z)))
  ((:pred = min-fixed 2)                ; at least 2 args
   (cmpw (:$w (ash 2 x8664::word-shift)) (:%w x8664::nargs))
   (je :yz2)                      ; skip arg_x if exactly 2
   (pushq (:%q x8664::arg_x))
   :yz2
   (pushq (:%q x8664::arg_y))
   (pushq (:%q x8664::arg_z)))
  ((:pred = min-fixed 1)                ; at least one arg
   (rcmpw (:%w x8664::nargs) (:$w  (ash 2 x8664::word-shift)))
   (jl :z1)                       ; branch if exactly one
   (je :yz1)                      ; branch if exactly two
   (pushq (:%q x8664::arg_x))
   :yz1
   (pushq (:%q x8664::arg_y))
   :z1
   (pushq (:%q x8664::arg_z)))
  ((:pred = min-fixed 0)
   (testw (:%w x8664::nargs) (:%w x8664::nargs))
   (je  :none)                     ; exactly zero
   (rcmpw (:%w x8664::nargs) (:$w (ash 2 x8664::word-shift)))
   (je :yz0)                      ; exactly two
   (jl :z0)                       ; one
                                        ; Three or more ...
   (pushq (:%q x8664::arg_x))
   :yz0
   (pushq (:%q x8664::arg_y))
   :z0
   (pushq (:%q x8664::arg_z))
   :none
   )
  (movzwl (:%w x8664::nargs) (:%l x8664::nargs))
  ((:not (:pred = min-fixed 0))
   (leaq (:@ (:apply - (:apply ash min-fixed x8664::word-shift)) (:%q x8664::nargs))
         (:%q x8664::nargs)))
  (pushq (:%q x8664::nargs))
  (movq (:%q x8664::rsp) (:%q x8664::arg_z)))




;;; The frame that was built (by SAVE-LISP-CONTEXT-VARIABLE-ARG-COUNT
;;; and SAVE-LEXPR-ARGREGS) contains an unknown number of arguments
;;; followed by the count of non-required arguments; the count is on
;;; top of the stack and its address is in %arg_z.  We need to build a
;;; frame so that the function can address its arguments (copies of
;;; the required arguments and the lexpr) and locals; when the
;;; function returns, it should one or more values (depending on how
;;; it was called) and discard the hidden lexpr frame.  At this point,
;;; %ra0 still contains the "real" return address. If it's not the
;;; magic multiple-value address, we can make the function return to
;;; something that does a single-value return (.SPpopj); otherwise, we
;;; need to make it return multiple values to the real caller. (Unlike
;;; the PPC, this case only involves creating one frame here, but that
;;; frame has two return addresses.)
(define-x8664-vinsn build-lexpr-frame (()
                                       ()
                                       ((temp :imm)))
  (movq (:@ (+ x8664::nil-value (x8664::%kernel-global 'x86::ret1valaddr)))
        (:%q temp))
  (cmpq (:%q temp)
        (:%q x8664::ra0))
  (je :multiple)
  (pushq (:@ (+ x8664::nil-value (x8664::%kernel-global 'x86::lexpr-return1v))))
  (jmp :finish)
  :multiple
  (pushq (:@ (+ x8664::nil-value (x8664::%kernel-global 'x86::lexpr-return))))
  (pushq (:%q temp))
  :finish
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp)))


(define-x8664-vinsn copy-lexpr-argument (()
					 ((n :u16const))
					 ((temp :imm)))
  (movq (:@ (:%q x8664::arg_z)) (:%q temp))
  (pushq (:@ (:apply ash n x8664::word-shift) (:%q x8664::arg_z) (:%q temp))))


(define-x8664-vinsn %current-tcr (((dest :lisp))
                                 ())
  (movq (:@ (:%seg :rcontext) x8664::tcr.linear) (:%q dest)))

(define-x8664-vinsn (setq-special :call :subprim-call)
    (()
     ((sym :lisp)
      (val :lisp)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPspecset))
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:%q x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn set-z-flag-if-istruct-typep (()
                                                 ((val :lisp)
                                                  (type :lisp))
                                                 ((tag :u8)
                                                  (valtype :lisp)))
  (xorl (:%l valtype) (:%l valtype))
  (movb (:%b val) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :have-tag)
  (movb (:@ x8664::misc-subtag-offset (:%q val)) (:%b tag))
  :have-tag
  (cmpb (:$b x8664::subtag-istruct) (:%b tag))
  (jne :do-compare)
  (movq (:@ x8664::misc-data-offset (:%q val)) (:%q valtype))
  :do-compare
  (cmpq (:%q valtype) (:%q type)))

(define-x8664-subprim-call-vinsn (misc-ref) .SPmisc-ref)

(define-x8664-subprim-call-vinsn (ksignalerr) .SPksignalerr)

(define-x8664-subprim-call-vinsn (misc-alloc-init) .SPmisc-alloc-init t)

(define-x8664-subprim-call-vinsn (misc-alloc) .SPmisc-alloc) 

(define-x8664-subprim-call-vinsn (make-stack-gvector)  .SPstkgvector)

(define-x8664-vinsn load-character-constant (((dest :lisp))
                                             ((code :u8const))
                                             ())
  (movl (:$l (:apply logior (:apply ash code 8) x8664::subtag-character))
        (:%l dest)))

(define-x8664-vinsn %scharcode8 (((code :imm))
				((str :lisp)
				 (idx :imm))
				((imm :u64)))
  (movq (:%q idx) (:%q imm))
  (sarq (:$ub x8664::fixnumshift) (:%q imm))
  (movzbl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (imulq (:$b x8664::fixnumone) (:%q imm)(:%q code)))

(define-x8664-vinsn %scharcode32 (((code :imm))
				((str :lisp)
				 (idx :imm))
				((imm :u64)))
  (movq (:%q idx) (:%q imm))
  (sarq (:$ub 1) (:%q imm))
  (movl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (imulq (:$b x8664::fixnumone) (:%q imm)(:%q code)))

(define-x8664-subprim-jump-vinsn (tail-call-sym-slide) .SPtcallsymslide)

(define-x8664-subprim-jump-vinsn (tail-call-sym-vsp) .SPtcallsymvsp)


(define-x8664-vinsn character->code (((dest :u32))
				     ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::charcode-shift) (:%q  dest)))

(define-x8664-vinsn adjust-vsp (()
				((amount :s32const)))
  ((:and (:pred >= amount -128) (:pred <= amount 127))
   (addq (:$b amount) (:%q x8664::rsp)))
  ((:not (:and (:pred >= amount -128) (:pred <= amount 127)))
   (addq (:$l amount) (:%q x8664::rsp))))

(define-x8664-vinsn (call-subprim-2 :call :subprim-call) (((dest t))
							  ((spno :s32const)
							   (y t)
							   (z t)))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ spno))
  (:align 3)
  (:long (:^ :back))
  :back
  (leaq (:@ (:apply - (:^ :back)) (:%q x8664::ra0)) (:%q x8664::fn)))

(define-x8664-vinsn set-macptr-address (()
					((addr :address)
					 (src :lisp))
					())
  (movq (:%q addr) (:@ x8664::macptr.address (:%q src))))

(define-x8664-vinsn %symbol->symptr (((dest :lisp))
                                     ((src :lisp))
                                     ((tag :u8)))
  (movl (:$l (+ x8664::nil-value x8664::nilsym-offset)) (:%l tag))
  (cmpb (:$b x8664::fulltag-nil) (:%b src))
  (cmoveq (:%q tag) (:%q dest))
  (movb (:%b src) (:%b tag))
  (je :ok)
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-symbol) (:%b tag))
  (je.pt :no-trap)
  (uuo-error-reg-not-tag (:%q src) (:$ub x8664::fulltag-symbol))
  :no-trap
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movq (:% src) (:% dest)))
  :ok)

(define-x8664-vinsn symbol-function (((val :lisp))
                                     ((sym (:lisp (:ne val))))
                                     ((tag :u8)))
  (movq (:@ x8664::symbol.fcell (:%q sym)) (:%q val))
  (movb (:%b val) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-function) (:%b tag))
  (je.pt :ok)
  (uuo-error-udf (:%q sym))
  :ok)

(define-x8664-subprim-jump-vinsn (tail-call-fn-slide) .SPtcallnfnslide)

(define-x8664-vinsn load-double-float-constant (((dest :double-float))
                                                ((lab :label)
))
  (movsd (:@ (:^ lab) (:%q x8664::fn)) (:%xmm dest)))

(define-x8664-vinsn load-single-float-constant (((dest :single-float))
                                                ((lab :label)
))
  (movss (:@ (:^ lab) (:%q x8664::fn)) (:%xmm dest)))

(define-x8664-subprim-call-vinsn (misc-set) .SPmisc-set)

(define-x8664-subprim-call-vinsn (slide-values) .SPmvslide)

(define-x8664-subprim-call-vinsn (spread-list)  .SPspreadargz)

;;; Even though it's implemented by calling a subprim, THROW is really
;;; a JUMP (to a possibly unknown destination).  If the destination's
;;; really known, it should probably be inlined (stack-cleanup, value
;;; transfer & jump ...)
(define-x8664-vinsn (throw :jump :jump-unknown) (()
						 ())
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (jmp (:@ .SPthrow))
  (:align 3)
  (:long (:^ :back))
  :back)



(define-x8664-vinsn unbox-base-char (((dest :u64))
				     ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::charcode-shift) (:%q dest))
  (cmpb (:$b x8664::subtag-character) (:%b src))
  (je.pt ::got-it)
  (uuo-error-reg-not-tag (:%q src) (:$ub x8664::subtag-character))
  :got-it)

(define-x8664-subprim-call-vinsn (save-values) .SPsave-values)

(define-x8664-subprim-call-vinsn (recover-values)  .SPrecover-values)

(define-x8664-subprim-call-vinsn (recover-values-for-mvcall) .SPrecover-values-for-mvcall)

(define-x8664-subprim-call-vinsn (add-values) .SPadd-values)

(define-x8664-subprim-call-vinsn (make-stack-block)  .SPmakestackblock)

(define-x8664-subprim-call-vinsn (make-stack-block0)  .Spmakestackblock0)

;;; "dest" is preallocated, presumably on a stack somewhere.
(define-x8664-vinsn store-double (()
				  ((dest :lisp)
				   (source :double-float))
				  ())
  (movsd (:%xmm source) (:@  x8664::double-float.value (:%q dest))))

(define-x8664-vinsn fixnum->char (((dest :lisp))
				  ((src :imm))
				  ((temp :u32)))
  (movl (:%l src) (:%l temp))
  (sarl (:$ub (+ x8664::fixnumshift 11)) (:%l temp))
  (cmpl (:$b (ash #xd800 -11))(:%l temp))
  (movl (:$l x8664::nil-value) (:%l temp))
  (cmovel (:%l temp) (:%l dest))
  (je :done)
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))
  (shll (:$ub (- x8664::charcode-shift x8664::fixnumshift)) (:%l dest))
  (addb (:$b x8664::subtag-character) (:%b dest))
  :done)


(define-x8664-vinsn sign-extend-halfword (((dest :imm))
					  ((src :imm)))
  (movq (:%q src ) (:%q dest))
  (shlq (:$ub (- 48 x8664::fixnumshift)) (:%q dest))
  (sarq (:$ub (- 48 x8664::fixnumshift)) (:%q dest)))

(define-x8664-subprim-jump-vinsn (tail-funcall-gen) .SPtfuncallgen)

(define-x8664-subprim-call-vinsn (gets64) .SPgets64)

(define-x8664-subprim-call-vinsn (getu64) .SPgetu64)

(define-x8664-vinsn %init-gvector (()
                                   ((v :lisp)
                                    (nbytes :u32const))
                                   ((count :imm)))
  (movl (:$l nbytes) (:%l count))
  (jmp :test)
  :loop
  (popq (:@ x8664::misc-data-offset (:%q v) (:%q count)))
  :test
  (subq (:$b x8664::node-size) (:%q count))
  (jge :loop))

(define-x8664-subprim-jump-vinsn (tail-funcall-slide) .SPtfuncallslide)

(define-x8664-vinsn nth-value (((result :lisp))
                               ()
                               ((imm0 :u64)))
  (movzwl (:%w x8664::nargs) (:%l x8664::nargs))
  (leaq (:@ (:%q x8664::rsp) (:%q x8664::nargs)) (:%q imm0))
  (subq (:@ (:%q imm0)) (:%q x8664::nargs))
  (movl (:$l x8664::nil-value) (:%l result))
  (jle :done)
  ;; I -think- that a CMOV would be safe here, assuming that N wasn't
  ;; extremely large.  Don't know if we can assume that.
  (movq (:@ (- x8664::node-size) (:%q x8664::rsp) (:%q x8664::nargs)) (:%q result))
  :done
  (leaq (:@ x8664::node-size (:%q imm0)) (:%q x8664::rsp)))


(define-x8664-subprim-call-vinsn (req-heap-rest-arg) .SPreq-heap-rest-arg)

(define-x8664-subprim-call-vinsn (stack-misc-alloc-init)  .SPstack-misc-alloc-init t)

(define-x8664-vinsn fixnum->unsigned-natural (((dest :u64))
                                              ((src :imm)))
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn %debug-trap (()
                                 ())
  (uuo-error-debug-trap))

(define-x8664-vinsn double-to-single (((result :single-float))
                                      ((arg :double-float)))
  (cvtsd2ss (:%xmm arg) (:%xmm result)))

(define-x8664-vinsn single-to-double (((result :double-float))
                                      ((arg :single-float)))
  (cvtss2sd (:%xmm arg) (:%xmm result)))


(define-x8664-vinsn alloc-c-frame (()
                                   ((nbytes :u32const)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%mmx x8664::stack-temp))
  ((:pred < nbytes 128)
   (subq (:$b nbytes) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp)))
  ((:not (:pred < nbytes 128))
   (subq (:$l nbytes) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movq (:%mmx x8664::stack-temp) (:@ (:%q x8664::ra0))))

(define-x8664-vinsn alloc-variable-c-frame (()
                                            ((nwords :imm))
                                            ((size :s64)))
  (leaq (:@ (* 9 x8664::node-size) (:%q nwords)) (:%q size))
  (andb (:$b (lognot x8664::fulltagmask)) (:%b size))

  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%mmx x8664::stack-temp))
  (subq (:%q size) (:@ (:%seg :rcontext) x8664::tcr.foreign-sp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movq (:%mmx x8664::stack-temp) (:@ (:%q x8664::ra0))))

(define-x8664-vinsn set-c-arg (()
                               ((arg :u64)
                                (offset :u32const)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movq (:%q arg) (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0))))

(define-x8664-vinsn set-single-c-arg (()
                                      ((arg :single-float)
                                       (offset :u32const)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movss (:%xmm arg) (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0))))

(define-x8664-vinsn reload-single-c-arg (((arg :single-float))
                                         ((offset :u32const)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movss (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0)) (:%xmm arg)))

(define-x8664-vinsn set-double-c-arg (()
                                      ((arg :double-float)
                                       (offset :u32const)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movsd (:%xmm arg) (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0))))

(define-x8664-vinsn reload-double-c-arg (((arg :double-float))
                                         ((offset :u32const)))
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movsd (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0)) (:%xmm arg)))

(define-x8664-subprim-call-vinsn (ff-call)  .SPffcall)

(define-x8664-subprim-call-vinsn (syscall)  .SPsyscall)

(define-x8664-subprim-call-vinsn (setqsym) .SPsetqsym)

(define-x8664-vinsn recover-fn-from-ra0 (()
                                         ((lab :label)))
  (leaq (:@ (:apply - (:^ lab)) (:%q x8664::ra0)) (:%q x8664::fn)))

(define-x8664-subprim-call-vinsn (makeu64) .SPmakeu64)

(define-x8664-subprim-call-vinsn (makes64) .SPmakes64)

(define-x8664-subprim-call-vinsn (stack-cons-list*)  .SPstkconslist-star)

(define-x8664-subprim-call-vinsn (list*) .SPconslist-star)

(define-x8664-vinsn make-tsp-vcell (((dest :lisp))
				    ((closed :lisp))
				    ((temp :imm)))
  (subq (:$b (+ x8664::value-cell.size x8664::dnode-size)) (:@ (:%seg :rcontext) x8664::tcr.next-tsp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.save-tsp) (:%mmx x8664::stack-temp))
  (movq (:@ (:%seg :rcontext) x8664::tcr.next-tsp) (:%q temp))
  (movapd (:%xmm x8664::fpzero) (:@ (:%q temp)))
  (movapd (:%xmm x8664::fpzero) (:@ x8664::dnode-size (:%q temp)))
  (movq (:%mmx x8664::stack-temp) (:@ (:%q temp))) 
  (movq (:%q temp) (:@ (:%seg :rcontext) x8664::tcr.save-tsp))  
  (movq (:$l x8664::value-cell-header) (:@ x8664::dnode-size (:%q temp)))
  (movq (:%q closed) (:@ (+ x8664::dnode-size x8664::node-size) (:%q temp)))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-misc) (:%q temp)) (:%q dest)))

(define-x8664-subprim-call-vinsn (bind-nil)  .SPbind-nil)

(define-x8664-subprim-call-vinsn (bind-self)  .SPbind-self)

(define-x8664-subprim-call-vinsn (bind-self-boundp-check)  .SPbind-self-boundp-check)

(define-x8664-subprim-call-vinsn (bind)  .SPbind)

(define-x8664-vinsn (dpayback :call :subprim-call) (()
                                                    ((n :s16const))
                                                    ((temp (:u32 #.x8664::imm0))))
  ((:pred > n 0)
   (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
   ((:pred > n 1)
    (movl (:$l n) (:%l temp))
    (jmp (:@ .SPunbind-n)))
   ((:pred = n 1)
    (jmp (:@ .SPunbind)))
   (:align 3)
   (:long (:^ :back))
   :back))  

(define-x8664-subprim-jump-vinsn (tail-call-sym-gen) .SPtcallsymgen)

(define-x8664-subprim-call-vinsn (make-stack-list)  .Spmakestacklist)

(define-x8664-vinsn node-slot-ref  (((dest :lisp))
				    ((node :lisp)
				     (cellno :u32const)))
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash cellno 3))
            (:%q node)) (:%q dest)))

(define-x8664-subprim-call-vinsn (stack-cons-list)  .SPstkconslist)


(define-x8664-vinsn  %slot-ref (((dest :lisp))
				((instance (:lisp (:ne dest)))
				 (index :lisp)))
  (movq (:@ x8664::misc-data-offset (:%q instance) (:%q index)) (:%q dest))
  (cmpb (:$b x8664::slot-unbound-marker) (:%b dest))
  (jne.pt :ok)
  (uuo-error-slot-unbound (:%q dest) (:%q instance) (:%q index))
  :ok)

(define-x8664-vinsn eep.address (((dest t))
				 ((src (:lisp (:ne dest )))))
  (movq (:@ (+ (ash 1 x8664::word-shift) x8664::misc-data-offset) (:%q src))
        (:%q dest))
  (cmpb (:$b x8664::fulltag-nil) (:%b dest))
  (jne :ok)
  (uuo-error-eep-unresolved (:%q src) (:%q dest))
  :ok)

(define-x8664-subprim-call-vinsn (heap-cons-rest-arg) .SPheap-cons-rest-arg)

(define-x8664-subprim-call-vinsn (stack-cons-rest-arg) .SPstack-cons-rest-arg)

(define-x8664-subprim-call-vinsn (make-stack-vector)  .SPmkstackv)

(define-x8664-vinsn %current-frame-ptr (((dest :imm))
					())
  (movq (:%q x8664::rbp) (:%q dest)))

(define-x8664-vinsn %foreign-stack-pointer (((dest :imm))
                                            ())
  (movq (:@ (:%seg :rcontext) x8664::tcr.foreign-sp) (:%q dest)))


(define-x8664-vinsn %set-scharcode8 (()
				    ((str :lisp)
				     (idx :imm)
				     (code :imm))
				    ((imm :u64)
				     (imm1 :u64)))
  (movq (:%q code) (:%q imm1))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub x8664::fixnumshift) (:%q imm1))
  (shrq (:$ub x8664::word-shift) (:%q imm))
  (movb (:%b imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm))))


(define-x8664-vinsn %set-scharcode32 (()
				    ((str :lisp)
				     (idx :imm)
				     (code :imm))
				    ((imm :u64)
				     (imm1 :u64)))
  (movq (:%q code) (:%q imm1))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub x8664::fixnumshift) (:%q imm1))
  (shrq (:$ub 1) (:%q imm))
  (movl (:%l imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm))))




(define-x8664-vinsn pop-argument-registers (()
                                            ())
  (testw (:%w x8664::nargs) (:%w x8664::nargs))
  (je :done)
  (rcmpw (:%w x8664::nargs) (:$w (ash 2 x8664::word-shift)))
  (popq (:%q x8664::arg_z))
  (jb :done)
  (popq (:%q x8664::arg_y))
  (je :done)
  (popq (:%q x8664::arg_x))
  :done)

(define-x8664-vinsn %symptr->symvector (((target :lisp))
                                        ((target :lisp)))
  (subb (:$b (- x8664::fulltag-symbol x8664::fulltag-misc)) (:%b target)))

(define-x8664-vinsn %symvector->symptr (((target :lisp))
                                        ((target :lisp)))
  (addb (:$b (- x8664::fulltag-symbol x8664::fulltag-misc)) (:%b target)))


(define-x8664-subprim-call-vinsn (spread-lexpr)  .SPspread-lexpr-z)

(define-x8664-vinsn mem-ref-double-float (((dest :double-float))
                                           ((src :address)
                                            (index :s64)))
  (movsd (:@ (:%q src) (:%q index)) (:%xmm dest)))

(define-x8664-vinsn mem-ref-single-float (((dest :single-float))
                                           ((src :address)
                                            (index :s64)))
  (movss (:@ (:%q src) (:%q index)) (:%xmm dest)))

(define-x8664-vinsn zero-extend-nargs (()
                                       ())
  (movzwl (:%w x8664::nargs) (:%l x8664::nargs)))

(define-x8664-vinsn load-adl (()
			      ((n :u32const)))
  (movl (:$l n) (:%l x8664::nargs)))

(define-x8664-subprim-call-vinsn (macro-bind) .SPmacro-bind)

(define-x8664-subprim-call-vinsn (destructuring-bind-inner) .SPdestructuring-bind-inner)

(define-x8664-subprim-call-vinsn (destructuring-bind) .SPdestructuring-bind)

(define-x8664-vinsn symbol-ref (((dest :lisp))
                                ((src :lisp)
                                 (cellno :u32const)))
  (movq (:@ (:apply + (- x8664::node-size x8664::fulltag-symbol)
                    (:apply ash cellno 3))
              (:%q src)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-bit-fixnum (((dest :lisp))
                                          ((src :address)
                                           (offset :s32const))
                                          ((temp :u32)))
  ((:pred = 0 (:apply ash offset -6))
   (btq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btq (:$ub (:apply logand 63 offset))
        (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  (movl (:$l x8664::fixnumone) (:%l temp))
  (leaq (:@ (- x8664::fixnumone) (:%q temp)) (:%q dest))
  (cmovbl (:%l temp) (:%l dest)))

(define-x8664-vinsn mem-ref-c-bit (((dest :lisp))
                                   ((src :address)
                                    (offset :s32const))
                                   ((temp :u32)))
  ((:pred = 0 (:apply ash offset -6))
   (btq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btq (:$ub (:apply logand 63 offset))
        (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  (setb (:%b temp))
  (movzbl (:%b temp) (:%l dest)))

(define-x8664-vinsn mem-ref-bit-fixnum (((dest :lisp)
                                         (src :address))
                                        ((src :address)
                                         (offset :lisp))
                                        ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btq (:%q temp) (:@ (:%q src)))
  (movl (:$l x8664::fixnumone) (:%l temp))
  (leaq (:@ (- x8664::fixnumone) (:%q temp)) (:%q dest))
  (cmovbl (:%l temp) (:%l dest)))

(define-x8664-vinsn mem-ref-bit (((dest :lisp)
                                  (src :address))
                                 ((src :address)
                                  (offset :lisp))
                                 ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btq (:%q temp) (:@ (:%q src)))
  (setb (:%b temp))
  (movzbl (:%b temp) (:%l dest)))

  
(define-x8664-vinsn mem-set-c-bit-0 (()
				     ((src :address)
                                      (offset :s32const)))
  
  ((:pred = 0 (:apply ash offset -6))
   (btrq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btrq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src)))))

(define-x8664-vinsn mem-set-c-bit-1 (()
				     ((src :address)
                                      (offset :s32const)))
  
  ((:pred = 0 (:apply ash offset -6))
   (btsq (:$ub (:apply logand 63 offset))
         (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btsq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src)))))

(define-x8664-vinsn mem-set-c-bit-variable-value (()
                                                  ((src :address)
                                                   (offset :s32const)
                                                   (value :lisp)))
  (testq (:%q value) (:%q value))
  (jne :set)
  ((:pred = 0 (:apply ash offset -6))
   (btrq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btrq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  (jmp :done)
  :set
  ((:pred = 0 (:apply ash offset -6))
   (btsq (:$ub (:apply logand 63 offset))
         (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btsq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  :done)


(define-x8664-vinsn mem-set-bit-0 (((src :address))
                                   ((src :address)
                                    (offset :lisp))
                                   ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btrq (:%q temp) (:@ (:%q src))))

(define-x8664-vinsn mem-set-bit-1 (((src :address))
                                   ((src :address)
                                    (offset :lisp))
                                   ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btsq (:%q temp) (:@ (:%q src))))


(define-x8664-vinsn mem-set-bit-variable-value (((src :address))
                                                ((src :address)
                                                 (offset :lisp)
                                                 (value :lisp))
                                                ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (testq (:%q value) (:%q value))
  (jne :set)
  (btrq (:%q temp) (:@ (:%q src)))
  (jmp :done)
  :set
  (btsq (:%q temp) (:@ (:%q src)))
  :done)

(define-x8664-vinsn %natural+  (((result :u64))
                               ((result :u64)
                                (other :u64)))
  (addq (:%q other) (:%q result)))

(define-x8664-vinsn %natural+-c (((result :u64))
                                ((result :u64)
                                 (constant :s32const)))
  (addq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-  (((result :u64))
                               ((result :u64)
                                (other :u64)))
  (subq (:%q other) (:%q result)))

(define-x8664-vinsn %natural--c (((result :u64))
                                ((result :u64)
                                 (constant :s32const)))
  (subq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-logior (((result :u64))
                                    ((result :u64)
                                     (other :u64)))
  (orq (:%q other) (:%q result)))

(define-x8664-vinsn %natural-logior-c (((result :u64))
                                      ((result :u64)
                                       (constant :s32const)))
  (orq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-logand (((result :u64))
                                    ((result :u64)
                                     (other :u64)))
  (andq (:%q other) (:%q result)))

(define-x8664-vinsn %natural-logand-c (((result :u64))
                                      ((result :u64)
                                       (constant :s32const)))
  (andq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-logxor (((result :u64))
                                    ((result :u64)
                                     (other :u64)))
  (xorq (:%q other) (:%q result)))

(define-x8664-vinsn %natural-logxor-c (((result :u64))
                                       ((result :u64)
                                        (constant :s32const)))
  (xorq (:$l constant) (:%q result)))

(define-x8664-vinsn natural-shift-left (((dest :u64))
                                        ((dest :u64)
                                         (amt :u8const)))
  (shlq (:$ub amt) (:%q dest)))

(define-x8664-vinsn natural-shift-right (((dest :u64))
                                         ((dest :u64)
                                          (amt :u8const)))
  (shrq (:$ub amt) (:%q dest)))

(define-x8664-vinsn trap-unless-array-header (()
                                              ((object :lisp))
                                              ((tag :u8)))
  (movb (:%b object) (:%b tag))
  (andb (:$b x8664::tagmask) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (jne :trap)
  (cmpb (:$b x8664::subtag-arrayH) (:@ x8664::misc-subtag-offset (:%q object)))
  (je :ok)
  :trap
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-arrayH))
  :ok)

(define-x8664-vinsn check-arrayH-rank (()
                                       ((header :lisp)
                                        (expected :u32const))
                                       ((rank :imm)))
  (movl (:$l (:apply ash expected x8664::fixnumshift)) (:%l rank))
  (cmpq (:@ x8664::arrayH.rank (:%q header)) (:%q rank))
  (je.pt :ok)
  (uuo-error-array-rank (:%q header) (:%q rank))
  :ok)

(define-x8664-vinsn check-arrayH-flags (()
                                       ((header :lisp)
                                        (expected :u32const)
                                        (type-error :u8const)))
  (cmpq (:$l (:apply ash expected x8664::fixnumshift))
        (:@ x8664::arrayH.flags (:%q header)))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q header) (:$ub type-error))
  :ok)

(define-x8664-vinsn misc-ref-c-u16  (((dest :u16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (movzwl (:@ (:apply + x8664::misc-data-offset (:apply ash idx 1)) (:%q v)) (:%l dest)))

(define-x8664-vinsn misc-ref-c-s16  (((dest :s16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (movswq (:@ (:apply + x8664::misc-data-offset (:apply ash idx 1)) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-set-single-float (()
					   ((val :single-float)
					    (v :lisp)
					    (scaled-idx :u32)))
  (movss (:%xmm val) (:@ x8664::misc-data-offset (:% v) (:% scaled-idx))))

(define-x8664-vinsn u16->u32 (((dest :u32))
			      ((src :u16)))
  (movzwl (:%w src) (:%l dest)))

(define-x8664-vinsn u8->u32 (((dest :u32))
			     ((src :u8)))
  (movzbl (:%b src) (:%l dest)))


(define-x8664-vinsn s16->s32 (((dest :s32))
			      ((src :s16)))
  (movswl (:%w src) (:%l dest)))

(define-x8664-vinsn s8->s32 (((dest :s32))
			     ((src :s8)))
  (movsbl (:%b src) (:%l dest)))

(define-x8664-subprim-jump-vinsn (tail-call-fn-gen) .SPtcallnfngen)

(define-x8664-subprim-jump-vinsn (tail-call-fn-vsp) .SPtcallnfnvsp)

(define-x8664-vinsn set-eq-bit (()
                                ())
  (testb (:%b x8664::arg_z) (:%b x8664::arg_z)))

(define-x8664-vinsn %schar8 (((char :imm))
			    ((str :lisp)
			     (idx :imm))
			    ((imm :u32)))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub x8664::fixnumshift) (:%q imm))
  (movzbl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (shll (:$ub x8664::charcode-shift) (:%l imm))
  (leaq (:@ x8664::subtag-character (:%q imm)) (:%q char)))

(define-x8664-vinsn %schar32 (((char :imm))
                              ((str :lisp)
                               (idx :imm))
                              ((imm :u32)))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub 1) (:%q imm))
  (movl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (shll (:$ub x8664::charcode-shift) (:%l imm))
  (leaq (:@ x8664::subtag-character (:%q imm)) (:%q char)))


(define-x8664-vinsn %set-schar8 (()
                                 ((str :lisp)
                                  (idx :imm)
                                  (char :imm))
                                 ((imm0 :u64)
                                  (imm1 :u64)))
  (movq (:%q idx) (:%q imm0))
  (movl (:%l char) (:%l imm1))
  (shrq (:$ub x8664::fixnumshift) (:%q imm0))
  (shrl (:$ub x8664::charcode-shift) (:%l imm1))
  (movb (:%b imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm0))))

(define-x8664-vinsn %set-schar32 (()
                                 ((str :lisp)
                                  (idx :imm)
                                  (char :imm))
                                 ((imm0 :u64)
                                  (imm1 :u64)))
  (movq (:%q idx) (:%q imm0))
  (movl (:%l char) (:%l imm1))
  (shrq (:$ub 1) (:%q imm0))
  (shrl (:$ub x8664::charcode-shift) (:%l imm1))
  (movl (:%l imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm0))))

(define-x8664-vinsn misc-set-c-single-float (((val :single-float))
					     ((v :lisp)
					      (idx :u32const)))
  (movsd (:%xmm val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2))(:%q v))))

(define-x8664-vinsn array-data-vector-ref (((dest :lisp))
					   ((header :lisp)))
  (movq (:@ x8664::arrayH.data-vector (:%q header)) (:%q dest)))

(define-x8664-subprim-call-vinsn (subtag-misc-ref) .SPsubtag-misc-ref)

(define-x8664-subprim-call-vinsn (subtag-misc-set) .SPsubtag-misc-set)

(define-x8664-vinsn mem-ref-c-absolute-u8 (((dest :u8))
                                           ((addr :s32const)))
  (movzbl (:@ addr) (:%l dest)))

(define-x8664-vinsn mem-ref-c-absolute-s8 (((dest :s8))
                                           ((addr :s32const)))
  (movsbq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-u16 (((dest :u16))
                                           ((addr :s32const)))
  (movzwl (:@ addr) (:%l dest)))

(define-x8664-vinsn mem-ref-c-absolute-s16 (((dest :s16))
                                           ((addr :s32const)))
  (movswq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-fullword (((dest :u32))
                                                 ((addr :s32const)))
  (movl (:@ addr) (:%l dest)))

(define-x8664-vinsn mem-ref-c-absolute-signed-fullword (((dest :s32))
                                                        ((addr :s32const)))
  (movslq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-doubleword (((dest :s64))
                                                   ((addr :s32const)))
  (movq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-signed-doubleword (((dest :s64))
                                                          ((addr :s32const)))
  (movq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-natural (((dest :u64))
                                                   ((addr :s32const)))
  (movq (:@ addr) (:%q dest)))

(define-x8664-vinsn event-poll (()
                                ())
  (btrq (:$ub 63) (:@ (:%seg :rcontext) x8664::tcr.interrupt-pending))
  (jae :no-interrupt)
  (ud2a)
  (:byte 2)
  :no-interrupt)

;;; Return dim1 (unboxed)
(define-x8664-vinsn check-2d-bound (((dim :u64))
				    ((i :imm)
				     (j :imm)
				     (header :lisp)))
  (cmpq (:@ (+ x8664::misc-data-offset (* 8 x8664::arrayH.dim0-cell)) (:%q header))
        (:%q i))
  (jb :i-ok)
  (uuo-error-array-bounds (:%q i) (:%q header))
  :i-ok
  (movq (:@ (+ x8664::misc-data-offset (* 8 (1+ x8664::arrayH.dim0-cell))) (:%q header))
        (:%q dim))
  (cmpq (:%q dim) (:%q j))
  (jb :j-ok)
  (uuo-error-array-bounds (:%q j) (:%q header))
  :j-ok
  (sarq (:$ub x8664::fixnumshift) (:%q dim)))

(define-x8664-vinsn 2d-dim1 (((dest :u64))
			     ((header :lisp)))
  (movq (:@ (+ x8664::misc-data-offset (* 8 (1+ x8664::arrayH.dim0-cell))) (:%q header))
        (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))


(define-x8664-vinsn 2d-unscaled-index (((dest :u64))
				       ((array :lisp)
					(i :imm)
					(j :imm)
					(dim1 :u64)))
  ((:not (:pred =
                (:apply %hard-regspec-value dim1)
                (:apply %hard-regspec-value dest)))
   (movq (:% dim1) (:% dest)))
  (imulq (:%q i) (:%q dest))
  (addq (:%q j) (:%q dest)))

(define-x8664-vinsn branch-unless-both-args-fixnums (()
                                                     ((a :lisp)
                                                      (b :lisp)
                                                      (dest :label))
                                                     ((tag :u8)))
  (movb (:%b a) (:%b tag))
  (orb (:%b b) (:%b tag))
  (testb (:$b x8664::fixnummask) (:%b tag))
  (jne dest))

(define-x8664-vinsn branch-unless-arg-fixnum (()
                                              ((a :lisp)
                                               (dest :label)))
  (testb (:$b x8664::fixnummask) (:%b a))
  (jne dest))

(define-x8664-vinsn fixnum->single-float (((f :single-float))
                                          ((arg :lisp))
                                          ((unboxed :s64)))
  (movq (:%q arg) (:%q unboxed))
  (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
  (cvtsi2ssq (:%q unboxed) (:%xmm f)))

(define-x8664-vinsn fixnum->double-float (((f :double-float))
                                          ((arg :lisp))
                                          ((unboxed :s64)))
  (movq (:%q arg) (:%q unboxed))
  (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
  (cvtsi2sdq (:%q unboxed) (:%xmm f)))


(queue-fixup
 (fixup-x86-vinsn-templates
  *x8664-vinsn-templates*
  x86::*x86-64-opcode-template-lists*))

(provide "X8664-VINSNS")

