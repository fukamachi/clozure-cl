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

(define-x8664-vinsn misc-ref-double-float  (((dest :double-float))
                                            ((v :lisp)
                                             (scaled-idx :imm)))
  (movsd (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-node  (((dest :lisp))
                                    ((v :lisp)
                                     (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-set-node (()
				   ((val :lisp)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movq (:%q val) (:@ x8664::misc-data-offset (:%q  v) (:%q unscaled-idx))))

(define-x8664-vinsn misc-ref-u8 (((dest :u8))
                                 ((v :lisp)
                                  (scaled-idx :s64)))
  (movzbq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

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
  (movl (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%l dest)))

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
  (movzbq (:@ (:apply + x8664::misc-data-offset idx) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-ref-c-s8  (((dest :s64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movsbq (:@ (:apply + x8664::misc-data-offset idx) (:%q v)) (:%q dest)))

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


(define-x8664-vinsn misc-set-c-node (()
				    ((val :lisp)
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
  ((:pred = n 0)
   (testw (:%w x8664::nargs) (:%w x8664::nargs)))
  ((:not (:pred = n 0))
   (cmpw (:$w (:apply ash n x8664::word-shift)) (:%w x8664::nargs)))
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


(define-x8664-vinsn save-lisp-context-offset (()
					      ((nbytes-vpushed :s32const))
					      ((temp :u64)))
  (leaq (:@ nbytes-vpushed (:%q x8664::rsp)) (:%q temp))
  (movq (:%q x8664::rbp) (:@ (:%q temp)))
  (movq (:%q temp) (:%q x8664::rbp))
  (movq (:% x8664::ra0) (:@ 8 (:%q x8664::rbp))))


(define-x8664-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (pushq (:% reg)))

(define-x8664-vinsn vframe-load (((dest :lisp))
				 ((frame-offset :u16const)
				  (cur-vsp :u16const)))
  (movq (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp)) (:%q dest)))

(define-x8664-vinsn vframe-store (()
				 ((src :lisp)
                                  (frame-offset :u16const)
				  (cur-vsp :u16const)))
  (movq (:%q src) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))

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
   (xorl (:%l dest) (:%l dest)))
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

(define-x8664-vinsn trap-unless-single-float (()
                                              ((object :lisp))
                                              ((tag :u8)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-single-float) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::tag-single-float))
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
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (cmovew (:@ x8664::misc-subtag-offset (:%q object)) (:%w tag))
  (cmpb (:$b tagval) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub tagval))
  :ok)

(define-x8664-vinsn trap-unless-double-float (()
                                              ((object :lisp))
                                              ((tag :u8)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (cmovew (:@ x8664::misc-subtag-offset (:%q object)) (:%w tag))
  (cmpb (:$b x8664::subtag-double-float) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-double-float))
  :ok)

(define-x8664-vinsn trap-unless-macptr (()
                                        ((object :lisp))
                                        ((tag :u8)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (cmovew (:@ x8664::misc-subtag-offset (:%q object)) (:%w tag))
  (cmpb (:$b x8664::subtag-macptr) (:%b tag))
  (je.pt :ok)
  (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-macptr))
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


(define-x8664-vinsn (load-nil :constant-ref) (((dest t))
					      ())
  (movl (:$l x8664::nil-value) (:%l dest)))


(define-x8664-vinsn (load-t :constant-ref) (((dest t))
					    ())
  (movl(:$l x8664::t-value) (:%l dest)))


(define-x8664-vinsn extract-tag (((tag :u8))
                                 ((object :lisp)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag)))

(define-x8664-vinsn extract-tag-fixnum (((tag :imm))
					((object :lisp)))
  (leal (:@ (:%q object) 8) (:%l tag))
  (andl (:$b (ash x8664::tagmask x8664::fixnumshift)) (:%l tag)))

(define-x8664-vinsn extract-fulltag (((tag :u8))
                                 ((object :lisp)))
  (movb (:$b x8664::fulltagmask) (:%b tag))
  (andb (:%b object) (:%b tag)))

(define-x8664-vinsn extract-fulltag-fixnum (((tag :imm))
                                            ((object :lisp)))
  (leal (:@ (:%q object) 8) (:%l tag))
  (andl (:$b (ash x8664::fulltagmask x8664::fixnumshift)) (:%l tag)))

(define-x8664-vinsn extract-typecode (((tag :u8))
                                      ((object :lisp)))
  (movb (:$b x8664::tagmask) (:%b tag))
  (andb (:%b object) (:%b tag))
  (cmpb (:$b x8664::tag-misc) (:%b tag))
  (cmovew (:@ x8664::misc-subtag-offset (:%q object)) (:%w tag)))

(define-x8664-vinsn extract-typecode-fixnum (((tag :imm))
                                             ((object :lisp))
                                             ((temp :u8)))
  (movb (:$b x8664::tagmask) (:%b temp))
  (andb (:%b object) (:%b temp))
  (cmpb (:$b x8664::tag-misc) (:%b temp))
  (cmovew (:@ x8664::misc-subtag-offset (:%q object)) (:%w temp))
  (movzbl (:%b temp) (:%l temp))
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
  (setcc (:$ub crbit) (:%b temp))
  (andl (:$b x8664::t-offset) (:%l temp))
  (leaq (:@ x8664::nil-value (:%q temp)) (:%q dest)))

#+no                                    ; this is larger than using setcc
(define-x8664-vinsn cr-bit->boolean (((dest :lisp))
                                     ((crbit :u8const))
                                     ((temp :u32)))
  (movl (:$l x8664::t-value) (:%l temp))
  (leaq (:@ (- x8664::t-offset) (:%q temp)) (:%q dest))
  (cmovccq (:$ub crbit) (:%q  temp) (:%q dest)))

(define-x8664-vinsn compare-s32-constant (()
                                            ((val :imm)
                                             (const :s32const)))
  ((:or  (:pred < const 128) (:pred > const 127))
   (cmpq (:$l const) (:%q val)))
  ((:not (:or  (:pred < const 128) (:pred > const 127)))
   (cmpq (:$b const) (:%q val))))

(define-x8664-vinsn compare-u8-constant (()
                                         ((val :u8)
                                          (const :u8const)))
  ((:pred logbitp 7 const)
   (movzbl (:%b val) (:%l val))
   (cmpw (:$w const) (:%w val)))
  ((:not (:pred logbitp 7 const))
   (cmpb (:$b const) (:%b val))))


(define-x8664-vinsn cons (((dest :lisp))
                          ((car :lisp)
                           (cdr :lisp)))
  (subq (:$b (- x8664::cons.size x8664::fulltag-cons)) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr))
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr) (:%q x8664::allocptr))
  (cmpq (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocbase) (:%q x8664::allocptr))
  (jg :no-trap)
  (uuo-alloc)
  :no-trap
  (andb (:$b (lognot x8664::fulltagmask)) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr))
  (movq (:%q car) (:@ x8664::cons.car (:%q x8664::allocptr)))
  (movq (:%q cdr) (:@ x8664::cons.cdr (:%q x8664::allocptr)))
  (movq (:%q x8664::allocptr) (:%q dest)))

(define-x8664-vinsn unbox-u8 (((dest :u8))
			      ((src :lisp)))
  (movq (:$l (ash #xff x8664::fixnumshift)) (:%q dest))
  (andq (:% src) (:% dest))
  (je.pt :ok)
  (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-8))
  :ok
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))


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
  (cmpq (:%q y) (:%q x)))

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
  (movq (:$q #xffff000000000000) (:%q header))
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
                                                       ((src :s64))
                                                       ((temp :s64)))
  (movq (:%q src) (:%q temp))
  (shlq (:$ub x8664::fixnumshift) (:%q temp))
  (movq (:%q temp) (:%q dest))          ; tagged as a fixnum
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (cmpq (:%q src) (:%q temp)))


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
  (subq (:%q x8664::imm1) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr))
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr) (:%q freeptr))
  (cmpq (:%q freeptr) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocbase))
  (jg :no-trap)
  (uuo-alloc)
  :no-trap
  (movq (:%q header) (:@ x8664::misc-header-offset (:%q freeptr)))
  (andb (:$b (lognot x8664::fulltagmask)) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr))
  ((:not (:pred = freeptr
                (:apply %hard-regspec-value dest)))
   (movq (:%q freeptr) (:%q dest))))

(define-x8664-vinsn set-bigits-after-fixnum-overflow (()
                                                      ((bignum :lisp)))
  (movq (:%mmx x8664::mm0) (:@ x8664::misc-data-offset (:%q bignum))))
  
                                                       
(define-x8664-vinsn box-fixnum (((dest :imm))
                                ((src :s8)))
  (leaq (:@ (:%q src) x8664::fixnumone) (:%q dest)))

(define-x8664-vinsn fix-fixnum-overflow (((val :lisp))
                                         ((val :lisp))
                                         ((unboxed (:s64 #.x8664::imm1))
                                          (header (:u64 #.x8664::imm0))
                                          (freeptr (:lisp #.x8664::allocptr))))
  (jno.pt :done)
  (movq (:%q val) (:%q unboxed))
  (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
  (movq (:$q #xffff000000000000) (:%q header))
  (xorq (:%q header) (:%q unboxed))
  (movd (:%q unboxed) (:%mmx x8664::mm0))
  (movq (:$l x8664::two-digit-bignum-header) (:%q header))
  (movq (:$l (- 16 x8664::fulltag-misc)) (:%q unboxed))
  (subq (:%q x8664::imm1) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr))
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr) (:%q freeptr))
  (cmpq (:%q freeptr) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocbase))
  (jg :no-trap)
  (uuo-alloc)
  :no-trap
  (movq (:%q header) (:@ x8664::misc-header-offset (:%q freeptr)))
  (andb (:$b (lognot x8664::fulltagmask)) (:@ (:%seg x8664::rcontext) x8664::tcr.save-allocptr))
  (movq (:%mmx x8664::mm0) (:@ x8664::misc-data-offset (:%q freeptr)))
  ((:not (:pred = freeptr
                (:apply %hard-regspec-value val)))
   (movq (:%q freeptr) (:%q val)))
  :done)

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


;;; If nothing's been pushed by the caller, we need to "reserve a stack frame"
;;; here.
(define-x8664-vinsn ensure-reserved-frame (()
                                           ())
  (cmpw (:$w (* 3 x8664::node-size)) (:%w x8664::nargs))
  (jbe :no-reserve)
  (pushq (:$b 0))
  (pushq (:$b 0))
  :no-reserve)

                                           
(define-x8664-vinsn (vpush-argregs :push :node :vsp) (()
                                                      ())
  (testw (:%w x8664::nargs) (:%w x8664::nargs))
  (jz :done)
  (cmpw (:$w (* 2 x8664::node-size)) (:%w x8664::nargs))
  (jl :one)
  (je :two)
  (pushq (:%q x8664::arg_x))
  :two
  (pushq (:%q x8664::arg_y))
  :one
  (pushq (:%q x8664::arg_z))
  :done)

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
              

(define-x8664-vinsn double-float+-2 (((result :double-float))
				     ((result :double-float)
				      (x :double-float)))
  (addsd (:%xmm x) (:%xmm result)))

(define-x8664-vinsn double-float--2 (((result :double-float))
				     ((result :double-float)
				      (x :double-float)))
  (subsd (:%xmm x) (:%xmm result)))

(define-x8664-vinsn double-float*-2 (((result :double-float))
				     ((result :double-float)
				      (x :double-float)))
  (mulsd (:%xmm x) (:%xmm result)))

(define-x8664-vinsn double-float/-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float)))
  (divsd (:%xmm x) (:%xmm result)))

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
   (addss (:%xmm x) (:%xmm result))))

(define-x8664-vinsn single-float--2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  (subss (:%xmm x) (:%xmm result)))

(define-x8664-vinsn single-float*-2 (((result :single-float))
				     ((result :single-float)
				      (x :single-float)))
  (divss (:%xmm x) (:%xmm result)))

(define-x8664-vinsn single-float/-2 (((result :single-float))
				     ((result :single-float)
				      (x :single-float)))
  (divss (:%xmm x) (:%xmm result)))

(define-x8664-vinsn get-single (((result :single-float))
                                ((source :lisp)))
  (movq (:%q source) (:@ (:%seg x8664::rcontext) x8664::tcr.single-float-convert))
  (movss (:@ (:%seg x8664::rcontext) x8664::tcr.single-float-convert.value) (:%xmm result)))

(define-x8664-vinsn get-double (((result :double-float))
                                ((source :lisp)))
  (movsd (:@  x8664::double-float.value (:%q source)) (:%xmm result)))

(define-x8664-vinsn single->node (((result :lisp))
                                  ((source :single-float)))
  (movss (:%xmm source) (:@ (:%seg x8664::rcontext) x8664::tcr.single-float-convert.value))
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.single-float-convert) (:%q result)))

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


(define-x8664-vinsn start-mv-call (()
                                   ((label :label)))
  (leaq (:@ (:^ label) (:%q x8664::fn)) (:%q x8664::ra0))
  (pushq (:%q x8664::ra0)))


(define-x8664-vinsn emit-aligned-label (()
                                        ((label :label)))
  (:align 3)
  (:long (:^ label)))

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
  (leaq (:@ (:^ :bad) (:%q x8664::fn)) (:%q x8664::fn))
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::fn))
  (cmovgq (:%q x8664::temp0) (:%q x8664::fn))
  (jmp (:%q x8664::fn))
  (:align 3)
  (:long (:^ :bad))
  :bad
  ;; If we don't do this (and leave %fn as a TRA into itself), reporting
  ;; the error is likely a little harder.  Tough.
  ;; (leaq (@ (:apply - (:^ :bad)) (:%q x8664::rn)) (:%q x8664::fn))
  (uuo-error-not-callable))



(define-x8664-vinsn reserve-outgoing-frame (()
                                            ())
  (pushq (:$b 0))
  (pushq (:$b 0)))


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
				   ((tempa :imm)
                                    (tempb :imm)))
  (movd (:%mmx x8664::tsp) (:%q tempa))
  (movq (:%q tempa) (:%q tempb))
  (subq (:$b (+ x8664::cons.size x8664::dnode-size)) (:%q tempb))
  (movd (:%q tempb) (:%mmx x8664::next-tsp))
  (movapd (:%xmm x8664::fp0) (:@ -32 (:%q tempa)))
  (movapd (:%xmm x8664::fp0) (:@ -16 (:%q tempa)))
  (movq (:%mmx x8664::next-tsp) (:%mmx x8664::tsp))
  (movq (:%q tempa) (:@ (:%q tempb)))
  (movq (:%q car) (:@ (+ x8664::dnode-size x8664::cons.car) (:%q tempb)))
  (movq (:%q cdr) (:@ (+ x8664::dnode-size x8664::cons.cdr) (:%q tempb)))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-cons) (:%q tempb)) (:%q dest)))

(define-x8664-vinsn make-fixed-stack-gvector (((dest :lisp))
                                              ((aligned-size :u32const)
                                               (header :s32const))
                                              ((tempa :imm)
                                               (tempb :imm)))
  (movd (:%mmx x8664::tsp) (:%q tempa))
  (movq (:%q tempa) (:%q tempb))
  ((:and (:pred >= aligned-size -128) (:pred <= aligned-size 127))
   (subq (:$b (:apply + aligned-size x8664::dnode-size)) (:%q tempa)))
  ((:not (:and (:pred >= aligned-size -128) (:pred <= aligned-size 127)))
   (subq (:$l (:apply + aligned-size x8664::dnode-size)) (:%q tempa)))
  (movd (:%q tempb) (:%mmx x8664::next-tsp))
  :loop
  (movapd (:%xmm x8664::fp0) (:@ -16 (:%q tempa)))
  (subq (:$b 16) (:%q tempa))
  (cmpq (:%q tempa) (:%q tempb))
  (jnz :loop)
  ((:and (:pred >= aligned-size -128) (:pred <= aligned-size 127))
   (addq (:$b (:apply + aligned-size x8664::dnode-size)) (:%q tempa)))
  ((:not (:and (:pred >= aligned-size -128) (:pred <= aligned-size 127)))
   (addq (:$l (:apply + aligned-size x8664::dnode-size)) (:%q tempa)))
  (movd (:%q tempb) (:%mmx  x8664::tsp))
  (movq (:%q tempa) (:@ (:%q tempb)))
  (movl (:$l header) (:@ x8664::dnode-size (:%q tempb)))
  (leaq (:@ (+ x8664::dnode-size x8664::misc-data-offset) (:%q tempb)) (:%q dest)))


(define-x8664-vinsn discard-temp-frame (()
					()
                                        ((temp :imm)))
  (movd (:%mmx x8664::tsp) (:%q temp))
  (movd (:@ (:%q temp)) (:%mmx x8664::tsp))
  (movq (:%mmx x8664::tsp) (:%mmx x8664::next-tsp)))

  
(define-x8664-vinsn vstack-discard (()
				    ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   (leaq (:@ (:apply ash nwords x8664::word-shift) (:%q x8664::rsp)) (:%q x8664::rsp))))


(defmacro define-x8664-subprim-call-vinsn ((name &rest other-attrs) spno &optional (recover-fn nil))
  `(define-x8664-vinsn (,name :call :subprim-call ,@other-attrs) (() ())
    (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
    (jmp (:@ ,spno))
    (:align 3)
    (:long (:^ :back))
    :back
    ,@(if recover-fn
          `((leaq (:@ (:apply - (:^ :back)) (:%q x8664::ra0)) (:%q x8664::fn))))))

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


(define-x8664-vinsn init-closure (()
                                  ((closure :lisp)))
  (movb (:$b 2) (:@ x8664::misc-data-offset (:%q closure))) ; code word count
  (movb (:$b -1) (:@ (+ x8664::misc-data-offset 7) (:%q closure))) ; 1st byte of jmp
  (movl (:$l (:apply logior #x2524 (:apply ash .SPcall-closure 16))) (:@ (+ x8664::misc-data-offset 8) (:%q closure)))) ; rest of jmp instruction, subprim address

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
  (cmpq (:@ (:%seg x8664::rcontext) x8664::tcr.tlb-limit) (:%q idx))
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.tlb-pointer) (:%q table))
  (jae :symbol)
  (movq (:@ (:%q table) (:%q idx)) (:%q dest))
  (cmpb (:$b x8664::subtag-no-thread-local-binding) (:%b dest))
  (jne :test)
  :symbol
  (movq (:@ x8664::symbol.vcell (:%q src)) (:%q dest))
  :test
  (cmpb (:$b x8664::unbound-marker) (:%b dest))
  (jne.pt :done)
  (uuo-error-reg-not-tag (:%q src) (:$ub x8664::unbound-marker))
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
  (cmpq (:@ (:%seg x8664::rcontext) x8664::tcr.tlb-limit) (:%q idx))
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.tlb-pointer) (:%q table))
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
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.tlb-pointer) (:%q temp))
  (movq (:@ x8664::INTERRUPT-LEVEL-BINDING-INDEX (:%q temp)) (:%q dest)))

(define-x8664-vinsn save-cleanup-context (()
                                          ((lab :label)))
  (leaq (:@ (:apply - (:^ lab)) (:%q x8664::xfn)) (:%q x8664::fn))
  (pushq (:%q x8664::ra0))
  )

(define-x8664-vinsn restore-cleanup-context (()
                                          ())
  (popq (:%q x8664::ra0))
  )

(define-x8664-vinsn setup-double-float-allocation (()
                                                   ())
  (movl (:$l (arch::make-vheader x8664::double-float.element-count x8664::subtag-double-float)) (:%l x8664::imm0.l))
  (movl (:$l (- x8664::double-float.size x8664::fulltag-misc)) (:%l x8664::imm0.l)))

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
  (andl (:$l x8664::fixnumone) (:%l bitnum))
  (movl (:%l bitnum) (:%l dest)))
                                            
                                                      
(define-x8664-vinsn misc-ref-c-bit-fixnum (((dest :imm))
                                           ((src :lisp)
                                            (idx :u64const))
                                           ((temp :u8)))
  (btq (:$ub (:apply logand 63 idx))
       (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src)))
  (setb (:%b temp))
  (andl (:$l x8664::fixnumone) (:%l temp))
  (movl (:%l temp) (:%l dest)))

(define-x8664-vinsn deref-macptr (((addr :address))
				  ((src :lisp))
				  ())
  (movq (:@ x8664::macptr.address (:%q src)) (:%q addr)))

(define-x8664-vinsn (temp-push-unboxed-word :push :word :csp)
    (()
     ((w :u64))
     ((temp :u64)))
  (subq (:$b 16) (:@ (:%seg x8664::rcontext) x8664::tcr.foreign-sp))
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.foreign-sp) (:%q temp))
  (movq (:%q w) (:@ (:%q temp))))

(define-x8664-vinsn (temp-pop-unboxed-word :pop :word :csp)
    (((w :u64))
     ())
  (movq (:@ (:%seg x8664::rcontext) x8664::tcr.foreign-sp) (:%q w))
  (movq (:@ (:%q w)) (:%q w))
  (addq (:$b 16) (:@ (:%seg x8664::rcontext) x8664::tcr.foreign-sp)))

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

(define-x8664-vinsn mem-set-indirect-doubleword (()
                                                 ((val :address)
                                                  (dest :address)))
  (movq (:%q val) (:@ (:%q dest))))

(define-x8664-vinsn mem-set-c-doubleword (()
                                          ((val :address)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movq (:%q val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movq (:%q val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-doubleword (()
                                                   ((val :s32const)
                                                    (dest :address)
                                                    (offset :s32const)))
  ((:pred = offset 0)
   (movq (:$l val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movq (:$l val) (:@ offset (:%q dest)))))


(define-x8664-vinsn mem-set-constant-doubleword (()
                                                   ((val :s32const)
                                                    (dest :address)
                                                    (offset :s64)))
   (movq (:$l val) (:@ (:%q dest) (:%q offset))))