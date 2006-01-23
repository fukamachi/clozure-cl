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
				      (idx :s32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn misc-ref-c-u8  (((dest :u64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movzbq (:@ (:apply + x8664::misc-data-offset idx) (:%q v)) (:%q dest)))

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
  (movd (:%q unboxed) (:%mmx x8664::mmx0))
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
  (movq (:%mmx x8664::mmx0) (:@ x8664::misc-data-offset (:%q freeptr)))
  ((:not (:pred = freeptr
                (:apply %hard-regspec-value val)))
   (movq (:%q freeptr) (:%q val)))
  :done)

(define-x8664-vinsn fix-fixnum-overflow-ool (((val :lisp))
                                             ((val :lisp))
                                             ((unboxed (:s64 #.x8664::imm1))
                                              (header (:u64 #.x8664::imm0))
                                              (freeptr (:lisp #.x8664::allocptr))))
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
