;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2006, Clozure Associates and contributors
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

(in-package "CCL")

(defx86lapfunction %function-vector-to-function ((arg arg_z))
  (trap-unless-fulltag= arg x8664::fulltag-function)
  (addb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (% arg_z.b))
  (single-value-return))

(defx86lapfunction %function-to-function-vector  ((arg arg_z))
  (trap-unless-fulltag= arg x8664::fulltag-function)
  (subb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (% arg_z.b))
  (single-value-return))

(defx86lapfunction %function-code-words ((fun arg_z))
  (trap-unless-fulltag= fun x8664::fulltag-function)
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %nth-immediate ((fun arg_y) (n arg_z))
  (trap-unless-fulltag= fun x8664::fulltag-function)
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (lea (@ (% n) (% imm0) 8) (% imm0))
  (movq (@ (- x8664::node-size x8664::fulltag-function) (% fun) (% imm0))
        (% arg_z))
  (single-value-return))

(defx86lapfunction %set-nth-immediate ((fun arg_x) (n arg_y) (new arg_z))
  (trap-unless-fulltag= fun x8664::fulltag-function)
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (lea (@ (% n) (% imm0) 8) (% arg_y))
  (subb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (%b arg_x))
  (jmp-subprim .SPgvset))

(defx86lapfunction %make-code-executable ((codev arg_z))
  (single-value-return))

;;; Make a new function, with PROTO's code and the specified immediates.
;;; IMMEDIATES should contain lfun-bits as the last element.
(defun %clone-x86-function (proto &rest immediates)
  (declare (dynamic-extent immediates))
  (let* ((protov (%function-to-function-vector proto))
         (code-words (%function-code-words proto))
         (numimms (length immediates))
         (newv (allocate-typed-vector :function (the fixnum (+ code-words numimms)))))
    (declare (fixnum code-words numimms))
    (%copy-ivector-to-ivector protov 0 newv 0 (the fixnum (ash code-words target::word-shift)))
    (do* ((k code-words (1+ k))
          (imms immediates (cdr imms)))
         ((null imms) (%function-vector-to-function newv))
      (declare (fixnum k) (list imms))
      (setf (%svref newv k) (car imms)))))

(defun replace-function-code (target proto)
  (let* ((target-words (%function-code-words target))
         (proto-words (%function-code-words proto)))
    (declare (fixnum target-words proto-words))
    (if (= target-words proto-words)
      (progn
        (%copy-ivector-to-ivector (%function-to-function-vector proto)
                                  0
                                  (%function-to-function-vector target)
                                  0
                                  (the fixnum (ash target-words
                                                   target::word-shift)))
        target)
      (error "Code size mismatch: target = ~s, proto = ~s"
             target-words proto-words))))
         

(defx86lapfunction %get-kernel-global-from-offset ((offset arg_z))
  (check-nargs 1)
  (unbox-fixnum offset imm0)
  (movq (@ target::nil-value (% imm0)) (% arg_z))
  (single-value-return))

(defx86lapfunction %set-kernel-global-from-offset ((offset arg_y) (new-value arg_z))
  (check-nargs 2)
  (unbox-fixnum offset imm0)
  (movq (% arg_z) (@ target::nil-value (% imm0)))
  (single-value-return))


(defx86lapfunction %get-kernel-global-ptr-from-offset ((offset arg_y)
						       (ptr arg_z))
  (check-nargs 2)
  (unbox-fixnum offset imm0)
  (movq (@ target::nil-value (% imm0)) (% imm0))
  (movq (% imm0) (@ x8664::macptr.address (% ptr)))
  (single-value-return))




(defx86lapfunction %fixnum-ref ((fixnum arg_y) #| &optional |# (offset arg_z))
  (check-nargs 1 2)
  (cmpw ($ x8664::fixnumone) (% nargs))
  (jne @2-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @2-args
  (unbox-fixnum offset imm0)
  (movq (@ (% fixnum) (% imm0)) (% arg_z))
  (single-value-return))

(defx86lapfunction %fixnum-ref-natural ((fixnum arg_y) #| &optional |# (offset arg_z))
  (check-nargs 1 2)
  (cmpw ($ x8664::fixnumone) (% nargs))
  (jne @2-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @2-args
  (unbox-fixnum offset imm0)
  (movq (@ (% fixnum) (% imm0)) (% imm0))
  (jmp-subprim .SPmakeu64))

(defx86lapfunction %fixnum-set ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (check-nargs 2 3)
  (cmpw ($ '2) (% nargs))
  (jne @3-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @3-args
  (unbox-fixnum offset imm0)
  (movq (% new-value) (@ (% fixnum) (% imm0)))
  (movq (% new-value) (% arg_z))
  (single-value-return))



(defx86lapfunction %fixnum-set-natural ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (check-nargs 2 3)
  (save-simple-frame)
  (cmpw ($ '2) (% nargs))
  (jne @3-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @3-args
  (call-subprim .SPgetu64)
  (unbox-fixnum offset imm1)
  (movq (% imm0) (@ (% fixnum) (% imm1)))
  (restore-simple-frame)
  (single-value-return))


(defx86lapfunction %current-frame-ptr ()
  (check-nargs 0)
  (movq (% rbp) (% arg_z))
  (single-value-return))


(defx86lapfunction %current-tsp ()
  (check-nargs 0)
  (movd (% tsp) (% arg_z))
  (single-value-return))


(defx86lapfunction %%frame-backlink ((p arg_z))
  (check-nargs 1)
  (movq (@ (% arg_z)) (% arg_z))
  (single-value-return))


;;; It's always been the case that the function associated with a
;;; frame pointer is the caller of the function that "uses" that frame.
(defx86lapfunction %cfp-lfun ((p arg_z))
  (movq (@ x8664::lisp-frame.return-address (% p)) (% arg_y))
  (extract-lisptag arg_y imm0)
  (cmpb ($ x8664::tag-tra) (%b imm0))
  (jne @no)
  (movl (@ -4 (% arg_y)) (%l imm0))
  (subq (% imm0) (% arg_y))
  (box-fixnum imm0 arg_z)
  (movq (% rsp) (% temp0))
  (pushq (% arg_y))
  (pushq (% arg_z))
  (set-nargs 2)
  (jmp-subprim .SPvalues)
  @no
  (movq (% rsp) (% temp0))
  (pushq ($ x8664::nil-value))
  (pushq ($ x8664::nil-value))
  (set-nargs 2)
  (jmp-subprim .SPvalues))



(defx86lapfunction %uvector-data-fixnum ((uv arg_z))
  (check-nargs 1)
  (trap-unless-fulltag= arg_z x8664::fulltag-misc)
  (addq ($ x8664::misc-data-offset) (% arg_z))
  (single-value-return))

(defx86lapfunction %catch-top ((tcr arg_z))
  (check-nargs 1)
  (movl ($ x8664::nil-value) (%l arg_y))
  (movq (@ (% rcontext) x8664::tcr.catch-top) (% arg_z))
  (testb (%b arg_z) (%b arg_z))
  (cmoveq (% arg_y) (% arg_z))
  (single-value-return))

(defx86lapfunction %catch-tsp ((catch arg_z))
  (check-nargs 1)
  (lea (@  (- (+ target::fulltag-misc
                                 (ash 1 (1+ target::word-shift)))) (% arg_z))
       (% arg_z))
  (single-value-return))



;;; Same as %address-of, but doesn't cons any bignums
;;; It also left shift fixnums just like everything else.
(defx86lapfunction %fixnum-address-of ((x arg_z))
  (check-nargs 1)
  (box-fixnum x arg_z)
  (single-value-return))

(defx86lapfunction %save-standard-binding-list ((bindings arg_z))
  (movq (@ (% rcontext) x8664::tcr.vs-area) (% imm0))
  (movq (@ x8664::area.high (% imm0)) (% imm1))
  (subq ($ x8664::node-size) (% imm1))
  (movq (% bindings) (@ (% imm1)))
  (single-value-return))

(defx86lapfunction %saved-bindings-address ()
  (movq (@ (% rcontext) x8664::tcr.vs-area) (% imm0))
  (movq (@ x8664::area.high (% imm0)) (% imm1))
  (lea (@ (- x8664::node-size) (% imm1)) (% arg_z))
  (single-value-return))

(defx86lapfunction %get-object ((macptr arg_y) (offset arg_z))
  (check-nargs 2)
  (trap-unless-typecode= macptr x8664::subtag-macptr)
  (macptr-ptr macptr imm0)
  (trap-unless-lisptag= offset target::tag-fixnum imm1)
  (unbox-fixnum offset imm1)
  (movq (@ (% imm0) (% imm1)) (% arg_z))
  (single-value-return))


(defx86lapfunction %set-object ((macptr arg_x) (offset arg_y) (value arg_z))
  (check-nargs 3)
  (trap-unless-typecode= macptr target::subtag-macptr)
  (macptr-ptr macptr imm0)
  (trap-unless-lisptag= offset target::tag-fixnum imm1)
  (unbox-fixnum offset imm1)
  (movq (% arg_z) (@ (% imm0) (% imm1)))
  (single-value-return))

(defx86lapfunction %apply-lexpr-with-method-context ((magic arg_x)
                                                     (function arg_y)
                                                     (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in x8664::next-method-context (= x8664::temp0).
  ;; Put function in x8664::xfn until we're ready to jump to it.
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves x866::xfn/x8664::next-method-context.
  ;; Jump to the function in x8664::xfn.
  (movq (% magic) (% next-method-context))
  (movq (% function) (% xfn))
  (set-nargs 0)
  (movq (@ (% args)) (% imm0))          ;lexpr-count
  (movw (% imm0.w) (% nargs))
  (subw ($ '3) (% imm0.w))
  (jbe @reg-only)
  ;; Some args will be pushed; reserve a frame
  (pushq ($ 0))
  (pushq ($ 0))
  @pushloop
  (pushq (@ (- x8664::node-size) (% imm1)))
  (subq ($ x8664::node-size) (% imm1))
  (subq ($ x8664::node-size) (% imm0))
  (jne @pushloop)
  @three
  (movq (@ (* x8664::node-size 3) (% arg_z)) (% arg_x))
  @two
  (movq (@ (* x8664::node-size 2) (% arg_z)) (% arg_y))
  @one
  (movq (@ (* x8664::node-size 1) (% arg_z)) (% arg_z))
  (jmp @go)
  @reg-only
  (testw (% nargs) (% nargs))
  (je @go)
  (rcmpw (% nargs) ($ '2))
  (je @two)
  (jb @one)
  (jmp @three)
  @go
  (xchgq (% xfn) (% fn))
  (jmp (% fn)))

(defx86lapfunction %apply-with-method-context ((magic arg_x)
                                               (function arg_y)
                                               (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in x8664::next-method-context (= x8664::temp0).
  ;; Put function in x8664::xfn (= x8664::temp1).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves x8664::xfn/x8664::next-method-context.
  ;; Jump to the function in x8664::xfn.
  ;; We need to inline the "spreadargz" operation, 'cause there's no
  ;; good place to keep %ra0.
  (movq (% magic) (% x8664::next-method-context))
  (movq (% function) (% x8664::xfn))
  (movq (% args) (% arg_y))             ; in case of error
  (set-nargs 0)
  (xorl (% imm0.l) (% imm0.l))
  (push (% imm0))                       ; reserve frame (might discard
  (push (% imm0))                       ; it if nothing is passed on stack.)
  (cmp-reg-to-nil arg_z)
  (je @done)
  @loop
  (extract-fulltag arg_z imm1)
  (cmpb ($ x8664::fulltag-cons) (%b imm1))
  (jne @bad)
  (%car arg_z arg_x)
  (%cdr arg_z arg_z)
  (lea (@ x8664::node-size (% imm0)) (% imm0))
  (cmp-reg-to-nil arg_z)
  (push (% arg_x))
  (jne @loop)
  @done
  (addw (% imm0.w) (% nargs))
  (jne @pop)
  @discard-and-go
  (discard-reserved-frame)
  (jmp @go)
  @pop
  (cmpw ($ '1) (% nargs))
  (pop (% arg_z))
  (je @discard-and-go)
  (cmpw ($ '2) (% nargs))
  (pop (% arg_y))
  (je @discard-and-go)
  (cmpw ($ '3) (% nargs))
  (pop (% arg_x))
  (je @discard-and-go)
  @go
  (xchgq (% xfn) (% fn))
  (jmp (% fn))
  @bad
  (addq (% imm0) (% rsp))
  (movq (% arg_y) (% arg_z))
  (movq ($ (ash $XNOSPREAD x8664::fixnumshift)) (% arg_y))
  (set-nargs 2)
  (jmp-subprim .SPksignalerr))


;;; The idea here is to call METHOD in the same stack frame in
;;; which the lexpr was originally called.  The lexpr can't
;;; have had any required arguments, %APPLY-LEXPR-TAIL-WISE
;;; must have been tail-called, and the frame built on lexpr
;;; entry must be in %rbp.
(defx86lapfunction %apply-lexpr-tail-wise ((method arg_y) (args arg_z))
  (uuo-error-debug-trap)
  (movq (% method) (% xfn))
  (movq (% args) (% rsp))
  (pop (%q nargs))
  (movq (@ x8664::lisp-frame.return-address (% rbp)) (% ra0))
  (movq (@ 0 (% rbp)) (% rbp))
  (rcmpw (% nargs) ($ '3))
  (jbe @pop-regs)
  ;; More than 3 args; some must have been pushed by caller,
  ;; so retain the reserved frame.
  (pop (% arg_z))
  (pop (% arg_y))
  (pop (% arg_x))
  (jmp @popped)
  @pop-regs
  (je @pop3)
  (rcmpw (% nargs) ($ '1))
  (jb @discard)
  (ja @pop2)
  (pop (% arg_z))
  (jmp @discard)
  @pop3
  (pop (% arg_z))
  (pop (% arg_y))
  (pop (% arg_x))
  (jmp @discard)
  @pop2
  (pop (% arg_z))
  (pop (% arg_y))
  @discard
  (discard-reserved-frame)
  @popped
  (xchgq (% xfn) (% fn))
  (jmp (% fn)))



(defun closure-function (fun)
  (while (and (functionp fun)  (not (compiled-function-p fun)))
    (setq fun (%nth-immediate fun 0))
    (when (vectorp fun)
      (setq fun (svref fun 0))))
  fun)

;;; end of x86-def.lisp
