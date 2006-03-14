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
  (addb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (% arg_z.b))
  (single-value-return))

(defx86lapfunction %function-to-function-vector  ((arg arg_z))
  (subb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (% arg_z.b))
  (single-value-return))

(defx86lapfunction %function-code-words ((fun arg_z))
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %nth-immediate ((fun arg_y) (n arg_z))
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (lea (@ (% n) (% imm0) 8) (% imm0))
  (movq (@ (- x8664::node-size x8664::fulltag-function) (% fun) (% imm0))
        (% arg_z))
  (single-value-return))

(defx86lapfunction %make-code-executable ((codev arg_z))
  (single-value-return))


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

(eval-when (:compile-toplevel)
  (warn "missing lexpr/apply stuff"))

#+notyet
(progn
(defx86lapfunction %apply-lexpr-with-method-context ((magic arg_x)
                                                     (function arg_y)
                                                     (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in x8664::next-method-context (= x8664::temp0).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves ppc::nfn/ppc::next-method-context.
  ;; Jump to the function in ppc::nfn.
  (mr x8664::next-method-context magic)
  (mr ppc::nfn function)
  (set-nargs 0)
  (mflr loc-pc)
  (bla .SPspread-lexpr-z)
  (mtlr loc-pc)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))


(defx86lapfunction %apply-with-method-context ((magic arg_x)
                                               (function arg_y)
                                               (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in ppc::next-method-context (= ppc::temp1).
  ;; Put function in ppc::nfn (= ppc::temp2).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves ppc::nfn/ppc::next-method-context.
  ;; Jump to the function in ppc::nfn.
  (mr ppc::next-method-context magic)
  (mr ppc::nfn function)
  (set-nargs 0)
  (mflr loc-pc)
  (bla .SPspreadargZ)
  (mtlr loc-pc)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))


(defx86lapfunction %apply-lexpr-tail-wise ((method arg_y) (args arg_z))
  ;; This assumes
  ;; a) that "args" is a lexpr made via the .SPlexpr-entry mechanism
  ;; b) That the LR on entry to this function points to the lexpr-cleanup
  ;;    code that .SPlexpr-entry set up
  ;; c) That there weren't any required args to the lexpr, e.g. that
  ;;    (%lexpr-ref args (%lexpr-count args) 0) was the first arg to the gf.
  ;; The lexpr-cleanup code will be EQ to either (lisp-global ret1valaddr)
  ;; or (lisp-global lexpr-return1v).  In the former case, discard a frame
  ;; from the cstack (multiple-value tossing).  Restore FN and LR from
  ;; the first frame that .SPlexpr-entry pushed, restore vsp from (+
  ;; args node-size), pop the argregs, and jump to the function.
  (mflr loc-pc)
  (ref-global imm0 ret1valaddr)
  (cmpr cr2 loc-pc imm0)
  (ldr nargs 0 args)
  (mr imm5 nargs)
  (cmpri cr0 nargs 0)
  (cmpri cr1 nargs '2)
  (mr nfn arg_y)
  (ldr temp0 target::misc-data-offset nfn)
  (mtctr temp0)
  (if (:cr2 :eq)
    (la sp target::lisp-frame.size sp))
  (ldr loc-pc target::lisp-frame.savelr sp)
  (ldr fn target::lisp-frame.savefn sp)
  (ldr imm0 target::lisp-frame.savevsp sp)
  (sub vsp imm0 nargs)
  (mtlr loc-pc)
  (la sp target::lisp-frame.size sp)
  (beqctr)
  (vpop arg_z)
  (bltctr cr1)
  (vpop arg_y)
  (beqctr cr1)
  (vpop arg_x)
  (bctr))

)



(defun closure-function (fun)
  (while (and (functionp fun)  (not (compiled-function-p fun)))
    (setq fun (%nth-immediate fun 0))
    (when (vectorp fun)
      (setq fun (svref fun 0))))
  fun)

;;; end of x86-def.lisp
