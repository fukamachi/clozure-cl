;;;-*- Mode: Lisp; Package: CCL -*-
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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "X8664-ARCH")
  (require "X86-LAPMACROS"))

;;; This assumes that macros & special-operators
;;; have something that's not FUNCTIONP in their
;;; function-cells.  It also assumes that NIL
;;; isn't a true symbol, but that NILSYM is.
(defx86lapfunction %function ((sym arg_z))
  (check-nargs 1)
  (let ((symaddr temp0))
    (movq ($ (+ x8664::nil-value x8664::nilsym-offset)) (% symaddr))
    (cmp-reg-to-nil sym)
    (cmovneq (% sym) (% symaddr))
    (trap-unless-fulltag= symaddr x8664::fulltag-symbol)
    (movq (@ x8664::symbol.fcell (% symaddr)) (% arg_z))
    (trap-unless-fulltag= arg_z x8664::fulltag-function)
    (single-value-return)))

;;; Traps unless sym is NIL or some other symbol.
;;; If it's NIL, map it to NILSYM's symbol-vector; if it's
;;; a true symbol, map it to that symbol's symbol-vector.
;;; (This is mostly done to perform typechecking and to
;;; allow lisp code to use %SVREF, etc.)
(defx86lapfunction %symbol->symptr ((sym arg_z))
  (let ((symaddr temp0))
    (movq ($ (+ x8664::nil-value x8664::nilsym-offset)) (% symaddr))
    (cmp-reg-to-nil sym)
    (cmoveq (% symaddr) (% sym))
    (trap-unless-fulltag= sym x8664::fulltag-symbol)
    (leaq (@ (- x8664::fulltag-misc x8664::fulltag-symbol) (% sym)) (% sym))
    (single-value-return)))

;;; If symptr is either a real symbol or NIL, returns it.
;;; Otherwise, traps unless symptr is a symbol-vector
;;; and returns the underlying symbol
(defx86lapfunction %symptr->symbol ((symptr arg_z))
  (movw ($ (logior (ash 1 x8664::fulltag-nil) (ash 1 x8664::fulltag-symbol)))
        (% imm0.w))
  (btw (%w symptr) (% imm0.w))
  (jb @done)
  (trap-unless-typecode= symptr x8664::subtag-symbol)
  (leaq (@ (- x8664::fulltag-symbol x8664::fulltag-misc) (% symptr)) (% arg_z))
  @done
  (single-value-return))

(defx86lapfunction %symptr-value ((symptr arg_z))
  (addq ($ (- x8664::fulltag-symbol x8664::fulltag-misc)) (% symptr))
  (jmp-subprim .SPspecref))

(defx86lapfunction %set-symptr-value ((symptr arg_y) (val arg_z))
  (addq ($ (- x8664::fulltag-symbol x8664::fulltag-misc)) (% symptr))
  (jmp-subprim .SPspecset))

;;; This does inded get a "symptr" (symbol-vector) as argument.
;;; It's supposed to return a "thing" and a "byte offset";
;;; it doesn't really matter whether that thing is a symbol
;;; or a symbol vector if there isn't a binding in the TCR.
(defx86lapfunction %symptr-binding-address ((symptr arg_z))
  (movq (@ x8664::symbol.binding-index (% symptr)) (% arg_y))
  (rcmp (% arg_y) (@ (% rcontext) x8664::tcr.tlb-limit))
  (movq (@ (% rcontext) x8664::tcr.tlb-pointer) (% arg_x))
  (jae @sym)
  (cmpb ($ x8664::no-thread-local-binding-marker) (@ (% arg_x) (% arg_y)))
  (je @sym)
  (shl ($ x8664::word-shift) (% arg_y))
  (push (% arg_x))
  (push (% arg_y))
  (set-nargs 2)
  (lea (@ '2 (% rsp)) (% temp0))
  (jmp-subprim .SPvalues)
  @sym
  (push (% arg_z))
  (pushq ($ '#.x8664::symptr.vcell))
  (set-nargs 2)
  (lea (@ '2 (% rsp)) (% temp0))
  (jmp-subprim .SPvalues))

(defx86lapfunction %tcr-binding-location ((tcr arg_y) (sym arg_z))
  (movq (@ x8664::symbol.binding-index (% sym)) (% arg_x))
  (movl ($ nil) (% arg_z.l))
  (rcmp (% arg_x) (@ x8664::tcr.tlb-limit (% tcr)))
  (movq (@ x8664::tcr.tlb-pointer (% tcr)) (% arg_y))
  (jae @done)
  (lea (@ (% arg_y) (% arg_x)) (% arg_y))
  ;; We're little-endian, so the tag is at the EA with no
  ;; displacement
  (cmpb ($ x8664::subtag-no-thread-local-binding) (@ (% arg_y)))
  (cmovneq (% arg_y) (% arg_z))
  @done
  (single-value-return))

  
(defx86lapfunction %pname-hash ((str arg_y) (len arg_z))
  (let ((accum imm0)
        (offset imm1))
    (xorq (% offset) (% offset))
    (xorq (% accum) (% accum))
    (cmpq ($ 0) (% len))
    (jz.pn @done)
    @loop8
    (roll ($ 5) (%l accum))
    (xorb (@ x8664::misc-data-offset (% str) (% offset))  (%b accum))
    (addq ($ 1) (% offset))    
    (subq ($ '1) (% len))
    (jnz @loop8)
    (shlq ($ 5) (% accum))
    (shrq ($ (- 5 x8664::fixnumshift)) (% accum))
    (movq (% accum) (% arg_z))
    @done
    (single-value-return)))
