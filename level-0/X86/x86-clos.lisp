;;;-*- Mode: Lisp; Package: CCL -*-
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

;;; It's easier to keep this is LAP; we want to play around with its
;;; constants.


;;; This just maps a SLOT-ID to a SLOT-DEFINITION or NIL.
;;; The map is a vector of (UNSIGNED-BYTE 8); this should
;;; be used when there are fewer than 255 slots in the class.
(defx86lapfunction %small-map-slot-id-lookup ((slot-id arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @have-table-index)
  (movq (% arg_x) (% imm1))
  (shrq ($ x8664::word-shift) (% imm1))
  (movzbl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (shlq ($ x8664::word-shift) (% imm1))
  @have-table-index
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (single-value-return))

;;; The same idea, only the map is a vector of (UNSIGNED-BYTE 32).
(defx86lapfunction %large-map-slot-id-lookup ((slot-id arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @have-table-index)
  (movq (% arg_x) (% imm1))
  (shrq ($ 1) (% imm1))
  (movl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  @have-table-index
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (single-value-return))


(defx86lapfunction %small-slot-id-value ((instance arg_y) (slot-id arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (movq (% arg_x) (% imm1))
  (shrq ($ x8664::word-shift) (% imm1))
  (movzbl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (movq (@ 'class (% fn)) (% arg_x))
  (movq (@ '%maybe-std-std-value-using-class (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 3)
  (jmp (% fn))
  @missing                              ; (%slot-id-ref-missing instance id)
  (movq (@'%slot-id-ref-missing (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 2)
  (jmp (% fn)))

(defx86lapfunction %large-slot-id-value ((instance arg_y) (slot-id arg_z))  
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (movq (% arg_x) (% imm1))
  (shrq ($ 1) (% imm1))
  (movl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (movq (@ 'class (% fn)) (% arg_x))
  (movq (@ '%maybe-std-std-value-using-class (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 3)
  (jmp (% fn))
  @missing                              ; (%slot-id-ref-missing instance id)
  (movq (@'%slot-id-ref-missing (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 2)
  (jmp (% fn)))

  
(defx86lapfunction %small-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index imm1)
  (vector-length temp1 imm0)
  (rcmpq (% imm1) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (shrq ($ x8664::word-shift) (% rdx))
  (movzbl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (pushq ($ 0))                         ; reserve frame
  (pushq ($ 0))
  (pushq (@ 'class (% fn)))
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_y))
  (movq (@ '%maybe-std-setf-slot-value-using-class (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 4)
  (jmp (% fn))
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (movq (@ '%slot-id-set-missing (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 3)
  (jmp (% fn)))


(defx86lapfunction %large-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index imm1)
  (vector-length temp1 imm0)
  (rcmpq (% imm1) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (shrq ($ x8664::word-shift) (% rdx))
  (movl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (pushq ($ 0))                         ; reserve frame
  (pushq ($ 0))
  (pushq (@ 'class (% fn)))
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_y))
  (movq (@ '%maybe-std-setf-slot-value-using-class (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 4)
  (jmp (% fn))
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (movq (@'%slot-id-ref-missing (% fn)) (% xfn))
  (xchgq (% xfn) (% fn))
  (set-nargs 3)
  (jmp (% fn)))


;;; All of the generic function trampoline functions have to be
;;; exactly the same size (x8664::gf-code-size) in words.  The
;;; largest of these - the general-case *GF-PROTO* - is currently
;;; "really" a little under 15 words, so X8664::GF-CODE-SIZE is
;;; just a little bigger than that.
(defparameter *gf-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (x86-lap-function 
      gag 
      ()
      (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
      (:code-size x8664::gf-code-size)
      (save-frame-variable-arg-count)
      (push-argregs)
      (movzwl (% nargs) (%l nargs))
      (pushq (%q nargs))
      (movq (% rsp) (% arg_z))
      (ref-global ret1valaddr imm0)
      (cmpq (% ra0) (% imm0))
      (je @multiple)
      (ref-global lexpr-return1v ra0)
      (jmp @call)
      @multiple
      (pushq (@ (+ x8664::nil-value (x8664::%kernel-global 'lexpr-return))))
      (movq (% imm0) (% ra0))
      @call
      (movq (@ 'dispatch-table (% fn)) (% arg_y))
      (set-nargs 2)
      (movq (@ 'dcode (% fn)) (% xfn))  ; dcode function
      (xchgq (% xfn) (% fn))
      (jmp (% fn))))))

;;; is a winner - saves ~15%
(defx86lapfunction gag-one-arg ((arg arg_z))
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8664::gf-code-size)
  (check-nargs 1)
  (movq (@ 'dispatch-table (% fn)) (% arg_y))
  (set-nargs 2)
  (movq (% fn) (% xfn))               ; don't let %fn get GCed
  (movq (@ 'dcode (% fn)) (% fn))
  (jmp (% fn)))

(defx86lapfunction gag-two-arg ((arg0 arg_y) (arg1 arg_z))
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8664::gf-code-size)
  (check-nargs 2)
  (movq (@ 'dispatch-table (% fn)) (% arg_x))
  (set-nargs 3)
  (movq (% fn) (% xfn))               ; don't let %fn get GCed
  (movq (@ 'dcode (% fn)) (% fn))
  (jmp (% fn)))



(defx86lapfunction funcallable-trampoline ()
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8664::gf-code-size)
  (movq (@ 'dcode (% fn)) (% xfn))
  (xchgq (% fn) (% xfn))
  (jmp (% fn)))


;;; This is in LAP so that it can reference itself in the error message.
;;; (It needs to be cloned, so %fn will be unique to each copy.)
;;; It can't work for this to reference any of its own constants.
(defx86lapfunction unset-fin-trampoline ()
  (:code-size x8664::gf-code-size)
  (save-frame-variable-arg-count)
  (call-subprim .SPheap-rest-arg nil)
  (pop (% arg_z))
  (movq ($ '#.$XNOFINFUNCTION) (% arg_x))
  (movq (% fn) (% arg_y))
  (set-nargs 3)
  (call-subprim .SPksignalerr)
  (movq ($ x8664::nil-value) (% arg_z))
  (leave)
  (single-value-return))



(defparameter *cm-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (x86-lap-function 
      gag 
      ()
      (:fixed-constants (thing dcode gf bits))
      (save-frame-variable-arg-count)
      (push-argregs)
      (movzwl (% nargs) (%l nargs))
      (pushq (%q nargs))
      (movq (% rsp) (% arg_z))
      (ref-global ret1valaddr imm0)
      (cmpq (% ra0) (% imm0))
      (je @multiple)
      (ref-global lexpr-return1v ra0)
      (jmp @call)
      @multiple
      (pushq (@ (+ x8664::nil-value (x8664::%kernel-global 'lexpr-return))))
      (movq (% imm0) (% ra0))
      @call
      (movq (@ 'thing (% fn)) (% arg_y))
      (movq (@ 'dcode (% fn)) (% xfn))
      (set-nargs 2)
      (xchgq (% xfn) (% fn))
      (jmp (% fn))))))




