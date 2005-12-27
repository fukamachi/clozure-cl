;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
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

;;; Low-level support for hash-consing.  The API is basically:
;;;
;;; (HONS-SPACE-FREE)
;;; Returns a constant value which is used to indicate a
;;; "free" cell in a HONS hash table.  (This value happens
;;; to be the same value that OpenMCL uses to denote unbound
;;; special variables, so setting a special variable to
;;; the value returned by (HONS-SPACE-FREE) will be roughly
;;; equivalent to calling MAKUNBOUND on that special variable.)
;;; This value generally prints as #<Unbound>.
;;;
;;; (HONS-SPACE-DELETED)
;;; Returns another constant value used to indicate a
;;; "deleted" cell in a HONS hash table; the CAR and CDR of
;;; a pair are set to this value by the GC if the HONS which
;;; addresses that pair becomes garbage.  This value is used
;;; in OpenMCL to denote unbound slots in STANDARD-INSTANCEs,
;;; so setting a slot in a standard-instance to this value
;;; is roughly equivalent to calling SLOT-MAKUNBOUND.  This
;;; value prints as #<Slot-Unbound>.
;;;
;;; (HONS-SPACE-SIZE)
;;; Returns a non-negative integer denoting the number of
;;; statically allocated pairs reserved for hash consing.
;;;
;;; (SET-HONS-SPACE-SIZE <npairs>)
;;; <npairs> should be a non-negative integer.  Tries to
;;; set the number of statically allocated pairs reserved
;;; for hash consing.  Returns the number of pairs actually
;;; allocated.
;;;   - may not change the hons-space size, because of
;;;     transient or persistent memory management issues
;;;     (need a way to distinguish between these cases.)
;;;   - if successful, may allocate slightly more pairs
;;;     than requested (the number of pairs allocated and
;;;     the return value may be rounded up to a multiple
;;;     of the number of bits in a machine word.)
;;; Newly allocated pairs will have their CAR and CDR both
;;; set to the value returned by (HONS-SPACE-FREE).
;;;
;;; (HONSP <thing>)
;;; If <thing> is a CONS and is allocated within hons-space,
;;; returns the index of the pair addressed by <thing> (e.g.,
;;; the return value will be a non-negative integer less than
;;; (HONS-SPACE-SIZE).  If <thing> is not a CONS or is not
;;; allocated within hons-space, returns NIL.
;;;
;;; (HONS-FROM-INDEX <index>)
;;; If <index> is a non-negative integer less than (HONS-SPACE-SIZE),
;;; returns a CONS-typed pointer to the <index>th pair in hons-space.
;;; (If <thing> is a HONS, then
;;;   (EQ (HONS-FROM-INDEX (HONSP <thing>)) <thing>) is true).
;;; Signals an error if <index> is not a valid index.
;;;
;;; (HONS-SPACE-REF-CAR <index>)
;;; (HONS-SPACE-REF-CDR <index>)
;;; Semantically equivalent to (CAR (HONS-FROM-INDEX <index>)) and
;;; (CDR (HONS-FROM-INDEX <index>)), respectively.  (May not be
;;; implemented in a way that actually calls HONS-FROM-INDEX.)
;;;
;;; (HONS-SPACE-CONS <index> <new-car> <new-cdr>)
;;; Equivalent to:
;;; (let* ((x (HONS-FROM-INDEX <index>)))
;;;   (setf (car x) <new-car>
;;;         (cdr x) <new-cdr>)
;;;   x)


(defun hons-space-free ()
  "Returns the value used to indicate free HONS cells."
  (%unbound-marker))

(defun hons-space-deleted ()
  "Returns the value used to indicate deleted HONS cells."
  (%slot-unbound-marker))


#+ppc-target
(defppclapfunction set-hons-space-size ((npairs arg_z))
  (check-nargs 1)
  (mflr loc-pc)
  #+ppc32-target
  (bla .SPgetu32)
  #+ppc64-target
  (bla .SPgetu64)
  (mtlr loc-pc)
  (mr imm1 imm0)
  (li imm0 arch::gc-trap-function-set-hons-area-size)
  (trlgei allocptr 0)
  #+ppc32-target
  (ba .SPmakeu32)
  #+ppc64-target
  (ba .SPmakeu64))

(defun hons-space-size ()
  (%fixnum-ref-natural (%get-kernel-global 'tenured-area)
                       target::area.static-dnodes))

#+ppc-target
(defppclapfunction honsp ((thing arg_z))
  (check-nargs 1)
  (extract-fulltag imm2 thing)
  (ref-global imm0 tenured-area)
  (cmpri cr2 imm2 target::fulltag-cons)
  (ldr imm1 target::area.static-dnodes imm0)
  (ldr imm0 target::area.low imm0)
  (slri imm1 imm1 (1+ target::word-shift))
  (bne cr2 @no)
  (add imm1 imm0 imm1)
  (cmpr cr0 thing imm0)
  (cmpr cr1 thing imm1)
  (blt cr0 @no)
  (bgt cr1 @no)
  (subi arg_z arg_z target::fulltag-cons)
  (sub arg_z arg_z imm0)
  (srri arg_z arg_z 1)
  (blr)
  @no
  (li arg_z nil)
  (blr))

#+ppc-target
(defppclapfunction hons-from-index ((index arg_z))
  (check-nargs 1)
  (extract-lisptag imm0 index)
  (cmpri cr0 index 0)
  (cmpri cr1 imm0 target::tag-fixnum)
  (ref-global imm0 tenured-area)
  (unbox-fixnum imm1 arg_z)
  (ldr imm2 target::area.static-dnodes imm0)
  (bne cr1 @bad)
  (cmpr cr2 imm1 imm2)
  (blt cr0 @bad)
  (ldr imm2 target::area.static-used imm0)
  (ldr imm0 target::area.low imm0)
  (bge cr2 @bad)
  (add arg_z index index)
  (add arg_z imm0 arg_z)
  (la arg_z target::fulltag-cons arg_z)
  (sub imm0 arg_z imm0)
  (set-bit-at-index imm2 imm0)
  (blr)
  @bad
  (save-lisp-context)
  (load-constant arg_y "Invalid HONS index: ~d. ")
  (set-nargs 2)
  (load-constant fname error)
  (bla .SPjmpsym)
  (ba .SPpopj))

#+ppc-target
(defppclapfunction hons-space-ref-car ((index arg_z))
  (check-nargs 1)
  (extract-lisptag imm0 index)
  (cmpri cr0 index 0)
  (cmpri cr1 imm0 target::tag-fixnum)
  (ref-global imm0 tenured-area)
  (unbox-fixnum imm1 arg_z)
  (ldr imm2 target::area.static-dnodes imm0)
  (bne cr1 @bad)
  (cmpr cr2 imm1 imm2)
  (blt cr0 @bad)
  (ldr imm0 target::area.low imm0)
  (bge cr2 @bad)
  (add arg_z index index)
  (add imm0 imm0 arg_z)
  (ldr arg_z (+ target::cons.car target::fulltag-cons) imm0)
  (blr)
  @bad
  (save-lisp-context)
  (load-constant arg_y "Invalid HONS index: ~d. ")
  (set-nargs 2)
  (load-constant fname error)
  (bla .SPjmpsym)
  (ba .SPpopj))

#+ppc-target
(defppclapfunction hons-space-ref-cdr ((index arg_z))
  (check-nargs 1)
  (extract-lisptag imm0 index)
  (cmpri cr0 index 0)
  (cmpri cr1 imm0 target::tag-fixnum)
  (ref-global imm0 tenured-area)
  (unbox-fixnum imm1 arg_z)
  (ldr imm2 target::area.static-dnodes imm0)
  (bne cr1 @bad)
  (cmpr cr2 imm1 imm2)
  (blt cr0 @bad)
  (ldr imm0 target::area.low imm0)
  (bge cr2 @bad)
  (add arg_z index index)
  (add imm0 imm0 arg_z)
  (ldr arg_z (+ target::cons.cdr target::fulltag-cons) imm0)
  (blr)
  @bad
  (save-lisp-context)
  (load-constant arg_y "Invalid HONS index: ~d. ")
  (set-nargs 2)
  (load-constant fname error)
  (bla .SPjmpsym)
  (ba .SPpopj))


(defun hons-space-cons (index new-car new-cdr)
  (let* ((hons (hons-from-index index)))
    (setf (car hons) new-car
          (cdr hons) new-cdr)
    hons))

;;; We might have multiple (logical) tables in hons space, and
;;; would probably like to know how many pairs had been deleted
;;; from each table.  (How to express that to the GC in some
;;; way that would allow it to efficiently track this is an
;;; open question.)  For now, the GC just maintains a global
;;; count of static pairs that it's deleted.
(defun deleted-hons-count ()
  (%get-kernel-global 'deleted-static-pairs))


(provide "HASH-CONS")
