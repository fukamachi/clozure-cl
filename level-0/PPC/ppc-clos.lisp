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

;;; It's easier to keep this is LAP; we want to play around with its
;;; constants.

;;; This just maps a SLOT-ID to a SLOT-DEFINITION or NIL.
;;; The map is a vector of (UNSIGNED-BYTE 8); this should
;;; be used when there are less than 255 slots in the class.
(defppclapfunction %small-map-slot-id-lookup ((slot-id arg_z))
  (lwz temp1 'map nfn)
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (header-length imm3 imm0)
  (lwz temp0 'table nfn)
  (cmplw arg_x imm3)
  (srwi imm0 arg_x 2)
  (la imm0 ppc32::misc-data-offset imm0)
  (li imm1 ppc32::misc-data-offset)
  (bge @have-scaled-table-index)
  (lbzx imm1 temp1 imm0)
  (slwi imm1 imm1 2)
  (la imm1 ppc32::misc-data-offset imm1)
  @have-scaled-table-index
  (lwzx arg_z temp0 imm1)
  (blr))

;;; The same idea, only the map is a vector of (UNSIGNED-BYTE 32).
(defppclapfunction %large-map-slot-id-lookup ((slot-id arg_z))
  (lwz temp1 'map nfn)
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (header-length imm3 imm0)
  (lwz temp0 'table nfn)
  (cmplw arg_x imm3)
  (la imm0 ppc32::misc-data-offset arg_x)
  (li imm1 ppc32::misc-data-offset)
  (bge @have-scaled-table-index)
  (lwzx imm1 temp1 imm0)
  (la imm1 ppc32::misc-data-offset imm1)
  @have-scaled-table-index
  (lwzx arg_z temp0 imm1)
  (blr))

(defppclapfunction %small-slot-id-value ((instance arg_y) (slot-id arg_z))
  (lwz temp1 'map nfn)
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (lwz temp0 'table nfn)
  (header-length imm3 imm0)
  (cmplw arg_x imm3)
  (srwi imm0 arg_x 2)
  (la imm0 ppc32::misc-data-offset imm0)
  (bge @missing)
  (lbzx imm1 temp1 imm0)
  (cmpwi imm1 0)
  (slwi imm1 imm1 2)
  (la imm1 ppc32::misc-data-offset imm1)
  (beq @missing)
  @have-scaled-table-index
  (lwzx arg_z temp0 imm1)
  (lwz arg_x 'class nfn)
  (lwz nfn '%maybe-std-slot-value nfn)
  (lwz temp0 ppc32::misc-data-offset nfn)
  (set-nargs 3)
  (mtctr temp0)
  (bctr)
  @missing                              ; (%slot-id-ref-missing instance id)
  (lwz nfn '%slot-id-ref-missing nfn)
  (set-nargs 2)
  (lwz temp0 ppc32::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))

(defppclapfunction %large-slot-id-value ((instance arg_y) (slot-id arg_z))
  (lwz temp1 'map nfn)
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (lwz temp0 'table nfn)
  (header-length imm3 imm0)
  (cmplw arg_x imm3)
  (la imm0 ppc32::misc-data-offset arg_x)
  (bge @missing)
  (lwzx imm1 temp1 imm0)
  (cmpwi imm1 0)
  (la imm1 ppc32::misc-data-offset imm1)
  (beq @missing)
  @have-scaled-table-index
  (lwz arg_x 'class nfn)
  (lwz nfn '%maybe-std-slot-value-using-class nfn)
  (lwzx arg_z temp0 imm1)
  (lwz temp0 ppc32::misc-data-offset nfn)
  (set-nargs 3)
  (mtctr temp0)
  (bctr)
  @missing                              ; (%slot-id-ref-missing instance id)
  (lwz nfn '%slot-id-ref-missing nfn)
  (set-nargs 2)
  (lwz temp0 ppc32::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))
  
(defppclapfunction %small-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (lwz temp1 'map nfn)
  (svref imm3 slot-id.index slot-id)
  (getvheader imm0 temp1)
  (lwz temp0 'table nfn)
  (header-length imm5 imm0)
  (cmplw imm3 imm5)
  (srwi imm0 imm3 2)
  (la imm0 ppc32::misc-data-offset imm0)
  (bge @missing)
  (lbzx imm1 temp1 imm0)
  (cmpwi imm1 0)
  (slwi imm1 imm1 2)
  (la imm1 ppc32::misc-data-offset imm1)
  (beq @missing)
  @have-scaled-table-index
  (lwz temp1 'class nfn)
  (lwzx arg_y temp0 imm1)
  (lwz nfn '%maybe-std-setf-slot-value-using-class nfn)
  (set-nargs 4)
  (lwz temp0 ppc32::misc-data-offset nfn)
  (vpush temp1)
  (mtctr temp0)
  (bctr)
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (lwz nfn '%slot-id-ref-missing nfn)
  (set-nargs 3)
  (lwz temp0 ppc32::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))

(defppclapfunction %large-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (lwz temp1 'map nfn)
  (svref imm3 slot-id.index slot-id)
  (getvheader imm0 temp1)
  (lwz temp0 'table nfn)
  (header-length imm5 imm0)
  (cmplw imm3 imm5)
  (la imm0 ppc32::misc-data-offset imm3)
  (bge @missing)
  (lwzx imm1 temp1 imm0)
  (cmpwi imm1 0)
  (la imm1 ppc32::misc-data-offset imm1)
  (beq @missing)
  @have-scaled-table-index
  (ldr temp1 'class nfn)
  (ldrx arg_y temp0 imm1)
  (ldr nfn '%maybe-std-setf-slot-value-using-class nfn)
  (set-nargs 4)
  (svref temp0 0 nfn)
  (vpush temp1)
  (mtctr temp0)
  (bctr)
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (lwz nfn '%slot-id-ref-missing nfn)
  (set-nargs 3)
  (lwz temp0 ppc32::misc-data-offset nfn)
  (mtctr temp0)
  (bctr))

(defparameter *gf-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (ppc-lap-function 
      gag 
      ()
      (mflr loc-pc)
      (vpush-argregs)
      (vpush nargs)
      (add imm0 vsp nargs)
      (la imm0 4 imm0)                  ; caller's vsp
      (bla .SPlexpr-entry)
      (mtlr loc-pc)                     ; return to kernel
      (mr arg_z vsp)                    ; lexpr
      (svref arg_y gf.dispatch-table nfn) ; dispatch table
      (set-nargs 2)
      (svref nfn gf.dcode nfn)		; dcode function
      (lwz temp0 ppc32::misc-data-offset nfn)
      (mtctr temp0)
      (bctr)))))

(defppclapfunction funcallable-trampoline ()
  (svref nfn gf.dcode nfn)
  (svref temp0 0 nfn)
  (mtctr temp0)
  (bctr))


(defppclapfunction unset-fin-trampoline ()
  (mflr loc-pc)
  (bla .SPheap-rest-arg)                ; cons up an &rest arg, vpush it
  (vpop arg_z)                          ; whoops, didn't really want to
  (bla .SPsavecontextvsp)
  (lwz arg_x '"Funcallable instance ~S was called with args ~s, but has no FUNCALLABLE-INSTANCE-FUNCTION" fn)
  (mr arg_y fn)
  (set-nargs 3)
  (ldr fname 'error fn)
  (bla .SPrestorecontext)
  (mtlr loc-pc)
  (ba .SPjmpsym))
