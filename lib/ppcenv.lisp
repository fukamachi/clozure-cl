; -*- Mode:Lisp; Package:CCL; -*-
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


(defconstant $numppcsaveregs 8)
(defconstant $numppcargregs 3)


(defconstant ppc-nonvolatile-registers-mask
  (logior (ash 1 ppc32::save0)
          (ash 1 ppc32::save1)
          (ash 1 ppc32::save2)
          (ash 1 ppc32::save3)
          (ash 1 ppc32::save4)
          (ash 1 ppc32::save5)
          (ash 1 ppc32::save6)
          (ash 1 ppc32::save7)))

(defconstant ppc-arg-registers-mask
  (logior (ash 1 ppc32::arg_z)
          (ash 1 ppc32::arg_y)
          (ash 1 ppc32::arg_x)))

(defconstant ppc-temp-registers-mask
  (logior (ash 1 ppc32::temp0)
          (ash 1 ppc32::temp1)
          (ash 1 ppc32::temp2)
          (ash 1 ppc32::temp3)
	  (ash 1 ppc32::temp4)))


(defconstant ppc-tagged-registers-mask
  (logior ppc-temp-registers-mask
          ppc-arg-registers-mask
          ppc-nonvolatile-registers-mask))

(defmacro make-mask (&rest weights)
  `(logior ,@(mapcar #'(lambda (w) `(ash 1 ,w)) weights)))

(defconstant ppc-temp-node-regs 
  (make-mask ppc32::temp0
             ppc32::temp1
             ppc32::temp2
             ppc32::temp3
	     ppc32::temp4
             ppc32::arg_x
             ppc32::arg_y
             ppc32::arg_z))

(defconstant ppc-nonvolatile-node-regs
  (make-mask ppc32::save0
             ppc32::save1
             ppc32::save2
             ppc32::save3
             ppc32::save4
             ppc32::save5
             ppc32::save6
             ppc32::save7))


(defconstant ppc-node-regs (logior ppc-temp-node-regs ppc-nonvolatile-node-regs))

(defconstant ppc-imm-regs (make-mask
                            ppc32::imm0
                            ppc32::imm1
                            ppc32::imm2
                            ppc32::imm3
                            ppc32::imm4
                            ppc32::imm5))

(defconstant ppc-temp-fp-regs (1- (ash 1 ppc32::fp14)))
                               
(defconstant ppc-cr-fields
  (make-mask 0 (ash 4 -2) (ash 8 -2) (ash 12 -2) (ash 16 -2) (ash 20 -2) (ash 24 -2) (ash 28 -2)))



(defconstant $undo-ppc-c-frame 16)

(defconstant $ppc-compound-branch-target-bit 28)
(defconstant $ppc-compound-branch-target-mask (ash 1 $ppc-compound-branch-target-bit))

(defconstant $ppc-mvpass-bit 29)
(defconstant $ppc-mvpass-mask (ash -1 $ppc-mvpass-bit))

(defconstant $ppc-return (- (ash 1 14) 1))
(defconstant $ppc-mvpass (- (ash 1 14) 2))

(defconstant $ppc-compound-branch-false-byte (byte 14 0))
(defconstant $ppc-compound-branch-true-byte (byte 14 14))

(ccl::provide "PPCENV")
