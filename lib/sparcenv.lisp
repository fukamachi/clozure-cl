;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Opensourced MCL.
;;;
;;;   Opensourced MCL is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Lesser General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2.1 of the License, or (at your option) any later version.
;;;
;;;   Opensourced MCL is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;   Lesser General Public License for more details.
;;;
;;;   You should have received a copy of the GNU Lesser General Public
;;;   License along with this library; if not, write to the Free Software
;;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;


(defconstant $numsparcsaveregs 6)
(defconstant $numsparcargregs 3)


(defconstant sparc-nonvolatile-registers-mask
  (logior (ash 1 sparc::%save0)
          (ash 1 sparc::%save1)
          (ash 1 sparc::%save2)
          (ash 1 sparc::%save3)
          (ash 1 sparc::%save4)
          (ash 1 sparc::%save5)))

(defconstant sparc-arg-registers-mask
  (logior (ash 1 sparc::%arg_z)
          (ash 1 sparc::%arg_y)
          (ash 1 sparc::%arg_x)))

(defconstant sparc-temp-registers-mask
  (logior (ash 1 sparc::%temp0)
          (ash 1 sparc::%temp1)
          (ash 1 sparc::%temp2)
          (ash 1 sparc::%temp3)))


(defconstant sparc-tagged-registers-mask
  (logior sparc-temp-registers-mask
          sparc-arg-registers-mask
          sparc-nonvolatile-registers-mask))

(defmacro make-mask (&rest weights)
  `(logior ,@(mapcar #'(lambda (w) `(ash 1 ,w)) weights)))

(defconstant sparc-temp-node-regs 
  (make-mask sparc::%temp0
             sparc::%temp1
             sparc::%temp2
             sparc::%temp3
             sparc::%arg_x
             sparc::%arg_y
             sparc::%arg_z))

(defconstant sparc-nonvolatile-node-regs
  (make-mask sparc::%save0
             sparc::%save1
             sparc::%save2
             sparc::%save3
             sparc::%save4
             sparc::%save5))


(defconstant sparc-node-regs (logior sparc-temp-node-regs sparc-nonvolatile-node-regs))

(defconstant sparc-imm-regs (make-mask
			     sparc::%imm0
			     sparc::%imm1
			     sparc::%imm2
			     sparc::%imm3
			     sparc::%imm4))

(defconstant sparc-temp-fp-regs (do* ((mask 0 (logior mask (ash 1 bit)))
				      (bit sparc::%f2 (+ bit 2)))
				     ((= bit 32) mask)))
                               




(defconstant $undo-sparc-c-frame 16)
(ccl::provide "SPARCENV")
