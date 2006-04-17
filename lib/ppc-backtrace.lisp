;;;-*-Mode: LISP; Package: CCL -*-
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


;;; Returns two values:
;;;  [nil, nil] if it can be reliably determined that function uses no registers at PC
;;;  [mask, savevsp]  if it can be reliably determined that the registers specified by "mask"
;;;      were saved at "savevsp" in the function's stack frame
;;;  [mask, nil] if registers in "mask" MAY have been saved, but we don't know how to restore them
;;;      (perhaps because the "at-pc" argument wasn't specified.


;;; If the last instruction in a code vector is an
;;; LWZ instruction (of the form "(LWZ rx s16 ry)"),
;;; then 
;;;   this function uses registers RX-R31.  Note that this leaves
;;;    us 2 extra bits, since we're only encoding 3 bits worth of
;;;    register info.
;;;   RX is saved nearest the top of the vstack
;;;   s16 is the offset from the saved-vsp to the address at which
;;;    RX was saved; this is a negative value whose low two bits
;;;    are ignored
;;;   (logior (ash (logand s16 3) 5) rY) is the pc at which
;;;   the registers were saved (a fullword code-vector index).
;;; This scheme lets us encode any "simple" register usage, where
;;; the registers were saved once, saved somewhere within the first 
;;; 128 instructions in the code vector, and nothing interesting (to
;;; backtrace) happens after the registers have been restored.
;;; If the compiler ever gets cleverer about this, we'll have to use
;;; some other scheme (perhaps a STW instruction, preceded by branches).
;;;
;;; Note that the "last instruction" really means "last instruction
;;; before any traceback table"; we should be able to truncate the code
;;; vector (probably by copying it) to strip off the traceback table
;;; without losing this information.
;;; Note also that the disassembler would probably ordinarily want to
;;; hide this last instruction ...
;;;   

#+ppc32-target
(defun registers-used-by (lfun &optional at-pc)
  (let* ((regs-used nil)
         (where-saved nil))
    (multiple-value-bind (op-high op-low) (%code-vector-last-instruction (uvref lfun 0))
      (declare (fixnum op-high op-low))
      (if (eql (ldb (byte 6 (- 26 16)) op-high) 32)       ; LWZ
        (let* ((nregs (- 32 (ldb (byte 5 (- 21 16)) op-high)))
               (pc (dpb (ldb (byte 2 0) op-low) (byte 2 5) (ldb (byte 5 (- 16 16)) op-high)))
               (offset (%word-to-int (logand op-low (lognot 3)))))
          (declare (fixnum nregs pc offset))
          (setq regs-used (1- (ash 1 nregs)))
          (if at-pc
            (if (>= at-pc pc)
              (setq where-saved (- (ash (- offset) -2) nregs))
              (setq regs-used nil))))))
    (values (and regs-used (bit-reverse-8 regs-used)) where-saved)))

#+ppc64-target
(defun registers-used-by (lfun &optional at-pc)
  (let* ((regs-used nil)
         (where-saved nil)
         (instr (%code-vector-last-instruction (uvref lfun 0))))
      (if (eql (ldb (byte 6 26) instr) 32)       ; LWZ
        (let* ((nregs (- 32 (ldb (byte 5 21) instr)))
               (pc (dpb (ldb (byte 2 0) instr) (byte 2 5) (ldb (byte 5 16) instr)))
               (offset (%word-to-int (logand instr (lognot 7)))))
          (declare (fixnum nregs pc offset))
          (setq regs-used (1- (ash 1 nregs)))
          (if at-pc
            (if (>= at-pc pc)
              (setq where-saved (- (ash (- offset) -3) nregs))
              (setq regs-used nil)))))        
      (values (and regs-used (bit-reverse-8 regs-used)) where-saved)))    
  

(defparameter *bit-reverse-8-table*
  #.(let ((table (make-array 256 :element-type '(unsigned-byte 8))))
      (dotimes (i 256)
        (let ((j 0)
              (out-mask (ash 1 7)))
          (declare (fixnum j out-mask))
          (dotimes (bit 8)
            (when (logbitp bit i)
              (setq j (logior j out-mask)))
            (setq out-mask (ash out-mask -1)))
          (setf (aref table i) j)))
      table))

(defun bit-reverse-8 (x)
  (aref *bit-reverse-8-table* x))

(defun %frame-savefn (p)
  (if (fake-stack-frame-p p)
    (%fake-stack-frame.fn p)
    (%%frame-savefn p)))

(defun %frame-savevsp (p)
  (if (fake-stack-frame-p p)
    (%fake-stack-frame.vsp p)
    (%%frame-savevsp p)))

(defun frame-vsp (frame)
  (%frame-savevsp frame))

;;; Return two values: the vsp of p and the vsp of p's "parent" frame.
;;; The "parent" frame vsp might actually be the end of p's segment,
;;; if the real "parent" frame vsp is in another segment.
(defun vsp-limits (p context)
  (let* ((vsp (%frame-savevsp p))
         parent)
    (when (eql vsp 0)
      ; This frame is where the code continues after an unwind-protect cleanup form
      (setq vsp (%frame-savevsp (child-frame p context))))
    (flet ((grand-parent (frame)
             (let ((parent (parent-frame frame context)))
               (when (and parent (eq parent (%frame-backlink frame context)))
                 (let ((grand-parent (parent-frame parent context)))
                   (when (and grand-parent (eq grand-parent (%frame-backlink parent context)))
                     grand-parent))))))
      (declare (dynamic-extent #'grand-parent))
      (let* ((frame p)
             grand-parent)
        (loop
          (setq grand-parent (grand-parent frame))
          (when (or (null grand-parent) (not (eql 0 (%frame-savevsp grand-parent))))
            (return))
          (setq frame grand-parent))
        (setq parent (parent-frame frame context)))
      (let* ((parent-vsp (if parent (%frame-savevsp parent) vsp))
             (tcr (if context (bt.tcr context) (%current-tcr)))
             (vsp-area (%fixnum-ref tcr target::tcr.vs-area)))
        (if (eql 0 parent-vsp)
          (values vsp vsp)              ; p is the kernel frame pushed by an unwind-protect cleanup form
          (progn
            (unless vsp-area
              (error "~s is not a stack frame pointer for context ~s" p tcr))
            (unless (%ptr-in-area-p parent-vsp vsp-area)
              (setq parent-vsp (%fixnum-ref vsp-area target::area.high)))
            (values vsp parent-vsp)))))))

(defun register-number->saved-register-index (regno)
  (- regno ppc::save7))

(defun %find-register-argument-value (context cfp regval bad)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (or
     (do* ((child (child-frame cfp context)
                  (child-frame child context)))
          ((null child))
       (if (fake-stack-frame-p child)
         (return (xp-gpr-lisp (%fake-stack-frame.xp child) regval))
         (multiple-value-bind (lfun pc)
             (cfp-lfun child)
           (when lfun
             (multiple-value-bind (mask where)
                 (registers-used-by lfun pc)
               (when (if mask (logbitp index mask))
                 (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))
                 (return (raw-frame-ref child context where bad))))))))
     (get-register-value nil last-catch index))))

(defun %raw-frame-ref (cfp context index bad)
  (multiple-value-bind (vfp parent-vfp)
      (vsp-limits cfp context)
      (if (< index (- parent-vfp vfp))
        (%fixnum-ref (- parent-vfp 1 index))
        bad)))
