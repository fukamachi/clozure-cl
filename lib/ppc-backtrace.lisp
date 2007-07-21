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

(def-accessors (fake-stack-frame) %svref
  nil                           ; 'fake-stack-frame
  %fake-stack-frame.sp          ; fixnum. The stack pointer where this frame "should" be
  %fake-stack-frame.next-sp     ; Either sp or another fake-stack-frame
  %fake-stack-frame.fn          ; The current function
  %fake-stack-frame.lr          ; fixnum offset from fn (nil if fn is not functionp)
  %fake-stack-frame.vsp         ; The value stack pointer
  %fake-stack-frame.xp          ; Exception frame.
  %fake-stack-frame.link        ; next in *fake-stack-frames* list
  )

;;; Linked list of fake stack frames.
;;; %frame-backlink looks here
(def-standard-initial-binding *fake-stack-frames* nil)
  

(defun fake-stack-frame-p (x)
  (istruct-typep x 'fake-stack-frame))

(defun cfp-lfun (p)
  (if (fake-stack-frame-p p)
    (let* ((fn (%fake-stack-frame.fn p))
           (lr (%fake-stack-frame.lr p)))
      (if (and (typep fn 'function)
               (typep lr 'fixnum))
        (values fn lr)
        (values nil nil)))
    (%cfp-lfun p)))


(defun %stack< (index1 index2 &optional context)
  (cond ((fake-stack-frame-p index1)
         (let ((sp1 (%fake-stack-frame.sp index1)))
           (declare (fixnum sp1))
           (if (fake-stack-frame-p index2)
             (or (%stack< sp1 (%fake-stack-frame.sp index2) context)
                 (eq index2 (%fake-stack-frame.next-sp index1)))
             (%stack< sp1 (%i+ index2 1) context))))
        ((fake-stack-frame-p index2)
         (%stack< index1 (%fake-stack-frame.sp index2) context))
        (t (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                  (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
             (and (%ptr-in-area-p index1 cs-area)
                  (%ptr-in-area-p index2 cs-area)
                  (< (the fixnum index1) (the fixnum index2)))))))

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


(defun catch-csp-p (p context)
  (let ((catch (if context
                 (bt.top-catch context)
                 (%catch-top (%current-tcr)))))
    (loop
      (when (null catch) (return nil))
      (let ((sp (catch-frame-sp catch)))
        (when (eql sp p)
          (return t)))
      (setq catch (next-catch catch)))))

(defun last-catch-since (sp context)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (catch (%catch-top tcr))
         (last-catch nil))
    (loop
      (unless catch (return last-catch))
      (let ((csp (uvref catch target::catch-frame.csp-cell)))
        (when (%stack< sp csp context) (return last-catch))
        (setq last-catch catch
              catch (next-catch catch))))))

(defun register-number->saved-register-index (regno)
  (- regno ppc::save7))

(defun %find-register-argument-value (context cfp regval bad)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp
                 (child-frame frame context))
          (first t))
         ((null frame))
      (if (fake-stack-frame-p frame)
        (return-from %find-register-argument-value
          (xp-gpr-lisp (%fake-stack-frame.xp frame) regval))
        (if first
          (setq first nil)
          (multiple-value-bind (lfun pc)
              (cfp-lfun frame)
            (when lfun
              (multiple-value-bind (mask where)
                  (registers-used-by lfun pc)
                (when (if mask (logbitp index mask))
                  (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))
                  (return-from
                   %find-register-argument-value
                    (raw-frame-ref frame context where bad)))))))))
    (get-register-value nil last-catch index)))

(defun %raw-frame-ref (cfp context idx bad)
  (declare (fixnum idx))
  (multiple-value-bind (frame base)
      (vsp-limits cfp context)
    (let* ((raw-size (- base frame)))
      (declare (fixnum frame base raw-size))
      (if (and (>= idx 0)
               (< idx raw-size))
        (let* ((addr (- (the fixnum (1- base))
                        idx)))
          (multiple-value-bind (db-count first-db last-db)
              (count-db-links-in-frame frame base context)
            (let* ((is-db-link
                    (unless (zerop db-count)
                      (do* ((last last-db (previous-db-link last first-db)))
                           ((null last))
                        (when (= addr last)
                          (return t))))))
              (if is-db-link
                (oldest-binding-frame-value context addr)
                (%fixnum-ref addr)))))
        bad))))

;;; Used for printing only.
(defun index->address (p)
  (when (fake-stack-frame-p p)
    (setq p (%fake-stack-frame.sp p)))
  (ldb (byte #+32-bit-target 32 #+64-bit-target 64 0)  (ash p target::fixnumshift)))


(defun match-local-name (cellno info pc)
  (when info
    (let* ((syms (%car info))
           (ptrs (%cdr info)))
      (dotimes (i (length syms))
        (let ((j (%i+ i (%i+ i i ))))
          (and (eq (uvref ptrs j) (%ilogior (%ilsl (+ 6 target::word-shift) cellno) #o77))
               (%i>= pc (uvref ptrs (%i+ j 1)))
               (%i< pc (uvref ptrs (%i+ j 2)))
               (return (aref syms i))))))))

(defun get-register-value (address last-catch index)
  (if address
    (%fixnum-ref address)
    (uvref last-catch (+ index target::catch-frame.save-save7-cell))))

;;; Inverse of get-register-value

(defun set-register-value (value address last-catch index)
  (if address
    (%fixnum-set address value)
    (setf (uvref last-catch (+ index target::catch-frame.save-save7-cell))
          value)))
