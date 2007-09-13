;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2006 Clozure Associates and contributors
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
;;;  [mask, saved-location]  if it can be reliably determined that the registers specified by "mask"
;;;      were saved at "saved-location" in the function's stack frame
;;;  [mask, nil] if registers in "mask" MAY have been saved, but we don't know how to restore them
;;;      (perhaps because the "at-pc" argument wasn't specified.


(defun registers-used-by (function &optional at-pc)
  (multiple-value-bind (mask stack-location rpc)
      (%function-register-usage function)
    (if (null mask)
      (values nil nil)
      (values (canonicalize-register-mask mask) (if (and at-pc rpc (> at-pc rpc)) stack-location)))))

(defun canonicalize-register-mask (mask)
  (dpb (ldb (byte 2 14) mask) (byte 2 2) (ldb (byte 2 11) mask)))

(defun xcf-p (p)
  (eql 0 (%fixnum-ref p x8664::lisp-frame.return-address)))

(defun %current-xcf ()
  (do* ((q (%get-frame-ptr) (%%frame-backlink q)))
       ((zerop q))
    (declare (fixnum q))
    (when (xcf-p q) (return q))))

;;; Try to determine the program counter value, relative to an xcf's nominal function.
(defun pc-from-xcf (xcf)
  (let* ((nominal-function (%fixnum-ref xcf x8664::xcf.nominal-function))
         (containing-object (%fixnum-ref xcf x8664::xcf.containing-object)))
    (when (typep nominal-function 'function)
      (if (eq containing-object (function-to-function-vector nominal-function))
        (- (%fixnum-ref xcf x8664::xcf.relative-pc)
           x8664::tag-function)
        (let* ((tra (%fixnum-ref xcf x8664::xcf.ra0)))
          (if (and (= (lisptag tra) x8664::tag-tra)
                   (eq nominal-function (%return-address-function tra)))
            (%return-address-offset tra)))))))
            
(defun cfp-lfun (p)
  (if (xcf-p p)
    (values
     (%fixnum-ref p x8664::xcf.nominal-function)
     (pc-from-xcf p))
    (%cfp-lfun p)))

;;; On PPC, some frames on the control stack are associated with catch
;;; frames rather than with function calls.  The whole concept doesn't
;;; really apply here (e.g., nothing we encounter while walking frame
;;; pointer links belongs to a catch frame.)
(defun catch-csp-p (p context)
  (declare (ignore p context)))

(defun %raw-frame-ref (frame context idx bad)
  (declare (fixnum frame idx))
  (let* ((base (parent-frame frame context))
         (raw-size (- base frame)))
    (declare (fixnum base raw-size))
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
      bad)))

(defun %raw-frame-set (frame context idx new)
  (declare (fixnum frame idx))
  (let* ((base (parent-frame frame context))
         (raw-size (- base frame)))
    (declare (fixnum base raw-size))
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
              (setf (%fixnum-ref addr) new))))))))

(defun %stack< (index1 index2 &optional context)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (vs-area (%fixnum-ref tcr target::tcr.vs-area)))
    (and (%ptr-in-area-p index1 vs-area)
         (%ptr-in-area-p index2 vs-area)
         (< (the fixnum index1) (the fixnum index2)))))




(defun register-number->saved-register-index (regnum)
  (ecase regnum
    (#.x8664::save3 0)
    (#.x8664::save2 1)
    (#.x8664::save1 2)
    (#.x8664::save0 3)))


(defun get-register-value (address last-catch index)
  (if address
    (%fixnum-ref address)
    (uvref last-catch (+ index target::catch-frame.save-save3-cell))))

;;; Inverse of get-register-value

(defun set-register-value (value address last-catch index)
  (if address
    (%fixnum-set address value)
    (setf (uvref last-catch (+ index target::catch-frame.save-save3-cell))
          value)))

(defun %find-register-argument-value (context cfp regval bad)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp (child-frame frame context))
          (first t))
         ((null frame))
      (if (xcf-p frame)
        (with-macptrs (xp)
          (%setf-macptr-to-object xp (%fixnum-ref frame x8664::xcf.xp))
          (return-from %find-register-argument-value
            (encoded-gpr-lisp xp regval)))
        (progn
          (unless first
            (multiple-value-bind (lfun pc)
                (cfp-lfun frame)
              (when lfun
                (multiple-value-bind (mask where)
                    (registers-used-by lfun pc)
                  (when (if mask (logbitp index mask))
                    (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))


                    (return-from %find-register-argument-value
                      (raw-frame-ref frame context where bad)))))))
          (setq first nil))))
    (get-register-value nil last-catch index)))

(defun %set-register-argument-value (context cfp regval new)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp (child-frame frame context))
          (first t))
         ((null frame))
      (if (xcf-p frame)
        (with-macptrs (xp)
          (%setf-macptr-to-object xp (%fixnum-ref frame x8664::xcf.xp))
          (return-from %set-register-argument-value
            (setf (encoded-gpr-lisp xp regval) new)))
        (progn
          (unless first
            (multiple-value-bind (lfun pc)
                (cfp-lfun frame)
              (when lfun
                (multiple-value-bind (mask where)
                    (registers-used-by lfun pc)
                  (when (if mask (logbitp index mask))
                    (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))

                    (return-from %set-register-argument-value
                      (raw-frame-set frame context where new)))))))
          (setq first nil))))
    (set-register-value new nil last-catch index)))

;;; Used for printing only.
(defun index->address (p)
  (ldb (byte #+32-bit-target 32 #+64-bit-target 64 0)  (ash p target::fixnumshift)))

(defun vsp-limits (frame context)
  (let* ((parent (parent-frame frame context)))
    (if (xcf-p frame)
      (values (+ frame (ash x8664::xcf.size (- x8664::word-shift)))
              parent)
      (let* ((tra (%fixnum-ref frame x8664::lisp-frame.return-address)))
        (values (+ frame 2 (if (eq tra (%get-kernel-global ret1valaddr)) 1 0))
                parent)))))

(defun last-catch-since (fp context)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (catch (%catch-top tcr))
         (last-catch nil))
    (loop
      (unless catch (return last-catch))
      (let ((catch-fp (uvref catch target::catch-frame.rbp-cell)))
        (when (%stack< fp catch-fp context) (return last-catch))
        (setq last-catch catch
              catch (next-catch catch))))))

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
