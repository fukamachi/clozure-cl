;;;-*-Mode: LISP; Package: ccl -*-
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

; low-level support for PPC threads and stack-backtrace printing

(in-package :ccl)


;;; Sure would be nice to have &optional in defppclapfunction arglists
;;; Sure would be nice not to do this at runtime.

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref)))))

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref-u32
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref-u32)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set-u32
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set-u32)))))

(defun %fixnum-ref-macptr (fixnum &optional (offset 0))
  (%int-to-ptr (%fixnum-ref-u32 fixnum offset)))

(defun %fixnum-set-macptr (fixnum offset &optional (newval offset newval-p))
  (%fixnum-set-u32 fixnum (if newval-p offset 0) (%ptr-to-int newval))
  newval)
  
				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defsetf interrupt-level set-interrupt-level)





    
    
(defun %frame-backlink (p &optional (tcr (%current-tcr)))
  (cond ((fake-stack-frame-p p)
         (%fake-stack-frame.next-sp p))
        ((fixnump p)
         (let ((backlink (%%frame-backlink p))
               (fake-frame (symbol-value-in-tcr '*fake-stack-frames* tcr)))
           (loop
             (when (null fake-frame) (return backlink))
             (when (eq backlink (%fake-stack-frame.sp fake-frame))
               (return fake-frame))
             (setq fake-frame (%fake-stack-frame.link fake-frame)))))
        (t (error "~s is not a valid stack frame" p))))





(defun lisp-frame-p (p tcr)
  (or (fake-stack-frame-p p)
      (locally (declare (fixnum p))
        (let ((next-frame (%frame-backlink p tcr)))
          (when (fake-stack-frame-p next-frame)
            (setq next-frame (%fake-stack-frame.sp next-frame)))
          (locally (declare (fixnum next-frame))
            (if (bottom-of-stack-p next-frame tcr)
              (values nil t)
              (and
               (eql (ash ppc32::lisp-frame.size (- ppc32::fixnum-shift))
                    (the fixnum (- next-frame p)))
               ;; EABI C functions keep their saved LRs where we save FN or 0
               ;; The saved LR of such a function would be fixnum-tagged and never 0.
               (let* ((fn (%fixnum-ref p ppc32::lisp-frame.savefn)))
                 (or (eql fn 0) (typep fn 'function))))))))))










;;; A callback to handle foreign thread preparation, initialization,
;;; and termination.
;;; "preparation" involves telling the kernel to reserve space for
;;; some initial thread-specific special bindings.  The kernel
;;; needs to reserve this space on the foreign thread's vstack;
;;; it needs us to tell it how much space to reserve (enough
;;; for bindings of *current-thread*, *current-process*, and
;;; the default initial bindings of *PACKAGE*, etc.)
;;;
;;; "initialization" involves making those special bindings in
;;; the vstack space reserved by the kernel, and setting the
;;; values of *current-thread* and *current-process* to newly
;;; created values.
;;;
;;; "termination" involves removing the current thread and
;;; current process from the global thread/process lists.
;;; "preparation" and "initialization" happen when the foreign
;;; thread first tries to call lisp code.  "termination" happens
;;; via the pthread thread-local-storage cleanup mechanism.
(defcallback %foreign-thread-control (:without-interrupts t :int param :int)
  (declare (fixnum param))
  (cond ((< param 0) (%foreign-thread-prepare))
	((= param 0) (%foreign-thread-initialize) 0)
	(t (%foreign-thread-terminate) 0)))



(defun %foreign-thread-prepare ()
  (let* ((initial-bindings (standard-initial-bindings)))
    (%save-standard-binding-list initial-bindings)
    (* 3 (+ 2 (length initial-bindings)))))


(defun %foreign-thread-initialize ()
  ;; Recover the initial-bindings alist.
  (let* ((bsp (%saved-bindings-address))
	 (initial-bindings (%fixnum-ref bsp )))
    (declare (fixnum bsp))
    ;; Um, this is a little more complicated now that we use
    ;; thread-local shallow binding
    (flet ((save-binding (new-value svar prev)
             (let* ((idx (%svref svar target::svar.idx-cell))
                    (byte-idx (ash idx target::fixnum-shift))
                    (binding-vector (%fixnum-ref (%current-tcr) target::tcr.tlb-pointer))
                    (old-value (%fixnum-ref  binding-vector byte-idx)))
	     (setf (%fixnum-ref binding-vector byte-idx) new-value
                   (%fixnum-ref bsp (ash -1 target::word-shift)) old-value
		   (%fixnum-ref bsp (ash -2 target::word-shift)) idx
		   (%fixnum-ref bsp (ash -3 target::word-shift)) prev
		   bsp (- bsp 3)))))
      (save-binding nil (ensure-svar '*current-lisp-thread*) 0)
      (save-binding nil (ensure-svar '*current-process*) bsp)
      (dolist (pair initial-bindings)
	(save-binding (funcall (cdr pair)) (ensure-svar (car pair)) bsp))
      (setf (%fixnum-ref (%current-tcr) ppc32::tcr.db-link) bsp)
      ;; Ensure that pending unwind-protects (for WITHOUT-INTERRUPTS
      ;; on the callback) don't try to unwind the binding stack beyond
      ;; where it was just set.
      (let* ((top-catch (%fixnum-ref (%current-tcr) ppc32::tcr.catch-top)))
        (unless (eql 0 top-catch)
          (setf (%fixnum-ref top-catch ppc32::catch-frame.db-link) bsp)))))
  (let* ((thread (new-lisp-thread-from-tcr (%current-tcr) "foreign")))
    (setq *current-lisp-thread* thread
	  *current-process*
	  (make-process "foreign" :thread thread))
    (setf (%process-whostate *current-process*) "Foreign thread callback")))
    
;;; Remove the foreign thread's lisp-thread and lisp process from
;;; the global lists.
(defun %foreign-thread-terminate ()
  (let* ((proc *current-process*))
    (when proc (remove-from-all-processes proc))))
