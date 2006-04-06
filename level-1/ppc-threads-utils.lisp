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

; low-level support for PPC threads and stack-backtrace printing

(in-package "CCL")


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
  (lfun-bits #'%fixnum-ref-natural
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref-natural)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set-natural
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set-natural)))))

(defun %fixnum-ref-macptr (fixnum &optional (offset 0))
  (%int-to-ptr (%fixnum-ref-natural fixnum offset)))

(defun %fixnum-set-macptr (fixnum offset &optional (newval offset newval-p))
  (%fixnum-set-natural fixnum (if newval-p offset 0) (%ptr-to-int newval))
  newval)
  
				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;



    
    
(defun %frame-backlink (p &optional context)
  (cond ((fake-stack-frame-p p)
         (%fake-stack-frame.next-sp p))
        ((fixnump p)
         (let ((backlink (%%frame-backlink p))
               (fake-frame
                (if context (bt.fake-frames context) *fake-stack-frames*)))
           (loop
             (when (null fake-frame) (return backlink))
             (when (eq backlink (%fake-stack-frame.sp fake-frame))
               (return fake-frame))
             (setq fake-frame (%fake-stack-frame.link fake-frame)))))
        (t (error "~s is not a valid stack frame" p))))




(defun catch-frame-sp (catch)
  (uvref catch target::catch-frame.csp-cell))

(defun bottom-of-stack-p (p context)
  (and (fixnump p)
       (locally (declare (fixnum p))
	 (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
	   (not (%ptr-in-area-p p cs-area))))))

(defun lisp-frame-p (p context)
  (or (fake-stack-frame-p p)
      (locally (declare (fixnum p))
        (let ((next-frame (%frame-backlink p context)))
          (when (fake-stack-frame-p next-frame)
            (setq next-frame (%fake-stack-frame.sp next-frame)))
          (locally (declare (fixnum next-frame))
            (if (bottom-of-stack-p next-frame context)
              (values nil t)
              (and
               (eql (ash target::lisp-frame.size (- target::fixnum-shift))
                    (the fixnum (- next-frame p)))
               ;; EABI C functions keep their saved LRs where we save FN or 0
               ;; The saved LR of such a function would be fixnum-tagged and never 0.
               (let* ((fn (%fixnum-ref p target::lisp-frame.savefn)))
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
    (flet ((save-binding (new-value sym prev)
             (let* ((idx (%svref sym target::symbol.binding-index-cell))
                    (byte-idx (ash idx target::fixnum-shift))
                    (binding-vector (%fixnum-ref (%current-tcr) target::tcr.tlb-pointer))
                    (old-value (%fixnum-ref  binding-vector byte-idx)))
	     (setf (%fixnum-ref binding-vector byte-idx) new-value
                   (%fixnum-ref bsp (ash -1 target::word-shift)) old-value
		   (%fixnum-ref bsp (ash -2 target::word-shift)) idx
		   (%fixnum-ref bsp (ash -3 target::word-shift)) prev
		   bsp (- bsp 3)))))
      (save-binding nil '*current-lisp-thread* 0)
      (save-binding nil '*current-process* bsp)
      (dolist (pair initial-bindings)
	(save-binding (funcall (cdr pair)) (car pair) bsp))
      ;; These may (or may not) be the most recent special bindings.
      ;; If they are, just set the current tcr's db-link to point
      ;; to BSP; if not, "append" them to the end of the current
      ;; linked list.
      (let* ((current-db-link (%fixnum-ref (%current-tcr) target::tcr.db-link)))
        (declare (fixnum current-db-link))
        (if (zerop current-db-link)
          (setf (%fixnum-ref (%current-tcr) target::tcr.db-link) bsp)
          (do* ((binding current-db-link)
                (next (%fixnum-ref binding 0)
                      (%fixnum-ref binding 0)))
               ()
            (if (zerop next)
              (return (setf (%fixnum-ref binding 0) bsp))
              (setq binding next)))))
      ;; Ensure that pending unwind-protects (for WITHOUT-INTERRUPTS
      ;; on the callback) don't try to unwind the binding stack beyond
      ;; where it was just set.
      (do* ((catch (%fixnum-ref (%current-tcr) target::tcr.catch-top)
                   (%fixnum-ref catch target::catch-frame.link)))
           ((zerop catch))
        (declare (fixnum catch))
        (when (eql 0 (%fixnum-ref catch target::catch-frame.db-link))
          (setf (%fixnum-ref catch target::catch-frame.db-link) bsp)))))
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
