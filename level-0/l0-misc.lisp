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



; Miscellany.

(defun memq (item list)
  (do* ((tail list (%cdr tail)))
       ((null tail))
    (if (eq item (car tail))
      (return tail))))




(defun append-2 (y z)
  (if (null y)
    z
    (let* ((new (cons (car y) nil))
           (tail new))
      (declare (list new tail))
      (dolist (head (cdr y))
        (setq tail (cdr (rplacd tail (cons head nil)))))
      (rplacd tail z)
      new)))









(defun dbg (&optional arg)
  (dbg arg))


; This takes a simple-base-string and passes a C string into
; the kernel "Bug" routine.  Not too fancy, but neither is #_DebugStr,
; and there's a better chance that users would see this message.
(defun bug (arg)
  (if (typep arg 'simple-base-string)
    (let* ((len (length arg)))
      (%stack-block ((buf (1+ len)))
        (%copy-ivector-to-ptr arg 0 buf 0 len)
        (setf (%get-byte buf len) 0)
        (ff-call 
         (%kernel-import ppc32::kernel-import-lisp-bug)
         :address buf
         :void)))
    (bug "Bug called with non-simple-base-string.")))

(defun total-bytes-allocated ()
  (%heap-bytes-allocated)
  #+not-any-more
  (+ (unsignedwide->integer *total-bytes-freed*)
     (%heap-bytes-allocated)))

(defun %freebytes ()
  (%normalize-areas)
  (let ((res 0))
    (with-macptrs (p)
      (do-consing-areas (area)
        (when (eql (%fixnum-ref area ppc32::area.code) ppc32::area-dynamic)
          (%setf-macptr-to-object p  area)
          (incf res (- (%get-unsigned-long p ppc32::area.high)
                       (%get-unsigned-long p ppc32::area.active))))))
    res))

(defun %reservedbytes ()
  (with-macptrs (p)
    (%setf-macptr-to-object p (%get-kernel-global 'ppc32::all-areas))
    (- (%get-unsigned-long p ppc32::area.high)
       (%get-unsigned-long p ppc32::area.low))))

(defun object-in-application-heap-p (address)
  (declare (ignore address))
  t)


(defun %usedbytes ()
  (%normalize-areas)
  (let ((static 0)
        (dynamic 0)
        (library 0))
    (with-macptrs (p)
      (do-consing-areas (area)
	(%setf-macptr-to-object p area)
	(let* ((active (%fixnum-ref area ppc32::area.active))
	       (bytes (- (%get-unsigned-long p ppc32::area.active)
			 (%get-unsigned-long p ppc32::area.low)))
	       (code (%fixnum-ref area ppc32::area.code)))
	  (when (object-in-application-heap-p active)
	    (if (eql code ppc32::area-dynamic)
	      (incf dynamic bytes)
	      (if (eql code ppc32::area-staticlib)
		(incf library bytes)
		(incf static bytes)))))))
    (values dynamic static library)))



(defun %stack-space ()
  (%normalize-areas)
  (let ((free 0)
        (used 0))
    (with-macptrs (p)
      (do-gc-areas (area)
	(when (member (%fixnum-ref area ppc32::area.code)
		      '(#.ppc32::area-vstack
			#.ppc32::area-cstack
                      #.ppc32::area-tstack))
	  (%setf-macptr-to-object p area)
	  (let ((active (%get-unsigned-long p ppc32::area.active))
		(high (%get-unsigned-long p ppc32::area.high))
		(low (%get-unsigned-long p ppc32::area.low)))
	    (incf used (- high active))
	    (incf free (- active low))))))
    (values (+ free used) used free)))



; Returns an alist of the form:
; ((thread cstack-free cstack-used vstack-free vstack-used tstack-free tstack-used)
;  ...)
(defun %stack-space-by-lisp-thread ()
  (let* ((res nil))
    (without-interrupts
     (do-unexhausted-lisp-threads (thread)
       (push (cons thread (multiple-value-list (%thread-stack-space thread))) res)))
    res))



; Returns six values.
;   sp free
;   sp used
;   vsp free
;   vsp used
;   tsp free
;   tsp used
(defun %thread-stack-space (&optional (thread *current-lisp-thread*))
  (when (eq thread *current-lisp-thread*)
    (%normalize-areas))
  (labels ((free-and-used (area)
	     (with-macptrs (p)
	       (%setf-macptr-to-object p area)
	       (let* ((low (%get-unsigned-long p ppc32::area.low))
		      (high (%get-unsigned-long p ppc32::area.high))
		      (active (%get-unsigned-long p ppc32::area.active))
		      (free (- active low))
		      (used (- high active)))
		 (loop
		     (setq area (%fixnum-ref area ppc32::area.older))
		     (when (eql area 0) (return))
		   (%setf-macptr-to-object p area)
		   (let ((low (%get-unsigned-long p ppc32::area.low))
			 (high (%get-unsigned-long p ppc32::area.high)))
		     (declare (fixnum low high))
		     (incf used (- high low))))
		 (values free used)))))
    (let* ((tcr (%svref thread ppc32::lisp-thread.tcr-cell)))
      (if (or (null tcr)
	      (zerop (%fixnum-ref (%fixnum-ref tcr ppc32::tcr.cs-area))))
	(values 0 0 0 0 0 0)
	(multiple-value-bind (cf cu) (free-and-used (%fixnum-ref tcr ppc32::tcr.cs-area))
	  (multiple-value-bind (vf vu) (free-and-used (%fixnum-ref tcr ppc32::tcr.vs-area))
	    (multiple-value-bind (tf tu) (free-and-used (%fixnum-ref tcr ppc32::tcr.ts-area ))
	      (values cf cu vf vu tf tu))))))))


(defun room (&optional (verbose :default))
  (let* ((freebytes nil)
         (usedbytes nil)
         (static-used nil)
         (staticlib-used nil)
         (lispheap nil)
         (reserved nil)
         (static nil)
         (stack-total)
         (stack-used)
         (stack-free)
         (stack-used-by-thread nil))
    (with-other-threads-suspended
        (without-gcing
         (setq freebytes (%freebytes))
         (when verbose
           (multiple-value-setq (usedbytes static-used staticlib-used) (%usedbytes))
           (setq lispheap (+ freebytes usedbytes)
                 reserved (%reservedbytes)
                 static (+ static-used staticlib-used))
           (multiple-value-setq (stack-total stack-used stack-free)
             (%stack-space))
           (unless (eq verbose :default)
             (setq stack-used-by-thread (%stack-space-by-lisp-thread))))))
    (format t "~&There are at least ~:D bytes of available RAM.~%"
            freebytes)
    (when verbose
      (flet ((k (n) (round n 1024)))
        (princ "
                  Total Size             Free                 Used")
        (format t "~&Lisp Heap: ~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)"
                lispheap (k lispheap)
                freebytes (k freebytes)
                usedbytes (k usedbytes))
        (format t "~&Stacks:    ~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)"
                stack-total (k stack-total)
                stack-free (k stack-free)
                stack-used (k stack-used))
        (format t "~&Static: ~12T~10D (~DK)  ~33T~10D (~DK) ~54T~10D (~DK)"
                static (k static)
                0 0
                static (k static))
        (format t "~&~,3f MB reserved for heap expansion."
                (/ reserved (float (ash 1 20))))
        (unless (eq verbose :default)
          (terpri)
          (let* ((processes (all-processes)))
            (dolist (thread-info stack-used-by-thread)
              (destructuring-bind (thread sp-free sp-used vsp-free vsp-used tsp-free tsp-used)
                  thread-info
                (let* ((process (dolist (p processes)
                                  (when (eq (process-thread p) thread)
                                    (return p)))))
                  (when process
                    (let ((sp-total (+ sp-used sp-free))
                          (vsp-total (+ vsp-used vsp-free))
                          (tsp-total (+ tsp-used tsp-free)))
                      (format t "~%~a(~d)~%  cstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)~
                               ~%  vstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)~
                               ~%  tstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)"
                              (process-name process)
                              (process-serial-number process)
                              sp-total (k sp-total) sp-free (k sp-free) sp-used (k sp-used)
                              vsp-total (k vsp-total) vsp-free (k vsp-free) vsp-used (k vsp-used)
                              tsp-total (k tsp-total) tsp-free (k tsp-free) tsp-used (k tsp-used)))))))))))))


(defun list-length (l)
  (do* ((n 0 (+ n 2))
        (fast l (cddr fast))
        (slow l (cdr slow)))
       ((null fast) n)
    (declare (fixnum n))
    (if (null (cdr fast))
      (return (the fixnum (1+ n)))
      (if (and (eq fast slow)
               (> n 0))
        (return nil)))))

(defun proper-list-p (x)
  (and (typep x 'list) (not (null (list-length x)))))

(defun proper-sequence-p (x)
  (cond ((typep x 'vector))
	((typep x 'list) (not (null (list-length x))))))


(defun length (seq)
  (seq-dispatch
   seq
   (or (list-length seq)
       (%err-disp $XIMPROPERLIST seq))
   (if (= (the fixnum (typecode seq)) ppc32::subtag-vectorH)
     (%svref seq ppc32::vectorH.logsize-cell)
     (uvsize seq))))

(defun %str-from-ptr (pointer len)
  (%copy-ptr-to-ivector pointer 0 (make-string len :element-type 'base-char) 0 len))

(defun %get-cstring (pointer &optional (offset 0) (end offset))
  (with-macptrs ((p pointer))
    (loop (if (%izerop (%get-byte pointer end))
            (return)
            (setq end (%i+ end 1))))
    (%str-from-ptr (%incf-ptr p offset) (%i- end offset))))

;;; This is mostly here so we can bootstrap shared libs without
;;; having to bootstrap #_strcmp.
;;; Return true if the cstrings are equal, false otherwise.
(defun %cstrcmp (x y)
  (do* ((i 0 (1+ i))
	(bx (%get-byte x i) (%get-byte x i))
	(by (%get-byte y i) (%get-byte y i)))
       ((not (= bx by)))
    (declare (fixnum i bx by))
    (when (zerop bx)
      (return t))))

(defvar %documentation nil)

(defvar %documentation-lock% nil)

(setq %documentation
  (make-hash-table :weak t :size 100 :test 'eq :rehash-threshold .95)
  %documentation-lock% (make-lock))

(defun %put-documentation (thing doc-id doc)
  (with-lock-grabbed (%documentation-lock%)
    (let* ((info (gethash thing %documentation))
	   (pair (assoc doc-id info)))
      (if doc
	(if pair
	  (setf (cdr pair) doc)
	  (setf (gethash thing %documentation) (cons (cons doc-id doc) info)))
	(when pair
	  (if (setq info (nremove pair info))
	    (setf (gethash thing %documentation) info)
	    (remhash thing %documentation))))))
  doc)

(defun %get-documentation (object doc-id)
  (cdr (assoc doc-id (gethash object %documentation))))

;;; This pretends to be (SETF DOCUMENTATION), until that generic function
;;; is defined.  It handles a few common cases.
(defun %set-documentation (thing doc-id doc-string)
  (case doc-id
    (function 
     (if (typep thing 'function)
       (%put-documentation thing t doc-string)
       (if (typep thing 'symbol)
         (let* ((def (fboundp thing)))
           (if def
             (if (or (functionp def)
                     (setq def (macro-function thing)))
               (%put-documentation def t doc-string)
               (%put-documentation thing doc-id doc-string))))
         (if (setf-function-name-p thing)
           (%set-documentation
            (setf-function-name thing) doc-id doc-string)))))
    (variable
     (if (typep thing 'symbol)
       (%put-documentation thing doc-id doc-string)))
    (t (%put-documentation thing doc-id doc-string)))
  doc-string)


(%fhave 'set-documentation #'%set-documentation)

;;; This is intended for use by debugging tools.  It's a horrible thing
;;; to do otherwise.  The caller really needs to hold the heap-segment
;;; lock; this grabs the tcr queue lock as well.
(defun %suspend-other-threads ()
  (ff-call (%kernel-import ppc32::kernel-import-suspend-other-threads)
           :void))

(defun %resume-other-threads ()
  (ff-call (%kernel-import ppc32::kernel-import-resume-other-threads)
           :void))

(defun %lock-recursive-lock (lock)
  (with-macptrs ((p)
		 (owner (%get-ptr lock ppc32::lockptr.owner))
		 (signal (%get-ptr lock ppc32::lockptr.signal)))
    (%setf-macptr-to-object p (%current-tcr))
    (if (eql p owner)
      (incf (%get-unsigned-long lock ppc32::lockptr.count))
      (loop
	(when (eql 1 (%atomic-incf-ptr lock))
	  (setf (%get-ptr lock ppc32::lockptr.owner) p
		(%get-unsigned-long lock ppc32::lockptr.count) 1)
	  (return t))
	(%timed-wait-on-semaphore-ptr signal 1 0)))))

(defun %try-recursive-lock (lock)
  (with-macptrs ((p)
		 (owner (%get-ptr lock ppc32::lockptr.owner)))
    (%setf-macptr-to-object p (%current-tcr))
    (cond ((eql p owner)
	   (incf (%get-unsigned-long lock ppc32::lockptr.count))
	   t)
	  ((eql 0 (%ptr-store-conditional lock 0 1))
	   (setf (%get-ptr lock ppc32::lockptr.owner) p
		(%get-unsigned-long lock ppc32::lockptr.count) 1)
	   t)
	  (t nil))))


(defun %unlock-recursive-lock (lock)
  (with-macptrs ((p)
		 (owner (%get-ptr lock ppc32::lockptr.owner))
		 (signal (%get-ptr lock ppc32::lockptr.signal)))
    (%setf-macptr-to-object p (%current-tcr))
    (unless (eql p owner)
      (error 'not-lock-owner :lock lock))
    (when (eql 0 (decf (the fixnum (%get-unsigned-long lock ppc32::lockptr.count))))
      (setf (%get-ptr lock ppc32::lockptr.owner) (%null-ptr))
      (let* ((pending (1- (the fixnum (%atomic-swap-ptr lock 0)))))
	(declare (fixnum pending))
	(with-macptrs ((waiting (%inc-ptr lock ppc32::lockptr.waiting)))
	  (%atomic-incf-ptr-by waiting pending)
	  (when (>= (the fixnum (%atomic-decf-ptr-if-positive waiting)) 0)
	    (%signal-semaphore-ptr signal)))))
    nil))



 
  
(defun %suspend-tcr (tcr)
  (not (zerop (the fixnum 
                (ff-call (%kernel-import ppc32::kernel-import-suspend-tcr)
                         :unsigned-fullword (ash tcr ppc32::fixnumshift)
                         :unsigned-fullword)))))

(defun %resume-tcr (tcr)
  (not (zerop (the fixnum
		(ff-call (%kernel-import ppc32::kernel-import-resume-tcr)
			 :unsigned-fullword (ash tcr ppc32::fixnumshift)
			 :unsigned-fullword)))))



(defun %rplaca-conditional (cons-cell old new)
  (%store-node-conditional ppc32::cons.car cons-cell old new))

(defun %rplacd-conditional (cons-cell old new)
  (%store-node-conditional ppc32::cons.cdr cons-cell old new))

;;; Atomically push NEW onto the list in the I'th cell of uvector V.
(defun atomic-push-uvector-cell (v i new)
  (let* ((cell (cons new nil))
         (offset (+ ppc32::misc-data-offset (ash 2 i))))
    (loop
      (let* ((old (uvref v i)))
        (rplacd cell old)
        (when (%store-node-conditional offset v old cell)
          (return cell))))))

(defun %atomic-incf-car (cell &optional (by 1))
  (%atomic-incf-node (require-type by 'fixnum)
		     (require-type cell 'cons)
		     ppc32::cons.car))

(defun %atomic-incf-cdr (cell &optional (by 1))
  (%atomic-incf-node (require-type by 'fixnum)
		     (require-type cell 'cons)
		     ppc32::cons.cdr))

(defun %atomic-incf-gvector (v i &optional (by 1))
  (setq v (require-type v 'gvector))
  (setq i (require-type i 'fixnum))
  (%atomic-incf-node by v (+ ppc32::misc-data-offset (ash i 2))))

(defun %atomic-incf-symbol-value (s &optional (by 1))
  (setq s (require-type s 'symbol))
  (let* ((binding-address (%symbol-binding-address s)))
    (declare (fixnum binding-address))
    (if (zerop binding-address)
      (%atomic-incf-node by s ppc32::symbol.vcell-cell)
      (%atomic-incf-node by binding-address 8))))
      


