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



; backtrace.lisp
; low-level support for stack-backtrace printing

(in-package :ccl)



; This PRINTS the call history on *DEBUG-IO*.  It's more dangerous (because of
;  stack consing) to actually return it.
                               
(defun print-call-history (&key (tcr (%current-tcr))
                                (start-frame (%get-frame-ptr))
                                (detailed-p t)
                                (count most-positive-fixnum))
  (if (eq tcr (%current-tcr))
    (%print-call-history-internal tcr start-frame detailed-p count)
    (unwind-protect
	 (progn
	   (%suspend-tcr tcr )
	   (%print-call-history-internal tcr start-frame detailed-p
					 count))
      (%resume-tcr tcr)))
  (values))

(defun %show-stack-frame (p tcr lfun pc)
  (multiple-value-bind (count vsp parent-vsp) (count-values-in-frame p tcr)
    (declare (fixnum count))
    (dotimes (i count)
      (multiple-value-bind (var type name) 
          (nth-value-in-frame p i tcr lfun pc vsp parent-vsp)
        (format t "~&  ~D " i)
        (when name (format t "~s" name))
        (format t ": ~s" var)
        (when type (format t " (~S)" type)))))
  (terpri)
  (terpri))

(defun %print-call-history-internal (tcr start-frame detailed-p
					 &optional (count most-positive-fixnum))
  (let ((*standard-output* *debug-io*)
        (*print-circle* *error-print-circle*))
    (do* ((p start-frame (parent-frame p tcr))
          (frame-number 0 (1+ frame-number))
          (q (last-frame-ptr tcr)))
         ((or (null p) (eq p q) (%stack< q p tcr)
              (>= frame-number count))
          (values))
      (declare (fixnum frame-number frames-shown))
      (progn
        (multiple-value-bind (lfun pc) (cfp-lfun p)
          (unless (and (typep detailed-p 'fixnum)
                       (not (= (the fixnum detailed-p) frame-number)))
            (format t "~&(~x) : ~D ~S ~d"
                    (index->address p) frame-number
                    (if lfun (%lfun-name-string lfun))
                    pc)
            (when detailed-p
              (%show-stack-frame p tcr lfun pc))))))))


(defun %access-lisp-data (vstack-index)
  (%fixnum-ref vstack-index))

(defun %store-lisp-data (vstack-index value)
  (setf (%fixnum-ref vstack-index) value))

(defun closed-over-value (data)
  (if (closed-over-value-p data)
    (uvref data 0)
    data))

(defun set-closed-over-value (value-cell value)
  (setf (uvref value-cell 0) value))



; Act as if VSTACK-INDEX points at some lisp data & return that data.
(defun access-lisp-data (vstack-index)
  (closed-over-value (%access-lisp-data vstack-index)))

(defun find-local-name (cellno lfun pc)
  (let* ((n cellno))
    (when lfun
      (multiple-value-bind (mask where) (registers-used-by lfun pc)
        (if (and where (< (1- where) n (+ where (logcount mask))))
          (let ((j *saved-register-count*))
            (decf n where)
            (loop (loop (if (logbitp (decf j) mask) (return)))
                  (if (< (decf n) 0) (return)))
            (values (format nil "saved ~a" (aref *saved-register-names* j))
                    nil))
          (multiple-value-bind (nreq nopt restp nkeys junk optinitp junk ncells nclosed)
                               (if lfun (function-args lfun))
            (declare (ignore junk optinitp))
            (if nkeys (setq nkeys (+ nkeys nkeys)))
            (values
             (if (and ncells (< n ncells))
               (if (< n nclosed)
                 :inherited
                 (if (< (setq n (- n nclosed)) nreq)
                   "required"
                   (if (< (setq n (- n nreq)) nopt)
                     "optional"
                     (progn
                       (setq n (- n nopt))
                       (if (and restp (eq n 0))
                         "rest"
                         (progn
                           (if restp (setq n (1- n)))
                           (if (and nkeys (< n nkeys))
                             (if (not (logbitp 0 n)) ; a keyword
                               "keyword"
                               "key-supplied-p")
                             "opt-supplied-p"))))))))
             (let* ((info (function-symbol-map lfun))
                    (syms (%car info))
                    (ptrs (%cdr info)))
               (when info
                 (dotimes (i (length syms))
                   (let ((j (%i+ i (%i+ i i ))))
                     (and (eq (uvref ptrs j) (%ilogior (%ilsl 8 cellno) #o77))
                          (%i>= pc (uvref ptrs (%i+ j 1)))
                          (%i< pc (uvref ptrs (%i+ j 2)))
                          (return (aref syms i))))))))))))))

(defun function-args (lfun)
  "Returns 9 values, as follows:
     req = number of required arguments
     opt = number of optional arguments
     restp = t if rest arg
     keys = number of keyword arguments or NIL if &key not mentioned
     allow-other-keys = t if &allow-other-keys present
     optinit = t if any optional arg has non-nil default value or supplied-p
               variable
     lexprp = t if function is a lexpr, in which case all other values are
              undefined.
     ncells = number of stack frame cells used by all arguments.
     nclosed = number of inherited values (now counted distinctly from required)
     All numeric values (but ncells) are mod 64."
  (let* ((bits (lfun-bits lfun))
         (req (ldb $lfbits-numreq bits))
         (opt (ldb $lfbits-numopt bits))
         (restp (logbitp $lfbits-rest-bit bits))
         (keyvect (lfun-keyvect lfun))
         (keys (and keyvect (length keyvect)))
         (allow-other-keys (logbitp $lfbits-aok-bit bits))
         (optinit (logbitp $lfbits-optinit-bit bits))
         (lexprp (logbitp $lfbits-restv-bit bits))
         (nclosed (ldb $lfbits-numinh bits)))
    (values req opt restp keys allow-other-keys optinit lexprp
            (unless (or lexprp)
              (+ req opt (if restp 1 0) (if keys (+ keys keys) 0)
                 (if optinit opt 0) nclosed))
            nclosed)))

;;; If we can tell reliably, return the function's minimum number of
;;; non-inherited arguments, the maximum number of such arguments (or NIL),
;;; and the actual number of such arguments.  We "can't tell" if either
;;; of the arguments to this function are null, and we can't tell reliably
;;; if any of the lfbits fields are full.
(defun min-max-actual-args (fn nargs)
  (let* ((lfbits (if (and fn nargs)
		   (lfun-bits fn)
		   -1))
	 (raw-req (ldb $lfbits-numreq lfbits))
	 (raw-opt (ldb $lfbits-numopt lfbits))
	 (raw-inh (ldb $lfbits-numinh lfbits)))
    (declare (fixnum raw-req raw-opt raw-inh))
    (if (or (eql raw-req (1- (ash 1 (byte-size $lfbits-numreq))))
	    (eql raw-opt (1- (ash 1 (byte-size $lfbits-numopt))))
	    (eql raw-inh (1- (ash 1 (byte-size $lfbits-numinh)))))
      (values nil nil nil)
      (values raw-req
	      (unless (or (lfun-keyvect fn)
			  (logbitp $lfbits-rest-bit lfbits)
			  (logbitp $lfbits-restv-bit lfbits))
		(+ raw-req raw-opt))
	      (- nargs raw-inh)))))
		 
	 
	   



(defun closed-over-value-p (value)
  (eql ppc32::subtag-value-cell (typecode value)))




(defun safe-cell-value (val)
  val)

; Returns two values:
;  [nil, nil] if it can be reliably determined that function uses no registers at PC
;  [mask, savevsp]  if it can be reliably determined that the registers specified by "mask"
;      were saved at "savevsp" in the function's stack frame
;  [mask, nil] if registers in "mask" MAY have been saved, but we don't know how to restore them
;      (perhaps because the "at-pc" argument wasn't specified.


;; If the last instruction in a code vector is an
;; LWZ instruction (of the form "(LWZ rx s16 ry)"),
;; then 
;;   this function uses registers RX-R31.  Note that this leaves
;;    us 2 extra bits, since we're only encoding 3 bits worth of
;;    register info.
;;   RX is saved nearest the top of the vstack
;;   s16 is the offset from the saved-vsp to the address at which
;;    RX was saved; this is a negative value whose low two bits
;;    are ignored
;;   (logior (ash (logand s16 3) 5) rY) is the pc at which
;;   the registers were saved (a fullword code-vector index).
;; This scheme lets us encode any "simple" register usage, where
;; the registers were saved once, saved somewhere within the first 
;; 128 instructions in the code vector, and nothing interesting (to
;; backtrace) happens after the registers have been restored.
;; If the compiler ever gets cleverer about this, we'll have to use
;; some other scheme (perhaps a STW instruction, preceded by branches).
;;
;; Note that the "last instruction" really means "last instruction
;; before any traceback table"; we should be able to truncate the code
;; vector (probably by copying it) to strip off the traceback table
;; without losing this information.
;; Note also that the disassembler would probably ordinarily want to
;; hide this last instruction ...
;;   

#+ppc-target
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


    
    



; End of backtrace.lisp
