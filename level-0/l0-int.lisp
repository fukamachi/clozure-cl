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
#+allow-in-package
(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")
)


(defun lsh (fixnum count)
  (require-type fixnum 'fixnum)
  (require-type count 'fixnum)
  (if (> count 0) 
    (%ilsl count fixnum)
    (%ilsr (- count) fixnum)))

; this called with fixnum
(defun %iabs  (n)
  (declare (fixnum n))
  (if (minusp  n) (- n) n))

; called with any integer - is there a cmu version of integer/bignum-abs?
(defun %integer-abs (n)
  (number-case n
    (fixnum
     (locally
	 (declare (fixnum n))
       (if (minusp n) (- n) n)))
    (bignum
     (if (minusp n) (- n) n))))


(eval-when (:compile-toplevel :execute)
  (assert (< (char-code #\9) (char-code #\A) (char-code #\a))))

(defun token2int (string start len radix)
  ; simple minded in case you hadn't noticed
  (let* ((n start)
         (end (+ start len))
         (char0 (schar string n))
         (val 0)
         minus)
    (declare (fixnum n end start len radix)) ; as if it mattered
    (when (or (eq char0 #\+)(eq char0 #\-))
      (setq n (1+ n))
      (if (eq char0 #\-)(setq minus t)))
    (while (< n end)
      (let ((code (%scharcode string n)))
        (if (<= code (char-code #\9)) 
          (setq code (- code (char-code #\0)))
          (progn
            (when (>= code (char-code #\a))
              (setq code (- code (- (char-code #\a) (char-code #\A)))))
            (setq code (- code (- (char-code #\A) 10)))))
        (setq val (+ (* val radix) code))
        (setq n (1+ n))))
    (if minus (- val) val)))
  

(defun %integer-to-string (int &optional (radix 10))
  (%pr-integer int radix nil t))


; it may be hard to believe, but this is much faster than the lap version (3 or 4X)
; for fixnums that is 
; (stream-write-string vs stream-tyo ???)

(defun %pr-integer (int &optional (radix 10) (stream *standard-output*) return-it  negate-it)
  (declare (fixnum radix)) ; assume caller has checked
  (if stream 
    (if (eq stream t) (setq stream *terminal-io*))
    (setq stream *standard-output*))
  (let ((digit-string "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))    
    (cond ((fixnump int)  ; ugh                      
           ; 29 bits max magnitude (base 2) + 1 for sign
           (let ((temstring (make-string (- 32 arch::fixnumshift) :element-type 'base-char))
                 (i (- 32 arch::fixnumshift 1))
                 (neg (< int 0))
                 (rem 0))
             (declare (fixnum i rem))
             (declare (dynamic-extent temstring))
             (when neg (setq int (- int)))
             (when (not (fixnump int))
               (return-from %pr-integer (%pr-integer int radix stream return-it t)))
             (locally (declare (fixnum int))  
               (loop
                 (multiple-value-setq  (int rem) (%fixnum-truncate int radix))                 
                 (setf (%schar temstring i)(%schar digit-string rem))
                 (when (eq 0 int)
                   (return))
                 (setq i (1- i)))
               (when neg 
                 (setf (%schar temstring (setq i (1- i))) #\-))
               (if return-it
                 (%substr temstring i (- 32 arch::fixnumshift))
                 (write-string temstring stream :start i :end (- 32 arch::fixnumshift))))))          
          (t (let* ((size-vect #(32 32 16 16 16 16 11 11 11
                                 11 11 11 11 11  8  8  8  8
                                  8  8  8  8  8  8  8  8  8
                                  8  8  8  7  7  7  7  7  7))
                    ; overestimate # digits by a little for weird radix
                    (bigwords (uvsize int))
                    (strlen (1+ (* bigwords (svref size-vect (1- radix)))))
                    (temstring (make-string strlen :element-type 'base-char))
                    (i (1- strlen))
                    (neg (< int 0))
                    ;(rem 0)
                    ;better-bignum-print?
                    )  ; warn
               (declare (dynamic-extent temstring)
                        (fixnum i strlen rem))
               (flet ((do-it (newbig)
                        (print-bignum-2 newbig radix temstring digit-string)))
                 (declare (dynamic-extent #'do-it))
                 (setq i (with-one-negated-bignum-buffer int do-it)))                            
               (when (or neg negate-it) 
                 (setf (%schar temstring (setq i (1- i))) #\-))
               (if return-it
                 (%substr temstring i strlen)
                 (write-string temstring stream :start i :end strlen)))))))



;;; *BASE-POWER* holds the number that we keep dividing into the bignum for
;;; each *print-base*.  We want this number as close to *most-positive-fixnum*
;;; as possible, i.e. (floor (log most-positive-fixnum *print-base*)).
;;; 
(defparameter *base-power* #(NIL NIL 268435456 387420489 268435456 244140625 362797056 
 282475249 134217728 387420489 100000000 214358881 429981696 62748517
  105413504 170859375 268435456 410338673 34012224 47045881 64000000
  85766121 113379904 148035889 191102976 244140625 308915776 387420489
  481890304 20511149 24300000 28629151 33554432 39135393 45435424 52521875 60466176))

;;; *FIXNUM-POWER--1* holds the number of digits for each *print-base* that
;;; fit in the corresponding *base-power*.
;;; 
(defparameter *fixnum-power--1* #(NIL NIL 27 17 13 11 10 9 8 8 7 7 7
                                      6 6 6 6 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4))




(defun print-bignum-2 (big radix string digit-string)
  (declare (optimize (speed 3) (safety 0))
           (simple-base-string string digit-string)) 
  (let* ((divisor (aref *base-power* radix))
         (power (aref *fixnum-power--1* radix))
         (index (1- (length string)))
         (new 0)
         (rem 0))
    (declare (fixnum index divisor power))
    ;(print index)
    (loop
      (multiple-value-setq (new rem) (truncate big divisor))
      (when (< rem 0)
        (Bug (format nil "big = ~s, rem = ~d, new = ~s, divisor = ~d"
                     (%address-of big) rem (%address-of new) divisor)))
      (setq big new)
      
      (let* ((int rem)
             (rem 0)
             (final-index (- index power 1)))
        (loop
          (multiple-value-setq (int rem) (%fixnum-truncate int radix))
          (setf (schar string index)(schar digit-string rem))
          (when (eq 0 int)
            (return index))
          (setq index (1- index)))
        (if (zerop big) (return index)        
            (dotimes (i (- index final-index) index)
              (declare (fixnum i))
              (setq index (1- index))
              (setf (schar string index) #\0)))))))
