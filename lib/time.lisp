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

(eval-when (:compile-toplevel :execute)
  (defconstant seconds-in-week (* 60 60 24 7))
  (defconstant weeks-offset 2145)
  (defconstant seconds-offset 432000)
  (defconstant minutes-per-day (* 24 60))
  (defconstant quarter-days-per-year (1+ (* 365 4)))
  (defconstant quarter-days-per-century 146097)
  (defconstant november-17-1858 678882)
  (defconstant weekday-november-17-1858 2)
)

(defun gctime ()
  (%stack-block ((copy (* 8 5)))
    (#_memmove copy *total-gc-microseconds* (* 8 5))
    (values
     (timeval->milliseconds copy)
     (timeval->milliseconds (%incf-ptr copy 8))
     (timeval->milliseconds (%incf-ptr copy 8))
     (timeval->milliseconds (%incf-ptr copy 8))
     (timeval->milliseconds (%incf-ptr copy 8)))))

(defun get-universal-time ()
  (rlet ((tv :timeval))
    (#_gettimeofday tv (%null-ptr))
    (+ (pref tv :timeval.tv_sec) unix-to-universal-time)))

;;; This should stop using #_localtime: not all times can be represented
;;; as a 32-bit offset from the start of Unix time, and (more importantly)
;;; #_localtime isn't reentrant.
;;; For now, if the time won't fit in 32 bits, use an arbitrary time
;;; value to get the time zone and assume that DST was -not- in effect.
(defun get-timezone (time)
  (let* ((toobig (not (or (typep time '(unsigned-byte 32))
			  (typep time '(signed-byte 32))))))
    (when toobig
      (setq time 0))
    (rlet ((when :long))
      (setf (%get-long when) time)
      (without-interrupts			;reentrancy ?
       (with-macptrs ((ltm (#_localtime when)))
	 (values (floor (pref ltm :tm.tm_gmtoff) -60)
		 (unless toobig (not (zerop (pref ltm :tm.tm_isdst))))))))))


(defun decode-universal-time (universal-time &optional time-zone)
  "Converts a universal-time to decoded time format returning the following
   nine values: second, minute, hour, date, month, year, day of week (0 =
   Monday), T (daylight savings time) or NIL (standard time), and timezone.
   Completely ignores daylight-savings-time when time-zone is supplied."
  (multiple-value-bind (weeks secs)
		       (truncate (+ universal-time seconds-offset)
				 seconds-in-week)
    (let* ((weeks (+ weeks weeks-offset))
	   (second NIL)
	   (minute NIL)
	   (hour NIL)
	   (date NIL)
	   (month NIL)
	   (year NIL)
	   (day NIL)
	   (daylight NIL)
	   (timezone (if (null time-zone)
			 (multiple-value-bind
			     (minwest dst)
			     (get-timezone (- universal-time
					      unix-to-universal-time))
			   (setf daylight dst)
			   minwest)
			 (* time-zone 60))))
      (declare (fixnum timezone))
      (multiple-value-bind (t1 seconds) (truncate secs 60)
	(setq second seconds)
	(setq t1 (- t1 timezone))
	(let* ((tday (if (< t1 0)
			 (1- (truncate (1+ t1) minutes-per-day))
			 (truncate t1 minutes-per-day))))
	  (multiple-value-setq (hour minute)
	    (truncate (- t1 (* tday minutes-per-day)) 60))
	  (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
		 (tcent (truncate t2 quarter-days-per-century)))
	    (setq t2 (mod t2 quarter-days-per-century))
	    (setq t2 (+ (- t2 (mod t2 4)) 3))
	    (setq year (+ (* tcent 100) (truncate t2 quarter-days-per-year)))
	    (let ((days-since-mar0 (1+ (truncate (mod t2 quarter-days-per-year)
						 4))))
	      (setq day (mod (+ tday weekday-november-17-1858) 7))
	      (let ((t3 (+ (* days-since-mar0 5) 456)))
		(cond ((>= t3 1989)
		       (setq t3 (- t3 1836))
		       (setq year (1+ year))))
		(multiple-value-setq (month t3) (truncate t3 153))
		(setq date (1+ (truncate t3 5))))))))
      (values second minute hour date month year day
	      daylight
	      (if daylight
		  (1+ (/ timezone 60))
		  (/ timezone 60))))))

(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
	  (truncate years 100))
       (truncate (+ years 300) 400))))

(defvar *days-before-month*
  (let* ((results (list nil)))
    (let ((sum 0))
      (dolist (days-per-month '(31 28 31 30 31 30 31 31 30 31 30 31))
	(push sum results)
	(incf sum days-per-month)))
    (coerce (nreverse results) 'vector)))

(defun encode-universal-time (second minute hour date month year
				     &optional time-zone)
  "The time values specified in decoded format are converted to 
   universal time, which is returned."
  (declare (type (mod 60) second)
	   (type (mod 60) minute)
	   (type (mod 24) hour)
	   (type (integer 1 31) date)
	   (type (integer 1 12) month)
	   (type (or (integer 0 99) (integer 1900)) year)
	   (type (or null rational) time-zone))
  (let* ((days (+ (1- date)
		  (aref *days-before-month* month)
		  (if (> month 2)
		    (leap-years-before (1+ year))
		    (leap-years-before year))
		  (* (- year 1900) 365)))
	 (hours (+ hour (* days 24))))
    (if time-zone
      (+ second (* (+ minute (* (+ hours time-zone) 60)) 60))
      (let* ((minwest-guess
	      (get-timezone (- (* hours 60 60)
			       unix-to-universal-time)))
	     (guess (+ minute (* hours 60) minwest-guess))
	     (minwest
	      (get-timezone (- (* guess 60)
			       unix-to-universal-time))))
	(+ second (* (+ guess (- minwest minwest-guess)) 60))))))



(defun sleep (seconds)
  (when (minusp seconds) (report-bad-arg seconds '(real 0 *)))
  (let* ((tps *ticks-per-second*)
	 (npt *ns-per-tick*)
	 (ticks (ceiling (* seconds tps))))
    (%nanosleep (floor ticks tps) (* npt (mod ticks tps)))))

(defun get-internal-run-time ()
  (rlet ((usage :rusage)
	 (total :timeval))
    (%%rusage usage)
    (timeval->milliseconds (%add-timevals total 
					  (pref usage :rusage.ru_utime) 
					  (pref usage :rusage.ru_stime)))))
(defun get-internal-real-time ()
  (rlet ((tv :timeval))
    (#_gettimeofday tv (%null-ptr))
    (timeval->milliseconds tv)))
