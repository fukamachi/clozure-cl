; -*- Mode:Lisp; Package:CCL; -*-
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
(in-package "CCL")

;;; event-metering for "proxy tasks".

(eval-when (:compile-toplevel :execute)
  (require "ARCH")
  (defrecord proxy-event
    (code :unsigned-long)
    (proxy :pointer)
    (timestamp (:array :unsigned-long 2)) ; actually, a timeval struct
    )
  (arch::defenum (:start 0 :prefix "PROXY-EVENT-")
      enqueued				; proxy task ready to run
      dequeued				; lisp notices
      thread-ready			; lisp thread running
      )
 )

;;; Allocate a buffer to hold N proxy-event records.
(defun allocate-proxy-events (n)
  (ff-call (%kernel-import arch::kernel-import-allocate-proxy-events)
	   :unsigned-fullword n
	   :void))

;;; Return a pointer to a previously allocated proxy-event buffer
;;; and the count of proxy-events in that buffer.
(defun retrieve-proxy-events ()
  (rlet ((n :unsigned-long))
    (values (ff-call (%kernel-import arch::kernel-import-retrieve-proxy-events)
		     :address n
		     :address)
	    (%get-unsigned-long n))))

;;; This shouldn't be called directly.
(defun %post-proxy-event (code)
  (ff-call (%kernel-import arch::kernel-import-record-proxy-event)
	   :unsigned-fullword code
	   :address (process.proxy *current-process*)
	   :void))


(defun post-lisp-thread-ready-event ()
  (%post-proxy-event proxy-event-thread-ready))

(defparameter *proxy-event-names*
  `((,proxy-event-enqueued . "TASK ENQUEUED")
    (,proxy-event-dequeued . "TASK DEQUEUED")
    (,proxy-event-thread-ready . "THREAD READY")))

(defun proxy-event-name (code)
  (or (cdr (assoc code *proxy-event-names*)) code))

(defun proxy-event-time (e)
  (+ (* (raref e :proxy-event.timestamp 0) 1000000)
     (raref e :proxy-event.timestamp 1)))

(defun proxy-event-process-name (e)
  (with-macptrs ((p (pref e :proxy-event.proxy)))
    (if (%null-ptr-p p)
       "Lisp Task"
       (let* ((proc (%proxy->process p)))
	 (if proc
	   (process-name proc)
	   "***unknown***")))))

(defun show-proxy-event (e stream)
  (format stream "~& ~12,'0d. : ~a (~a)"
	  (proxy-event-time e)
	  (proxy-event-name (pref e :proxy-event.code))
	  (proxy-event-process-name e)))

(defun print-proxy-events (&key (stream *debug-io*)
				(print-function #'show-proxy-event))
  (multiple-value-bind (base n) (retrieve-proxy-events)
    (unless (%null-ptr-p base)
      (unwind-protect
	(let* ((p (%inc-ptr base 0)))	; doesn't share structure w/base
	  (dotimes (i n)
	    (funcall print-function p stream)
	    (%incf-ptr p (record-length :proxy-event))))
	(free base)))))
