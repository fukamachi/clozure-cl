;;;; -*- mode: lisp -*-
;;;; Copyright 2002 Barry Perryman. All rights reserved.
;;;; 
;;;; Please send all problem reports to gekki_uk@hotmail.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:
;;;; 
;;;;    1. Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;    2. Redistributions in binary form must reproduce the above
;;;;       copyright notice, this list of conditions and the following
;;;;       disclaimer in the documentation and/or other materials provided
;;;;       with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE BARRY PERRYMAN ``AS IS'' AND ANY
;;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE BARRY PERRYMAN OR
;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; finger.lisp
;;;; A simple finger client and server as specified by RFC 1288.
;;;;
;;;; Changes:
;;;; 2002-07-15: New processes are optional. The system can now forward on
;;;;             nested queries onto other servers, which can be a security
;;;;             risk, so by default this is not enabled.
;;;;
;;;;
;;;; TODO:
;;;; * Gracefully handle errors - well, any error handling would be good.
;;;;

(defpackage NET.PROTOCOLS
  (:use common-lisp ccl)
  (:export
   "WRITE-NET-LINE"
   "READ-NET-LINE"
   "FINGER"
   "POPULATE-VENDING-MACHINE"
   "VENDING-MACHINE-DEMO"))

(in-package :net.protocols)

(defconstant +input-buffer-size+ 1024
  "Size of the input buffer used by read-sequence.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start off with a couple of utility functions
(defun write-net-line (line stream)
  "Write out the string line to the stream, terminating with CRLF."
  (format stream "~a~c~c" line #\return #\linefeed))

(defun read-net-line (stream)
  "Read a line from stream."
  (let ((line (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
    (do ((c (read-char stream nil nil) (read-char stream nil nil)))
	((or (null c)
	     (and (char= c #\return)
		  (char= (peek-char nil stream nil nil) #\linefeed)))
	 (progn
	   (read-char stream nil nil)
	   line))
      (vector-push-extend c line))))

(defmacro aif (test yes no)
  `(let ((it ,test))
    (if it
	,yes
	,no)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now we get on with the meat of it.
(defun %finger (host query port)
  "Send query to host:port using the finger protocol, RFC 1288. Returns the output as a string."
  (declare (ignore verbose))
  (with-open-socket (net :remote-host host :remote-port port)
    (write-net-line query net)
    (force-output net)			; Doesn't seem to be needed, but just incase
    (let ((inbuf (make-array +input-buffer-size+ :element-type 'character :initial-element #\space)))
      (do* ((pos (read-sequence inbuf net) (read-sequence inbuf net))
	    (output (subseq inbuf 0 pos) (concatenate 'string output (subseq inbuf 0 pos))))
	   ((zerop pos) output)))))

(defun finger (query &optional (verbose nil) (port 79))
  "Takes a query, in the same format as the unix command line tool and execute it."
  (let (host
	(host-query (if verbose "/W " "")))
    (aif (position #\@ query :from-end t)
	 (setf host (subseq query (1+ it))
	       host-query (concatenate 'string host-query (subseq query 0 it)))
	 (setf host query))
    (%finger host host-query port)))

;; For testing try:
;;   (finger "idsoftware.com")
;; and/or
;;   (finger "johnc@idsoftware.com") 
;; to find out how Doom 3 is comming along!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server code
(defun finger-daemon (handler &optional (port 79) &key (new-process t) (subqueries nil))
  "Start up a listner on port that responds to the finger protocol"
  (process-run-function (format nil "finger-daemon on port ~d" port) #'%finger-daemon handler port new-process subqueries))
  
(defun %finger-daemon (handler port new-process subqueries)
  "Specific implementation routine."
  (declare (ignore subqueries))
  (with-open-socket (sock :type :stream :connect :passive :local-port port :reuse-address t)
    (do* ((insock (accept-connection sock) (accept-connection sock))
	  (line (read-net-line insock) (read-net-line insock))
	  (proc-handler (if (find #\@ line) #'finger-forward-handler handler)
			(if (find #\@ line) #'finger-forward-handler handler)))
	 ((null insock) nil)
      (if new-process
	  (process-run-function "Finger request handler" #'%finger-daemon-handler proc-handler insock line)
	  (funcall #'%finger-daemon-handler proc-handler insock line)))))

(defun %finger-daemon-handler (handler socket line)
  (let* ((verbose (and (>= (length line) 3)
		       (string= line "/W " :end1 3)))
	 (proc-line (if verbose (subseq line 3) line)))
    (funcall handler proc-line verbose socket)
    (force-output socket)
    (close socket)))

(defun finger-forward-handler (line verbose socket)
  "Handler for forwarding requests a third party"
  (handler-bind ((error #'(lambda (c)
			    (declare (ignore c))
			    (format socket "Unable to process the request.")
			    (return-from finger-forward-handler nil))))
    (let ((ret (finger line verbose)))
      (write-sequence ret socket))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vending machine code, which becomes a simple server
(defstruct vending
  button
  contents
  description
  price)

(defparameter *vending-machine* nil
  "Holds the data for the vending machine.")

(defun populate-vending-machine (data)
  "Takes a list of data in the format (button short-desc long-desc price) and turns it into a vending mahcine."
  (setf *vending-machine* (mapcar #'(lambda (x)
				      (destructuring-bind (b c d p) x
					(make-vending :button b
						      :contents c
						      :description d
						      :price p)))
				  data)))

(populate-vending-machine
 '(("T1" "English Breakfast Tea" "Probably the best tea in the world." 1.0)
   ("T2" "Earl Grey" "Well if you like the taste of washing up liquid..." 1.1)
   ("T3" "Herbal Tea (Various)" "Smells great, tastes just like water." 0.80)
   ("C1" "Cheap 'n' Nasty coffee." "It's awful, doesn't even taste like coffee." 0.50)
   ("C2" "Freeze Dried Coffee." "Do yourself a favour and go to a coffee shop and get a real coffee." 1.0)
   ("H1" "Hot Chocolate" "Carefull, this is so hot it'll cook your tastebuds." 1.0)))

(defun write-vending-machine-details (stream)
  (format stream "~%Button~10,0TContents~50,4TPrice~%")
  (format stream "-------------------------------------------------------~%")
  (dolist (i *vending-machine*)
    (format stream "~a~10,0T~a~50,4T~,2f~%"
	    (vending-button i)
	    (vending-contents i)
	    (vending-price i))))

(defun write-specific-button-details (button stream)
  "This write the specific information for the button"
  (let ((item (find button *vending-machine*
		    :key #'vending-button
		    :test #'string-equal)))
    (cond ((null item)
	   (format stream "Not available on this machine.~%"))
	  (t
	   (format stream "Button: ~a~50,0tPrice: ~,2f~%"
		   (vending-button item)
		   (vending-price item))
	   (format stream "Contents: ~a~%"
		   (vending-contents item))
	   (format stream "Description: ~a~%"
		   (vending-description item))))))

(defun process-vending-machine-command (command verbose stream)
  "This is the vending machine."
  (declare (ignore verbose))
  (if (string= command "")
      (write-vending-machine-details stream)
      (write-specific-button-details command stream)))

(defun vending-machine-demo (port)
  (finger-daemon #'process-vending-machine-command port :new-process t))