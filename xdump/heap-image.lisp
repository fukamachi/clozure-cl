;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2002 Clozure Associates
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
(defconstant image-sig0 (dpb (char-code #\O)
			     (byte 8 24)
			     (dpb (char-code #\p)
				  (byte 8 16)
				  (dpb (char-code #\e)
				       (byte 8 8)
				       (char-code #\n)))))
(defconstant image-sig1 (dpb (char-code #\M)
			     (byte 8 24)
			     (dpb (char-code #\C)
				  (byte 8 16)
				  (dpb (char-code #\L)
				       (byte 8 8)
				       (char-code #\I)))))
(defconstant image-sig2 (dpb (char-code #\m)
			     (byte 8 24)
			     (dpb (char-code #\a)
				  (byte 8 16)
				  (dpb (char-code #\g)
				       (byte 8 8)
				       (char-code #\e)))))
(defconstant image-sig3 (dpb (char-code #\F)
			     (byte 8 24)
			     (dpb (char-code #\i)
				  (byte 8 16)
				  (dpb (char-code #\l)
				       (byte 8 8)
				       (char-code #\e)))))

(def-foreign-type
    openmcl-image-section-header
    (:struct nil
	     (:code :unsigned)
	     (:area (:* t))
	     (:memory-size :unsigned)
	     (:disk-size :unsigned)))

(defparameter *image-section-size*
  (%foreign-type-or-record-size :openmcl-image-section-header :bytes))

(def-foreign-type
    openmcl-image-file-header
    (:struct nil
	     (:sig0 :unsigned)
     	     (:sig1 :unsigned)
	     (:sig2 :unsigned)
	     (:sig3 :unsigned)
	     (:timestamp :unsigned)
	     (:canonical-image-base :unsigned)
	     (:actual-image-base :unsigned)
	     (:nsections :unsigned)
	     (:abi-version :unsigned)
	     (:pad (:array :unsigned 7))))


(defparameter *image-header-size*
  (%foreign-type-or-record-size :openmcl-image-file-header :bytes))

(defun image-write-fullword (w f)
  (write-byte (ldb (byte 8 24) w) f)
  (write-byte (ldb (byte 8 16) w) f)
  (write-byte (ldb (byte 8 8) w) f)
  (write-byte (ldb (byte 8 0) w) f))

(defun image-align-output-position (f)
  (file-position f (logand (lognot 4095)
			   (+ 4095 (file-position f)))))


(defun write-image-file (pathname image-base spaces)
  (with-open-file (f pathname
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
    (let* ((nsections (length spaces))
	   (header-pos (- 4096 (+ *image-header-size*
				(* nsections *image-section-size*)))))
      (file-position f header-pos)
      (image-write-fullword image-sig0 f)
      (image-write-fullword image-sig1 f)
      (image-write-fullword image-sig2 f)
      (image-write-fullword image-sig3 f)
      (image-write-fullword (get-universal-time) f)
      (image-write-fullword *xload-image-base-address* f)
      (image-write-fullword image-base f)
      (image-write-fullword nsections f)
      (image-write-fullword (dpb *openmcl-major-version*
				 (byte 8 24)
				 (dpb *openmcl-minor-version*
				      (byte 8 16)
				      *openmcl-revision*))
			    f)
      (dotimes (i 7) (image-write-fullword 0 f))
      (dolist (sect spaces)
	(image-write-fullword (ash (xload-space-code sect) target::fixnumshift)
			      f)
	(image-write-fullword 0 f)
	(let* ((size (xload-space-lowptr sect)))
	  (image-write-fullword size f)
	  (image-write-fullword size f)))
      (dolist (sect spaces)
	(image-align-output-position f)
	(stream-write-ivector f
			      (xload-space-data sect)
			      0
			      (xload-space-lowptr sect)))
      (image-write-fullword image-sig0 f)
      (image-write-fullword image-sig1 f)
      (image-write-fullword image-sig2 f)
      (let* ((pos (+ 4 (file-position f))))
	(image-write-fullword (- header-pos pos) f))
      nil)))

      
      
    
