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

#|
(def-foreign-type
    openmcl-image-section-header
    (:struct nil
	     (:code :unsigned-long)
	     (:area (:* t))
	     (:memory-size :unsigned-long)
	     (:disk-size :unsigned-long)))
|#

(defparameter *image-section-size* ())



(defparameter *image-header-size* nil)

(defun target-setup-image-header-sizes ()
  (setq *image-header-size* (* 4 16))
  (setq *image-section-size* (* 4 (target-arch-case
                                   (:ppc32 4)
                                   (:ppc64 8)))))

(defun image-write-fullword (w f)
  (write-byte (ldb (byte 8 24) w) f)
  (write-byte (ldb (byte 8 16) w) f)
  (write-byte (ldb (byte 8 8) w) f)
  (write-byte (ldb (byte 8 0) w) f))

(defun image-write-doubleword (dw f)
  (image-write-fullword (ldb (byte 32 32) dw) f)
  (image-write-fullword (ldb (byte 32 0) dw) f))

(defun image-write-natural (n f)
  (target-arch-case
   (:ppc32 (image-write-fullword n f))
   (:ppc64 (image-write-doubleword n f))))

(defun image-align-output-position (f)
  (file-position f (logand (lognot 4095)
			   (+ 4095 (file-position f)))))


(defun write-image-file (pathname image-base spaces)
  (target-setup-image-header-sizes)
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
      (image-write-fullword (target-arch-case
                             (:ppc32 *xload-image-base-address*)
                             (:ppc64 0)) f)
      (image-write-fullword (target-arch-case
                             (:ppc32 image-base)
                             (:ppc64 0)) f)
      (image-write-fullword nsections f)
      (image-write-fullword (dpb *openmcl-major-version*
				 (byte 8 24)
				 (dpb *openmcl-minor-version*
				      (byte 8 16)
				      *openmcl-revision*))
			    f)
      (target-arch-case
       (:ppc32
        (dotimes (i 7) (image-write-fullword 0 f)))
       (:ppc64
        (image-write-fullword 0 f)
        (image-write-fullword 0 f)
        (image-write-fullword 1 f)
        (image-write-doubleword *xload-image-base-address* f)
        (image-write-doubleword image-base f)))
      (dolist (sect spaces)
	(image-write-natural (ash (xload-space-code sect)
                                   *xload-target-fixnumshift*)
			      f)
	(image-write-natural 0 f)
	(let* ((size (xload-space-lowptr sect)))
	  (image-write-natural size f)
	  (image-write-natural size f)))
      (dolist (sect spaces)
	(image-align-output-position f)
	(stream-write-ivector f
			      (xload-space-data sect)
			      0
			      (xload-space-lowptr sect)))
      ;; Write an openmcl_image_file_trailer.
      (image-write-fullword image-sig0 f)
      (image-write-fullword image-sig1 f)
      (image-write-fullword image-sig2 f)
      (let* ((pos (+ 4 (file-position f))))
	(image-write-fullword (- header-pos pos) f))
      nil)))

      
      
    
