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


;; L1-boot.lisp


(defparameter *gensym-counter* 0 "counter for generating unique GENSYM symbols")

(defparameter *inhibit-greeting* nil)

;the below 3 variables are expected to be redefined in the user's init file
(defparameter *short-site-name* nil)
(defparameter *long-site-name* nil)
#|
(defparameter *machine-instance* nil)
|#

(defun lisp-implementation-type () "OpenMCL")



(defun host-platform ()
  (let* ((pf (%get-kernel-global 'ppc::host-platform)))
    (case pf
      (0 :macos)
      (1 :linux)
      (2 :vxworks)
      (3 :darwin)
      (16 :solaris)
      (t :unknown))))

(defun lisp-implementation-version ()
  (%str-cat "Version " (format nil *openmcl-version* (software-type))))




(defun replace-base-translation (host-dir new-base-dir)
  (let* ((host (pathname-host host-dir))
         (host-dir (full-pathname host-dir))
         (trans (logical-pathname-translations host))
         (host-wild (merge-pathnames "**/*.*" host-dir)))
    (setq host-dir (pathname-directory host-dir))
    (setq new-base-dir (pathname-directory new-base-dir))
    (setf 
     (logical-pathname-translations host)
     (mapcar
      #'(lambda (pair)
          (let ((rhs (cadr pair)))
            (if (and (physical-pathname-p rhs)
                     (pathname-match-p rhs host-wild))
              (list (car pair)
                    (merge-pathnames 
                     (make-pathname 
                      :defaults nil 
                      :directory (append new-base-dir
                                         (nthcdr (length host-dir) 
                                                 (pathname-directory rhs))))
                     rhs))
              pair)))
      trans))))




; only do these if exist
(defun init-logical-directories ()  
  (let ((startup (mac-default-directory)))
    (replace-base-translation "home:" (or (user-homedir-pathname) startup))
    (replace-base-translation "ccl:" (ccl-directory))
    ))

(push #'init-logical-directories *lisp-system-pointer-functions*)


(catch :toplevel
  (setq *loading-file-source-file* nil)  ;Reset from last %fasload...
  (init-logical-directories)
  )






