;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2002-2003 Clozure Associates
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


(in-package "CCL")			; for now.

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))

;;; loading cocoa.lisp creates an IDE bundle in *cocoa-application-path*,
;;; perhaps copying headers as per *cocoa-application-copy-headers-p*
(defvar *cocoa-application-path* "ccl:Clozure CL.app;")
(defvar *cocoa-application-copy-headers-p* t)
(require "COCOA")

(defclass cocoa-application (application)
    ())

(defmethod application-error ((a application) condition error-pointer)
  (break-loop-handle-error condition error-pointer))


;;; If we're launched via the Finder, the only argument we'll
;;; get is of the form -psnXXXXXX.  That's meaningless to us;
;;; it's easier to pretend that we didn't get any arguments.
;;; (If it seems like some of this needs to be thought out a
;;; bit better ... I'd tend to agree.)
(defmethod parse-application-arguments ((a cocoa-application))
  (values nil nil nil nil))

(defmethod toplevel-function ((a cocoa-application) init-file)
  (declare (ignore init-file))
  (when (< #&NSAppKitVersionNumber 824)
    (#_NSLog #@"This application requires features introduced in OSX 10.4.")
    (#_ _exit -1))
  (setq *standalone-cocoa-ide* t)
  ;; TODO: to avoid confusion, should now reset *cocoa-application-path* to
  ;; actual bundle path where started up.
  (start-cocoa-application))


  ;;; The saved image will be an instance of COCOA-APPLICATION (mostly
  ;;; so that it'll ignore its argument list.)  When it starts up, it'll
  ;;; run the Cocoa event loop in the cocoa event process.
  ;;; If you use an init file ("home:ccl-init"), it'll be loaded
  ;;; in an environment in which *STANDARD-INPUT* always generates EOF
  ;;; and where output and error streams are directed to the OSX console
  ;;; (see below).  If that causes problems, you may want to suppress
  ;;; the loading of your init file (via an :INIT-FILE nil arg to
  ;;; the call to SAVE-APPLICATION, below.)

(defun build-ide (bundle-path)
  (setq bundle-path (ensure-directory-pathname bundle-path))

  ;; The bundle is expected to exists, we'll just add the executable into it.
  (assert (probe-file bundle-path))

  ;; Wait until we're sure that the Cocoa event loop has started.
  (wait-on-semaphore *cocoa-application-finished-launching*)

  (require :easygui)

  (maybe-map-objc-classes t)
  (let* ((missing ()))
    (do-interface-dirs (d)
      (cdb-enumerate-keys
       (db-objc-classes d)
       (lambda (name)
	 (let* ((class (lookup-objc-class name nil))) (unless (objc-class-id  class) (push name missing))))))
    (when missing
      (break "ObjC classes ~{~&~a~} are declared but not defined." missing)))

  (touch bundle-path)

  (let ((image-file (make-pathname :name (standard-kernel-name) :type nil :version nil
				   :defaults (merge-pathnames ";Contents;MacOS;" bundle-path))))
    (ensure-directories-exist image-file)
    (save-application image-file
		      :prepend-kernel t
		      :application-class 'cocoa-application)))

;;; If things go wrong, you might see some debugging information via
;;; the OSX console (/Applications/Utilities/Console.app.)  Standard
;;; and error output for the initial lisp process will be directed
;;; there.
(build-ide *cocoa-application-path*)
