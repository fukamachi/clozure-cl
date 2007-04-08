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

(require "COCOA")
;;; Alternately, one could
;;; (require "COCOA-INSPECTOR").  I haven't tried this yet, but think
;;; that it -should- work.

;;; This is a hack to try to set the CCL logical host's translations
;;; appropriately.  If the environment variable CCL_DEFAULT_DIRECTORY
;;; is set, assume that it's correct and do nothing.  Otherwise, if
;;; there's a key #@"CCLDefaultDirectory" in the main bundle's Info.plist,
;;; use it and the bundle's path to set CCL_DEFAULT_DIRECTORY and
;;; setup the CCL logical host translations again.
(defun reset-ccl-directory ()
  (or (getenv "CCL_DEFAULT_DIRECTORY")
      (with-autorelease-pool
          (let* ((bundle (#/mainBundle ns:ns-bundle))
                 (ccl-dir (unless (%null-ptr-p bundle)
                            (#/objectForInfoDictionaryKey: bundle
                                  #@"CCLDefaultDirectory")))
                 (bundle-path (unless (%null-ptr-p bundle)
                                (#/bundlePath bundle))))
            (when (and ccl-dir (not (%null-ptr-p ccl-dir))
                       bundle-path (not (%null-ptr-p bundle-path)))
              (let* ((bundle-string (lisp-string-from-nsstring bundle-path))
                     (ccl-string (lisp-string-from-nsstring ccl-dir))
                     (bundle-len (length bundle-string)))
                (if (and (> bundle-len 0)
                         (not (eql (schar bundle-string (1- bundle-len)) #\/)))
                  (setq bundle-string (concatenate 'string bundle-string "/")))
                (let* ((default-dir (native-translated-namestring
                                     (merge-pathnames ccl-string bundle-string))))
                  (setenv "CCL_DEFAULT_DIRECTORY" default-dir t)
                  (init-logical-directories))))))))


(defclass cocoa-application (lisp-development-system)
    ())

;;; If we're launched via the Finder, the only argument we'll
;;; get is of the form -psnXXXXXX.  That's meaningless to us;
;;; it's easier to pretend that we didn't get any arguments.
;;; (If it seems like some of this needs to be thought out a
;;; bit better ... I'd tend to agree.)
(defmethod parse-application-arguments ((a cocoa-application))
  (values nil nil nil))

(defmethod toplevel-function ((a cocoa-application) init-file)
  (declare (ignore init-file))
  (reset-ccl-directory)
  (start-cocoa-application))


;;; Wait until we're sure that the Cocoa event loop has started.
(wait-on-semaphore *cocoa-application-finished-launching*)

 
;;; The saved image will be an instance of COCOA-APPLICATION (mostly
;;; so that it'll ignore its argument list.)  When it starts up, it'll
;;; run the Cocoa event loop in the cocoa event process.
;;; If you use an init file ("home:openmcl-init"), it'll be loaded
;;; in an environment in which *STANDARD-INPUT* always generates EOF
;;; and where output and error streams are directed to the OSX console
;;; (see below).  If that causes problems, you may want to suppress
;;; the loading of your init file (via an :INIT-FILE nil arg to
;;; the call to SAVE-APPLICATION, below.)

;;; As things are distributed, the file "dppccl" in the application
;;; bundle is just a placeholder.  LaunchServices may have already
;;; decided that the application isn't really executable and may
;;; have cached that fact; touching the bundle directory
;;; here is an attempt to force LaunchServices to discard that
;;; cached information.

(touch "ccl:openmcl.app;")

(save-application "ccl:OpenMCL.app;Contents;MacOS;dppccl"
                  :prepend-kernel t
		  :application-class 'cocoa-application)

;;; If things go wrong, you might see some debugging information via
;;; the OSX console (/Applications/Utilities/Console.app.)  Standard
;;; and error output for the initial lisp process will be directed
;;; there.

