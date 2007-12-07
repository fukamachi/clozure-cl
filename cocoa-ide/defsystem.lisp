;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates
;;;

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))

;; These are used to communicate with ide-bundle, which must be loaded before objc-support.
;; They are defvar'ed so the caller can set them before loading us.
(defvar *cocoa-application-path* nil)
(defvar *cocoa-application-copy-headers-p* nil)
(require "IDE-BUNDLE")

(require "OBJC-SUPPORT")

(require "PTY")


(defpackage "GUI"
  (:use :common-lisp :ccl)
  (:import-from
   "CCL"
   ;; symbols defined here
   *cocoa-application-path*
   *cocoa-application-copy-headers-p*
   load-ide
   build-ide
   ;; Misc symbols that perhaps should be exported from CCL but currently aren't.
   report-bad-arg
   native-translated-namestring
   make-id-map
   assign-id-map-id
   id-map-free-object
   process-thread
   process-serial-number
   ensure-directory-pathname
   recursive-copy-directory
   application
   ;; Symbols that perhaps should be exported by ObjC support but aren't
   @selector
   *nsapp*
   with-nsstr
   %make-nsstring
   lisp-string-from-nsstring
   with-autorelease-pool
   ns-height
   ns-width
   *cocoa-event-process*
   create-autorelease-pool
   release-autorelease-pool
   release-canonical-nsobject
   objc-message-send
   open-main-bundle
   ;; Symbols perhaps that should be exported by library;pty.lisp but aren't
   open-pty-pair
   set-tty-raw
   )
  (:export
   ))

(defparameter *ide-files*
  '(;"ide-bundle" - loaded by hand above
    "cocoa-utils"
    "cocoa-defaults"
    "cocoa-prefs"
    "cocoa-typeout"
    "cocoa-window"
    "cocoa-doc"
    "compile-hemlock"
    "hemlock"  ;; treated specially below, compile-hemlock must come before.
    "cocoa-editor"
    "cocoa-listener"
    "cocoa-grep"
    "cocoa-backtrace"
    "cocoa-inspector"
    "preferences"
    "processes-window"
    "apropos-window"
    "app-delegate"
    "start"
    ))

(defun load-ide (&optional force-compile)
  (declare (special *hemlock-files*)) ;; kludge
  (let ((src-dir "ccl:cocoa-ide;")
	(bin-dir "ccl:cocoa-ide;fasls;"))
    (ensure-directories-exist bin-dir)
    (with-compilation-unit ()
      (dolist (name *ide-files*)
	(let* ((source (make-pathname :name name :type (pathname-type *.lisp-pathname*)
				      :defaults src-dir))
	       (fasl (make-pathname :name name :type (pathname-type *.fasl-pathname*)
				    :defaults bin-dir))
	       (sources (cons source
			      (and (equalp name "hemlock")
				   ;; This is defined in compile-hemlock, which is loaded first
				   (mapcar #'hemlock-source-pathname *hemlock-files*)))))
	  (if (needs-compile-p fasl sources force-compile)
	    (progn
	      ;; Once compile something, keep compiling, in case macros changed.
	      (setq force-compile t)
	      (compile-file source :output-file fasl :verbose t :load t))
	    (load fasl :verbose t)))))
    (provide "COCOA")))