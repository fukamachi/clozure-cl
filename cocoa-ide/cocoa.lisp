(in-package "CCL")

;;; We need to be able to point the CoreFoundation and Cocoa libraries
;;; at some bundle very early in the process.  If you want to use some
;;; other bundle path, you may need to change the call to FAKE-CFBUNDLE-PATH
;;; below.

#+darwin-target
(progn
  (require "FAKE-CFBUNDLE-PATH")
  (defparameter *fake-cfbundle-path*
    #+clozure-common-lisp "ccl:cocoa-ide;Clozure CL.app;"
    #-clozure-common-lisp "ccl:cocoa-ide;OpenMCL.app;")
  (fake-cfbundle-path *fake-cfbundle-path* "ccl:cocoa-ide;Info.plist-proto" "com.clozure"))


(require "OBJC-SUPPORT")

(if (< #&NSAppKitVersionNumber 824)
  (error "This application requires features introduced in OSX 10.4."))

(defparameter *standalone-cocoa-ide* nil)

  
(require "COCOA-UTILS")
(require "COCOA-WINDOW")
(require "COCOA-DOC")
(require "COCOA-LISTENER")
(require "COCOA-GREP")
(require "COCOA-BACKTRACE")
(require "COCOA-INSPECTOR")
(require "PROCESSES-WINDOW")

(def-cocoa-default *ccl-directory* :string "" nil #'(lambda (old new)
						      (when (equal new "") (setq new nil))
						      (unless (and new (equal old new))
							(init-interfaces-root)
							(replace-base-translation "ccl:"
										  (or new (find-ccl-directory))))))

;; If there are interfaces inside the bundle, use those rather than the ones
;; in CCL:, since they're more likely to be valid.  CCL: could be some random
;; sources we're just using just for meta-.
(defun init-interfaces-root ()
  (let* ((subpath (cdb-subdirectory-path))
	 (path (pathname-directory (ccl-directory))))
    (when (and *standalone-cocoa-ide*
	       (equalp (last path 2) '("Contents" "MacOS")))
      (setq path (butlast path))
      (when (or (probe-file (make-pathname :directory (append path subpath)))
		(probe-file (make-pathname :directory (append (setq path `(,@path "Resources")) subpath))))
	(setq *interfaces-root* (make-pathname :directory path))))))

(defun find-ccl-directory ()
  (let* ((path (ccl-directory))
	 (dir (pathname-directory path)))
    (if (equalp (last dir 2) '("Contents" "MacOS"))
	(make-pathname :directory (butlast dir 3))
	path)))



;;; The application delegate gets notified of state changes in the
;;; application object.
(defclass lisp-application-delegate (ns:ns-object)
    ()
  (:metaclass ns:+ns-object))


(objc:defmethod (#/applicationWillFinishLaunching: :void)
    ((self lisp-application-delegate) notification)
  (declare (ignore notification))
  (initialize-user-interface))

(objc:defmethod (#/applicationWillTerminate: :void)
    ((self lisp-application-delegate) notification)
  (declare (ignore notification))
  ;; UI has decided to quit; terminate other lisp threads.
  (prepare-to-quit))

(objc:defmethod (#/newListener: :void) ((self lisp-application-delegate)
                                        sender)
  (declare (ignore sender))
  (#/openUntitledDocumentOfType:display:
   (#/sharedDocumentController ns:ns-document-controller)
   #@"Listener"
   t))

(objc:defmethod (#/showListener: :void) ((self lisp-application-delegate)
                                        sender)
  (declare (ignore sender))
  (let* ((all-windows (#/orderedWindows *NSApp*))
	 (key-window (#/keyWindow *NSApp*))
	 (listener-windows ())
	 (top-listener nil))
    (dotimes (i (#/count all-windows))
      (let* ((w (#/objectAtIndex: all-windows i))
	     (wc (#/windowController w)))
	(when (eql (#/class wc) hemlock-listener-window-controller)
	  (push w listener-windows))))
    (setq listener-windows (nreverse listener-windows))
    (setq top-listener (car listener-windows))
    (cond 
     ((null listener-windows)
      (#/newListener: self +null-ptr+))
     ((eql key-window top-listener)
      ;; The current window is a listener.  If there is more than
      ;; one listener, bring the rear-most forward.
      (let* ((w (car (last listener-windows))))
	(if (eql top-listener w)
	  (#_NSBeep)
	  (#/makeKeyAndOrderFront: w +null-ptr+))))
     (t
      (#/makeKeyAndOrderFront: (car listener-windows) +null-ptr+)))))
  
(defloadvar *processes-window-controller* nil)

(objc:defmethod (#/showProcessesWindow: :void) ((self lisp-application-delegate)
						sender)
  (declare (ignore sender))
  (when (null *processes-window-controller*)
    (setf *processes-window-controller* (make-instance 'processes-window-controller)))
  (#/showWindow: *processes-window-controller* self))

(defvar *cocoa-application-finished-launching* (make-semaphore)
  "Semaphore that's signaled when the application's finished launching ...")

(objc:defmethod (#/applicationDidFinishLaunching: :void)
    ((self lisp-application-delegate) notification)
  (declare (ignore notification))
  (signal-semaphore *cocoa-application-finished-launching*))

(objc:defmethod (#/applicationOpenUntitledFile: :<BOOL>)
    ((self lisp-application-delegate) app)
  (when (zerop *cocoa-listener-count*)
    (#/newListener: self app)
    t))


(defmethod ui-object-do-operation ((o ns:ns-application)
                                   operation
                                   &rest args)
  (declare (ignore operation args))
  ;; Do nothing.  Would it be better to warn and/or log this ?
  )

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :note-current-package))
                                   &rest args)
  (ui-object-note-package o (car args)))

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :eval-selection))
                                   &rest args)
  (ui-object-eval-selection o (car args)))

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :enter-backtrace-context))
                                   &rest args)
  (ui-object-enter-backtrace-context o (car args)))

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :exit-backtrace-context))
                                   &rest args)
  (ui-object-exit-backtrace-context o (car args)))




(start-cocoa-application)

