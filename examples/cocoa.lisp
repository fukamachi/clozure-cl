(in-package "CCL")

;;; We need to be able to point the CoreFoundation and Cocoa libraries
;;; at some bundle very early in the process.  If you want to use some
;;; other bundle path, you may need to change the call to FAKE-CFBUNDLE-PATH
;;; below.

#+darwinppc-target
(progn
(require "FAKE-CFBUNDLE-PATH")
(fake-cfbundle-path "ccl:OpenMCL.app;Contents;MacOS;dppccl"))

(defclass cocoa-ide-ui-object (ui-object)
    ())

(setf (application-ui-object *application*) (make-instance 'cocoa-ide-ui-object))

(require "OBJC-SUPPORT")
(require "COCOA-WINDOW")
(require "COCOA-LISTENER")


  


;;; Maintain a list of all open documents.
(defparameter *open-editor-documents* ())

(defparameter *open-editor-documents-lock* (make-lock))


;;; The application delegate gets notified of state changes in the
;;; application object.
(defclass lisp-application-delegate (ns:ns-object)
    ()
  (:metaclass ns:+ns-object))


;;; The application's been initialized and is about to enter its
;;; run loop.  Try to load some application-specific key bindings
;;; from the application's bundle and from the user's Library
;;; directory.
;;; This basically works by repeatedly merging a new dictionary
;;; into an old one (where the new dictionary's bindings replace
;;; any conflicting bindings in the old dictionary.)  The order
;;; in which this happens is:
;;; 1) Standard global bindings:
;;;   /System/Library/Frameworks/AppKit.framework/Resources/StandardKeyBindings.dict
;;; 2) User global bindings:
;;;   ~/Library/KeyBindings/DefaultKeyBinding.dict
;;; 3) Standard application bindings:
;;;   $OpenMCL.app/Contents/Resources/OpenMCLKeyBindings.dict
;;; 4) User application bindings:
;;;   ~/Library/KeyBindings/OpenMCLKeyBindings.dict
;;;
;;; This means that OpenMCL's "standard" bindings may override the user's
;;; global bindings.  The user does get the last laugh, but it may be
;;; worth inverting steps 2 and 3.
;;;
;;; The NSKeyBindingManager class doesn't seem to be documented, so I
;;; suppose that this could all break at some point.  Project Builder
;;; seems to use a similar mechanism to establish its application-specific
;;; key bindings, so I don't think that this is likely to break soon.
(define-objc-method ((:void :application-will-finish-launching (:id notification))
		     lisp-application-delegate)
  (declare (ignore notification))
  (let* ((standard-dict-path (send (send (@class ns-bundle) 'main-bundle)
				   :path-for-resource #@"OpenMCLKeyBindings"
				   :of-type #@"dict"))
	 (standard-dict (send (@class ns-dictionary)
			      :dictionary-with-contents-of-file
			      standard-dict-path))
	 (user-dict-path (send
			  #@"~/Library/KeyBindings/OpenMCLKeyBindings.dict"
			  'string-by-expanding-tilde-in-path))
	 (user-dict (send (@class ns-dictionary)
			  :dictionary-with-contents-of-file user-dict-path))
	 (manager (send (@class ns-key-binding-manager) 'shared-key-binding-manager))
	 (installed-dict (send manager 'dictionary)))
    (unless (%null-ptr-p standard-dict)
      (send installed-dict :add-entries-from-dictionary standard-dict))
    (unless (%null-ptr-p user-dict)
      (send installed-dict :add-entries-from-dictionary user-dict))
    ;; Not sure if this step is necessary (installed-dict is already
    ;; the shared manager's dictionary), but setDictionary: might
    ;; have some additional side-effects.
    (send manager :set-Dictionary  installed-dict)))

(define-objc-method ((:void :new-listener sender) lisp-application-delegate)
  (declare (ignore sender))
  (send (send (@class ns-document-controller) 'shared-document-controller)
	:open-untitled-document-of-type #@"Listener" :display t))

(defvar *cocoa-application-finished-launching* (make-semaphore)
  "Semaphore that's signaled when the application's finished launching ...")

(define-objc-method ((:void :application-did-finish-launching notification)
		     lisp-application-delegate)
  (declare (ignore notification))
  (signal-semaphore *cocoa-application-finished-launching*))


(define-objc-method ((:<BOOL> :application-open-untitled-file app)
		     lisp-application-delegate)
  (when (zerop *cocoa-listener-count*)
    (send self :new-listener app)
    t))


(start-cocoa-application)

