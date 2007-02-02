
;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA")
  (augment-objc-interfaces :webkit)
  )

;;; Create web browser objects, via the OSX WebKit.
;;; WebKit is bundled with versions of OSX >= 10.3; it is (or was)
;;; also available as part of Safari 1.0 (for OSX 10.2).
;;; Some very old versions had a bug which rendered NSTextViews
;;; inoperable if WebKit was loaded after an NSTextView had been
;;; created.

(let* ((checked-for-webkit nil)
       (webkit-loaded nil))
  (defun reset-checked-for-webkit ()
    (setq checked-for-webkit nil
	  webkit-loaded nil))
  (defun check-for-webkit ()
    (if checked-for-webkit
      webkit-loaded
      (setq checked-for-webkit t
            webkit-loaded (load-objc-extension-framework "WebKit")))))

(defun require-webkit () 
  (or (check-for-webkit)
      (error "The WebKit framework doesn't seem to be installed on this machine.~&
	      It's available as part of Safari 1.0.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require-webkit))

(defun pathname-to-file-url (pathname)
  ;; NATIVE-TRANSLATED-NAMESTRING returns a simple string that can be
  ;; passed to a filesystem function.  (It may be exactly the same as
  ;; what NAMESTRING returns, or it may differ if special characters
  ;; were escaped in NAMESTRING's result.)
  (with-autorelease-pool
   (send       
    (send (@class "NSURL")
	  :file-url-with-path (%make-nsstring
			       (native-translated-namestring pathname)))
    'retain)))

(defun url-from-string (s)
  (with-autorelease-pool
   (send 
    (send (@class "NSURL") "URLWithString:" (%make-nsstring (string s)))
    'retain)))
		  

(defun browser-window (urlspec)
  ;; Content rect for window, bounds rect for view.
  (slet ((r (ns-make-rect 100.0 100.0 800.0 600.0)))
	(with-autorelease-pool 
	 (let* ((url (if (typep urlspec 'pathname)
			 (pathname-to-file-url urlspec)
			 (url-from-string urlspec)))
		;; Create a window with titlebar, close & iconize buttons
		(w (make-objc-instance
		    'ns:ns-window
		    :with-content-rect r
		    :style-mask (logior #$NSTitledWindowMask
					#$NSClosableWindowMask
					#$NSMiniaturizableWindowMask
					#$NSResizableWindowMask)
		    ;; Backing styles other than #$NSBackingStoreBuffered
		    ;; don't work at all in Cocoa.
		    :backing #$NSBackingStoreBuffered
		    :defer t)))
	   (send w :set-title (send (the ns-url url) 'absolute-string))
	   ;; Create a web-view instance,
	   (let* ((v (make-objc-instance
		      'web-view
		      :with-frame r
		      :frame-name #@"frame"	; could be documented a bit better ...
		      :group-name #@"group"))) ; as could this
	     ;; Make the view be the window's content view.
	     (send w :set-content-view v)
	     ;; Start a URL request.  The request is processed
	     ;; asynchronously, but apparently needs to be initiated
	     ;; from the event-handling thread.
	     (let* ((webframe (send (the web-view v) 'main-frame))
		    (request (send (@class "NSURLRequest") :request-with-url url)))
	       (send (the web-frame webframe)
		     :perform-selector-on-main-thread
		     (@selector "loadRequest:")
		     :with-object  request
		     ;; Failing to wait until the main thread has
		     ;; initiated the request seems to cause
		     ;; view-locking errors.  Maybe that's just
		     ;; an artifact of some other problem.
		     :wait-until-done t)
	       ;; Make the window visible & activate it
	       ;; The view knows how to draw itself and respond
	       ;; to events.
	       (send w :make-key-and-order-front nil))
	     v)))))
	
;;; (browser-window "http://openmcl.clozure.com")
