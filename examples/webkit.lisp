;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA"))

;;; Create web browser objects, via the OSX WebKit.
;;; As of this writing (July 2003), the WebKit framework is installed
;;;  as part of Safari 1.0 (but apparently not as part of earlier versions
;;;  of Safari); Safari 1.0 is available as a free download from Apple's
;;;  web site.

(let* ((checked-for-webkit nil)
       (webkit-loaded nil))
  (defun reset-checked-for-webkit ()
    (setq checked-for-webkit nil
	  webkit-loaded nil))
  (defun check-for-webkit ()
    (if checked-for-webkit
      webkit-loaded
      (with-autorelease-pool
	  (let* ((bundle
		  (send
		   (@class "NSBundle")
		   :bundle-with-path
		   #@"/System/Library/Frameworks/WebKit.framework")))
	    (setq checked-for-webkit t
		  webkit-loaded (unless (%null-ptr-p bundle)
				  (send bundle 'load))))))))

(defun require-webkit ()
  (or (check-for-webkit)
      (error "The WebKit framework doesn't seem to be installed on this machine.~&It's available as part of Safari 1.0.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require-webkit)
  (update-type-signatures))

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
		   'ns-window
		   :with-content-rect r
		   :style-mask (logior #$NSTitledWindowMask
				       #$NSClosableWindowMask
				       #$NSMiniaturizableWindowMask
				       #$NSResizableWindowMask)
		   ;; Backing styles other than #$NSBackingStoreBuffered
		   ;; don't work at all in Cocoa.
		   :backing #$NSBackingStoreBuffered
		   :defer nil)))
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
	