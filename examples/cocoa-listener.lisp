;;;-*- Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA-EDITOR")
  (require "PTY"))

(def-cocoa-default *listener-rows* :int 16)
(def-cocoa-default *listener-columns* :int 80)

;;; Setup the server end of a pty pair.
(defun setup-server-pty (pty)
  pty)

;;; Setup the client end of a pty pair.
(defun setup-client-pty (pty)
  ;; Since the same (Unix) process will be reading from and writing
  ;; to the pty, it's critical that we make the pty non-blocking.
  (fd-set-flag pty #$O_NONBLOCK)
  (disable-tty-local-modes pty (logior #$ECHO #$ECHOCTL #$ISIG))
  pty)


(defloadvar *cocoa-listener-count* 0)

(defclass cocoa-listener-process (process)
    ((input-stream :reader cocoa-listener-process-input-stream)))

(defun new-cocoa-listener-process (procname input-fd output-fd peer-fd)
  (let* ((input-stream (make-selection-input-stream
                        input-fd
                        :peer-fd peer-fd
                        :elements-per-buffer (#_fpathconf
                                              input-fd
                                              #$_PC_MAX_INPUT)))
         (proc
          (make-mcl-listener-process 
           procname
           input-stream
           (make-fd-stream output-fd :direction :output
                           :elements-per-buffer
                           (#_fpathconf
                            output-fd
                            #$_PC_MAX_INPUT))
           #'(lambda ()`
               (let* ((buf (find *current-process* hi:*buffer-list*
                                 :key #'hi::buffer-process))
                      (doc (if buf (hi::buffer-document buf))))
                 (when doc
                   (setf (hi::buffer-process buf) nil)
                   (send doc
                         :perform-selector-on-main-thread (@selector "close")
                         :with-object (%null-ptr)
                         :wait-until-done nil))))
           :initial-function
           #'(lambda ()
               (setq *listener-autorelease-pool* (create-autorelease-pool))
               (listener-function))
           :class 'cocoa-listener-process)))
    (setf (slot-value proc 'input-stream) input-stream)
    proc))
         

(defloadvar *NSFileHandleNotificationDataItem*
    (%get-ptr (foreign-symbol-address "_NSFileHandleNotificationDataItem")))

(defloadvar *NSFileHandleReadCompletionNotification*
    (%get-ptr (foreign-symbol-address "_NSFileHandleReadCompletionNotification")))



(defclass hemlock-listener-window-controller (hemlock-editor-window-controller)
    ((filehandle :foreign-type :id)	;Filehandle for I/O
     (clientfd :foreign-type :int)	;Client (listener)'s side of pty
     )
  (:metaclass ns:+ns-object)
  )

(define-objc-method ((:id :init-with-window w)
		     hemlock-listener-window-controller)
  (let* ((self (send-super :init-with-window w)))
    (unless (%null-ptr-p self)
      (multiple-value-bind (server client) (ignore-errors (open-pty-pair))
	(when server
	  (let* ((fh (make-objc-instance
		      'ns-file-handle
		      :with-file-descriptor (setup-server-pty server)
		      :close-on-dealloc t)))
	    (setf (slot-value self 'filehandle) fh)
	    (setf (slot-value self 'clientfd) (setup-client-pty client))
	    (send (send (@class ns-notification-center) 'default-center)
		  :add-observer self
		  :selector (@selector "gotData:")
		  :name *NSFileHandleReadCompletionNotification*
		  :object fh)
	    (send fh 'read-in-background-and-notify)))))
    self))

(define-objc-method ((:void :got-data notification)
		     hemlock-listener-window-controller)
  (with-slots (filehandle) self
    (let* ((data (send (send notification 'user-info)
		       :object-for-key *NSFileHandleNotificationDataItem*))
	   (document (send self 'document))
           (textstorage (slot-value document 'textstorage))
	   (data-length (send data 'length))
	   (buffer (hemlock-document-buffer document))
	   (string (make-string data-length))
	   (fh filehandle))
      (%copy-ptr-to-ivector (send data 'bytes) 0 string 0 data-length)
      (enqueue-buffer-operation
       buffer
       #'(lambda ()
           (let* ((input-mark (hi::variable-value 'hemlock::buffer-input-mark :buffer buffer)))
             (hi:with-mark ((mark input-mark :left-inserting))
               (hi::insert-string mark string)
               (hi::move-mark input-mark mark)))
           (send textstorage
                 :perform-selector-on-main-thread
                 (@selector "ensureSelectionVisible")
                 :with-object (%null-ptr)
                 :wait-until-done t)))
      (send fh 'read-in-background-and-notify))))
	     
#|    
;;; The Hemlock-Listener-Window-Controller is the textview's "delegate": it
;;; gets consulted before certain actions are performed, and can
;;; perform actions on behalf of the textview.

(define-objc-method ((:<BOOL> :text-view tv
			      :should-change-text-in-range (:<NSR>ange range)
			      :replacement-string replacement-string)
		     hemlock-listener-window-controller)
  (declare (ignorable replacement-string))
  (if (< (pref range :<NSR>ange.location) (slot-value self 'outpos))
    (progn
      (#_NSBeep)			;Overkill, maybe.
      nil)
    (progn
      (send tv :set-typing-attributes (slot-value self 'userta))
      t)))
|#


(define-objc-method ((:void dealloc) hemlock-listener-window-controller)
  (send (send (@class ns-notification-center) 'default-center)
	:remove-observer self)
  (send-super 'dealloc))



;;; The HemlockListenerDocument class.


(defclass hemlock-listener-document (hemlock-editor-document)
    ()
  (:metaclass ns:+ns-object))

(defun hemlock::listener-document-send-string (document string)
  (let* ((controller (send (send document 'window-controllers)
                          :object-at-index 0))
         (filehandle (slot-value controller 'filehandle))
         (len (length string))
         (data (send (make-objc-instance 'ns-mutable-data
                                         :with-length len) 'autorelease))
         (bytes (send data 'mutable-bytes)))
    (%copy-ivector-to-ptr string 0 bytes 0 len)
    (send filehandle :write-data data)
    (send filehandle 'synchronize-file)))


(define-objc-class-method ((:id top-listener) hemlock-listener-document)
  (let* ((all-documents (send *NSApp* 'ordered-Documents)))
    (dotimes (i (send all-documents 'count) (%null-ptr))
      (let* ((doc (send all-documents :object-at-index i)))
	(when (eql (send doc 'class) self)
	  (return doc))))))

(defun symbol-value-in-top-listener-process (symbol)
  (let* ((listenerdoc (send (@class hemlock-listener-document) 'top-listener))
	 (buffer (unless (%null-ptr-p listenerdoc)
		   (hemlock-document-buffer listenerdoc)))
	 (process (if buffer (hi::buffer-process buffer))))
     (if process
       (ignore-errors (symbol-value-in-process symbol process))
       (values nil t))))
  


(define-objc-method ((:<BOOL> is-document-edited) hemlock-listener-document)
  nil)


(define-objc-method ((:id init)
		     hemlock-listener-document)
  (let* ((doc (send-super 'init)))
    (unless (%null-ptr-p doc)
      (let* ((listener-name (if (eql 1 (incf *cocoa-listener-count*))
			    "Listener"
			    (format nil
				    "Listener-~d" *cocoa-listener-count*)))
	     (buffer (hemlock-document-buffer doc)))
	(send doc :set-file-name  (%make-nsstring listener-name))
	(setf (hi::buffer-pathname buffer) nil
	      (hi::buffer-minor-mode buffer "Listener") t
	      (hi::buffer-name buffer) listener-name)
        (hi::sub-set-buffer-modeline-fields buffer hemlock::*listener-modeline-fields*)))
    doc))

(define-objc-method ((:void make-window-controllers) hemlock-listener-document)
  (let* ((textstorage (slot-value self 'textstorage))
	 (controller (make-objc-instance
		      'hemlock-listener-window-controller
		      :with-window (%hemlock-frame-for-textstorage
                                    textstorage
				    *listener-columns*
				    *listener-rows*
				    t)))
	 (listener-name (hi::buffer-name (hemlock-document-buffer self))))
    (send self :add-window-controller controller)
    (send controller 'release)
    (setf (hi::buffer-process (hemlock-document-buffer self))
	  (let* ((tty (slot-value controller 'clientfd))
		 (peer-tty (send (slot-value controller 'filehandle)
				 'file-descriptor)))
	    (new-cocoa-listener-process listener-name tty tty peer-tty)))
    controller))

;;; This is almost completely wrong: we need to ensure that the form
;;; is read in the correct package, etc.
#|
(defun send-to-top-listener (sender-info nsstring &optional (append-newline t))
  (declare (ignorable sender-info))
  (let* ((listener
	  (info-from-document (send (@class hemlock-listener-document)
				    'top-listener))))
    (when listener
      (let* ((controller (cocoa-editor-info-controller listener)))
	(send controller :send-string nsstring)
	(when append-newline
	  (send controller :send-string #@"
"
	  ))))))
|#

(defun shortest-package-name (package)
  (let* ((name (package-name package))
         (len (length name)))
    (dolist (nick (package-nicknames package) name)
      (let* ((nicklen (length nick)))
        (if (< nicklen len)
          (setq name nick len nicklen))))))

(defmethod ui-object-note-package ((app ns:ns-application) package)
  (with-autorelease-pool
      (process-interrupt *cocoa-event-process*
			 #'(lambda (proc name)
			     (dolist (buf hi::*buffer-list*)
			       (when (eq proc (hi::buffer-process buf))
				 (setf (hi::variable-value 'hemlock::current-package :buffer buf) name))))
			 *current-process*
			 (shortest-package-name package))))

(defmethod hi::send-string-to-listener-process ((process cocoa-listener-process)
                                                string &key path package)
  (let* ((selection (make-input-selection :package package
                                          :source-file path
                                          :string-stream
                                          (make-string-input-stream string))))
    (enqueue-input-selection (cocoa-listener-process-input-stream process) selection)))


(defun hemlock::evaluate-input-selection (selection)
  (application-ui-operation *application* :eval-selection selection))
			    
(defmethod ui-object-choose-listener-for-selection ((app ns:ns-application)
						    selection)
  (declare (ignore selection))
  (let* ((top-listener-document (send (find-class 'hemlock-listener-document)
				      'top-listener)))
    (if top-listener-document
      (let* ((buffer (hemlock-document-buffer top-listener-document)))
	(if buffer
	  (let* ((proc (hi::buffer-process buffer)))
	    (if (typep proc 'cocoa-listener-process)
	      proc)))))))

(defmethod ui-object-eval-selection ((app ns:ns-application)
				     selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection
			   app selection)))
    (if (typep target-listener 'cocoa-listener-process)
      (enqueue-input-selection (cocoa-listener-process-input-stream
				target-listener)
			       selection))))
  

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   operation &rest args)
  (case operation
    (:note-current-package (ui-object-note-package o (car args)))
    (:eval-selection (ui-object-eval-selection o (car args)))))


       
  