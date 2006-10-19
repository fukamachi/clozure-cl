;;-*- Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA-EDITOR")
  (require "PTY"))

(def-cocoa-default *listener-rows* :int 16 "Initial height of listener windows, in characters")
(def-cocoa-default *listener-columns* :int 80 "Initial height of listener windows, in characters")

(def-cocoa-default hi::*listener-output-style* :int 0 "Text style index for listener output")

(def-cocoa-default hi::*listener-input-style* :int 1 "Text style index for listener output")

(def-cocoa-default *listener-background-red-component* :float 0.90f0 "Red component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")
(def-cocoa-default *listener-background-green-component* :float 0.90f0 "Green component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")
(def-cocoa-default *listener-background-blue-component* :float 0.90f0 "Blue component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")
(def-cocoa-default *listener-background-alpha-component* :float 1.0f0 "Red component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")

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
    ((input-stream :reader cocoa-listener-process-input-stream)
     (backtrace-contexts :initform nil
                         :accessor cocoa-listener-process-backtrace-contexts)))
  

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
                           :sharing :lock
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
		  :name #&NSFileHandleReadCompletionNotification
		  :object fh)
	    (send fh 'read-in-background-and-notify)))))
    self))

(define-objc-method ((:void :got-data notification)
		     hemlock-listener-window-controller)
  (with-slots (filehandle) self
    (let* ((data (send (send notification 'user-info)
		       :object-for-key #&NSFileHandleNotificationDataItem))
	   (document (send self 'document))
	   (data-length (send (the ns:ns-data data) 'length))
	   (buffer (hemlock-document-buffer document))
	   (string (%str-from-ptr (send data 'bytes) data-length))
	   (fh filehandle))
      (enqueue-buffer-operation
       buffer
       #'(lambda ()
           (hemlock::append-buffer-output buffer string)))
      (send fh 'read-in-background-and-notify))))
	     


(define-objc-method ((:void dealloc) hemlock-listener-window-controller)
  (send (send (@class ns-notification-center) 'default-center)
	:remove-observer self)
  (send-super 'dealloc))



;;; The HemlockListenerDocument class.


(defclass hemlock-listener-document (hemlock-editor-document)
    ()
  (:metaclass ns:+ns-object))

(defmethod textview-background-color ((doc hemlock-listener-document))
  (send (find-class 'ns:ns-color)
        :color-with-calibrated-red *listener-background-red-component*
        :green *listener-background-green-component*
        :blue *listener-background-blue-component*
        :alpha *listener-background-alpha-component*))


(defun hemlock::listener-document-send-string (document string)
  (let* ((controller (send (send document 'window-controllers)
                          :object-at-index 0))
         (filehandle (slot-value controller 'filehandle))
         (len (length string))
         (data (send (make-objc-instance 'ns-mutable-data
                                         :with-length len) 'autorelease))
         (bytes (send data 'mutable-bytes)))
    (declare (type ns:ns-file-handle filehandle))
    (%cstr-pointer string bytes nil)
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
	(setf (slot-value (slot-value self 'textstorage) 'append-edits) 1)
	(send doc :set-file-name  (%make-nsstring listener-name))
	(setf (hi::buffer-pathname buffer) nil
	      (hi::buffer-minor-mode buffer "Listener") t
	      (hi::buffer-name buffer) listener-name)
        (hi::sub-set-buffer-modeline-fields buffer hemlock::*listener-modeline-fields*)))
    doc))

(def-cocoa-default *initial-listener-x-pos* :float 400.0f0 "X position of upper-left corner of initial listener")

(def-cocoa-default *initial-listener-y-pos* :float 400.0f0 "Y position of upper-left corner of initial listener")

(defloadvar *next-listener-x-pos* nil) ; set after defaults initialized
(defloadvar *next-listener-y-pos* nil) ; likewise

(define-objc-method ((:void make-window-controllers) hemlock-listener-document)
  (let* ((textstorage (slot-value self 'textstorage))
         (window (%hemlock-frame-for-textstorage
                                    textstorage
				    *listener-columns*
				    *listener-rows*
				    t
                                    (textview-background-color self)))
	 (controller (make-objc-instance
		      'hemlock-listener-window-controller
		      :with-window window))
	 (listener-name (hi::buffer-name (hemlock-document-buffer self))))
    (send self :add-window-controller controller)
    (send controller 'release)
    (slet ((current-point (ns-make-point (or *next-listener-x-pos*
                                             *initial-listener-x-pos*)
                                         (or *next-listener-y-pos*
                                             *initial-listener-y-pos*))))
      (slet ((new-point (send window
                              :cascade-top-left-from-point current-point)))
        (setf *next-listener-x-pos* (pref new-point :<NSP>oint.x)
              *next-listener-y-pos* (pref new-point :<NSP>oint.y))))
    (setf (hi::buffer-process (hemlock-document-buffer self))
	  (let* ((tty (slot-value controller 'clientfd))
		 (peer-tty (send (slot-value controller 'filehandle)
				 'file-descriptor)))
	    (new-cocoa-listener-process listener-name tty tty peer-tty)))
    controller))

;;; Action methods
(define-objc-method ((:void :interrupt sender) hemlock-listener-document)
  (declare (ignore sender))
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (ccl::force-break-in-listener process))))

(defmethod listener-backtrace-context ((proc cocoa-listener-process))
  (car (cocoa-listener-process-backtrace-contexts proc)))

(define-objc-method ((:void :backtrace sender) hemlock-listener-document)
  (declare (ignore sender))
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (let* ((context (listener-backtrace-context process)))
        (when context
          (send (backtrace-controller-for-context context)
                :show-window (%null-ptr)))))))

;;; Menu item action validation.  It'd be nice if we could distribute this a
;;; bit better, so that this method didn't have to change whenever a new
;;; action was implemented in this class.  For now, we have to do so.

(defmethod document-validate-menu-item ((doc hemlock-listener-document) item)
  ;; Return two values: the first is true if the second is definitive.
  ;; So far, all actions demand that there be an underlying process, so
  ;; check for that first.
  (let* ((buffer (hemlock-document-buffer doc))
         (process (if buffer (hi::buffer-process buffer))))
    (if (typep process 'cocoa-listener-process)
      (let* ((action (send item 'action)))
        (cond
          ((eql action (@selector "interrupt:")) (values t t))
          ((eql action (@selector "backtrace:"))
           (values t
                   (not (null (listener-backtrace-context process)))))))
      (values nil nil))))

(define-objc-method ((:<BOOL> :validate-menu-item item)
                     hemlock-listener-document)
  (multiple-value-bind (have-opinion opinion)
      (document-validate-menu-item self item)
    (if have-opinion
      opinion
      (send-super :validate-menu-item item))))

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
  




       
  
