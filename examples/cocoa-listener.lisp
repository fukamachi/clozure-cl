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
  ;; Has this been true for the last few years (native threads) ?
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
                   (#/performSelectorOnMainThread:withObject:waitUntilDone:
                    doc
                    (@selector #/close)
                    +null-ptr+
                    nil))))
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


;;; Listener documents are never (or always) ediited.  Don't cause their
;;; close boxes to be highlighted.
(objc:defmethod (#/setDocumentEdited: :void)
    ((self hemlock-listener-window-controller) (edited :<BOOL>))
  (declare (ignorable edited)))
 

(objc:defmethod #/initWithWindow: ((self hemlock-listener-window-controller) w)
  (let* ((new (call-next-method w)))
    (unless (%null-ptr-p new)
      (multiple-value-bind (server client) (ignore-errors (open-pty-pair))
	(when server
	  (let* ((fh (make-instance
		      'ns:ns-file-handle
		      :with-file-descriptor (setup-server-pty server)
		      :close-on-dealloc t)))
	    (setf (slot-value new 'filehandle) fh)
	    (setf (slot-value new 'clientfd) (setup-client-pty client))
            (#/addObserver:selector:name:object:
             (#/defaultCenter ns:ns-notification-center)
             new
             (@selector #/gotData:)
             #&NSFileHandleReadCompletionNotification
             fh)
            (#/readInBackgroundAndNotify fh)))))
    new))

(objc:defmethod (#/gotData: :void) ((self hemlock-listener-window-controller)
                                    notification)
  #+debug (#_NSLog #@"gotData: !")
  (with-slots (filehandle) self
    (let* ((data (#/objectForKey: (#/userInfo notification)
                                  #&NSFileHandleNotificationDataItem))
	   (document (#/document self))
	   (data-length (#/length data))
	   (buffer (hemlock-document-buffer document))
	   (string (%str-from-ptr (#/bytes data) data-length))
	   (fh filehandle))
      (enqueue-buffer-operation
       buffer
       #'(lambda ()
           (hemlock::append-buffer-output buffer string)))
      (#/readInBackgroundAndNotify fh))))
	     


(objc:defmethod (#/dealloc :void) ((self hemlock-listener-window-controller))
  (#/removeObserver: (#/defaultCenter ns:ns-notification-center) self)
  (call-next-method))



;;; The HemlockListenerDocument class.


(defclass hemlock-listener-document (hemlock-editor-document)
    ()
  (:metaclass ns:+ns-object))

(defmethod textview-background-color ((doc hemlock-listener-document))
  (#/colorWithCalibratedRed:green:blue:alpha:
   ns:ns-color
   (float *listener-background-red-component* +cgfloat-zero+)
   (float *listener-background-green-component* +cgfloat-zero+)
   (float *listener-background-blue-component* +cgfloat-zero+)
   (float *listener-background-alpha-component* +cgfloat-zero+)))


(defun hemlock::listener-document-send-string (document string)
  (let* ((controller (#/objectAtIndex: (#/windowControllers document) 0))
         (filehandle (slot-value controller 'filehandle))
         (len (length string))
         (data (#/autorelease (make-instance 'ns:ns-mutable-data
                                             :with-length len)))
         (bytes (#/mutableBytes data)))
    (%cstr-pointer string bytes nil)
    (#/writeData: filehandle data)
    (#/synchronizeFile filehandle)))


(objc:defmethod #/topListener ((self +hemlock-listener-document))
  (let* ((all-documents (#/orderedDocuments *NSApp*)))
    (dotimes (i (#/count all-documents) (%null-ptr))
      (let* ((doc (#/objectAtIndex: all-documents i)))
	(when (eql (#/class doc) self)
	  (return doc))))))

(defun symbol-value-in-top-listener-process (symbol)
  (let* ((listenerdoc (#/topListener hemlock-listener-document))
	 (buffer (unless (%null-ptr-p listenerdoc)
		   (hemlock-document-buffer listenerdoc)))
	 (process (if buffer (hi::buffer-process buffer))))
     (if process
       (ignore-errors (symbol-value-in-process symbol process))
       (values nil t))))
  


(objc:defmethod (#/isDocumentEdited :<BOOL>) ((self hemlock-listener-document))
  nil)

(objc:defmethod #/init ((self hemlock-listener-document))
  (let* ((doc (call-next-method)))
    (unless (%null-ptr-p doc)
      (let* ((listener-name (if (eql 1 (incf *cocoa-listener-count*))
			    "Listener"
			    (format nil
				    "Listener-~d" *cocoa-listener-count*)))
	     (buffer (hemlock-document-buffer doc)))
	(setf (slot-value (slot-value self 'textstorage) 'append-edits) 1)
        (#/setFileName: doc  (%make-nsstring listener-name))
	(setf (hi::buffer-pathname buffer) nil
	      (hi::buffer-minor-mode buffer "Listener") t
	      (hi::buffer-name buffer) listener-name)
        (hi::sub-set-buffer-modeline-fields buffer hemlock::*listener-modeline-fields*)))
    doc))

(def-cocoa-default *initial-listener-x-pos* :float 400.0f0 "X position of upper-left corner of initial listener")

(def-cocoa-default *initial-listener-y-pos* :float 400.0f0 "Y position of upper-left corner of initial listener")

(defloadvar *next-listener-x-pos* nil) ; set after defaults initialized
(defloadvar *next-listener-y-pos* nil) ; likewise

(objc:defmethod (#/makeWindowControllers :void) ((self hemlock-listener-document))
  (let* ((textstorage (slot-value self 'textstorage))
         (window (%hemlock-frame-for-textstorage
                  textstorage
                  *listener-columns*
                  *listener-rows*
                  t
                  (textview-background-color self)))
	 (controller (make-instance
		      'hemlock-listener-window-controller
		      :with-window window))
	 (listener-name (hi::buffer-name (hemlock-document-buffer self))))
    ;; Disabling background layout on listeners is an attempt to work
    ;; around a bug.  The bug's probably gone ...
    (let* ((layout-managers (#/layoutManagers textstorage)))
      (dotimes (i (#/count layout-managers))
        (let* ((layout (#/objectAtIndex: layout-managers i)))
          (#/setBackgroundLayoutEnabled: layout nil))))
    (#/addWindowController: self controller)
    (#/release controller)
    (ns:with-ns-point (current-point
                       (or *next-listener-x-pos* *initial-listener-x-pos*)
                       (or *next-listener-y-pos* *initial-listener-y-pos*))
      (let* ((new-point (#/cascadeTopLeftFromPoint: window current-point)))
        (setf *next-listener-x-pos* (ns:ns-point-x new-point)
              *next-listener-y-pos* (ns:ns-point-y new-point))))
    (setf (hi::buffer-process (hemlock-document-buffer self))
	  (let* ((tty (slot-value controller 'clientfd))
		 (peer-tty (#/fileDescriptor (slot-value controller 'filehandle))))
	    (new-cocoa-listener-process listener-name tty tty peer-tty)))
    controller))

;;; Action methods
(objc:defmethod (#/interrupt: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (ccl::force-break-in-listener process))))

(defmethod listener-backtrace-context ((proc cocoa-listener-process))
  (car (cocoa-listener-process-backtrace-contexts proc)))

(objc:defmethod (#/backtrace: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (let* ((context (listener-backtrace-context process)))
        (when context
          (#/showWindow: (backtrace-controller-for-context context) +null-ptr+))))))

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
      (let* ((action (#/action item)))
        (cond
          ((eql action (@selector #/interrupt:)) (values t t))
          ((eql action (@selector #/backtrace:))
           (values t
                   (not (null (listener-backtrace-context process)))))))
      (values nil nil))))

(objc:defmethod (#/validateMenuItem: :<BOOL>)
    ((self hemlock-listener-document) item)
  (multiple-value-bind (have-opinion opinion)
      (document-validate-menu-item self item)
    (if have-opinion
      opinion
      (call-next-method item))))

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
  (let* ((top-listener-document (#/topListener hemlock-listener-document)))
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
  




       
  
