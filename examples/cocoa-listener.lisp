;;;-*- Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA-EDITOR")
  (require "PTY"))

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

(declaim (special *open-editor-documents*)
	 (type list *open-editor-documents*))


(defloadvar *cocoa-listener-count* 0)


(defun new-listener-process (procname input-fd output-fd)
  (make-mcl-listener-process
   procname
   (make-fd-stream
		   input-fd
		   :elements-per-buffer (#_fpathconf
					 input-fd
					 #$_PC_MAX_INPUT))
   (make-fd-stream output-fd :direction :output
				   :elements-per-buffer
				   (#_fpathconf
				    output-fd
				    #$_PC_MAX_INPUT))
   #'(lambda ()
       (let* ((buf (find *current-process* hi:*buffer-list*
			 :key #'hi::buffer-process))
	      (doc (if buf (hi::buffer-document buf))))
	 (when doc
	   (setf (hi::buffer-process buf) nil)
	   (send doc
		 :perform-selector-on-main-thread (@selector "close")
		 :with-object (%null-ptr)
		 :wait-until-done nil))))
   #'(lambda ()
       (setq *listener-autorelease-pool* (create-autorelease-pool))
       (listener-function))))

(defloadvar *NSFileHandleNotificationDataItem*
    (%get-ptr (foreign-symbol-address "_NSFileHandleNotificationDataItem")))

(defloadvar *NSFileHandleReadCompletionNotification*
    (%get-ptr (foreign-symbol-address "_NSFileHandleReadCompletionNotification")))



(defclass lisp-listener-window-controller (lisp-editor-window-controller)
    ((filehandle :foreign-type :id)	;Filehandle for I/O
     (clientfd :foreign-type :int)	;Client (listener)'s side of pty
     )
  (:metaclass ns:+ns-object)
  )

(define-objc-method ((:id :init-with-window w)
		     lisp-listener-window-controller)
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
		     lisp-listener-window-controller)
  (with-slots (filehandle) self
    (let* ((data (send (send notification 'user-info)
		       :object-for-key *NSFileHandleNotificationDataItem*))
	   (document (send self 'document))
	   (data-length (send data 'length))
	   (buffer (hemlock-document-buffer document))
	   (string (make-string data-length))
	   (fh filehandle))
      (declare (dynamic-extent string))
      (%copy-ptr-to-ivector (send data 'bytes) 0 string 0 data-length)
      (hi::insert-string (hi::buffer-point buffer) string)
      (send fh 'read-in-background-and-notify))))
	     
#|    
;;; The Lisp-Listener-Window-Controller is the textview's "delegate": it
;;; gets consulted before certain actions are performed, and can
;;; perform actions on behalf of the textview.

(define-objc-method ((:<BOOL> :text-view tv
			      :should-change-text-in-range (:<NSR>ange range)
			      :replacement-string replacement-string)
		     lisp-listener-window-controller)
  (declare (ignorable replacement-string))
  (if (< (pref range :<NSR>ange.location) (slot-value self 'outpos))
    (progn
      (#_NSBeep)			;Overkill, maybe.
      nil)
    (progn
      (send tv :set-typing-attributes (slot-value self 'userta))
      t)))
|#

;;; Action methods implemented by the controller (in its role as the
;;; textview's delegate).


(define-objc-method ((:void :send-string string)
		     lisp-listener-window-controller)
  (send (slot-value self 'filehandle)
	:write-data (send string
			  :data-using-encoding #$NSASCIIStringEncoding
			  :allow-lossy-conversion t)))




(define-objc-method ((:id :delete-forward tv)  lisp-listener-window-controller)
  (with-slots (outpos filehandle) self
    (slet ((selection (send tv 'selected-range)))
      (let* ((textbuf (send tv 'text-storage))
	     (length (send textbuf 'length)))
	(if (and (eql length (pref selection :<NSR>ange.location))
		 (or (eql outpos length)
		     (and (> length 1)
			  (= (send textbuf :character-at-index  (1- length))
			     (char-code #\NewLine)))))
	  (%stack-block ((buf 1))
	    (setf (%get-byte buf 0) (logand (char-code #\d) #x1f))
	    (send filehandle
		  :write-data (send (@class ns-data)
				    :data-with-bytes buf
				    :length 1))
	    (send filehandle 'synchronize-file)
					;(#_NSLog #@"wrote ctrl-d packet")
	    )
	  (send tv :delete-forward self))
	self))))





(define-objc-method ((:void dealloc) lisp-listener-window-controller)
  (send (send (@class ns-notification-center) 'default-center)
	:remove-observer self)
  (send-super 'dealloc))



;;; The LispListenerDocument class.


(defclass lisp-listener-document (lisp-editor-document)
    ()
  (:metaclass ns:+ns-object))


(define-objc-class-method ((:id top-listener) lisp-listener-document)
  (let* ((all-documents (send *NSApp* 'ordered-Documents)))
    (dotimes (i (send all-documents 'count) (%null-ptr))
      (let* ((doc (send all-documents :object-at-index i)))
	(when (eql (send doc 'class) self)
	  (return doc))))))

(defun symbol-value-in-top-listener-process (symbol)
  (let* ((listenerdoc (send (@class lisp-listener-document) 'top-listener))
	 (buffer (unless (%null-ptr-p listenerdoc)
		   (hemlock-document-buffer listenerdoc)))
	 (process (if buffer (hi::buffer-process buffer))))
     (if process
       (ignore-errors (symbol-value-in-process symbol process))
       (values nil t))))
  


(define-objc-method ((:<BOOL> is-document-edited) lisp-listener-document)
  nil)


(define-objc-method ((:id init)
		     lisp-listener-document)
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
	      (hi::buffer-name buffer) listener-name)))
    doc))

(define-objc-method ((:void make-window-controllers) lisp-listener-document)
  (let* ((controller (make-objc-instance
		      'lisp-listener-window-controller
		      :with-window (%hemlock-frame-for-textstorage
                                    (slot-value self 'textstorage) nil nil)))
	 (listener-name (hi::buffer-name (hemlock-document-buffer self))))
    (send self :add-window-controller controller)
    (send controller 'release)
    (setf (hi::buffer-process (hemlock-document-buffer self))
	  (let* ((tty (slot-value controller 'clientfd)))
	    (new-listener-process listener-name tty tty)))
    controller))

;;; This is almost completely wrong: we need to ensure that the form
;;; is read in the correct package, etc.
#|
(defun send-to-top-listener (sender-info nsstring &optional (append-newline t))
  (declare (ignorable sender-info))
  (let* ((listener
	  (info-from-document (send (@class lisp-listener-document)
				    'top-listener))))
    (when listener
      (let* ((controller (cocoa-editor-info-controller listener)))
	(send controller :send-string nsstring)
	(when append-newline
	  (send controller :send-string #@"
"
	  ))))))
|#



