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

#-hemlock
(progn
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
       (let* ((info (with-lock-grabbed (*open-editor-documents-lock*)
		      (find *current-process* *open-editor-documents*
			    :key #'cocoa-editor-info-listener))))
	 (when info
	   (setf (cocoa-editor-info-listener info) nil)
	   (send (cocoa-editor-info-document info)
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
     (outpos :foreign-type :unsigned)	;Position in textview buffer
     (userta :foreign-type :id)		;Typing attributes for user input
     (systa :foreign-type :id)		;Typing attributes for system output
     (usercolor :foreign-type :id)	;Text foreground color for user input
     )
  (:metaclass ns:+ns-object)
  )

(define-objc-method ((:void window-did-load) lisp-listener-window-controller)
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

(define-objc-method ((:void :got-data notification)
		     lisp-listener-window-controller)
  (with-slots (filehandle systa outpos textview) self
    (let* ((data (send (send notification 'user-info)
		       :object-for-key *NSFileHandleNotificationDataItem*))
	   (tv textview)
	   (fh filehandle))
      (unless (%null-ptr-p tv)
	(let* ((buffer-text (send tv 'text-storage))
	       (s (make-objc-instance 'ns-string
				      :with-data data
				      :encoding #$NSASCIIStringEncoding))
	       (str (make-objc-instance 'ns-attributed-string
					:with-string s
					:attributes systa)))
	  (send buffer-text :append-attributed-string str)

	  (let* ((textlen (send buffer-text 'length)))
	    (send tv :scroll-range-to-visible (ns-make-range textlen 0))
	    (setq outpos textlen))
	  (send str 'release)))
      (send self 'update-package-name)
      (send fh 'read-in-background-and-notify))))
	     
(define-objc-method ((:void update-package-name)
		     lisp-listener-window-controller)
  (let* ((info (info-from-controller self))
	 (proc (if info (cocoa-editor-info-listener info)))
	 (package (if proc (ignore-errors (symbol-value-in-process
					   '*package*
					   proc))))
	 (name (if (typep package 'package)
		 (shortest-package-name package)
		 "")))
    (with-cstrs ((name name))
      (send self :display-package-name (send (@class ns-string)
					     :string-with-c-string name)))))
      

    
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


;;; Action methods implemented by the controller (in its role as the
;;; textview's delegate).


(define-objc-method ((:void :send-string string)
		     lisp-listener-window-controller)
  (send (slot-value self 'filehandle)
	:write-data (send string
			  :data-using-encoding #$NSASCIIStringEncoding
			  :allow-lossy-conversion t)))

(define-objc-method ((:void :insert-newline tv)
		     lisp-listener-window-controller)
  (with-slots (outpos usercolor) self
    (let* ((textbuf (send tv 'text-storage))
	   (textlen (send textbuf 'length))
	   (textstring (send tv 'string)))
      (slet ((r (send tv 'selected-range)))
	(let* ((curpos (pref r :<NSR>ange.location))
	       (curlen (pref r :<NSR>ange.length)))
	  (cond ((>= curpos outpos)
		 ;; Insert the newline at the end of any selection.
		 (incf curpos (pref r :<NSR>ange.length))
		 (send tv :set-selected-range (ns-make-range curpos 0))
		 (send tv :insert-newline self)
		 (incf curpos)
		 (incf textlen)
		 (when (= curpos textlen)
		   (let* ((sendlen (- textlen outpos))
			  (sendstring
			   (send textstring
				 :substring-with-range (ns-make-range outpos sendlen))))
		     (setf (pref r :<NSR>ange.location) 0
			   (pref r :<NSR>ange.length) sendlen)
		     (multiple-value-bind (ok second-value)
			 (balanced-expressions-in-range-forward r sendstring)
		       (if ok
			 (if second-value
			   (progn
			     (send self :send-string sendstring)
			     (setq outPos textlen)))
			 (if second-value
			   (#_NSBeep)))))))
		;; If there's a selection, copy it to the end of the
		;; buffer, then move to the end of the buffer.
		((> curlen 0)
		 (slet ((endrange (ns-make-range textlen 0)))
		   (send tv :set-selected-range endrange)
		   (send tv :insert-text
			 (send textstring :substring-with-range r))
		   (setf (pref endrange :<NSR>ange.location)
			 (send textbuf 'length))
		   (send tv :scroll-range-to-visible endrange)))
		;; No selection, insertion point is before outpos (in
		;; history or in output.  If in history, copy history
		;; item to end of buffer, otherwise, do nothing.
		(t
		 (rlet ((lr :<NSR>ange)
			(fullrange :<NSR>ange :location 0 :length textlen))
		   (let* ((attr
			   (send textbuf
				 :attribute #@"NSColor"
				 :at-index curpos
				 :longest-effective-range lr
				 :in-range fullrange)))
		     (when (send attr :is-equal  usercolor)
		       (let* ((history-start (pref lr :<NSR>ange.location))
			      (history-len (pref lr :<NSR>ange.length)))
			 (when (eql
				(send textstring
				      :character-at-index 
				      (+ history-start (1- history-len)))
				(char-code #\NewLine))
			   (decf (pref lr :<NSR>ange.length)))
			 (unless (eql 0 history-len)
			   (setf (pref fullrange :<NSR>ange.location)
				 textlen
				 (pref fullrange :<NSR>ange.length)
				 0)
			   (send tv :set-selected-range  fullrange)
			   (send tv :insert-text
				 (send textstring :substring-with-range lr))
			   (setf (pref fullrange :<NSR>ange.location)
				 (send textbuf 'length))
			   (send tv :scroll-range-to-visible fullrange)))))))))))))

;;; Force a break in the listener process.
(define-objc-method ((:id :interrupt tv) lisp-listener-window-controller)
  (declare (ignore tv))
  (let* ((info (info-from-controller self))
	 (proc (if info (cocoa-editor-info-listener info))))
    (when proc (force-break-in-listener proc))
    self))

;;; This exists solely for debugging.
(define-objc-method ((:id :log-attrs tv)  lisp-listener-window-controller)
  (slet ((selection (send tv 'selected-range)))
    (rlet ((lr :<NSR>ange))
      (let* ((textbuf (send tv 'text-storage))
	     (attr
	      (send textbuf
		    :attributes-at-index (pref selection :<NSR>ange.location)
		    :longest-effective-range lr
		    :in-range (ns-make-range 0 (send textbuf 'length)))))
	(#_NSLog #@"Attr = %@, range = [%d,%d]"
		 :address attr
		 :unsigned-fullword (pref lr :<NSR>ange.location)
		 :unsigned-fullword (pref lr :<NSR>ange.length)))
      self)))

;;; If we're at the end of the buffer and at the start of a line (either
;;; at outpos or after a newline), send an EOF (0 bytes of data) to the
;;; listener.  Otherwise, have the textview do a "deleteForward:"
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

(define-objc-method ((:id :add-modeline tv) lisp-listener-window-controller)
  (declare (ignore tv))
  self
  )

(define-objc-method ((:id :reparse-modeline tv)
		     lisp-listener-window-controller)
  (declare (ignore tv))
  self
  )

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
	 (info (info-from-document listenerdoc))
	 (process (if info (cocoa-editor-info-listener info))))
     (if process
       (ignore-errors (symbol-value-in-process symbol process))
       (values nil t))))
  


(define-objc-method ((:<BOOL> is-document-edited) lisp-listener-document)
  nil)

(define-objc-method ((:void make-window-controllers)  lisp-listener-document)
  (let* ((controller (make-objc-instance
		      'lisp-listener-window-controller
		      :with-window-nib-name (send self 'window-nib-name)
		      :owner self)))
    (send self :add-window-controller controller)
    (send controller 'release)))

      



(defloadvar *cocoa-listener-count* 17)

(define-objc-method ((:void :window-controller-did-load-nib acontroller)
		     lisp-listener-document)
  (send-super :window-controller-did-load-nib acontroller)
  ;; We'll use attribute-change information to distinguish user
  ;; input from system output.  Be fascist about letting the
  ;; user change anything.
  (with-slots ((textview text-view) packagename echoarea filedata) self
    (send textview :set-rich-text nil)
    (send textview :set-uses-font-panel nil)
    (let* ((listener-name (if (eql 1 (incf *cocoa-listener-count*))
			    "Listener"
			    (format nil
				    "Listener-~d" *cocoa-listener-count*)))
	   (info (info-from-document self)))
      (setf (cocoa-editor-info-listener info)
	    (let* ((tty (slot-value acontroller 'clientfd)))
	      (new-listener-process listener-name tty tty)))
      (send self :set-file-name  (%make-nsstring listener-name)))
    (setf (slot-value acontroller 'textview) textview
	  (slot-value acontroller 'echoarea) echoarea
	  (slot-value acontroller 'packagename) packagename)
    (let* ((userta (send (send textview 'typing-attributes) 'retain))
	   (systa (create-text-attributes :color (send (@class ns-color)
						       'blue-color))))
      (setf (slot-value acontroller 'userta)
	    userta
	    (slot-value acontroller 'usercolor)
	    (send userta :value-for-key #@"NSColor")
	    (slot-value acontroller 'systa)
	    systa))
    (send textview :set-delegate  acontroller)
    (unless (%null-ptr-p filedata)
      (send textview
	    :replace-characters-in-range (ns-make-range 0 0)
	    :with-rtfd filedata))))

;;; This is almost completely wrong: we need to ensure that the form
;;; is read in the correct package, etc.
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


); #-hemlock
