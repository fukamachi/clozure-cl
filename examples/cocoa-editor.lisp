;;;-*- Mode: LISP; Package: CCL -*-


(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA-WINDOW")
  (require "HEMLOCK-TEXTSTORAGE"))

(declaim
  (special *open-editor-documents* *open-editor-documents-lock*)
  (type list *open-editor-documents*)
  (type lock *open-editor-documents-lock*))

    
(defstruct cocoa-editor-info
  (document nil)			; the NSDocument
  (controller nil)			; the NSWindowController (maybe).
  (listener nil)			; True (a lisp process) if a listener
  (modeline-plist nil)			; info from attribute line
)

	   
    

(defun size-of-char-in-font (f)
  (let* ((sf (send f 'screen-font)))
    (if (%null-ptr-p sf) (setq sf f))
    (values (send sf 'default-line-height-for-font)
	    (send sf :width-of-string #@" "))))
         
    
(defun get-size-for-textview (font nrows ncols)
  (multiple-value-bind (h w) (size-of-char-in-font font)
    (values (fceiling (* nrows h))
	    (fceiling (* ncols w)))))


(defun size-textview-containers (tv char-height char-width nrows ncols)
  (let* ((height (fceiling (* nrows char-height)))
	 (width (fceiling (* ncols char-width)))
	 (scrollview (send (send tv 'superview) 'superview))
	 (window (send scrollview 'window)))
    (rlet ((tv-size :<NSS>ize :height height
		    :width (+ width (* 2 (send (send tv 'text-container)
		      'line-fragment-padding)))))
      (when (send scrollview 'has-vertical-scroller)
	(send scrollview :set-vertical-line-scroll char-height)
	(send scrollview :set-vertical-page-scroll char-height))
      (slet ((sv-size
	      (send (@class ns-scroll-view)
		    :frame-size-for-content-size tv-size
		    :has-horizontal-scroller
		    (send scrollview 'has-horizontal-scroller)
		    :has-vertical-scroller
		    (send scrollview 'has-vertical-scroller)
		    :border-type (send scrollview 'border-type))))
	(slet ((sv-frame (send scrollview 'frame)))
	  (incf (pref sv-size :<NSS>ize.height)
		(pref sv-frame :<NSR>ect.origin.y))
	  (send window :set-content-size sv-size)
	  (send window :set-resize-increments
		(ns-make-size char-width char-height)))))))
      
(defun info-from-document (doc)
  (with-lock-grabbed (*open-editor-documents-lock*)
    (find doc *open-editor-documents* :key #'cocoa-editor-info-document)))

(defun info-from-controller (controller)
  (with-lock-grabbed (*open-editor-documents-lock*)
    (find controller *open-editor-documents* :key #'cocoa-editor-info-controller)))



				    
  
(defclass lisp-editor-window-controller (ns:ns-window-controller)
    ()
  (:metaclass ns:+ns-object))

    
;;; The LispEditorWindowController is the textview's "delegate": it
;;; gets consulted before certain actions are performed, and can
;;; perform actions on behalf of the textview.



;;; The LispEditorDocument class.


(defclass lisp-editor-document (ns:ns-document)
    ((textstorage :foreign-type :id))
  (:metaclass ns:+ns-object))

(define-objc-method ((:id init) lisp-editor-document)
  (let* ((doc (send-super 'init)))
    (unless (%null-ptr-p doc)
      (let* ((buffer (hi::make-buffer
		      (lisp-string-from-nsstring (send doc 'display-name))
		      :modes '("Lisp"))))
	(setf (slot-value doc 'textstorage)
	      (make-textstorage-for-hemlock-buffer
	       buffer)
	      (hi::buffer-document buffer) doc)))
    doc))
                     

(define-objc-method ((:id :read-from-file filename
			  :of-type type)
		     lisp-editor-document)
  (declare (ignorable type))
  (let* ((pathname (lisp-string-from-nsstring filename))
	 (buffer-name (hi::pathname-to-buffer-name pathname))
	 (buffer (hi::make-buffer buffer-name))
	 (data (make-objc-instance 'ns:ns-data
				   :with-contents-of-file filename))
	 (string (make-objc-instance 'ns:ns-string
				     :with-data data
				     :encoding #$NSMacOSRomanStringEncoding)))
    (setf (hi::buffer-pathname buffer) pathname)
    (nsstring-to-buffer string buffer)
    (hi::buffer-start (hi::buffer-point buffer))
    (setf (hi::buffer-modified buffer) nil)
    (hi::process-file-options buffer pathname)
    (setf (slot-value self 'textstorage)
	  (make-textstorage-for-hemlock-buffer buffer)
	  (hi::buffer-document buffer) (%setf-macptr (%null-ptr) self))))
    
  

(define-objc-method ((:id :data-representation-of-type type)
		      lisp-editor-document)
  (declare (ignorable type))
  (send (send (slot-value self 'text-view) 'string)
	:data-using-encoding #$NSASCIIStringEncoding
	:allow-lossy-conversion t))

(define-objc-method ((:void make-window-controllers) lisp-editor-document)
  (let* ((controller (make-objc-instance
		      'lisp-editor-window-controller
		      :with-window (%hemlock-frame-for-textstorage
                                    (slot-value self 'textstorage) nil nil))))
    (send self :add-window-controller controller)
    (send controller 'release)))	 

#|
(define-objc-method ((:void :window-controller-did-load-nib acontroller)
		     lisp-editor-document)
  (send-super :window-controller-did-load-nib  acontroller)
  ;; Apple/NeXT thinks that adding extra whitespace around cut & pasted
  ;; text is "smart".  Really, really smart insertion and deletion
  ;; would alphabetize the selection for you (byChars: or byWords:);
  ;; sadly, if you want that behavior you'll have to do it yourself.
  ;; Likewise with the extra spaces.
  (with-slots (text-view echoarea packagename filedata) self
    (send text-view :set-alignment  #$NSNaturalTextAlignment)
    (send text-view :set-smart-insert-delete-enabled nil)
    (send text-view :set-rich-text nil)
    (send text-view :set-uses-font-panel t)
    (send text-view :set-uses-ruler nil)
    (with-lock-grabbed (*open-editor-documents-lock*)
      (push (make-cocoa-editor-info
	     :document (%setf-macptr (%null-ptr) self)
	     :controller (%setf-macptr (%null-ptr) acontroller)
	     :listener nil)
	    *open-editor-documents*))
    (setf (slot-value acontroller 'textview) text-view
	  (slot-value acontroller 'echoarea) echoarea
	  (slot-value acontroller 'packagename) packagename)
    (send text-view :set-delegate acontroller)
    (let* ((font (default-font)))
      (multiple-value-bind (height width)
	  (size-of-char-in-font font)
	(size-textview-containers text-view height width 24 80))
      (send text-view
	    :set-typing-attributes
	    (create-text-attributes
	     :font font
	     :color (send (@class ns-color) 'black-color)))
      (unless (%null-ptr-p filedata)
	(send text-view
	      :replace-characters-in-range (ns-make-range 0 0)
	      :with-string (make-objc-instance
			    'ns-string
			    :with-data filedata
			    :encoding #$NSASCIIStringEncoding))
))))
|#

(define-objc-method ((:void close) lisp-editor-document)
  (send-super 'close)
  (let* ((textstorage (slot-value self 'textstorage)))
    (setf (slot-value self 'textstorage) (%null-ptr))
    (unless (%null-ptr-p textstorage)
      (close-hemlock-textstorage textstorage)))
  (let* ((info (info-from-document self)))
    (when info
      (let* ((proc (cocoa-editor-info-listener info)))
        (when proc
	      (setf (cocoa-editor-info-listener info) nil)
	      (process-kill proc)))
      (with-lock-grabbed (*open-editor-documents-lock*)
	(setq *open-editor-documents*
	      (delete info *open-editor-documents*))))))


(provide "COCOA-EDITOR")
