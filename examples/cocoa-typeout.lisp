(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))

;;
;; the typeout panel is just an ns-panel containing a scroll-view
;; which contains a text-view. The text is read only.
;;
;; There is only one panel which is created with the first invocation
;; of the 'shared-panel class method. The panel is bound to the 
;; variable ccl::*typeout-panel*
;;
;; the panel is implicitly bound to a stream, and text written to
;; the stream is written into the text-view object. The stream is 
;; available via the function (ccl::typeout-stream)
;;
;; the panel width is set to 600 pixels, which is fine since hemlock
;; looks like it wants to wrap the documentation at 80 characters
;; anyway. In the long run this window should use a variable size font
;; and maybe compute the width as 80 times the width of the letter W.
;;
;; I'll revisit this after the preferences are more defined.
;;
;; @class typeout-view
;;
(defclass typeout-view (ns:ns-view)
  ((scroll-view :foreign-type :id :reader typeout-view-scroll-view)
   (text-view :foreign-type :id :reader typeout-view-text-view)
   (text-storage :foreign-type :id :reader typeout-view-text-storage))
  (:metaclass ns:+ns-object))

(define-objc-method ((:id :init-with-frame (:<NSR>ect frame))
                     typeout-view)
  (send-super :init-with-frame frame)
  (let* ((scrollview (make-objc-instance 'ns:ns-scroll-view
					 :with-frame frame))
	 (scroll-content (send scrollview 'content-view))) 
    (send scrollview :set-border-type #$NSBezelBorder)
    (send scrollview :set-has-vertical-scroller t)
    (send scrollview :set-has-horizontal-scroller nil)
    (send scrollview :set-rulers-visible nil)
    (send scrollview :set-autoresizing-mask #$NSViewHeightSizable)
    (send scroll-content :set-autoresizes-subviews t)
    (send self :add-subview scrollview)
    (setf (slot-value self 'scroll-view) scrollview)
    (slet* ((contentsize (send scrollview 'content-size))
	    (text-frame (ns-make-rect
			 +cgfloat-zero+
			 +cgfloat-zero+
			 (pref contentsize :<NSS>ize.width)
			 (pref contentsize :<NSS>ize.height))))
	   (let* ((text-view (make-objc-instance 'ns:ns-text-view
					    :with-frame text-frame))
		  (text-storage (send text-view 'text-storage)))
	     (send text-view :set-editable 0)
	     (setf (slot-value self 'text-storage) text-storage)
	     (send scrollview :set-document-view text-view)
	     (setf (slot-value self 'text-view) text-view))))
  self)

;;
;; @class typeout-panel
;;
(defloadvar *typeout-panel* nil)

(defclass typeout-panel (ns:ns-panel)
    ((typeout-view :foreign-type :id :accessor typeout-panel-typeout-view))
  (:metaclass ns:+ns-object))

(define-objc-class-method ((:id shared-panel) 
			   typeout-panel)
  (cond (*typeout-panel*)
        (t
         (let* ((panel (new-cocoa-window :class self
                                         :title "Typeout"
					 :width 600
                                         :activate nil)))
	   (rlet ((size :<NSS>ize
                    :width (float 600.0f0 +cgfloat-zero+)
                    :height (float 10000.0f0 +cgfloat-zero+)))
		 (send panel :set-max-size size)
		 (setf (pref size :<NSS>ize.height) (float 1.0f0 +cgfloat-zero+))
		 (send panel :set-min-size size))
           (slet ((bounds (send (send panel 'content-view) 'bounds)))
		 (let* ((view (make-instance 'typeout-view :with-frame bounds)))
		   (send panel :set-content-view view)
		   (send view :set-needs-display t)
		   (setf (slot-value panel 'typeout-view) view)
		   (setq *typeout-panel* panel)))))))

(define-objc-method ((:id init)
		     typeout-panel)
  (let* ((class (class-of self)))
    (send self 'dealloc)
    (send class 'shared-panel)))

(define-objc-method ((:void show)
		     typeout-panel)
  (send self :order-front (%null-ptr)))

(defloadvar *typeout-attributes* nil)

(defclass typeout-stream (fundamental-stream)
  ((text-storage :initform nil :accessor typeout-stream-text-storage)
   (line-number :initform 0 :accessor typeout-stream-line-number)
   (line-position :initform 0 :accessor typeout-stream-line-position)))

(defun prepare-typeout-stream (stream)
  (let ((panel (send (@class typeout-panel) 'shared-panel)))
    (unless (typeout-stream-text-storage stream)
      (setf (typeout-stream-text-storage stream) (typeout-view-text-storage (typeout-panel-typeout-view panel))))
    (unless *typeout-attributes*
      (setf *typeout-attributes* (create-text-attributes 
				  :font (default-font :name *default-font-name* :size *default-font-size*)
				  :line-break-mode :word)))
    (send panel 'show)))


;;;
;;;  TYPEOUT-STREAM methods
;;;

(defmethod stream-write-char ((stream typeout-stream) char)
  (prepare-typeout-stream stream)
  ;;
  ;;  convert tabs to spaces.
  ;;
  (if (eq char #\tab)
      (return-from stream-write-char
	(progn
	  (format stream "(make-string (- 8 (mod ~A 8)) :initial-element #\space)~%" (typeout-stream-line-position stream))
          (stream-write-string stream (make-string (- 8 (mod (typeout-stream-line-position stream) 8))
						   :initial-element #\space)))))

  ;;
  ;;  Maybe convert non-printable characters to something else?
  ;;  This is a problem for the editor, but probably not here.

  ;;
  ;;  adjust the line and column #s accordingly
  ;;
  (if (eq char #\newline)
      (progn
	(incf (typeout-stream-line-number stream))
	(setf (typeout-stream-line-position stream) 0))
    (incf (typeout-stream-line-position stream)))

  ;;
  ;;  print the character by converting it to a string and appending
  ;;  it to the text-storage buffer.
  ;;
  (let* ((typeout-view (typeout-panel-typeout-view *typeout-panel*))
	 (text-storage (slot-value typeout-view 'text-storage))
	 (str (make-string 1 :initial-element char))
	 (attr-str (make-instance 'ns:ns-attributed-string 
				  :with-string str
				  :attributes *typeout-attributes*)))
    (send text-storage :append-attributed-string attr-str)))

(defmethod stream-write-string ((stream typeout-stream) string &optional (start 0) end)
  (prepare-typeout-stream stream)
  (let* ((str (if start 
		  (subseq string start end)
		string))
	 (attr-str (make-instance 'ns:ns-attributed-string 
				  :with-string str
				  :attributes *typeout-attributes*))
	 (typeout-view (typeout-panel-typeout-view *typeout-panel*))
	 (text-storage (slot-value typeout-view 'text-storage)))
    (setf (typeout-stream-line-position stream) (length string))
    (send text-storage :append-attributed-string attr-str)))

(defmethod stream-fresh-line ((stream typeout-stream))
  (prepare-typeout-stream stream)
  (stream-write-char stream #\newline))

(defmethod stream-line-column ((stream typeout-stream))
  (typeout-stream-line-position stream))

(defmethod stream-clear-output ((stream typeout-stream))
  (prepare-typeout-stream stream)
  (let* ((typeout-view (typeout-panel-typeout-view *typeout-panel*))
	 (text-storage (slot-value typeout-view 'text-storage))
	 (len (send text-storage 'length)))
    (declare (type ns:ns-text-storage text-storage))
    (send text-storage :delete-characters-in-range (ns-make-range 0 len))))

(defloadvar *typeout-stream* (make-instance 'typeout-stream))

(defun typeout-stream ()
  *typeout-stream*)

