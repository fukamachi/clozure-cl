(in-package "CCL")

(require "COCOA")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))


(defstruct hemlock-display
  buffer				; the hemlock buffer
  buflen				; length of buffer, if known
  workline				; cache for character-at-index
  workline-offset			; cached offset of workline
  workline-length			; length of cached workline
  )

(defun reset-display-cache (d &optional (buffer (hemlock-display-buffer d)
						buffer-p))
  (when buffer-p (setf (hemlock-display-buffer d) buffer))
  (let* ((workline (hemlock::mark-line
		    (hemlock::buffer-start-mark buffer))))
    (setf (hemlock-display-buflen d) (hemlock-buffer-length buffer)
	  (hemlock-display-workline-offset d) 0
	  (hemlock-display-workline d) workline
	  (hemlock-display-workline-length d) (hemlock::line-length workline))
    d))
	 

(defun hemlock-buffer-length (buffer)
  (hemlock::count-characters (hemlock::buffer-region buffer)))

(defclass hemlock-buffer-string (ns:ns-string)
    ((display :initform nil :initarg :display :accessor hemlock-buffer-string-display))
  (:metaclass ns:+ns-object))


(defun update-line-cache-for-index (d index)
  (let* ((line (or
		(hemlock-display-workline d)
		(progn
		  (reset-display-cache d)
		  (hemlock-display-workline d))))
	 (pos (hemlock-display-workline-offset d))
	 (len (hemlock-display-workline-length d))
	 (moved nil))
    (loop
      (when (and (>= index pos)
		   (< index (1+ (+ pos len))))
	  (let* ((idx (- index pos)))
	    (when moved
	      (setf (hemlock-display-workline d) line
		    (hemlock-display-workline-offset d) pos
		    (hemlock-display-workline-length d) len))
	    (return (values line idx))))
	(setq moved t)
      (if (< index pos)
	(setq line (hemlock::line-previous line)
	      len (hemlock::line-length line)
	      pos (1- (- pos len)))
	(setq line (hemlock::line-next line)
	      pos (1+ (+ pos len))
	      len (hemlock::line-length line))))))
  
(defun hemlock-char-at-index (d index)
  (multiple-value-bind (line idx) (update-line-cache-for-index d index)
    (let* ((len (hemlock::line-length line)))
      (if (< idx len)
	(hemlock::line-character line idx)
	#\newline))))

(defun move-hemlock-mark-to-absolute-position (mark d index)
  (multiple-value-bind (line idx) (update-line-cache-for-index d index)
    (hemlock::move-to-position mark idx line)))

(defun mark-absolute-position (mark)
  (let* ((pos (hemlock::mark-charpos mark)))
    (do* ((line (hemlock::line-previous (hemlock::mark-line mark))
		(hemlock::line-previous line)))
	 ((null line) pos)
      (incf pos (1+ (hemlock::line-length line))))))



(define-objc-method ((:unichar :character-at-index (unsigned index))
		     hemlock-buffer-string)
  ;(#_NSLog #@"Character at index %d" :unsigned index )
  (char-code (hemlock-char-at-index (hemlock-buffer-string-display self) index)))


(define-objc-method ((:unsigned length)
		     hemlock-buffer-string)
  (let* ((display-object (hemlock-buffer-string-display self)))
      (or (hemlock-display-buflen display-object)
	  (setf (hemlock-display-buflen display-object)
		(hemlock-buffer-length (hemlock-display-buffer display-object))))))



(define-objc-method ((:id description)
		     hemlock-buffer-string)
  (let* ((d (hemlock-buffer-string-display self))
	 (b (hemlock-display-buffer d)))
    (with-cstrs ((s (format nil "~a" b)))
      (send (@class ns-string) :string-with-format #@"<%s for %s>"
	(:address (#_object_getClassName self) :address s)))))

		     
(defclass lisp-text-storage (ns:ns-text-storage)
    ((string :foreign-type :id)
     (defaultattrs :foreign-type :id))
  (:metaclass ns:+ns-object))

(define-objc-method ((:id :init-with-string s) lisp-text-storage)
  (let* ((newself (send-super 'init)))
    (setf (slot-value newself 'string) s
	  (slot-value newself 'defaultattrs) (create-text-attributes))
    newself))
	  
(define-objc-method ((:id string) lisp-text-storage)
  (slot-value self 'string))

(define-objc-method ((:id :attributes-at-index (:unsigned index)
			  :effective-range ((* :<NSR>ange) rangeptr))
		     lisp-text-storage)
  '(#_NSLog #@"Attributes at index %d, rangeptr = %x"
	   :unsigned index :address rangeptr)
  (let* ((hemlock-display (hemlock-buffer-string-display (slot-value self 'string)))
	 (len (hemlock-display-buflen hemlock-display)))
    (if (>= index len)
      (error "This should be an NSRangeError"))
    (unless (%null-ptr-p rangeptr)
      (setf (pref rangeptr :<NSR>ange.location) 0
	    (pref rangeptr :<NSR>ange.length) len))
    (slot-value self 'defaultattrs)))

(define-objc-method ((:void :replace-characters-in-range (:<NSR>ange r)
			    :with-string string)
		     lisp-text-storage)
  (#_NSLog #@"replace-characters-in-range (%d %d) with-string %@"
	   :unsigned (pref r :<NSR>ange.location)
	   :unsigned (pref r :<NSR>ange.length)
	   :id string))

(define-objc-method ((:void :replace-characters-in-range  (:<NSR>ange r)
			    :with-attributed-string string)
		     lisp-text-storage)
  (#_NSLog #@"replace-characters-in-range (%d %d) with-attributed-string %@"
	   :unsigned (pref r :<NSR>ange.location)
	   :unsigned (pref r :<NSR>ange.length)
	   :id string))

(define-objc-method ((:void :set-attributes attributes
			    :range (:<NSR>ange r))
		     lisp-text-storage)
  (#_NSLog #@"set-attributes %@ range (%d %d)"
	   :id attributes
	   :unsigned (pref r :<NSR>ange.location)
	   :unsigned (pref r :<NSR>ange.length)))


(define-objc-method ((:id description)
		     lisp-text-storage)
  (send (@class ns-string) :string-with-format #@"%s : string %@"
	(:address (#_object_getClassName self) :id (slot-value self 'string))))


(defclass lisp-text-view (ns:ns-text-view)
    ((timer :foreign-type :id :accessor blink-timer)
     (blink-pos :foreign-type :int :accessor blink-pos)
     (blink-phase :foreign-type :<BOOL> :accessor blink-phase)
     (blink-char :foreign-type :int :accessor blink-char))
  (:metaclass ns:+ns-object))

(defmethod text-view-buffer ((self lisp-text-view))
  (hemlock-display-buffer (hemlock-buffer-string-display (send (send self 'text-storage) 'string))))

;;; HEMLOCK-EXT::DEFINE-CLX-MODIFIER is kind of misnamed; we can use
;;; it to map NSEvent modifier keys to key-event modifiers.
(hemlock-ext::define-clx-modifier #$NSShiftKeyMask "Shift")
(hemlock-ext::define-clx-modifier #$NSControlKeyMask "Control")
(hemlock-ext::define-clx-modifier #$NSAlternateKeyMask "Meta")
(hemlock-ext::define-clx-modifier #$NSAlphaShiftKeyMask "Lock")

(defun nsevent-to-key-event (nsevent)
  (let* ((unmodchars (send nsevent 'characters-ignoring-modifiers))
	 (n (if (%null-ptr-p unmodchars)
	      0
	      (send unmodchars 'length)))
	 (c (if (eql n 1)
	      (send unmodchars :character-at-index 0))))
    (when c
      (let* ((bits 0)
	     (modifiers (send nsevent 'modifier-flags)))
	(dolist (map hemlock-ext::*modifier-translations*)
	  (when (logtest modifiers (car map))
	    (setq bits (logior bits (hemlock-ext::key-event-modifier-mask
				     (cdr map))))))
	(hemlock-ext::make-key-event c bits)))))
    
  
(define-objc-method ((:void :key-down event)
		     lisp-text-view)
  (#_NSLog #@"Key down event = %@" :address event)
  (format t "~& keycode = ~s~&" (send event 'key-code))
  (let* ((buffer (text-view-buffer self)))
    (when buffer
      (let* ((info (hemlock-frame-command-info (send self 'window))))
	(when info
	  (let* ((key-event (nsevent-to-key-event event)))
	    (when event
	      (unless (eq buffer hi::*current-buffer*)
		(setf (hi::current-buffer) buffer))
	      (hi::interpret-key-event key-event info))))))))

(define-objc-method ((:void :set-selected-range (:<NSR>ange r)
			    :affinity (:<NSS>election<A>ffinity affinity)
			    :still-selecting (:<BOOL> still-selecting))
		     lisp-text-view)
  (let* ((d (hemlock-buffer-string-display (send self 'string)))
	 (point (hemlock::buffer-point (hemlock-display-buffer d)))
	 (location (pref r :<NSR>ange.location))
	 (len (pref r :<NSR>ange.length)))
    (when (eql len 0)
      (move-hemlock-mark-to-absolute-position point d location))
    (send-super :set-selected-range r
		:affinity affinity
		:still-selecting still-selecting)))
  

(defun make-textstorage-for-hemlock-buffer (buffer)
  (setf (hi::buffer-text-storage buffer)
	(make-objc-instance 'lisp-text-storage
			    :with-string
			    (make-instance
			     'hemlock-buffer-string
			     :display
			     (reset-display-cache
			      (make-hemlock-display)
			      buffer)))))

(defun make-scrolling-text-view-for-buffer (buffer x y width height hscroll-p)
  (slet ((contentrect (ns-make-rect x y width height)))
    (let* ((textstorage (make-textstorage-for-hemlock-buffer buffer))
	   (scrollview (send (make-objc-instance
			      'ns-scroll-view
			      :with-frame contentrect) 'autorelease)))
      (send scrollview :set-border-type #$NSBezelBorder)
      (send scrollview :set-has-vertical-scroller t)
      (send scrollview :set-has-horizontal-scroller hscroll-p)
      (send scrollview :set-rulers-visible nil)
      (send scrollview :set-autoresizing-mask (logior
					       #$NSViewWidthSizable
					       #$NSViewHeightSizable))
      (send (send scrollview 'content-view) :set-autoresizes-subviews t)
      (let* ((layout (make-objc-instance 'ns-layout-manager)))
	(send textstorage :add-layout-manager layout)
	(send layout 'release)
	(slet* ((contentsize (send scrollview 'content-size))
		(containersize (ns-make-size
				1.0f7
				1.0f7))
		(tv-frame (ns-make-rect
			   0.0f0
			   0.0f0
			   (pref contentsize :<NSS>ize.width)
			   (pref contentsize :<NSS>ize.height))))
          (let* ((container (send (make-objc-instance
				   'ns-text-container
				   :with-container-size containersize)
				  'autorelease)))
	    (send layout :add-text-container container)
	    (let* ((tv (send (make-objc-instance 'lisp-text-view
						 :with-frame tv-frame
						 :text-container container)
			     'autorelease)))
	      (send tv :set-min-size (ns-make-size
				      0.0f0
				      (pref contentsize :<NSS>ize.height)))
	      (send tv :set-max-size (ns-make-size 1.0f7 1.0f7))
	      (send tv :set-rich-text nil)
	      (send tv :set-horizontally-resizable hscroll-p)
	      (send tv :set-vertically-resizable t) 
	      (send tv :set-autoresizing-mask #$NSViewWidthSizable)
	      (send container :set-width-tracks-text-view (not hscroll-p))
	      (send container :set-height-tracks-text-view nil)
	      (send scrollview :set-document-view tv)	      
	      (values tv scrollview))))))))


(defun make-scrolling-textview-for-view (superview buffer hscroll-p)
  (slet ((contentrect (send (send superview 'content-view) 'frame)))
    (multiple-value-bind (tv scrollview)
	(make-scrolling-text-view-for-buffer
	 buffer
	 (pref contentrect :<NSR>ect.origin.x)
	 (pref contentrect :<NSR>ect.origin.y)
	 (pref contentrect :<NSR>ect.size.width)
	 (pref contentrect :<NSR>ect.size.height)
	 hscroll-p)
      (send superview :set-content-view scrollview)
      tv)))

(defun make-scrolling-textview-for-window (&key window buffer hscroll-p)
  (make-scrolling-textview-for-view (send window 'content-view) buffer hscroll-p))

(defmethod hemlock-frame-command-info ((w ns:ns-window))
  nil)

(defclass hemlock-frame (ns:ns-window)
    ((command-info :initform (hi::make-command-interpreter-info)
		   :accessor hemlock-frame-command-info))
  (:metaclass ns:+ns-object))

(defmethod shared-initialize :after ((w hemlock-frame)
				     slot-names
				     &key &allow-other-keys)
  (declare (ignore slot-names))
  (let ((info (hemlock-frame-command-info w)))
    (when info
      (setf (hi::command-interpreter-info-frame info) w))))

(defun get-cocoa-window-flag (w flagname)
  (case flagname
    (:accepts-mouse-moved-events
     (send w 'accepts-mouse-moved-events))
    (:cursor-rects-enabled
     (send w 'are-cursor-rects-enabled))
    (:auto-display
     (send w 'is-autodisplay))))

(defun (setf get-cocoa-window-flag) (value w flagname)
  (case flagname
    (:accepts-mouse-moved-events
     (send w :set-accepts-mouse-moved-events value))
    (:auto-display
     (send w :set-autodisplay value))))

(defun activate-window (w)
  ;; Make w the "key" and frontmost window.  Make it visible, if need be.
  (send w :make-key-and-order-front nil))

(defun new-hemlock-document-window (title &key
					  (x 0.0)
					  (y 0.0)
					  (height 200.0)
					  (width 500.0)
					  (closable t)
					  (iconifyable t)
					  (metal t)
					  (expandable t)
					  (backing :buffered)
					  (defer nil)
					  (accepts-mouse-moved-events nil)
					  (auto-display t)
					  (activate t))
  (rlet ((frame :<NSR>ect :origin.x (float x) :origin.y (float y) :size.width (float width) :size.height (float height)))
    (let* ((stylemask
            (logior #$NSTitledWindowMask
                    (if closable #$NSClosableWindowMask 0)
                    (if iconifyable #$NSMiniaturizableWindowMask 0)
                    (if expandable #$NSResizableWindowMask 0)
		    (if metal #$NSTexturedBackgroundWindowMask 0)))
           (backing-type
            (ecase backing
              ((t :retained) #$NSBackingStoreRetained)
              ((nil :nonretained) #$NSBackingStoreNonretained)
              (:buffered #$NSBackingStoreBuffered)))
           (w (make-instance
	       'hemlock-frame
	       :with-content-rect frame
	       :style-mask stylemask
	       :backing backing-type
	       :defer defer)))
      (send w :set-title (%make-nsstring title))
      (setf (get-cocoa-window-flag w :accepts-mouse-moved-events)
            accepts-mouse-moved-events
            (get-cocoa-window-flag w :auto-display)
            auto-display)
      (when activate (activate-window w))
      (values w (add-box-to-window w :reserve-below 20.0)))))

(defun add-box-to-window (w &key (reserve-above 0.0f0) (reserve-below 0.0f0))
  (let* ((window-content-view (send w 'content-view)))
    (slet ((window-frame (send window-content-view 'frame)))
      (slet ((box-rect (ns-make-rect 0.0f0
				      reserve-below
				      (pref window-frame :<NSR>ect.size.width)
				      (- (pref window-frame :<NSR>ect.size.height) (+ reserve-above reserve-below)))))
	(let* ((box (make-objc-instance 'ns-box :with-frame box-rect)))
	  (send box :set-autoresizing-mask (logior
					    #$NSViewWidthSizable
					    #$NSViewHeightSizable))
	  (send box :set-box-type #$NSBoxSecondary)
	  (send box :set-border-type #$NSLineBorder)
	  (send box :set-title-position #$NSBelowBottom)
	  (send window-content-view :add-subview box)
	  box)))))
	  
					
				      
(defun textview-for-hemlock-buffer (b &key (horizontal-scroll-p t))
  (process-interrupt
   *cocoa-event-process*
   #'(lambda ()
      (let* ((name (hi::buffer-name b)))
	(multiple-value-bind (window box)
	    (new-hemlock-document-window name :activate nil)
	  (let* ((tv (make-scrolling-textview-for-view box
						       b
						       horizontal-scroll-p)))
	    (multiple-value-bind (height width)
		(size-of-char-in-font (default-font))
	      (size-textview-containers tv height width 24 80))
	    (activate-window window)
	    tv))))))


(defun read-file-to-hemlock-buffer (path)
  (hemlock::find-file-buffer path))

(defun hemlock-buffer-from-nsstring (nsstring name)
  (let* ((buffer (hi::make-buffer name)))
    (hi::delete-region (hi::buffer-region buffer))
    (hi::modifying-buffer buffer)
    (hi::with-mark ((mark (hi::buffer-point buffer) :left-inserting))
      (let* ((string-len (send nsstring 'length))
	     (line-start 0)
	     (first-line (hi::mark-line mark))
	     (previous first-line)
	     (buffer (hi::line-%buffer first-line)))
	(slet ((remaining-range (ns-make-range 0 1)))
	  (rlet ((line-end-index :unsigned)
		 (contents-end-index :unsigned))
	    (do* ((number (+ (hi::line-number first-line) hi::line-increment)
			  (+ number hi::line-increment)))
		 ((= line-start string-len))
	      (setf (pref remaining-range :<NSR>ange.location) line-start)
	      (send nsstring
		    :get-line-start (%null-ptr)
		    :end line-end-index
		    :contents-end contents-end-index
		    :for-range remaining-range)
	      (let* ((contents-end (pref contents-end-index :unsigned))
		     (chars (make-string (- contents-end line-start))))
		(do* ((i line-start (1+ i))
		      (j 0 (1+ j)))
		     ((= i contents-end))
		  (setf (schar chars j) (code-char (send nsstring :character-at-index i))))
		(if (eq previous first-line)
		  (progn
		    (hi::insert-string mark chars)
		    (hi::insert-character mark #\newline)
		    (setq first-line nil))
		  (if (eq (pref line-end-index :unsigned) string-len)
		    (hi::insert-string mark chars)
		    (let* ((line (hi::make-line
				  :previous previous
				  :%buffer buffer
				  :chars chars
				  :number number)))
		      (setf (hi::line-next previous) line)
		      (setq previous line))))
		(setq line-start (pref line-end-index :unsigned))))))))
    buffer))

(setq hi::*beep-function* #'(lambda (stream)
			      (declare (ignore stream))
			      (#_NSBeep)))

(defun edit (path)
  (textview-for-hemlock-buffer (read-file-to-hemlock-buffer path)))

(defun for-each-textview-using-storage (textstorage f)
  (let* ((layouts (send textstorage 'layout-managers)))
    (unless (%null-ptr-p layouts)
      (dotimes (i (send layouts 'count))
	(let* ((layout (send layouts :object-at-index i))
	       (containers (send layout 'text-containers)))
	  (unless (%null-ptr-p containers)
	    (dotimes (j (send containers 'count))
	      (let* ((container (send containers :object-at-index j))
		     (tv (send container 'text-view)))
		(funcall f tv)))))))))
  

(defun hi::textstorage-set-point-position (textstorage)
  (format t "~& setting point ...")
  (let* ((string (send textstorage 'string))
	 (buffer (hemlock-display-buffer (hemlock-buffer-string-display string)))
	 (point (hi::buffer-point buffer))
	 (pos (mark-absolute-position point)))
    (for-each-textview-using-storage
     textstorage
     #'(lambda (tv)
	 (send tv :set-selected-range (ns-make-range pos 0))))))

	      
	 