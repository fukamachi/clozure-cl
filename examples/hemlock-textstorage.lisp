;;;-*- Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "HEMLOCK"))

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
  #+debug
  (#_NSLog #@"Character at index %d = %c" :unsigned index
           :unsigned (char-code (hemlock-char-at-index (hemlock-buffer-string-display self) index)))
  (char-code (hemlock-char-at-index (hemlock-buffer-string-display self) index)))


(define-objc-method ((:unsigned length)
		     hemlock-buffer-string)
  (let* ((display-object (hemlock-buffer-string-display self)))
    #+debug
    (#_NSLog #@"Length: cached = %d, actual = %d"
             :unsigned (or (hemlock-display-buflen display-object) -1)
             :unsigned (hemlock-buffer-length (hemlock-display-buffer display-object)))
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
  (declare (ignorable index))
  #+debug
  (#_NSLog #@"Attributes at index %d, rangeptr = %x"
	   :unsigned index :address rangeptr)
  (let* ((hemlock-display (hemlock-buffer-string-display (slot-value self 'string)))
	 (len (hemlock-display-buflen hemlock-display)))
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
     (blink-char :foreign-type :int :accessor blink-char)
     (pane :foreign-type :id :accessor text-view-pane))
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
	     (modifiers (send nsevent 'modifier-flags))
             (useful-modifiers (logandc2 modifiers
                                         (logior #$NSShiftKeyMask
                                                 #$NSAlphaShiftKeyMask))))
	(dolist (map hemlock-ext::*modifier-translations*)
	  (when (logtest useful-modifiers (car map))
	    (setq bits (logior bits (hemlock-ext::key-event-modifier-mask
				     (cdr map))))))
	(hemlock-ext::make-key-event c bits)))))
    
  
(define-objc-method ((:void :key-down event)
		     lisp-text-view)
  #+debug
  (#_NSLog #@"Key down event = %@" :address event)
  (let* ((buffer (text-view-buffer self)))
    (when buffer
      (let* ((info (hemlock-frame-command-info (send self 'window))))
	(when info
	  (let* ((key-event (nsevent-to-key-event event)))
	    (when event
	      (unless (eq buffer hi::*current-buffer*)
		(setf (hi::current-buffer) buffer))
	      (let* ((pane (text-view-pane self)))
		(unless (eql pane (hi::current-window))
		  (setf (hi::current-window) pane)))
	      #+debug
	      (format t "~& key-event = ~s" key-event)
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
  (make-objc-instance 'lisp-text-storage
		      :with-string
		      (make-instance
		       'hemlock-buffer-string
		       :display
		       (reset-display-cache
			(make-hemlock-display)
			buffer))))

(defclass modeline-view (ns:ns-view)
    ((pane :foreign-type :id :accessor modeline-view-pane))
  (:metaclass ns:+ns-object))


(defloadvar *modeline-text-attributes* nil)
(defparameter *modeline-font-name* "Courier New Bold Italic")
(defparameter *modeline-font-size* 10.0)

(defun buffer-for-modeline-view (mv)
  (let* ((pane (modeline-view-pane mv)))
    (unless (%null-ptr-p pane)
      (let* ((tv (text-pane-text-view pane)))
        (unless (%null-ptr-p tv)
          (let* ((textstorage (send tv 'text-storage)))
            (unless (%null-ptr-p textstorage)
              (let* ((display (hemlock-buffer-string-display
                               (send textstorage 'string))))
                (when display
                  (hemlock-display-buffer display))))))))))

(defun draw-modeline-string (modeline-view)
  (let* ((pane (modeline-view-pane modeline-view))
         (buffer (buffer-for-modeline-view modeline-view)))
    (when buffer
      ;; You don't want to know why this is done this way.
      (unless *modeline-text-attributes*
	(setq *modeline-text-attributes*
	      (create-text-attributes :color (send (@class "NSColor") 'black-color)
				      :font (default-font
					      :name *modeline-font-name*
					      :size *modeline-font-size*))))
      
      (let* ((string
              (apply #'concatenate 'string
                     (mapcar
                      #'(lambda (field)
                          (funcall (hi::modeline-field-function field)
                                   buffer pane))
                      (hi::buffer-modeline-fields buffer))))
             (len (length string)))
        (with-cstrs ((cstr string))
          (with-nsstr (nsstr cstr len)
            (send nsstr
                  :draw-at-point (ns-make-point 0.0f0 0.0f0)
                  :with-attributes *modeline-text-attributes*)))))))
  

(define-objc-method ((:void :draw-rect (:<NSR>ect rect)) 
                     modeline-view)
  (declare (ignore rect))
  (slet ((frame (send self 'bounds)))
     (#_NSDrawWhiteBezel frame frame)
     (draw-modeline-string self)))

  

(defclass modeline-scroll-view (ns:ns-scroll-view)
    ((modeline :foreign-type :id :accessor scroll-view-modeline)
     (pane :foreign-type :id :accessor scroll-view-pane))
  (:metaclass ns:+ns-object))

(define-objc-method ((:id :init-with-frame (:<NSR>ect frame))
                     modeline-scroll-view)
    (let* ((v (send-super :init-with-frame frame)))
      (when v
        (let* ((modeline (make-objc-instance 'modeline-view)))
          (send v :add-subview modeline)
          (setf (scroll-view-modeline v) modeline)))
      v))

(define-objc-method ((:void tile) modeline-scroll-view)
  (send-super 'tile)
  (let* ((modeline (scroll-view-modeline self)))
    (when (and (send self 'has-horizontal-scroller)
               (not (%null-ptr-p modeline)))
      (let* ((hscroll (send self 'horizontal-scroller)))
        (slet ((scrollbar-frame (send hscroll 'frame))
               (modeline-frame (send hscroll 'frame))) ; sic
           (let* ((modeline-width (* (pref modeline-frame
                                           :<NSR>ect.size.width)
                                     0.75e0)))
             (declare (single-float modeline-width))
             (setf (pref modeline-frame :<NSR>ect.size.width)
                   modeline-width
                   (the single-float
                     (pref scrollbar-frame :<NSR>ect.size.width))
                   (- (the single-float
                        (pref scrollbar-frame :<NSR>ect.size.width))
                      modeline-width)
                   (the single-float
                     (pref scrollbar-frame :<NSR>ect.origin.x))
                   (+ (the single-float
                        (pref scrollbar-frame :<NSR>ect.origin.x))
                      modeline-width))
             (send hscroll :set-frame scrollbar-frame)
             (send modeline :set-frame modeline-frame)))))))

(defclass text-pane (ns:ns-box)
    ((text-view :foreign-type :id :accessor text-pane-text-view)
     (mode-line :foreign-type :id :accessor text-pane-mode-line)
     (scroll-view :foreign-type :id :accessor text-pane-scroll-view))
  (:metaclass ns:+ns-object))

(define-objc-method ((:id :init-with-frame (:<NSR>ect frame))
                     text-pane)
    (let* ((pane (send-super :init-with-frame frame)))
      (unless (%null-ptr-p pane)
        (send pane :set-autoresizing-mask (logior
                                           #$NSViewWidthSizable
                                           #$NSViewHeightSizable))
        (send pane :set-box-type #$NSBoxSecondary)
        (send pane :set-border-type #$NSLineBorder)
        (send pane :set-title-position #$NSNoTitle))
      pane))
        

    
(defun make-scrolling-text-view-for-textstorage (textstorage x y width height)
  (slet ((contentrect (ns-make-rect x y width height)))
    (let* ((scrollview (send (make-objc-instance
			      'modeline-scroll-view
			      :with-frame contentrect) 'autorelease)))
      (send scrollview :set-border-type #$NSBezelBorder)
      (send scrollview :set-has-vertical-scroller t)
      (send scrollview :set-has-horizontal-scroller t)
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
	      (send tv :set-horizontally-resizable t)
	      (send tv :set-vertically-resizable t) 
	      (send tv :set-autoresizing-mask #$NSViewWidthSizable)
	      (send container :set-width-tracks-text-view nil)
	      (send container :set-height-tracks-text-view nil)
	      (send scrollview :set-document-view tv)	      
	      (values tv scrollview))))))))


(defun make-scrolling-textview-for-pane (pane textstorage)
  (slet ((contentrect (send (send pane 'content-view) 'frame)))
    (multiple-value-bind (tv scrollview)
	(make-scrolling-text-view-for-textstorage
	 textstorage
	 (pref contentrect :<NSR>ect.origin.x)
	 (pref contentrect :<NSR>ect.origin.y)
	 (pref contentrect :<NSR>ect.size.width)
	 (pref contentrect :<NSR>ect.size.height))
      (send pane :set-content-view scrollview)
      (setf (slot-value pane 'scroll-view) scrollview
            (slot-value pane 'text-view) tv
            (slot-value tv 'pane) pane
            (slot-value scrollview 'pane) pane)
      (let* ((modeline  (scroll-view-modeline scrollview)))
        (setf (slot-value pane 'mode-line) modeline
              (slot-value modeline 'pane) pane))
      tv)))



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

(defun new-hemlock-document-window (&key
                                    (x 200.0)
                                    (y 200.0)
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
      (setf (get-cocoa-window-flag w :accepts-mouse-moved-events)
            accepts-mouse-moved-events
            (get-cocoa-window-flag w :auto-display)
            auto-display)
      (when activate (activate-window w))
      (values w (add-pane-to-window w :reserve-below 20.0)))))

(defun add-pane-to-window (w &key (reserve-above 0.0f0) (reserve-below 0.0f0))
  (let* ((window-content-view (send w 'content-view)))
    (slet ((window-frame (send window-content-view 'frame)))
      (slet ((pane-rect (ns-make-rect 0.0f0
				      reserve-below
				      (pref window-frame :<NSR>ect.size.width)
				      (- (pref window-frame :<NSR>ect.size.height) (+ reserve-above reserve-below)))))
	(let* ((pane (make-objc-instance 'text-pane :with-frame pane-rect)))
	  (send window-content-view :add-subview pane)
	  pane)))))
	  
					
				      
(defun textpane-for-textstorage (ts)
  (let* ((pane (nth-value
                1
                (new-hemlock-document-window :activate nil)))
         (tv (make-scrolling-textview-for-pane pane ts)))
    (multiple-value-bind (height width)
        (size-of-char-in-font (default-font))
      (size-textview-containers tv height width 24 80))
    pane))


(defun read-file-to-hemlock-buffer (path)
  (hemlock::find-file-buffer path))

(defun hemlock-buffer-from-nsstring (nsstring name &rest modes)
  (let* ((buffer (hi::make-buffer name :modes modes)))
    (nsstring-to-buffer nsstring buffer)))

(defun nsstring-to-buffer (nsstring buffer)    
    (hi::delete-region (hi::buffer-region buffer))
    (hi::modifying-buffer buffer)
    (hi::with-mark ((mark (hi::buffer-point buffer) :left-inserting))
      (let* ((string-len (send nsstring 'length))
	     (line-start 0)
	     (first-line-terminator ())
	     (first-line (hi::mark-line mark))
	     (previous first-line)
	     (buffer (hi::line-%buffer first-line)))
	(slet ((remaining-range (ns-make-range 0 1)))
	  (rlet ((line-end-index :unsigned)
		 (contents-end-index :unsigned))
	    (do* ((number (+ (hi::line-number first-line) hi::line-increment)
			  (+ number hi::line-increment)))
		 ((= line-start string-len)
		  (let* ((line (hi::mark-line mark)))
		    (hi::insert-string mark (make-string 0))
		    (setf (hi::line-next previous) line
			  (hi::line-previous line) previous))
		  nil)
	      (setf (pref remaining-range :<NSR>ange.location) line-start)
	      (send nsstring
		    :get-line-start (%null-ptr)
		    :end line-end-index
		    :contents-end contents-end-index
		    :for-range remaining-range)
	      (let* ((contents-end (pref contents-end-index :unsigned))
		     (line-end (pref line-end-index :unsigned))
		     (chars (make-string (- contents-end line-start))))
		(do* ((i line-start (1+ i))
		      (j 0 (1+ j)))
		     ((= i contents-end))
		  (setf (schar chars j) (code-char (send nsstring :character-at-index i))))
		(unless first-line-terminator
		  (let* ((terminator (code-char
				      (send nsstring :character-at-index
					    contents-end))))
		    (setq first-line-terminator
		    (case terminator
		      (#\return (if (= line-end (+ contents-end 2))
				  :cp/m
				  :mac))
		      (t :unix)))))
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
		(setq line-start line-end)))))
	(when first-line-terminator
	  (setf (hi::buffer-external-format buffer) first-line-terminator))))
    (setf (hi::buffer-modified buffer) nil)
    buffer)



	
	
(setq hi::*beep-function* #'(lambda (stream)
			      (declare (ignore stream))
			      (#_NSBeep)))

;;; This function must run in the main event thread.
(defun %hemlock-frame-for-textstorage (ts title activate)
  (let* ((pane (textpane-for-textstorage ts))
         (w (send pane 'window)))
    (when title (send w :set-title (%make-nsstring title)))
    (when activate (activate-window w))
    w))

(defun hemlock-frame-for-textstorage (ts title activate)
  (process-interrupt *cocoa-event-process*
                     #'%hemlock-frame-for-textstorage
                     ts title activate))
  

(defun edit (path)
  (let* ((buffer (read-file-to-hemlock-buffer path))
         (textstorage (make-textstorage-for-hemlock-buffer buffer)))
    (hemlock-frame-for-textstorage textstorage (hi::buffer-name buffer) t)))

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
  
(defun hi::document-begin-editing (document)
  (send (slot-value document 'textstorage) 'begin-editing))

(defun hi::document-end-editing (document)
  (send (slot-value document 'textstorage) 'end-editing))

(defun hi::document-set-point-position (document)
  (let* ((textstorage (slot-value document 'textstorage))
	 (string (send textstorage 'string))
	 (buffer (hemlock-display-buffer (hemlock-buffer-string-display string)))
	 (point (hi::buffer-point buffer))
	 (pos (mark-absolute-position point)))
    (for-each-textview-using-storage
     textstorage
     #'(lambda (tv)
         (slet ((selection (ns-make-range pos 0)))
          (send tv :set-selected-range selection)
          (send tv :scroll-range-to-visible selection))))))

	      
(defun hi::buffer-note-insertion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        (let* ((pos (mark-absolute-position mark)))
          (unless (eq (hi::mark-%kind mark) :right-inserting)
            (decf pos n))
          (let* ((display (hemlock-buffer-string-display (send textstorage 'string))))
            (reset-display-cache display) 
            (update-line-cache-for-index display pos))
          
          (send textstorage
                :edited #$NSTextStorageEditedAttributes
                :range (ns-make-range pos 0)
                :change-in-length n)
          (send textstorage
                :edited #$NSTextStorageEditedCharacters
                :range (ns-make-range pos n)
                :change-in-length 0))))))

  

(defun hi::buffer-note-deletion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        (let* ((pos (mark-absolute-position mark)))
          (setq n (abs n))
          (let* ((display (hemlock-buffer-string-display (send textstorage 'string))))
            (reset-display-cache display) 
            (update-line-cache-for-index display pos))

	  (send textstorage
                :edited #$NSTextStorageEditedAttributes
                :range (ns-make-range pos n)
                :change-in-length (- n)))))))

(defun hi::set-document-modified (document flag)
  (let* ((windowcontrollers (send document 'window-controllers)))
    (dotimes (i (send windowcontrollers 'length))
      (send (send windowcontrollers :object-at-index i)
	    :set-document-edited flag))))

(defun hi::document-panes (document)
  (let* ((ts (slot-value document 'textstorage))
	 (panes ()))
    (for-each-textview-using-storage
     ts
     #'(lambda (tv)
	 (let* ((pane (text-view-pane tv)))
	   (unless (%null-ptr-p pane)
	     (push pane panes)))))
    panes))
    
	 
    
