(in-package "CCL")

(require "COCOA")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))

(defvar *buffer-id-map* (make-id-map))

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
    ((id :foreign-type :unsigned))
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
  (char-code (hemlock-char-at-index
	      (id-map-object *buffer-id-map* (slot-value self 'id)) index)))


(define-objc-method ((:unsigned length)
		     hemlock-buffer-string)
  (let* ((display-object (id-map-object *buffer-id-map* (slot-value self 'id))))
      (or (hemlock-display-buflen display-object)
	  (setf (hemlock-display-buflen display-object)
		(hemlock-buffer-length (hemlock-display-buffer display-object))))))


(define-objc-method ((:unsigned lisp-id)
		     hemlock-buffer-string)
  (slot-value self 'id))

(define-objc-method ((:id description)
		     hemlock-buffer-string)
  (send (@class ns-string) :string-with-format #@"%s : stringid %d/len %d"
	(:address (#_object_getClassName self)
		  :unsigned (slot-value self 'id)
		  :unsigned (send self 'length))))

(define-objc-method ((:id :init-with-buffer-id (:unsigned n))
		     hemlock-buffer-string)
  (send-super 'init)
  (setf (slot-value self 'id) n)
  self)



		     
(defclass lisp-text-storage (ns:ns-text-storage)
    ((string :foreign-type :id)
     (defaultattrs :foreign-type :id))
  (:metaclass ns:+ns-object))


(define-objc-method ((:id string) lisp-text-storage)
  (slot-value self 'string))

(define-objc-method ((:id :attributes-at-index (:unsigned index)
			  :effective-range ((* :<NSR>ange) rangeptr))
		     lisp-text-storage)
  '(#_NSLog #@"Attributes at index %d, rangeptr = %x"
	   :unsigned index :address rangeptr)
  (let* ((hemlock-display (id-map-object *buffer-id-map* (send (slot-value self 'string) 'lisp-id)))
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

(define-objc-method ((:id :init-with-buffer-id (:unsigned buffer-id-number))
		     lisp-text-storage)
  (send-super 'init)
  (with-slots (string defaultattrs) self
    (setq string (make-objc-instance
		  'hemlock-buffer-string 
		  :with-buffer-id buffer-id-number))
    (setq defaultattrs (create-text-attributes)))
  self)



(define-objc-method ((:id description)
		     lisp-text-storage)
  (send (@class ns-string) :string-with-format #@"%s : string %@"
	(:address (#_object_getClassName self)
	 :id (slot-value self 'string))))


(defclass lisp-text-view (ns:ns-text-view)
    ()
  (:metaclass ns:+ns-object))

(define-objc-method ((:void :key-down event)
		     lisp-text-view)
  (#_NSLog #@"Key down event : %@" :address event)
  (send-super :key-down event))

(define-objc-method ((:void :set-selected-range (:<NSR>ange r)
			    :affinity (:<NSS>election<A>ffinity affinity)
			    :still-selecting (:<BOOL> still-selecting))
		     lisp-text-view)
  (let* ((d (id-map-object *buffer-id-map*
			   (send (send self 'string) 'lisp-id)))
	 (point (hemlock::buffer-point (hemlock-display-buffer d)))
	 (location (pref r :<NSR>ange.location))
	 (len (pref r :<NSR>ange.length)))
    (when (eql len 0)
      (move-hemlock-mark-to-absolute-position point d location))
    (send-super :set-selected-range r
		:affinity affinity
		:still-selecting still-selecting)))
  
			   

(define-objc-class-method ((:id :scrollview-with-rect (:<NSR>ect contentrect)
				:lisp-buffer-id (:unsigned stringid)
				:horizontal-scroll-p (:<BOOL> hscroll-p))
			   lisp-text-view)
    (let* ((textstorage (make-objc-instance
			 'lisp-text-storage
			 :with-buffer-id stringid))
	   (scrollview
	    (send (make-objc-instance
		   'ns-scroll-view
		   :with-frame contentrect)
		  'autorelease)))
      (send scrollview :set-border-type #$NSBezelBorder)
      (send scrollview :set-has-vertical-scroller t)
      (send scrollview :set-has-horizontal-scroller hscroll-p)
      (send scrollview :set-rulers-visible nil)
      (send scrollview :set-autoresizing-mask (logior #$NSViewWidthSizable
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
	    (let* ((tv (send
			(send (send self 'alloc)
			      :init-with-frame tv-frame
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
	      tv))))))



(define-objc-class-method ((:id :scrollview-for-window window
				:buffer-id (:unsigned buffer-id)
				:horizontal-scroll-p (:<BOOL> hscroll-p))
			   lisp-text-view)
    (let* ((contentview (send window 'content-view)))
      (slet ((contentrect (send contentview 'frame)))
	(let* ((tv  (send
			    (@class lisp-text-view)
			    :scrollview-with-rect contentrect
			    :lisp-buffer-id buffer-id
			    :horizontal-scroll-p hscroll-p))
	       (scrollview (send (send tv 'superview) 'superview)))
	  (send window :set-content-view scrollview)
	  tv))))
    

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

(defun new-cocoa-document-window (title &key
                                        (class-name "NSWindow")
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
           (w (make-objc-instance
	       class-name
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
      w)))
					   
(defun textview-for-buffer (id &key (horizontal-scroll-p t))
  (process-interrupt
   *cocoa-event-process*
   #'(lambda ()
      (let* ((d (id-map-object *buffer-id-map* id))
	     (name (hi::buffer-name (hemlock-display-buffer d)))
	     (w (new-cocoa-document-window name :activate nil))
	     (tv 
	      (send (@class lisp-text-view)
		    :scrollview-for-window w
		    :buffer-id id
		    :horizontal-scroll-p horizontal-scroll-p)))
	(multiple-value-bind (height width)
	    (size-of-char-in-font (default-font))
	  (size-textview-containers tv height width 24 80))
	(activate-window w)
	tv))))

(defun put-textview-in-box (box)
  (slet ((r (send (send box 'content-view) 'bounds)))
    (let* ((sv (make-objc-instance 'ns-scroll-view :with-frame r))
	   (sv-content-view (send sv 'content-view)))
      (declare (ignorable sv-content-view))
      (send box :set-content-view sv)
      (slet ((sv-content-size (send sv 'content-size)))
	(slet ((tv-frame (ns-make-rect 0.0f0 0.0f0
				       (pref sv-content-size :<NSS>ize.width)
				       (pref sv-content-size :<NSS>ize.height))))
	  (let* ((tv (make-objc-instance 'ns-text-view
					 :with-frame tv-frame)))
	    (send sv :set-document-view tv)
	    (send box :set-content-view sv)
	    (values tv sv)))))))

(defun read-file-to-hemlock-buffer (path)
  (let* ((buffer (hemlock::find-file-buffer path)))
    (reset-display-cache (make-hemlock-display) buffer)))


(defun edit (path)
  (textview-for-buffer (assign-id-map-id *buffer-id-map*
					 (read-file-to-hemlock-buffer path))))
