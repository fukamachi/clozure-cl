;;;-*- Mode: LISP; Package: CCL -*-


(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA-WINDOW")
  (require "HEMLOCK"))

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))

(def-cocoa-default *editor-rows* :int 24)
(def-cocoa-default *editor-columns* :int 80)

;;; Background color components: red, blue, green, alpha.
;;; All should be single-floats between 0.0f0 and 1.0f0, inclusive.
(def-cocoa-default *editor-background-red-component* :int 1.0f0)
(def-cocoa-default *editor-background-blue-component* :int 1.0f0)
(def-cocoa-default *editor-background-green-component* :int 1.0f0)
(def-cocoa-default *editor-background-alpha-component* :int 1.0f0)

;;; At runtime, this'll be a vector of character attribute dictionaries.
(defloadvar *styles* ())

(defun make-editor-style-map ()
  (let* ((font-name *default-font-name*)
	 (font-size *default-font-size*)
	 (fonts (vector (default-font :name font-name :size font-size
				      :attributes ())
			(default-font :name font-name :size font-size
				      :attributes '(:bold))
			(default-font  :name font-name :size font-size
				      :attributes '(:italic))
			(default-font :name font-name :size font-size
				      :attributes '(:bold :italic))))
	 (color-class (find-class 'ns:ns-color))
	 (colors (vector (send color-class 'black-color)
			 (send color-class 'white-color)
			 (send color-class 'dark-gray-color)
			 (send color-class 'light-gray-color)
			 (send color-class 'red-color)
			 (send color-class 'blue-color)
			 (send color-class 'green-color)
			 (send color-class 'yellow-color)))
	 (styles (make-array (the fixnum (* (length fonts) (length colors)))))
	 (s 0))
    (declare (dynamic-extent fonts colors))
    (dotimes (c (length colors))
      (dotimes (f (length fonts))
	(setf (svref styles s) (create-text-attributes :font (svref fonts f)
						       :color (svref colors c)))
	(incf s)))
    (setq *styles* styles)))

(defun make-hemlock-buffer (&rest args)
  (let* ((buf (apply #'hi::make-buffer args)))
    (if buf
      (progn
	(setf (hi::buffer-gap-context buf) (hi::make-buffer-gap-context))
	buf)
      (progn
	(format t "~& couldn't make hemlock buffer with args ~s" args)
	(dbg)
	nil))))
	 
;;; Define some key event modifiers.

;;; HEMLOCK-EXT::DEFINE-CLX-MODIFIER is kind of misnamed; we can use
;;; it to map NSEvent modifier keys to key-event modifiers.

(hemlock-ext::define-clx-modifier #$NSShiftKeyMask "Shift")
(hemlock-ext::define-clx-modifier #$NSControlKeyMask "Control")
(hemlock-ext::define-clx-modifier #$NSAlternateKeyMask "Meta")
(hemlock-ext::define-clx-modifier #$NSAlphaShiftKeyMask "Lock")


;;; We want to display a Hemlock buffer in a "pane" (an on-screen
;;; view) which in turn is presented in a "frame" (a Cocoa window).  A
;;; 1:1 mapping between frames and panes seems to fit best into
;;; Cocoa's document architecture, but we should try to keep the
;;; concepts separate (in case we come up with better UI paradigms.)
;;; Each pane has a modeline (which describes attributes of the
;;; underlying document); each frame has an echo area (which serves
;;; to display some commands' output and to provide multi-character
;;; input.)


;;; I'd pretty much concluded that it wouldn't be possible to get the
;;; Cocoa text system (whose storage model is based on NSString
;;; NSMutableAttributedString, NSTextStorage, etc.) to get along with
;;; Hemlock, and (since the whole point of using Hemlock was to be
;;; able to treat an editor buffer as a rich lisp data structure) it
;;; seemed like it'd be necessary to toss the higher-level Cocoa text
;;; system and implement our own scrolling, redisplay, selection
;;; ... code.
;;;
;;; Mikel Evins pointed out that NSString and friends were
;;; abstract classes and that there was therefore no reason (in
;;; theory) not to implement a thin wrapper around a Hemlock buffer
;;; that made it act like an NSString.  As long as the text system can
;;; ask a few questions about the NSString (its length and the
;;; character and attributes at a given location), it's willing to
;;; display the string in a scrolling, mouse-selectable NSTextView;
;;; as long as Hemlock tells the text system when and how the contents
;;; of the abstract string changes, Cocoa will handle the redisplay
;;; details.
;;;


;;; Hemlock-buffer-string objects:

(defclass hemlock-buffer-string (ns:ns-string)
    ((cache :initform nil :initarg :cache :accessor hemlock-buffer-string-cache))
  (:metaclass ns:+ns-object))

;;; Cocoa wants to treat the buffer as a linear array of characters;
;;; Hemlock wants to treat it as a doubly-linked list of lines, so
;;; we often have to map between an absolute position in the buffer
;;; and a relative position on a line.  We can certainly do that
;;; by counting the characters in preceding lines every time that we're
;;; asked, but we're often asked to map a sequence of nearby positions
;;; and wind up repeating a lot of work.  Caching the results of that
;;; work seems to speed things up a bit in many cases; this data structure
;;; is used in that process.  (It's also the only way to get to the
;;; actual underlying Lisp buffer from inside the network of text-system
;;; objects.)

(defstruct buffer-cache 
  buffer				; the hemlock buffer
  buflen				; length of buffer, if known
  workline				; cache for character-at-index
  workline-offset			; cached offset of workline
  workline-length			; length of cached workline
  workline-start-font-index		; current font index at start of worklin
  )

;;; Initialize (or reinitialize) a buffer cache, so that it points
;;; to the buffer's first line (which is the only line whose
;;; absolute position will never change).  Code which modifies the
;;; buffer generally has to call this, since any cached information
;;; might be invalidated by the modification.

(defun reset-buffer-cache (d &optional (buffer (buffer-cache-buffer d)
						buffer-p))
  (when buffer-p (setf (buffer-cache-buffer d) buffer))
  (let* ((hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
	 (workline (hi::mark-line
		    (hi::buffer-start-mark buffer))))
    (setf (buffer-cache-buflen d) (hemlock-buffer-length buffer)
	  (buffer-cache-workline-offset d) 0
	  (buffer-cache-workline d) workline
	  (buffer-cache-workline-length d) (hi::line-length workline)
	  (buffer-cache-workline-start-font-index d) 0)
    d))


;;; Update the cache so that it's describing the current absolute
;;; position.
(defun update-line-cache-for-index (cache index)
  (let* ((buffer (buffer-cache-buffer cache))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
	 (line (or
		(buffer-cache-workline cache)
		(progn
		  (reset-buffer-cache cache)
		  (buffer-cache-workline cache))))
	 (pos (buffer-cache-workline-offset cache))
	 (len (buffer-cache-workline-length cache))
	 (moved nil))
    (loop
      (when (and (>= index pos)
		   (< index (1+ (+ pos len))))
	  (let* ((idx (- index pos)))
	    (when moved
	      (setf (buffer-cache-workline cache) line
		    (buffer-cache-workline-offset cache) pos
		    (buffer-cache-workline-length cache) len))
	    (return (values line idx))))
	(setq moved t)
      (if (< index pos)
	(setq line (hi::line-previous line)
	      len (hi::line-length line)
	      pos (1- (- pos len)))
	(setq line (hi::line-next line)
	      pos (1+ (+ pos len))
	      len (hi::line-length line))))))

;;; Ask Hemlock to count the characters in the buffer.
(defun hemlock-buffer-length (buffer)
  (let* ((hi::*buffer-gap-context* (hi::buffer-gap-context buffer)))
    (hemlock::count-characters (hemlock::buffer-region buffer))))

;;; Find the line containing (or immediately preceding) index, which is
;;; assumed to be less than the buffer's length.  Return the character
;;; in that line or the trailing #\newline, as appropriate.
(defun hemlock-char-at-index (cache index)
  (let* ((hi::*buffer-gap-context*
	  (hi::buffer-gap-context (buffer-cache-buffer cache))))
    (multiple-value-bind (line idx) (update-line-cache-for-index cache index)
      (let* ((len (hemlock::line-length line)))
        (if (< idx len)
          (hemlock::line-character line idx)
          #\newline)))))

;;; Given an absolute position, move the specified mark to the appropriate
;;; offset on the appropriate line.
(defun move-hemlock-mark-to-absolute-position (mark cache abspos)
  (let* ((hi::*buffer-gap-context*
	  (hi::buffer-gap-context (buffer-cache-buffer cache))))
    (multiple-value-bind (line idx) (update-line-cache-for-index cache abspos)
      #+debug
      (#_NSLog #@"Moving point from current pos %d to absolute position %d"
	       :int (mark-absolute-position mark)
	       :int abspos)
      (hemlock::move-to-position mark idx line)
      #+debug
      (#_NSLog #@"Moved mark to %d" :int (mark-absolute-position mark)))))

;;; Return the absolute position of the mark in the containing buffer.
;;; This doesn't use the caching mechanism, so it's always linear in the
;;; number of preceding lines.
(defun mark-absolute-position (mark)
  (let* ((pos (hi::mark-charpos mark)))
    (do* ((line (hi::line-previous (hi::mark-line mark))
		(hi::line-previous line)))
	 ((null line) pos)
      (incf pos (1+ (hi::line-length line))))))

;;; Return the length of the abstract string, i.e., the number of
;;; characters in the buffer (including implicit newlines.)
(define-objc-method ((:unsigned length)
		     hemlock-buffer-string)
  (let* ((cache (hemlock-buffer-string-cache self)))
    (or (buffer-cache-buflen cache)
        (setf (buffer-cache-buflen cache)
              (let* ((buffer (buffer-cache-buffer cache)))
		(hemlock-buffer-length buffer))))))


;;; Return the character at the specified index (as a :unichar.)
(define-objc-method ((:unichar :character-at-index (unsigned index))
		     hemlock-buffer-string)
  (char-code (hemlock-char-at-index (hemlock-buffer-string-cache self) index)))


;;; Return an NSData object representing the bytes in the string.  If
;;; the underlying buffer uses #\linefeed as a line terminator, we can
;;; let the superclass method do the work; otherwise, we have to
;;; ensure that each line is terminated according to the buffer's
;;; conventions.
(define-objc-method ((:id :data-using-encoding (:<NSS>tring<E>ncoding encoding)
			  :allow-lossy-conversion (:<BOOL> flag))
		     hemlock-buffer-string)
  (let* ((buffer (buffer-cache-buffer (hemlock-buffer-string-cache self)))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
	 (external-format (if buffer (hi::buffer-external-format buffer )))
	 (raw-length (if buffer (hemlock-buffer-length buffer) 0)))
    
    (if (eql 0 raw-length)
      (make-objc-instance 'ns:ns-mutable-data :with-length 0)
      (case external-format
	((:unix nil)
	 (send-super :data-using-encoding encoding :allow-lossy-conversion flag))
	((:macos :cp/m)
	 (let* ((cp/m-p (eq external-format :cp/m)))
	   (when cp/m-p
	     ;; This may seem like lot of fuss about an ancient OS and its
	     ;; odd line-termination conventions.  Of course, I'm actually
	     ;; referring to CP/M-86.
	     (do* ((line (hi::mark-line (hi::buffer-start-mark buffer))
			 next)
		   (next (hi::line-next line) (hi::line-next line)))
		  ((null line))
	       (when next (incf raw-length))))
	   (let* ((pos 0)
		  (data (make-objc-instance 'ns:ns-mutable-data
					    :with-length raw-length))
		  (bytes (send data 'mutable-bytes)))
	     (do* ((line (hi::mark-line (hi::buffer-start-mark buffer))
			 next)
		   (next (hi::line-next line) (hi::line-next line)))
		  ((null line) data)
	       (let* ((chars (hi::line-chars line))
		      (len (length chars)))
		 (unless (zerop len)
		   (%copy-ivector-to-ptr chars 0 bytes pos len)
		   (incf pos len))
		 (when next
		   (setf (%get-byte bytes pos) (char-code #\return))
		   (when cp/m-p
		     (incf pos)
		   (setf (%get-byte bytes pos) (char-code #\linefeed))  
		   (incf pos))))))))))))


;;; For debugging, mostly: make the printed representation of the string
;;; referenence the named Hemlock buffer.
(define-objc-method ((:id description)
		     hemlock-buffer-string)
  (let* ((cache (hemlock-buffer-string-cache self))
	 (b (buffer-cache-buffer cache)))
    (with-cstrs ((s (format nil "~a" b)))
      (send (@class ns-string) :string-with-format #@"<%s for %s>"
	(:address (#_object_getClassName self) :address s)))))



;;; hemlock-text-storage objects
(defclass hemlock-text-storage (ns:ns-text-storage)
    ((string :foreign-type :id)
     (edit-count :foreign-type :int))
  (:metaclass ns:+ns-object))


;;; Return true iff we're inside a "beginEditing/endEditing" pair
(define-objc-method ((:<BOOL> editing-in-progress) hemlock-text-storage)
  (not (eql (slot-value self 'edit-count) 0)))

(defun textstorage-note-insertion-at-position (self pos n)
  (send self
        :edited #$NSTextStorageEditedAttributes
        :range (ns-make-range pos 0)
        :change-in-length n)
  (send self
        :edited #$NSTextStorageEditedCharacters
        :range (ns-make-range pos n)
        :change-in-length 0))

(define-objc-method ((:void :note-insertion params) hemlock-text-storage)
  (let* ((pos (send (send params :object-at-index 0) 'int-value))
         (n (send (send params :object-at-index 1) 'int-value)))
    (textstorage-note-insertion-at-position self pos n)))

(define-objc-method ((:void :note-deletion params) hemlock-text-storage)
  (let* ((pos (send (send params :object-at-index 0) 'int-value))
         (n (send (send params :object-at-index 1) 'int-value)))
    (send self
          :edited #$NSTextStorageEditedCharacters
          :range (ns-make-range pos n)
          :change-in-length (- n))
    (let* ((display (hemlock-buffer-string-cache (send self 'string))))
            (reset-buffer-cache display) 
            (update-line-cache-for-index display pos))))

(define-objc-method ((:void :note-modification params) hemlock-text-storage)
  (let* ((pos (send (send params :object-at-index 0) 'int-value))
         (n (send (send params :object-at-index 1) 'int-value)))
    #+debug
    (#_NSLog #@"Note modification: pos = %d, n = %d" :int pos :int n)
    (send self
          :edited (logior #$NSTextStorageEditedCharacters
                          #$NSTextStorageEditedAttributes)
          :range (ns-make-range pos n)
          :change-in-length 0)))

(define-objc-method ((:void begin-editing) hemlock-text-storage)
  #+debug
  (#_NSLog #@"begin-editing")
  (incf (slot-value self 'edit-count))
  (send-super 'begin-editing))

(define-objc-method ((:void end-editing) hemlock-text-storage)
  #+debug
  (#_NSLog #@"end-editing")
  (send-super 'end-editing)
  (decf (slot-value self 'edit-count)))

;;; Return true iff we're inside a "beginEditing/endEditing" pair
(define-objc-method ((:<BOOL> editing-in-progress) hemlock-text-storage)
  (not (eql (slot-value self 'edit-count) 0)))

  

;;; Access the string.  It'd be nice if this was a generic function;
;;; we could have just made a reader method in the class definition.
(define-objc-method ((:id string) hemlock-text-storage)
  (slot-value self 'string))

(define-objc-method ((:id :init-with-string s) hemlock-text-storage)
  (let* ((newself (send-super 'init)))
    (setf (slot-value newself 'string) s)
    newself))

;;; This is the only thing that's actually called to create a
;;; hemlock-text-storage object.  (It also creates the underlying
;;; hemlock-buffer-string.)
(defun make-textstorage-for-hemlock-buffer (buffer)
  (make-objc-instance 'hemlock-text-storage
		      :with-string
		      (make-instance
		       'hemlock-buffer-string
		       :cache
		       (reset-buffer-cache
			(make-buffer-cache)
			buffer))))

;;; So far, we're ignoring Hemlock's font-marks, so all characters in
;;; the buffer are presumed to have default attributes.
(define-objc-method ((:id :attributes-at-index (:unsigned index)
			  :effective-range ((* :<NSR>ange) rangeptr))
		     hemlock-text-storage)
  (declare (ignorable index))
  (let* ((buffer-cache (hemlock-buffer-string-cache (slot-value self 'string)))
	 (len (buffer-cache-buflen buffer-cache)))
    (unless (%null-ptr-p rangeptr)
      (setf (pref rangeptr :<NSR>ange.location) 0
	    (pref rangeptr :<NSR>ange.length) len))
    (svref *styles* 0)))

;;; The range's origin should probably be the buffer's point; if
;;; the range has non-zero length, we probably need to think about
;;; things harder.
(define-objc-method ((:void :replace-characters-in-range (:<NSR>ange r)
			    :with-string string)
		     hemlock-text-storage)
  (declare (ignorable r string))
  #+debug
  (#_NSLog #@"replace-characters-in-range (%d %d) with-string %@"
	   :unsigned (pref r :<NSR>ange.location)
	   :unsigned (pref r :<NSR>ange.length)
	   :id string))

;;; I'm not sure if we want the text system to be able to change
;;; attributes in the buffer.
(define-objc-method ((:void :set-attributes attributes
			    :range (:<NSR>ange r))
		     hemlock-text-storage)
  (declare (ignorable attributes r))
  #+debug
  (#_NSLog #@"set-attributes %@ range (%d %d)"
	   :id attributes
	   :unsigned (pref r :<NSR>ange.location)
	   :unsigned (pref r :<NSR>ange.length)))

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

;;; Again, it's helpful to see the buffer name when debugging.
(define-objc-method ((:id description)
		     hemlock-text-storage)
  (send (@class ns-string) :string-with-format #@"%s : string %@"
	(:address (#_object_getClassName self) :id (slot-value self 'string))))

;;; This needs to happen on the main thread.
(define-objc-method ((:void ensure-selection-visible)
                     hemlock-text-storage)
  (for-each-textview-using-storage
   self
   #'(lambda (tv)
       (send tv :scroll-range-to-visible (send tv 'selected-range)))))


(defun close-hemlock-textstorage (ts)
  (let* ((string (slot-value ts 'string)))
    (setf (slot-value ts 'string) (%null-ptr))
    (unless (%null-ptr-p string)
      (let* ((cache (hemlock-buffer-string-cache string))
	     (buffer (if cache (buffer-cache-buffer cache))))
	(when buffer
	  (setf (buffer-cache-buffer cache) nil
		(slot-value string 'cache) nil
		(hi::buffer-document buffer) nil)
	  (let* ((p (hi::buffer-process buffer)))
	    (when p
	      (setf (hi::buffer-process buffer) nil)
	      (process-kill p)))
	  (when (eq buffer hi::*current-buffer*)
	    (setf (hi::current-buffer)
		  (car (last hi::*buffer-list*))))
	  (hi::invoke-hook (hi::buffer-delete-hook buffer) buffer)
	  (hi::invoke-hook hemlock::delete-buffer-hook buffer)
	  (setq hi::*buffer-list* (delq buffer hi::*buffer-list*))
	  (hi::delete-string (hi::buffer-name buffer) hi::*buffer-names*))))))

      


;;; An abstract superclass of the main and echo-area text views.
(defclass hemlock-textstorage-text-view (ns::ns-text-view)
    ((save-blink-color :foreign-type :id))
  (:metaclass ns:+ns-object))

;;; Set and display the selection at pos, whose length is len and whose
;;; affinity is affinity.  This should never be called from some Cocoa
;;; event handler; it should not call anything that'll try to set the
;;; underlying buffer's point and/or mark.
(define-objc-method ((:void :update-selection (:int pos)
                            :length (:int len)
                            :affinity (:<NSS>election<A>ffinity affinity))
                     hemlock-textstorage-text-view)
  (slet ((range (ns-make-range pos len)))
    (send-super :set-selected-range range
                :affinity affinity
                :still-selecting nil)
    (send self :scroll-range-to-visible range)))
  
;;; A specialized NSTextView.  Some of the instance variables are intended
;;; to support paren highlighting by blinking, but that doesn't work yet.
;;; The NSTextView is part of the "pane" object that displays buffers.
(defclass hemlock-text-view (hemlock-textstorage-text-view)
    ((pane :foreign-type :id :accessor text-view-pane))
  (:metaclass ns:+ns-object))

;;; Access the underlying buffer in one swell foop.
(defmethod text-view-buffer ((self hemlock-text-view))
  (buffer-cache-buffer (hemlock-buffer-string-cache (send (send self 'text-storage) 'string))))

;;; Translate a keyDown NSEvent to a Hemlock key-event.
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

(defun pass-key-down-event-to-hemlock (self event)
  #+debug
  (#_NSLog #@"Key down event = %@" :address event)
  (let* ((buffer (text-view-buffer self)))
    (when buffer
      (let* ((q (hemlock-frame-event-queue (send self 'window))))
        (hi::enqueue-key-event q (nsevent-to-key-event event)))))
  ;; Probably not the right place for this, but needs to happen
  ;; -somewhere-, and needs to happen in the event thread.
  
  )

(defun enqueue-buffer-operation (buffer thunk)
  (dolist (w (hi::buffer-windows buffer))
    (let* ((q (hemlock-frame-event-queue (send w 'window)))
           (op (hi::make-buffer-operation :thunk thunk)))
      (hi::enqueue-key-event q op))))

  
;;; Process a key-down NSEvent in a Hemlock text view by translating it
;;; into a Hemlock key event and passing it into the Hemlock command
;;; interpreter.  The underlying buffer becomes Hemlock's current buffer
;;; and the containing pane becomes Hemlock's current window when the
;;; command is processed.  Use the frame's command state object.

(define-objc-method ((:void :key-down event)
		     hemlock-text-view)
  (pass-key-down-event-to-hemlock self event))

;;; Update the underlying buffer's point.  Should really set the
;;; active region (in Hemlock terms) as well.
(define-objc-method ((:void :set-selected-range (:<NSR>ange r)
			    :affinity (:<NSS>election<A>ffinity affinity)
			    :still-selecting (:<BOOL> still-selecting))
		     hemlock-text-view)
  (unless (send (send self 'text-storage) 'editing-in-progress)
    (let* ((d (hemlock-buffer-string-cache (send self 'string)))
	 (point (hemlock::buffer-point (buffer-cache-buffer d)))
	 (location (pref r :<NSR>ange.location))
	 (len (pref r :<NSR>ange.length)))
    (when (eql len 0)
      #+debug
      (#_NSLog #@"Moving point to absolute position %d" :int location)
      (move-hemlock-mark-to-absolute-position point d location))))
  (send-super :set-selected-range r
	      :affinity affinity
	      :still-selecting still-selecting))



;;; Modeline-view

;;; The modeline view is embedded in the horizontal scroll bar of the
;;; scrollview which surrounds the textview in a pane.  (A view embedded
;;; in a scrollbar like this is sometimes called a "placard").  Whenever
;;; the view's invalidated, its drawRect: method draws a string containing
;;; the current values of the buffer's modeline fields.

(defclass modeline-view (ns:ns-view)
    ((pane :foreign-type :id :accessor modeline-view-pane))
  (:metaclass ns:+ns-object))


;;; Attributes to use when drawing the modeline fields.  There's no
;;; simple way to make the "placard" taller, so using fonts larger than
;;; about 12pt probably wouldn't look too good.  10pt Courier's a little
;;; small, but allows us to see more of the modeline fields (like the
;;; full pathname) in more cases.

(defloadvar *modeline-text-attributes* nil)

(def-cocoa-default *modeline-font-name* :string "Courier New Bold Italic")
(def-cocoa-default  *modeline-font-size* :float 10.0)


;;; Find the underlying buffer.
(defun buffer-for-modeline-view (mv)
  (let* ((pane (modeline-view-pane mv)))
    (unless (%null-ptr-p pane)
      (let* ((tv (text-pane-text-view pane)))
        (unless (%null-ptr-p tv)
	  (text-view-buffer tv))))))

;;; Draw a string in the modeline view.  The font and other attributes
;;; are initialized lazily; apparently, calling the Font Manager too
;;; early in the loading sequence confuses some Carbon libraries that're
;;; used in the event dispatch mechanism,
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
                      (hi::buffer-modeline-fields buffer)))))
	(send (%make-nsstring string)
	      :draw-at-point (ns-make-point 0.0f0 0.0f0)
	      :with-attributes *modeline-text-attributes*)))))

;;; Draw the underlying buffer's modeline string on a white background
;;; with a bezeled border around it.
(define-objc-method ((:void :draw-rect (:<NSR>ect rect)) 
                     modeline-view)
  (declare (ignore rect))
  (slet ((frame (send self 'bounds)))
     (#_NSDrawWhiteBezel frame frame)
     (draw-modeline-string self)))

;;; Hook things up so that the modeline is updated whenever certain buffer
;;; attributes change.
(hi::%init-mode-redisplay)


;;; Modeline-scroll-view

;;; This is just an NSScrollView that draws a "placard" view (the modeline)
;;; in the horizontal scrollbar.  The modeline's arbitrarily given the
;;; leftmost 75% of the available real estate.
(defclass modeline-scroll-view (ns:ns-scroll-view)
    ((modeline :foreign-type :id :accessor scroll-view-modeline)
     (pane :foreign-type :id :accessor scroll-view-pane))
  (:metaclass ns:+ns-object))

;;; Making an instance of a modeline scroll view instantiates the
;;; modeline view, as well.

(define-objc-method ((:id :init-with-frame (:<NSR>ect frame))
                     modeline-scroll-view)
    (let* ((v (send-super :init-with-frame frame)))
      (when v
        (let* ((modeline (make-objc-instance 'modeline-view)))
          (send v :add-subview modeline)
          (setf (scroll-view-modeline v) modeline)))
      v))

;;; Scroll views use the "tile" method to lay out their subviews.
;;; After the next-method has done so, steal some room in the horizontal
;;; scroll bar and place the modeline view there.

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


;;; Text-pane

;;; The text pane is just an NSBox that (a) provides a draggable border
;;; around (b) encapsulates the text view and the mode line.

(defclass text-pane (ns:ns-box)
    ((text-view :foreign-type :id :accessor text-pane-text-view)
     (mode-line :foreign-type :id :accessor text-pane-mode-line)
     (scroll-view :foreign-type :id :accessor text-pane-scroll-view))
  (:metaclass ns:+ns-object))

;;; Mark the pane's modeline as needing display.  This is called whenever
;;; "interesting" attributes of a buffer are changed.

(defun hi::invalidate-modeline (pane)
  (send (text-pane-mode-line pane) :set-needs-display t))

(def-cocoa-default *text-pane-margin-width* :float 0.0f0 "width of indented margin around text pane")
(def-cocoa-default *text-pane-margin-height* :float 0.0f0 "height of indented margin around text pane")


(define-objc-method ((:id :init-with-frame (:<NSR>ect frame))
                     text-pane)
    (let* ((pane (send-super :init-with-frame frame)))
      (unless (%null-ptr-p pane)
        (send pane :set-autoresizing-mask (logior
                                           #$NSViewWidthSizable
                                           #$NSViewHeightSizable))
        (send pane :set-box-type #$NSBoxPrimary)
        (send pane :set-border-type #$NSNoBorder)
        (send pane :set-content-view-margins (ns-make-size *text-pane-margin-width* *text-pane-margin-height*))
        (send pane :set-title-position #$NSNoTitle))
      pane))


(defun make-scrolling-text-view-for-textstorage (textstorage x y width height tracks-width color)
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
	    (let* ((tv (send (make-objc-instance 'hemlock-text-view
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
              (send tv :set-background-color color)
	      (send container :set-width-tracks-text-view tracks-width)
	      (send container :set-height-tracks-text-view nil)
	      (send scrollview :set-document-view tv)	      
	      (values tv scrollview))))))))

(defun make-scrolling-textview-for-pane (pane textstorage track-width color)
  (slet ((contentrect (send (send pane 'content-view) 'frame)))
    (multiple-value-bind (tv scrollview)
	(make-scrolling-text-view-for-textstorage
	 textstorage
	 (pref contentrect :<NSR>ect.origin.x)
	 (pref contentrect :<NSR>ect.origin.y)
	 (pref contentrect :<NSR>ect.size.width)
	 (pref contentrect :<NSR>ect.size.height)
	 track-width
         color)
      (send pane :set-content-view scrollview)
      (setf (slot-value pane 'scroll-view) scrollview
            (slot-value pane 'text-view) tv
            (slot-value tv 'pane) pane
            (slot-value scrollview 'pane) pane)
      (let* ((modeline  (scroll-view-modeline scrollview)))
        (setf (slot-value pane 'mode-line) modeline
              (slot-value modeline 'pane) pane))
      tv)))


(defmethod hi::activate-hemlock-view ((view text-pane))
  (let* ((hemlock-frame (send view 'window))
	 (text-view (text-pane-text-view view)))
    (send hemlock-frame :make-first-responder text-view)))


(defclass echo-area-view (hemlock-textstorage-text-view)
    ()
  (:metaclass ns:+ns-object))

(defmethod hi::activate-hemlock-view ((view echo-area-view))
  (let* ((hemlock-frame (send view 'window)))
    (send hemlock-frame :make-first-responder view)))

(defmethod text-view-buffer ((self echo-area-view))
  (buffer-cache-buffer (hemlock-buffer-string-cache (send (send self 'text-storage) 'string))))

;;; The "document" for an echo-area isn't a real NSDocument.
(defclass echo-area-document (ns:ns-object)
    ((textstorage :foreign-type :id))
  (:metaclass ns:+ns-object))

(define-objc-method ((:void :update-change-count (:<NSD>ocument<C>hange<T>ype change)) echo-area-document)
  (declare (ignore change)))

(define-objc-method ((:void :key-down event)
		     echo-area-view)
  (pass-key-down-event-to-hemlock self event))


(defloadvar *hemlock-frame-count* 0)

(defun make-echo-area (hemlock-frame x y width height gap-context color)
  (slet ((frame (ns-make-rect x y width height))
	 (containersize (ns-make-size 1.0f7 height)))
    (let* ((buffer (hi:make-buffer (format nil "Echo Area ~d"
					   (prog1
					       *hemlock-frame-count*
					     (incf *hemlock-frame-count*)))
				   :modes '("Echo Area")))
	   (textstorage
	    (progn
	      (setf (hi::buffer-gap-context buffer) gap-context)
	      (make-textstorage-for-hemlock-buffer buffer)))
	   (doc (make-objc-instance 'echo-area-document))
	   (layout (make-objc-instance 'ns-layout-manager))
	   (container (send (make-objc-instance 'ns-text-container
						:with-container-size
						containersize)
			    'autorelease)))
      (send textstorage :add-layout-manager layout)
      (send layout :add-text-container container)
      (send layout 'release)
      (let* ((echo (make-objc-instance 'echo-area-view
				       :with-frame frame
				       :text-container container)))
	(send echo :set-min-size (ns-make-size 0.0f0 height))
	(send echo :set-max-size (ns-make-size 1.0f7 1.0f7))
	(send echo :set-rich-text nil)
	(send echo :set-horizontally-resizable nil)
	(send echo :set-vertically-resizable nil)
	(send echo :set-autoresizing-mask #$NSViewWidthSizable)
        (send echo :set-background-color color)
	(send container :set-width-tracks-text-view nil)
	(send container :set-height-tracks-text-view nil)
	(setf (hemlock-frame-echo-area-buffer hemlock-frame) buffer
	      (slot-value doc 'textstorage) textstorage
	      (hi::buffer-document buffer) doc)
	
	echo))))
		    
(defun make-echo-area-for-window (w gap-context-for-echo-area-buffer color)
  (let* ((content-view (send w 'content-view)))
    (slet ((bounds (send content-view 'bounds)))
      (let* ((echo-area (make-echo-area w 7.0f0 5.0f0 (- (pref bounds :<NSR>ect.size.width) 29.0f0) 15.0f0 gap-context-for-echo-area-buffer color)))
	(send content-view :add-subview echo-area)
	echo-area))))
               
(defclass hemlock-frame (ns:ns-window)
    ((echo-area-view :foreign-type :id)
     (event-queue :initform (ccl::init-dll-header (hi::make-frame-event-queue))
                  :reader hemlock-frame-event-queue)
     (command-thread :initform nil)
     (echo-area-buffer :initform nil :accessor hemlock-frame-echo-area-buffer)
     (echo-area-stream :initform nil :accessor hemlock-frame-echo-area-stream))
  (:metaclass ns:+ns-object))

(defun double-%-in (string)
  ;; Replace any % characters in string with %%, to keep them from
  ;; being treated as printf directives.
  (let* ((%pos (position #\% string)))
    (if %pos
      (concatenate 'string (subseq string 0 %pos) "%%" (double-%-in (subseq string (1+ %pos))))
      string)))

(defun nsstring-for-lisp-condition (cond)
  (%make-nsstring (double-%-in (princ-to-string cond))))

(define-objc-method ((:void :run-error-sheet info) hemlock-frame)
  (let* ((message (send info :object-at-index 0))
         (signal (send info :object-at-index 1)))
    (#_NSBeginAlertSheet #@"Error in Hemlock command processing" ;title
                         (if (logbitp 0 (random 2))
                           #@"Not OK, but what can you do?"
                           #@"The sky is falling. FRED never did this!")
                         (%null-ptr)
                         (%null-ptr)
                         self
                         self
                         (@selector "sheetDidEnd:returnCode:contextInfo:")
                         (@selector "sheetDidDismiss:returnCode:contextInfo:")
                         signal
                         message)))

(define-objc-method ((:void :sheet-did-end sheet
                            :return-code code
                            :context-info info)
                     hemlock-frame)
 (declare (ignore sheet code info)))

(define-objc-method ((:void :sheet-did-dismiss sheet
                            :return-code code
                            :context-info info)
                     hemlock-frame)
  (declare (ignore sheet code))
  (ccl::%signal-semaphore-ptr (%int-to-ptr (send info 'unsigned-int-value))))
  
(defun report-condition-in-hemlock-frame (condition frame)
  (let* ((semaphore (make-semaphore))
         (message (nsstring-for-lisp-condition condition))
         (sem-value (make-objc-instance 'ns:ns-number
                                        :with-unsigned-int (%ptr-to-int (semaphore.value semaphore)))))
    (%stack-block ((paramptrs (ash 2 target::word-shift)))
      (setf (%get-ptr paramptrs 0) message
            (%get-ptr paramptrs (ash 1 target::word-shift)) sem-value)
      (let* ((params (make-objc-instance 'ns:ns-array
                                         :with-objects paramptrs
                                         :count 2)))
        (send frame
              :perform-selector-on-main-thread
              (@selector "runErrorSheet:")
              :with-object params
              :wait-until-done t)
        (wait-on-semaphore semaphore)))))

(defun hi::report-hemlock-error (condition)
  (report-condition-in-hemlock-frame condition (send (hi::current-window) 'window)))
                       
                       
(defun hemlock-thread-function (q buffer pane echo-buffer echo-window)
  (let* ((hi::*real-editor-input* q)
         (hi::*editor-input* q)
         (hi::*current-buffer* hi::*current-buffer*)
         (hi::*current-window* pane)
         (hi::*echo-area-window* echo-window)
         (hi::*echo-area-buffer* echo-buffer)
         (region (hi::buffer-region echo-buffer))
         (hi::*echo-area-region* region)
         (hi::*echo-area-stream* (hi::make-hemlock-output-stream
                              (hi::region-end region) :full))
	 (hi::*parse-starting-mark*
	  (hi::copy-mark (hi::buffer-point hi::*echo-area-buffer*)
			 :right-inserting))
	 (hi::*parse-input-region*
	  (hi::region hi::*parse-starting-mark*
		      (hi::region-end region)))
         (hi::*cache-modification-tick* -1)
         (hi::now-tick 0)
         (hi::*disembodied-buffer-counter* 0)
         (hi::*in-a-recursive-edit* nil)
         (hi::*last-key-event-typed* nil)
         (hi::*input-transcript* nil)
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
         (hemlock::*target-column* 0)
         (hemlock::*last-comment-start* 0)
         (hemlock::*last-search-string* ())
         (hemlock::*last-search-pattern*
            (hemlock::new-search-pattern :string-insensitive :forward "Foo"))
         )
    
    (setf (hi::current-buffer) buffer)
    (unwind-protect
         (loop
           (catch 'hi::editor-top-level-catcher
             (handler-bind ((error #'(lambda (condition)
                                       (hi::lisp-error-error-handler condition
                                                                     :internal))))
               (hi::invoke-hook hemlock::abort-hook)
               (hi::%command-loop))))
      (hi::invoke-hook hemlock::exit-hook))))


(define-objc-method ((:void close) hemlock-frame)
  (let* ((proc (slot-value self 'command-thread)))
    (when proc
      (setf (slot-value self 'command-thread) nil)
      (process-kill proc)))
  (send-super 'close))
  
(defun new-hemlock-document-window ()
  (let* ((w (new-cocoa-window :class (find-class 'hemlock-frame)
                              :activate nil)))
      (values w (add-pane-to-window w :reserve-below 20.0))))



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


	  
					
				      
(defun textpane-for-textstorage (ts ncols nrows container-tracks-text-view-width color)
  (let* ((pane (nth-value
                1
                (new-hemlock-document-window)))
         (tv (make-scrolling-textview-for-pane pane ts container-tracks-text-view-width color)))
    (multiple-value-bind (height width)
        (size-of-char-in-font (default-font))
      (size-textview-containers tv height width nrows ncols))
    pane))




(defun hemlock-buffer-from-nsstring (nsstring name &rest modes)
  (let* ((buffer (make-hemlock-buffer name :modes modes)))
    (nsstring-to-buffer nsstring buffer)))

(defun nsstring-to-buffer (nsstring buffer)
  (let* ((document (hi::buffer-document buffer))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer)))
    (setf (hi::buffer-document buffer) nil)
    (unwind-protect
	 (progn
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
					       :macos))
				   (t :unix)))))
		       (if (eq previous first-line)
			 (progn
			   (hi::insert-string mark chars)
			   (hi::insert-character mark #\newline)
			   (setq first-line nil))
			 (if (eq string-len contents-end)
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
	   (hi::buffer-start (hi::buffer-point buffer))
	   buffer)
      (setf (hi::buffer-document buffer) document))))

(setq hi::*beep-function* #'(lambda (stream)
			      (declare (ignore stream))
			      (#_NSBeep)))


;;; This function must run in the main event thread.
(defun %hemlock-frame-for-textstorage (ts ncols nrows container-tracks-text-view-width color)
  (let* ((pane (textpane-for-textstorage ts ncols nrows container-tracks-text-view-width color))
         (frame (send pane 'window))
         (buffer (text-view-buffer (text-pane-text-view pane))))
      (setf (slot-value frame 'echo-area-view)
            (make-echo-area-for-window frame (hi::buffer-gap-context buffer) color))
    (setf (slot-value frame 'command-thread)
          (process-run-function (format nil "Hemlock window thread")
                                #'(lambda ()
                                    (hemlock-thread-function
                                     (hemlock-frame-event-queue frame)
                                     buffer
                                     pane
                                     (hemlock-frame-echo-area-buffer frame)
                                     (slot-value frame 'echo-area-view)))))
    frame))
         
    


(defun hemlock-frame-for-textstorage (ts ncols nrows container-tracks-text-view-width color)
  (process-interrupt *cocoa-event-process*
                     #'%hemlock-frame-for-textstorage
                     ts  ncols nrows container-tracks-text-view-width color))



(defun hi::lock-buffer (b)
  (grab-lock (hi::buffer-gap-context-lock (hi::buffer-gap-context b))))

(defun hi::unlock-buffer (b)
  (release-lock (hi::buffer-gap-context-lock (hi::buffer-gap-context b)))) 
  
(defun hi::document-begin-editing (document)
  (send (slot-value document 'textstorage)
        :perform-selector-on-main-thread
        (@selector "beginEditing")
        :with-object (%null-ptr)
        :wait-until-done t))



(defun hi::document-end-editing (document)
  (send (slot-value document 'textstorage)
        :perform-selector-on-main-thread
        (@selector "endEditing")
        :with-object (%null-ptr)
        :wait-until-done t))

(defun hi::document-set-point-position (document)
  (declare (ignorable document))
  #+debug
  (#_NSLog #@"Document set point position called")
  (let* ((textstorage (slot-value document 'textstorage)))
    (send textstorage
          :perform-selector-on-main-thread
          (@selector "updateHemlockSelection")
          :with-object (%null-ptr)
          :wait-until-done t)))



(defun perform-edit-change-notification (textstorage selector pos n)
  (let* ((number-for-pos
          (send (send (@class "NSNumber") 'alloc)
                :init-with-int pos))
         (number-for-n 
          (send (send (@class "NSNumber") 'alloc)
                :init-with-int n)))
    (%stack-block ((paramptrs (ash 2 target::word-shift)))
      (setf (%get-ptr paramptrs 0) number-for-pos
            (%get-ptr paramptrs (ash 1 target::word-shift))
            number-for-n)
      (let* ((params (make-objc-instance 'ns:ns-array
                                         :with-objects paramptrs
                                         :count 2)))
        (send textstorage
                    :perform-selector-on-main-thread
                    selector
                    :with-object params
                    :wait-until-done t)
              (send params 'release)
              (send number-for-pos 'release)
              (send number-for-n 'release)))))

(defun textstorage-note-insertion-at-position (textstorage pos n)
  #+debug
  (#_NSLog #@"insertion at position %d, len %d" :int pos :int n)
  (send textstorage
	:edited #$NSTextStorageEditedAttributes
	:range (ns-make-range pos 0)
	:change-in-length n)
  (send textstorage
	:edited #$NSTextStorageEditedCharacters
	:range (ns-make-range pos n)
	:change-in-length 0))




	  
(defun hi::buffer-note-insertion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        (let* ((pos (mark-absolute-position mark)))
          (unless (eq (hi::mark-%kind mark) :right-inserting)
            (decf pos n))
          #+debug
	  (format t "~&insert: pos = ~d, n = ~d" pos n)
          (let* ((display (hemlock-buffer-string-cache (send textstorage 'string))))
            (reset-buffer-cache display) 
            (update-line-cache-for-index display pos))
          (perform-edit-change-notification textstorage
                                            (@selector "noteInsertion:")
                                            pos
                                            n))))))

(defun hi::buffer-note-modification (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        #+debug
        (#_NSLog #@"enqueue modify: pos = %d, n = %d"
                 :int (mark-absolute-position mark)
                 :int n)
        (perform-edit-change-notification textstorage
                                          (@selector "noteModification:")
                                          (mark-absolute-position mark)
                                          n)))))
  

(defun hi::buffer-note-deletion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        (perform-edit-change-notification textstorage
                                          (@selector "noteDeletion:")
                                          (mark-absolute-position mark)
                                          (abs n))))))
(defun hi::set-document-modified (document flag)
  (send document
	:update-change-count (if flag #$NSChangeDone #$NSChangeCleared)))


(defmethod hi::document-panes ((document t))
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
				    
  
(defclass hemlock-editor-window-controller (ns:ns-window-controller)
    ()
  (:metaclass ns:+ns-object))

    


;;; The HemlockEditorDocument class.


(defclass hemlock-editor-document (ns:ns-document)
    ((textstorage :foreign-type :id))
  (:metaclass ns:+ns-object))

(defmethod textview-background-color ((doc hemlock-editor-document))
  (send (find-class 'ns:ns-color)
        :color-with-calibrated-red *editor-background-red-component*
        :green *editor-background-green-component*
        :blue *editor-background-blue-component*
        :alpha *editor-background-alpha-component*))


(define-objc-method ((:id init) hemlock-editor-document)
  (let* ((doc (send-super 'init)))
    (unless (%null-ptr-p doc)
      (let* ((buffer (make-hemlock-buffer
		      (lisp-string-from-nsstring (send doc 'display-name))
		      :modes '("Lisp" "Editor"))))
	(setf (slot-value doc 'textstorage)
	      (make-textstorage-for-hemlock-buffer buffer)
	      (hi::buffer-document buffer) doc)))
    doc))
                     

(define-objc-method ((:id :read-from-file filename
			  :of-type type)
		     hemlock-editor-document)
  (declare (ignorable type))
  (let* ((pathname (lisp-string-from-nsstring filename))
	 (buffer-name (hi::pathname-to-buffer-name pathname))
	 (buffer (or
		  (hemlock-document-buffer self)
		  (let* ((b (make-hemlock-buffer buffer-name)))
		    (setf (hi::buffer-pathname b) pathname)
		    (setf (slot-value self 'textstorage)
			  (make-textstorage-for-hemlock-buffer b))
		    b)))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
	 (data (make-objc-instance 'ns:ns-data
				   :with-contents-of-file filename))
	 (string (make-objc-instance 'ns:ns-string
				     :with-data data
				     :encoding #$NSASCIIStringEncoding)))
    (hi::document-begin-editing self)
    (nsstring-to-buffer string buffer)
    (let* ((textstorage (slot-value self 'textstorage))
	   (display (hemlock-buffer-string-cache (send textstorage 'string))))
      (reset-buffer-cache display) 
      (update-line-cache-for-index display 0)
      (textstorage-note-insertion-at-position
       textstorage
       0
       (hemlock-buffer-length buffer)))
    (hi::document-end-editing self)
    (setf (hi::buffer-modified buffer) nil)
    (hi::process-file-options buffer pathname)
    self))
    
  
(defmethod hemlock-document-buffer (document)
  (let* ((string (send (slot-value document 'textstorage) 'string)))
    (unless (%null-ptr-p string)
      (let* ((cache (hemlock-buffer-string-cache string)))
	(when cache (buffer-cache-buffer cache))))))

(defmethod hi::document-panes ((document hemlock-editor-document))
  (let* ((ts (slot-value document 'textstorage))
	 (panes ()))
    (for-each-textview-using-storage
     ts
     #'(lambda (tv)
	 (let* ((pane (text-view-pane tv)))
	   (unless (%null-ptr-p pane)
	     (push pane panes)))))
    panes))

(define-objc-method ((:id :data-representation-of-type type)
		      hemlock-editor-document)
  (declare (ignorable type))
  (let* ((buffer (hemlock-document-buffer self)))
    (when buffer
      (setf (hi::buffer-modified buffer) nil)))
  (send (send (slot-value self 'textstorage) 'string)
	:data-using-encoding #$NSASCIIStringEncoding
	:allow-lossy-conversion t))


;;; Shadow the setFileName: method, so that we can keep the buffer
;;; name and pathname in synch with the document.
(define-objc-method ((:void :set-file-name full-path)
		     hemlock-editor-document)
  (send-super :set-file-name full-path)
  (let* ((buffer (hemlock-document-buffer self)))
    (when buffer
      (let* ((new-pathname (lisp-string-from-nsstring full-path)))
	(setf (hi::buffer-name buffer) (hi::pathname-to-buffer-name new-pathname))
	(setf (hi::buffer-pathname buffer) new-pathname)))))
  
(define-objc-method ((:void make-window-controllers) hemlock-editor-document)
  #+debug
  (#_NSLog #@"Make window controllers")
  (let* ((controller (make-objc-instance
		      'hemlock-editor-window-controller
		      :with-window (%hemlock-frame-for-textstorage 
                                    (slot-value self 'textstorage)
				    *editor-columns*
				    *editor-rows*
				    nil
                                    (textview-background-color self)))))
    (send self :add-window-controller controller)
    (send controller 'release)))	 


(define-objc-method ((:void close) hemlock-editor-document)
  (let* ((textstorage (slot-value self 'textstorage)))
    (setf (slot-value self 'textstorage) (%null-ptr))
    (unless (%null-ptr-p textstorage)
      (close-hemlock-textstorage textstorage)))
    (send-super 'close))


(defun initialize-user-interface ()
  (update-cocoa-defaults)
  (make-editor-style-map))

(defun hi::scroll-window (textpane n)
  (let* ((textview (text-pane-text-view textpane)))
    (unless (%null-ptr-p textview)
      (if (> n 0)
        (send textview :page-down nil)
        (send textview :page-up nil)))))

;;; This needs to run on the main thread.
(define-objc-method ((void update-hemlock-selection)
                     hemlock-text-storage)
  (let* ((string (send self 'string))
         (buffer (buffer-cache-buffer (hemlock-buffer-string-cache string)))
         (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
         (point (hi::buffer-point buffer))
         (pos (mark-absolute-position point))
         (len 0))
    #+debug
    (#_NSLog #@"update Hemlock selection: charpos = %d, abspos = %d"
             :int (hi::mark-charpos point) :int pos)
    (for-each-textview-using-storage
     self
     #'(lambda (tv)
         (send tv
               :update-selection pos
               :length len
               :affinity #$NSSelectionAffinityUpstream)))))


(defun hi::allocate-temporary-object-pool ()
  (create-autorelease-pool))

(defun hi::free-temporary-objects (pool)
  (release-autorelease-pool pool))

(provide "COCOA-EDITOR")
