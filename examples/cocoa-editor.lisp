;;;-*- Mode: LISP; Package: CCL -*-


(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA-WINDOW")
  (require "HEMLOCK"))

(eval-when (:compile-toplevel :execute)
  ;; :ALL-IN-COCOA-THREAD selects code that does all rendering
  ;; in the Cocoa event thread.
  ;; Something else that could be conditionalized (and might
  ;; be similarly named) would force all Hemlock commands -
  ;; as well as rendering and event handling - to happen in
  ;; the Cocoa thread.
  (pushnew :all-in-cocoa-thread *features*)
  (use-interface-dir :cocoa))

(def-cocoa-default *editor-rows* :int 24 "Initial height of editor windows, in characters")
(def-cocoa-default *editor-columns* :int 80 "Initial width of editor windows, in characters")

;;; Background color components: red, blue, green, alpha.
;;; All should be single-floats between 0.0f0 and 1.0f0, inclusive.
(def-cocoa-default *editor-background-red-component* :float 1.0f0 "Red component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")
(def-cocoa-default *editor-background-green-component* :float 1.0f0 "Green component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")
(def-cocoa-default *editor-background-blue-component* :float 1.0f0 "Blue component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")
(def-cocoa-default *editor-background-alpha-component* :float 1.0f0 "Alpha component of editor background color.  Should be a float between 0.0 and 1.0, inclusive.")

;;; At runtime, this'll be a vector of character attribute dictionaries.
(defloadvar *styles* ())

(defun make-editor-style-map ()
  (let* ((font-name *default-font-name*)
	 (font-size *default-font-size*)
         (font (default-font :name font-name :size font-size))
	 (color-class (find-class 'ns:ns-color))
	 (colors (vector (send color-class 'black-color)
			 (send color-class 'white-color)
			 (send color-class 'dark-gray-color)
			 (send color-class 'light-gray-color)
			 (send color-class 'red-color)
			 (send color-class 'blue-color)
			 (send color-class 'green-color)
			 (send color-class 'yellow-color)))
	 (styles (make-array (the fixnum (* 4 (length colors)))))
         (bold-stroke-width 9.0f0)
	 (s 0))
    (declare (dynamic-extent fonts colors))
    (dotimes (c (length colors))
      (dotimes (i 4)
	(setf (svref styles s) (create-text-attributes :font font
						       :color (svref colors c)
                                                       :obliqueness
                                                       (if (logbitp 1 i)
                                                         0.15f0)
                                                       :stroke-width
                                                       (if (logbitp 0 i)
                                                         bold-stroke-width)))
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
	;;(dbg)
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


(defun adjust-buffer-cache-for-insertion (display pos n)
  (if (buffer-cache-workline display)
    (let* ((hi::*buffer-gap-context* (hi::buffer-gap-context (buffer-cache-buffer display))))
      (if (> (buffer-cache-workline-offset display) pos)
        (incf (buffer-cache-workline-offset display) n)
        (when (>= (+ (buffer-cache-workline-offset display)
                    (buffer-cache-workline-length display))
                 pos)
          (setf (buffer-cache-workline-length display)
                (hi::line-length (buffer-cache-workline display)))))
      (incf (buffer-cache-buflen display) n))
    (reset-buffer-cache display)))

          
           

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
  (let* ((pos (hi::mark-charpos mark))
         (hi::*buffer-gap-context* (hi::buffer-gap-context (hi::line-%buffer
                                                            (hi::mark-line mark)))))
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
  #+debug
  (#_NSLog #@"Character at index: %d" :unsigned index)
  (char-code (hemlock-char-at-index (hemlock-buffer-string-cache self) index)))


(define-objc-method ((:void :get-characters ((:* :unichar) buffer) :range (:<NSR>ange r))
                     hemlock-buffer-string)
  (let* ((cache (hemlock-buffer-string-cache self))
         (index (pref r :<NSR>ange.location))
         (length (pref r :<NSR>ange.length))
         (hi::*buffer-gap-context*
	  (hi::buffer-gap-context (buffer-cache-buffer cache))))
    #+debug
    (#_NSLog #@"get characters: %d/%d"
             :unsigned index
             :unsigned length)
    (multiple-value-bind (line idx) (update-line-cache-for-index cache index)
      (let* ((len (hemlock::line-length line)))
        (do* ((i 0 (1+ i))
              (p 0 (+ p 2)))
             ((= i length))
          (cond ((< idx len)
                 (setf (%get-unsigned-word buffer p)
                       (char-code (hemlock::line-character line idx)))
                 (incf idx))
                (t
                 (setf (%get-unsigned-word buffer p)
                       (char-code #\Newline)
                       line (hi::line-next line)
                       len (hi::line-length line)
                  idx 0))))))))

(define-objc-method ((:void :get-line-start ((:* :unsigned) startptr)
                            :end ((:* :unsigned) endptr)
                            :contents-end ((:* :unsigned) contents-endptr)
                            :for-range (:<NSR>ange r))
                     hemlock-buffer-string)
  (let* ((cache (hemlock-buffer-string-cache self))
         (index (pref r :<NSR>ange.location))
         (length (pref r :<NSR>ange.length))
         (hi::*buffer-gap-context*
	  (hi::buffer-gap-context (buffer-cache-buffer cache))))
    #+debug 0
    (#_NSLog #@"get line start: %d/%d"
             :unsigned index
             :unsigned length)
    (update-line-cache-for-index cache index)
    (unless (%null-ptr-p startptr)
      ;; Index of the first character in the line which contains
      ;; the start of the range.
      (setf (pref startptr :unsigned)
            (buffer-cache-workline-offset cache)))
    (unless (%null-ptr-p endptr)
      ;; Index of the newline which terminates the line which
      ;; contains the start of the range.
      (setf (pref endptr :unsigned)
            (+ (buffer-cache-workline-offset cache)
               (buffer-cache-workline-length cache))))
    (unless (%null-ptr-p contents-endptr)
      ;; Index of the newline which terminates the line which
      ;; contains the start of the range.
      (unless (zerop length)
        (update-line-cache-for-index cache (+ index length)))
      (setf (pref contents-endptr :unsigned)
            (1+ (+ (buffer-cache-workline-offset cache)
                   (buffer-cache-workline-length cache)))))))

                     
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
    (hi::%set-buffer-modified buffer nil)
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
                   (%cstr-pointer chars (%inc-ptr bytes pos) nil)
		   (incf pos len))
		 (when next
		   (when cp/m-p
                     (setf (%get-byte bytes pos) (char-code #\return))
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
     (edit-count :foreign-type :int)
     (append-edits :foreign-type :int))
  (:metaclass ns:+ns-object))

(define-objc-method ((:unsigned :line-break-before-index (:unsigned index)
                                :within-range (:<NSR>ange r))
                     hemlock-text-storage)
  (#_NSLog #@"Line break before index: %d within range: %@"
           :unsigned index
           :id (#_NSStringFromRange r))
  (send-super :line-break-before-index index :within-range r))



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

(define-objc-method ((:void :note-attr-change params) hemlock-text-storage)
  (let* ((pos (send (send params :object-at-index 0) 'int-value))
         (n (send (send params :object-at-index 1) 'int-value)))
    #+debug (#_NSLog #@"attribute-change at %d/%d" :int pos :int n)
    (send self
          :edited #$NSTextStorageEditedAttributes
          :range (ns-make-range pos n)
          :change-in-length 0)))

(define-objc-method ((:void begin-editing) hemlock-text-storage)
  #+debug
  (#_NSLog #@"begin-editing")
  (incf (slot-value self 'edit-count))
  #+debug
  (#_NSLog #@"after beginEditing edit-count now = %d" :int (slot-value self 'edit-count))
  (send-super 'begin-editing))

(define-objc-method ((:void end-editing) hemlock-text-storage)
  #+debug
  (#_NSLog #@"end-editing")
  (send-super 'end-editing)
  (decf (slot-value self 'edit-count))
  #+debug
  (#_NSLog #@"after endEditing edit-count now = %d" :int (slot-value self 'edit-count)))

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

(define-objc-method ((:id :attributes-at-index (:unsigned index)
			  :effective-range ((* :<NSR>ange) rangeptr))
		     hemlock-text-storage)
  #+debug
  (#_NSLog #@"Attributes at index: %d" :unsigned index)
  (let* ((buffer-cache (hemlock-buffer-string-cache (slot-value self 'string)))
	 (buffer (buffer-cache-buffer buffer-cache))
         (hi::*buffer-gap-context* (hi::buffer-gap-context buffer)))
    (update-line-cache-for-index buffer-cache index)
    (multiple-value-bind (start len style)
        (ccl::do-dll-nodes (node
                            (hi::buffer-font-regions buffer)
                            (values 0 (buffer-cache-buflen buffer-cache) 0))
          (let* ((region (hi::font-region-node-region node))
                 (start (hi::region-start region))
                 (end (hi::region-end region))
                 (startpos (mark-absolute-position start))
                 (endpos (mark-absolute-position end)))
            (when (and (>= index startpos)
                       (< index endpos))
              (return (values startpos
                              (- endpos startpos)
                              (hi::font-mark-font start))))))
      #+debug
      (#_NSLog #@"Start = %d, len = %d, style = %d"
               :int start :int len :int style)
      (unless (%null-ptr-p rangeptr)
        (setf (pref rangeptr :<NSR>ange.location) start
              (pref rangeptr :<NSR>ange.length) len))
      (svref *styles* style))))

(define-objc-method ((:void :replace-characters-in-range (:<NSR>ange r)
  			    :with-string string)
  		     hemlock-text-storage)
  (let* ((cache (hemlock-buffer-string-cache (send self 'string)))
	 (buffer (if cache (buffer-cache-buffer cache)))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
	 (location (pref r :<NSR>ange.location))
	 (length (pref r :<NSR>ange.length))
	 (mark (hi::buffer-%mark buffer))
	 (point (hi::buffer-point buffer))
	 input-mark)

    ;;
    ;; special behavior for listener windows.
    ;;
    (if (and (> (slot-value self 'append-edits) 0)
	     (progn
	       (setf input-mark (hi::variable-value 'hemlock::buffer-input-mark :buffer buffer))
	       (not (hi::same-line-p point input-mark))))
	(progn
	  ;;
	  ;;  move the point to the end of the buffer
	  ;;
          (setf (hi::buffer-region-active buffer) nil)
	  (move-hemlock-mark-to-absolute-position point cache (hemlock-buffer-length buffer)))
      (cond ((> length 0)
	     (move-hemlock-mark-to-absolute-position mark cache location)
	     (move-hemlock-mark-to-absolute-position point cache (+ location length))
	     (hemlock::%buffer-activate-region buffer))
	    (t
	     (move-hemlock-mark-to-absolute-position point cache location))))
    (hi::insert-string point (lisp-string-from-nsstring string))))


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
    ((blink-location :foreign-type :unsigned :accessor text-view-blink-location)
     (blink-color-attribute :foreign-type :id :accessor text-view-blink-color)
     (blink-enabled :foreign-type :<BOOL> :accessor text-view-blink-enabled) )
  (:metaclass ns:+ns-object))


(def-cocoa-default *layout-text-in-background* :int 1 "When non-zero, do text layout when idle.")

(define-objc-method ((:void :layout-manager layout
                            :did-complete-layout-for-text-container cont
                            :at-end (:<BOOL> flag))
                     hemlock-textstorage-text-view)
  (declare (ignore cont))
  (when (zerop *layout-text-in-background*)
    (send layout :set-delegate (%null-ptr))
    (send layout :set-background-layout-enabled nil)))
    
;;; Note changes to the textview's background color; record them
;;; as the value of the "temporary" foreground color (for blinking).
(define-objc-method ((:void :set-background-color color)
                     hemlock-textstorage-text-view)
  (setf (text-view-blink-color self) color)
  (send-super :set-background-color color))

;;; Maybe cause 1 character in the textview to blink (by drawing an empty
;;; character rectangle) in synch with the insertion point.

(define-objc-method ((:void :draw-insertion-point-in-rect (:<NSR>ect r)
                            :color color
                            :turned-on (:<BOOL> flag))
                     hemlock-textstorage-text-view)
  (unless (send (send self 'text-storage) 'editing-in-progress)
    (unless (eql #$NO (text-view-blink-enabled self))
      (let* ((layout (send self 'layout-manager))
             (container (send self 'text-container))
             (blink-color (text-view-blink-color self)))
        ;; We toggle the blinked character "off" by setting its
        ;; foreground color to the textview's background color.
        ;; The blinked character should be "on" whenever the insertion
        ;; point is drawn as "off"
        (slet ((glyph-range
                (send layout
                      :glyph-range-for-character-range
                      (ns-make-range (text-view-blink-location self) 1)
                      :actual-character-range (%null-ptr))))
          #+debug (#_NSLog #@"Flag = %d, location = %d" :<BOOL> (if flag #$YES #$NO) :int (text-view-blink-location self))
          (slet ((rect (send layout
                             :bounding-rect-for-glyph-range glyph-range
                             :in-text-container container)))
            (send (the ns:ns-color blink-color) 'set)
            (#_NSRectFill rect))
          (if flag
            (send layout
                  :draw-glyphs-for-glyph-range glyph-range
                  :at-point  (send self 'text-container-origin)))
          )))
    (send-super :draw-insertion-point-in-rect r
                :color color
                :turned-on flag)))
                
(defmethod disable-blink ((self hemlock-textstorage-text-view))
  (when (eql (text-view-blink-enabled self) #$YES)
    (setf (text-view-blink-enabled self) #$NO)
    ;; Force the blinked character to be redrawn.  Let the text
    ;; system do the drawing.
    (let* ((layout (send self 'layout-manager)))
      (send layout :invalidate-display-for-character-range
            (ns-make-range (text-view-blink-location self) 1)))))

(defmethod update-blink ((self hemlock-textstorage-text-view))
  (disable-blink self)
  (let* ((d (hemlock-buffer-string-cache (send self 'string)))
         (buffer (buffer-cache-buffer d)))
    (when (and buffer (string= (hi::buffer-major-mode buffer) "Lisp"))
      (let* ((hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
             (point (hi::buffer-point buffer)))
        #+debug (#_NSLog #@"Syntax check for blinking")
        (cond ((eql (hi::next-character point) #\()
               (hemlock::pre-command-parse-check point)
               (when (hemlock::valid-spot point nil)
                 (hi::with-mark ((temp point))
                   (when (hemlock::list-offset temp 1)
                     #+debug (#_NSLog #@"enable blink, forward")
                     (setf (text-view-blink-location self)
                           (1- (mark-absolute-position temp))
                           (text-view-blink-enabled self) #$YES)))))
              ((eql (hi::previous-character point) #\))
               (hemlock::pre-command-parse-check point)
               (when (hemlock::valid-spot point nil)
                 (hi::with-mark ((temp point))
                   (when (hemlock::list-offset temp -1)
                     #+debug (#_NSLog #@"enable blink, backward")
                     (setf (text-view-blink-location self)
                           (mark-absolute-position temp)
                           (text-view-blink-enabled self) #$YES))))))))))

;;; Set and display the selection at pos, whose length is len and whose
;;; affinity is affinity.  This should never be called from any Cocoa
;;; event handler; it should not call anything that'll try to set the
;;; underlying buffer's point and/or mark.
(define-objc-method ((:void :update-selection (:int pos)
                            :length (:int len)
                            :affinity (:<NSS>election<A>ffinity affinity))
                     hemlock-textstorage-text-view)
  (when (eql len 0)
    (update-blink self))
  (slet ((range (ns-make-range pos len)))
    (send-super :set-selected-range range
                :affinity affinity
                :still-selecting nil)
    (send self :scroll-range-to-visible range)))
  
;;; A specialized NSTextView. The NSTextView is part of the "pane"
;;; object that displays buffers.
(defclass hemlock-text-view (hemlock-textstorage-text-view)
    ((pane :foreign-type :id :accessor text-view-pane))
  (:metaclass ns:+ns-object))

;;; Access the underlying buffer in one swell foop.
(defmethod text-view-buffer ((self hemlock-text-view))
  (buffer-cache-buffer (hemlock-buffer-string-cache (send (send self 'text-storage) 'string))))

(define-objc-method ((:void :set-string s)
                     hemlock-textstorage-text-view)
  (#_NSLog #@"hemlock-text-view %@ string set to %@" :id self :id s)
  (send-super :set-string s))

(define-objc-method (((:struct :_<NSR>ange r)
                      :selection-range-for-proposed-range (:<NSR>ange proposed)
                      :granularity (:<NSS>election<G>ranularity g))
                     hemlock-textstorage-text-view)
  #+debug
  (#_NSLog #@"Granularity = %d" :int g)
  (block HANDLED
    (let* ((index (pref proposed :<NSR>ange.location))
           (length (pref proposed :<NSR>ange.length)))
      (when (and (eql 0 length)              ; not extending existing selection
                 (not (eql g #$NSSelectByCharacter)))
        (let* ((textstorage (send self 'text-storage))
               (cache (hemlock-buffer-string-cache (send textstorage 'string)))
               (buffer (if cache (buffer-cache-buffer cache))))
          (when (and buffer (string= (hi::buffer-major-mode buffer) "Lisp"))
            (let* ((hi::*buffer-gap-context* (hi::buffer-gap-context buffer)))
              (hi::with-mark ((m1 (hi::buffer-point buffer)))
                (move-hemlock-mark-to-absolute-position m1 cache index)
                (hemlock::pre-command-parse-check m1)
                (when (hemlock::valid-spot m1 nil)
                  (cond ((eql (hi::next-character m1) #\()
                         (hi::with-mark ((m2 m1))
                           (when (hemlock::list-offset m2 1)
                             (setf (pref r :<NSR>ange.location) index
                                   (pref r :<NSR>ange.length)
                                   (- (mark-absolute-position m2) index))
                             (return-from HANDLED nil))))
                        ((eql (hi::previous-character m1) #\))
                         (hi::with-mark ((m2 m1))
                           (when (hemlock::list-offset m2 -1)
                             (setf (pref r :<NSR>ange.location)
                                   (mark-absolute-position m2)
                                   (pref r :<NSR>ange.length)
                                   (- index (mark-absolute-position m2)))
                             (return-from HANDLED nil))))))))))))
    (objc-message-send-super-stret r (super) "selectionRangeForProposedRange:granularity:"
                                   :<NSR>ange proposed
                                   :<NSS>election<G>ranularity g)
    #+debug
    (#_NSLog #@"range = %@, proposed = %@, granularity = %d"
             :address (#_NSStringFromRange r)
             :address (#_NSStringFromRange proposed)
             :<NSS>election<G>ranularity g)))

;;; Translate a keyDown NSEvent to a Hemlock key-event.
(defun nsevent-to-key-event (nsevent)
  (let* ((unmodchars (send nsevent 'characters-ignoring-modifiers))
	 (n (if (%null-ptr-p unmodchars)
	      0
	      (send (the ns:ns-string unmodchars) 'length)))
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
        (hi::enqueue-key-event q (nsevent-to-key-event event))))))

(defun enqueue-buffer-operation (buffer thunk)
  (dolist (w (hi::buffer-windows buffer))
    (let* ((q (hemlock-frame-event-queue (send w 'window)))
           (op (hi::make-buffer-operation :thunk thunk)))
      (hi::event-queue-insert q op))))

  
;;; Process a key-down NSEvent in a Hemlock text view by translating it
;;; into a Hemlock key event and passing it into the Hemlock command
;;; interpreter. 

(define-objc-method ((:void :key-down event)
		     hemlock-text-view)
  (pass-key-down-event-to-hemlock self event))

;;; Update the underlying buffer's point (and "active region", if appropriate.
;;; This is called in response to a mouse click or other event; it shouldn't
;;; be called from the Hemlock side of things.
(define-objc-method ((:void :set-selected-range (:<NSR>ange r)
			    :affinity (:<NSS>election<A>ffinity affinity)
			    :still-selecting (:<BOOL> still-selecting))
		     hemlock-text-view)
  #+debug 
  (#_NSLog #@"Set selected range called: location = %d, length = %d, affinity = %d, still-selecting = %d"
           :int (pref r :<NSR>ange.location)
           :int (pref r :<NSR>ange.length)
           :<NSS>election<A>ffinity affinity
           :<BOOL> (if still-selecting #$YES #$NO))
  #+debug
  (#_NSLog #@"text view string = %@, textstorage string = %@"
           :id (send self 'string)
           :id (send (send self 'text-storage) 'string))
  (unless (send (send self 'text-storage) 'editing-in-progress)
    (let* ((d (hemlock-buffer-string-cache (send self 'string)))
           (buffer (buffer-cache-buffer d))
           (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
           (point (hi::buffer-point buffer))
           (location (pref r :<NSR>ange.location))
           (len (pref r :<NSR>ange.length)))
      (cond ((eql len 0)
             #+debug
             (#_NSLog #@"Moving point to absolute position %d" :int location)
             (setf (hi::buffer-region-active buffer) nil)
             (move-hemlock-mark-to-absolute-position point d location)
             (update-blink self))
            (t
             ;; We don't get much information about which end of the
             ;; selection the mark's at and which end point is at, so
             ;; we have to sort of guess.  In every case I've ever seen,
             ;; selection via the mouse generates a sequence of calls to
             ;; this method whose parameters look like:
             ;; a: range: {n0,0} still-selecting: false  [ rarely repeats ]
             ;; b: range: {n0,0) still-selecting: true   [ rarely repeats ]
             ;; c: range: {n1,m} still-selecting: true   [ often repeats ]
             ;; d: range: {n1,m} still-selecting: false  [ rarely repeats ]
             ;;
             ;; (Sadly, "affinity" doesn't tell us anything interesting.
             ;; We've handled a and b in the clause above; after handling
             ;; b, point references buffer position n0 and the
             ;; region is inactive.
             ;; Let's ignore c, and wait until the selection's stabilized.
             ;; Make a new mark, a copy of point (position n0).
             ;; At step d (here), we should have either
             ;; d1) n1=n0.  Mark stays at n0, point moves to n0+m.
             ;; d2) n1+m=n0.  Mark stays at n0, point moves to n0-m.
             ;; If neither d1 nor d2 apply, arbitrarily assume forward
             ;; selection: mark at n1, point at n1+m.
             ;; In all cases, activate Hemlock selection.
             (unless still-selecting
                (let* ((pointpos (mark-absolute-position point))
                       (selection-end (+ location len))
                       (mark (hi::copy-mark point :right-inserting)))
                   (cond ((eql pointpos location)
                          (move-hemlock-mark-to-absolute-position point
                                                                  d
                                                                  selection-end))
                         ((eql pointpos selection-end)
                          (move-hemlock-mark-to-absolute-position point
                                                                  d
                                                                  location))
                         (t
                          (move-hemlock-mark-to-absolute-position mark
                                                                  d
                                                                  location)
                          (move-hemlock-mark-to-absolute-position point
                                                                  d
                                                                  selection-end)))
                   (hemlock::%buffer-push-buffer-mark buffer mark t)))))))
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

(def-cocoa-default *modeline-font-name* :string "Courier New Bold Italic"
                   "Name of font to use in modelines")
(def-cocoa-default  *modeline-font-size* :float 10.0 "Size of font to use in modelines" (single-float 4.0 14.0))


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

;;; We want to constrain the scrolling that happens under program control,
;;; so that the clipview is always scrolled in character-sized increments.
#+doesnt-work-yet
(define-objc-method ((:void :scroll-clip-view clip-view :to-point (:<NSP>oint p))
                     modeline-scroll-view)
  #+debug
  (#_NSLog #@"Scrolling to point %@" :id (#_NSStringFromPoint p))
  
  (let* ((char-height (send self 'vertical-line-scroll)))
    (slet ((proposed (ns-make-point (pref p :<NSP>oint.x)
                                         (* char-height
                                            (round (pref p :<NSP>oint.y)
                                                    char-height)))))
    #+debug
    (#_NSLog #@" Proposed point = %@" :id
             (#_NSStringFromPoint proposed)))
    (send-super :scroll-clip-view clip-view
                :to-point p #+nil (ns-make-point (pref p :<NSP>oint.x)
                                         (* char-height
                                            (ffloor (pref p :<NSP>oint.y)
                                                    char-height))))))



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
              (send layout :set-delegate tv)
	      (send tv :set-min-size (ns-make-size
				      0.0f0
				      (pref contentsize :<NSS>ize.height)))
	      (send tv :set-max-size (ns-make-size 1.0f7 1.0f7))
	      (send tv :set-rich-text nil)
	      (send tv :set-horizontally-resizable t)
	      (send tv :set-vertically-resizable t) 
	      (send tv :set-autoresizing-mask #$NSViewWidthSizable)
              (send tv :set-background-color color)
              (send tv :set-smart-insert-delete-enabled nil)
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
    #+debug
    (#_NSLog #@"Activating echo area")
    (send hemlock-frame :make-first-responder view)))

(defmethod text-view-buffer ((self echo-area-view))
  (buffer-cache-buffer (hemlock-buffer-string-cache (send (send self 'text-storage) 'string))))

;;; The "document" for an echo-area isn't a real NSDocument.
(defclass echo-area-document (ns:ns-object)
    ((textstorage :foreign-type :id))
  (:metaclass ns:+ns-object))

(define-objc-method ((:void close) echo-area-document)
  (let* ((ts (slot-value self 'textstorage)))
    (unless (%null-ptr-p ts)
      (setf (slot-value self 'textstorage) (%null-ptr))
      (close-hemlock-textstorage ts))))

(define-objc-method ((:void :update-change-count (:<NSD>ocument<C>hange<T>ype change)) echo-area-document)
  (declare (ignore change)))

(define-objc-method ((:void :key-down event)
		     echo-area-view)
  (pass-key-down-event-to-hemlock self event))


(defloadvar *hemlock-frame-count* 0)

(defun make-echo-area (hemlock-frame x y width height gap-context color)
  (slet ((frame (ns-make-rect x y width height)))
    (let* ((box (make-objc-instance "NSView"
                                    :with-frame frame)))
      (send box :set-autoresizing-mask #$NSViewWidthSizable)
      (slet* ((box-frame (send box 'bounds))
              (containersize (ns-make-size 1.0f7 (pref box-frame :<NSR>ect.size.height))))
        (let* ((clipview (make-objc-instance "NSClipView"
                                             :with-frame box-frame)))
          (send clipview :set-autoresizing-mask (logior #$NSViewWidthSizable
                                                        #$NSViewHeightSizable))
          (send clipview :set-background-color color)
          (send box :add-subview clipview)
          (send box :set-autoresizes-subviews t)
          (send clipview 'release)
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
                                             :with-frame box-frame
                                             :text-container container)))
              (send echo :set-min-size (pref box-frame :<NSR>ect.size))
              (send echo :set-max-size (ns-make-size 1.0f7 (pref box-frame :<NSR>ect.size)))
              (send echo :set-rich-text nil)
              (send echo :set-horizontally-resizable t)
              (send echo :set-vertically-resizable nil)
              (send echo :set-autoresizing-mask #$NSViewNotSizable)
              (send echo :set-background-color color)
              (send container :set-width-tracks-text-view nil)
              (send container :set-height-tracks-text-view nil)
              (setf (hemlock-frame-echo-area-buffer hemlock-frame) buffer
                    (slot-value doc 'textstorage) textstorage
                    (hi::buffer-document buffer) doc)
              (send clipview :set-document-view echo)
              (send clipview :set-autoresizes-subviews nil)
              (send echo 'size-to-fit)
              (values echo box))))))))
		    
(defun make-echo-area-for-window (w gap-context-for-echo-area-buffer color)
  (let* ((content-view (send w 'content-view)))
    (slet ((bounds (send content-view 'bounds)))
      (multiple-value-bind (echo-area box)
          (make-echo-area w
                          0.0f0
                          0.0f0
                          (- (pref bounds :<NSR>ect.size.width) 24.0f0)
                          20.0f0
                          gap-context-for-echo-area-buffer
                          color)
	(send content-view :add-subview box)
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
                                         :count 2))
             (*debug-io* *typeout-stream*))
        (stream-clear-output *debug-io*)
        (print-call-history :detailed-p nil)
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
         (hi::*disembodied-buffer-counter* 0)
         (hi::*in-a-recursive-edit* nil)
         (hi::*last-key-event-typed* nil)
         (hi::*input-transcript* nil)
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
         (hemlock::*target-column* 0)
         (hemlock::*last-comment-start* " ")
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
  (let* ((content-view (send self 'content-view))
         (subviews (send content-view 'subviews)))
    (do* ((i (1- (send subviews 'count)) (1- i)))
         ((< i 0))
      (send (send subviews :object-at-index i)
            'remove-from-superview-without-needing-display)))
  (let* ((proc (slot-value self 'command-thread)))
    (when proc
      (setf (slot-value self 'command-thread) nil)
      (process-kill proc)))
  (let* ((buf (hemlock-frame-echo-area-buffer self))
         (echo-doc (if buf (hi::buffer-document buf))))
    (when echo-doc
      (setf (hemlock-frame-echo-area-buffer self) nil)
      (send echo-doc 'close)))
  (release-canonical-nsobject self)
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
                (new-hemlock-document-window))))
    (make-scrolling-textview-for-pane pane ts container-tracks-text-view-width color)
    (multiple-value-bind (height width)
        (size-of-char-in-font (default-font))
      (size-text-pane pane height width nrows ncols))
    pane))




(defun hemlock-buffer-from-nsstring (nsstring name &rest modes)
  (let* ((buffer (make-hemlock-buffer name :modes modes)))
    (nsstring-to-buffer nsstring buffer)))

(defun %nsstring-to-mark (nsstring mark)
  "returns external-format of string"
  (let* ((string-len (send (the ns:ns-string nsstring) 'length))
         (line-start 0)
         (first-line-terminator ())
         (first-line (hi::mark-line mark))
         (previous first-line)
         (buffer (hi::line-%buffer first-line))
         (hi::*buffer-gap-context*
          (or 
           (hi::buffer-gap-context buffer)
           (setf (hi::buffer-gap-context buffer)
                 (hi::make-buffer-gap-context)))))
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
    first-line-terminator))
  
(defun nsstring-to-buffer (nsstring buffer)
  (let* ((document (hi::buffer-document buffer))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
         (region (hi::buffer-region buffer)))
    (setf (hi::buffer-document buffer) nil)
    (unwind-protect
	 (progn
	   (hi::delete-region region)
	   (hi::modifying-buffer buffer)
	   (hi::with-mark ((mark (hi::buffer-point buffer) :left-inserting))
             (setf (hi::buffer-external-format buffer)
                   (%nsstring-to-mark nsstring mark)))
)
	   (setf (hi::buffer-modified buffer) nil)
	   (hi::buffer-start (hi::buffer-point buffer))
           (hi::renumber-region region)
	   buffer)
      (setf (hi::buffer-document buffer) document)))

;;; This assumes that the buffer has no document and no textstorage (yet).
(defun hi::cocoa-read-file (lisp-pathname mark buffer)
  (let* ((lisp-namestring (native-translated-namestring lisp-pathname))
         (cocoa-pathname (%make-nsstring lisp-namestring))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
	 (data (make-objc-instance 'ns:ns-data
				   :with-contents-of-file cocoa-pathname))
	 (string (make-objc-instance 'ns:ns-string
				     :with-data data
				     :encoding #$NSASCIIStringEncoding))
         (external-format (%nsstring-to-mark string mark)))
    (unless (hi::buffer-external-format buffer)
      (setf (hi::buffer-external-format buffer) external-format))
    buffer))
    



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
  #-all-in-cocoa-thread
  (send (slot-value document 'textstorage) 'begin-editing)
  #+all-in-cocoa-thread
  (send (slot-value document 'textstorage)
        :perform-selector-on-main-thread
        (@selector "beginEditing")
        :with-object (%null-ptr)
        :wait-until-done t))

(defun document-edit-level (document)
  (slot-value (slot-value document 'textstorage) 'edit-count))



(defun hi::document-end-editing (document)
  #-all-in-cocoa-thread
  (send (slot-value document 'textstorage) 'end-editing)
  #+all-in-cocoa-thread
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
      (let* ((params
              (send (send (@class "NSArray") 'alloc)
                    :init-with-objects paramptrs
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




(defun hi::buffer-note-font-change (buffer region)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage)))
           (pos (mark-absolute-position (hi::region-start region)))
           (n (- (mark-absolute-position (hi::region-end region)) pos)))
      (perform-edit-change-notification textstorage
                                        (@selector "noteAttrChange:")
                                        pos
                                        n))))

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
            ;(reset-buffer-cache display)
            (adjust-buffer-cache-for-insertion display pos n)
            (update-line-cache-for-index display pos))
          #-all-in-cocoa-thread
          (textstorage-note-insertion-at-position textstorage pos n)
          #+all-in-cocoa-thread
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
        #-all-in-cocoa-thread
        (send textstorage
          :edited (logior #$NSTextStorageEditedCharacters
                          #$NSTextStorageEditedAttributes)
          :range (ns-make-range (mark-absolute-position mark) n)
          :change-in-length 0)
        #+all-in-cocoa-thread
        (perform-edit-change-notification textstorage
                                          (@selector "noteModification:")
                                          (mark-absolute-position mark)
                                          n)))))
  

(defun hi::buffer-note-deletion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        #-all-in-cocoa-thread
        (let* ((pos (mark-absolute-position mark)))
          (send textstorage
          :edited #$NSTextStorageEditedCharacters
          :range (ns-make-range pos n)
          :change-in-length (- n))
          (let* ((display (hemlock-buffer-string-cache (send textstorage 'string))))
            (reset-buffer-cache display) 
            (update-line-cache-for-index display pos)))
        #+all-in-cocoa-thread
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
         


(defun size-text-pane (pane char-height char-width nrows ncols)
  (let* ((tv (text-pane-text-view pane))
         (height (fceiling (* nrows char-height)))
	 (width (fceiling (* ncols char-width)))
	 (scrollview (text-pane-scroll-view pane))
	 (window (send scrollview 'window)))
    (rlet ((tv-size :<NSS>ize :height height
		    :width (+ width (* 2 (send (send tv 'text-container)
                                               'line-fragment-padding)))))
      (when (send scrollview 'has-vertical-scroller)
	(send scrollview :set-vertical-line-scroll char-height)
	(send scrollview :set-vertical-page-scroll 0.0f0 #|char-height|#))
      (when (send scrollview 'has-horizontal-scroller)
	(send scrollview :set-horizontal-line-scroll char-width)
	(send scrollview :set-horizontal-page-scroll 0.0f0 #|char-width|#))
      (slet ((sv-size
	      (send (@class ns-scroll-view)
		    :frame-size-for-content-size tv-size
		    :has-horizontal-scroller
		    (send scrollview 'has-horizontal-scroller)
		    :has-vertical-scroller
		    (send scrollview 'has-vertical-scroller)
		    :border-type (send scrollview 'border-type))))
	(slet ((pane-frame (send pane 'frame))
               (margins (send pane 'content-view-margins)))
	  (incf (pref sv-size :<NSS>ize.height)
		(+ (pref pane-frame :<NSR>ect.origin.y)
                   (* 2 (pref margins :<NSS>ize.height))))
          (incf (pref sv-size :<NSS>ize.width)
                (pref margins :<NSS>ize.width))
	  (send window :set-content-size sv-size)
	  (send window :set-resize-increments
		(ns-make-size char-width char-height)))))))
				    
  
(defclass hemlock-editor-window-controller (ns:ns-window-controller)
    ()
  (:metaclass ns:+ns-object))



(define-objc-method ((:void :_window-will-close notification)
                     hemlock-editor-window-controller)
  #+debug
  (let* ((w (send notification 'object)))
    (#_NSLog #@"Window controller: window will close: %@" :id w))
  (send-super :_window-will-close notification))

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


(define-objc-method ((:void :set-text-storage ts)
                     hemlock-editor-document)
  (let* ((doc (%inc-ptr self 0))
         (string (send ts 'string))
         (cache (hemlock-buffer-string-cache string))
         (buffer (buffer-cache-buffer cache)))
    (unless (%null-ptr-p doc)
      (setf (slot-value doc 'textstorage) ts
            (hi::buffer-document buffer) doc))))

(define-objc-method ((:<BOOL> :revert-to-saved-from-file filename
                              :of-type filetype)
                     hemlock-editor-document)
  (declare (ignore filetype))
  #+debug
  (#_NSLog #@"revert to saved from file %@ of type %@"
           :id filename :id filetype)
  (let* ((data (make-objc-instance 'ns:ns-data
                                   :with-contents-of-file filename))
         (nsstring (make-objc-instance 'ns:ns-string
				     :with-data data
				     :encoding #$NSASCIIStringEncoding))
         (buffer (hemlock-document-buffer self))
         (old-length (hemlock-buffer-length buffer))
         (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
         (textstorage (slot-value self 'textstorage))
         (point (hi::buffer-point buffer))
         (pointpos (mark-absolute-position point)))
    (send textstorage 'begin-editing)
    (send textstorage
          :edited #$NSTextStorageEditedCharacters
          :range (ns-make-range 0 old-length)
          :change-in-length (- old-length))
    (nsstring-to-buffer nsstring buffer)
    (let* ((newlen (hemlock-buffer-length buffer)))
      (send textstorage
            :edited #$NSTextStorageEditedAttributes
            :range (ns-make-range 0 0)
            :change-in-length newlen)
      (send textstorage
            :edited #$NSTextStorageEditedCharacters
            :range (ns-make-range 0 newlen)
            :change-in-length 0)
      (let* ((ts-string (send textstorage 'string))
             (display (hemlock-buffer-string-cache ts-string)))
        (reset-buffer-cache display) 
        (update-line-cache-for-index display 0)
        (move-hemlock-mark-to-absolute-position point
                                                display
                                                (min newlen pointpos)))
      (send textstorage 'end-editing))
    (hi::document-set-point-position self)
    (setf (hi::buffer-modified buffer) nil)
    (hi::queue-buffer-change buffer)
    t))
         
            
  
(define-objc-method ((:id init) hemlock-editor-document)
  (let* ((doc (send-super 'init)))
    (unless  (%null-ptr-p doc)
      (send doc
        :set-text-storage (make-textstorage-for-hemlock-buffer
                           (make-hemlock-buffer
                            (lisp-string-from-nsstring
                             (send doc 'display-name))
                            :modes '("Lisp" "Editor")))))
    doc))
                     

(define-objc-method ((:<BOOL> :read-from-file filename
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
    #$YES))

#+experimental
(define-objc-method ((:<BOOL> :write-with-backup-to-file path
                              :of-type type
                              :save-operation (:<NSS>ave<O>peration<T>ype save-operation))
                     hemlock-editor-document)
  #+debug
  (#_NSLog #@"saving file to %@" :id path)
  (send-super :write-with-backup-to-file path :of-type type :save-operation save-operation))

;;; This should be a preference.
(define-objc-method ((:<BOOL> keep-backup-file)
                     hemlock-editor-document)
  #$YES)


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


(def-cocoa-default *initial-editor-x-pos* :float 200.0f0 "X position of upper-left corner of initial editor")

(def-cocoa-default *initial-editor-y-pos* :float 400.0f0 "Y position of upper-left corner of initial editor")

(defloadvar *next-editor-x-pos* nil) ; set after defaults initialized
(defloadvar *next-editor-y-pos* nil)

(define-objc-method ((:void make-window-controllers) hemlock-editor-document)
  #+debug
  (#_NSLog #@"Make window controllers")
  (let* ((window (%hemlock-frame-for-textstorage 
                                    (slot-value self 'textstorage)
				    *editor-columns*
				    *editor-rows*
				    nil
                                    (textview-background-color self)))
         (controller (make-objc-instance
		      'hemlock-editor-window-controller
		      :with-window window)))
    (send self :add-window-controller controller)
    (send controller 'release)
    (slet ((current-point (ns-make-point (or *next-editor-x-pos*
                                             *initial-editor-x-pos*)
                                         (or *next-editor-y-pos*
                                             *initial-editor-y-pos*))))
      (slet ((new-point (send window
                              :cascade-top-left-from-point current-point)))
            (setf *next-editor-x-pos* (pref new-point :<NSP>oint.x)
                  *next-editor-y-pos* (pref new-point :<NSP>oint.y))))))


(define-objc-method ((:void close) hemlock-editor-document)
  #+debug
  (#_NSLog #@"Document close: %@" :id self)
  (let* ((textstorage (slot-value self 'textstorage)))
    (unless (%null-ptr-p textstorage)
      (setf (slot-value self 'textstorage) (%null-ptr))
      (for-each-textview-using-storage
       textstorage
       #'(lambda (tv)
           (let* ((layout (send tv 'layout-manager)))
             (send layout :set-background-layout-enabled nil))))
      (close-hemlock-textstorage textstorage)))
  (send-super 'close))


(defun initialize-user-interface ()
  (send (find-class 'preferences-panel) 'shared-panel)
  (update-cocoa-defaults)
  (make-editor-style-map))

(defun hi::scroll-window (textpane n)
  (declare (ignore textpane))
  (let* ((point (hi::current-point)))
    (or (hi::line-offset point (if (and n (< n 0)) -24 24) 0))))

(defmethod hemlock::center-text-pane ((pane text-pane))
  (send (text-pane-text-view pane)
        :center-selection-in-visible-area (%null-ptr)))


(defun hi::open-document ()
  (send (send (find-class 'ns:ns-document-controller)
              'shared-document-controller)
        :perform-selector-on-main-thread (@selector "openDocument:")
        :with-object (%null-ptr)
        :wait-until-done t))
  
(defmethod hi::save-hemlock-document ((self hemlock-editor-document))
  (send self
        :perform-selector-on-main-thread (@selector "saveDocument:")
        :with-object (%null-ptr)
        :wait-until-done t))


(defmethod hi::save-hemlock-document-as ((self hemlock-editor-document))
  (send self
        :perform-selector-on-main-thread (@selector "saveDocumentAs:")
        :with-object (%null-ptr)
        :wait-until-done t))

;;; This needs to run on the main thread.
(define-objc-method ((:void update-hemlock-selection)
                     hemlock-text-storage)
  (let* ((string (send self 'string))
         (buffer (buffer-cache-buffer (hemlock-buffer-string-cache string)))
         (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
         (point (hi::buffer-point buffer))
         (pointpos (mark-absolute-position point))
         (location pointpos)
         (len 0))
    (when (hemlock::%buffer-region-active-p buffer)
      (let* ((mark (hi::buffer-%mark buffer)))
        (when mark
          (let* ((markpos (mark-absolute-position mark)))
            (if (< markpos pointpos)
              (setq location markpos len (- pointpos markpos))
              (if (< pointpos markpos)
                (setq location pointpos len (- markpos pointpos))))))))
    #+debug
    (#_NSLog #@"update Hemlock selection: charpos = %d, abspos = %d"
             :int (hi::mark-charpos point) :int pos)
    (for-each-textview-using-storage
     self
     #'(lambda (tv)
         (send tv
               :update-selection location
               :length len
               :affinity (if (eql location 0)
                           #$NSSelectionAffinityUpstream
                           #$NSSelectionAffinityDownstream))))))


(defun hi::allocate-temporary-object-pool ()
  (create-autorelease-pool))

(defun hi::free-temporary-objects (pool)
  (release-autorelease-pool pool))

(provide "COCOA-EDITOR")
