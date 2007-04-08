;;-*- Mode: LISP; Package: CCL -*-


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

;;; In the double-float case, this is probably way too small.
;;; Traditionally, it's (approximately) the point at which
;;; a single-float stops being able to accurately represent
;;; integral values.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant large-number-for-text (float 1.0f7 +cgfloat-zero+)))

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
	 (colors (vector (#/blackColor color-class)
			 (#/whiteColor  color-class)
			 (#/darkGrayColor color-class)
			 (#/lightGrayColor color-class)
			 (#/redColor color-class)
			 (#/blueColor color-class)
			 (#/greenColor color-class)
			 (#/yellowColor color-class)))
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
(objc:defmethod (#/length :<NSUI>nteger) ((self hemlock-buffer-string))
  (let* ((cache (hemlock-buffer-string-cache self)))
    (or (buffer-cache-buflen cache)
        (setf (buffer-cache-buflen cache)
              (let* ((buffer (buffer-cache-buffer cache)))
		(hemlock-buffer-length buffer))))))



;;; Return the character at the specified index (as a :unichar.)

(objc:defmethod (#/characterAtIndex: :unichar)
    ((self hemlock-buffer-string) (index :<NSUI>nteger))
  #+debug
  (#_NSLog #@"Character at index: %d" :<NSUI>nteger index)
  (char-code (hemlock-char-at-index (hemlock-buffer-string-cache self) index)))

(objc:defmethod (#/getCharacters:range: :void)
    ((self hemlock-buffer-string)
     (buffer (:* :unichar))
     (r :<NSR>ange))
  (let* ((cache (hemlock-buffer-string-cache self))
         (index (ns:ns-range-location r))
         (length (ns:ns-range-length r))
         (hi::*buffer-gap-context*
	  (hi::buffer-gap-context (buffer-cache-buffer cache))))
    #+debug
    (#_NSLog #@"get characters: %d/%d"
             :<NSUI>nteger index
             :<NSUI>nteger length)
    (multiple-value-bind (line idx) (update-line-cache-for-index cache index)
      (let* ((len (hemlock::line-length line)))
        (do* ((i 0 (1+ i)))
             ((= i length))
          (cond ((< idx len)
                 (setf (paref buffer (:* :unichar) i)
                       (char-code (hemlock::line-character line idx)))
                 (incf idx))
                (t
                 (setf (paref buffer (:* :unichar) i)
                       (char-code #\Newline)
                       line (hi::line-next line)
                       len (hi::line-length line)
                  idx 0))))))))

(objc:defmethod (#/getLineStart:end:contentsEnd:forRange: :void)
    ((self hemlock-buffer-string)
     (startptr (:* :<NSUI>nteger))
     (endptr (:* :<NSUI>nteger))
     (contents-endptr (:* :<NSUI>nteger))
     (r :<NSR>ange))
  (let* ((cache (hemlock-buffer-string-cache self))
         (index (pref r :<NSR>ange.location))
         (length (pref r :<NSR>ange.length))
         (hi::*buffer-gap-context*
	  (hi::buffer-gap-context (buffer-cache-buffer cache))))
    #+debug
    (#_NSLog #@"get line start: %d/%d"
             :unsigned index
             :unsigned length)
    (update-line-cache-for-index cache index)
    (unless (%null-ptr-p startptr)
      ;; Index of the first character in the line which contains
      ;; the start of the range.
      (setf (pref startptr :<NSUI>nteger)
            (buffer-cache-workline-offset cache)))
    (unless (%null-ptr-p endptr)
      ;; Index of the newline which terminates the line which
      ;; contains the start of the range.
      (setf (pref endptr :<NSUI>nteger)
            (+ (buffer-cache-workline-offset cache)
               (buffer-cache-workline-length cache))))
    (unless (%null-ptr-p contents-endptr)
      ;; Index of the newline which terminates the line which
      ;; contains the start of the range.
      (unless (zerop length)
        (update-line-cache-for-index cache (+ index length)))
      (setf (pref contents-endptr :<NSUI>nteger)
            (1+ (+ (buffer-cache-workline-offset cache)
                   (buffer-cache-workline-length cache)))))))

                     
;;; Return an NSData object representing the bytes in the string.  If
;;; the underlying buffer uses #\linefeed as a line terminator, we can
;;; let the superclass method do the work; otherwise, we have to
;;; ensure that each line is terminated according to the buffer's
;;; conventions.
(objc:defmethod #/dataUsingEncoding:allowLossyConversion:
    ((self hemlock-buffer-string)
     (encoding :<NSS>tring<E>ncoding)
     (flag :<BOOL>))
  (let* ((buffer (buffer-cache-buffer (hemlock-buffer-string-cache self)))
	 (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
	 (external-format (if buffer (hi::buffer-external-format buffer )))
	 (raw-length (if buffer (hemlock-buffer-length buffer) 0)))
    (hi::%set-buffer-modified buffer nil)
    (if (eql 0 raw-length)
      (make-instance 'ns:ns-mutable-data :with-length 0)
      (case external-format
	((:unix nil)
         (call-next-method encoding flag))
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
		  (data (make-instance 'ns:ns-mutable-data
                                       :with-length raw-length))
		  (bytes (#/mutableBytes data)))
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
(objc:defmethod #/description ((self hemlock-buffer-string))
  (let* ((cache (hemlock-buffer-string-cache self))
	 (b (buffer-cache-buffer cache)))
    (with-cstrs ((s (format nil "~a" b)))
      (#/stringWithFormat: ns:ns-string #@"<%s for %s>" (#_object_getClassName self) s))))



;;; hemlock-text-storage objects
(defclass hemlock-text-storage (ns:ns-text-storage)
    ((string :foreign-type :id)
     (edit-count :foreign-type :int)
     (append-edits :foreign-type :int))
  (:metaclass ns:+ns-object))


;;; This is only here so that calls to it can be logged for debugging.
#+debug
(objc:defmethod (#/lineBreakBeforeIndex:withinRange: :<NSUI>nteger)
    ((self hemlock-text-storage)
     (index :<NSUI>nteger)
     (r :<NSR>ange))
  (#_NSLog #@"Line break before index: %d within range: %@"
           :unsigned index
           :id (#_NSStringFromRange r))
  (call-next-method index r))



;;; Return true iff we're inside a "beginEditing/endEditing" pair
(objc:defmethod (#/editingInProgress :<BOOL>) ((self hemlock-text-storage))
  (not (eql (slot-value self 'edit-count) 0)))

(defun textstorage-note-insertion-at-position (self pos n)
  (ns:with-ns-range (r pos 0)
    (#/edited:range:changeInLength: self #$NSTextStorageEditedAttributes r n)
    (setf (ns:ns-range-length r) n)
    (#/edited:range:changeInLength: self #$NSTextStorageEditedCharacters r 0)))

(objc:defmethod (#/noteInsertion: :void) ((self hemlock-text-storage) params)
  (let* ((pos (#/longValue (#/objectAtIndex: params 0)))
         (n (#/longValue (#/objectAtIndex: params 1))))
    (textstorage-note-insertion-at-position self pos n)))

(objc:defmethod (#/noteDeletion: :void) ((self hemlock-text-storage) params)
  (let* ((pos (#/longValue (#/objectAtIndex: params 0)))
         (n (#/longValue (#/objectAtIndex: params 1))))
    (rlet ((range :ns-range :location pos :length n))
      (#/edited:range:changeInLength: self #$NSTextStorageEditedCharacters range (- n)))
    (let* ((display (hemlock-buffer-string-cache (#/string self))))
      (reset-buffer-cache display) 
      (update-line-cache-for-index display pos))))

(objc:defmethod (#/noteModification: :void) ((self hemlock-text-storage) params)
  (let* ((pos (#/longValue (#/objectAtIndex: params 0)))
         (n (#/longValue (#/objectAtIndex: params 1))))
    #+debug
    (#_NSLog #@"Note modification: pos = %d, n = %d" :int pos :int n)
    (rlet ((range :ns-range :location pos :length n))
      (#/edited:range:changeInLength: self (logior #$NSTextStorageEditedCharacters
                                                  #$NSTextStorageEditedAttributes) range 0))))

(objc:defmethod (#/noteAttrChange: :void) ((self hemlock-text-storage) params)
  (let* ((pos (#/longValue (#/objectAtIndex: params 0)))
         (n (#/longValue (#/objectAtIndex: params 1))))
    #+debug (#_NSLog #@"attribute-change at %d/%d" :int pos :int n)
    (rlet ((range :ns-range :location pos :length n))
      (#/edited:range:changeInLength: self #$NSTextStorageEditedAttributes range 0))))

(objc:defmethod (#/beginEditing :void) ((self hemlock-text-storage))
  #+debug
  (#_NSLog #@"begin-editing")
  (incf (slot-value self 'edit-count))
  #+debug
  (#_NSLog #@"after beginEditing edit-count now = %d" :int (slot-value self 'edit-count))
  (call-next-method))

(objc:defmethod (#/endEditing :void) ((self hemlock-text-storage))
  #+debug
  (#_NSLog #@"end-editing")
  (call-next-method)
  (decf (slot-value self 'edit-count))
  #+debug
  (#_NSLog #@"after endEditing edit-count now = %d" :int (slot-value self 'edit-count)))

;;; Return true iff we're inside a "beginEditing/endEditing" pair
(objc:defmethod (#/editingInProgress :<BOOL>) ((self hemlock-text-storage))
  (not (eql (slot-value self 'edit-count) 0)))

  

;;; Access the string.  It'd be nice if this was a generic function;
;;; we could have just made a reader method in the class definition.
(objc:defmethod #/string ((self hemlock-text-storage))
  (slot-value self 'string))

(objc:defmethod #/initWithString: ((self hemlock-text-storage) s)
  (let* ((newself (#/init self)))
    (setf (slot-value newself 'string) s)
    newself))

;;; This is the only thing that's actually called to create a
;;; hemlock-text-storage object.  (It also creates the underlying
;;; hemlock-buffer-string.)
(defun make-textstorage-for-hemlock-buffer (buffer)
  (make-instance 'hemlock-text-storage
                 :with-string
                 (make-instance
                  'hemlock-buffer-string
                  :cache
                  (reset-buffer-cache
                   (make-buffer-cache)
                   buffer))))

(objc:defmethod #/attributesAtIndex:effectiveRange:
    ((self hemlock-text-storage) (index :<NSUI>nteger) (rangeptr (* :<NSR>ange)))
  #+debug
  (#_NSLog #@"Attributes at index: %ld" :<NSUI>nteger index)
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

(objc:defmethod (#/replaceCharactersInRange:withString: :void)
    ((self hemlock-text-storage) (r :<NSR>ange) string)
  (let* ((cache (hemlock-buffer-string-cache (#/string  self)))
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
;;; attributes in the buffer.  This method is only here so we can
;;; see if/when it tries to do so.
(objc:defmethod (#/setAttributes:range: :void) ((self hemlock-text-storage)
                                                attributes
                                                (r :<NSR>ange))
  (declare (ignorable attributes r))
  #+debug
  (#_NSLog #@"set-attributes %@ range (%d %d)"
	   :id attributes
	   :unsigned (pref r :<NSR>ange.location)
	   :unsigned (pref r :<NSR>ange.length)))

(defun for-each-textview-using-storage (textstorage f)
  (let* ((layouts (#/layoutManagers textstorage)))
    (unless (%null-ptr-p layouts)
      (dotimes (i (#/count layouts))
	(let* ((layout (#/objectAtIndex: layouts i))
	       (containers (#/textContainers layout)))
	  (unless (%null-ptr-p containers)
	    (dotimes (j (#/count containers))
	      (let* ((container (#/objectAtIndex: containers j))
		     (tv (#/textView container)))
		(funcall f tv)))))))))

;;; Again, it's helpful to see the buffer name when debugging.
(objc:defmethod #/description ((self hemlock-text-storage))
  (#/stringWithFormat: ns:ns-string #@"%s : string %@" (#_object_getClassName self) (slot-value self 'string)))

;;; This needs to happen on the main thread.
(objc:defmethod (#/ensureSelectionVisible :void) ((self hemlock-text-storage))
  (for-each-textview-using-storage
   self
   #'(lambda (tv)
       (#/scrollRangeToVisible: tv (#/selectedRange tv)))))


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

(objc:defmethod (#/layoutManager:didCompleteLayoutForTextContainer:atEnd: :void)
    ((self hemlock-textstorage-text-view) layout cont (flag :<BOOL>))
  (declare (ignorable cont flag))
  (when (zerop *layout-text-in-background*)
    (#/setDelegate: layout +null-ptr+)
    (#/setBackgroundLayoutEnabled: layout nil)))
    
;;; Note changes to the textview's background color; record them
;;; as the value of the "temporary" foreground color (for blinking).
(objc:defmethod (#/setBackgroundColor: :void)
    ((self hemlock-textstorage-text-view) color)
  (setf (text-view-blink-color self) color)
  (call-next-method color))

;;; Maybe cause 1 character in the textview to blink (by drawing an empty
;;; character rectangle) in synch with the insertion point.

(objc:defmethod (#/drawInsertionPointInRect:color:turnedOn: :void)
    ((self hemlock-textstorage-text-view)
     (r :<NSR>ect)
     color
     (flag :<BOOL>))
  (unless (#/editingInProgress (#/textStorage self))
    (unless (eql #$NO (text-view-blink-enabled self))
      (let* ((layout (#/layoutManager self))
             (container (#/textContainer self))
             (blink-color (text-view-blink-color self)))
        ;; We toggle the blinked character "off" by setting its
        ;; foreground color to the textview's background color.
        ;; The blinked character should be "on" whenever the insertion
        ;; point is drawn as "off"
        (ns:with-ns-range  (char-range (text-view-blink-location self) 1)
          (let* ((glyph-range (#/glyphRangeForCharacterRange:actualCharacterRange:
                               layout
                               char-range
                               +null-ptr+)))
            #+debug (#_NSLog #@"Flag = %d, location = %d" :<BOOL> (if flag #$YES #$NO) :int (text-view-blink-location self))
            (let* ((rect (#/boundingRectForGlyphRange:inTextContainer:
                          layout
                          glyph-range
                          container)))
              (#/set blink-color)
              (#_NSRectFill rect))
          (if flag
            (#/drawGlyphsForGlyphRange:atPoint: layout glyph-range (#/textContainerOrigin self)))))))
    (call-next-method r color flag)))
                
(defmethod disable-blink ((self hemlock-textstorage-text-view))
  (when (eql (text-view-blink-enabled self) #$YES)
    (setf (text-view-blink-enabled self) #$NO)
    ;; Force the blinked character to be redrawn.  Let the text
    ;; system do the drawing.
    (let* ((layout (#/layoutManager self)))
      (rlet ((invalid-range :ns-range 
                            :location  (text-view-blink-location self)
                            :length 1))
        (#/invalidateDisplayForCharacterRange: layout invalid-range)))))

(defmethod update-blink ((self hemlock-textstorage-text-view))
  (disable-blink self)
  (let* ((d (hemlock-buffer-string-cache (#/string self)))
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
;;; underlying buffer's point and/or mark

(objc:defmethod (#/updateSelection:length:affinity: :void)
    ((self hemlock-textstorage-text-view)
     (pos :int)
     (length :int)
     (affinity :<NSS>election<A>ffinity))
  (when (eql length 0)
    (update-blink self))
  (rlet ((range :ns-range :location pos :length length))
    (%call-next-objc-method self
                            hemlock-textstorage-text-view
                            (@selector #/setSelectedRange:affinity:stillSelecting:)
                            '(:void :<NSR>ange :<NSS>election<A>ffinity :<BOOL>)
                            range
                            affinity
                            nil)
    (#/scrollRangeToVisible: self range)))
  
;;; A specialized NSTextView. The NSTextView is part of the "pane"
;;; object that displays buffers.
(defclass hemlock-text-view (hemlock-textstorage-text-view)
    ((pane :foreign-type :id :accessor text-view-pane))
  (:metaclass ns:+ns-object))

;;; Access the underlying buffer in one swell foop.
(defmethod text-view-buffer ((self hemlock-text-view))
  (buffer-cache-buffer (hemlock-buffer-string-cache (#/string (#/textStorage self)))))

(objc:defmethod (#/setString: :void) ((self hemlock-textstorage-text-view) s)
  #+debug
  (#_NSLog #@"hemlock-text-view %@ string set to %@" :id self :id s)
  (call-next-method) s)

(objc:defmethod (#/selectionRangeForProposedRange:granularity: :ns-range)
    ((self hemlock-textstorage-text-view)
     (proposed :ns-range)
     (g :<NSS>election<G>ranularity))
  #+debug
  (#_NSLog #@"Granularity = %d" :int g)
  (objc:returning-foreign-struct (r)
    (block HANDLED
      (let* ((index (ns:ns-range-location proposed))             
             (length (ns:ns-range-length proposed)))
      (when (and (eql 0 length)              ; not extending existing selection
                 (not (eql g #$NSSelectByCharacter)))
        (let* ((textstorage (#/textStorage self))
               (cache (hemlock-buffer-string-cache (#/string textstorage)))
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
                             (ns:init-ns-range r index (- (mark-absolute-position m2) index))
                             (return-from HANDLED r))))
                        ((eql (hi::previous-character m1) #\))
                         (hi::with-mark ((m2 m1))
                           (when (hemlock::list-offset m2 -1)
                             (ns:init-ns-range r (mark-absolute-position m2) (- index (mark-absolute-position m2)))
                             (return-from HANDLED r))))))))))))
      (call-next-method proposed g)
      #+debug
      (#_NSLog #@"range = %@, proposed = %@, granularity = %d"
               :address (#_NSStringFromRange r)
               :address (#_NSStringFromRange proposed)
               :<NSS>election<G>ranularity g))))

  


;;; Translate a keyDown NSEvent to a Hemlock key-event.
(defun nsevent-to-key-event (nsevent)
  (let* ((unmodchars (#/charactersIgnoringModifiers nsevent))
	 (n (if (%null-ptr-p unmodchars)
	      0
	      (#/length unmodchars)))
	 (c (if (eql n 1)
	      (#/characterAtIndex: unmodchars 0))))
    (when c
      (let* ((bits 0)
	     (modifiers (#/modifierFlags nsevent))
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
      (let* ((q (hemlock-frame-event-queue (#/window self))))
        (hi::enqueue-key-event q (nsevent-to-key-event event))))))

(defun enqueue-buffer-operation (buffer thunk)
  (dolist (w (hi::buffer-windows buffer))
    (let* ((q (hemlock-frame-event-queue (#/window w)))
           (op (hi::make-buffer-operation :thunk thunk)))
      (hi::event-queue-insert q op))))

  
;;; Process a key-down NSEvent in a Hemlock text view by translating it
;;; into a Hemlock key event and passing it into the Hemlock command
;;; interpreter. 

(objc:defmethod (#/keyDown: :void) ((self hemlock-text-view) event)
  (pass-key-down-event-to-hemlock self event))

;;; Update the underlying buffer's point (and "active region", if appropriate.
;;; This is called in response to a mouse click or other event; it shouldn't
;;; be called from the Hemlock side of things.

(objc:defmethod (#/setSelectedRange:affinity:stillSelecting: :void)
    ((self hemlock-text-view)
     (r :<NSR>ange)
     (affinity :<NSS>election<A>ffinity)
     (still-selecting :<BOOL>))
  #+debug 
  (#_NSLog #@"Set selected range called: location = %d, length = %d, affinity = %d, still-selecting = %d"
           :int (pref r :<NSR>ange.location)
           :int (pref r :<NSR>ange.length)
           :<NSS>election<A>ffinity affinity
           :<BOOL> (if still-selecting #$YES #$NO))
  #+debug
  (#_NSLog #@"text view string = %@, textstorage string = %@"
           :id (#/string self)
           :id (#/string (#/textStorage self)))
  (unless (#/editingInProgress (#/textStorage self))
    (let* ((d (hemlock-buffer-string-cache (#/string self)))
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
             ;; (Sadly, "affinity" doesn't tell us anything interesting.)
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
  (call-next-method r affinity still-selecting))



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
(defun draw-modeline-string (the-modeline-view)
  (let* ((pane (modeline-view-pane the-modeline-view))
         (buffer (buffer-for-modeline-view the-modeline-view)))
    (when buffer
      ;; You don't want to know why this is done this way.
      (unless *modeline-text-attributes*
	(setq *modeline-text-attributes*
	      (create-text-attributes :color (#/blackColor ns:ns-color)
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
        (rletZ ((zpoint :ns-point))
          (#/drawAtPoint:withAttributes: (%make-nsstring string)
                                         zpoint
                                         *modeline-text-attributes*))))))

;;; Draw the underlying buffer's modeline string on a white background
;;; with a bezeled border around it.
(objc:defmethod (#/drawRect: :void) ((self modeline-view) (rect :<NSR>ect))
  (declare (ignorable rect))
  (let* ((frame (#/bounds self)))
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

(objc:defmethod #/initWithFrame: ((self modeline-scroll-view) (frame :<NSR>ect))
    (let* ((v (call-next-method frame)))
      (when v
        (let* ((modeline (make-instance 'modeline-view)))
          (#/addSubview: v modeline)
          (setf (scroll-view-modeline v) modeline)))
      v))

;;; Scroll views use the "tile" method to lay out their subviews.
;;; After the next-method has done so, steal some room in the horizontal
;;; scroll bar and place the modeline view there.

(objc:defmethod (#/tile :void) ((self modeline-scroll-view))
  (call-next-method)
  (let* ((modeline (scroll-view-modeline self)))
    (when (and (#/hasHorizontalScroller self)
               (not (%null-ptr-p modeline)))
      (let* ((hscroll (#/horizontalScroller self))
             (scrollbar-frame (#/frame hscroll))
             (modeline-frame (#/frame hscroll)) ; sic
             (modeline-width (* (pref modeline-frame
                                      :<NSR>ect.size.width)
                                0.75f0)))
        (declare (type cgfloat modeline-width))
        (setf (pref modeline-frame :<NSR>ect.size.width)
              modeline-width
              (the cgfloat
                (pref scrollbar-frame :<NSR>ect.size.width))
              (- (the cgfloat
                   (pref scrollbar-frame :<NSR>ect.size.width))
                 modeline-width)
              (the cg-float
                (pref scrollbar-frame :<NSR>ect.origin.x))
              (+ (the cgfloat
                   (pref scrollbar-frame :<NSR>ect.origin.x))
                 modeline-width))
        (#/setFrame: hscroll scrollbar-frame)
        (#/setFrame: modeline modeline-frame)))))

;;; We want to constrain the scrolling that happens under program control,
;;; so that the clipview is always scrolled in character-sized increments.
#+doesnt-work-yet
(objc:defmethod (#/scrollClipView:toPoint: :void)
    ((self modeline-scroll-view)
     clip-view
     (p :ns-point))
  #+debug
  (#_NSLog #@"Scrolling to point %@" :id (#_NSStringFromPoint p))
  (let* ((char-height (#/verticalLineScroll self)))
    (ns:with-ns-point (proposed (ns:ns-point-x p) (* char-height (round (ns:ns-point-y p) char-height)))
    #+debug
    (#_NSLog #@" Proposed point = %@" :id
             (#_NSStringFromPoint proposed)))
    (call-next-method clip-view proposed)))



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
  (#/setNeedsDisplay: (text-pane-mode-line pane) t))

(def-cocoa-default *text-pane-margin-width* :float 0.0f0 "width of indented margin around text pane")
(def-cocoa-default *text-pane-margin-height* :float 0.0f0 "height of indented margin around text pane")


(objc:defmethod #/initWithFrame: ((self text-pane) (frame :<NSR>ect))
  (let* ((pane (call-next-method frame)))
    (unless (%null-ptr-p pane)
      (#/setAutoresizingMask: pane (logior
                                    #$NSViewWidthSizable
                                    #$NSViewHeightSizable))
      (#/setBoxType: pane #$NSBoxPrimary)
      (#/setBorderType: pane #$NSNoBorder)
      (#/setContentViewMargins: pane (ns:make-ns-size *text-pane-margin-width*  *text-pane-margin-height*))
      (#/setTitlePosition: pane #$NSNoTitle))
    pane))


(defun make-scrolling-text-view-for-textstorage (textstorage x y width height tracks-width color)
  (let* ((scrollview (#/autorelease
                      (make-instance
                       'modeline-scroll-view
                       :with-frame (ns:make-ns-rect x y width height)))))
    (#/setBorderType: scrollview #$NSBezelBorder)
    (#/setHasVerticalScroller: scrollview t)
    (#/setHasHorizontalScroller: scrollview t)
    (#/setRulersVisible: scrollview nil)
    (#/setAutoresizingMask: scrollview (logior
                                        #$NSViewWidthSizable
                                        #$NSViewHeightSizable))
    (#/setAutoresizesSubviews: (#/contentView scrollview) t)
    (let* ((layout (make-instance 'ns:ns-layout-manager)))
      (#/addLayoutManager: textstorage layout)
      (#/release layout)
      (let* ((contentsize (#/contentSize scrollview)))
        (ns:with-ns-size (containersize large-number-for-text large-number-for-text)
          (ns:with-ns-rect (tv-frame 0 0 (ns:ns-size-width contentsize) (ns:ns-size-height contentsize))
            (ns:init-ns-size containersize large-number-for-text large-number-for-text)
            (ns:init-ns-rect tv-frame 0 0 (ns:ns-size-width contentsize) (ns:ns-size-height contentsize))
            (let* ((container (#/autorelease (make-instance
                                              'ns:ns-text-container
                                              :with-container-size containersize))))
              (#/addTextContainer: layout  container)
              (let* ((tv (#/autorelease (make-instance 'hemlock-text-view
                                                       :with-frame tv-frame
                                                       :text-container container))))
                (#/setDelegate: layout tv)
                (#/setMinSize: tv (ns:make-ns-size 0 (ns:ns-size-height contentsize)))
                (#/setMaxSize: tv (ns:make-ns-size large-number-for-text large-number-for-text))
                (#/setRichText: tv nil)
                (#/setHorizontallyResizable: tv t)
                (#/setVerticallyResizable: tv t) 
                (#/setAutoresizingMask: tv #$NSViewWidthSizable)
                (#/setBackgroundColor: tv color)
                (#/setSmartInsertDeleteEnabled: tv nil)
                (#/setWidthTracksTextView: container tracks-width)
                (#/setHeightTracksTextView: container nil)
                (#/setDocumentView: scrollview tv)	      
                (values tv scrollview)))))))))

(defun make-scrolling-textview-for-pane (pane textstorage track-width color)
  (let* ((contentrect (#/frame (#/contentView pane))))
    (multiple-value-bind (tv scrollview)
	(make-scrolling-text-view-for-textstorage
	 textstorage
         (ns:ns-rect-x contentrect)
         (ns:ns-rect-y contentrect)
         (ns:ns-rect-width contentrect)
         (ns:ns-rect-height contentrect)
	 track-width
         color)
      (#/setContentView: pane scrollview)
      (setf (slot-value pane 'scroll-view) scrollview
            (slot-value pane 'text-view) tv
            (slot-value tv 'pane) pane
            (slot-value scrollview 'pane) pane)
      (let* ((modeline  (scroll-view-modeline scrollview)))
        (setf (slot-value pane 'mode-line) modeline
              (slot-value modeline 'pane) pane))
      tv)))


(defmethod hi::activate-hemlock-view ((view text-pane))
  (let* ((the-hemlock-frame (#/window view))
	 (text-view (text-pane-text-view view)))
    (#/makeFirstResponder: the-hemlock-frame text-view)))


(defclass echo-area-view (hemlock-textstorage-text-view)
    ()
  (:metaclass ns:+ns-object))

(defmethod hi::activate-hemlock-view ((view echo-area-view))
  (let* ((the-hemlock-frame (#/window view)))
    #+debug
    (#_NSLog #@"Activating echo area")
    (#/makeFirstResponder: the-hemlock-frame view)))

(defmethod text-view-buffer ((self echo-area-view))
  (buffer-cache-buffer (hemlock-buffer-string-cache (#/string (#/textStorage self)))))

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

(defun make-echo-area (the-hemlock-frame x y width height gap-context color)
  (let* ((box (make-instance 'ns:ns-view :with-frame (ns:make-ns-rect x y width height))))
    (#/setAutoresizingMask: box #$NSViewWidthSizable)
    (let* ((box-frame (#/bounds box))
           (containersize (ns:make-ns-size large-number-for-text (ns:ns-rect-height box-frame)))
           (clipview (make-instance 'ns:ns-clip-view
                                    :with-frame box-frame)))
      (#/setAutoresizingMask: clipview (logior #$NSViewWidthSizable
                                               #$NSViewHeightSizable))
      (#/setBackgroundColor: clipview color)
      (#/addSubview: box clipview)
      (#/setAutoresizesSubviews: box t)
      (#/release clipview)
      (let* ((buffer (hi:make-buffer (format nil "Echo Area ~d"
                                             (prog1
                                                 *hemlock-frame-count*
                                               (incf *hemlock-frame-count*)))
                                     :modes '("Echo Area")))
             (textstorage
              (progn
                (setf (hi::buffer-gap-context buffer) gap-context)
                (make-textstorage-for-hemlock-buffer buffer)))
             (doc (make-instance 'echo-area-document))
             (layout (make-instance 'ns:ns-layout-manager))
             (container (#/autorelease
                         (make-instance 'ns:ns-text-container
                                        :with-container-size
                                        containersize))))
        (#/addLayoutManager: textstorage layout)
        (#/addTextContainer: layout container)
        (#/release layout)
        (let* ((echo (make-instance 'echo-area-view
                                    :with-frame box-frame
                                    :text-container container)))
          (#/setMinSize: echo (pref box-frame :<NSR>ect.size))
          (#/setMaxSize: echo (ns:make-ns-size large-number-for-text large-number-for-text))
          (#/setRichText: echo nil)
          (#/setHorizontallyResizable: echo t)
          (#/setVerticallyResizable: echo nil)
          (#/setAutoresizingMask: echo #$NSViewNotSizable)
          (#/setBackgroundColor: echo color)
          (#/setWidthTracksTextView: container nil)
          (#/setHeightTracksTextView: container nil)
          (setf (hemlock-frame-echo-area-buffer the-hemlock-frame) buffer
                (slot-value doc 'textstorage) textstorage
                (hi::buffer-document buffer) doc)
          (#/setDocumentView: clipview echo)
          (#/setAutoresizesSubviews: clipview nil)
          (#/sizeToFit echo)
          (values echo box))))))
		    
(defun make-echo-area-for-window (w gap-context-for-echo-area-buffer color)
  (let* ((content-view (#/contentView w))
         (bounds (#/bounds content-view)))
      (multiple-value-bind (echo-area box)
          (make-echo-area w
                          0.0f0
                          0.0f0
                          (- (ns:ns-rect-width bounds) 24.0f0)
                          20.0f0
                          gap-context-for-echo-area-buffer
                          color)
	(#/addSubview: content-view box)
	echo-area)))
               
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

(objc:defmethod (#/runErrorSheet: :void) ((self hemlock-frame) info)
  (let* ((message (#/objectAtIndex: info 0))
         (signal (#/objectAtIndex: info 1)))
    (#_NSBeginAlertSheet #@"Error in Hemlock command processing" ;title
                         (if (logbitp 0 (random 2))
                           #@"Not OK, but what can you do?"
                           #@"The sky is falling. FRED never did this!")
                         +null-ptr+
                         +null-ptr+
                         self
                         self
                         (@selector #/sheetDidEnd:returnCode:contextInfo:)
                         (@selector #/sheetDidDismiss:returnCode:contextInfo:)
                         signal
                         message)))

(objc:defmethod (#/sheetDidEnd:returnCode:contextInfo: :void) ((self hemlock-frame))
 (declare (ignore sheet code info)))

(objc:defmethod (#/sheetDidDismiss:returnCode:contextInfo: :void)
    ((self hemlock-frame) sheet code info)
  (declare (ignore sheet code))
  (ccl::%signal-semaphore-ptr (%int-to-ptr (#/unsignedLongValue info))))
  
(defun report-condition-in-hemlock-frame (condition frame)
  (let* ((semaphore (make-semaphore))
         (message (nsstring-for-lisp-condition condition))
         (sem-value (make-instance 'ns:ns-number
                                   :with-unsigned-long (%ptr-to-int (semaphore.value semaphore)))))
    (%stack-block ((paramptrs (ash 2 target::word-shift)))
      (setf (%get-ptr paramptrs 0) message
            (%get-ptr paramptrs (ash 1 target::word-shift)) sem-value)
      (let* ((params (make-instance 'ns:ns-array
                                    :with-objects paramptrs
                                    :count 2))
             (*debug-io* *typeout-stream*))
        (stream-clear-output *debug-io*)
        (print-call-history :detailed-p nil)
        (#/performSelectorOnMainThread:withObject:waitUntilDone:
         frame (@selector #/runErrorSheet:) params t)
        (wait-on-semaphore semaphore)))))

(defun hi::report-hemlock-error (condition)
  (report-condition-in-hemlock-frame condition (#/window (hi::current-window))))
                       
                       
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


(objc:defmethod (#/close :void) ((self hemlock-frame))
  (let* ((content-view (#/contentView self))
         (subviews (#/subviews content-view)))
    (do* ((i (1- (#/count subviews)) (1- i)))
         ((< i 0))
      (#/removeFromSuperviewWithoutNeedingDisplay (#/objectAtIndex: subviews i))))
  (let* ((proc (slot-value self 'command-thread)))
    (when proc
      (setf (slot-value self 'command-thread) nil)
      (process-kill proc)))
  (let* ((buf (hemlock-frame-echo-area-buffer self))
         (echo-doc (if buf (hi::buffer-document buf))))
    (when echo-doc
      (setf (hemlock-frame-echo-area-buffer self) nil)
      (#/close echo-doc)))
  (release-canonical-nsobject self)
  (call-next-method))
  
(defun new-hemlock-document-window ()
  (let* ((w (new-cocoa-window :class hemlock-frame
                              :activate nil)))
      (values w (add-pane-to-window w :reserve-below 20.0))))



(defun add-pane-to-window (w &key (reserve-above 0.0f0) (reserve-below 0.0f0))
  (let* ((window-content-view (#/contentView w))
         (window-frame (#/frame window-content-view)))
    (ns:with-ns-rect (pane-rect  0 reserve-below (ns:ns-rect-width window-frame) (- (ns:ns-rect-height window-frame) (+ reserve-above reserve-below)))
      (let* ((pane (make-instance 'text-pane :with-frame pane-rect)))
        (#/addSubview: window-content-view pane)
        pane))))

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
  (let* ((string-len (#/length nsstring))
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
    (rlet ((remaining-range :ns-range :location 0 :length  1)
           (line-end-index :<NSUI>nteger)
           (contents-end-index :<NSUI>nteger))
      (do* ((number (+ (hi::line-number first-line) hi::line-increment)
                    (+ number hi::line-increment)))
           ((= line-start string-len)
            (let* ((line (hi::mark-line mark)))
              (hi::insert-string mark (make-string 0))
              (setf (hi::line-next previous) line
                    (hi::line-previous line) previous))
            nil)
        (setf (pref remaining-range :<NSR>ange.location) line-start)
        (#/getLineStart:end:contentsEnd:forRange:
         nsstring
         +null-ptr+
         line-end-index
         contents-end-index
         remaining-range)
        (let* ((contents-end (pref contents-end-index :<NSUI>nteger))
               (line-end (pref line-end-index :<NSUI>nteger))
               (chars (make-string (- contents-end line-start))))
          (do* ((i line-start (1+ i))
                (j 0 (1+ j)))
               ((= i contents-end))
            (setf (schar chars j) (code-char (#/characterAtIndex: nsstring i))))
          (unless first-line-terminator
            (let* ((terminator (code-char
                                (#/characterAtIndex: nsstring contents-end))))
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
          (setq line-start line-end))))
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
	 (data (make-instance 'ns:ns-data
                              :with-contents-of-file cocoa-pathname))
	 (string (make-instance 'ns:ns-string
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
         (frame (#/window pane))
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
  (#/beginEditing (slot-value document 'textstorage))
  #+all-in-cocoa-thread
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   (slot-value document 'textstorage)
   (@selector #/beginEditing)
   +null-ptr+
   t))

(defun document-edit-level (document)
  (slot-value (slot-value document 'textstorage) 'edit-count))

(defun hi::document-end-editing (document)
  #-all-in-cocoa-thread
  (#/endEditing (slot-value document 'textstorage))
  #+all-in-cocoa-thread
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   (slot-value document 'textstorage)
   (@selector #/endEditing)
   +null-ptr+
   t))

(defun hi::document-set-point-position (document)
  (declare (ignorable document))
  #+debug
  (#_NSLog #@"Document set point position called")
  (let* ((textstorage (slot-value document 'textstorage)))
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     textstorage (@selector #/updateHemlockSelection) +null-ptr+ t)))



(defun perform-edit-change-notification (textstorage selector pos n)
  (let* ((number-for-pos
          (#/initWithLong: (#/alloc ns:ns-number) pos))
         (number-for-n
          (#/initWithLong: (#/alloc ns:ns-number) n)))
    (rlet ((paramptrs (:array :id 2)))
      (setf (paref paramptrs (:* :id) 0) number-for-pos
            (paref paramptrs (:* :id) 1) number-for-n)
      (let* ((params (#/initWithObjects:count: (#/alloc ns:ns-array) paramptrs 2)))
        (#/performSelectorOnMainThread:withObject:waitUntilDone:
         textstorage selector params  t)
        (#/release params)
        (#/release number-for-n)
        (#/release number-for-pos)))))

(defun textstorage-note-insertion-at-position (textstorage pos n)
  #+debug
  (#_NSLog #@"insertion at position %d, len %d" :int pos :int n)
  (rlet ((range ns:ns-range :location pos :length 0))
    (#/edited:range:changeInLength:
     textstorage #$NSTextStorageEditedAttributes range n)
    (setf (ns:ns-range-length range) n)
    (#/edited:range:changeInLength:
     textstorage  #$NSTextStorageEditedCharacters range 0)))


(defun hi::buffer-note-font-change (buffer region)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage)))
           (pos (mark-absolute-position (hi::region-start region)))
           (n (- (mark-absolute-position (hi::region-end region)) pos)))
      (perform-edit-change-notification textstorage
                                        (@selector #/noteAttrChange:)
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
          (let* ((display (hemlock-buffer-string-cache (#/string textstorage))))
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
        (rlet ((range :ns-range) :location (mark-absolute-position mark) :length n)
          (#/edited:range:changeInLength:
           textstorage
           (logior #$NSTextStorageEditedCharacters
                          #$NSTextStorageEditedAttributes)
           range
           0))
        #+all-in-cocoa-thread
        (perform-edit-change-notification textstorage
                                          (@selector #/noteModification:)
                                          (mark-absolute-position mark)
                                          n)))))
  

(defun hi::buffer-note-deletion (buffer mark n)
  (when (hi::bufferp buffer)
    (let* ((document (hi::buffer-document buffer))
	   (textstorage (if document (slot-value document 'textstorage))))
      (when textstorage
        #-all-in-cocoa-thread
        (let* ((pos (mark-absolute-position mark)))
          (rlet ((range :ns-range :location pos :length n))
          (#/edited:range:changeInLength:
           textstorage #$NSTextStorageEditedCharacters range (- n)))
          (let* ((display (hemlock-buffer-string-cache (#/string textstorage))))
            (reset-buffer-cache display) 
            (update-line-cache-for-index display pos)))
        #+all-in-cocoa-thread
        (perform-edit-change-notification textstorage
                                          (@selector #/noteDeletion:)
                                          (mark-absolute-position mark)
                                          (abs n))))))

(defun hi::set-document-modified (document flag)
  (#/updateChangeCount: document (if flag #$NSChangeDone #$NSChangeCleared)))


(defmethod hi::document-panes ((document t))
  )



    

(defun size-of-char-in-font (f)
  (let* ((sf (#/screenFont f))
         (screen-p t))
    (if (%null-ptr-p sf) (setq sf f screen-p nil))
    (let* ((layout (#/autorelease (#/init (#/alloc ns:ns-layout-manager)))))
      (#/setUsesScreenFonts: layout screen-p)
      (values (fround (#/defaultLineHeightForFont: layout sf))
              (fround (ns:ns-size-width (#/advancementForGlyph: sf (#/glyphWithName: sf #@" "))))))))
         


(defun size-text-pane (pane char-height char-width nrows ncols)
  (let* ((tv (text-pane-text-view pane))
         (height (fceiling (* nrows char-height)))
	 (width (fceiling (* ncols char-width)))
	 (scrollview (text-pane-scroll-view pane))
	 (window (#/window scrollview))
         (has-horizontal-scroller (#/hasHorizontalScroller scrollview))
         (has-vertical-scroller (#/hasVerticalScroller scrollview)))
    (ns:with-ns-size (tv-size
                      (+ width (* 2 (#/lineFragmentPadding (#/textContainer tv))))
                      height)
      (when has-vertical-scroller 
	(#/setVerticalLineScroll: scrollview char-height)
	(#/setVerticalPageScroll: scrollview +cgfloat-zero+ #|char-height|#))
      (when has-horizontal-scroller
	(#/setHorizontalLineScroll: scrollview char-width)
	(#/setHorizontalPageScroll: scrollview +cgfloat-zero+ #|char-width|#))
      (let* ((sv-size (#/frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType: ns:ns-scroll-view tv-size has-horizontal-scroller has-vertical-scroller (#/borderType scrollview)))
             (pane-frame (#/frame pane))
             (margins (#/contentViewMargins pane)))
        (incf (ns:ns-size-height sv-size)
              (+ (ns:ns-rect-y pane-frame)
                 (* 2 (ns:ns-size-height  margins))))
        (incf (ns:ns-size-width sv-size)
              (ns:ns-size-width margins))
        (#/setContentSize: window sv-size)
        (#/setResizeIncrements: window
                                (ns:make-ns-size char-width char-height))))))
				    
  
(defclass hemlock-editor-window-controller (ns:ns-window-controller)
    ()
  (:metaclass ns:+ns-object))



;;; The HemlockEditorDocument class.


(defclass hemlock-editor-document (ns:ns-document)
    ((textstorage :foreign-type :id))
  (:metaclass ns:+ns-object))

(defmethod textview-background-color ((doc hemlock-editor-document))
  (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color
                                              (float *editor-background-red-component*
                                                     +cgfloat-zero+)
                                              (float *editor-background-green-component* +cgfloat-zero+)
                                              (float *editor-background-blue-component* +cgfloat-zero+)
                                              (float *editor-background-alpha-component* +cgfloat-zero+)))


(objc:defmethod (#/setTextStorage: :void) ((self hemlock-editor-document) ts)
  (let* ((doc (%inc-ptr self 0))        ; workaround for stack-consed self
         (string (#/string ts))
         (cache (hemlock-buffer-string-cache string))
         (buffer (buffer-cache-buffer cache)))
    (unless (%null-ptr-p doc)
      (setf (slot-value doc 'textstorage) ts
            (hi::buffer-document buffer) doc))))

;; This runs on the main thread.
(objc:defmethod (#/revertToSavedFromFile:ofType: :<BOOL>)
    ((self hemlock-editor-document) filename filetype)
  (declare (ignore filetype))
  #+debug
  (#_NSLog #@"revert to saved from file %@ of type %@"
           :id filename :id filetype)
  (let* ((data (make-instance ns:ns-data
                              :with-contents-of-file filename))
         (nsstring (make-instance ns:ns-string
                                  :with-data data
                                  :encoding #$NSASCIIStringEncoding))
         (buffer (hemlock-document-buffer self))
         (old-length (hemlock-buffer-length buffer))
         (hi::*buffer-gap-context* (hi::buffer-gap-context buffer))
         (textstorage (slot-value self 'textstorage))
         (point (hi::buffer-point buffer))
         (pointpos (mark-absolute-position point)))
    (#/beginEditing textstorage)
    (rlet ((changed :ns-range :location 0 :length old-length))
      (#/edited:range:changeInLength:
       textstorage #$NSTextStorageEditedCharacters changed (- old-length)))
    (nsstring-to-buffer nsstring buffer)
    (rletZ ((new-range :ns-range))
      (let* ((newlen (hemlock-buffer-length buffer)))
        (#/edited:range:changeInLength: textstorage  #$NSTextStorageEditedAttributes new-range newlen)
        (setf (ns:ns-range-length new-range) newlen)
        (#/edited:range:changeInLength: textstorage #$NSTextStorageEditedCharacters new-range 0)
        (let* ((ts-string (#/string textstorage))
               (display (hemlock-buffer-string-cache ts-string)))
          (reset-buffer-cache display) 
          (update-line-cache-for-index display 0)
          (move-hemlock-mark-to-absolute-position point
                                                  display
                                                  (min newlen pointpos))))
      (#/endEditing textstorage))
    (hi::document-set-point-position self)
    (setf (hi::buffer-modified buffer) nil)
    (hi::queue-buffer-change buffer)
    t))
         
            
  
(objc:defmethod #/init ((self hemlock-editor-document))
  (let* ((doc (call-next-method)))
    (unless  (%null-ptr-p doc)
      (#/setTextStorage: doc (make-textstorage-for-hemlock-buffer
                              (make-hemlock-buffer
                               (lisp-string-from-nsstring
                                (#/displayName doc))
                               :modes '("Lisp" "Editor")))))
    doc))
                     
(objc:defmethod (#/readFromFile:ofType: :<BOOL>)
    ((self hemlock-editor-document) filename type)
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
	 (data (make-instance 'ns:ns-data :with-contents-of-file filename))
	 (string (make-instance 'ns:ns-string
                                :with-data data
                                :encoding #$NSASCIIStringEncoding)))
    (hi::document-begin-editing self)
    (nsstring-to-buffer string buffer)
    (let* ((textstorage (slot-value self 'textstorage))
	   (display (hemlock-buffer-string-cache (#/string textstorage))))
      (reset-buffer-cache display) 
      (update-line-cache-for-index display 0)
      (textstorage-note-insertion-at-position
       textstorage
       0
       (hemlock-buffer-length buffer)))
    (hi::document-end-editing self)
    (setf (hi::buffer-modified buffer) nil)
    (hi::process-file-options buffer pathname)
    t))

#+experimental
(objc:defmethod (#/writeWithBackupToFile:ofType:saveOperation: :<BOOL>)
    ((self hemlock-editor-document) path type (save-operation :<NSS>ave<O>peration<T>ype))
  #+debug
  (#_NSLog #@"saving file to %@" :id path)
  (call-next-method path type save-operation))

;;; This should be a preference.
(objc:defmethod (#/keepBackupFile :<BOOL>) ((self hemlock-editor-document))
  t)


(defmethod hemlock-document-buffer (document)
  (let* ((string (#/string (slot-value document 'textstorage))))
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

(objc:defmethod #/dataRepresentationOfType: ((self hemlock-editor-document)
                                             type)
  (declare (ignorable type))
  (let* ((buffer (hemlock-document-buffer self)))
    (when buffer
      (setf (hi::buffer-modified buffer) nil)))
  (#/dataUsingEncoding:allowLossyConversion:
   (#/string (slot-value self 'textstorage)) #$NSASCIIStringEncoding t))


;;; Shadow the setFileName: method, so that we can keep the buffer
;;; name and pathname in synch with the document.
(objc:defmethod (#/setFileName: :void) ((self hemlock-editor-document)
                                        full-path)
  (call-next-method full-path)
  (let* ((buffer (hemlock-document-buffer self)))
    (when buffer
      (let* ((new-pathname (lisp-string-from-nsstring full-path)))
	(setf (hi::buffer-name buffer) (hi::pathname-to-buffer-name new-pathname))
	(setf (hi::buffer-pathname buffer) new-pathname)))))


(def-cocoa-default *initial-editor-x-pos* :float 200.0f0 "X position of upper-left corner of initial editor")

(def-cocoa-default *initial-editor-y-pos* :float 400.0f0 "Y position of upper-left corner of initial editor")

(defloadvar *next-editor-x-pos* nil) ; set after defaults initialized
(defloadvar *next-editor-y-pos* nil)

(objc:defmethod (#/makeWindowControllers :void) ((self hemlock-editor-document))
  #+debug
  (#_NSLog #@"Make window controllers")
  (let* ((window (%hemlock-frame-for-textstorage 
                                    (slot-value self 'textstorage)
				    *editor-columns*
				    *editor-rows*
				    nil
                                    (textview-background-color self)))
         (controller (make-instance
		      'hemlock-editor-window-controller
		      :with-window window)))
    (#/addWindowController: self controller)
    (#/release controller)
    (ns:with-ns-point  (current-point
                        (or *next-editor-x-pos*
                            *initial-editor-x-pos*)
                        (or *next-editor-y-pos*
                            *initial-editor-y-pos*))
      (let* ((new-point (#/cascadeTopLeftFromPoint: window current-point)))
        (setq *next-editor-x-pos* (ns:ns-point-x new-point)
              *next-editor-y-pos* (ns:ns-point-y new-point))))))


(objc:defmethod (#/close :void) ((self hemlock-editor-document))
  #+debug
  (#_NSLog #@"Document close: %@" :id self)
  (let* ((textstorage (slot-value self 'textstorage)))
    (unless (%null-ptr-p textstorage)
      (setf (slot-value self 'textstorage) (%null-ptr))
      (for-each-textview-using-storage
       textstorage
       #'(lambda (tv)
           (let* ((layout (#/layoutManager tv)))
             (#/setBackgroundLayoutEnabled: layout nil))))
      (close-hemlock-textstorage textstorage)))
  (call-next-method))


(defun initialize-user-interface ()
  (#/sharedPanel preferences-panel)
  (update-cocoa-defaults)
  (make-editor-style-map))

(defun hi::scroll-window (textpane n)
  (declare (ignore textpane))
  (let* ((point (hi::current-point)))
    (or (hi::line-offset point (if (and n (< n 0)) -24 24) 0))))

(defmethod hemlock::center-text-pane ((pane text-pane))
  (#/centerSelectionInVisibleArea: (text-pane-text-view pane) +null-ptr+))


(defun hi::open-document ()
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   (#/sharedDocumentController ns:ns-document-controller)
   (@selector #/openDocument:) +null-ptr+ t))
  
(defmethod hi::save-hemlock-document ((self hemlock-editor-document))
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   self (@selector #/saveDocument:) +null-ptr+ t))


(defmethod hi::save-hemlock-document-as ((self hemlock-editor-document))
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   self (@selector #/saveDocumentAs:) +null-ptr+ t))

;;; This needs to run on the main thread.
(objc:defmethod (#/updateHemlockSelection :void) ((self hemlock-text-storage))
  (let* ((string (#/string self))
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
         (#/updateSelection:length:affinity: tv location len (if (eql location 0) #$NSSelectionAffinityUpstream #$NSSelectionAffinityDownstream))))))


(defun hi::allocate-temporary-object-pool ()
  (create-autorelease-pool))

(defun hi::free-temporary-objects (pool)
  (release-autorelease-pool pool))

(provide "COCOA-EDITOR")
