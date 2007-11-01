(in-package "CCL")

(defclass apropos-window-controller (ns:ns-window-controller)
  ((apropos-array :foreign-type :id :initform +null-ptr+
		  :reader apropos-array
		  :documentation "Bound to NSArrayController in nib file")
   (table-view :foreign-type :id :accessor table-view)
   (symbols :initform nil :accessor symbols
	    :documentation "List of symbols being displayed")
   (previous-input :initform nil :accessor previous-input
		   :documentation "Last string entered"))
  (:metaclass ns:+ns-object))

(defmethod (setf apropos-array) (value (self apropos-window-controller))
  (with-slots (apropos-array) self
    (unless (eql value apropos-array)
      (#/release apropos-array))
    (setf apropos-array (#/retain value))))

(objc:defmethod (#/awakeFromNib :void) ((self apropos-window-controller))
  (#/setDoubleAction: (table-view self) (@selector #/inspectSelectedSymbol:)))

(objc:defmethod #/init ((self apropos-window-controller))
  (prog1
      (#/initWithWindowNibName: self #@"apropos")
    (#/setShouldCascadeWindows: self nil)
    (#/setWindowFrameAutosaveName: self #@"apropos panel")
    (setf (apropos-array self) (#/array ns:ns-mutable-array))))

(objc:defmethod (#/dealloc :void) ((self apropos-window-controller))
  (#/release (slot-value self 'apropos-array))
  (call-next-method))

(objc:defmethod (#/apropos: :void) ((self apropos-window-controller) sender)
  (let* ((input (lisp-string-from-nsstring (#/stringValue sender)))
	 (array (#/mutableArrayValueForKey: self #@"aproposArray")))
    (when (and (plusp (length input))
	       (not (string-equal input (previous-input self))))
      (setf (previous-input self) input)
      (#/removeAllObjects array)
      (flet ((%make-nsstring-with-highlighted-range (s start len)
	       (let* ((output (make-instance 'ns:ns-mutable-attributed-string
					     :with-string (#/autorelease
							   (%make-nsstring s))))
		      (range (ns:make-ns-range start len)))
		 (#/applyFontTraits:range: output #$NSBoldFontMask range)
		 output)))
	(setf (symbols self)
	      (mapc #'(lambda (x)
			(let* ((pkg-name (package-name (symbol-package x)))
			       (sym-name (symbol-name x))
			       (pos (search input sym-name :test #'string-equal)))
			  (#/addObject: array (#/dictionaryWithObjectsAndKeys:
					       ns:ns-dictionary
					       (#/autorelease
						(%make-nsstring pkg-name))
					       #@"package"
					       (if (numberp pos)
						 (#/autorelease
						  (%make-nsstring-with-highlighted-range
						   sym-name pos (length input)))
						 (#/autorelease
						  (%make-nsstring sym-name)))
					       #@"symbol"
					       +null-ptr+))))
		    (apropos-list input)))))))

(objc:defmethod (#/inspectSelectedSymbol: :void) ((self apropos-window-controller) sender)
  (with-accessors ((symbols symbols)) self
    (let* ((row (#/clickedRow sender)))
      (unless (minusp row)
	(cinspect (nth row symbols))))))
