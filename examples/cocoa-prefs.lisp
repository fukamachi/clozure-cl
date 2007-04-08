;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2004 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))



(defclass prefs-view (ns:ns-view)
    ((form :foreign-type :id :accessor prefs-view-form)
     (nvalues :foreign-type :int :accessor prefs-view-nvalues)
     (nchanges :foreign-type :int :accessor prefs-view-nchanges)
     (revert-button :foreign-type :id :accessor prefs-view-revert-button)
     (commit-button :foreign-type :id :accessor prefs-view-commit-button)
     (scroll-view :foreign-type :id :reader prefs-view-scroll-view)
     (domain :foreign-type :id
             :accessor prefs-view-domain)
     (defaults-vector :initform nil :accessor prefs-view-defaults-vector))
  (:metaclass ns:+ns-object))


(defmethod set-prefs-cell-from-default ((self prefs-view) cell default form val index)
  (let* ((doc (cocoa-default-doc default))
         (type (cocoa-default-type default)))
    (#/setTag: cell index)
    (#/setStringValue: cell val)
    (when doc
      (#/setToolTip:forCell: form (%make-nsstring doc) cell))
    (case type
      (:int
       (#/setEntryType: cell #$NSIntType)
       '(#/setAlignment: cell #$NSRightTextAlignment))
      (:float
       (#/setEntryType: cell #$NSFloatType)
       '(#/setAlignment: cell #$NSRightTextAlignment))
      (t
       (#/setScrollable: cell t)))
    (#/setAction: cell (@selector #/notePrefsChange:))
    (#/setTarget: cell self)))

(defmethod create-prefs-view-form ((self prefs-view))
  (let* ((scrollview (prefs-view-scroll-view self))
         (contentsize (#/contentSize scrollview)))
    (ns:with-ns-rect (form-frame 0 0 (ns:ns-size-width contentsize) (ns:ns-size-height contentsize))
      (ns:with-ns-size (intercell-spacing-size 1 4)
        (ns:with-ns-size (cell-size 500 22)
          (let* ((form (make-instance 'ns:ns-form :with-frame form-frame)))
            (#/setScrollable: form t)
            (#/setIntercellSpacing: form intercell-spacing-size)
            (#/setCellSize: form cell-size)
            (setf (prefs-view-form self) form)
            (#/setDocumentView: scrollview form)
            form))))))

(defmethod init-prefs-form-from-defaults ((self prefs-view))
  (let* ((defaults (setf (prefs-view-defaults-vector self)
                         (apply #'vector (reverse (cocoa-defaults)))))
         (form (create-prefs-view-form self))
         (domain (setf (prefs-view-domain self) (#/standardUserDefaults ns:ns-user-defaults)))
         (n (length defaults)))
    (setf (prefs-view-nvalues self) n)
    (dotimes (i n)
      (let* ((d (svref defaults i))
             (key (objc-constant-string-nsstringptr (cocoa-default-string d)))
             (val (#/objectForKey: domain key)))
        (when (%null-ptr-p val)
          (#/setObject:forKey:
           domain (setq val (%make-nsstring (format nil "~a" (cocoa-default-value d)))) key))
        (set-prefs-cell-from-default self
                                     (#/addEntry: form key)
                                     d
                                     form
                                     val
                                     i)))
    (setf (prefs-view-nchanges self) 0)
    (#/setEnabled: (prefs-view-revert-button self) nil)
    (#/setEnabled: (prefs-view-commit-button self) nil)
    (#/sizeToCells form)))

(objc:defmethod (#/notePrefsChange: :void) ((self prefs-view) form)
  (let* ((cell (#/cellAtIndex: form (#/indexOfSelectedItem form)))
         (n (prefs-view-nvalues self))
         (form (prefs-view-form self))
         (current (#/tag  cell))
         (d (svref (prefs-view-defaults-vector self) current))
         (next (mod (1+ current) n))
         (value (#/stringValue cell)))
    (unless (#/isEqualTo: value
                          (#/objectForKey: (prefs-view-domain self)
                                           (objc-constant-string-nsstringptr (cocoa-default-string d))))
      ;; If there's a constraint, sanity-check the value.
      (when (zerop (prefs-view-nchanges self))
        (#/setEnabled: (prefs-view-commit-button self) t)
        (#/setEnabled:  (prefs-view-revert-button self) t))
      (incf (prefs-view-nchanges self)))
    (#/selectCell: form (#/cellAtIndex: form next))))

(objc:defmethod (#/commitPrefs: :void) ((self prefs-view) sender)
  (declare (ignore sender))
  (let* ((form (prefs-view-form self))
         (domain (prefs-view-domain self)))
    (dotimes (i (prefs-view-nvalues self))
      (let* ((cell (#/cellAtIndex: form i))
             (key (#/title  cell))
             (val (#/stringValue  cell)))
        (#/setObject:forKey: domain val key)))
    (#/synchronize domain)
    (setf (prefs-view-nchanges self) 0)
    (#/setEnabled: (prefs-view-revert-button self) nil)
    (#/setEnabled: (prefs-view-commit-button self) nil)
    (update-cocoa-defaults-vector domain (prefs-view-defaults-vector self))))

(objc:defmethod (#/revertPrefs: :void) ((self prefs-view) sender)
  (declare (ignore sender))
  (let* ((form (prefs-view-form self))
         (domain (prefs-view-domain self)))
    (dotimes (i (prefs-view-nvalues self))
      (let* ((cell (#/cellAtIndex: form i))
             (key (#/title cell)))
        (#/setStringValue: cell (#/objectForKey: domain key))))
    (setf (prefs-view-nchanges self) 0)
    (#/setEnabled: (prefs-view-revert-button self) nil)
    (#/setEnabled: (prefs-view-commit-button self) nil)))

  
(objc:defmethod #/initWithFrame: ((self prefs-view) (frame :<NSR>ect))
  (call-next-method frame)
  (ns:with-ns-rect (scroll-frame 20 40 (- (ns:ns-rect-width frame) 40) (- (ns:ns-rect-height frame) 60))
    (let* ((scrollview (make-instance 'ns:ns-scroll-view
                                      :with-frame scroll-frame))
           (scroll-content (#/contentView scrollview))) 
      (#/setBorderType: scrollview #$NSBezelBorder)
      (#/setHasVerticalScroller: scrollview t)
      (#/setHasHorizontalScroller: scrollview t)
      (#/setRulersVisible: scrollview nil)
      (#/setAutoresizingMask: scrollview (logior
                                          #$NSViewWidthSizable
                                          #$NSViewHeightSizable))
      (#/setAutoresizesSubviews: scroll-content t)
      (setf (slot-value self 'scroll-view) scrollview)
      (ns:with-ns-rect (revert-frame 20 10 80 20)
        (ns:with-ns-rect (commit-frame (- (+ (ns:ns-rect-x frame)
                                             (ns:ns-rect-width frame)
                                             (+ 80.0f0 20.0f0)))
                                       10 80 20)
        (let* ((commit-button (make-instance
                               'ns:ns-button
                               :with-frame commit-frame))
               (revert-button (make-instance
                               'ns:ns-button
                               :with-frame revert-frame)))
          (#/setTitle: commit-button #@"Commit")
          (#/setTitle: revert-button #@"Revert")
          (#/setEnabled: commit-button nil)
          (#/setEnabled: revert-button nil)
          (#/setAction: commit-button (@selector "commitPrefs:"))
          (#/setTarget: commit-button self)
          (#/setAction: revert-button (@selector "revertPrefs:"))
          (#/setTarget: revert-button self)
          (#/setAutoresizingMask: commit-button #$NSViewMinXMargin)
          (#/setAutoresizingMask: revert-button #$NSViewMaxXMargin)
          (#/setBezelStyle: revert-button #$NSRoundedBezelStyle)
          (#/setBezelStyle: commit-button #$NSRoundedBezelStyle)
          (setf (prefs-view-revert-button self) revert-button
                (prefs-view-commit-button self) commit-button)
          (#/addSubview: self revert-button)
          (#/addSubview: self commit-button)
          (#/addSubview: self scrollview)
          self))))))

(defloadvar *preferences-panel* nil)

(defclass preferences-panel (ns:ns-panel)
    ((prefs-view :foreign-type :id :accessor preferences-panel-prefs-view))
  (:metaclass ns:+ns-object))

(objc:defmethod #/sharedPanel ((self +preferences-panel))
  (cond (*preferences-panel*)
        (t
         (let* ((panel (new-cocoa-window :class self
                                         :title "Preferences"
                                         :activate nil))
                (view (#/contentView panel))
                (bounds (#/bounds view))
                (v (make-instance 'prefs-view :with-frame bounds)))
           (#/setContentView: panel v)
           (#/setNeedsDisplay: v t)
           (setf (slot-value panel 'prefs-view) v)
           (setq *preferences-panel* panel)))))

(objc:defmethod #/init ((self preferences-panel))
  (let* ((class (class-of self)))
    (#/dealloc self)
    (#/sharedPanel class)))

(objc:defmethod (#/show :void) ((self preferences-panel))
  (init-prefs-form-from-defaults (preferences-panel-prefs-view self))
  (#/makeKeyAndOrderFront: self +null-ptr+))

