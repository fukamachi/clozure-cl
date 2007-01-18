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
    (send cell :set-tag index)
          (send cell :set-string-value val)
          (when doc
            (send form
                  :set-tool-tip (%make-nsstring doc)
                  :for-cell cell))
          (case type
            (:int
             (send cell :set-entry-type #$NSIntType)
             '(send cell :set-alignment #$NSRightTextAlignment))
            (:float
             (send cell :set-entry-type #$NSFloatType)
             '(send cell :set-alignment #$NSRightTextAlignment))
            (t
             (send cell :set-scrollable t)))
          (send cell :set-action (@selector "notePrefsChange:"))
          (send cell :set-target self)))

(defmethod create-prefs-view-form ((self prefs-view))
  (let* ((scrollview (prefs-view-scroll-view self)))
    (slet* ((contentsize (send scrollview 'content-size))
            (form-frame (ns-make-rect
                         +cgfloat-zero+
                         +cgfloat-zero+
                         (pref contentsize :<NSS>ize.width)
                         (pref contentsize :<NSS>ize.height))))
      (let* ((form (make-objc-instance 'ns:ns-form :with-frame form-frame)))
        (send form :set-scrollable t)
        (send form :set-intercell-spacing (ns-make-size (float 1.0f0 +cgfloat-zero+) (float 4.0f0 +cgfloat-zero+)))
        (send form :set-cell-size (ns-make-size (float 500.0f0 +cgfloat-zero+) (float 22.0f0 +cgfloat-zero+)))
        (setf (prefs-view-form self) form)
        (send scrollview :set-document-view form)
        form))))

(defmethod init-prefs-form-from-defaults ((self prefs-view))
  (let* ((defaults (setf (prefs-view-defaults-vector self)
                         (apply #'vector (reverse (cocoa-defaults)))))
         (form (create-prefs-view-form self))
         (domain (setf (prefs-view-domain self) (send (@class "NSUserDefaults") 'standard-user-defaults)))
         (n (length defaults)))
    (setf (prefs-view-nvalues self) n)
    (dotimes (i n)
      (let* ((d (svref defaults i))
             (key (objc-constant-string-nsstringptr (cocoa-default-string d)))
             (val (send domain :object-for-key key)))
        (when (%null-ptr-p val)
          (send domain
                :set-object (setq val (%make-nsstring (format nil "~a" (cocoa-default-value d))))
                :for-key key))
        (set-prefs-cell-from-default self
                                     (send form :add-entry key)
                                     d
                                     form
                                     val
                                     i)))
    (setf (prefs-view-nchanges self) 0)
    (send (prefs-view-revert-button self) :set-enabled nil)
    (send (prefs-view-commit-button self) :set-enabled nil)
    (send form 'size-to-cells)))

(define-objc-method ((:void :note-prefs-change form) prefs-view)
  (let* ((cell (send form :cell-at-index (send form 'index-of-selected-item)))
         (n (prefs-view-nvalues self))
         (form (prefs-view-form self))
         (current (send cell 'tag))
         (d (svref (prefs-view-defaults-vector self) current))
         (next (mod (1+ current) n))
         (value (send cell 'string-value)))
    (unless (send value
                  :is-equal-to
                  (send (prefs-view-domain self)
                        :object-for-key
                        (objc-constant-string-nsstringptr (cocoa-default-string d))))
      ;; If there's a constraint, sanity-check the value.
      (when (zerop (prefs-view-nchanges self))
        (send (prefs-view-commit-button self) :set-enabled t)
        (send (prefs-view-revert-button self) :set-enabled t))
      (incf (prefs-view-nchanges self)))
    (send form :select-cell (send form :cell-at-index next))))

(define-objc-method ((:void :commit-prefs sender) prefs-view)
  (declare (ignore sender))
  (let* ((form (prefs-view-form self))
         (domain (prefs-view-domain self)))
    (dotimes (i (prefs-view-nvalues self))
      (let* ((cell (send form :cell-at-index i))
             (key (send cell 'title))
             (val (send cell 'string-value)))
        (send domain :set-object val :for-key key)))
    (send domain 'synchronize)
    (setf (prefs-view-nchanges self) 0)
    (send (prefs-view-revert-button self) :set-enabled nil)
    (send (prefs-view-commit-button self) :set-enabled nil)
    (update-cocoa-defaults-vector domain (prefs-view-defaults-vector self))))

(define-objc-method ((:void :revert-prefs sender) prefs-view)
  (declare (ignore sender))
  (let* ((form (prefs-view-form self))
         (domain (prefs-view-domain self)))
    (dotimes (i (prefs-view-nvalues self))
      (let* ((cell (send form :cell-at-index i))
             (key (send cell 'title)))
        (send cell :set-string-value (send domain :object-for-key key))))
    (setf (prefs-view-nchanges self) 0)
    (send (prefs-view-revert-button self) :set-enabled nil)
    (send (prefs-view-commit-button self) :set-enabled nil)))

  
(define-objc-method ((:id :init-with-frame (:<NSR>ect frame))
                     prefs-view)
    (send-super :init-with-frame frame)
  (slet ((scroll-frame (ns-make-rect (float 20.0f0 +cgfloat-zero+)
                                     (float 40.0f0 +cgfloat-zero+)
                                     (- (pref frame :<NSR>ect.size.width) 40.0f0)
                                     (- (pref frame :<NSR>ect.size.height) 60.0f0))))
    (let* ((scrollview (make-objc-instance 'ns:ns-scroll-view
                                           :with-frame scroll-frame))
           (scroll-content (send scrollview 'content-view))) 
      (send scrollview :set-border-type #$NSBezelBorder)
      (send scrollview :set-has-vertical-scroller t)
      (send scrollview :set-has-horizontal-scroller t)
      (send scrollview :set-rulers-visible nil)
      (send scrollview :set-autoresizing-mask (logior
					       #$NSViewWidthSizable
					       #$NSViewHeightSizable))
      (send scroll-content :set-autoresizes-subviews t)
      (setf (slot-value self 'scroll-view) scrollview)
      (slet ((revert-frame (ns-make-rect (float 20.0f0 +cgfloat-zero+)
                                         (float 10.0f0 +cgfloat-zero+)
                                         (float 80.0f0 +cgfloat-zero+)
                                         (float 20.0f0 +cgfloat-zero+)))
             (commit-frame (ns-make-rect (- (+ (pref frame :<NSR>ect.origin.x)
                                               (pref frame :<NSR>ect.size.width))
                                            (+ 80.0f0 20.0f0))
                                         (float 10.0f0 +cgfloat-zero+)
                                         (float 80.0f0 +cgfloat-zero+)
                                         (float 20.0f0 +cgfloat-zero+))))
        (let* ((commit-button (make-objc-instance
                               'ns:ns-button
                               :with-frame commit-frame))
               (revert-button (make-objc-instance
                               'ns:ns-button
                               :with-frame revert-frame)))
          (send commit-button :set-title #@"Commit")
          (send revert-button :set-title #@"Revert")
          (send commit-button :set-enabled nil)
          (send revert-button :set-enabled nil)
          (send commit-button :set-action (@selector "commitPrefs:"))
          (send commit-button :set-target self)
          (send revert-button :set-action (@selector "revertPrefs:"))
          (send revert-button :set-target self)
          (send commit-button :set-autoresizing-mask #$NSViewMinXMargin)
          (send revert-button :set-autoresizing-mask #$NSViewMaxXMargin)
          (send revert-button :set-bezel-style #$NSRoundedBezelStyle)
          (send commit-button :set-bezel-style #$NSRoundedBezelStyle)
          (setf (prefs-view-revert-button self) revert-button
                (prefs-view-commit-button self) commit-button)
          (send self :add-subview revert-button)
          (send self :add-subview commit-button)
          (send self :add-subview scrollview)
          self)))))

(defloadvar *preferences-panel* nil)

(defclass preferences-panel (ns:ns-panel)
    ((prefs-view :foreign-type :id :accessor preferences-panel-prefs-view))
  (:metaclass ns:+ns-object))

(define-objc-class-method ((:id shared-panel) preferences-panel)
  (cond (*preferences-panel*)
        (t
         (let* ((panel (new-cocoa-window :class self
                                         :title "Preferences"
                                         :activate nil)))
           (slet ((bounds (send (send panel 'content-view) 'bounds)))
             (let* ((v (make-instance 'prefs-view :with-frame bounds)))
               (send panel :set-content-view v)
               (send v :set-needs-display t)
               (setf (slot-value panel 'prefs-view) v)
               (setq *preferences-panel* panel)))))))

(define-objc-method ((:id init) preferences-panel)
  (let* ((class (class-of self)))
    (send self 'dealloc)
    (send class 'shared-panel)))

(define-objc-method ((:void show) preferences-panel)
  (init-prefs-form-from-defaults (preferences-panel-prefs-view self))
  (send self :make-key-and-order-front (%null-ptr)))
