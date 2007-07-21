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


(defloadvar *lisp-preferences-panel* nil)

(defclass lisp-preferences-panel (ns:ns-panel)
    ()
  (:metaclass ns:+ns-object))

(defclass font-name-transformer (ns:ns-value-transformer)
    ()
  (:metaclass ns:+ns-object))

(objc:defmethod #/transformedNameClass ((self +font-name-transformer))
  ns:ns-string)


(objc:defmethod (#/allowsReverseTransformation :<BOOL>)
    ((self +font-name-transformer))
  nil)

(objc:defmethod #/transformValue ((self font-name-transformer) value)
  ;; Is there any better way of doing this that doesn't involve
  ;; making a font ?
  (#/displayName (make-instance ns:ns-font
                                :with-name value
                                :size (float 12.0 +cgfloat-zero+))))



(defclass lisp-preferences-window-controller (ns:ns-window-controller)
    ((selected-font-index :foreign-type :int))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/fontPanelForDefaultFont: :void)
    ((self lisp-preferences-window-controller) sender)
  (with-slots (selected-font-index) self
    (setq selected-font-index 1))
  (#/orderFrontFontPanel: *NSApp* sender))


(objc:defmethod (#/fontPanelForModelineFont: :void)
    ((self lisp-preferences-window-controller) sender)
  (with-slots (selected-font-index) self
    (setq selected-font-index 2))
  (#/orderFrontFontPanel: *NSApp* sender))

(objc:defmethod (#/changeFont: :void) ((self lisp-preferences-window-controller) sender)
  #+debug (#_NSLog #@"ChangeFont.")
  (with-slots ((idx selected-font-index)) self
    (when (> idx 0)
      (let* ((f (#/convertFont: sender (default-font))))
        (when (is-fixed-pitch-font f)
          (let* ((values (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))))
            (#/setValue:forKey: values (#/fontName f) (if (eql 1 idx) #@"defaultFontName" #@"modelineFontName:"))
            (#/setValue:forKey: values (#/stringWithFormat: ns:ns-string #@"%u" (round (#/pointSize f))) (if (eql 1 idx) #@"defaultFontSize" #@"modelineFontSize"))))))))


(objc:defmethod (#/changeColor: :void) ((self lisp-preferences-panel)
                                        sender)
  (declare (ignore sender)))


(objc:defmethod (#/selectHyperspecFileURL: :void)
    ((self lisp-preferences-window-controller)
     sender)
  (declare (ignore sender))
  (let* ((panel (make-instance 'ns:ns-open-panel))
         (values (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))))
    (#/setAllowsMultipleSelection: panel nil)
    (#/setCanChooseDirectories: panel t)
    (#/setCanChooseFiles: panel nil)
    (when (eql
           (#/runModalForDirectory:file:types:
            panel
            (#/valueForKey: values #@"hyperspecFileURLString")
            +null-ptr+
            +null-ptr+)
           #$NSOKButton)
      (let* ((filename (#/objectAtIndex: (#/filenames panel) 0)))
        (#/setValue:forKey: values filename #@"hyperspecFileURLString")))))

(objc:defmethod (#/selectCCLdirectory: :void)
    ((self lisp-preferences-window-controller)
     sender)
  (declare (ignore sender))
  (let* ((panel (make-instance 'ns:ns-open-panel))
         (values (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))))
    (#/setAllowsMultipleSelection: panel nil)
    (#/setCanChooseDirectories: panel t)
    (#/setCanChooseFiles: panel nil)
    (when (eql
           (#/runModalForDirectory:file:types:
            panel
            (#/valueForKey: values #@"cclDirectory")
            +null-ptr+
            +null-ptr+)
           #$NSOKButton)
      (let* ((filename (#/pathWithComponents: ns:ns-string
                                              (#/arrayWithObjects:
                                               ns:ns-array
                                               (#/objectAtIndex: (#/filenames panel) 0)
                                               #@""
                                               +null-ptr+))))
        (#/setValue:forKey: values filename #@"cclDirectory")))))



(objc:defmethod #/sharedPanel ((self +lisp-preferences-panel))
  (cond (*lisp-preferences-panel*)
        (t
         (let* ((domain (#/standardUserDefaults ns:ns-user-defaults))
                (initial-values (cocoa-defaults-initial-values)))
           (#/registerDefaults: domain initial-values)
           (update-cocoa-defaults)
           (#/setValueTransformer:forName:
            ns:ns-value-transformer
            (make-instance 'font-name-transformer)
            #@"FontNameTransformer")
           (let* ((sdc (#/sharedUserDefaultsController ns:ns-user-defaults-controller)))
             (#/setAppliesImmediately: sdc nil)
             (#/setInitialValues: sdc initial-values)
             (let* ((controller (make-instance lisp-preferences-window-controller
                                             :with-window-nib-name #@"preferences"))
                  (window (#/window controller)))
               (unless (%null-ptr-p window)
                 (#/setFloatingPanel: window t)
                 (#/addObserver:selector:name:object:
                  (#/defaultCenter ns:ns-notification-center)
                  controller
                  (@selector #/defaultsChanged:)
                  #&NSUserDefaultsDidChangeNotification
                  (#/standardUserDefaults ns:ns-user-defaults))
                 (setq *lisp-preferences-panel* window))))))))

  
(objc:defmethod #/init ((self lisp-preferences-panel))
  (let* ((class (class-of self)))
    (#/dealloc self)
    (#/sharedPanel class)))


(objc:defmethod (#/makeKeyAndOrderFront: :void)
    ((self lisp-preferences-panel) sender)
  (let* ((color-panel (#/sharedColorPanel ns:ns-color-panel)))
    (#/close color-panel)
    (#/setAction: color-panel +null-ptr+)
    (#/setShowsAlpha: color-panel t))
  (call-next-method sender))

(objc:defmethod (#/show :void) ((self lisp-preferences-panel))
  (#/makeKeyAndOrderFront: self +null-ptr+))

(objc:defmethod (#/defaultsChanged: :void)
    ((self lisp-preferences-window-controller)
     notification)
  (declare (ignore notification))
  (update-cocoa-defaults))
  


