; -*- Mode: Lisp; Package: CCL; -*-

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa))

(defclass sequence-window-controller (ns:ns-window-controller)
    ((table-view :foreign-type :id :reader sequence-window-controller-table-view)
     (sequence :initform nil :initarg :sequence :type sequence :reader sequence-window-controller-sequence)
     (result-callback :initarg :result-callback)
     (display :initform #'(lambda (item stream) (prin1 item stream)) :initarg :display)
     (title :initform "Sequence dialog" :initarg :title))
  (:metaclass ns:+ns-object))


(objc:defmethod #/init ((self sequence-window-controller))
  (let* ((w (new-cocoa-window :activate nil))
         (contentview (#/contentView w))
         (contentframe (#/frame contentview))
         (scrollview (make-instance 'ns:ns-scroll-view :with-frame contentframe)))
    (#/setWindow: self w)
    (#/setHasVerticalScroller: scrollview t)
    (#/setHasHorizontalScroller: scrollview t)
    (#/setRulersVisible: scrollview nil)
    (#/setAutoresizingMask: scrollview (logior
                                        #$NSViewWidthSizable
                                        #$NSViewHeightSizable))
    (#/setAutoresizesSubviews: (#/contentView scrollview) t)
    (let* ((table-view (make-instance 'ns:ns-table-view)))
      (#/setDocumentView: scrollview table-view)
      (setf (slot-value self 'table-view) table-view)
      (let* ((column (make-instance 'ns:ns-table-column :with-identifier #@"")))
        (#/setEditable: column nil)
        (#/addTableColumn: table-view column))
      (#/setAutoresizingMask: table-view (logior
                                          #$NSViewWidthSizable
                                          #$NSViewHeightSizable))
      (#/sizeToFit table-view)
      (#/setDataSource: table-view self)
      (#/setTarget: table-view self)
      (#/setHeaderView: table-view +null-ptr+)
      (#/setUsesAlternatingRowBackgroundColors: table-view t)
      (#/setDoubleAction: table-view (@selector #/sequenceDoubleClick:))
      (#/addSubview: contentview scrollview)
      self)))

(objc:defmethod (#/sequenceDoubleClick: :void)
    ((self sequence-window-controller) sender)
  (let* ((n (#/clickedRow sender)))
    (when (>= n 0)
      (with-slots (sequence result-callback) self
        (funcall result-callback (elt sequence n))))))

(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
    ((self sequence-window-controller) view)
  (declare (ignore view))
  (length (slot-value self 'sequence)))


(objc:defmethod #/tableView:objectValueForTableColumn:row:
    ((self sequence-window-controller) view column (row :<NSI>nteger))
  (declare (ignore column view))
  (with-slots (display sequence) self
    (%make-nsstring (with-output-to-string (s)
                      (funcall display (elt sequence row) s)))))

(defmethod initialize-instance :after ((self sequence-window-controller) &key &allow-other-keys)
  (let* ((window (#/window self)))
    (with-slots (title) self
      (when title (#/setTitle: window (%make-nsstring title))))
    (#/reloadData (sequence-window-controller-table-view self))
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     self
     (@selector #/showWindow:)
     +null-ptr+
     nil)))

;;; Looks like a "util" to me ...
(defun pathname-to-url (pathname)
  (make-instance 'ns:ns-url
                 :file-url-with-path
                 (%make-nsstring (native-translated-namestring pathname))))

(defun color-values-to-nscolor (red green blue alpha)
  (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color
                                              (float red +cgfloat-zero+)
                                              (float green +cgfloat-zero+)
                                              (float blue +cgfloat-zero+)
                                              (float alpha +cgfloat-zero+)))
