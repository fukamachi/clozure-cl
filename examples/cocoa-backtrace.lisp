; -*- Mode: Lisp; Package: CCL; -*-

(in-package "CCL")

(defclass ns-lisp-string (ns:ns-string)
    ((lisp-string :initarg :string :reader ns-lisp-string-string))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/length :<NSUI>nteger) ((self ns-lisp-string))
    (length (ns-lisp-string-string self)))

(objc:defmethod (#/characterAtIndex: :unichar) ((self ns-lisp-string) (index :<NSUI>nteger))
  (char-code (schar (ns-lisp-string-string self) index)))

(defclass frame-label (ns-lisp-string)
    ((frame-number :initarg :frame-number :foreign-type :int :accessor frame-label-number)
     (controller :initarg :controller :foreign-type :id :reader frame-label-controller)
     (frame-inspector :initform nil :accessor frame-label-frame-inspector))
  (:metaclass ns:+ns-object))

(defclass frame-item (ns-lisp-string)
    ((frame-label :initarg :frame-label :foreign-type :id :accessor frame-item-label)
     (index :initarg :index :foreign-type :int :accessor frame-item-index))
  (:metaclass ns:+ns-object))


(defclass backtrace-window-controller (ns:ns-window-controller)
    ((context :initarg :context :reader backtrace-controller-context)
     (inspector :initform nil :reader backtrace-controller-inspector)
     (outline-view :foreign-type :id :reader backtrace-controller-outline-view))
  (:metaclass ns:+ns-object))

(objc:defmethod #/windowNibName ((self backtrace-window-controller))
  #@"backtrace")

(objc:defmethod (#/close :void) ((self backtrace-window-controller))
  (setf (slot-value self 'context) nil)
  (call-next-method))

(defmethod our-frame-label-p ((self backtrace-window-controller) thing)
  (and (typep thing 'frame-label)
       (eql self (frame-label-controller thing))))

(objc:defmethod (#/windowDidLoad :void) ((self backtrace-window-controller))
  (let* ((outline (slot-value self 'outline-view))
         (font (default-font :name "Monaco" :size 12)))
    (unless (%null-ptr-p outline)
      (let* ((columns (#/tableColumns outline)))
        (dotimes (i (#/count columns))
          (let* ((column (#/objectAtIndex:  columns i))
                 (data-cell (#/dataCell column)))
            (#/setFont: data-cell font)
            (when (eql i 0)
              (let* ((header-cell (#/headerCell column))
                     (inspector (backtrace-controller-inspector self))
                     (break-condition
                      (inspector::break-condition
                                 (inspector::inspector-object inspector)))
                     (break-condition-string
                      (let* ((*print-level* 5)
                             (*print-length* 5)
                             (*print-circle* t))
                        (format nil "~a: ~a"
                                (class-name (class-of break-condition))
                                break-condition))))
                      
                (#/setFont: header-cell (default-font :attributes '(:bold)))
                (#/setStringValue: header-cell (%make-nsstring break-condition-string))))))))
    (let* ((window (#/window  self)))
      (unless (%null-ptr-p window)
        (let* ((context (backtrace-controller-context self))
               (process (tcr->process (bt.tcr context))))
          (#/setTitle:  window (%make-nsstring
                                (format nil "Backtrace for ~a(~d), break level ~d"
                                        (process-name process)
                                        (process-serial-number process)
                                        (bt.break-level context)))))))))

(objc:defmethod (#/outlineView:isItemExpandable: :<BOOL>)
    ((self backtrace-window-controller) view item)
    (declare (ignore view))
    (or (%null-ptr-p item)
        (our-frame-label-p self item)))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: :<NSI>nteger)
    ((self backtrace-window-controller) view item)
    (declare (ignore view))
    (let* ((inspector (backtrace-controller-inspector self)))
      (cond ((%null-ptr-p item)
             (inspector::inspector-line-count inspector))
            ((our-frame-label-p self item)
             (let* ((frame-inspector
                     (or (frame-label-frame-inspector item)
                         (setf (frame-label-frame-inspector item)
                               (make-instance
                                'inspector::stack-frame-inspector
                                :frame-number (frame-label-number item)
                                :object (inspector::inspector-object inspector)
				:update-line-count t)))))
               (inspector::inspector-line-count frame-inspector)))
            (t -1))))

(objc:defmethod #/outlineView:child:ofItem:
    ((self backtrace-window-controller) view (index :<NSI>nteger) item)
  (declare (ignore view))
  (let* ((inspector (backtrace-controller-inspector self)))
    (cond ((%null-ptr-p item)
           (let* ((label
                   (make-instance 'frame-label
                                  :string
                                  (let* ((value 
                                          (inspector::line-n inspector index)))
                                    (if value
                                      (%lfun-name-string value)
                                      ":kernel")))))
             (setf (slot-value label 'controller) self
                   (slot-value label 'frame-number) index)
             label))
          ((our-frame-label-p self item)
           (let* ((frame-inspector
                   (or (frame-label-frame-inspector item)
                       (setf (frame-label-frame-inspector item)
                             (make-instance
                              'inspector::stack-frame-inspector
                              :frame-number (frame-label-number item)
                              :object (inspector::inspector-object inspector)
                              :update-line-count t)))))
             (make-instance 'frame-item
                            :frame-label item
                            :index index
                            :string
                            (let* ((ccl::*aux-vsp-ranges* (inspector::vsp-range inspector))
                                   (ccl::*aux-tsp-ranges* (inspector::tsp-range inspector)))
                              (with-output-to-string (s)
                                                     (multiple-value-bind (value label)
                                                         (inspector::line-n
                                                          frame-inspector
                                                          index)
                                                       (inspector::prin1-value
                                                        frame-inspector
                                                        s
                                                        value
                                                        label)))))))
          (t (break) (%make-nsstring "Huh?")))))

(objc:defmethod #/outlineView:objectValueForTableColumn:byItem:
    ((self backtrace-window-controller) view column item)
  (declare (ignore view column))
  (if (%null-ptr-p item)
    #@"Open this"
    (%setf-macptr (%null-ptr) item)))

(defmethod initialize-instance :after ((self backtrace-window-controller)
                                       &key &allow-other-keys)
  (setf (slot-value self 'inspector)
        (make-instance 'inspector::stack-inspector :context (backtrace-controller-context self) :update-line-count t)))

(defun backtrace-controller-for-context (context)
  (or (bt.dialog context)
      (setf (bt.dialog context)
            (make-instance 'backtrace-window-controller
                           :with-window-nib-name #@"backtrace"
                           :context context))))

#+debug
(objc:defmethod (#/willLoad :void) ((self backtrace-window-controller))
  (#_NSLog #@"will load %@" :address  (#/windowNibName self)))

(defmethod ui-object-enter-backtrace-context ((app ns:ns-application)
                                              context)
  (let* ((proc *current-process*))
    (when (typep proc 'cocoa-listener-process)
      (push context (cocoa-listener-process-backtrace-contexts proc)))))

(defmethod ui-object-exit-backtrace-context ((app ns:ns-application)
                                              context)
  (let* ((proc *current-process*))
    (when (typep proc 'cocoa-listener-process)
      (when (eq context (car (cocoa-listener-process-backtrace-contexts proc)))
        (setf (cocoa-listener-process-backtrace-contexts proc)
              (cdr (cocoa-listener-process-backtrace-contexts proc)))
        (let* ((window (bt.dialog context)))
          (when window
            (#/performSelectorOnMainThread:withObject:waitUntilDone: window (@selector @/close)  +null-ptr+ t)))))))

  





