; -*- Mode: Lisp; Package: CCL; -*-

(in-package "CCL")

(defclass ns-lisp-string (ns:ns-string)
    ((lisp-string :initarg :string :reader ns-lisp-string-string))
  (:metaclass ns:+ns-object))

(define-objc-method ((:unsigned length) ns-lisp-string)
    (length (ns-lisp-string-string self)))

(define-objc-method ((:unichar :character-at-index (unsigned index))
		     ns-lisp-string)
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
     (inspector :initform nil :reader backtrace-window-controller-inspector))
  (:metaclass ns:+ns-object))

(defmethod our-frame-label-p ((self backtrace-window-controller) thing)
  (and (typep thing 'frame-label)
       (eql self (frame-label-controller thing))))

(define-objc-method ((:<BOOL> :outline-view view
                              :is-item-expandable item)
                     backtrace-window-controller)
    (declare (ignore view))
  (#_NSLog #@"is expandable")
    (or (%null-ptr-p item)
        (our-frame-label-p self item)))

(define-objc-method ((:int :outline-view view
                           :number-of-children-of-item item)
                     backtrace-window-controller)
    (declare (ignore view))
  (#_NSLog #@"Number of children")
    (let* ((inspector (backtrace-window-controller-inspector self)))
      (cond ((%null-ptr-p item)
             (inspector::inspector-line-count inspector))
            ((our-frame-label-p self item)
             (let* ((frame-inspector
                     (or (frame-label-frame-inspector item)
                         (setf (frame-label-frame-inspector item)
                               (make-instance
                                'inspector::stack-frame-inspector
                                :frame-number (frame-label-number item)
                                :object (inspector::inspector-object inspector))))))
               (inspector::inspector-line-count frame-inspector)))
            (t -1))))
             
(define-objc-method ((:id :outline-view view
                          :child (:int index)
                          :of-item item)
                     backtrace-window-controller)
    (declare (ignore view))
  (#_NSLog #@"child of item")
    (let* ((inspector (backtrace-window-controller-inspector self)))
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
                                :object (inspector::inspector-object inspector))))))
               (make-objc-instance 'frame-item
                                   :frame-label item
                                   :index index
                                   :string
                                   (with-output-to-string (s)
                                     (multiple-value-bind (value label)
                                         (inspector::line-n
                                          frame-inspector
                                          index)
                                     (inspector::prin1-value
                                      frame-inspector
                                      s
                                      value
                                      label))))))
            (t (break) (%make-nsstring "Huh?")))))

(define-objc-method ((:id :outline-view view
                          :object-value-for-table-column column
                          :by-item item)
                     backtrace-window-controller)
    (declare (ignore view column))
  (#_NSLog #@"value for item")
    (if (%null-ptr-p item)
      #@"Open this"
      (%setf-macptr (%null-ptr) item)))

(defmethod initialize-instance :after ((self backtrace-window-controller)
                                       &key &allow-other-keys)
  (setf (slot-value self 'inspector)
        (make-instance 'inspector::stack-inspector :info (backtrace-controller-context self) :update-line-count t))
  (let* ((w (send self 'window))
         (content-view (send w 'content-view)))
    (slet ((frame (send content-view 'frame)))
      (let* ((outline-view (make-objc-instance 'ns:ns-outline-view
                                               :with-frame frame))
             (column (make-objc-instance 'ns:ns-table-column
                                         :with-identifier #@"")))
        
        (send outline-view :set-autoresizing-mask (logior
                                                   #$NSViewWidthSizable
                                                   #$NSViewHeightSizable))
        (send column :set-max-width 10000.0)
        (send column :set-min-width 10.0)
        (send column :set-width (pref frame :<NSR>ect.size.width))
        (send column :set-resizable t)
        (send outline-view :add-table-column column)
        (send outline-view :set-data-source self)
        (send content-view :add-subview outline-view)
        (activate-window w)))))

        




(defun backtrace-window-for-context (context)
  (make-instance 'backtrace-window-controller
                 :with-window (new-cocoa-window :title "Backtrace"
                                                :activate nil)
                 :context context))

#+notyet
(progn


;;;;;;;
;;
;; Interface to the break-loop
;;
(defun select-backtrace (&aux (info (car ccl::*backtrace-dialogs*)))
    (unless info (error "No context for backtrace"))
    (if (ccl::bt.dialog info)
      (window-select (ccl::bt.dialog info))
      (make-instance 'backtrace-window :info info
                     :window-title (format nil "Backtrace for ~A"
                                           (process-name
                                            (ccl::stack-group-process
                                             (ccl::bt.sg info)))))))


;; Interface to apply-in-nth-frame
(defmethod ccl::nth-frame ((w backtrace-window) target n)
  (let ((error-frame (inspector-object (view-named 'stack-pane w))))
    (unless (eql target (stack-start error-frame))
      (error "Inconsistent args to nth-frame"))
    (error-frame-n error-frame n)))

;;;;;;;
;;
;; Interface to LOCAL
;;
(defun ccl::names-in-frame (&optional (window (front-window :class 'backtrace-window)))
  (when window
    (let* ((view (inspector-view (view-named 'stack-frame-pane window)))
           (inspector (inspector view))
           (lines (inspector-line-count inspector))
           res)
      (dotimes (i lines)
        (multiple-value-bind (val label) (cached-line-n view i)
          (declare (ignore val))
          (push (cddr label) res)))
      (nreverse res))))

(defun ccl::nth-frame-info (n &optional (window (front-window :class 'backtrace-window)))
  (when window
    (let* ((view (inspector-view (view-named 'stack-frame-pane window))))
      (values (cached-line-n view n)))))

(defun ccl::set-nth-frame-value (n new-value)
  (let ((window (front-window :class 'backtrace-window)))
    (let* ((view (inspector-view (view-named 'stack-frame-pane window)))
           (inspector (inspector view)))
      (setf (line-n inspector n) new-value)
      (resample view)))
  new-value)

(defun ccl::frame-lfun (&optional (window (front-window :class 'backtrace-window)))
  (when window
    (let* ((inspector (inspector (view-named 'stack-frame-pane window)))
           (info (frame-info inspector)))
      (when info
        (values (cadr info) (caddr info))))))

; Old inspector function that some folks were used to
(defun ccl::top-inspect-form ()
  (let ((w (front-window :class 'inspector-window)))
    (and w (inspector-object w))))

;;;;;;;
;;
;; return-from and restart frame
;;

                           
(defun backtrace-return-from-frame (w)
  (setq w (require-type w 'backtrace-window))
  (let* ((i (inspector (view-named 'stack-frame-pane w)))
         (info (frame-info i))
         (sg (stack-group (inspector-object i)))
         (frame (car info))
         (srv (ccl::frame-restartable-p frame sg)))
    (if (not srv)
      (ed-beep)                   ; Paranoia is a wonderful thing
      (multiple-value-bind (value ok-button-p) (edit-value nil nil)
        (when ok-button-p
          (ccl::apply-in-frame-internal 
           sg
           frame
           #'values
           (if (and (consp value) (eq (car value) 'values)) (cdr value) (list value))
           srv))))))

(defun add-child-window (w child)
  (view-put w :child-windows (push child (view-get w :child-windows))))

(defun backtrace-restart-frame (w)
  (setq w (require-type w 'backtrace-window))
  (let* ((inspector (inspector (view-named 'stack-frame-pane w)))
         (info (frame-info inspector))
         (error-frame (inspector-object inspector))
         (sg (stack-group error-frame)))
    (destructuring-bind (frame lfun pc child &rest rest) info
      (declare (ignore rest))
      (multiple-value-bind (args types names count nclosed)
                           (ccl::frame-supplied-args frame lfun pc child sg)
        (let* ((frame (car info))
               (srv (ccl::frame-restartable-p frame sg)))
          (if (not (and (or (eq count t) (>= count nclosed)) frame srv))
            (ed-beep)
            (let* ((name (function-name lfun))
                   (f (ignore-errors (fboundp name))))
              (cond ((null f))
                    ((eq (ccl::closure-function f) lfun)
                     (setq lfun name
                           args (nthcdr nclosed args)
                           types (nthcdr nclosed types)
                           names (nthcdr nclosed names)
                           nclosed 0))
                    (f (setq lfun name)))
              (let ((i (make-instance 'function-args-inspector
                         :stack-frame-inspector inspector
                         :restart-srv srv
                         :frame-to-restart frame
                         :object (cons lfun args)
                         :types types :names names :nclosed nclosed)))
                (add-child-window w
                                  (make-instance 'inspector-window
                                    :inspector i :view-position '(:top 50)))))))))))

(defclass function-args-inspector (inspector)
  ((types :initarg :types :accessor types)
   (names :initarg :names :accessor names)
   (nclosed :initarg :nclosed :accessor nclosed)
   (stack-frame-inspector :initarg :stack-frame-inspector :reader stack-frame-inspector)
   (frame-to-restart :initarg :frame-to-restart :reader frame-to-restart)
   (restart-srv :initarg :restart-srv :reader restart-srv)))

(defmethod inspector-window-title ((i function-args-inspector))
  (format nil "Restart frame at #x~x" (ccl::index->address (frame-to-restart i))))

(defmethod compute-line-count ((i function-args-inspector))
  (+ 3 (length (inspector-object i))))

(defmethod line-n ((i function-args-inspector) n)
  (let ((f&args (inspector-object i)))
    (case n
      (0 f&args)
      (1 (values nil "Choose \"Restart\" from \"Commands\" menu when ready"
                 :comment))
      (2 (values (car f&args) "Function" :colon))
      (3 (values (ignore-errors (arglist (car f&args))) "Arglist: " :static))
      (t (decf n 4)
         (let ((args (nthcdr n (cdr f&args)))
               (type (nth n (types i)))
               (name (nth n (names i))))
           (unless args (line-n-out-of-range i (+ n 4)))
           (values (car args) (list n type name)))))))

(defmethod (setf line-n) (value (i function-args-inspector) n)
  (flet ((install-new-function (i function)
           (let ((arglist (arglist function))
                 (types nil)
                 (names nil)
                 (type "required"))
             (dolist (name arglist)
               (cond ((eq name '&optional) (setq type "optional"))
                     ((memq name lambda-list-keywords) (return))
                     (t (push type types)
                        (push name names))))
             (setf (types i) types
                   (names i) names)
             (unless (eql 0 (nclosed i))
               (let ((f&args (inspector-object i)))
                 (setf (cdr f&args) (nthcdr (nclosed i) (cdr f&args))))
               (setf (nclosed i) 0)))))
    (case n
      (0 (if (ignore-errors
              (and (listp value) (length value) (or (functionp (car value)) 
                                                    (fboundp (car value)))))
           (progn
             (setf (inspector-object i) value)
             (install-new-function i (car value))
             (resample-it))
           (ed-beep)))
      ((1 3) (setf-line-n-out-of-range i n))
      (2 (if (ignore-errors (or (functionp value) (fboundp value)))
           (progn
             (setf (car (inspector-object i)) value)
             (install-new-function i value)
             (resample-it))
           (ed-beep)))
      (t (decf n 4)
         (let ((args (nthcdr n (cdr (inspector-object i)))))
           (unless args (setf-line-n-out-of-range i (+ n 4)))
           (setf (car args) value)
           (resample-it))))))

(defmethod prin1-label ((i function-args-inspector) stream value &optional label type)
  (declare (ignore value type))
  (if (consp label)
    (format stream "~d: " (car label))
    (call-next-method)))

(defmethod prin1-value ((i function-args-inspector) stream value &optional label type)
  (declare (ignore type))
  (if (consp label)
    (destructuring-bind (n type name) label
      (declare (ignore n))
      (when name
        (princ name stream)
        (tyo #\space stream))
      (when type
        (format stream "(~a) " type))))
  (prin1 value stream))


(defmethod inspector-commands ((i function-args-inspector))
  (let ((res 
         `(("Restart "
            ,#'(lambda ()
                 (window-close (view-window (inspector-view i)))
                 (let* ((stack-frame-inspector (stack-frame-inspector i)))
                   (if (wptr (inspector-view stack-frame-inspector))
                     (let* ((frame (frame-to-restart i))
                            (srv (restart-srv i))
                            (f&args (inspector-object i))
                            (sg (stack-group (inspector-object stack-frame-inspector))))
                       (ccl::apply-in-frame-internal
                        sg
                        frame
                        (car f&args)  ; fn
                        (cdr f&args)  ; args
                        srv)))))))))      ; saved registers
    (let* ((view (inspector-view i))
           (selection (selection view)))
      (let ((f&args (inspector-object i)))
        (push `("Insert arg after selection"
                ,(and selection (>= (decf selection 3) 0)
                      #'(lambda ()
                          (push nil (cdr (nthcdr selection f&args)))
                          (resample-it))))
              res)
        (push `("Delete (and Copy) selected arg"
                ,(and selection (> selection 0)
                      #'(lambda ()
                          (copy view)
                          (pop (nthcdr selection f&args))
                          (if (>= selection (length f&args))
                            (set-selection (inspector-view i) nil))
                          (resample-it))))
              res)))
    (nreverse res)))
        
)
