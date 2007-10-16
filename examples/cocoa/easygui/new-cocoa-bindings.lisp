(in-package :easygui)

;;; Helper types:

(defclass eg-point ()
     ((x :initarg :x :reader point-x)
      (y :initarg :y :reader point-y)))

(defun point (x y)
  (assert (>= x 0))
  (assert (>= y 0))
  (make-instance 'eg-point :x x :y y))

(defclass eg-range ()
     ((start :initarg :start :reader range-start)
      (end :initarg :end :reader range-end)))

(defun range (start end)
  (assert (>= end start))
  (make-instance 'eg-range :start start :end end))

(defun range-nsrange (range)
  (ns:make-ns-range (range-start range) (range-end range)))

(defclass eg-rectangle ()
     ((x :initarg :x :reader rectangle-x)
      (y :initarg :y :reader rectangle-y)
      (width :initarg :width :reader rectangle-width)
      (height :initarg :height :reader rectangle-height)))

(defun rectangle (x y width height)
  (assert (>= x 0))
  (assert (>= y 0))
  (assert (>= width 0))
  (assert (>= height 0))
  (make-instance 'eg-rectangle :x x :y y :width width :height height))

(defun rectangle-nsrect (r)
  (ns:make-ns-rect (rectangle-x r) (rectangle-y r)
                   (rectangle-width r) (rectangle-height r)))

(defun nsrect-rectangle (r)
  (rectangle (ns:ns-rect-x r) (ns:ns-rect-y r)
             (ns:ns-rect-width r) (ns:ns-rect-height r)))

;;;
(defclass easy-cocoa-object ()
     ((ref :initarg :cocoa-ref :accessor cocoa-ref)))

(defvar *window-position-default-x* 200)
(defvar *window-position-default-y* 200)
(defvar *window-size-default-x* 200)
(defvar *window-size-default-y* 200)

(defun ns-rect-from-points (posn size)
  (ns:make-ns-rect (point-x posn) (point-y posn)
                   (point-x size) (point-y size)))

(defparameter *flag-to-mask-alist*
              `( ;; (:zoomable-p . #$NSZoomableWindowMask) ; doesn't work
                (:minimizable-p . ,#$NSMiniaturizableWindowMask)
                (:resizable-p . ,#$NSResizableWindowMask)
                (:closable-p . ,#$NSClosableWindowMask)))

(defun flag-mask (keyword enabled-p)
  (if enabled-p
      (or (cdr (assoc keyword *flag-to-mask-alist*)) 0)
      0))

(defparameter *key-to-mask-alist*
              `((:control . ,#$NSControlKeyMask)
                (:alt     . ,#$NSAlternateKeyMask)
                (:command . ,#$NSCommandKeyMask)))

(defun key-mask (keyword)
  (or (cdr (assoc keyword *key-to-mask-alist*)) 0))

;;; debug macro for #/ funcalls:

(defvar *debug-cocoa-calls* t)

(defmacro dcc (form)
  `(progn
     (when *debug-cocoa-calls*
       (format *trace-output* "Calling ~A on ~S~%"
               ',(first form) (list ,@(rest form))))
     ,form))

;;; Running things on the main thread:

(defclass cocoa-thunk (ns:ns-object)
     ((thunk :accessor thunk-of))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/run :void) ((self cocoa-thunk))
  (funcall (thunk-of self)))

(defun run-on-main-thread (waitp thunk)
  (let ((thunk* (make-instance 'cocoa-thunk)))
    (setf (thunk-of thunk*) thunk)
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     thunk*
     (@selector #/run)
     +null-ptr+
     (not (not waitp)))))

(defmacro running-on-main-thread ((&key (waitp t)) &body body)
  `(run-on-main-thread ,waitp (lambda () ,@body)))