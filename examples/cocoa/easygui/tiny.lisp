;;; Another example:
;;; This one creates a full-window view and draws in it.
;;; This is the easygui equivalent of examples/cocoa/tiny.lisp.

(in-package :easygui-user)

(defclass tiny-demo-drawing-view (drawing-view) ())

(defconstant short-pi (coerce pi 'short-float))
(defconstant numsides 12)

(defmethod draw-view-rectangle ((view tiny-demo-drawing-view) rectangle)
  (declare (ignore rectangle))
  (format *trace-output* "hallo test!~%")
  (let* ((view (cocoa-ref view))
         (bounds (#/bounds view))
         (width (ns:ns-rect-width bounds))
         (height (ns:ns-rect-height bounds)))
    (macrolet ((X (tt) `(* (1+ (sin ,tt)) width 0.5))
               (Y (tt) `(* (1+ (cos ,tt)) height 0.5)))
      ;; Fill the view with white
      (#/set (#/whiteColor ns:ns-color))
      ;; Trace two polygons with N sides and connect all of the vertices 
      ;; with lines
      (#/set (#/blackColor ns:ns-color))
      (loop 
        for f from 0.0 below (* 2 short-pi) by (* 2 (/ short-pi numsides))
        do (loop 
             for g from 0.0 below (* 2 short-pi) by (* 2 (/ short-pi numsides))
             do (#/strokeLineFromPoint:toPoint:
                                      ns:ns-bezier-path
                                      (ns:make-ns-point (X f) (Y f))
                                      (ns:make-ns-point (X g) (Y g))))))))

(defclass tiny-demo-window (window) ()
  (:default-initargs :size (point 400 400)
     :position (point 100 350)
     :title "Tiny rectangle drawing demo"
     :resizable-p nil
     :minimizable-p t))

(defmethod initialize-view :after ((window tiny-demo-window))
  (let ((draw-view (make-instance 'tiny-demo-drawing-view)))
    (setf (content-view window) draw-view)
    (window-show window)))