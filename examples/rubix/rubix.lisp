(in-package :cl-user)

(defparameter theta 0.0)
(defparameter thetainc 1.0)
(defparameter thetainctemp 0.0)
(defparameter face -1)
(defparameter nextface -1)
(defparameter clockwise t)
(defparameter nextclockwise t)

(defparameter cube nil)

(defparameter camera-pos #(10.0 5.0 12.0))

(defparameter light0 nil)
(defparameter light0-pos (make-array 3 :initial-contents '(5.0 3.0 0.0) ;; default to distant light source
                                     :element-type 'single-float))
(defparameter diffuse0 (make-array 4 :initial-contents '(0.0 0.0 0.0 1.0)
                                   :element-type 'single-float))
(defparameter ambient0 (make-array 4 :initial-contents '(1.0 1.0 1.0 1.0)
                                   :element-type 'single-float))
(defparameter specular0 (make-array 4 :initial-contents '(0.0 0.0 0.0 1.0)
                                   :element-type 'single-float))

(defparameter global-ambient (make-array 4 :initial-contents '(1.0 1.0 1.0 1.0) :element-type 'single-float)) ;; really really dim grey light

(defclass rubix-opengl-view (ns:ns-opengl-view)
  ()
  (:metaclass ns:+ns-object))

(ccl::define-objc-method ((:void prepare-opengl) rubix-opengl-view)
  (declare (special *the-origin* *y-axis*))
  (opengl:with-matrix-mode #$GL_PROJECTION ;; default is GL_MODELVIEW
    (#_glLoadIdentity)
    (#_glFrustum -1.0d0 1.0d0 -1.0d0 1.0d0 10.0d0 20.0d0))
  (#_glLoadIdentity)
  (mylookat camera-pos *the-origin* *y-axis*)

  (#_glShadeModel #$GL_SMOOTH)
  (#_glClearColor 0.05 0.05 0.05 0.0)
  (#_glClearDepth 1.0d0)
  (#_glEnable #$GL_DEPTH_TEST)
  (#_glDepthFunc #$GL_LEQUAL)
  (#_glHint #$GL_PERSPECTIVE_CORRECTION_HINT #$GL_NICEST)

  (setf cube (make-instance 'rubix-cube))

  (#_glEnable #$GL_LIGHTING)

  (setf light0 (make-instance 'light :lightid #$GL_LIGHT0))
  (setpointsource light0 t)
  (setlocation light0 light0-pos)
  (setdiffuse light0 diffuse0)
  (setambient light0 ambient0)
  (setspecular light0 specular0)
  (on light0)

  (ccl::%stack-block ((foreign-float-vector (* 4 4))) ; make room for 4 single-floats
    (ccl::%copy-ivector-to-ptr global-ambient ; source
      0     ; offset to first element (alignment padding)
      foreign-float-vector ; destination
      0                    ; byte offset in destination
      (* 4 4))             ; number of bytes to copy
    (#_glLightModelfv #$GL_LIGHT_MODEL_AMBIENT foreign-float-vector)) ;; <- coersion issue

  (#_glFlush))

(ccl::define-objc-method ((:void :draw-rect a-rect) rubix-opengl-view)
  (declare (ignorable a-rect))
  ;; drawing callback
  (#_glClear (logior #$GL_COLOR_BUFFER_BIT #$GL_DEPTH_BUFFER_BIT))
  (render cube)
  (#_glFlush))

;; want to be able to send keystrokes to the rubix cube
#+ignore
(ccl::define-objc-method ((:<BOOL> accepts-first-responder) rubix-opengl-view)
  #$YES)

;; for starters, just get the lights to turn on and off when a key is
;; pressed or the mouse is clicked
(ccl::define-objc-method ((:void :mouse-down the-event) rubix-opengl-view)
  ;; this makes dragging spin the cube
  (let ((dragging-p t))
    (rlet ((last-loc :<NSP>oint))
      (ccl::send/stret last-loc the-event 'location-in-window)
      (loop while dragging-p do
            #+drag-debug
	    (#_NSLog #@"Dragging")
	    (let ((the-event (ccl::send (ccl::send self 'window)
					:next-event-matching-mask
					(logior #$NSLeftMouseUpMask
						#$NSLeftMouseDraggedMask))))
	      (rlet ((mouse-loc :<NSP>oint))
		(ccl::send/stret mouse-loc the-event 'location-in-window)
		(cond ((eq #$NSLeftMouseDragged (ccl::send the-event 'type))
                       #+drag-debug
		       (#_NSLog #@"Really Dragging")
		       (let ((deltax (- (pref mouse-loc :<NSP>oint.x)
					(pref last-loc :<NSP>oint.x)))
			     (deltay (- (pref mouse-loc :<NSP>oint.y)
					(pref last-loc :<NSP>oint.y)))
			     (vert-rot-axis (cross *y-axis* camera-pos)))
			 ;; possible improvement -- cross camera-pos with
			 ;; cartesian distance to get axis of rotation,
			 ;; and apply that -- spin will be in direction of
			 ;; mouse
			 (setf (pref last-loc :<NSP>oint.x) (pref mouse-loc :<NSP>oint.x)
			       (pref last-loc :<NSP>oint.y) (pref mouse-loc :<NSP>oint.y))
			 (rotate-relative cube
					  (mulquats (axis-angle->quat vert-rot-axis deltay)
						    (axis-angle->quat *y-axis* deltax))))
		       (ccl::send self :set-needs-display #$YES))
		      (t
                       #+drag-debug
		       (#_NSLog #@"Done Dragging")
		       (setf dragging-p nil))))))
      (ccl::send self :set-needs-display #$YES))))

(defclass rubix-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

(defparameter *aluminum-margin* 5.0f0)

(defun run-rubix-demo ()
  (let* ((w (ccl::new-cocoa-window :class (find-class 'rubix-window)
				   :title "Rubix Cube"
				   :height 500
				   :width 500
				   :expandable nil))
	 (w-content-view (ccl::send w 'content-view)))
    ;; Q: why slet here?
    (ccl::slet ((w-frame (ccl::send w-content-view 'frame)))
      (ccl::slet ((glview-rect (ccl::ns-make-rect *aluminum-margin*
					*aluminum-margin*
					(- (pref w-frame :<NSR>ect.size.width)
					   (* 2 *aluminum-margin*))
					(- (pref w-frame :<NSR>ect.size.height)
					   *aluminum-margin*))))
	;; Q: why make-objc-instance here?
	(let ((glview (ccl::send (ccl::send (ccl::@class rubix-opengl-view) 'alloc)
			    :init-with-frame glview-rect
			    :pixel-format #+ignore
			                  (ccl::send (ccl::@class ns-opengl-view)
						     'default-pixel-format)
					  (opengl:new-pixel-format ;#$NSOpenGLPFADoubleBuffer
								   #$NSOpenGLPFAAccelerated
								   #$NSOpenGLPFAColorSize 32
								   #$NSOpenGLPFADepthSize 32))))
	  (ccl::send w-content-view :add-subview glview)
	  w)))))
