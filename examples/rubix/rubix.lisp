(in-package :cl-user)


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
  (declare (ignore a-rect))
  (opengl:with-matrix-mode (#$GL_PROJECTION) ;; default is GL_MODELVIEW
    (#_glLoadIdentity)
    (#_glFrustum -0.6d0 0.6d0 -0.6d0 0.6d0 10.0d0 20.0d0))
  (#_glLoadIdentity)
  (mylookat *camera-pos* *the-origin* *y-axis*)

  (#_glShadeModel #$GL_SMOOTH)
  (#_glClearColor 0.05 0.05 0.05 0.0)
  ;; these next three are all needed to enable the z-buffer
  (#_glClearDepth 1.0d0)
  (#_glEnable #$GL_DEPTH_TEST)
  (#_glDepthFunc #$GL_LEQUAL)
  (#_glHint #$GL_PERSPECTIVE_CORRECTION_HINT #$GL_NICEST)

  (setf *cube* (make-instance 'rubix-cube))

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

(ccl::define-objc-method ((:void :draw-rect (:<NSR>ect a-rect)) rubix-opengl-view)
  (declare (ignorable a-rect))
  ;; drawing callback
  (#_glClear (logior #$GL_COLOR_BUFFER_BIT #$GL_DEPTH_BUFFER_BIT))
  (render *cube*)
  (#_glFlush))

;; want to be able to send keystrokes to the rubix cube
#+ignore
(ccl::define-objc-method ((:<BOOL> accepts-first-responder) rubix-opengl-view)
  #$YES)

;; want to be able to click and start dragging (without moving the window)
(ccl::define-objc-method ((:<BOOL> accepts-first-mouse) rubix-opengl-view)
  #$YES)

(defparameter *rubix-face-snap* 8.0) ; degrees

(ccl::define-objc-method ((:void :mouse-down the-event) rubix-opengl-view)
  ;; this makes dragging spin the cube
  (cond ((zerop (logand #$NSControlKeyMask (ccl::send the-event 'modifier-flags))) ; not ctrl-click
	 (let ((dragging-p t))
	   (rlet ((last-loc :<NSP>oint))
		 (ccl::send/stret last-loc the-event 'location-in-window)
		 (loop while dragging-p do
		       (let ((the-event (ccl::send (ccl::send self 'window)
						   :next-event-matching-mask
						   (logior #$NSLeftMouseUpMask
							   #$NSLeftMouseDraggedMask))))
			 (rlet ((mouse-loc :<NSP>oint))
			       (ccl::send/stret mouse-loc the-event 'location-in-window)
			       (cond ((eq #$NSLeftMouseDragged (ccl::send (the ns:ns-event the-event) 'type))
				      (let ((deltax (- (pref mouse-loc :<NSP>oint.x)
						       (pref last-loc :<NSP>oint.x)))
					    (deltay (- (pref last-loc :<NSP>oint.y)
						       (pref mouse-loc :<NSP>oint.y)))
					    (vert-rot-axis (cross *y-axis* *camera-pos*)))
					(setf (pref last-loc :<NSP>oint.x) (pref mouse-loc :<NSP>oint.x)
					      (pref last-loc :<NSP>oint.y) (pref mouse-loc :<NSP>oint.y))
					(rotate-relative *cube*
							 (mulquats (axis-angle->quat vert-rot-axis deltay)
								   (axis-angle->quat *y-axis* deltax))))
				      (ccl::send self :set-needs-display #$YES))
				     (t
				      (setf dragging-p nil))))))
		 (ccl::send self :set-needs-display #$YES))))
	(t ;; ctrl-click, do what right-click does... note that once
	   ;; ctrl-click is done dragging will not require ctrl be held down

	 ;; NOTE THE GRATUITOUS CUT-AND-PASTE, debug the right-mouse-down
	 ;; version preferentially and update this one with fixes as needed
	 (rlet ((first-loc :<NSP>oint)
		(pick-loc :<NSP>oint))
	       (ccl::send/stret first-loc the-event 'location-in-window)
	       (ccl::send/stret pick-loc self :convert-point first-loc :from-view nil)
	       (let ((dragging-p t)
		     (reference-snap 0))
		 (setf (turning-face *cube*) (render-for-selection
                                              *cube*
                                              (opengl:unproject (pref pick-loc :<NSP>oint.x)
                                                                (pref pick-loc :<NSP>oint.y)))
		       (face-turning-p *cube*) (when (numberp (turning-face *cube*)) t)
		       (face-theta *cube*) 0.0)
		 (loop while (and dragging-p (face-turning-p *cube*)) do
		       (let ((the-event (ccl::send (ccl::send self 'window)
						   :next-event-matching-mask
						   (logior #$NSLeftMouseUpMask
							   #$NSLeftMouseDraggedMask))))
			 (rlet ((mouse-loc :<NSP>oint))
			       (ccl::send/stret mouse-loc the-event 'location-in-window)
			       (cond ((eq #$NSLeftMouseDragged (ccl::send (the ns:ns-event the-event) 'type))
				      (let ((deltax (- (pref mouse-loc :<NSP>oint.x)
						       (pref first-loc :<NSP>oint.x))))
					(multiple-value-bind (snap-to snap-dist) (round deltax 90.0)
							     (cond ((>= *rubix-face-snap* (abs snap-dist)) ; snap
								    ;; update cube structure
								    (let ((rotations (- snap-to reference-snap)))
								      (cond ((zerop rotations) nil)
									    ((< 0 rotations)
									     (dotimes (i rotations)
									       (turnfaceclockwise *cube* (turning-face *cube*)))
									     (setf reference-snap snap-to))
									    ((> 0 rotations)
									     (dotimes (i (abs rotations))
									       (turnfacecounterclockwise *cube* (turning-face *cube*)))
									     (setf reference-snap snap-to))))
								    ;; determine where face will be drawn
								    (setf (face-theta *cube*) 0.0))
								   (t ; no snap
								    (setf (face-theta *cube*) (- deltax (* 90.0 reference-snap))))
								   )))
				      (ccl::send self :set-needs-display #$YES))
				     (t
				      (setf (face-turning-p *cube*) nil
					    (turning-face *cube*) nil
					    (face-theta *cube*) nil
					    dragging-p nil))))))
		 (ccl::send self :set-needs-display #$YES)))
	 )))

(ccl::define-objc-method ((:void :right-mouse-down the-event) rubix-opengl-view)
  ;; this makes dragging left/right turn a face counterclockwise/clockwise
  ;; ... clicked-on face determines face turned
  ;; ... with an n-degree "snap"
  ;; ... with the snap updating the data structure
  ;; ... releasing the mouse clears rotation angle (face will snap to last position)
  (rlet ((first-loc :<NSP>oint)
	 (pick-loc :<NSP>oint))
    (ccl::send/stret first-loc the-event 'location-in-window)
    (ccl::send/stret pick-loc self :convert-point first-loc :from-view nil)
    (let ((dragging-p t)
	  (reference-snap 0))
      (setf (turning-face *cube*) (render-for-selection
				 *cube*
				 (opengl:unproject (pref pick-loc :<NSP>oint.x)
						   (pref pick-loc :<NSP>oint.y)))
	    (face-turning-p *cube*) (when (numberp (turning-face *cube*)) t)
	    (face-theta *cube*) 0.0)
      (loop while (and dragging-p (face-turning-p *cube*)) do
	    (let ((the-event (ccl::send (ccl::send self 'window)
					:next-event-matching-mask
					(logior #$NSRightMouseUpMask
						#$NSRightMouseDraggedMask))))
	      (rlet ((mouse-loc :<NSP>oint))
		(ccl::send/stret mouse-loc the-event 'location-in-window)
		(cond ((eq #$NSRightMouseDragged (ccl::send (the ns:ns-event the-event) 'type))
		       (let ((deltax (- (pref mouse-loc :<NSP>oint.x)
					(pref first-loc :<NSP>oint.x))))
			 (multiple-value-bind (snap-to snap-dist) (round deltax 90.0)
			   (cond ((>= *rubix-face-snap* (abs snap-dist)) ; snap
				  ;; update cube structure
				  (let ((rotations (- snap-to reference-snap)))
				    (cond ((zerop rotations) nil)
					  ((< 0 rotations)
					   (dotimes (i rotations)
					     (turnfaceclockwise *cube* (turning-face *cube*)))
					   (setf reference-snap snap-to))
					  ((> 0 rotations)
					   (dotimes (i (abs rotations))
					     (turnfacecounterclockwise *cube* (turning-face *cube*)))
					   (setf reference-snap snap-to))))
				  ;; determine where face will be drawn
				  (setf (face-theta *cube*) 0.0))
				 (t ; no snap
				  (setf (face-theta *cube*) (- deltax (* 90.0 reference-snap))))
				 )))
		       (ccl::send self :set-needs-display #$YES))
		      (t
		       (setf (face-turning-p *cube*) nil
			     (turning-face *cube*) nil
			     (face-theta *cube*) nil
			     dragging-p nil))))))
      (ccl::send self :set-needs-display #$YES))))

(defclass rubix-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

(defparameter *aluminum-margin* 5.0f0)

(defun run-rubix-demo ()
  (let* ((w (ccl::new-cocoa-window :class (find-class 'rubix-window)
				   :title "Rubix Cube"
				   :height 250
				   :width 250
				   :expandable nil))
	 (w-content-view (ccl::send w 'content-view)))
    ;; Q: why slet here?
    (ccl::slet ((w-frame (ccl::send w-content-view 'frame)))
      (ccl::slet ((glview-rect (ccl::ns-make-rect
                                (float *aluminum-margin* ccl::+cgfloat-zero+)
                                (float *aluminum-margin* ccl::+cgfloat-zero+)
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
