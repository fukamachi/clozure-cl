(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :GL))

;;; Cocoa-based OpenGL package, for handy macros and classes of common
;;; things like transformations, lights, cameras, quaternions, etc.
;;; (note not all that is in this package yet)

#|

To use this functionality, subclass NSOpenGLView, specialize
drawRect on your class to render whatever you like, and make an
intance in a window.

|#

(defpackage "OPENGL"
  (:nicknames :opengl :gl)
  (:export "WITH-OPENGL-CONTEXT"
	   "WITH-MATRIX-MODE"
	   "NEW-PIXEL-FORMAT"))

(in-package :opengl)

;; WITH-OPENGL-CONTEXT is not needed in the PREPARE-OPENGL
;; and DRAW-RECT functions of a specialized NS-OPENGL-VIEW
(defparameter *opengl-context* nil)
(defmacro with-opengl-context (context &body body)
  (let ((contextsym (gensym)))
    `(let ((,contextsym ,context))
       (unwind-protect
	   (let ((*opengl-context* ,contextsym))
	     (send ,contextsym 'make-current-context)
	     ,@body)
	 ;; the following resets the current context to what it was
	 ;; previously as far as the special bindings are concerned
	 (if *opengl-context*
	     (send *opengl-context* 'make-current-context)
	   (send (@class ns-opengl-context) 'clear-current-context))))))

(defparameter *matrix-mode* #$GL_MODELVIEW)
(defmacro with-matrix-mode (mode &body body)
  `(unwind-protect
       (let ((*matrix-mode* ,mode))
	 (#_glMatrixMode *matrix-mode*)
	 ,@body)
     (#_glMatrixMode *matrix-mode*)))

(defun new-pixel-format (&rest attributes)
  ;; take a list of opengl pixel format attributes (enums and other
  ;; small ints), make an array (character array?), and create and
  ;; return an NSOpenGLPixelFormat
  (let* ((attribute-size (ccl::foreign-size :<NSO>pen<GLP>ixel<F>ormat<A>ttribute :bytes)))
    (ccl::%stack-block ((objc-attributes (* attribute-size (1+ (length attributes)))))
      (loop for i from 0 to (* (1- (length attributes)) attribute-size) by attribute-size
	    for attribute in attributes do
	    (setf (%get-long objc-attributes i) attribute) ; <- autocoerced?
	    finally (let ((lastpos (* (length attributes) attribute-size)))
		      (setf (%get-long objc-attributes lastpos) 0))) ; <- objc nil = null ptr
      (let* ((pixelformat (ccl::send (ccl::send (ccl::@class ns-opengl-pixel-format) 'alloc)
				     :init-with-attributes objc-attributes)))
	pixelformat))))

#|
(setf pf (opengl:new-pixel-format #$NSOpenGLPFADoubleBuffer #$NSOpenGLPFADepthSize 32))
(%stack-block ((a-long 4))
  (send pf :get-values a-long :for-attribute #$NSOpenGLPFADepthSize :for-virtual-screen 0)
  (%get-long a-long))
|#

