;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2002-2003 Clozure Associates
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


(in-package "CCL")			; for now.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OBJC-SUPPORT")
  ;;
  ;;  this stuff should all be integrated with a preferences file in ~/Library/OpenMCL/
  ;; (Um, it -is- integrated with the preferences file.)
  ;;
  (require "COCOA-DEFAULTS")
  (def-cocoa-default *default-font-name* :string "Courier" "Name of font to use in editor windows")
  (def-cocoa-default *default-font-size* :float 12.0f0 "Size of font to use in editor windows, as a positive SINGLE-FLOAT")
  (def-cocoa-default *tab-width* :int 8 "Width of editor tab stops, in characters" (integer 1 32))
  (require "COCOA-PREFS")
  (require "COCOA-TYPEOUT"))

(eval-when (:compile-toplevel :execute)
  (use-interface-dir #+apple-objc  :cocoa #+gnu-objc :gnustep))


(defun init-cocoa-application ()
  (with-autorelease-pool
      (let* ((bundle (open-main-bundle))
	     (dict (#/infoDictionary  bundle))
	     (classname (#/objectForKey: dict #@"NSPrincipalClass"))
	     (mainnibname (#/objectForKey: dict  #@"NSMainNibFile"))
	     (progname (#/objectForKey: dict #@"CFBundleName")))
	(if (%null-ptr-p classname)
	  (error "problems loading bundle: can't determine class name"))
	(if (%null-ptr-p mainnibname)
	  (error "problems loading bundle: can't determine main nib name"))
	(unless (%null-ptr-p progname)
          (#/setProcessName: (#/processInfo ns:ns-process-info) progname))
	(let* ((appclass (#_NSClassFromString classname))
	       (app (#/sharedApplication appclass)))
          (#/loadNibNamed:owner: ns:ns-bundle mainnibname  app)
	  app))))



#+apple-objc
(defun trace-dps-events (flag)
  (external-call "__DPSSetEventsTraced"
		 :unsigned-byte (if flag #$YES #$NO)
		 :void))

(defvar *appkit-process-interrupt-ids* (make-id-map))
(defun register-appkit-process-interrupt (thunk)
  (assign-id-map-id *appkit-process-interrupt-ids* thunk))
(defun appkit-interrupt-function (id)
  (id-map-free-object *appkit-process-interrupt-ids* id))

(defclass appkit-process (process) ())

(defconstant process-interrupt-event-subtype 17)




(defclass lisp-application (ns:ns-application)
    ((termp :foreign-type :<BOOL>))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/postEventAtStart: :void) ((self  ns:ns-application) e)
  (#/postEvent:atStart: self e t))

;;; Interrupt the AppKit event process, by enqueing an event (if the
;;; application event loop seems to be running.)  It's possible that
;;; the event loop will stop after the calling thread checks; in that
;;; case, the application's probably already in the process of
;;; exiting, and isn't that different from the case where asynchronous
;;; interrupts are used.  An attribute of the event is used to identify
;;; the thunk which the event handler needs to funcall.
(defmethod process-interrupt ((process appkit-process) function &rest args)
  (if (eq process *current-process*)
    (apply function args)
    (if (or (not *NSApp*) (not (#/isRunning *NSApp*)))
      (call-next-method)
      (rletZ ((point :ns-point))
        (let* ((e (#/otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:
                   ns:ns-event
                   #$NSApplicationDefined
                   point
                   0
                   0.0d0
                   0
                   +null-ptr+
                   process-interrupt-event-subtype
                   (register-appkit-process-interrupt
                    #'(lambda () (apply function args))) 0)))
	(#/retain e)
        (#/performSelectorOnMainThread:withObject:waitUntilDone:
         *NSApp* (@selector "postEventAtStart:") e  t))))))


(defloadvar *default-ns-application-proxy-class-name*
    "LispApplicationDelegate")

#+apple-objc
(defun enable-foreground ()
  (%stack-block ((psn 8))
    (external-call "_GetCurrentProcess" :address psn)
    (external-call "_CPSEnableForegroundOperation" :address psn)
    (eql 0 (external-call "_SetFrontProcess" :address psn :signed-halfword))))

;;; I'm not sure if there's another way to recognize events whose
;;; type is #$NSApplicationDefined.
(objc:defmethod (#/sendEvent: :void) ((self lisp-application) e)
  (if (and (eql (#/type e) #$NSApplicationDefined)
	   (eql (#/subtype e)  process-interrupt-event-subtype))
    ;;; The thunk to funcall is identified by the value
    ;;; of the event's data1 attribute.
    (funcall (appkit-interrupt-function (#/data1 e)))
    (call-next-method e)))


(objc:defmethod (#/showPreferences: :void) ((self lisp-application) sender)
  (declare (ignore sender))
  (#/show (#/sharedPanel preferences-panel)))

(objc:defmethod (#/toggleTypeout: :void) ((self lisp-application) sender)
  (declare (ignore sender))
  (#/show (#/sharedPanel typeout-panel)))

(defun nslog-condition (c)
  (let* ((rep (format nil "~a" c)))
    (with-cstrs ((str rep))
      (with-nsstr (nsstr str (length rep))
	(#_NSLog #@"Error in event loop: %@" :address nsstr)))))


(defmethod process-exit-application ((process appkit-process) thunk)
  (when (eq process *initial-process*)
    (%set-toplevel thunk)
    (#/terminate: *NSApp* +null-ptr+)))

(defun run-event-loop ()
  (%set-toplevel nil)
  (change-class *cocoa-event-process* 'appkit-process)
  (let* ((app *NSApp*))
    (loop
	(handler-case (#/run app)
	  (error (c) (nslog-condition c)))
	(unless (#/isRunning app)
	  (return)))))



(defun start-cocoa-application (&key
				(application-proxy-class-name
				 *default-ns-application-proxy-class-name*))
  
  (flet ((cocoa-startup ()
	   ;; Start up a thread to run periodic tasks.
	   (process-run-function "housekeeping"
				 #'(lambda ()
				     (loop
                                       (%nanosleep *periodic-task-seconds*
                                                   *periodic-task-nanoseconds*)
                                       (housekeeping))))
	   
           (with-autorelease-pool
             (enable-foreground)
             (or *NSApp* (setq *NSApp* (init-cocoa-application)))
             (let* ((icon (#/imageNamed: ns:ns-image #@"NSApplicationIcon")))
               (unless (%null-ptr-p icon)
                 (#/setApplicationIconImage: *NSApp* icon)))
             (setf (application-ui-object *application*) *NSApp*)
             (when application-proxy-class-name
               (let* ((classptr (%objc-class-classptr
                                 (load-objc-class-descriptor application-proxy-class-name)))
                      (instance (#/init (#/alloc classptr))))

                 (#/setDelegate: *NSApp* instance))))
           (run-event-loop)))
    (process-interrupt *cocoa-event-process* #'(lambda ()
						 (%set-toplevel 
						  #'cocoa-startup)
						 (toplevel)))))

(defparameter *font-attribute-names*
  '((:bold . #.#$NSBoldFontMask)
    (:italic . #.#$NSItalicFontMask)
    (:small-caps . #.#$NSSmallCapsFontMask)))


;;; The NSFont method #/isFixedPitch has returned random answers
;;; in many cases for the last few OSX releases.  Try to return
;;; a reasonable answer, by checking to see if the width of the
;;; advancement for the #\i glyph matches that of the advancement
;;; of the #\m glyph.

(defun is-fixed-pitch-font (font)
  (= (ns:ns-size-width (#/advancementForGlyph: font (#/glyphWithName: font #@"i")))
     (ns:ns-size-width (#/advancementForGlyph: font (#/glyphWithName: font #@"m")))))

;;; Try to find the specified font.  If it doesn't exist (or isn't
;;; fixed-pitch), try to find a fixed-pitch font of the indicated size.
(defun default-font (&key (name *default-font-name*)
			  (size *default-font-size*)
			  (attributes ()))
				
  (setq size (float size +cgfloat-zero+))
  (with-cstrs ((name name))
    (with-autorelease-pool
	(rletz ((matrix (:array :<CGF>loat 6)))
	  (setf (paref matrix (:* :<CGF>loat) 0) size
                (paref matrix (:* :<CGF>loat) 3) size)
          (let* ((fontname (#/stringWithCString: ns:ns-string name))
		 (font (#/fontWithName:matrix: ns:ns-font fontname matrix))
                   
		 (implemented-attributes ()))
	    (if (or (%null-ptr-p font)
		    (and 
		     (not (is-fixed-pitch-font font))))
	      (setq font (#/userFixedPitchFontOfSize: ns:ns-font size)))
	    (when attributes
	      (dolist (attr-name attributes)
		(let* ((pair (assoc attr-name *font-attribute-names*))
		       (newfont))
		  (when pair
		    (setq newfont
                          (#/convertFont:toHaveTrait:
                           (#/sharedFontManager ns:ns-font-manager) font (cdr pair)))
		    (unless (eql font newfont)
		      (setq font newfont)
		      (push attr-name implemented-attributes))))))
	    (values (#/retain font) implemented-attributes))))))

;;; Create a paragraph style, mostly so that we can set tabs reasonably.
(defun create-paragraph-style (font line-break-mode)
  (let* ((p (make-instance 'ns:ns-mutable-paragraph-style))
	 (charwidth (fround (ns:ns-size-width (#/maximumAdvancement font)))))
    (#/setLineBreakMode: p
                         (ecase line-break-mode
                           (:char #$NSLineBreakByCharWrapping)
                           (:word #$NSLineBreakByWordWrapping)
                           ;; This doesn't seem to work too well.
                           ((nil) #$NSLineBreakByClipping)))
    ;; Clear existing tab stops.
    (#/setTabStops: p (#/array ns:ns-array))
    (do* ((i 1 (1+ i)))
	 ((= i 100) p)
      (let* ((tabstop (make-instance
		       'ns:ns-text-tab
		       :with-type #$NSLeftTabStopType
		       :location  (* (* i *tab-width*)
					charwidth))))
        (#/addTabStop: p tabstop)
        (#/release tabstop)))))
    
(defun create-text-attributes (&key (font (default-font))
				    (line-break-mode :char)
				    (color nil)
                                    (obliqueness nil)
                                    (stroke-width nil))
  (let* ((dict (#/retain (make-instance 'ns:ns-mutable-dictionary :with-capacity 5))))
    (#/setObject:forKey: dict (create-paragraph-style font line-break-mode) #&NSParagraphStyleAttributeName)
    (#/setObject:forKey: dict font #&NSFontAttributeName)
    (when color
      (#/setObject:forKey: dict color #&NSForegroundColorAttributeName))
    (when stroke-width
      (#/setObject:forKey: dict (make-instance 'ns:ns-number
                                               :with-float (float stroke-width)) #&NSStrokeWidthAttributeName))
    (when obliqueness
      (#/setObject:forKey:  dict (make-instance 'ns:ns-number
                                                :with-float (float obliqueness)) #&NSObliquenessAttributeName))
    dict))


(defun get-cocoa-window-flag (w flagname)
  (case flagname
    (:accepts-mouse-moved-events
     (#/acceptsMouseMovedEvents w))
    (:cursor-rects-enabled
     (#/areCursorRectsEnabled w))
    (:auto-display
     (#/isAutodisplay w))))



(defun (setf get-cocoa-window-flag) (value w flagname)
  (case flagname
    (:accepts-mouse-moved-events
     (#/setAcceptsMouseMovedEvents: w value))
    (:auto-display
     (#/setAutodisplay: w value))))



(defun activate-window (w)
  ;; Make w the "key" and frontmost window.  Make it visible, if need be.
  (#/makeKeyAndOrderFront: w nil))

(defun new-cocoa-window (&key
                         (class (find-class 'ns:ns-window))
                         (title nil)
                         (x 200.0)
                         (y 200.0)
                         (height 200.0)
                         (width 500.0)
                         (closable t)
                         (iconifyable t)
                         (metal t)
                         (expandable t)
                         (backing :buffered)
                         (defer t)
                         (accepts-mouse-moved-events nil)
                         (auto-display t)
                         (activate t))
  (ns:with-ns-rect (frame x y width height)
    (let* ((stylemask
            (logior #$NSTitledWindowMask
                    (if closable #$NSClosableWindowMask 0)
                    (if iconifyable #$NSMiniaturizableWindowMask 0)
                    (if expandable #$NSResizableWindowMask 0)
		    (if metal #$NSTexturedBackgroundWindowMask 0)))
           (backing-type
            (ecase backing
              ((t :retained) #$NSBackingStoreRetained)
              ((nil :nonretained) #$NSBackingStoreNonretained)
              (:buffered #$NSBackingStoreBuffered)))
           (w (make-instance
	       class
	       :with-content-rect frame
	       :style-mask stylemask
	       :backing backing-type
	       :defer defer)))
      (setf (get-cocoa-window-flag w :accepts-mouse-moved-events)
            accepts-mouse-moved-events
            (get-cocoa-window-flag w :auto-display)
            auto-display)
      (when activate (activate-window w))
      (when title (#/setTitle: w (%make-nsstring title)))
      w)))




