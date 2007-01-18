;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BRIDGE"))

#-apple-objc-2.0
(progn
  (def-foreign-type :<CGF>loat :float)
  (def-foreign-type :<NSUI>nteger :unsigned)'
  (def-foreign-type :<NSI>nteger :signed))

(defconstant +cgfloat-zero+
  #+(and apple-objc-2.0 64-bit-target) 0.0d0
  #-(and apple-objc-2.0 64-bit-target) 0.0f0)

#+apple-objc
(defun iterate-over-objc-classes (fn)
  (let* ((n (#_objc_getClassList (%null-ptr) 0)))
    (declare (fixnum n))
    (%stack-block ((buffer (the fixnum (ash n target::word-shift))))
      (#_objc_getClassList buffer n)
      (do* ((i 0 (1+ i)))
           ((= i n) (values))
        (declare (fixnum i))
        (funcall fn (%get-ptr buffer (the fixnum  (ash i target::word-shift))))))))

#+gnu-objc
(defun iterate-over-objc-classes (fn)
  (rletZ ((enum-state :address))
    (loop
      (let* ((class (#_objc_next_class enum-state)))
        (if (%null-ptr-p class)
          (return)
          (funcall fn class))))))

(defun %note-protocol (p)
  (with-macptrs ((cname (objc-message-send p "name" :address)))
    (let* ((namelen (%cstrlen cname))
           (name (make-string namelen)))
      (declare (dynamic-extent name))
      (%str-from-ptr cname namelen name)
      (unless (gethash name *objc-protocols*)
        (setf (gethash (subseq name 0) *objc-protocols*)
              (%inc-ptr p 0))))))

(defun note-class-protocols (class)
  #-apple-objc-2.0
  (do* ((protocols (pref class :objc_class.protocols)
                   (pref protocols :objc_protocol_list.next)))
       ((%null-ptr-p protocols))
    (let* ((count (pref protocols :objc_protocol_list.count)))
      (with-macptrs ((list (pref protocols :objc_protocol_list.list)))
        (dotimes (i count)
          (with-macptrs ((p (paref list (:* (:* (:struct :<P>rotocol))) i)))
            (%note-protocol p))))))
  #+apple-objc-2.0
  (rlet ((p-out-count :int))
    (with-macptrs ((protocols (#_class_copyProtocolList class p-out-count)))
      (let* ((n (pref p-out-count :int)))
        (dotimes (i n)
          (with-macptrs ((p (paref protocols (:* (:* (:struct :<P>rotocol))) i)))
            (%note-protocol p))))
      (unless (%null-ptr-p protocols) (#_free protocols)))))
            

(defun map-objc-classes (&optional (lookup-in-database-p t))
  (iterate-over-objc-classes
   #'(lambda (class)
       (note-class-protocols class)
       (install-foreign-objc-class class lookup-in-database-p))))
  

(map-objc-classes)

#+gnu-objc
(defun iterate-over-class-methods (class method-function)
  (do* ((mlist (pref class :objc_class.methods)
	       (pref mlist :objc_method_list.method_next)))
       ((%null-ptr-p mlist))
    (do* ((n (pref mlist :objc_method_list.method_count))
	  (i 0 (1+ i))
	  (method (pref mlist :objc_method_list.method_list)
		  (%incf-ptr method (record-length :objc_method))))
	 ((= i n))
      (declare (fixnum i n))
      (funcall method-function method class))))

#+gnu-objc
(progn
  ;; Er, um ... this needs lots-o-work.a
  (let* ((objc-class-count 0))
    (defun reset-objc-class-count () (setq objc-class-count 0))
    (defun note-all-library-methods (method-function)
      (do* ((i objc-class-count (1+ i))
	    (class (id->objc-class i) (id->objc-class i)))
	   ((eq class 0))
	(iterate-over-class-methods class method-function)
	(iterate-over-class-methods (id->objc-metaclass i) method-function))))
  (def-ccl-pointers revive-objc-classes ()
    (reset-objc-class-count)))

(defun retain-obcj-object (x)
  (objc-message-send x "retain"))



(defvar *condition-id-map* (make-id-map) "Map lisp conditions to small integers")

;;; Encapsulate an NSException in a lisp condition.
(define-condition ns-exception (error)
  ((ns-exception :initarg :ns-exception :accessor ns-exception))
  (:report (lambda (c s)
             (format s "Objective-C runtime exception: ~&~a"
                     (nsobject-description (ns-exception c))))))



(defclass ns-lisp-exception (ns::ns-exception)
    ((condition :initarg :condition :initform nil :reader ns-lisp-exception-condition))
  (:metaclass ns::+ns-object))

(define-objc-method ((:id init)
		     ns-lisp-exception)
  (send self
	:init-with-name #@"lisp exception"
	:reason #@"lisp exception"
	:user-info (%null-ptr)))


(define-objc-method ((:id reason) ns-lisp-exception)
  (with-slots (condition) self
    (if condition
      (%make-nsstring (format nil "~A" condition))
      (send-super 'reason))))
    
(define-objc-method ((:id description) ns-lisp-exception)
  (send (find-class 'ns:ns-string)
        :string-with-format #@"Lisp exception: %@"
        (:id (send self 'reason))))


        
             
                     
(defun ns-exception->lisp-condition (nsexception)
  (if (typep nsexception 'ns-lisp-exception)
    (ns-lisp-exception-condition nsexception)
    (make-condition 'ns-exception :ns-exception nsexception)))


(defmethod ns-exception ((c condition))
  "Map a lisp condition object to an NSException.  Note that instances
of the NS-EXCEPTION condition class implement this by accessing an
instance variable."
  ;;; Create an NSLispException with a lispid that encapsulates
  ;;; this condition.
  ;;;


  #|(dbg (format nil "~a" c))|#
  ;;(#_NSLog #@"Lisp exception: %@" :id (%make-nsstring (format nil "~a" c)))
  (make-instance 'ns-lisp-exception :condition c))
  




#+apple-objc
(progn
;;; (#__NSRaiseError nsexception) is entirely equivalent to
;;; -[NSException raise].  If we get nervous about passing the former
;;; around, we can always look up the method imp of the latter.
(defmacro raising-ns-exception-on-error (&body body)
  `(handler-case (progn ,@body)
    (error (c) (external-call "__NSRaiseError" :address (ns-exception c) :void))))

(defun objc-callback-error-return (condition return-value-pointer return-address-pointer)
  (%set-object return-address-pointer 0
               (%reference-external-entry-point (load-time-value (external "__NSRaiseError"))))
  (setf (%get-ptr return-value-pointer 0) (ns-exception condition))
  nil)

)



(defun open-main-bundle ()
  (send (@class ns-bundle) 'main-bundle))

;;; Create a new immutable dictionary just like src, replacing the
;;; value of "newkey" with "newvalue".
(defun copy-dictionary (src &rest key-value-pairs)
  (declare (dynamic-extent key-value-pairs))
  ;(#_NSLog #@"src = %@" :id src)
  (let* ((count (send src 'count))
	 (enum (send src 'key-enumerator))
	 (keys (send (@class "NSMutableArray") :array-with-capacity count))
	 (values (send (@class "NSMutableArray") :array-with-capacity count)))
    (loop
	(let* ((nextkey (send enum 'next-object)))
	  (when (%null-ptr-p nextkey)
	    (return))
	  (do* ((kvps key-value-pairs (cddr kvps))
		(newkey (car kvps) (car kvps))
		(newval (cadr kvps) (cadr kvps)))
	       ((null kvps)
		;; Copy the key, value pair from the src dict
                (send keys :add-object nextkey)
                (send values :add-object (send src :object-for-key nextkey)))
	    (when (send nextkey :is-equal-to-string  newkey)
              (send keys :add-object nextkey)
              (send values :add-object newval)
	      (return)))))
    (make-objc-instance 'ns-dictionary
                        :with-objects values
                        :for-keys keys)))


(defun nsobject-description (nsobject)
  "Returns a lisp string that describes nsobject.  Note that some
NSObjects describe themselves in more detail than others."
  (with-autorelease-pool
      (lisp-string-from-nsstring  (send nsobject 'description))))




;;; This can fail if the nsstring contains non-8-bit characters.
(defun lisp-string-from-nsstring-substring (nsstring start length)
  (%stack-block ((cstring (1+ length)))
    (send nsstring
          :get-c-string cstring
          :max-length length
          :range (ns-make-range start length)
          :remaining-range (%null-ptr))
    (%get-cstring cstring)))

(def-standard-initial-binding *listener-autorelease-pool* nil)

(setq *listener-autorelease-pool* (create-autorelease-pool))

(define-toplevel-command :global rap () "Release and reestablish *LISTENER-AUTORELEASE-POOL*"
  (when (eql *break-level* 0)
    (without-interrupts
     (when (boundp '*listener-autorelease-pool*)
       (let* ((old *listener-autorelease-pool*))
	 (if old (release-autorelease-pool old))
	 (setq *listener-autorelease-pool* (create-autorelease-pool)))))))

#+apple-objc
(defun show-autorelease-pools ()
  (objc-message-send (@class ns-autorelease-pool) "showPools" :void))

#+gnu-objc
(defun show-autorelease-pools ()
  (do* ((current (objc-message-send (@class ns-autorelease-pool) "currentPool")
		 (objc-message-send current "_parentAutoreleasePool"))
	(i 0 (1+ i)))
       ((%null-ptr-p current) (values))
    (format t "~& ~d : ~a [~d]"
	    i
	    (nsobject-description current)
	    (pref current :<NSA>utorelease<P>ool._released_count))))

(define-toplevel-command :global sap () "Log information about current thread's autorelease-pool(s) to C's standard error stream"
  (show-autorelease-pools))

(define-toplevel-command :global kap () "Release (but don't reestablish) *LISTENER-AUTORELEASE-POOL*"
  (when (eql *break-level* 0)
    (without-interrupts
     (when (boundp '*listener-autorelease-pool*)
       (let* ((p *listener-autorelease-pool*))
	 (setq *listener-autorelease-pool* nil)
	 (release-autorelease-pool p))))))

;;; Use the interfaces for an add-on ObjC framework.  We need to
;;; tell the bridge to reconsider what it knows about the type
;;; signatures of ObjC messages, since the new headers may define
;;; a method whose type signature differs from the message's existing
;;; methods.  (This probably doesn't happen too often, but it's
;;; possible that some SENDs that have already been compiled would
;;; need to be recompiled with that augmented method type info, e.g.,
;;; because ambiguity was introduced.)

(defun augment-objc-interfaces (dirname)
  (use-interface-dir dirname)
  (update-objc-method-info))

;;; A list of "standard" locations which are known to contain
;;; framework bundles.  We should look in ~/Library/Frameworks/" first,
;;; if it exists.
(defparameter *standard-framework-directories*
  (list #p"/Library/Frameworks/"
        #p"/System/Library/Frameworks/"))



;;; This has to run during application (re-)initializtion, so it
;;; uses lower-level bridge features.
(defun %reload-objc-framework (path)
  (when (probe-file path)
    (let* ((namestring (native-translated-namestring path)))
      (with-cstrs ((cnamestring namestring))
        (with-nsstr (nsnamestring cnamestring (length namestring))
          (with-autorelease-pool
              (let* ((bundle (send (@class "NSBundle")
                                   :bundle-with-path nsnamestring)))
                (unless (%null-ptr-p bundle)
                  (coerce-from-bool
                   (objc-message-send bundle "load" :<BOOL>))))))))))


(defun load-objc-extension-framework (name)
  (let* ((dirs *standard-framework-directories*)
         (home-frameworks (make-pathname :defaults nil
                                         :directory
                                         (append (pathname-directory
                                                  (user-homedir-pathname))
                                                 '("Library" "Frameworks"))))
         (fname (list (format nil "~a.framework" name))))
    (when (probe-file home-frameworks)
      (pushnew home-frameworks dirs :test #'equalp))
    (dolist (d dirs)
      (let* ((path (probe-file (make-pathname :defaults nil
                                              :directory (append (pathname-directory d)
                                                                 fname)))))
        (when path
          (let* ((namestring (native-translated-namestring path)))
            (with-cstrs ((cnamestring namestring))
              (with-nsstr (nsnamestring cnamestring (length namestring))
                (with-autorelease-pool
                    (let* ((bundle (send (find-class 'ns:ns-bundle)
                                         :bundle-with-path nsnamestring))
                           (winning (unless (%null-ptr-p bundle)
                                      (or t
                                          (send (the ns:ns-bundle bundle) 'load)))))
                      (when winning
                        (let* ((libpath (send bundle 'executable-path)))
                          (unless (%null-ptr-p libpath)
                            (open-shared-library (lisp-string-from-nsstring
                                                  libpath))))
                        (send (the ns:ns-bundle bundle) 'load)
                        (pushnew path *extension-framework-paths*
                                 :test #'equalp)
                        (map-objc-classes))
                      (return winning)))))))))))

                      
(defmethod print-object ((p ns:protocol) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a (#x~x)"
            (%get-cstring (send p 'name))
            (%ptr-to-int p))))

                                         


(provide "OBJC-SUPPORT")
