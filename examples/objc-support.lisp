;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BRIDGE"))


#+apple-objc
(progn
  (let* ((class-count 0))
    (declare (fixnum class-count))
    (defun reset-objc-class-count () (setq class-count 0))
    (defun map-objc-classes ()
      (let* ((n (#_objc_getClassList (%null-ptr) 0)))
	(declare (fixnum n))
	(if (> n class-count)
	  (%stack-block ((buffer (the fixnum (ash n ppc32::word-shift))))
	    (#_objc_getClassList buffer n)
	  (do* ((i class-count (1+ i)))
	       ((= i n (setq class-count i)))
	    (declare (fixnum i))
	    (install-foreign-objc-class
	     (%get-ptr buffer (the fixnum  (ash i ppc32::word-shift))))))))))
  (reset-objc-class-count)
  (map-objc-classes)
  (canonicalize-type-signature-classes)
)

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

#+apple-objc
(progn
;;; NSException-handling stuff.
;;; First, we have to jump through some hoops so that #_longjmp can
;;; jump through some hoops (a jmp_buf) and wind up throwing to a
;;; lisp catch tag.

;;; These constants (offsets in the jmp_buf structure) come from
;;; the _setjmp.h header file in the Darwin LibC source.

(defconstant JMP-lr #x54 "link register (return address) offset in jmp_buf")
(defconstant JMP-ctr #x5c "count register jmp_buf offset")
(defconstant JMP-sp 0 "stack pointer offset in jmp_buf")
(defconstant JMP-r13 8 "offset of r13 (which we clobber) in jmp_buf")

;;; A malloc'ed pointer to two words of machine code.  The first
;;; instruction (rather obviously) copies r13 to r4.  A C function
;;; passes its second argument in r4, but since r4 isn't saved in a
;;; jmp_buf, we have to do this copy.  The second instruction just
;;; jumps to the address in the count register, which is where we
;;; really wanted to go in the first place.

(macrolet ((ppc-lap-word (instruction-form)
             (uvref (uvref (compile nil
                                    `(lambda (&lap 0)
                                      (ppc-lap-function () ((?? 0))
                                       ,instruction-form)))
                           0) 0)))
  (defloadvar *setjmp-catch-lr-code*
      (let* ((p (malloc 8)))
        (setf (%get-unsigned-long p 0) (ppc-lap-word (mr 4 13))
              (%get-unsigned-long p 4) (ppc-lap-word (bctr)))
        ;;; Force this code out of the data cache and into memory, so
        ;;; that it'll get loaded into the icache.
        (ff-call (%kernel-import #.target::kernel-import-makedataexecutable) 
                 :address p 
                 :unsigned-fullword 8
                 :void)
        p)))

;;; Catch frames are allocated on a stack, so it's OK to pass their
;;; addresses around to foreign code.
(defcallback throw-to-catch-frame (:signed-fullword value
                                   :address frame
                                   :void)
  (throw (%get-object frame target::catch-frame.catch-tag) value))

;;; Initialize a jmp_buf so that when it's #_longjmp-ed to, it'll
;;; wind up calling THROW-TO-CATCH-FRAME with the specified catch
;;; frame as its second argument.  The C frame used here is just
;; an empty C stack frame from which the callback will be called.

(defun %associate-jmp-buf-with-catch-frame (jmp-buf catch-frame c-frame)
  (%set-object jmp-buf JMP-sp c-frame)
  (%set-object jmp-buf JMP-r13 catch-frame)
  (setf (%get-ptr jmp-buf JMP-lr) *setjmp-catch-lr-code*
        (%get-ptr jmp-buf JMP-ctr) throw-to-catch-frame)
  t)

)

(defvar *condition-id-map* (make-id-map) "Map lisp conditions to small integers")

;;; Encapsulate an NSException in a lisp condition.
(define-condition ns-exception (error)
  ((ns-exception :initarg :ns-exception :accessor ns-exception))
  (:report (lambda (c s)
             (format s "Objective-C runtime exception: ~&~a"
                     (nsobject-description (ns-exception c))))))



(defclass ns-lisp-exception (ns::ns-exception)
    ((condition :initarg :condition :reader ns-lisp-exception-condition))
  (:metaclass ns::+ns-object))

(define-objc-method ((:id init)
		     ns-lisp-exception)
  (send self
	:init-with-name #@"lisp exception"
	:reason #@"lisp exception"
	:user-info (%null-ptr)))

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
  (send (@class ns-autorelease-pool) 'show-pools))

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


(provide "OBJC-SUPPORT")
