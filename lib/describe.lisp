; -*- Mode:Lisp; Package:INSPECTOR -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

(defpackage "INSPECTOR"
  (:use "CL" "CCL"))

(in-package "INSPECTOR")

(defvar ccl::@)

; The basic inspector object.
; Note that this knows nothing about windows.
; It merely knows how to number the constituent parts of an object,
; How to access a constituent, and how to print a constituent to a stream.
(defclass inspector ()
  ((object :accessor inspector-object :initarg :object)
   (line-count :accessor inspector-line-count :initarg :line-count :initform nil)))

; The usual way to cons up an inspector
(defmethod make-inspector (object)
  (multiple-value-bind (class alias) (inspector-class object)
    (make-instance class :object (or alias object))))

(defmethod initialize-instance :after ((i inspector) &key update-line-count)
  (when update-line-count
    (update-line-count i)))

;;;;;;;
;;
;; The protocol for an inspector.
;; Change these to defgeneric's when it exists.
;;
;; Usually, you need to define methods only for
;; inspector-class, compute-line-count, line-n, and (setf line-n)

; Return the type of inspector for an object
(defmethod inspector-class (object)
  (cond ((method-exists-p #'line-n object 0) 'usual-inspector)
        ((and (uvectorp object)
              (find-class 'uvector-inspector nil))
         'uvector-inspector)
        (t 'basic-inspector)))

; Return three values: the value, label, and type of the nth line of the object
; Valid types are:
;  :NORMAL or NIL  - a normal constituent line: changeable
;  :COLON          - a normal line with ": " between the label and the value
;  :COMMENT        - a commentary line - Print only the label
;  :STATIC         - a commentary line with an inspectable value: not changeable
(defmethod line-n ((i inspector) n)
  (declare (ignore n)))

; set the value of line n of the object (the label is fixed)
(defmethod (setf line-n) (value (i inspector) n)
  (declare (ignore value n)))

; Compute the number of lines in the object
(defmethod compute-line-count ((i inspector))
  0
  )

; Compute the number of lines in the object and set the line-count slot
; If the length is greater than the limit, return (list limit)
(defun update-line-count (inspector)
  (setf (inspector-line-count inspector) (compute-line-count inspector)))

; Print the nth line to a stream
(defmethod prin1-line-n ((i inspector) stream n)
  (multiple-value-call #'prin1-line i stream (line-n i n)))

(defmethod prin1-line ((i inspector) stream value &optional
                       label type function)
  (unless function
    (setq function (inspector-print-function i type)))
  (funcall function i stream value label type))

(defmethod inspector-print-function ((i inspector) type)
  (if (consp type) (setq type (car type)))
  (if (eq type :comment)
    'prin1-comment
    'prin1-normal-line))


; Print a value to a stream.
(defmethod prin1-normal-line ((i inspector) stream value &optional label type
                              colon-p)
  (let* ((type-sym (parse-type i type)))
    (if (eq type-sym :colon) (setq colon-p t))
    (when label
      (prin1-label i stream value label type)
      (if colon-p (princ ": " stream)))
    (end-of-label stream)              ; used by cacheing code
    (prin1-value i stream value label type)))

(defun prin1-colon-line (i stream value &optional label type)
  (prin1-normal-line i stream value label type t))

(defmethod prin1-label ((i inspector) stream value &optional label type)
  (declare (ignore value type))
  (if (stringp label)
    (write-string label stream)
    (princ label stream)))

(defmethod prin1-value ((i inspector) stream value &optional label type)
  (declare (ignore label type))
  (prin1 value stream))

(defmethod prin1-comment ((i inspector) stream value &optional label type)
  (when label
    (prin1-label i stream value label type)
    (end-of-label stream)))
  
; Call function on the inspector object and its value, label, & type, for
; each line in the selected range (default to the whole thing).
; This can avoid (e.g.) doing NTH for each element of a list.
; This is the generic-function which the inspector-window uses to
; display a screenful.
(defmethod map-lines ((i inspector) function &optional 
                      (start 0) 
                      end)
  (unless end
    (setq end (inspector-line-count i)))
  (when (and start end)
    (let ((index start))
      (dotimes (c (- end start))
        (declare (fixnum c))
        (multiple-value-call function i (line-n i index))
        (incf index)))))

;;;;;;;
;;
;; Dealing with unbound slots and bogus objects
;;
(defclass unbound-marker () ())

(defvar *unbound-marker* (make-instance 'unbound-marker))
(defvar *slot-unbound-marker* (make-instance 'unbound-marker))

(defmethod print-object ((x unbound-marker) stream)
  (print-object (ccl::%unbound-marker-8) stream))

(defclass bogus-object-wrapper ()
  ((address :initarg :address)))

(defmethod print-object ((x bogus-object-wrapper) stream)
  (print-unreadable-object (x stream)
    (format stream "BOGUS object @ #x~x" (slot-value x 'address))))

(defvar *bogus-object-hash*
  (make-hash-table :test 'eql :weak :value :size 0))

(defun bogus-object-wrapper (x)
  (let ((address (%address-of x)))
    (or (gethash address *bogus-object-hash*)
        (setf (gethash address *bogus-object-hash*)
              (make-instance 'bogus-object-wrapper :address address)))))

(defun eliminate-unbound (x)
  (cond ((eq x (ccl::%unbound-marker-8))
         *unbound-marker*)
        ((eq x (ccl::%slot-unbound-marker))
         *slot-unbound-marker*)
        ((ccl::bogus-thing-p x)
         (bogus-object-wrapper x))
        (t x)))

(defun restore-unbound (x)
  (if (eq x *unbound-marker*)
    (ccl::%unbound-marker-8)
    (if (eq x *slot-unbound-marker*)
      (ccl::%slot-unbound-marker)
      x)))

(defmethod line-n :around ((i inspector) n)
  (declare (ignore n))
  (let ((res (multiple-value-list (call-next-method))))
    (declare (dynamic-extent res))
    (apply #'values (eliminate-unbound (car res)) (cdr res))))

(defmethod (setf line-n) :around (new-value (i inspector) n)
  (call-next-method (restore-unbound new-value) i n))


;;;;;;;
;;
;; describe-object
;; Eventually, this wants to reuse a global inspector rather than
;; consing one.
(defparameter *describe-pretty* t)

(defmacro with-errorfree-printing (&body body)
  `(let ((*print-readably* nil)
         (*signal-printing-errors* nil))
     ,@body))

(defun describe (object &optional stream)
  (cond ((null stream) (setq stream *standard-output*))
        ((eq stream t) (setq stream *terminal-io*)))
  (setq stream (require-type stream 'stream))
  (let* ((*print-circle* t)
         (*print-length* 20))
    (describe-object object stream)
    (values)))

(defmethod describe-object (object stream)
  (let ((inspector (make-inspector object)))
    (when (null (inspector-line-count inspector))
      (update-line-count inspector))
    (with-errorfree-printing
        (let* ((*print-pretty* (or *print-pretty* *describe-pretty*))
               (temp #'(lambda (i value &rest rest)
                         (declare (dynamic-extent rest))
                         (apply #'prin1-line i stream value rest)
                         (terpri stream))))
          (declare (dynamic-extent temp))
          (map-lines inspector temp))))
  (values))

;; usual-inspector
;; Objects that know how to inspect themselves but don't need any
;; special info other than the object can be a usual-inspector.
;; This class exists mostly to save consing a class for every type
;; of object in the world.
(defclass usual-inspector (inspector)
  ())

;;;;;;;
;;
;; formatting-inspector
;; This one prints using a format string.
;; Expects line-n to return (values value label type format-string)

(defclass formatting-inspector (inspector) ())
(defclass usual-formatting-inspector (usual-inspector formatting-inspector) ())

(defmethod prin1-line ((i formatting-inspector) stream value
                       &optional label type (format-string "~s"))
  (if (eq :comment (if (consp type) (car type) type))
    (prin1-comment i stream value label type)
    (funcall (if (listp format-string) #'apply #'funcall)
             #'format-normal-line i stream value label type format-string)))

(defmethod format-normal-line ((i inspector) stream value &optional 
                               label type (format-string "~s") colon-p)
  (let* ((type-sym (parse-type i type)))
    (if (eq type-sym :colon) (setq colon-p t))
    (when label
      (if (stringp label)
          (write-string label stream)
          (princ label stream))
      (if colon-p (princ ": " stream)))
    (end-of-label stream)              ; used by cacheing code
    (format stream format-string value)))

;;;;;;;
;;
;; inspectors for CCL objects
;;


(defmethod parse-type ((i inspector) type &optional default1 default2)
  (declare (ignore default1 default2))
  (values (if (consp type) (car type) type)))

;;; Used by the cache-entry-stream class to save the column where the label ends.
(defmethod end-of-label (stream)
  (declare (ignore stream)))



;;;;;
;;
;; The default inspector class
;; Used when we don't know what else to do
;;

(defclass basic-inspector (inspector) ())

(defmethod compute-line-count ((i basic-inspector))
  3)                                    ; type, class, value

(defun line-n-out-of-range (i n)
  (error "~s is not a valid index for line-n of ~s" n i))

(defun setf-line-n-out-of-range (i n)
  (error "~s is not a valid index for setf-line-n of ~s" n i))

(defmethod line-n ((i basic-inspector) n)
  (let ((object (inspector-object i)))
    (case n
      (0 (values object nil :static))
      (1 (values (type-of object) "Type: " :static))
      (2 (values (class-of object) "Class: " :static))
      (t (line-n-out-of-range i n)))))

;;;;;;;
;;
;; Automate the object being the first line
;;
(defclass object-first-mixin () ())
(defclass object-first-inspector (object-first-mixin inspector) ())

(defmethod compute-line-count :around ((i object-first-mixin))
  (1+ (call-next-method)))

(defmethod line-n :around ((i object-first-mixin) n)
  (if (eql 0 n)
    (values (inspector-object i) nil)
    (call-next-method i (1- n))))

(defmethod (setf line-n) :around (value (i object-first-mixin) n)
  (if (eql n 0)
    (replace-object i value)
    (call-next-method value i (1- n))))

(defun replace-object (inspector new-object)
  (declare (ignore inspector))
  (make-inspector new-object))


; A mixin that displays the object, its type, and its class as the first three lines.
(defclass basics-first-mixin () ())

(defmethod compute-line-count :around ((i basics-first-mixin))
  (+ 3 (call-next-method)))

(defmethod line-n :around ((i basics-first-mixin) n)
  (let ((object (inspector-object i)))
    (case n
      (0 (values object nil))
      (1 (values (type-of object) "Type: " :static))
      (2 (values (class-of object) "Class: " :static))
      (t (call-next-method i (- n 3))))))

(defmethod (setf line-n) :around (new-value (i basics-first-mixin) n)
  (case n
    (0 (replace-object i new-value))
    ((1 2) (setf-line-n-out-of-range i n))
    (t (call-next-method new-value i (- n 3)))))

;;;;;;;
;;
(defclass usual-object-first-inspector (object-first-mixin usual-inspector)
  ())
(defclass usual-basics-first-inspector (basics-first-mixin usual-inspector)
  ())

(defvar *inspector*)

(defmethod compute-line-count ((i usual-inspector))
  (let ((*inspector* i))
    (compute-line-count (inspector-object i))))

(defmethod line-n ((i usual-inspector) n)
  (let ((*inspector* i))
    (line-n (inspector-object i) n)))

(defmethod (setf line-n) (value (i usual-inspector) n)
  (let ((*inspector* i))
    (setf (line-n (inspector-object i) n) value)))

(defmethod inspector-commands ((i usual-inspector))
  (let ((*inspector* i))
    (inspector-commands (inspector-object i))))

(defmethod inspector-commands (random)
  (declare (ignore random))
  nil)

;;;;;;;
;;
;; Bogus objects
;;

(defclass bogus-object-inspector (object-first-inspector)
  ())

(defmethod compute-line-count ((i bogus-object-inspector))
  3)

(defmethod line-n ((i bogus-object-inspector) n)
  (values
   nil
   (case n
     (0 "One cause of a bogus object is when a stack consed object is stored")
     (1 "in a register and then control exits the dynamic-extent of the object.")
     (2 "The compiler doesn't bother to clear the register since it won't be used again."))
   '(:comment :plain :plain)))

(defmethod inspector-class :around (object)
  (if (ccl::bogus-thing-p object)
    'bogus-object-inspector
    (call-next-method)))

;;;;;;;
;;
;; A general sequence inspector
;;
(defclass sequence-inspector (inspector)
  ((print-function :initarg :print-function :initform #'prin1 :reader print-function)
   (commands :initarg :commands :initform nil :accessor inspector-commands)
   (line-n-inspector :initform nil :initarg :line-n-inspector
                     :accessor line-n-inspector-function)
   (replace-object-p :initform nil :initarg :replace-object-p
                     :reader replace-object-p)
   (resample-function :initform nil :initarg :resample-function
                      :reader resample-function)
   (line-n-function :initform nil :initarg :line-n-function
                    :reader line-n-function)
   (setf-line-n-p :initform t :initarg :setf-line-n-p
                  :reader setf-line-n-p))
  (:default-initargs :update-line-count t))



(defmethod compute-line-count ((i sequence-inspector))
  (let ((resample-function (resample-function i)))
    (when resample-function
      (setf (inspector-object i) (funcall resample-function i))))
  (length (inspector-object i)))

(defmethod line-n ((i sequence-inspector) n)
  (let ((f (line-n-function i)))
    (if f
      (funcall f i n)
      (values (elt (inspector-object i) n) nil (unless (setf-line-n-p i) :static)))))

(defmethod (setf line-n) (new-value (i sequence-inspector) n)
  (if (setf-line-n-p i)
    (setf (elt (inspector-object i) n) new-value)
    (setf-line-n-out-of-range i n)))

(defmethod prin1-value ((inspector sequence-inspector) stream value
                        &optional label type)
  (declare (ignore label type))
  (funcall (print-function inspector) value stream))

(defmethod line-n-inspector ((i sequence-inspector) n value label type)
  (let ((f (line-n-inspector-function i)))
    (or (and f (funcall f i n value label type)) (call-next-method))))

;;;;;;;
;;
;; standard-object
;; This should be redone to use the exported class query functions
;; (as soon as they exist)
;;
(defclass standard-object-inspector (object-first-inspector)
  ())

(defmethod inspector-class ((o standard-object))
  'standard-object-inspector)

(defmethod compute-line-count ((i standard-object-inspector))
  (standard-object-compute-line-count i))

(defun standard-object-compute-line-count (i)  
  (let* ((object (ccl::maybe-update-obsolete-instance (inspector-object i)))
         (class (class-of object)))
    (multiple-value-bind (instance-slots class-slots) (ccl::extract-instance-and-class-slotds (ccl::class-slots class))
      (let* ((ninstance-slots (length instance-slots))
             (nclass-slots (length class-slots)))
        (+ 2                                ; class, wrapper
           (if (eql 0 ninstance-slots)
             0
             (1+ ninstance-slots))
           (if (eql 0 nclass-slots)
             0
             (1+ nclass-slots))
           (if (eql 0 (+ nclass-slots ninstance-slots))
             1
             0))))))

(defun slot-value-or-unbound (instance slot-name)
  (eliminate-unbound (ccl::slot-value-if-bound instance slot-name
					       (ccl::%slot-unbound-marker))))

(defparameter *standard-object-type* (list nil))
(defparameter *standard-object-static-type*
  (cons :static (cdr *standard-object-type*)))
(defparameter *standard-object-comment-type* 
  (list :comment))

(defmethod line-n ((i standard-object-inspector) n)
  (standard-object-line-n i n))

(defmethod prin1-label ((i standard-object-inspector) stream value &optional label type)
  (declare (ignore value type))
  (if (symbolp label)
    (prin1 label stream)
    (call-next-method)))

; Looks like
; Class:
; Wrapper:
; [Instance slots:
;  slots...]
; [Class slots:
;  slots...]
(defun standard-object-line-n (i n)
  (let* ((instance (inspector-object i))
         (class (class-of instance))
         (wrapper (ccl::standard-object-p instance))
	 (instance-start 2))
    (if (< n instance-start)
      (if (eql n 0)
	(values class "Class: " :normal)
	(values wrapper "Wrapper: " :static))
      (let* ((slotds (ccl::extract-instance-effective-slotds class))
             (instance-count (length slotds))
             (shared-start (+ instance-start instance-count
                              (if (eql 0 instance-count) 0 1))))
        (if (< n shared-start)
          (if (eql n instance-start)
            (values nil "Instance slots" :comment)
            (let ((slot-name (slot-definition-name
                              (elt slotds (- n instance-start 1)))))
              (values (slot-value-or-unbound instance slot-name)
                      slot-name
                      :colon)))
          (let* ((slotds (ccl::extract-class-effective-slotds class))
                 (shared-count (length slotds))
                 (shared-end (+ shared-start shared-count
                                (if (eql shared-count 0) 0 1))))
            (if (< n shared-end)
              (if (eql n shared-start)
                (values nil "Class slots" :comment)
                (let ((slot-name (slot-definition-name 
                                  (elt slotds (- n shared-start 1)))))
                  (values (slot-value-or-unbound instance slot-name)
                           slot-name
                           :colon)))
              (if (and (eql 0 instance-count) (eql 0 shared-count) (eql n shared-end))
                (values nil "No Slots" :comment)
                (line-n-out-of-range i n)))))))))

(defmethod (setf line-n) (value (i standard-object-inspector) n)
  (standard-object-setf-line-n value i n))

(defun standard-object-setf-line-n (value i n)
  (let* ((instance (inspector-object i))
         (class (class-of instance))
         (forwarded-p (forwarded-p i))
         (instance-start (if forwarded-p 3 2)))
    (if (< n instance-start)
      (cond
       ((eql n 0) (change-class instance value)
         (update-line-count i))
        (t (setf-line-n-out-of-range i n)))
      (let* ((slotds (ccl::extract-instance-effective-slotds class))
             (instance-count (length slotds))
             (shared-start (+ instance-start instance-count
                              (if (eql 0 instance-count) 0 1))))
        (if (< n shared-start)
          (if (eql n instance-start)
            (setf-line-n-out-of-range i n)
            (let ((slot-name (slot-definition-name
                              (elt slotds (- n instance-start 1)))))
              (setf (slot-value instance slot-name) (restore-unbound value))))
          (let* ((slotds (ccl::extract-class-effective-slotds class))
                 (shared-count (length slotds))
                 (shared-end (+ shared-start shared-count
                                (if (eql shared-count 0) 0 1))))
            (if (< n shared-end)
              (if (eql n shared-start)
                (setf-line-n-out-of-range i n)
                (let ((slot-name (slot-definition-name 
                                  (elt slotds (- n shared-start 1)))))
                  (setf (slot-value instance slot-name)
                        (restore-unbound value))))
              (setf-line-n-out-of-range i n))))))))


;;;;;;;;;;;  Inspector objects for common classes.

(defparameter *plain-comment-type* '(:comment (:plain)))
(defparameter *bold-comment-type* '(:comment (:bold)))

(defun resample-it ()
  )

;;;;;;;
;;
;; Lists
;;
(defclass cons-inspector (basics-first-mixin inspector) ())

(defclass list-inspector (basics-first-mixin inspector)
  ((length :accessor list-inspector-length)
   (dotted-p :accessor list-inspector-dotted-p)
   (nthcdr :accessor list-inspector-nthcdr)
   (n :accessor list-inspector-n)))

(defmethod inspector-class ((o list))
  (if (listp (cdr o))
    'list-inspector
    'cons-inspector))

; Same as list-length-and-final-cdr, but computes the real length of the list
(defun real-list-length (list)
  (multiple-value-bind (len final-cdr max-circ-len)
      (ccl::list-length-and-final-cdr list)
    (if (null max-circ-len)
      (values len final-cdr nil)
      (let ((middle (nthcdr max-circ-len list))
            (n 1))
        (loop (when (eq list middle) (return))
          (pop list)
          (incf n))
        (pop list)
        (loop (when (eq list middle) (return))
          (pop list)
          (incf n))
        (values nil nil n)))))        

(defmethod compute-line-count ((i list-inspector))
  (multiple-value-bind (len final-cdr circ-len) (real-list-length (inspector-object i))
    (setf (list-inspector-dotted-p i) final-cdr)
    (setf (list-inspector-nthcdr i) (inspector-object i))
    (setf (list-inspector-n i) 0)
    (+ 1                                ; regular, dotted, or circular
       1                                ; length
       (abs (setf (list-inspector-length i)
                  (or len (- circ-len))))   ; the elements
       (if final-cdr 2 0))))            ; the final-cdr and it's label

(defmethod compute-line-count ((i cons-inspector))
  2)                                    ; car & cdr

(defmethod line-n ((i list-inspector) en &aux (n en))
  (let* ((circ? (list-inspector-length i))
         (length (abs circ?)))
    (cond ((eql 0 n)
           (values nil (cond ((list-inspector-dotted-p i) "Dotted List")
                             ((< circ? 0) "Circular List")
                             (t "Normal List"))
                   *plain-comment-type*))
          ((eql 0 (decf n)) (values length "Length: "))
          ((>= (decf n) (setq length length))   ; end of dotted list
           (let ((final-cdr (list-inspector-dotted-p i)))
             (unless final-cdr (line-n-out-of-range i en))
             (if (eql n length)
               (values nil "Non-nil final cdr" *plain-comment-type*)
               (values final-cdr (- length 0.5) :colon))))
          (t (let* ((saved-n (list-inspector-n i))
                    (nthcdr (if (>= n saved-n)
                              (nthcdr (- n saved-n) (list-inspector-nthcdr i))
                              (nthcdr n (inspector-object i)))))
               (setf (list-inspector-nthcdr i) nthcdr
                     (list-inspector-n i) n)
               (values (car nthcdr) n :colon))))))

(defmethod line-n ((i cons-inspector) n)
  (let ((object (inspector-object i)))
    (ecase n
           (0 (values (car object) "Car: "))
           (1 (values (cdr object) "Cdr: ")))))

(defmethod (setf line-n) (value (i list-inspector) n)
  (when (< n 2)
    (setf-line-n-out-of-range i n))
  (decf n 2)
  (setf (elt (inspector-object i) n) value)
  (resample-it))

(defmethod (setf line-n) (value (i cons-inspector) n)
  (let ((object (inspector-object i)))
    (ecase n
           (0 (setf (car object) value))
           (1 (setf (cdr object) value))))
  (resample-it))

;;;;;;;
;;
;; General uvector's
;;
(defclass uvector-inspector (basics-first-mixin inspector)
  ((name-list :initarg :name-list :initform nil :accessor name-list)))

(defmethod uvector-name-list (object) 
  (let* ((type (type-of object))
         (names (cdr (assq type ccl::*def-accessor-types*)))
         (names-size (length names))
         res)
    (when names
      (dotimes (i (uvsize object))
        (declare (fixnum i))
        (let ((name (and (> names-size i) (aref names i))))
          (if name
            (push (if (listp name) (car name) name) res)
            (if (and (eql i 0) (typep object 'ccl::internal-structure))
              (push 'type res)
              (push i res)))))
      (nreverse res))))

(defmethod compute-line-count ((i uvector-inspector))
  (setf (name-list i) (uvector-name-list (inspector-object i)))
  (uvsize (inspector-object i)))

(defmethod line-n ((i uvector-inspector) n)
  (values (uvref (inspector-object i) n)
          (or (let ((name-list (name-list i))) (and name-list (nth n (name-list i))))
              n)
          :colon))

(defmethod (setf line-n) (new-value (i uvector-inspector) n)
  (setf (uvref (inspector-object i) n) new-value))

(defmethod inspector-commands ((i uvector-inspector))
  (let ((object (inspector-object i)))
    (if (method-exists-p #'inspector-commands object)
      (inspector-commands object))))

;;;;;;;
;;
;; Vectors & Arrays
;;
(defmethod inspector-class ((v ccl::simple-1d-array))
  'usual-basics-first-inspector)

(defmethod compute-line-count ((v ccl::simple-1d-array))
  (+ 1 (length v)))

(defmethod line-n ((v ccl::simple-1d-array) n)
  (cond ((eql 0 n) (values (length v) "Length" :static 'prin1-colon-line))
        (t (decf n 1)
           (values (aref v n) n :colon))))

(defmethod (setf line-n) (value (v ccl::simple-1d-array) n)
  (when (<= n 0)
    (setf-line-n-out-of-range v n))
  (decf n 1)
  (prog1 (setf (aref v n) value)
    (resample-it)))

(defclass array-inspector (uvector-inspector) ())

(defmethod inspector-class ((v array))
  'array-inspector)

(defmethod uvector-name-list ((a array))
  (if (eql 1 (array-rank a))
    (if (array-has-fill-pointer-p a)
      '("Fill Pointer" "Physical size" "Data vector" "Displacement" "Flags")
      '("Logical size" "Physical size" "Data vector" "Displacement" "Flags"))
    `("Rank" "Physical size" "Data vector" "Displacement" "Flags" "Dim0" "Dim1" "Dim2" "Dim3")))

(defmethod compute-line-count ((i array-inspector))
  (let* ((a (inspector-object i))
         (rank (array-rank a)))
    (call-next-method)                  ; calculate name list
    (+ (if (eql rank 1) (1+ (uvsize a))  7)
       (apply #'* (array-dimensions a)))))

(defmethod line-n ((i array-inspector) n)
  (let* ((v (inspector-object i))
         (rank (array-rank v))
         (uvsize (if (eql rank 1)
                   (+ (uvsize v) 1)
                   7)))
    (cond ((eql 0 n) (values (array-element-type v)
                             (if (adjustable-array-p v)
                               "Adjustable, Element type"
                               "Element type")
                             :static 'prin1-colon-line))
          ((eql  5 n)
           (values  (uvref v ppc32::vectorH.flags-cell)
                   "Flags: "
                   :static
                   #'(lambda (i s v l type)
                       (format-normal-line i s v l type "#x~x"))))
          ((and (eql  6 n) (not (eql rank 1)))
           (values (array-dimensions v) "Dimensions: " :static))
          ((< n uvsize) (call-next-method i (1- n)))
          (t (let ((index (- n uvsize)))
               (values (row-major-aref v index) (array-indices v index) :colon))))))

(defmethod (setf line-n) (new-value (i array-inspector) n)
  (let* ((v (inspector-object i))
         (rank (array-rank v))
         (uvsize (if (eql rank 1)
                   (+ (uvsize v) 1)
                   7)))
    (prog1
      (cond ((or (eql 0 n) (eql 1 n) (and (eql 4 n) (not (eql rank 1))))
             (setf-line-n-out-of-range i n))
            ((< n uvsize)
             (if (eql 3 n)
               (setq new-value (require-type new-value 'array))
               (setq new-value (require-type new-value 'fixnum)))
             (call-next-method new-value i (1- n)))
          (t (let ((index (- n uvsize)))
               (setf (row-major-aref v index) new-value))))
      (resample-it))))

(defun array-indices (a row-major-index)
  (let ((rank (array-rank a)))
    (if (eql 1 rank)
      row-major-index
      (let ((res nil)
            dim
            (dividend row-major-index)
            remainder)
        (loop
          (when (zerop rank) (return res))
          (setq dim (array-dimension a (decf rank)))
          (multiple-value-setq (dividend remainder) (floor dividend dim))
          (push remainder res))))))
  
(defmethod prin1-line ((i array-inspector) stream value &optional
                       label type function)
  (declare (ignore stream value type function))
  (if (or (numberp label) (listp label))   ; First line or contents lines
    (call-next-method)
    (let ((*print-array* nil))
      (call-next-method))))

;;;;;;;
;;
;; Numbers
;;
(defmethod inspector-class ((num number)) 'usual-formatting-inspector)

; floats
(defmethod compute-line-count ((num float)) 5)

(defmethod line-n ((num float) n)
  (let ((type :static))
    (ecase n
      (0 (values num "Float:           " type))
      (1 (values num "Scientific:      " type
                 (if (< num 0) "~8,2e" "~7,2e")))
      (2 (values (if (zerop num) "illegal" (log num 2))
                     "Log base 2:      " type "~d"))
      (3 (values (rationalize num)
                     "Ratio equiv:     " type))
      (4 (values (round num)
                     "Nearest integer: " type)))))

; complex numbers
(defmethod compute-line-count ((num complex)) 3)

(defmethod line-n ((num complex) n)
  (let ((type :static))
    (ecase n
      (0 (values num            "Complex num:    " type))
      (1 (values (realpart num) "Real part:      " type))
      (2 (values (imagpart num) "Imaginary part: " type)))))

; ratios
(defmethod compute-line-count ((num ratio)) 6)

(defmethod line-n ((num ratio) n)
  (let ((type :static))
    (ecase n
      (0 (values num               "Ratio:           " type))
      (1 (values (float num)       "Scientific:      " type 
                 (if (< num 0) "~8,2e" "~7,2E")))
      (2 (values (if (zerop num) "illegal" (log num 2))
                                   "Log base 2:      " type "~d"))
      (3 (values (round num)       "Nearest integer: " type))
      (4 (values (numerator num)   "Numerator:       " type))
      (5 (values (denominator num) "Denominator:     " type)))))

; integers
(defmethod compute-line-count ((num integer)) 
  (let ((res 12))
    (unless (< 0 num 4000) (decf res))   ; not a roman number
    (unless (<= 0 num 255) (decf res))   ; not a character
    res))

(defmethod line-n ((num integer) n)
  (if (and (>= n 7) (not (< 0 num 4000))) (incf n))   ; maybe skip roman.
  (if (and (>= n 8) (not (<= 0 num 255))) (incf n))   ; maybe skip character.
  (let* ((type :static)
         (neg? (< num 0))
         (norm (if neg? 
                 (+ num (expt 2 (max 32 (* 4 (round (+ (integer-length num) 4) 4)))))
                 num)))
    (ecase n
      (0  (values num
                (if (fixnump num)
                  "Fixnum:      "
                  "Bignum:      ")
                type "~s"))
      (1  (values (float num)
                  "Scientific:  " type
                  (if (< num 0) "~8,2e" "~7,2e")))
      (2  (values (if (zerop num) "illegal" (log num 2)) 
                  "Log base 2:  " type "~d"))
      (3  (values norm
                  "Binary:      " type
                  (if neg? "#b...~b" "#b~b")))
      (4  (values norm
                  "Octal:       " type
                  (if neg? "#o...~o" "#o~o")))
      (5  (values num
                  "Decimal:     " type "~d."))
      (6  (values norm
                  "Hex:         " type
                  (if neg? "#x...~x" "#x~x")))
      (7  (values (format nil "~@r" num)
                  "Roman:       " type "~a"))
      (8  (values (code-char num)
                  "Character:   " type "~s"))
      (9 (values (ccl::ensure-simple-string (prin1-to-string num))
                  "Abbreviated: "
                  type #'format-abbreviated-string))
      (10 (values (or (ignore-errors (universal-time-string num)) "#<error>")
                  "As time:     " type "~a"))
      (11 (if (< num 0)
            (values most-negative-fixnum 'most-negative-fixnum type '("~d." t))
            (values most-positive-fixnum 'most-positive-fixnum type '("~d." t)))))))

(defun format-abbreviated-string (stream string)
  (setq string (require-type string 'simple-string))
  (let ((length (length string)))
    (if (< length 7)
      (princ string stream)
      (format stream "~a <- ~s digits -> ~a"
              (subseq string 0 3)
              (- length 6)
              (subseq string (- length 3) length)))))

(defun universal-time-string (num)
  (multiple-value-bind (second minute hour date month year day)
                       (decode-universal-time num)
    (with-output-to-string (s)
      (format s "~d:~2,'0d:~2,'0d " hour minute second)
      (princ (nth day '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                        "Saturday" "Sunday"))
             s)
      (format s ", ~d " date)
      (princ (nth month '("" "January" "February" "March" "April" "May" "June" "July"
                          "August" "September" "October" "November" "December"))
             s)
      (format s ", ~d" year))))

; Characters
(defmethod compute-line-count ((ch character)) 2)

(defmethod line-n ((ch character) n)
  (let ((type :static))
    (ecase n
      (0 (values ch             "Character: " type))
      (1 (values (char-code ch) "char-code: " type)))))

;;;;;;;
;;
;; Symbols
;;
(defun symbol-has-bindings-p (sym)
  (or (constantp sym) (proclaimed-special-p sym) (boundp sym)
      (special-operator-p sym) (macro-function sym) (fboundp sym)
      (type-specifier-p sym) (record-type-p sym nil)
      (find-class sym nil)))

(defmethod inspector-class ((sym symbol)) 'usual-inspector)

(defmethod compute-line-count ((sym symbol))
  (+ 1                                  ; The symbol
     (if (symbol-has-bindings-p sym) 1 0)
     1                                  ; package
     1                                  ; symbol-name
     1                                  ; symbol-value
     1                                  ; symbol-function
     (if (fboundp sym) 1 0)             ; arglist
     1                                  ; plist
     (if (find-class sym nil) 1 0)      ; class
     ))


(defmethod normalize-line-number ((sym symbol) n)
  (if (and (>= n 1) (not (symbol-has-bindings-p sym))) (incf n))
  (if (and (>= n 6) (not (fboundp sym))) (incf n))
  n)

(defmethod line-n ((sym symbol) n)
  (setq n (normalize-line-number sym n))
  (let ((type :normal)
        (comment '(:comment (:bold)))
        (static :static))
    (ecase n
      (0 (values sym "Symbol: " type))
      (1 (values nil (symbol-type-line sym) comment))
      (2 (let ((p (symbol-package sym)))
           (if (null p)
             (values nil "No home package." comment)
             (multiple-value-bind (found kind) (find-symbol (symbol-name sym) p)
               (values p 
                       (if (or (null kind) (neq found sym))
                         "NOT PRESENT in home package: "
                         (format nil "~a in package: " kind))
                       static)))))
      (3 (values (symbol-name sym) "Print name: " static))
      (4 (values (if (boundp sym) (symbol-value sym) *unbound-marker*)
                 "Value: " type))
      (5 (values (if (fboundp sym)
                   (cond ((macro-function sym))
                         ((special-operator-p sym) sym)
                         (t (symbol-function sym)))
                   *unbound-marker*)
                 "Function: " type))
      (6 (values (and (fboundp sym) (arglist sym))
                 "Arglist: " static))
      (7 (values (symbol-plist sym) "Plist: " type))
      (8 (values (find-class sym) "Class: " static)))))

(defmethod (setf line-n) (value (sym symbol) n)
  (let (resample-p)
    (setq n (normalize-line-number sym n))
    (setq value (restore-unbound value))
    (ecase n
      (0 (replace-object *inspector* value))
      ((1 2 3 6) (setf-line-n-out-of-range sym n))
      (4 (setf resample-p (not (boundp sym))
               (symbol-value sym) value))
      (5 (setf resample-p (not (fboundp sym))
               (symbol-function sym) value))
      (7 (setf (symbol-plist sym) value)))
    (when resample-p (resample-it))
    value))

(defun record-type-p (name &optional check-database)
  (declare (ignore check-database))
  (ignore-errors (ccl::%foreign-type-or-record name)))

; Add arglist here.
(defun symbol-type-line (sym)
  (let ((types (list
                (cond ((constantp sym)
                       "Constant")
                      ((proclaimed-special-p sym)
                       "Special Variable")
                      ((boundp sym)
                       "Non-special Variable")
                      (t nil))
                (cond ((special-operator-p sym)
                       "Special Form")
                      ((macro-function sym)
                       "Macro")
                      ((fboundp sym)
                       "Function")
                      (t nil))
                (if (type-specifier-p sym) "Type Specifier")
                (if (record-type-p sym nil) "Record Type")
                (if (find-class sym nil) "Class Name")))
        flag)
    (with-output-to-string (s)
      (dolist (type types)
        (when type
          (if flag (write-string ", " s))
          (setq flag t)
          (write-string type s))))))
    

(defmethod inspector-commands ((sym symbol))
  (let ((res nil))
    '(push (list "Documentation" #'(lambda () (show-documentation sym)))
          res)
    (let ((class (find-class sym nil)))
      (if class
        (push (list "Inspect Class" #'(lambda () (inspect class))) res)))
    (if (boundp sym)
      (push (list "MAKUNBOUND" #'(lambda () (when (y-or-n-p (format nil "~s?" `(makunbound ',sym)))
                                              (makunbound sym) (resample-it))))
            res))
    (if (fboundp sym)
      (push (list "FMAKUNBOUND" #'(lambda () (when (y-or-n-p (format nil "~s?" `(fmakunbound ',sym)))
                                               (fmakunbound sym) (resample-it))))
            res))
    '(if (record-type-p sym)
      (push (list "Inspect Record Type" #'(lambda () (inspect-record-type sym)))
            res))
    (nreverse res)))


(defmethod line-n-inspector ((sym symbol) n value label type)
  (declare (ignore label type))
  (setq n (normalize-line-number sym n))
  (if (eql n 7)
    (make-instance 'plist-inspector :symbol sym :object value)
    (call-next-method)))

(defclass plist-inspector (inspector)
  ((symbol :initarg :symbol :reader plist-symbol)))

(defmethod inspector-window-title ((i plist-inspector))
  (format nil "~a of ~s" 'plist (plist-symbol i)))

(defmethod compute-line-count ((i plist-inspector))
  (+ 3 (/ (length (inspector-object i)) 2)))

(defmethod line-n ((i plist-inspector) n)
  (let* ((plist (inspector-object i)))
    (cond ((eql 0 n) (values plist "Plist: "))
          ((eql 1 n) (values (plist-symbol i) "Symbol: " :static))
          ((eql 2 n) (values nil nil :comment))
          (t (let ((rest (nthcdr (* 2 (- n 3)) plist)))
               (values (cadr rest) (car rest) :colon))))))

(defmethod (setf line-n) (new-value (i plist-inspector) n)
  (let* ((plist (inspector-object i)))
    (if (eql n 0)
      (replace-object i new-value)
      (if (< n 3)
        (setf-line-n-out-of-range i n)
        (let ((rest (nthcdr (* 2 (- n 3)) plist)))
          (setf (cadr rest) new-value)
          (resample-it))))))

(defparameter *inspector-disassembly* nil)

;;;;;;;
;;
;; Functions
;;
(defclass function-inspector (inspector)
  ((disasm-p :accessor disasm-p :initform *inspector-disassembly*)
   (disasm-info :accessor disasm-info)
   (pc-width :accessor pc-width)
   (pc :initarg :pc :initform nil :accessor pc)))

(defclass closure-inspector (function-inspector)
  ((n-closed :accessor closure-n-closed)))



(defmethod inspector-class ((f function)) 'function-inspector)
(defmethod inspector-class ((f compiled-lexical-closure)) 'closure-inspector)

(defmethod compute-line-count ((f function-inspector))
  (when (typep (inspector-object f) 'ccl::interpreted-function)
    (setf (disasm-p f) nil))
  (+ 1                                  ; the function
     1                                  ; name
     1                                  ; arglist
     (compute-disassembly-lines f))) 

(defmethod line-n ((f function-inspector) n)
  (let ((o (inspector-object f)))
    (case n
      (0 (values o ""))
      (1 (values (function-name o) "Name" :colon))
      (2 (multiple-value-bind (arglist type) (arglist o)
           (let ((label (if type (format nil "Arglist (~(~a~))" type) "Arglist unknown")))
             (values arglist label (if type :colon '(:comment (:plain)))))))
      (t (disassembly-line-n f (- n 3))))))

(defmethod compute-line-count ((f closure-inspector))
  (let* ((o (inspector-object f))
	 (nclosed (nth-value 8 (function-args (ccl::closure-function o)))))
    (setf (closure-n-closed f) nclosed)
    (+ (call-next-method)
       1                              ; the function we close over
       1                              ; "Closed over values"
       nclosed
       (if (disasm-p f) 1 0))))      ; "Disassembly"

(defmethod line-n ((f closure-inspector) n)
  (let ((o (inspector-object f))
        (nclosed (closure-n-closed f)))
    (if (<= (decf n 2) 0)
      (call-next-method)
      (cond ((eql (decf n) 0)
             (values (ccl::closure-function o) "Inner lfun: " :static))
            ((eql (decf n) 0)
             (values nclosed "Closed over values" :comment #'prin1-comment))
            ((< (decf n) nclosed)
             (let* ((value (ccl::%svref o (1+ (- nclosed n))))
                    (map (car (ccl::function-symbol-map (ccl::closure-function o))))
                    (label (or (and map (svref map (+ n (- (length map) nclosed))))
                               n))
                    (cellp (ccl::closed-over-value-p value)))
               (when cellp
                 (setq value (ccl::closed-over-value value)
                       label (format nil "(~a)" label)))
               (values value label (if cellp :normal :static) #'prin1-colon-line)))
            ((eql (decf n nclosed) 0)
             (values 0 "Disassembly" :comment #'prin1-comment))
            (t (disassembly-line-n f (- n 1)))))))

(defmethod (setf line-n) (new-value (f function-inspector) n)
  (let ((o (inspector-object f)))
    (case n
      (0 (replace-object f new-value))
      (1 (ccl::lfun-name o new-value) (resample-it))
      (2 (setf (arglist o) new-value))
      (t
       (if (>= n 3) 
         (set-disassembly-line-n f (- n 3) new-value)
         (setf-line-n-out-of-range f n)))))
  new-value)

(defmethod (setf line-n) (new-value (f closure-inspector) en &aux (n en))
  (let ((o (inspector-object f))
        (nclosed (closure-n-closed f)))
    (if (<= (decf n 2) 0)               ; function itself, name, or arglist
      (call-next-method)
      (cond ((<= (decf n 2) 0)          ; inner-lfun or "Closed over values"
             (setf-line-n-out-of-range f en))
            ((< (decf n) nclosed)       ; closed-over variable
             (let* ((value (ccl::%svref o (1+ (- nclosed n))))
                    (cellp (ccl::closed-over-value-p value)))
               (unless cellp (setf-line-n-out-of-range f en))
               (ccl::set-closed-over-value value new-value)))
            ((eql (decf n nclosed) 0)   ; "Disassembly"
             (setf-line-n-out-of-range f en))
            (t (set-disassembly-line-n f (- n 1) new-value))))))

(defun compute-disassembly-lines (f &optional (function (inspector-object f)))
  (if (functionp function)
    (let* ((info (and (disasm-p f) (list-to-vector (ccl::disassemble-list function))))
           (length (length info))
           (last-pc (if info (car (svref info (1- length))) 0)))
      (if (listp last-pc) (setq last-pc (cadr last-pc)))
      (setf (pc-width f) (length (format nil "~d" last-pc)))
      (setf (disasm-info f) info)
      length)
    0))

(defun list-to-vector (list)
  (let* ((length (length list))
         (vec (make-array length)))
    (dotimes (i length)
      (declare (fixnum i))
      (setf (svref vec i) (pop list)))
    vec))

(defun disassembly-line-n (f n)
  (let* ((line (svref (disasm-info f) n))
         (value (disasm-line-immediate line)))
    (values value line (if value :static :comment))))

(defun set-disassembly-line-n (f n new-value &optional 
                                 (function (inspector-object f)))
  (declare (ignore new-value function))
  (setf-line-n-out-of-range f n))

(defun disasm-line-immediate (line &optional (lookup-functions t))
  (pop line)                        ; remove address
  (when (eq (car line) 'ccl::jsr_subprim)
    (return-from disasm-line-immediate (find-symbol (cadr line) :ccl)))
  (let ((res nil))
    (labels ((inner-last (l)
               (cond ((atom l) l)
                     ((null (cdr l)) (car l))
                     (t (inner-last (last l))))))
      (dolist (e line)
        (cond ((numberp e) (when (null res) (setq res e)))
              ((consp e)
               (cond ((eq (car e) 'function)
                      (setq res (or (and lookup-functions (fboundp (cadr e))) (cadr e))))
                     ((eq (car e) 17)   ; locative
                      (setq e (cadr e))
                      (unless (atom e)
                        (cond ((eq (car e) 'special) 
                               (setq res (cadr e)))
                              ((eq (car e) 'function) 
                               (setq res (or (and lookup-functions (fboundp (cadr e))) (cadr e))))
                              (t (setq res (inner-last e))))))
                     ((or (null res) (numberp res))
                      (setq res (inner-last e))))))))
    res))

(defmethod inspector-print-function ((i function-inspector) type)
  (declare (ignore type))
  'prin1-normal-line)

(defmethod prin1-label ((f function-inspector) stream value &optional label type)
  (declare (ignore value type))
  (if (atom label)                      ; not a disassembly line
    (call-next-method)
    (let* ((pc (car label))
           (label-p (and (listp pc) (setq pc (cadr pc))))
           (pc-mark (pc f)))
      (if (eq pc pc-mark)
        (format stream "*~vd" (pc-width f) pc)
        (format stream "~vd" (+ (pc-width f) (if pc-mark 1 0)) pc))
      (write-char (if label-p #\= #\ ) stream))))

(defmethod prin1-value ((f function-inspector) stream value &optional label type)
  (if (atom label)                      ; not a disassembly line
    (unless (eq (if (consp type) (car type) type) :comment)
      (call-next-method))
    (let ((q (cdr label)))
      (write-char #\( stream)
      (loop (if (null q) (return))
        (ccl::disasm-prin1 (pop q) stream)
        (if q (write-char #\space stream)))
      (write-char #\) stream)))
  value)

;; Generic-functions
;; Display the list of methods on a line of its own to make getting at them faster
;; (They're also inside the dispatch-table which is the first immediate in the disassembly).
(defclass gf-inspector (function-inspector)
  ((method-count :accessor method-count)
   (slot-count :accessor slot-count :initform 0)
   (forwarded-p :accessor forwarded-p :initform nil)))

(defmethod inspector-class ((f standard-generic-function))
  (if (functionp f) 
    'gf-inspector
    'standard-object-inspector))

(defmethod compute-line-count ((f gf-inspector))
  (let* ((gf (inspector-object f))
         (count (length (generic-function-methods gf)))
         (res (+ 1 (setf (method-count f) count)  
                 (call-next-method))))
    (if (disasm-p f) (1+ res) res)))

(defmethod line-n ((f gf-inspector) n)
  (let* ((count (method-count f))
         (slot-count (slot-count f))
         (lines (1+ count)))
    (if (<= 3 n (+ lines slot-count 3))
      (let ((methods (generic-function-methods (inspector-object f))))
        (cond ((eql (decf n 3) 0) (values methods "Methods: " :static))
              ((<= n count)
               (values (nth (- n 1) methods) nil :static))
              ((< (decf n (1+ count)) slot-count)
               (standard-object-line-n f n))
              (t
               (values 0 "Disassembly" :comment #'prin1-comment))))
      (call-next-method f (if (< n 3) n (- n lines slot-count 1))))))

(defmethod (setf line-n) (new-value (f gf-inspector) n)
  (let* ((count (method-count f))
         (slot-count (slot-count f))
         (lines (1+ count)))
    (if (<= 3 n (+ lines slot-count 3))
      (let ((en n))
        (cond ((<= (decf en 3) count)
               (setf-line-n-out-of-range f n))
              ((< (decf en (1+ count)) slot-count)
               (standard-object-setf-line-n new-value f en))
              (t (setf-line-n-out-of-range f n))))
      (call-next-method new-value f (if (< n 3) n (- n lines slot-count 1))))))

#|
(defmethod inspector-commands ((f gf-inspector))
  (let* ((function (inspector-object f))
         (method (selected-object (inspector-view f))))
    (if (typep method 'method)
      (nconc
       (call-next-method)
       `(("Remove method"
         ,#'(lambda ()
              (remove-method function method)
              (resample-it)))))
      (call-next-method))))
|#

(defclass method-inspector (standard-object-inspector function-inspector)
  ((standard-object-lines :accessor standard-object-lines)))

(defmethod inspector-class ((object standard-method))
  'method-inspector)

(defmethod compute-line-count ((i method-inspector))
  (+ (setf (standard-object-lines i) (call-next-method))
     (if (disasm-p i) 1 0)              ; "Disassembly"
     (compute-disassembly-lines i (method-function (inspector-object i)))))

(defmethod line-n ((i method-inspector) n)
  (let ((sol (standard-object-lines i)))
    (cond ((< n sol) (call-next-method))
          ((eql n sol) (values nil "Disassembly" :comment))
          (t (disassembly-line-n i (- n sol 1))))))

(defmethod (setf line-n) (new-value (i method-inspector) n)
  (let ((sol (standard-object-lines i)))
    (cond ((< n sol) (call-next-method))
          ((eql n sol) (setf-line-n-out-of-range i n))
          (t (set-disassembly-line-n
              i n new-value (method-function (inspector-object i)))))))

; funtion-inspector never does prin1-comment.
(defmethod prin1-normal-line ((i method-inspector) stream value &optional
                              label type colon-p)
  (declare (ignore colon-p))
  (if (eq type :comment)
    (prin1-comment i stream value label type)
    (call-next-method)))


;;;;;;;
;;
;; Structures
;;
(defmethod inspector-class ((s structure-object))
  'usual-basics-first-inspector)

(defun structure-slots (s)
  (let ((slots (ccl::sd-slots (ccl::struct-def s))))
    (if (symbolp (caar slots))
      slots
      (cdr slots))))

(defmethod compute-line-count ((s structure-object))
  (length (structure-slots s)))

(defmethod line-n ((s structure-object) n)
  (let ((slot (nth n (structure-slots s))))
    (if slot
      (values (uvref s (ccl::ssd-offset slot)) (ccl::ssd-name slot) :colon)
      (line-n-out-of-range s n))))

(defmethod (setf line-n) (new-value (s structure-object) n)
  (let ((slot (nth n (structure-slots s))))
    (if slot
      (setf (uvref s (ccl::ssd-offset slot)) new-value)
      (setf-line-n-out-of-range s n))))

;;;;;;;
;;
;; packages
;;
(defclass package-inspector (uvector-inspector) ())

(defmethod inspector-class ((p package)) 'package-inspector)

(defmethod compute-line-count ((i package-inspector))
  (+ 2 (call-next-method)))

(defmethod line-n ((i package-inspector) n)
  (cond ((eql n 0) (values (ccl::%pkgtab-count (ccl::pkg.itab (inspector-object i)))
                           "Internal Symbols: " :static))
        ((eql n 1) (values (ccl::%pkgtab-count (ccl::pkg.etab (inspector-object i)))
                           "External Symbols: " :static))
        (t (call-next-method i (- n 2)))))

(defmethod (setf line-n) (new-value (i package-inspector) n)
  (if (< n 2)
    (setf-line-n-out-of-range i n)
    (call-next-method new-value i (- n 2))))

(defmethod inspector-commands ((i package-inspector))
  `(("Inspect all packages" ,#'(lambda () (inspect (list-all-packages))))
    (,(format nil "(setq *package* '~a" (inspector-object i))
     ,#'(lambda () (setq *package* (inspector-object i))))))

;;;;;;;
;;
;; Records
;;
(defclass record-inspector (object-first-inspector)
  ((record-type :accessor record-type)
   (field-names :accessor field-names)
   (unlock :initform nil :accessor unlock)))

(defmethod inspector-class ((o macptr))
  'record-inspector)


;;; Still needs work.
(defclass thread-inspector (uvector-inspector) ())

(defmethod inspector-class ((thread ccl::lisp-thread))
  'thread-inspector)

(defmethod compute-line-count :before ((i thread-inspector))
  (when (eq (inspector-object i) ccl::*current-lisp-thread*)
    (ccl::%normalize-areas)))

(defmethod line-n ((thread thread-inspector) n)
  (declare (ignore n))
  (multiple-value-bind (value label type) (call-next-method)
    (values
     (or (and (fixnump value)
              (>= value 0)
              (memq label '(ccl::sg.xframe ccl::sg.cs-area ccl::sg.vs-area
                            ccl::sg.ts-area ccl::sg.cs-overflow-limit))
              (%int-to-ptr (ash value 2)))
         value)
     label
     type)))

(defmethod line-n-inspector ((i thread-inspector) n value label type)
  (declare (ignore n type))
  (or (and value
           (macptrp value)
           (not (%null-ptr-p value))
           (cond ((memq label '(ccl::sg.cs-area ccl::sg.vs-area ccl::sg.ts-area))
                  (make-instance 'record-inspector
                    :object value
                    :record-type :gc-area))
                 ((eq label 'ccl::sg.xframe)
                  (make-instance 'record-inspector
                    :object value
                    :record-type :xframe-list))
                 (t nil)))
      (call-next-method)))



(defmethod line-n-inspector (i n value label type)
  (declare (ignore i n label type))
  (make-inspector value))

(defmethod line-n-inspector ((i usual-inspector) n value label type)
  (let ((object (inspector-object i)))
    (if (typep object 'usual-inspector)
      (make-inspector value)
      (line-n-inspector (inspector-object i) n value label type))))



;;;;;;;
;;
;; an ERROR-FRAME stores the stack addresses that the backtrace window displays
;;

;; set to list of function you don't want to see
;; Functions can be symbols, nil for kernel, or #'functions
(defparameter *backtrace-internal-functions*  
  (list :kernel))

(defvar *backtrace-hide-internal-functions-p* t)

(defclass error-frame ()
  ((addresses :accessor addresses)
   (restart-info :accessor restart-info)
   (sampling-period :initarg :sampling-period :initform 32 :reader sampling-period)
   (stack-start :initarg :stack-start :initform (ccl::%get-frame-ptr) :reader stack-start)
   (stack-end :initarg :stack-end :initform (ccl::last-frame-ptr) :reader stack-end)
   (tcr :initarg :tcr :initform (ccl::%current-tcr) :reader tcr)
   (frame-count :accessor frame-count)
   (ignored-functions :accessor ignored-functions
                      :initform (and *backtrace-hide-internal-functions-p*
                                     *backtrace-internal-functions*))
   (break-condition :accessor break-condition
                    :initarg :break-condition)))
  

; This is set up to access the result of
; (multiple-value-call #'vector (ccl::parent-frame-saved-vars ...))
(ccl::def-accessors svref
  %sv.frame
  %sv.last-catch
  %sv.srv)



(defun ignore-function-in-backtrace? (error-frame function)
  (loop for ignored-fn in (ignored-functions error-frame)
        when (and (null function) (eq ignored-fn :kernel)) return t
        when (and (symbolp ignored-fn) (eq (function-name function) ignored-fn)) return t
        when (eq ignored-fn function) return t
        finally (return nil)))

; use parent-frame-saved-vars to cons a vector for each element of ADDRESSES
(defmethod initialize-instance ((f error-frame) &key)
  (call-next-method)
  (initialize-addresses f))

(defmethod initialize-addresses ((f error-frame))
  (let ((end (stack-end f)))
    (flet ((skip-to-important-frame (frame tcr)
             (loop for this? = (or (eq frame end)
                                   (not (ignore-function-in-backtrace?
                                         f
                                         (ccl::cfp-lfun frame))))
                   until this?
                   do (setf frame (ccl::parent-frame frame tcr))
                   finally (return frame))))
      (setf (slot-value f 'stack-start)
            (skip-to-important-frame (stack-start f) (tcr f)))))
  
      (let* ((count 0)
             (tcr (tcr f))
             (p (stack-start f))
             (p-child (ccl::child-frame p tcr))
             (q (stack-end f))
             (period (sampling-period f))
             (addresses nil)
             (last-frame nil))
        (multiple-value-bind (frame catch srv)
                             (ccl::last-catch-since-saved-vars p-child tcr)
          (loop
            (if (null frame) (error "Can't find saved vars info"))
            (if (eq frame p-child) (return))
            (multiple-value-setq (frame catch srv)
              (ccl::parent-frame-saved-vars tcr frame catch srv srv)))
          (push (vector p-child catch (ccl::copy-srv srv))
                addresses)
          (setq last-frame frame)
          (multiple-value-setq (frame catch srv)
            (ccl::parent-frame-saved-vars tcr frame catch srv srv))
          (unless (eq frame p) (error "(~s (~s ~d)) <> ~d"
                                      'ccl::parent-frame 'ccl::child-frame p p))
          (push (vector frame catch (ccl::copy-srv srv))
                addresses)
          (flet ((done-p ()
                     (or (null frame) (eql last-frame q))))
             (block loop
                (do* ((cnt (1+ period)))
                     ((done-p))
                  (loop while (ignore-function-in-backtrace?
                             f (ccl::cfp-lfun frame))
                      do 
                      (setq last-frame frame)
                      (multiple-value-setq (frame catch srv)
                        (ccl::parent-frame-saved-vars tcr frame catch srv srv))
                      (when (done-p) (return-from loop)))
                  (when (eql 0 (decf cnt))
                    (setq cnt period)
                    (push (vector frame catch (ccl::copy-srv srv))
                          addresses))
                  (setq last-frame frame)
                  (multiple-value-setq (frame catch srv)
                    (ccl::parent-frame-saved-vars tcr frame catch srv srv))
                  (incf count))))
          (setf (frame-count f) count
                (addresses f) (list-to-vector (nreverse addresses))))))

(defun error-frame-n (error-frame n)
  (let* ((addresses (addresses error-frame))
         (period (sampling-period error-frame))
	 (tcr (tcr error-frame))
         p child)
    (flet ((skipping-uninteresting-parent-frames (child)
            (loop while (ignore-function-in-backtrace? 
                         error-frame (ccl::cfp-lfun (ccl::parent-frame child tcr)))
                  do (setq child (ccl::parent-frame child tcr))
                  finally (return child))))
      (unless (< -1 n (frame-count error-frame))
        (setq n (require-type n `(integer 0 ,(1- (frame-count error-frame))))))
      (if (eql 0 n)
        (setq child (%sv.frame (svref addresses 0))
              p (%sv.frame (svref addresses 1)))
        (multiple-value-bind (idx offset) (floor (1- n) period)
          (setq child (skipping-uninteresting-parent-frames 
                       (%sv.frame (svref addresses (1+ idx)))))
          (dotimes (i offset)
            (declare (fixnum i))
            (setq child (skipping-uninteresting-parent-frames 
                         (ccl::parent-frame child tcr))))
          (setq p (ccl::parent-frame child tcr))))
      (values p child))))

(defmethod error-frame-address-n ((f error-frame) n)
  (multiple-value-bind (p child) (error-frame-n f n)
    (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
      (values p lfun pc child))))

; Returns 6 values:
; p lfun pc child last-catch srv
; Where the last-catch & register values are those for the CHILD's frame
; (the ones we need to look at to display values for frame P).
(defmethod error-frame-regs-n ((f error-frame) n)
  (let* ((addresses (addresses f))
         (period (sampling-period f))
         (tcr (tcr f))
         p child last-catch srv)
    (unless (< -1 n (frame-count f))
      (setq n (require-type n `(integer 0 ,(1- (frame-count f))))))
    (if (eql 0 n)
      (let ((child-sv (svref addresses 0)))
        (setq child (%sv.frame child-sv)
              last-catch (%sv.last-catch child-sv)
              srv (ccl::copy-srv (%sv.srv child-sv))))
      (multiple-value-bind (idx offset) (floor (1- n) period)
        (let ((child-sv (svref addresses (1+ idx))))
          (setq child (%sv.frame child-sv)
                last-catch (%sv.last-catch child-sv)
                srv (ccl::copy-srv (%sv.srv child-sv))))
        (flet ((maybe-ignore ()
                 (loop while (ignore-function-in-backtrace? 
                              f
                              (ccl::cfp-lfun (ccl::parent-frame child tcr))) 
                     do (multiple-value-setq (child last-catch srv)
                          (ccl::parent-frame-saved-vars tcr child last-catch srv srv)))))
         (maybe-ignore)
           (dotimes (i offset)
             (declare (fixnum i))
             (multiple-value-setq (child last-catch srv)
               (ccl::parent-frame-saved-vars tcr child last-catch srv srv))
            (maybe-ignore)
            ))))
    (unless child (error "shouldn't happen"))
    (setq p (ccl::parent-frame child tcr))
    (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
      (values p lfun pc child last-catch srv))))
      

(defun print-error-frame-limits (f stream)
  (format stream "#x~x - #x~x" (stack-start f) (stack-end f)))

(defmethod print-object ((f error-frame) stream)
  (print-unreadable-object (f stream :type 'frame-ptr)
    (print-error-frame-limits f stream)))



;;;;;;;
;;
;; The inspector for error-frame objects
;;

; True to show more info about backtrace frames
(defvar *show-backtrace-frame-addresses* nil)

(defclass stack-inspector (inspector)
  ((show-frame-addresses :initform *show-backtrace-frame-addresses*
                         :accessor show-frame-addresses)
   (vsp-range :accessor vsp-range :initarg :vsp-range)
   (tsp-range :accessor tsp-range :initarg :tsp-range)))

(defun make-tsp-stack-range (tcr bt-info)
  (list (cons (ccl::%catch-tsp (ccl::bt.top-catch bt-info))
              (ccl::%fixnum-ref (ccl::%fixnum-ref tcr target::tcr.ts-area)
                                target::area.high))))

(defun make-vsp-stack-range (tcr bt-info)
  (list (cons (ccl::%fixnum-ref
               (ccl::%svref (ccl::bt.top-catch bt-info) target::catch-frame.csp-cell)
               target::lisp-frame.savevsp)
              (ccl::%fixnum-ref (ccl::%fixnum-ref tcr target::tcr.vs-area)
                                target::area.high))))

                           
(defmethod initialize-instance ((i stack-inspector) &rest initargs &key info)
  (declare (dynamic-extent initargs))
  (let* ((tcr (ccl::bt.tcr info))
         (start (ccl::child-frame (ccl::parent-frame (ccl::bt.youngest info) tcr) tcr))
         (end (ccl::child-frame (ccl::parent-frame (ccl::bt.oldest info) tcr) tcr)))
    (apply #'call-next-method i
           :object 
           (make-instance 'error-frame
             :stack-start start
             :stack-end end
             :tcr tcr
             :break-condition (ccl::bt.break-condition info))
           :tsp-range (make-tsp-stack-range tcr info)
           :vsp-range (make-vsp-stack-range tcr info)
           initargs)))

(defmethod print-object ((i stack-inspector) stream)
  (print-unreadable-object (i stream :type 'stack-inspector)
    (print-error-frame-limits (inspector-object i) stream)))

(defmethod addresses ((f stack-inspector))
  (addresses (inspector-object f)))

(defmethod error-frame-address-n ((f stack-inspector) n)
  (error-frame-address-n (inspector-object f) n))

(defmethod error-frame-regs-n ((f stack-inspector) n)
  (error-frame-regs-n (inspector-object f) n))

(defmethod compute-line-count ((f stack-inspector))
  (setf (show-frame-addresses f) *show-backtrace-frame-addresses*)
  (frame-count (inspector-object f)))

(defmethod line-n ((f stack-inspector) n)
  (multiple-value-bind (p lfun) (error-frame-address-n (inspector-object f) n)
    (values lfun 
            (if (show-frame-addresses f) p n)
            (if lfun :static '(:comment (:bold) (:plain :italic))) 'prin1-colon-line)))

(defmethod prin1-label ((i stack-inspector) stream value &optional label type)
  (declare (ignore value type))
  (if (show-frame-addresses i)
    (format stream "#x~x/#x~x"
            (ccl::index->address label) (ccl::index->address (ccl::%frame-savevsp label)))
    (call-next-method)))

(defmethod prin1-value ((i stack-inspector) stream value &optional label type)
  (declare (ignore label type))
  (if value
    (ccl::%lfun-name-string value stream)
    (write-string "kernel" stream)))

(defmethod line-n-inspector ((f stack-inspector) n value label type)
  (declare (ignore value label type))
  (multiple-value-bind (p lfun pc) (error-frame-address-n (inspector-object f) n)
    (declare (ignore p))
    (make-instance (inspector-class lfun) :object lfun :pc pc)))

;; inspecting a single stack frame
;; The inspector-object is expected to be an error-frame
(defclass stack-frame-inspector (inspector)
  ((frame-number :initarg :frame-number :initform nil :reader frame-number)
   (frame-info :accessor frame-info)
   (label-columns :accessor label-columns)
   (saved-register-count :accessor saved-register-count)))


(defmethod initialize-instance ((i stack-frame-inspector) &rest initargs &key
                                object frame-number)
  (declare (dynamic-extent initargs))
  (setq object (require-type object 'error-frame))
  (apply #'call-next-method i 
         :object object
         initargs)
  (setf (frame-number i) frame-number))

(defun integer-digits (integer &optional (base 10))
  (setq integer (require-type integer 'fixnum))
  (do ((digits (if (< integer 0) 2 1) (1+ digits))
       (n (abs integer) (floor n base)))
      ((if (< n base) (return digits)))))    

(defmethod compute-line-count ((i stack-frame-inspector))
  (let ((frame-number (frame-number i)))
    (if (null frame-number)
      0
      (let* ((error-frame (inspector-object i))
             (frame-info (multiple-value-list 
                          (error-frame-regs-n error-frame frame-number))))
        (setf (frame-info i) frame-info)
        (let ((count (ccl::count-values-in-frame
                      (car frame-info)          ; this frame
                      (tcr error-frame)
                      (cadddr frame-info))))    ; child frame
          (setf (label-columns i) (integer-digits count))
          (let ((lfun (cadr frame-info))
                (pc (caddr frame-info)))
            (declare (ignore p))
            (+ count (setf (saved-register-count i)
                           (logcount (or (ccl::registers-used-by lfun pc) 0))))))))))

(defmethod line-n ((i stack-frame-inspector) n)
  (unless (< -1 n (inspector-line-count i))
    (line-n-out-of-range i n))
  (let ((frame-info (frame-info i))
        (saved-register-count (saved-register-count i)))
    (if (< n saved-register-count)
      (multiple-value-bind (mask regs) (apply #'ccl::saved-register-values (cdr frame-info))
        (let* ((srv (nth 5 frame-info))
               (unresolved (ccl::srv.unresolved srv))
               (j ccl::*saved-register-count*)
               (i n))
          (loop
            (loop (if (logbitp (decf j) mask) (return)))
            (if (< (decf i) 0) (return)))
          (let ((name (saved-register-name
                       (elt ccl::*saved-register-numbers* (- ccl::*saved-register-count* 1 j)) (cadr frame-info) (caddr frame-info))))
            (values (if (setq unresolved (logbitp j unresolved))
                      *unbound-marker*
                      (ccl::srv.register-n regs (- ccl::*saved-register-count* 1 j)))
                    (cons n
                          (cons (elt ccl::*saved-register-names* j) name))
                    (if unresolved :static :normal)))))
      (destructuring-bind (p lfun pc child &rest rest) frame-info
        (declare (ignore rest))
        (let ((offset (- n saved-register-count)))
          (multiple-value-bind (var type name)
                               (ccl::nth-value-in-frame p offset (tcr (inspector-object i)) lfun pc child)
            (values var (cons n (cons type name)) :normal)))))))

(defmethod (setf line-n) (value (i stack-frame-inspector) n)
  (unless (< -1 n (inspector-line-count i))
    (line-n-out-of-range i n))
  (let ((frame-info (frame-info i))
        (saved-register-count (saved-register-count i)))
    (if (< n saved-register-count)
      (let* ((mask (apply #'ccl::saved-register-values (cdr frame-info)))
             (srv (nth 5 frame-info))
             (unresolved (ccl::srv.unresolved srv))
             (j ccl::*saved-register-count*)
             (i n))
        (loop
          (loop (if (logbitp (decf j) mask) (return)))
          (if (< (decf i) 0) (return)))
        (if (logbitp j unresolved) (line-n-out-of-range i n))
        (apply #'ccl::set-saved-register value (- ccl::*saved-register-count* 1 j) (cdr frame-info)))
      (destructuring-bind (p lfun pc child &rest rest) frame-info
        (declare (ignore lfun pc rest))
        (let ((offset (- n saved-register-count))
              (tcr (tcr (inspector-object i))))
          (ccl::set-nth-value-in-frame p offset tcr value child))))))

(defun saved-register-name (reg lfun pc)
  (let* ((map (ccl::function-symbol-map lfun))
         (names (car map))
         (info (cdr map))
         (j 0))
    (dotimes (i (length names))
      (when (and (eq reg (aref info j))
                 (<= (aref info (1+ j)) pc (aref info (+ j 2))))
        (return (aref names i)))
      (incf j 3))))
        

(defmethod prin1-label ((i stack-frame-inspector) stream value &optional label type)
  (declare (ignore value type))
  (format stream "~vd: " (label-columns i) (car label)))

(defmethod prin1-value ((i stack-frame-inspector) stream value &optional label type)
  (declare (ignore type))
  (destructuring-bind (n type . name) label
    (declare (ignore n))
    (if name (format stream "~s " name))
    (if type (format stream "(~a) " type))
    (if (eq value *unbound-marker*)
      (format stream "??")
      (prin1 value stream))))

(defmethod (setf frame-number) (frame-number (i stack-frame-inspector))
  (let ((max (1- (frame-count (inspector-object i)))))
    (unless (or (null frame-number)
                (and (<= 0 frame-number max)))
      (setq frame-number (require-type frame-number `(or null (integer 0 ,max))))))
  (unless (eql frame-number (frame-number i))
    (setf (slot-value i 'frame-number) frame-number)
    (setf (inspector-line-count i) nil)
    frame-number))



;;; Inspector


(defvar *inspector-ui* ())

(defclass inspector-ui ()
    ((inspector :initarg :inspector :accessor inspector-ui-inspector)
     (level :initarg :level :accessor inspector-ui-level)))

(defclass inspector-tty-ui (inspector-ui)
    ((origin :initarg :origin :initform 0 :accessor inspector-tty-ui-origin)
     (pagesize :initarg :pagesize :initform 20 :accessor
	       inspector-tty-ui-pagesize)))

(defmethod ui-initialize ((ui inspector-tty-ui)))

(defmethod ui-present ((ui inspector-tty-ui))
  (let* ((inspector (inspector-ui-inspector ui)))
    (when (null (inspector-line-count inspector))
      (update-line-count inspector))
    (with-errorfree-printing
	(let* ((stream *debug-io*)
	       (origin (inspector-tty-ui-origin ui))
	       (pagesize (inspector-tty-ui-pagesize ui))
	       (page-end (+ origin pagesize))
	       (n (compute-line-count inspector))
	       (end (min page-end n))
	       (tag origin)
	       (*print-pretty* (or *print-pretty* *describe-pretty*))
	       (*print-length* 5)
	       (*print-level* 5)
	       (func #'(lambda (i value &rest rest)
			 (declare (dynamic-extent rest))
			 (let* ((type (cadr rest)))
			   (unless (or (eq type :comment)
				   (and (consp type)
					(eq (car type) :comment)))
			     (format stream "[~d] " tag))
			   (incf tag))
			 (format stream "~8t")
			 (apply #'prin1-line i stream value rest)
			 (terpri stream))))
	  (declare (dynamic-extent func))
	  (map-lines inspector func origin end)))
    (values)))

(ccl::define-toplevel-command
    :tty-inspect i (n)
    "inspect <n>th item"
    (inspector-ui-inspect-nth *inspector-ui* n))

(ccl::define-toplevel-command
    :tty-inspect pop ()
    "exit current inspector"
    (invoke-restart 'exit-inspector))

(ccl::define-toplevel-command
    :tty-inspect show ()
    "re-show currently inspected object"
    (ui-present *inspector-ui*))

(defmethod inspector-ui-next-page ((ui inspector-tty-ui))
  (let* ((nlines (compute-line-count (inspector-ui-inspector ui)))
	 (origin (inspector-tty-ui-origin ui))
	 (page-size (inspector-tty-ui-pagesize ui))
	 (new-origin (+ origin page-size)))
    (if (< new-origin nlines)
      (setf (inspector-tty-ui-origin ui) new-origin))
    (ui-present ui)))
    
(ccl::define-toplevel-command
    :tty-inspect next ()
    "show next page of object data"
    (inspector-ui-next-page *inspector-ui*))

(defmethod inspector-ui-prev-page ((ui inspector-tty-ui))
  (let* ((origin (inspector-tty-ui-origin ui))
	 (page-size (inspector-tty-ui-pagesize ui))
	 (new-origin (max 0 (- origin page-size))))
    (setf (inspector-tty-ui-origin ui) new-origin)
    (ui-present ui)))

(ccl::define-toplevel-command
    :tty-inspect prev ()
    "show previous page of object data"
    (inspector-ui-prev-page *inspector-ui*))

(ccl::define-toplevel-command
    :tty-inspect home ()
    "show first page of object data"
    (progn
      (setf (inspector-tty-ui-origin *inspector-ui*) 0)
      (ui-present *inspector-ui*)))

(ccl::define-toplevel-command
    :tty-inspect s (n v)
    "set the <n>th line of object data to value <v>"
    (let* ((ui *inspector-ui*))
      (setf (line-n (inspector-ui-inspector ui) n) v)
      (ui-present ui)))


(defmethod ui-interact ((ui inspector-tty-ui))
  (let* ((level (inspector-ui-level ui)))
    (restart-case
     (ccl:with-terminal-input
	 (ccl::with-toplevel-commands :tty-inspect
	   (ccl::read-loop
	    :prompt-function #'(lambda ()
				 (if (eql level 0)
				   (format t "~&Inspect> ")
				   (format t "~&Inspect ~d> " level))))))
     (exit-inspector () (terpri *debug-io*)))))

(defmethod inspector-ui-inspect-nth ((ui inspector-tty-ui) n)
  (let* ((inspector (inspector-ui-inspector ui)))
    (multiple-value-bind (value label type)
	(line-n inspector n)
      (unless (or (eq type :comment)
		  (and (consp type) (eq (car type) :comment)))
	(let* ((new-inspector (line-n-inspector inspector n value label type))
	       (ccl::@ value))
	  (inspector-ui-inspect
	   (make-instance 'inspector-tty-ui
			  :level (1+ (inspector-ui-level ui))
			  :inspector new-inspector)))))))
      
(defparameter *default-inspector-ui-class-name* 'inspector-tty-ui)

(defmethod inspector-ui-inspect ((ui inspector-ui))
  (let* ((*inspector-ui* ui))
    (ui-initialize ui)
    (ui-present ui)
    (ui-interact ui)
    (values)))

(defun inspect (thing)
  (let* ((ccl::@ thing))
    (inspector-ui-inspect (make-instance *default-inspector-ui-class-name*
					 :inspector (make-inspector thing)
					 :level 0))))
