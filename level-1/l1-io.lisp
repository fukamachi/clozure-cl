; -*- Mode: LISP; Package: CCL -*-
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

;; L1-io.lisp

(defun %new-ptr (size &optional clear-p)
  (let* ((p (malloc size)))
    (if (and clear-p (not (%null-ptr-p p)))
      (#_bzero p size))
    p))


;;;; ======================================================================
;;;; Standard CL IO frobs


;;; OK, EOFP isn't CL ...
(defun eofp (&optional (stream *standard-input*))
  (stream-eofp stream))

(defun force-output (&optional stream)
  (stream-force-output (real-print-stream stream))
  nil)

(defun listen (&optional (stream *standard-input*))
  (let* ((stream (designated-input-stream stream)))
    (stream-listen stream)))

(defun fresh-line (&optional (stream *standard-output*))
  (stream-fresh-line (real-print-stream stream)))


(defun clear-input (&optional stream)
  (stream-clear-input (designated-input-stream stream))
  nil)

(defun write-char (char &optional (output-stream nil))
  (stream-write-char (real-print-stream output-stream) char)
  char)

(defun write-string (string &optional output-stream &key (start 0 start-p)
			    (end nil end-p))
  (if (and (not start-p) (not end-p))
    (stream-write-string (real-print-stream output-stream) string)
    (stream-write-string (real-print-stream output-stream) string start end))
  string)

(defun write-line (string &optional output-stream
                          &key (start 0) (end (length string)))
  (let ((stream (real-print-stream output-stream)))
    (write-string string stream :start start :end end)
    (terpri stream)
    string))

(defun terpri (&optional (stream *standard-output*))
  (stream-write-char  (real-print-stream stream) #\newline)
  nil)

;;;; ----------------------------------------------------------------------



;;;; ======================================================================
;;;; The Lisp Printer


;; coral extensions
(defvar *print-abbreviate-quote* t
  "Non-NIL means that the normal lisp printer --
not just the pretty-printer -- should print
lists whose first element is QUOTE or FUNCTION specially.
This variable is not part of standard Common Lisp.")

(defvar *print-structure* t
  "Non-NIL means that lisp structures should be printed using
\"#S(...)\" syntax.  if nil, structures are printed using \"#<...>\".
This variable is not part of standard Common Lisp.")

;; things Richard Mlynarik likes.
(defvar *print-simple-vector* nil
  "Non-NIL means that simple-vectors whose length is less than
the value of this variable are printed even if *PRINT-ARRAY* is false.
this variable is not part of standard Common Lisp.")

(defvar *print-simple-bit-vector* nil
  "Non-NIL means that simple-bit-vectors whose length is less than
the value of this variable are printed even if *PRINT-ARRAY* is false.
This variable is not part of standard Common Lisp.")

(defvar *print-string-length* nil
  "Non-NIL means that strings longer than this are printed
using abbreviated #<string ...> syntax.
This variable is not part of standard Common Lisp.")

(defvar *print-escape* t
  "Non-NIL means that the lisp printer should -attempt- to output
expressions `readably.'  When NIL the attempts to produce output
which is a little more human-readable (for example, pathnames
are represented by the characters of their namestring.)")

(defvar *print-pretty* nil
  "Non-NIL means that the lisp printer should insert extra
indentation and newlines to make output more readable and `prettier.'")

(defvar *print-base* 10.
  "The output base for integers and rationals.
Must be an integer between 2 and 36.")

(defvar *print-radix* nil
  "Non-NIL means that the lisp printer will explicitly indicate
the output radix (see *PRINT-BASE*) which is used to print
integers and rational numbers.")

(defvar *print-level* nil
  "Specifies the depth at which printing of lisp expressions
should be truncated.  NIL means that no such truncation should occur.
Truncation is indicated by printing \"#\" instead of the
representation of the too-deeply-nested structure.
See also *PRINT-LENGTH*")

(defvar *print-length* nil
  "Specifies the length at which printing of lisp expressions
should be truncated.  NIL means that no such truncation should occur.
truncation is indicated by printing \"...\" instead of the
rest of the overly-long list or vector.
See also *PRINT-LEVEL*")

(defvar *print-circle* nil
  "Non-NIL means that the lisp printer should attempt to detect
circular structures, indicating them by using \"#n=\" and \"#n#\" syntax.
If this variable is false then an attempt to
output circular structure may cause unbounded output.")

(defvar *print-case* ':upcase
  "Specifies the alphabetic case in which symbols should
be printed.  Possible values include :UPCASE, :DOWNCASE and :CAPITALIZE") ; and :StuDLy

(defvar *print-array* t
  "Non-NIL means that arrays should be printed using \"#(...)\" or
\"=#nA(...)\" syntax to show their contents.
If NIL, arrays other than strings are printed using \"#<...>\".
See also the (non-Common Lisp) variables *PRINT-SIMPLE-VECTOR*
and *PRINT-SIMPLE-BIT-VECTOR*")

(defvar *print-gensym* t
  "Non-NIL means that symbols with no home package should be
printed using \"#:\" syntax.  NIL means no prefix is printed.")

(defvar *print-readably* nil
  "Non-NIL means that attempts to print unreadable objects
   signal PRINT-NOT-READABLE errors.  NIL doesn't.")

(defvar *PRINT-RIGHT-MARGIN* nil
  "+#/NIL the right margin for pretty printing")

(defvar *PRINT-MISER-WIDTH* 40.
  "+#/NIL miser format starts when there is less than this width left")

(defvar *PRINT-LINES* nil
  "+#/NIL truncates printing after # lines")

(defvar *DEFAULT-RIGHT-MARGIN* 70
  "Controls default line length;  Must be a non-negative integer")

(defvar *PRINT-PPRINT-DISPATCH* nil) ; We have to support this.

(defvar *xp-current-object* nil)  ; from xp

(defvar *circularity-hash-table* nil) ; ditto

(defvar *current-level* nil)

(defvar *current-length* nil) ; must be nil at top level


;;;; ======================================================================

(defclass xp-stream (output-stream)
   (xp-structure))

(defun %write-string (string stream)
  (if (characterp string)
    (stream-write-char stream string)
    (stream-write-entire-string stream string)))


;; *print-simple-vector*
;; *print-simple-bit-vector*
;; *print-string-length*
;; for things like *print-level* which must [no longer] be integers > 0
(defun get-*print-frob* (symbol
                         &optional (nil-means most-positive-fixnum)
                         (t-means nil))
  (declare (type symbol symbol))
  (let ((value (symbol-value symbol)))
    (when *print-readably*
      (case symbol
        ((*print-length* *print-level* *print-lines* *print-string-length*)
         (setq value nil))
        ((*print-escape* *print-gensym* *print-array* *print-simple-vector*
                         *print-simple-bit-vector*)
         (setq value t))
        (t nil)))
    (cond ((null value)
           nil-means)
          ((and (integerp value)) ; (> value 0))
           (min (max value -1) value most-positive-fixnum))
          ((and t-means (eq value 't))
           t-means)
          (t
           (setf (symbol-value symbol) nil)
           (error "~s had illegal value ~s.  reset to ~s"
                  symbol value 'nil)))))


(defun pp-newline (stream kind)
  (case kind
    ((:newline)
     (fresh-line stream))
    ((:unconditional :mandatory)
     (stream-write-char stream #\Newline))
    (t nil)))


(defun pp-space (stream &optional (newline-kind ':fill))
  (stream-write-char stream #\space)
  (pp-newline stream newline-kind))

(defun pp-start-block (stream &optional prefix)
  (cond ((null prefix))
        ((characterp prefix)
         (stream-write-char stream prefix))
        ((stringp prefix)
         (%write-string prefix stream))
        (t (report-bad-arg prefix '(or character string (eql nil))))))


(defun pp-end-block (stream &optional suffix)
  (cond ((null suffix))
        ((characterp suffix)
         (stream-write-char stream suffix))
        ((stringp suffix)
         (%write-string suffix stream))
        (t (report-bad-arg suffix '(or character string (eql nil))))))


#|
(defmethod pp-set-indentation ((stream stream) kind n)
  (declare (ignore kind n))
  nil)
|#


;;;; ======================================================================
;; list-kludge is so that we can simultaneously detect shared list tails
;;   and avoid printing lists as (foo . (bar . (baz . nil)))
;; if non-nil, it is the remaining *print-length* and object is
;;   a list tail



(defmethod write-internal-1 ((stream t) object level list-kludge)
  (declare (type fixnum level) (type (or null fixnum) list-kludge))
  ;;>> Anybody passing in list-kludge had better be internal to the lisp printer.
  ;(if list-kludge (error "Internal printer error"))
    (let ((circle *print-circle*)
          (pretty *print-pretty*))
      (cond ((or pretty circle)
             ; what about this level stuff??
             ; most peculiar
             (maybe-initiate-xp-printing
              #'(lambda (s o) (write+ o s)) stream object))
            ((not list-kludge)
             (write-a-frob object stream level list-kludge))
            ((null object))
            (t
             (stream-write-char stream #\space)
             (when (not (consp object))
               (stream-write-char stream #\.)
               (stream-write-char stream #\space))
             (write-a-frob object stream level list-kludge)))))



(defmethod write-internal-1 ((stream xp-stream) object level list-kludge)
  (when level
    (setq *current-level* (if (and *print-level* (not *print-readably*))
                            (- *print-level* level)
                            0)))
  (write+ object (slot-value stream 'xp-structure) list-kludge))


(defvar *inside-printer-error* nil)

(defvar *signal-printing-errors* nil)
(queue-fixup (setq *signal-printing-errors* t))

(defun write-internal (stream object level list-kludge)
  (if (bogus-thing-p object)
    (print-unreadable-object
      (object stream)
      (princ (%str-cat "BOGUS object @ #x" (%integer-to-string (%address-of object) 16.)) 
             stream))
    (progn
      (flet ((handler (condition)
               (declare (ignore condition))
               (unless *signal-printing-errors*
                 (return-from write-internal
                   (let ((*print-pretty* nil)
                         (*print-circle* nil))
                     (if *inside-printer-error*
                       (when (eql 1 (incf *inside-printer-error*))
                         (%write-string "#<Recursive printing error " stream)
			 (stream-write-char stream #\space)
                         (%write-address (%address-of object) stream)
                         (stream-write-char stream #\>))
                       (let ((*inside-printer-error* 0))
                         ; using format here considered harmful.
                         (%write-string "#<error printing " stream)
                         (write-internal stream (type-of object) (max level 2) nil)
                         (stream-write-char stream #\space)
                         (%write-address (%address-of object) stream)
                         (stream-write-char stream #\>))))))))
        (declare (dynamic-extent #'handler))
        (handler-bind
          ((error #'handler))
          (write-internal-1 stream object level list-kludge)))
      object)))


;;;; ======================================================================
;;;; internals of write-internal

;; bd common-lisp (and lisp machine) printer depth counts
;;  count from 0 upto *print-level* instead of from
;;  *print-level* down to 0 (which this printer sensibly does.)
(defun backtranslate-level (level)
  (let ((print-level (get-*print-frob* '*print-level*)))
    (if (not (and level print-level))
      most-positive-fixnum
      (if (> level print-level)
        ;; wtf!
        1
        (- print-level level)))))

; so we can print-circle for print-object methods.
(defvar %current-write-level% nil)
(defvar %current-write-stream% nil)
(defun %current-write-level% (stream &optional decrement?)
  (if (eq stream %current-write-stream%)
    (if decrement? (1- %current-write-level%) %current-write-level%)
    (get-*print-frob* '*print-level*)))
      
;;>> Some notes:
;;>> CL defining print-object to be a multmethod dispatching on
;;>>  both the object and the stream just can't work
;;>> There are a couple of reasons:
;;>>  - CL wants *print-circle* structure to be automatically detected
;;>>    This means that there must be a printing pre-pass to some stream
;;>>    other than the one specified by the user, which means that any
;;>>    print-object method which specialises on its second argument is
;;>>    going to lose big.

;;>>  - CL wants *print-level* truncation to happen automatically
;;>>    and doesn't pass a level argument to print-object (as it should)
;;>>    This means that the current level must be associated with the
;;>>    stream in some fashion.  The quicky kludge Bill uses here
;;>>    (binding a special variable) loses for
;;>>    + Entering a break loop whilst printing to a stream
;;>>      (Should start level from (get-*print-level*) again)
;;>>    + Performing output to more than one stream in an interleaved fashion
;;>>      (Say a print-object method which writes to *trace-output*)
;;>>    The solution, again, is to actually call the print-object methods
;;>>    on a write-aux-stream, where that stream is responsible for
;;>>    doing *print-level* truncation.
;;>>  - BTW The select-method-order should be (stream object) to even have
;;>>    a chance of winning.  Not that it could win in any case, for the above reasons.
;;>> It isn't that much work to change the printer to always use an
;;>> automatically-level-truncating write-aux-stream
;;>> It is a pity that CL is so BD.
;;>>

(defun write-a-frob (object stream level list-kludge)
  (declare (type stream stream) (type fixnum level)
           (type (or null fixnum) list-kludge))
  (cond ((not list-kludge)
         (let ((%current-write-stream% stream)   ;>> SIGH
               (%current-write-level% level))
           (print-object object stream)))
        ((%i< list-kludge 1)
         ;; *print-length* truncation
         (stream-write-entire-string stream "..."))
        ((not (consp object))
         (write-a-frob object stream level nil))
        (t
         (write-internal stream (%car object) level nil)
         ;;>> must do a tail-call!!
         (write-internal-1 stream (%cdr object) level (%i- list-kludge 1)))))

(defmethod print-object ((object t) stream)
  (let ((level (%current-write-level% stream))   ; what an abortion.  This should be an ARGUMENT!
        (%type (%type-of object)))
    (declare (type symbol %type)
             (type fixnum level))
    (flet ((depth (stream v)
             (declare (type fixnum v) (type stream stream))
             (when (%i<= v 0)
               ;; *print-level* truncation
               (stream-write-entire-string stream "#")
               t)))
      (cond
        ((eq %type 'cons)
         (unless (depth stream level)
           (write-a-cons object stream level)))
        ((or (eq %type 'symbol) (null object))
         ;; Don't do *print-level* truncation;
         ;;  even though strictly necessary it's pretty pointless for symbols
         (write-a-symbol object stream))
        ((depth stream level))
        ((eq %type 'package)
         (write-a-package object stream))
        ((eq %type 'macptr)
         (write-a-macptr object stream))
        ((eq %type 'dead-macptr)
         (write-a-dead-macptr object stream))
        ((eq %type 'internal-structure)
         (write-an-istruct object stream level))        
        ((and (eq %type 'structure)
              (not (null (ccl::struct-def object))))  ; ??
         ;; else fall through to write-a-uvector
         (if (and *print-pretty* *print-structure*)
           (let ((*current-level* (if (and *print-level* (not *print-readably*))
                                    (- *print-level* level)
                                    0)))
             (pretty-structure stream object)) 
           (write-a-structure object stream level)))
        ((functionp object)
         (write-a-function object stream level))
        ((arrayp object)
         (cond ((or (not (stringp object))
                    (%i> (length (the string object))
                         (get-*print-frob* '*print-string-length*)))
                (write-an-array object stream level))
               ((or *print-escape* *print-readably*)
                (write-escaped-string object stream))
               (t
                (%write-string object stream))))

 ; whazzat        
        ((uvectorp object)  ; in ppc land this is catch all for misc - pretty much same as 68k here.
         ; does e.g. population, pool, hash-table-vector
         (write-a-uvector object stream level))
        (t
         (print-unreadable-object (object stream)
           (let* ((address (%address-of object))
                  (low-bits (logand #xff address)))
             (cond ((eq object (%unbound-marker-8))
                    (%write-string "Unbound" stream))
                   ((eq object (%slot-unbound-marker))
                    (%write-string "Slot-Unbound" stream))
                   (t
                    (cond
                     ((eq ppc32::subtag-block-tag low-bits)
                      (%write-string "BLOCK-TAG " stream))
                     ((eq  ppc32::subtag-go-tag low-bits)
                      (%write-string "GO TAG " stream))
                     (t
                      (%write-string "Unprintable " stream)
                      (write-a-symbol %type stream)
                      (%write-string " : " stream)))
                    (%write-address address stream))))))))
    nil))

(defun write-a-dead-macptr (macptr stream)
  (print-unreadable-object (macptr stream)
    (%write-string "A Dead Mac Pointer" stream)))


;;;; ======================================================================
;;;; Powerful, wonderful tools for printing unreadable objects.

(defun print-not-readable-error (object stream)
  (error (make-condition 'print-not-readable :object object :stream stream)))

; Start writing an unreadable OBJECT on STREAM, error out if *PRINT-READABLY* is true.
(defun write-unreadable-start (object stream)
  (if *print-readably* 
    (print-not-readable-error object stream)
    (pp-start-block stream "#<")))

(defun %print-unreadable-object (object stream type id thunk)
  (write-unreadable-start object stream)
  (when type
    (princ (type-of object) stream))
  (when thunk 
    (when type (stream-write-char stream #\space))
    (funcall thunk))
  (if id
    (%write-address object stream #\>)
    (pp-end-block stream ">")))

;;;; ======================================================================
;;;; internals of internals of write-internal

(defmethod print-object ((char character) stream &aux name)
  (cond ((or *print-escape* *print-readably*)   ;print #\ for read-ability
         (stream-write-char stream #\#)
         (stream-write-char stream #\\)
         (if (setq name (char-name char))
             (%write-string name stream)
             (stream-write-char stream char)))
        (t
         (stream-write-char stream char))))

(defun get-*print-base* ()
  (let ((base *print-base*))
    (unless (and (fixnump base)
                 (%i< 1 base) (%i< base 37.))
      (setq *print-base* 10.)
      (error "~S had illegal value ~S.  Reset to ~S"
             '*print-base* base 10))
    base))

(defun write-radix (base stream)
  (stream-write-char stream #\#)
  (case base
    (2 (stream-write-char stream #\b))
    (8 (stream-write-char stream #\o))
    (16 (stream-write-char stream #\x))
    (t (%pr-integer base 10. stream)
       (stream-write-char stream #\r))))

(defun write-an-integer (num stream
                         &optional (base (get-*print-base*))
                                   (print-radix *print-radix*))
  (when (and print-radix (not (eq base 10)))
    (write-radix base stream))
  (%pr-integer num base stream)
  (when (and print-radix (eq base 10))
    (stream-write-char stream #\.)))

(defmethod print-object ((num integer) stream)
  (write-an-integer num stream))

(defun %write-address (object stream &optional foo)
  (if foo (pp-space stream))
  (write-an-integer (if (integerp object) object (%address-of object)) stream 16. t)
  (if foo (pp-end-block stream foo)))

(defmethod print-object ((num ratio) stream)
  (let ((base (get-*print-base*)))
    ;;>> What to do when for *print-radix* and *print-base* = 10?
    (when (and *print-radix* (not (eq base 10)))
      (write-radix base stream))
    (%pr-integer (numerator num) base stream)
    (stream-write-char stream #\/)
    (%pr-integer (denominator num) base stream)))

;;>> Doesn't do *print-level* truncation
(defmethod print-object ((c complex) stream)
  (pp-start-block stream "#c(")
  (print-object (realpart c) stream)
  (pp-space stream)
  (print-object (imagpart c) stream)
  (pp-end-block stream #\)))

(defmethod print-object ((float float) stream)
  (print-a-float float stream))

(defun float-exponent-char (float)
  (if (case *read-default-float-format*
        (single-float (typep float 'single-float))
        (double-float (typep float 'double-float))
        (t (typep float *read-default-float-format*)))
    #\E  
    (if (typep float 'double-float)
      #\D
      #\S)))

(defun default-float-p (float)
  (case *read-default-float-format*
        (single-float (typep float 'single-float))
        (double-float (typep float 'double-float))
        (t (typep float *read-default-float-format*))))

; maybe should be more specific
#+ppc-target 
(defun print-a-nan (float stream)
  (stream-write-entire-string stream (if (infinity-p float)
                                       "#<INFINITY "
                                       "#<NaN "))
  ; print it with invalid-operation disabled
  (if (typep float 'single-float)
    (stream-write-char stream #\>)
    (let ((flags (%get-fpscr-control)))
      (unwind-protect
	   (progn (%set-fpscr-control (logand (lognot (ash 1 (- 31 ppc::fpscr-ve-bit))) flags))
		  (print-a-float float stream nil t)
		  (stream-write-char stream #\>))
	(%set-fpscr-status 0) ; why do we need this? status bits get set in any case? 
	(%set-fpscr-control flags)))))
             
;; nanning => recursive from print-a-nan - don't check again
(defun print-a-float (float stream &optional exp-p nanning)
  (let ((strlen 0) (exponent-char (float-exponent-char float)))
    (declare (fixnum exp strlen))
    (Setq stream (real-print-stream stream))
    (if (and (not nanning)(nan-or-infinity-p float))
      (print-a-nan float stream)    
      (multiple-value-bind (string before-pt #|after-pt|#)
                           (flonum-to-string float)
        (declare (fixnum before-pt after-pt))
        (setq strlen (length string))
        (when (minusp (float-sign float))
          (stream-write-char stream #\-))
        (cond
         ((and (not exp-p) (zerop strlen))
          (stream-write-entire-string stream "0.0"))
         ((and (> before-pt 0)(<= before-pt 7)(not exp-p))
          (cond ((> strlen before-pt)
                 (write-string string stream :start  0 :end before-pt)
                 (stream-write-char stream #\.)
                 (write-string string stream :start  before-pt :end strlen))
                (t ; 0's after
                 (stream-write-entire-string stream string)
                 (dotimes (i (-  before-pt strlen))
                   (stream-write-char stream #\0))
                 (stream-write-entire-string stream ".0"))))
         ((and (> before-pt -3)(<= before-pt 0)(not exp-p))
          (stream-write-entire-string stream "0.")
          (dotimes (i (- before-pt))
            (stream-write-char stream #\0))
          (stream-write-entire-string stream string))
         (t
          (setq exp-p t)
          (stream-write-char stream (if (> strlen 0)(char string 0) #\0))
          (stream-write-char stream #\.)
          (if (> strlen 1)
            (write-string string stream :start  1 :end strlen)
            (stream-write-char stream #\0))
          (stream-write-char stream exponent-char)
          (when (and exp-p (not (minusp (1- before-pt))))
            (stream-write-char stream #\+))
          (let ((*print-base* 10)
                (*print-radix* nil))
            (princ (1- before-pt) stream))))
        (when (and (not exp-p)
                   (not (default-float-p float)))
          (stream-write-char stream exponent-char)
          (stream-write-char stream #\0))))))

;;>> Doesn't do *print-level* truncation
(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream)
    (print-object (class-name (class-of class)) stream)
    (pp-space stream)
    (print-object (class-name class) stream)))


(defmethod print-object ((value-cell value-cell) stream)
  (print-unreadable-object (value-cell stream :type t :identity t)
    (prin1 (uvref value-cell ppc32::value-cell.value-cell) stream)))

;(defun symbol-begins-with-vowel-p (sym)
;  (and (symbolp sym)
;       (not (%izerop (%str-length (setq sym (symbol-name sym)))))
;       (%str-member (schar sym 0) "AEIOU")))

;;;; ----------------------------------------------------------------------
;;;; CLOSsage

;;>> Doesn't do *print-level* truncation
(defmethod print-object ((instance standard-object) stream)
  (print-unreadable-object (instance stream :identity t)
    (let* ((class (class-of instance))
           (class-name (class-name class)))
      (cond ((not (and (symbolp class-name)
                       (eq class (find-class class-name))))
             (%write-string "An instance of" stream)
             (pp-space stream)
             (print-object class stream))
            (t
             (write-a-symbol class-name stream))))))

(defmethod print-object ((method standard-method) stream)
  (print-method method stream (%class.name (class-of method))))

(defmethod print-object ((method-function method-function) stream)
  (let ((method (%method-function-method method-function)))
    (if (typep method 'standard-method)
      (print-method (%method-function-method method-function)
                    stream
                    (%class.name (class-of method-function)))
      (call-next-method))))

(defmethod print-object ((method-function interpreted-method-function) stream)
  (let ((method (%method-function-method method-function)))
    (if (typep method 'standard-method)
      (print-method (%method-function-method method-function)
                    stream
                    (%class.name (class-of method-function)))
      (call-next-method))))

(defun print-method (method stream type-string)
  (print-unreadable-object (method stream)
    (let ((name (%method-name method))
          (qualifiers (%method-qualifiers method))
          (specializers (mapcar #'(lambda (specializer)
                                    (if (typep specializer 'eql-specializer)
				      (list 'eql
					    (eql-specializer-object specializer))
				      (or (class-name specializer)
					  specializer)))
                                (%method-specializers method)))
          (level-1 (%i- %current-write-level% 1)))
      (cond
       ((< level-1 0)
        ;; *print-level* truncation
        (stream-write-entire-string stream "#"))
       (t 
        (prin1 type-string stream)
        (pp-space stream)
        (write-internal stream name level-1 nil)
        (pp-space stream)
        (when qualifiers
          (write-internal stream (if (cdr qualifiers) qualifiers (car qualifiers))
                          level-1 nil)
          (pp-space stream))
        (write-internal stream specializers level-1 nil))))))

;; Need this stub or we'll get the standard-object method
(defmethod print-object ((gf standard-generic-function) stream)
  (write-a-function gf stream (%current-write-level% stream)))

;; This shouldn't ever happen, but if it does, don't want the standard-object method
(defmethod print-object ((mo metaobject) stream)
  (print-unreadable-object (mo stream :type t :identity t)))

(defmethod print-object ((cm combined-method) stream)
  (print-unreadable-object (cm stream :identity t)
    (%write-string "Combined-Method" stream)
    (pp-space stream)
    (let ((name (function-name cm)))
      (if (and (functionp name) (function-is-current-definition? name))
        (setq name (function-name name)))
      (write-internal stream name (%current-write-level% stream) nil))))

(defun print-specializer-names (specializers stream)
  (flet ((print-specializer (spec stream)
           (write-1 (if (typep spec 'class) (%class.name spec) spec) stream)))
    (pp-start-block stream #\()
    (if (atom specializers)
        (print-specializer specializers stream)
      (progn (print-specializer (car specializers) stream)
             (dolist (spec (cdr specializers))
               (pp-space stream)
               (print-specializer spec stream))))
    (pp-end-block stream #\))))


;;;; ----------------------------------------------------------------------
            
(defun write-a-cons (cons stream level)
  (declare (type cons cons) (type stream stream) (type fixnum level))
  (let ((print-length (get-*print-frob* '*print-length*))
        (level-1 (%i- level 1))
        (head (%car cons))
        (tail (%cdr cons)))
    (declare (type fixnum print-length) (type fixnum level-1))
    (unless (and *print-abbreviate-quote*
                 (write-abbreviate-quote head tail stream level-1))
        (progn
          (pp-start-block stream #\()
          (write-internal stream head level-1 nil)
          (write-internal-1 stream tail level-1 (%i- print-length 1))
          (pp-end-block stream #\))))))

;;;; hack for quote and backquote

;; for debugging
;(setq *backquote-expand* nil)

(defvar *backquote-hack* (list '*backquote-hack*)) ;uid
(defun write-abbreviate-quote (head tail stream level-1)
  (declare (type stream stream) (type fixnum level-1))
  (when (symbolp head)
    (cond ((or (eq head 'quote) (eq head 'function))
           (when (and (consp tail)
                      (null (%cdr tail)))
             (%write-string (if (eq head 'function) "#'" "'") stream)
             (write-internal stream (%car tail) level-1 nil)
             t))
          ((eq head 'backquote-expander)
           (when (and (consp tail)
		      (consp (cdr tail))
		      (consp (cddr tail))
		      (consp (cdddr tail))
		      (null (cddddr tail)))
             (let ((tail tail))
               (set (%car tail)
                    *backquote-hack*)  ;,
               (set (%car (setq tail (%cdr tail)))
                    *backquote-hack*)  ;,.
               (set (%car (setq tail (%cdr tail)))
                    *backquote-hack*)  ;,@
               (stream-write-char stream #\`)
               (write-internal stream (%cadr tail) level-1 nil)
               t)))
          ((and (boundp head)
                (eq (symbol-value head) *backquote-hack*))
           ;",foo" = (#:|,| . foo)
           (stream-write-char stream #\,)
           (let* ((n (symbol-name head))
                  (l (length n)))
             (declare (type simple-string n) (type fixnum l))
             ;; possibilities are #:|`,| #:|,.| and #:|,@|
             (if (eql l 3)
               (stream-write-char stream (schar n 2)))
             (write-internal stream tail level-1 nil)
             t))
          (t nil))))

(eval-when (compile eval)
(defmacro %char-needs-escape-p (char escape &rest losers)
  (setq losers (remove-duplicates (cons escape losers)))
  (setq char (require-type char 'symbol))
  (dolist (c losers)
    (unless (or (characterp c) (symbolp c)) (report-bad-arg c '(or character symbol))))
  (cond ((null (cdr losers))
         `(eq ,char ,escape))
        ((and (every #'characterp losers)
              ;(every #'string-char-p losers)
              (%i> (length losers) 2))
         `(%str-member ,char ,(concatenate 'string losers)))
        (t
         `(or ,@(mapcar #'(lambda (e) `(eq ,char ,e))
                        losers)))))

(defmacro %write-escaped-char (stream char escape &rest losers)
  `(progn
     (when (%char-needs-escape-p ,char ,escape ,@losers)
       (stream-write-char ,stream ,escape))
     (stream-write-char ,stream ,char)))
)

(defun write-escaped-string (string stream &optional (delim #\"))
  (declare (type string string) (type character delim)
           (type stream stream))
  (stream-write-char stream delim)
  (do* ((limit (length string))
        (i 0 (1+ i)))
       ((= i limit))
    (declare (type fixnum last)) (declare (type fixnum limit) (type fixnum i))
    (let* ((char (char string i))
           (needs-escape? (%char-needs-escape-p char #\\ delim)))
      (if needs-escape?
          (stream-write-char stream #\\))
      (stream-write-char stream char)))
  (stream-write-char stream delim))


;;;; ----------------------------------------------------------------------
;;;; printing symbols

(defun get-*print-case* ()
  (let ((case *print-case*))
    (unless (or (eq case ':upcase) (eq case ':downcase) 
                (eq case ':capitalize) (eq case ':studly))
      (setq *print-case* ':upcase)
      (error "~S had illegal value ~S.  Reset to ~S"
             '*print-case* case ':upcase))
    case))

(defun write-a-symbol (symbol stream)
  (declare (type symbol symbol) (type stream stream))
  (let ((case (get-*print-case*))
        (name (symbol-name symbol))
        (package (symbol-package symbol)))
    (declare (type simple-string name) (type package package))
    (when (or *print-readably* *print-escape*)
      (cond ((keywordp symbol)
             (stream-write-char stream #\:))
            ((null package)
             (when (or *print-readably* *print-gensym*)
               (multiple-value-bind (s flag)
                                    (find-symbol name *package*)
                 (unless (and flag (eq s symbol))
                   (stream-write-char stream #\#)
                   (stream-write-char stream #\:)))))
            (t
             (multiple-value-bind (s flag)
                                  (find-symbol name *package*)
               (unless (and flag (eq s symbol))
                 (multiple-value-setq (s flag)
                                      (find-symbol name package))
                 (unless (and flag (eq s symbol))
                   (%write-string "#|symbol not found in home package!!|#"
                                  stream))
                 (write-pname (package-name package) case stream)
                 (stream-write-char stream #\:)
                 (unless (eq flag ':external)
                   (stream-write-char stream #\:)))))))
    (write-pname name case stream)))


(defvar *pname-buffer* (%cons-pool "12345678901234567890"))

(defun write-pname (name case stream)
  (declare (type simple-string name) (stream stream)
           (optimize (speed 3)(safety 0)))
  (let* ((readtable *readtable*)
         (readcase (readtable-case readtable))
         (escape? (or *print-readably* *print-escape*)))
      (flet ((slashify? (char)
               (declare (type character char))
               (and escape?
                    (if (alpha-char-p char) 
                      (if (eq readcase :upcase)
                        (lower-case-p char)  ; _tolower
                        (if (eq readcase :downcase)
                          (upper-case-p char)))
                      ; should be using readtable here - but (get-macro-character #\|) is nil
                      (not (%str-member
                            char
                            "!$%&*0123456789.<=>?@[]^_{}~+-/")))))
             (single-case-p (name)
               (let ((sofar nil))
                 (dotimes (i (length name) sofar)
                   (declare (type fixnum i))
                   (declare (type simple-string name))
                   (let* ((c (schar name i))
                          (c-case (if (upper-case-p c)
                                    :upcase
                                    (if (lower-case-p c)
                                      :downcase))))
                     (when c-case
                       (if sofar 
                         (if (neq sofar c-case)
                           (return nil))
                         (setq sofar c-case))))))))
        (declare (dynamic-extent slashify? single-case-p))
        (block alice
          (let ((len (length name))
                (slash-count 0)
                (last-slash-pos 0))
            (declare (type fixnum len)
                     (type fixnum slash-count last-slash-pos))                
            (when escape?
              (when (or (%izerop len)
                        (and (not (memq readcase '(:invert :preserve))) ; these never slashify alpha-p
                             (let ((m (max (floor len 4) 2)))
                               (dotimes (i (the fixnum len) nil)
                                 (declare (type fixnum i))
                                 (when (slashify? (schar name i))
                                   (setq slash-count (%i+ slash-count 1))
                                   (when (or (eql slash-count m)
                                             (eq i (1+ last-slash-pos)))
                                     (return t))
                                   (setq last-slash-pos i))))))
                (return-from alice  ; if several slashified
                  (write-escaped-string name stream #\|)))
              (when (or ;; could be read as a number -- is there no simpler way?
                     (%parse-number-token name 0 len *read-base*)
                     ;; commonlisp doesn't like symbols consisting entirely of .'s
                     (dotimes (i len t)
                       (declare (fixnum i))
                       (unless (eql (schar name i) #\.)
                         (return nil))))
                (stream-write-char stream #\\)))
            (case readcase
              (:preserve (return-from alice  (write-string name stream :start  0 :end len)))
              (:invert (return-from alice
                         (cond ((single-case-p name)(write-perverted-string name stream len :invert))
                               (t (write-string name stream :start  0 :end len)))))
              (t 
               (when (eql slash-count 0)
                 (return-from alice
                   (cond ((eq readcase case)
                          (write-string name stream :start  0 :end len))
                         (t (write-perverted-string name stream len case)))))))
            (let* ((outbuf-len (+ len len))
                   (outbuf-ptr -1)
                   (pool *pname-buffer*)
                   (outbuf (pool.data pool)))
              (declare (fixnum outbuf-ptr) (simple-string outbuf))
              (setf (pool.data pool) nil)   ; grab it.
              (unless (and outbuf (>= (length outbuf) outbuf-len))
                (setq outbuf (make-array outbuf-len :element-type 'character)))
              (dotimes (pos (the fixnum len))
                (declare (type fixnum pos))
                (let* ((char (schar name pos))
                       (slashify? (cond ((eql slash-count 0)
                                         nil)
                                        ((eql slash-count 1)
                                         (eql pos last-slash-pos))
                                        (t
                                         (slashify? char)))))
                  (declare (type character char))
                  (when slashify?
                    (setq slash-count (%i- slash-count 1))
                    (setf (schar outbuf (incf outbuf-ptr)) #\\))
                  (setf (schar outbuf (incf outbuf-ptr)) char)))
              (write-string outbuf stream :start  0 :end (1+ outbuf-ptr))
              (setf (pool.data pool) outbuf)))))))

#|
(defun write-studly-string (string stream)
  (declare (type string string) (stream stream))
  (let* ((offset 0)
         (end (length string))
         (pool *pname-buffer*)
         (outbuf-ptr -1)
         (outbuf (pool.data pool)))
    (declare (fixnum offset end outbuf-ptr))
    (setf (pool.data pool) nil)
    (unless (and outbuf (>= (length outbuf) end))
      (setq outbuf (make-array end :element-type 'character)))
    (do ((i 0 (%i+ i 1)))
        ((%i>= i end))
      (declare (type fixnum i))
      (setq offset (%i+ offset (char-int (char string i)))))
    (do ((i 0 (%i+ i 1)))
        ((%i>= i end))
      (declare (type fixnum i))
      (let ((c (char string i)))
        (declare (type character c))
        (cond ((not (and (%i< (%ilogand2
                                     (%i+ (char-int c) offset)
                                     15.)
                                   6.)
                         (alpha-char-p c))))
              ((upper-case-p c)
               (setq c (char-downcase c)))
              (t
               (setq c (char-upcase c))))
        (setf (schar outbuf (incf outbuf-ptr)) c)))
    (write-string outbuf stream :start  0 :end end)
    (setf (pool.data pool) outbuf)))
|#

(defun write-perverted-string (string stream end type)
  ; type :invert :upcase :downcase :capitalize or :studly
  (declare (fixnum end))
  (let* ((pool *pname-buffer*)
         (outbuf-ptr -1)
         (outbuf (pool.data pool))
         (word-start t)
         (offset 0))
    (declare (fixnum offset outbuf-ptr))
    (setf (pool.data pool) nil)
    (unless (and outbuf (>= (length outbuf) end))
      (setq outbuf (make-array end :element-type 'character)))  ; this  may be fat string now - do we care?
    (when (eq type :studly)
      (do ((i 0 (%i+ i 1)))
          ((%i>= i end))
        (declare (type fixnum i))
        (setq offset (%i+ offset (char-int (char string i))))))
    (do ((i 0 (%i+ i 1)))
        ((%i>= i end))
      (declare (type fixnum i))
      (let ((c (char string i)))
        (declare (type character c))        
        (cond ((alpha-char-p c)
               (case type
                 (:studly
                  (cond ((not (%i< (%ilogand2
                                    (%i+ (char-int c) offset)
                                    15.)
                                   6.)))
                        ((upper-case-p c)
                         (setq c (char-downcase c)))
                        (t
                         (setq c (char-upcase c)))))
                 (:invert
                  (setq c (if (upper-case-p c)(char-downcase c)(char-upcase c))))
                 (:upcase
                  (setq c (char-upcase c)))
                 (:downcase
                  (setq c (char-downcase c)))
                 (:capitalize (setq c (if word-start 
                                        (progn (setq word-start nil)
                                               (char-upcase c))
                                        (char-downcase c))))))
              ((digit-char-p c)(setq word-start nil))
              (t (setq word-start t)))
        (setf (schar outbuf (incf outbuf-ptr)) c)))
    (write-string outbuf stream :start  0 :end end)
    (setf (pool.data pool) outbuf)))


;;;; ----------------------------------------------------------------------
;;;; printing arrays

;; *print-array*
;; *print-simple-vector*
;; *print-simple-bit-vector*
;; *print-string-length*
(defun write-an-array (array stream level)
  (declare (type array array) (type stream stream) (type fixnum level))
  (let* ((rank (array-rank array))
         (vector? (eql rank 1))
         (simple? (simple-array-p array))
         (simple-vector? (simple-vector-p array))
         ;; non-*print-string-length*-truncated strings are printed by
         ;;  write-a-frob
         (string? (stringp array))
         (bit-vector? (bit-vector-p array))
         (fill-pointer? (array-has-fill-pointer-p array))
         (adjustable? (adjustable-array-p array))
         (displaced? (displaced-array-p array))
         (total-size (array-total-size array))
         (length (and vector? (length array)))
         (print-length (get-*print-frob* '*print-length*))
         (print-array (get-*print-frob* '*print-array* nil t)))
    (declare (type fixnum rank) (type fixnum total-size)
             (type fixnum print-length))
    (unless
      (cond (string?
             nil)
            (bit-vector?
             (when (or print-array
                       (and simple?
                            (%i<= length
                                  (max 26 (get-*print-frob*
                                           '*print-simple-bit-vector*
                                           ;; there's no reason not to print
                                           ;;  short bit-vectors -- #*0101010
                                           ;;  is shorter than #<array ...>
                                           0
                                           most-positive-fixnum)))))
               (stream-write-char stream #\#) (stream-write-char stream #\*)
               (do ((i 0 (%i+ i 1))
                    (l print-length (%i- l 1)))
                   (nil)
                 (declare (type fixnum i) (type fixnum l))
                 (cond ((eql i length)
                        (return))
                       ((eql l 0)
                        (%write-string "..." stream))
                       (t
                        (stream-write-char stream (if (eql (bit array i) 0) #\0 #\1)))))
               t))
            ((and *print-pretty* print-array)
             (let ((*current-level* (if (and *print-level* (not *print-readably*))
                                      (- *print-level* level)
                                      0)))
               (pretty-array stream array))
             t)
            (vector?
             (when (or print-array
                       (and simple-vector?
                            (%i<= length (get-*print-frob* 
                                          '*print-simple-vector*
                                          0
                                          most-positive-fixnum))))
               (pp-start-block stream "#(")
               (do ((i 0 (%i+ i 1))
                    (l print-length (%i- l 1)))
                   (nil)
                 (declare (type fixnum i) (type fixnum l))
                 (cond ((eql i length)
                        (return))
                       ((eql l 0)
                        ;; can't use write-abbreviation since there is
                        ;;  no `object' for the abbreviation to represent
                        (%write-string " ..." stream)
                        (return))
                       (t (unless (eql i 0) (pp-space stream))
                          (write-internal stream (aref array i) (%i- level 1) nil))))
               (pp-end-block stream #\))
               t))
            ((and print-array (and (eq (array-element-type array) t)
                                   (not fill-pointer?)(not adjustable?)))
             (let ((rank (array-rank array)))
               (stream-write-char stream #\#)
               (%pr-integer rank 10. stream)
               (stream-write-char stream #\a)
               (if (eql rank 0)
                 (write-internal stream (aref array) (%i- level 1) nil)
                 (multiple-value-bind (array-data offset)
                                      (array-data-and-offset array)
                   (write-array-elements-1 
                     stream level
                     array-data offset
                     (array-dimensions array)))))
             t)
            (t 
             ;; fall through -- print randomly
             nil))
      ;; print array using #<...>
      (print-unreadable-object (array stream)
        (if vector?
          (progn
            (write-a-symbol (cond (simple-vector?
                                   'simple-vector)
                                  (string?
                                   (if simple? 'simple-string 'string))
                                  (bit-vector?
                                   (if simple? 'simple-bit-vector 'bit-vector))
                                  (t 'vector))
                            stream)
            (pp-space stream)
            (%pr-integer total-size 10. stream)
            (when fill-pointer?
              (let ((fill-pointer (fill-pointer array)))
                (declare (fixnum fill-pointer))
                (pp-space stream)
                (%write-string "fill-pointer" stream)
                (unless (eql fill-pointer total-size)
                  (stream-write-char stream #\space)
                  (%pr-integer fill-pointer 10. stream)))))
          (progn
            (write-a-symbol 'array stream)
            (pp-space stream)
            (if (eql rank 0) (%write-string "0-dimensional" stream))
            (dotimes (i (the fixnum rank))
              (unless (eql i 0) (stream-write-char stream #\x))
              (%pr-integer (array-dimension array i) 10. stream))))
        (let ((type (array-element-type array)))
          (unless (or simple-vector? string? bit-vector?   ; already written "#<string" or whatever
                      (eq type 't))
            (pp-space stream)
            (%write-string "type " stream)
            (write-internal stream type
                            ;; yes, I mean level, not (1- level)
                            ;; otherwise we end up printing things
                            ;; like "#<array 4 type #>"
                            level nil)))
        (cond (simple?
               (unless (or simple-vector? string? bit-vector?)
                 ;; already written "#<simple-xxx"
                 (stream-write-char stream #\,)
                 (pp-space stream)
                 (%write-string "simple" stream)))
              (adjustable?
               (stream-write-char stream #\,)
               (pp-space stream)
               (%write-string "adjustable" stream))
              (displaced?
               ;; all multidimensional (and adjustable) arrays in ccl are
               ;;  displaced, even when they are simple-array-p
               (stream-write-char stream #\,)
               (pp-space stream)
               (%write-string "displaced" stream)))
        ;; (when stack-allocated? ...) etc, etc
        (when (and string? (%i> length 20))
          (flet ((foo (stream string start end)
                      (declare (type fixnum start) (type fixnum end)
                               (type string string))
                      (do ((i start (%i+ i 1)))
                          ((%i>= i end))
                        (let ((c (char string i)))
                          (declare (type character c))
                          (if (not (graphic-char-p c))
                            (return)
                            (%write-escaped-char stream c #\\ #\"))))))
            #|(%write-string " \"" stream)|# (pp-space stream)
            (foo stream array 0 12)
            (%write-string "..." stream)
            (foo stream array (%i- length 6) length)
              #|(stream-write-char stream #\")|#))))))

(defun write-array-elements-1 (stream level
                               array-data offset
                               dimensions)
  (declare (type stream stream) (type fixnum level) 
           (type vector array-data) (type fixnum offset)
           (type list dimensions))
  (block written
    (let ((tail (%cdr dimensions))
          (print-length (get-*print-frob* '*print-length*))
          (level-1 (%i- level 1))
          (limit (%car dimensions))
          (step 1))
      (when (and (null tail)
                 (%i> level-1 0)
                 (or (bit-vector-p array-data)
                     (and (stringp array-data)
                          (%i<= limit print-length))))
        (return-from written
          ;;>> cons cons.  I was lazy.
          ;;>>  Should code a loop to write the elements instead
          (write-an-array (%make-displaced-array
                            ;; dimensions displaced-to
                            limit array-data 
                            ;; fill-pointer adjustable
                            nil nil
                            ;; displaced-index-offset
                            offset)
                          stream level-1)))
      (pp-start-block stream #\()
      (dolist (e tail) (setq step (%i* e step)))
      (do* ((o offset (%i+ o step))
            (i 0 (1+ i)))
           (nil)
        (declare (type fixnum o) (type fixnum i) (type fixnum limit)
                 (type fixnum step) (type fixnum print-length) 
                 (type fixnum level-1))
        (cond ((eql i print-length)
               (%write-string " ..." stream)
               (return))
              ((eql i limit)
               (return))
              ((= i 0))
              (t
               (pp-space stream (if (null tail) ':fill ':linear))))
        (cond ((null tail)
               (write-internal stream (aref array-data o) level-1 nil))
              ((eql level-1 0)
               ;; can't use write-abbreviation since this doesn't really
               ;;  abbreviate a single object
               (stream-write-char stream #\#))
              (t
               (write-array-elements-1 stream level-1
                                       array-data o tail))))
      (pp-end-block stream #\)))))
    
;;;; ----------------------------------------------------------------------

; A "0" in the sd-print-function => inherit from superclass.
(defun structure-print-function (class)
  (let* ((pf (ccl::sd-print-function class))
         (supers (cdr (sd-superclasses class))))
    (do* ()
         ((neq pf 0) pf)
      (if supers 
        (setq pf (sd-print-function (gethash (pop supers) %defstructs%)))
        (return)))))

(defun write-a-structure (object stream level)
  (declare (type stream stream) (type fixnum level))
  (let* ((class (ccl::struct-def object)) ;;guaranteed non-NIL if this function is called
         (pf (structure-print-function class)))
    (cond (pf
	   (if (consp pf)
	     (funcall (%car pf) object stream)
	     (funcall pf 
		      object stream (backtranslate-level level))))
          ((and (not *print-structure*) (not *print-readably*))
           (print-unreadable-object (object stream :identity t)
            (write-a-symbol (ccl::sd-name class) stream)))
          (t
           (let ((level-1 (ccl::%i- level 1))
                 (slots (ccl::sd-slots class)))
             (declare (type fixnum level-1) (type list slots))
             (%write-string "#S(" stream)
             ;; deliberately not (1- level)
             (write-a-symbol (ccl::sd-name class) stream)
             (when (not (null (cdr slots)))(pp-start-block stream #\Space))
             (if (and (%i< level 1) (not (null slots)))
                 ;; It would be stupid to print "#S(# # # # # # #)"
                 (%write-string " # ..." stream)
               (do ((l (%i- (get-*print-frob* '*print-length*) 1)
                       (%i- l 2))
                    (first? t)
                    (print-case (get-*print-case*)))
                   (nil)
                 (declare (type fixnum l))
                 (cond ((null slots)
                        (return))
                       ((%i< l 1)
                        ;; Note write-abbreviation since it isn't abbreviating an object
                        (%write-string " ..." stream)
                        (return)))
                 (let* ((slot (prog1 (%car slots)
                                (setq slots (%cdr slots))))
                        (symbol (ccl::ssd-name slot)))
                   (when (symbolp symbol)
                     (if first?
                         (setq first? nil)
                         (pp-space stream ':linear))
                     (stream-write-char stream #\:)
                     (write-pname (symbol-name symbol) print-case stream)
                     (when (%i> l 1)
                       (pp-space stream)
                       (write-internal stream (uvref object (ccl::ssd-offset slot))
                                       level-1 nil)))))))
           (pp-end-block stream #\))))))

(%fhave 'encapsulated-function-name ;(fn) ;Redefined in encapsulate
        (qlfun bootstrapping-encapsulated-function-name (fn)
          (declare (ignore fn))
          nil))


(%fhave '%traced-p ;(fn) ;Redefined in encapsulate
        (qlfun bootstrapping-%traced-p (fn)
          (declare (ignore fn))
          nil))

(%fhave '%advised-p ;(fn) ;Redefined in encapsulate
        (qlfun bootstrapping-%advised-p (fn)
          (declare (ignore fn))
          nil))



(defun write-a-function (lfun stream level)  ; screwed up
  (print-unreadable-object (lfun stream :identity t)
    (let* ((name (function-name lfun))
           ; actually combined-method has its oun print-object method and doesn't get here.
           ; standard-generic-function has a print-object method that just calls this.
           (gf-or-cm (or (standard-generic-function-p lfun) (combined-method-p lfun))))
      (cond ((and (not (compiled-function-p lfun))
                  (not gf-or-cm))
             ; i.e. closures
             (write-internal stream (%type-of lfun) level nil)
             (when name
               (pp-space stream)
               (write-internal stream name (%i- level 1) nil)))
            ((not name)
             (%lfun-name-string lfun stream t))
            (t
             (if gf-or-cm
               (write-internal stream (class-name (class-of lfun)) level nil)
               ; we also have print-object methods for method-function and interpreted-method-function
               (%write-string (cond ((typep lfun 'interpreted-method-function)
                                     "Interpreted Method-function")
                                    ((typep lfun 'method-function)
                                     "Compiled Method-function")
                                    ((typep lfun 'interpreted-function)
                                     "Interpreted-function")
                                    (t "Compiled-function"))
                            stream))
             (stream-write-char stream #\space)
             (write-internal stream name (%i- level 1) nil)
             (cond ((and (symbolp name) (eq lfun (macro-function name)))
                    (%write-string " Macroexpander" stream)) ;What better?                 
                   ((not (function-is-current-definition? lfun))
                    ;;>> Nice if it could print (Traced), (Internal), (Superseded), etc
                    (cond ((%traced-p name)
                           (%write-string " (Traced Original) " stream))
                          ((%advised-p name)
                           (%write-string " (Advised Original) " stream))
                          (t (%write-string " (Non-Global) " stream))))))))))


(defun function-is-current-definition? (function)
  (let ((name (function-name function)))
    (and name
         (valid-function-name-p name)
         (eq function (fboundp name)))))

;; outputs to stream or returns a string.  Barf!
;; Making not matters not worse ...
(defun %lfun-name-string (lfun &optional stream suppress-address)
  (unless (functionp lfun) (report-bad-arg lfun 'function))
  (if (null stream)
    (with-output-to-string (s) (%lfun-name-string lfun s))
    (let ((name (function-name lfun)))
      (if name
	(prin1 name stream)
	(let* ((fnaddr (%address-of lfun))
	       (kernel-function-p (kernel-function-p lfun)))
	  (%write-string (if kernel-function-p
			   "Internal " "Anonymous ")
			 stream)
	  (if (standard-generic-function-p lfun)
	    (prin1 (class-name (class-of lfun)) stream)
	    (%write-string "Function" stream))
	  (unless suppress-address
	    (stream-write-char stream #\ )
	    (write-an-integer  fnaddr
			       stream 16. t)))))))


;;;; ----------------------------------------------------------------------

(defun write-a-package (pkg stream)
  (print-unreadable-object (pkg stream)
    (if (null (pkg.names pkg))
      (%write-string "Deleted Package" stream)
      (progn
        (%write-string "Package " stream)
        (write-escaped-string (package-name pkg) stream)))))



(defun write-a-macptr (macptr stream)
  (let* ((null (%null-ptr-p macptr))
	 (ftype (%macptr-ftype macptr))
	 (ftype-info (if ftype (unparse-foreign-type ftype))))
    (print-unreadable-object (macptr stream)
      (if null
	(progn
	  (%write-string "A Null Mac Pointer" stream)
	  (when ftype-info
	    (format stream " to foreign type ~s" ftype-info)))
	(progn
	  (pp-start-block stream "A Mac Pointer")
	  (%write-macptr-termination-info macptr stream)
	  (when ftype-info
	    (format stream " to foreign type ~s at" ftype-info))
	  (stream-write-char stream #\ )
	  (write-an-integer (%ptr-to-int macptr) stream 16. t))))))

; redefined by macptr-termination.lisp
(defun %write-macptr-termination-info (macptr stream)
  (declare (ignore macptr stream)))



; This special-casing for wrappers is cheaper than consing a class
(defun write-an-istruct (istruct stream level)
  (let* ((type (uvref istruct 0))
         (wrapper-p  (eq type 'class-wrapper)))
    (print-unreadable-object (istruct stream :identity t)
      (write-internal stream type (%i- level 1) nil)
      (when wrapper-p
        (pp-space stream)
        (print-object (class-name (%wrapper-class istruct)) stream)))))

(defun write-a-uvector (uvec stream level)
  (declare (ignore level))
  (print-unreadable-object (uvec stream :identity t :type t)))
  

(defmethod print-object ((slotdef slot-definition) stream)
  (print-unreadable-object (slotdef stream :identity t :type t)
    (format stream "for ~a slot ~s"
            (string-downcase (slot-definition-allocation slotdef))
            (standard-slot-definition.name slotdef))))

(defmethod print-object ((spec eql-specializer) stream)
  (print-unreadable-object (spec stream :identity t :type t)
    (format stream "~s" (if (slot-boundp spec 'object)
			  (eql-specializer-object spec)
			  "<unbound>"))))

(defmethod print-object ((svar svar) stream)
  (print-unreadable-object (svar stream :identity t :type t)
    (format stream "~s ~s"
            (%svref svar ppc32::svar.symbol-cell)
            (%svref svar ppc32::svar.idx-cell))))

(defmethod print-object ((slot-id slot-id) stream)
  (print-unreadable-object (slot-id stream :identity t :type t)
    (format stream "for ~s/~d"
            (slot-id.name  slot-id)
            (slot-id.index  slot-id))))
            

;;; ======================================================================


(defun real-print-stream (&optional (stream nil))
  (cond ((null stream)
         *standard-output*)
        ((eq stream t)
         *terminal-io*)
        ((streamp stream)
         stream)
        ((istruct-typep stream 'xp-structure)
         (get-xp-stream stream))
        (t
         (report-bad-arg stream '(or stream (member nil t))))))

(defun write-1 (object stream &optional levels-left)
  (setq stream (real-print-stream stream))
  (when (not levels-left)
    (setq levels-left
          (if *current-level* 
            (if *print-level*
              (- *print-level* *current-level*)
              most-positive-fixnum)
            (%current-write-level% stream t))))
  (cond 
   ((< levels-left 0)
    ;; *print-level* truncation
    (stream-write-entire-string stream "#"))
   (t (write-internal stream
                      object 
                      (min levels-left most-positive-fixnum)
                      nil)))
  object)

;;;; ----------------------------------------------------------------------
;;;; User-level interface to the printer


(defun write (object
              &key (stream *standard-output*)
                   (escape *print-escape*)
                   (radix *print-radix*)
                   (base *print-base*)
                   (circle *print-circle*)
                   (pretty *print-pretty*)
                   (level *print-level*)
                   (length *print-length*)
                   (case *print-case*)
                   (gensym *print-gensym*)
                   (array *print-array*)
                   (readably *print-readably*)
                   (right-margin *print-right-margin*)
                   (miser-width *print-miser-width*)
                   (lines *print-lines*)
                   (pprint-dispatch *print-pprint-dispatch*)
                   ;;>> Do I really want to add these to WRITE??
                   (structure *print-structure*)
                   (simple-vector *print-simple-vector*)
                   (simple-bit-vector *print-simple-bit-vector*)
                   (string-length *print-string-length*))
  (let ((*print-escape* escape)
        (*print-radix* radix)
        (*print-base* base)
        (*print-circle* circle)
        (*print-pretty* pretty)
        (*print-level* level)
        (*print-length* length)
        (*print-case* case)
        (*print-gensym* gensym)
        (*print-array* array)
        (*print-readably* readably)
        (*print-right-margin* right-margin)
        (*print-miser-width* miser-width)
        (*print-lines* lines)
        (*print-pprint-dispatch* pprint-dispatch)
        ;;>> Do I really want to add these to WRITE??
        (*print-structure* structure)
        (*print-simple-vector* simple-vector)
        (*print-simple-bit-vector* simple-bit-vector)
        (*print-string-length* string-length))
    (write-1 object stream)))

(defun write-to-string (object
                        &key (escape *print-escape*)
                             (radix *print-radix*)
                             (base *print-base*)
                             (circle *print-circle*)
                             (pretty *print-pretty*)
                             (level *print-level*)
                             (length *print-length*)
                             (case *print-case*)
                             (gensym *print-gensym*)
                             (array *print-array*)
                             (readably *print-readably*)
                             (right-margin *print-right-margin*)
                             (miser-width *print-miser-width*)
                             (lines *print-lines*)
                             (pprint-dispatch *print-pprint-dispatch*)
                             ;;>> Do I really want to add these to WRITE??
                             (structure *print-structure*)
                             (simple-vector *print-simple-vector*)
                             (simple-bit-vector *print-simple-bit-vector*)
                             (string-length *print-string-length*))
    (let ((*print-escape* escape)
          (*print-radix* radix)
          (*print-base* base)
          (*print-circle* circle)
          (*print-pretty* pretty)
          (*print-level* level)
          (*print-length* length)
          (*print-case* case)
          (*print-gensym* gensym)
          (*print-array* array)
          ;; I didn't really wan't to add these, but I had to.
          (*print-readably* readably)
          (*print-right-margin* right-margin)
          (*print-miser-width* miser-width)
          (*print-lines* lines)
          (*print-pprint-dispatch* pprint-dispatch)
          ;;>> Do I really want to add these to WRITE??
          (*print-structure* structure)
          (*print-simple-vector* simple-vector)
          (*print-simple-bit-vector* simple-bit-vector)
          (*print-string-length* string-length))
      (with-output-to-string (stream)
        (write-1 object stream))))

(defun prin1-to-string (object)
  (with-output-to-string (s)
    (prin1 object s)))

(defun princ-to-string (object)
  (with-output-to-string (s)
    (princ object s)))

(defun prin1 (object &optional stream)
  (let ((*print-escape* t))
    (write-1 object stream)))

(defun princ (object &optional stream)
  (let ((*print-escape* nil)
        (*print-readably* nil)
        (*print-circle* nil))
    (write-1 object stream)))

(defun print (object &optional stream)
  (setq stream (real-print-stream stream))
  (terpri stream)
  (let ((*print-escape* t))
    (write-1 object stream))
  (write-char #\Space stream)
  object)

; redefined by pprint module if loaded
(defun pprint (object &optional stream)
  (print object stream)
  nil)                                  ; pprint returns nil


(defun read-sequence (seq stream &key (start 0) end)
  (setq end (check-sequence-bounds seq start end))
  (locally (declare (fixnum start end))
    (if (= start end)
      start
      (seq-dispatch
       seq
       (+ start (the fixnum (stream-read-list
			     stream
			     (nthcdr start seq)
			     (the fixnum (- end start)))))
       (multiple-value-bind (vector offset) (array-data-and-offset seq)
	 (declare (fixnum offset))
	 (-
	  (stream-read-vector
	   stream
	   vector
	   (the fixnum (+ offset start))
	   (the fixnum (+ offset end)))
	  offset))))))



(defun write-sequence (seq stream &key (start 0) end)
  (setq end (check-sequence-bounds seq start end))
  (locally (declare (fixnum start end))
    (seq-dispatch
     seq
     (stream-write-list stream (nthcdr start seq) (the fixnum (- end start)))
     (multiple-value-bind (vector offset) (array-data-and-offset seq)
       (stream-write-vector
	stream
	vector
	(the fixnum (+ offset start))
	(the fixnum (+ offset end))))))
  seq)

(defpackage "GRAY"
  (:use)
  (:import-from "CCL"
                "FUNDAMENTAL-STREAM"
                "FUNDAMENTAL-INPUT-STREAM"
                "FUNDAMENTAL-OUTPUT-STREAM"
                "FUNDAMENTAL-CHARACTER-STREAM"
                "FUNDAMENTAL-CHARACTER-INPUT-STREAM"
                "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
                "FUNDAMENTAL-BINARY-STREAM"
                "FUNDAMENTAL-BINARY-INPUT-STREAM"
                "FUNDAMENTAL-BINARY-OUTPUT-STREAM"

                "STREAM-READ-CHAR"
                "STREAM-UNREAD-CHAR"
                "STREAM-READ-CHAR-NO-HANG"
                "STREAM-PEEK-CHAR"
                "STREAM-LISTEN"
                "STREAM-READ-LINE"
                "STREAM-CLEAR-INPUT"

                "STREAM-WRITE-CHAR"
                "STREAM-LINE-COLUMN"
                "STREAM-START-LINE-P"
                "STREAM-WRITE-STRING"
                "STREAM-TERPRI"
                "STREAM-FRESH-LINE"
                "STREAM-FORCE-OUTPUT"
                "STREAM-FINISH-OUTPUT"
                "STREAM-CLEAR-OUTPUT"
                "STREAM-ADVANCE-TO-COLUMN"

                "STREAM-READ-BYTE"
                "STREAM-WRITE-BYTE"
                )
  (:export
   "FUNDAMENTAL-STREAM"
   "FUNDAMENTAL-INPUT-STREAM"
   "FUNDAMENTAL-OUTPUT-STREAM"
   "FUNDAMENTAL-CHARACTER-STREAM"
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM"
   "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
   "FUNDAMENTAL-BINARY-STREAM"
   "FUNDAMENTAL-BINARY-INPUT-STREAM"
   "FUNDAMENTAL-BINARY-OUTPUT-STREAM"

   "STREAM-READ-CHAR"
   "STREAM-UNREAD-CHAR"
   "STREAM-READ-CHAR-NO-HANG"
   "STREAM-PEEK-CHAR"
   "STREAM-LISTEN"
   "STREAM-READ-LINE"
   "STREAM-CLEAR-INPUT"

   "STREAM-WRITE-CHAR"
   "STREAM-LINE-COLUMN"
   "STREAM-START-LINE-P"
   "STREAM-WRITE-STRING"
   "STREAM-TERPRI"
   "STREAM-FRESH-LINE"
   "STREAM-FORCE-OUTPUT"
   "STREAM-FINISH-OUTPUT"
   "STREAM-CLEAR-OUTPUT"
   "STREAM-ADVANCE-TO-COLUMN"

   "STREAM-READ-BYTE"
   "STREAM-WRITE-BYTE"
))
                
                
