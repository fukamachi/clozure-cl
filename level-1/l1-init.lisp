;;;-*-Mode: LISP; Package: CCL -*-
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


(defvar *ccl-file-creator* #+ppc-target :|CCL2| #-ppc-target :|CCL2|)


(eval-when (:compile-toplevel :execute :load-toplevel)

(defconstant most-positive-short-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 0))
(defconstant most-negative-short-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 -1))
(defconstant most-positive-single-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 0))
(defconstant most-negative-single-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 -1))


(defconstant least-positive-short-float (make-short-float-from-fixnums 1 0 0))
(defconstant least-negative-short-float (make-short-float-from-fixnums 1 0 -1))
(defconstant least-positive-single-float (make-short-float-from-fixnums 1 0 0))
(defconstant least-negative-single-float (make-short-float-from-fixnums 1 0 -1))

(defconstant short-float-epsilon (make-short-float-from-fixnums 1 103 0))
(defconstant short-float-negative-epsilon (make-short-float-from-fixnums 1 102 0))
(defconstant single-float-epsilon (make-short-float-from-fixnums 1 103 0))
(defconstant single-float-negative-epsilon (make-short-float-from-fixnums 1 102 0))

(defconstant least-positive-normalized-short-float (make-short-float-from-fixnums 1 1 0))
(defconstant least-negative-normalized-short-float (make-short-float-from-fixnums 1 1 -1))
(defconstant least-positive-normalized-single-float (make-short-float-from-fixnums 1 1 0))
(defconstant least-negative-normalized-single-float (make-short-float-from-fixnums 1 1 -1))

(let ((bigfloat (make-float-from-fixnums #x1ffffff #xfffffff #x7fe 0)))
  ; do it this way if you want to be able to compile before reading floats works  
  (defconstant most-positive-double-float bigfloat)
  (defconstant most-positive-long-float bigfloat)
  )

(let ((littleposfloat (make-float-from-fixnums 0 1 0 0 )))
  (defconstant least-positive-double-float littleposfloat)
  (defconstant least-positive-long-float littleposfloat)
  )

(let ((littlenegfloat (make-float-from-fixnums 0 1 0 -1)))  
  (defconstant least-negative-double-float littlenegfloat)
  (defconstant least-negative-long-float littlenegfloat)
  )

(let ((bignegfloat (make-float-from-fixnums #x1ffffff #xfffffff #x7fe -1)))
  (defconstant most-negative-double-float bignegfloat)
  (defconstant most-negative-long-float bignegfloat)
  )

(let ((eps (make-float-from-fixnums #x1000000 1 #x3ca 0))) ;was wrong
  (defconstant double-float-epsilon eps)
  (defconstant long-float-epsilon eps)
  )

(let ((eps- (make-float-from-fixnums #x1000000 1 #x3c9 1)))
  (defconstant double-float-negative-epsilon eps-)
  (defconstant long-float-negative-epsilon eps-)
  )

(let ((norm (make-float-from-fixnums 0 0 1 0)))
  (defconstant least-positive-normalized-double-float norm)
  (defconstant least-positive-normalized-long-float norm)
  )

(let ((norm- (make-float-from-fixnums 0 0 1 -1)))
  (defconstant least-negative-normalized-double-float norm-)
  (defconstant least-negative-normalized-long-float norm-)
  )

(defconstant pi (make-float-from-fixnums #x921fb5 #x4442d18 #x400 0))

)



(defconstant boole-clr 0)
(defconstant boole-set 1)
(defconstant boole-1 2)
(defconstant boole-2 3)
(defconstant boole-c1 4)
(defconstant boole-c2 5)
(defconstant boole-and 6)
(defconstant boole-ior 7)
(defconstant boole-xor 8)
(defconstant boole-eqv 9)
(defconstant boole-nand 10)
(defconstant boole-nor 11)
(defconstant boole-andc1 12)
(defconstant boole-andc2 13)
(defconstant boole-orc1 14)
(defconstant boole-orc2 15)



(defconstant internal-time-units-per-second 1000)

(defconstant char-code-limit #x100)

(defconstant array-rank-limit #x2000)
(defconstant multiple-values-limit 200)
(defconstant lambda-parameters-limit #x2000)
(defconstant call-arguments-limit #x2000)

; Currently, vectors can be at most (expt 2 22) bytes, and
; the largest element (double-float or long-float) is 8 bytes:
#| to get largest element size...
(apply #'max (mapcar #'(lambda (type)
                         (%vect-byte-size (make-array 1 :element-type type)))
                     *cl-types*))
|#

(defconstant array-dimension-limit array-total-size-limit)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hide-lsh (a b) ; so the compiler won't constant fold at compile time
    (lsh a b)))

(defconstant most-positive-fixnum (load-time-value (hide-lsh -1 -1)))
(defconstant most-negative-fixnum (load-time-value (1- (- (hide-lsh -1 -1)))))






; single-float ought to be ok if only one format but not sure it will work today
;(defvar *read-default-float-format* 'double-float)

(defconstant lambda-list-keywords 
  '(&OPTIONAL &REST &AUX &KEY &ALLOW-OTHER-KEYS &BODY &ENVIRONMENT &WHOLE))





(defparameter %toplevel-catch% ':toplevel)

(defvar *read-default-float-format* 'single-float)

(defvar *read-suppress* nil)

(defvar *read-base* 10.)


(defvar %doc-string-file ())
(defvar *fast-help* t)
(defglobal *idle* nil)
(defparameter *foreground* t)
(defparameter *warn-if-redefine-kernel* nil)
(defvar *next-screen-context-lines* 2 "Number of lines to show of old screen
  after a scroll-up or scroll-down.")

(defparameter *compiling-file* nil 
  "Name of outermost file being compiled or NIL if not compiling a file.")

(defvar *eval-fn-name* nil)


(defvar *compile-definitions* t
  "When non-NIL and the evaluator's lexical environment contains no
  lexical entities, causes FUNCTION and NFUNCTION forms to be compiled.")
#|
(defvar *fast-eval* ()
  "If non-nil, compile-and-call any forms which would be expensive to evaluate.")
|#
(defvar *declaration-handlers* ())

(defvar *eval-macro-displacement* nil)

(defvar *cursorhook* nil)
;(defvar *scrap-ok* t)
(defvar *scrap-count* -1)
(defvar *current-view* nil)
(defvar *current-font-view* nil)

(defglobal *selected-window* nil)
(defglobal *modal-dialog-on-top* nil)

(defvar *lisp-system-pointer-functions* nil)
(defvar *lisp-user-pointer-functions* nil)
(defvar *lisp-cleanup-functions* nil)   ; list of (0-arg) functions to call before quitting Lisp
(defvar *lisp-startup-functions* nil)   ; list of funs to call after startup.
(defvar %lisp-system-fixups% nil)


(setf (*%saved-method-var%*) nil)

; The GC expects these to be NIL or a function of no args
(defvar *pre-gc-hook* nil)
(defvar *post-gc-hook* nil)

; These are used by add-gc-hook, delete-gc-hook
(defvar *pre-gc-hook-list* nil)
(defvar *post-gc-hook-list* nil)

(defvar *backtrace-dialogs* nil)
;(defvar *stepper-running* nil)
(defparameter *last-mouse-down-time* 0)
(defparameter *last-mouse-down-position* 0)

(defvar %handlers% ())


#|
(defvar %restarts% (list (list (%cons-restart 'abort
                                              #'(lambda (&rest ignore)
                                                  (declare (ignore ignore))
                                                  (throw :toplevel nil))
                                              "Restart the toplevel loop."
                                              nil
                                              nil))))
|#

(defvar %restarts% nil)

(defvar ccl::*kernel-restarts* nil)
(defvar *condition-restarts* nil "explicit mapping between c & r")
(declaim (type list %handlers% %restarts% ccl::*kernel-restarts* *condition-restarts*))




(defparameter *%periodic-tasks%* nil)
(defparameter *dribble-stream* nil)

(defconstant *keyword-package* *keyword-package*)
(defconstant *common-lisp-package* *common-lisp-package*)
(defconstant *ccl-package* *ccl-package*)

(defparameter *load-print* nil)
(defparameter *loading-files* nil)
(defvar *loading-file-source-file* nil)
(defparameter *break-level* 0)
(defparameter *last-break-level* 0)
(defvar *record-source-file* nil)       ; set in l1-utils.
(defvar *warn-if-redefine* nil)         ; set in l1-utils.
(defparameter *level-1-loaded* nil)     ; set t by l1-boot
(defparameter *save-definitions* nil)
(defparameter *save-local-symbols* t)

(defvar *modules* nil
  "Holds list of names of modules that have been loaded thus far.
   The names are case sensitive strings.")




(defparameter *eof-value* (cons nil nil))

(defvar *gc-event-status-bits*)         ; also initialized by kernel

(defparameter *top-listener* nil)

(defvar *main-listener-process-name* "Initial")

(defvar *open-file-streams* nil)

; Note: all the stream definitions have moved to l1-streams:
; *terminal-io*, *standard-input*, *standard-output*, *pop-up-terminal-io*,
; *error-output*, *trace-output*
;
; All the window definitions are in l1-windows & l1-edwin



(defvar *listener-indent* nil)

(defparameter *autoload-lisp-package* nil)   ; Make 'em suffer
(defparameter *apropos-case-sensitive-p* nil)

(defloadvar *total-gc-microseconds* (let* ((p (malloc (* 5 8))))
                                      (dotimes (i 10 p)
                                        (setf (%get-long p (* i 4)) 0))))

(defloadvar *total-bytes-freed* (let* ((p (malloc 8)))
                                  (setf (%get-long p 0) 0
                                        (%get-long p 4) 0)
                                  p))



; MCL-Alice build constants...
(defconstant *genapp-src* "./AppGen.lisp")
(defconstant *genapp-fsl* #.(merge-pathnames *.fasl-pathname* "./AppGen"))

; end of L1-init.lisp

