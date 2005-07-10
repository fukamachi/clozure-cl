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


(eval-when (eval compile)
  (require 'defstruct-macros))

(defun short-site-name  ()
  "Return a string with the abbreviated site name, or NIL if not known."
  (or *short-site-name* "unspecified"))

(defun long-site-name   ()
  "Return a string with the long form of the site name, or NIL if not known."
  (or *long-site-name* "unspecified"))

(defun machine-instance ()
  "Return a string giving the name of the local machine."
  (%uname 1))


(defun machine-type ()
  "Returns a string describing the type of the local machine."
  (%uname 4))



(defun machine-version ()
  "Return a string describing the version of the computer hardware we
are running on, or NIL if we can't find any useful information."
  #+darwinppc-target
  (%stack-block ((mib 8))
    (setf (%get-long mib 0) #$CTL_HW
	  (%get-long mib 4) #$HW_MODEL)
    (%stack-block ((res 256)
		   (reslen 4))
      (setf (%get-byte res 0) 0
	    (%get-long reslen 0) 256)
      (if (zerop (#_sysctl mib 2 res reslen (%null-ptr) 0))
	(return-from machine-version (%get-cstring res)))))
  #+linuxppc-target
  (with-open-file (f "/proc/device-tree/model" :if-does-not-exist nil)
    (when f
      (let* ((s (read-line f nil nil)))
	(when s (return-from machine-version s)))))
  "Unknown")


(defun software-type ()
  "Return a string describing the supporting software."
  (%uname 0))


(defun software-version ()
  "Return a string describing version of the supporting software, or NIL
   if not available."
  (%uname 2))







;;; Yawn.



(defmethod documentation (thing doc-id)
  (%get-documentation thing doc-id))

(defun set-documentation (thing doc-id new)
  (setf (documentation thing doc-id) new))

(defmethod (setf documentation) (new thing doc-id)
  (%put-documentation thing doc-id new))


(defmethod documentation ((symbol symbol) (doc-type (eql 'function)))
  (let* ((def (fboundp symbol)))	; FBOUNDP returns info about definition
    (when def
      (%get-documentation def t))))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'function)))
  (let* ((def (fboundp symbol)))	; FBOUNDP returns info about definition
    (when def
      (%put-documentation def
                          t
                          new))
    new))

(defmethod documentation ((symbol symbol) (doc-type (eql 'setf)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'setf)))
  (call-next-method))


(defmethod documentation ((symbol symbol) (doc-type (eql 'variable)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'variable)))
  (call-next-method))

(defmethod documentation ((symbol symbol) (doc-type (eql 'compiler-macro)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'compiler-macro)))
  (call-next-method))

(defmethod documentation ((symbol symbol) (doc-type (eql 'type)))
  (let* ((class (find-class symbol nil)))
    (if class
      (documentation class doc-type)
      (call-next-method))))

(defmethod (setf documentation) (new (symbol symbol) (doc-type (eql 'type)))
  (let* ((class (find-class symbol nil)))
    (if class
      (setf (documentation class doc-type) new)
      (call-next-method))))

(defmethod documentation ((symbol symbol) (doc-type (eql 'method-combination)))
  (let* ((mci (method-combination-info symbol)))
    (if mci
      (documentation mci doc-type))))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'method-combination)))
  (let* ((mci (method-combination-info symbol)))
    (if mci
      (setf (documentation mci doc-type) new))))


(defmethod documentation ((symbol symbol) (doc-type (eql 'structure)))
  (let* ((class (find-class symbol nil)))
    (if (typep class 'structure-class)
      (documentation class 'type)
      (call-next-method))))

(defmethod (setf documentation) ((new t)
				 (symbol symbol)
				 (doc-type (eql 'structure)))
  (let* ((class (find-class symbol nil)))
    (if (typep class 'structure-class)
      (setf (documentation class 'type) new)
      (call-next-method))))

(defmethod documentation ((p package) (doc-type (eql 't)))
  (call-next-method))

(defmethod (setf documentation) ((new t) (p package) (doc-type (eql 't)))
  (call-next-method))

(defmethod documentation ((f function) (doc-type (eql 't)))
  (call-next-method))

(defmethod (setf documentation) ((new t) (f function) (doc-type (eql 't)))
  (call-next-method))

(defmethod documentation ((f function) (doc-type (eql 'function)))
  (documentation f t))

(defmethod (setf documentation) ((new t)
				 (f function)
				 (doc-type (eql 'function)))
  (setf (documentation f t) new))

(defmethod documentation ((l cons) (doc-type (eql 'function)))
  (let* ((name (setf-function-spec-name l)))
    (if name
      (documentation name doc-type)
      (%get-documentation l doc-type))))

(defmethod (setf documentation) ((new t) (l cons) (doc-type (eql 'function)))
  (let* ((name  (setf-function-spec-name l)))
    (if name
      (setf (documentation name doc-type) new)
      (%put-documentation l doc-type new))))


(defmethod documentation ((l cons) (doc-type (eql 'compiler-macro)))
  (let* ((name (setf-function-spec-name l)))
    (if name
      (documentation name doc-type)
      (%get-documentation l doc-type))))

(defmethod (setf documentation) ((new t) (l cons) (doc-type (eql 'compiler-macr0)))
  (let* ((name (setf-function-spec-name l)))
    (if name
      (setf (documentation name doc-type) new)
      (%put-documentation l doc-type new))))


(defmethod documentation ((m method-combination)
			  (doc-type (eql 'method-combination)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (m method-combination)
				 (doc-type (eql 'method-combination)))
  (call-next-method))

(defmethod documentation ((m method-combination)
			  (doc-type (eql t)))
  (documentation m 'method-combination))

(defmethod (setf documentation) ((new t)
				 (m method-combination)
				 (doc-type (eql t)))
  (setf (documentation m 'method-combination) new))

(defmethod documentation ((m standard-method)
			  (doc-type (eql t)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (m standard-method)
				 (doc-type (eql t)))
  (call-next-method))

(defmethod documentation ((c standard-class) (doc-type (eql 'type)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (c standard-class)
				 (doc-type (eql 'type)))
  (call-next-method))

(defmethod documentation ((c standard-class) (doc-type (eql 't)))
  (documentation c 'type))

(defmethod (setf documentation) ((new t)
				 (c standard-class)
				 (doc-type (eql 't)))
  (setf (documentation c 'type) new))

(defmethod documentation ((c structure-class) (doc-type (eql 'type)))
  (call-next-method))

(defmethod (setf documentation) ((new t)
				 (c structure-class)
				 (doc-type (eql 'type)))
  (call-next-method))

(defmethod documentation ((c structure-class) (doc-type (eql 't)))
  (documentation c 'type))

(defmethod (setf documentation) ((new t)
				 (c structure-class)
				 (doc-type (eql 't)))
  (setf (documentation c 'type) new))

;;; This is now deprecated; things which call it should stop doing so.
(defun set-documentation (symbol doc-type string)
  (setf (documentation symbol doc-type) string))

(defun set-function-info (symbol info)
  (let* ((doc-string (if (consp info) (car info) info)))
    (if (and *save-doc-strings* (stringp doc-string))
      (set-documentation  symbol 'function doc-string)))
  (let* ((cons (assq symbol *nx-globally-inline*))
         (lambda-expression (if (consp info) (cdr info))))
    (if (and (proclaimed-inline-p symbol)
             (not (compiler-special-form-p symbol))
             (lambda-expression-p lambda-expression)
             (let* ((lambda-list (cadr lambda-expression)))
               (and (not (memq '&lap lambda-list))
                    (not (memq '&method lambda-list))
                    (not (memq '&lexpr lambda-list)))))
      (if cons 
        (%rplacd cons lambda-expression)
        (push (cons symbol lambda-expression) *nx-globally-inline*))
      (if cons (setq *nx-globally-inline* (delete cons *nx-globally-inline*)))))
  symbol)


(setf (documentation 'if 'function)
      "If Predicate Then [Else]
  If Predicate evaluates to non-null, evaluate Then and returns its values,
  otherwise evaluate Else and return its values. Else defaults to NIL.")

(setf (documentation 'progn 'function)
      "progn form*
  Evaluates each FORM and returns the value(s) of the last FORM.")



#|
(setf (documentation 'car 'variable) "Preferred brand of automobile")
(documentation 'car 'variable)
(setf (documentation 'foo 'structure) "the structure is grand.")
(documentation 'foo 'structure)
(setf (documentation 'foo 'variable) "the metasyntactic remarker")
(documentation 'foo 'variable)
(setf (documentation 'foo 'obscure) "no one really knows what it means")
(documentation 'foo 'obscure)
(setf (documentation 'foo 'structure) "the structure is solid")
(documentation 'foo 'function)
|#



(defun report-time (form thunk)
  (flet ((integer-size-in-bytes (i)
           (if (typep i 'fixnum)
             0
             (* (logand (+ 2 (uvsize i)) (lognot 1)) 4))))
    (rlet ((start :rusage)
	   (stop :rusage)
	   (timediff :timeval))
      (let* ((initial-real-time (get-internal-real-time))
	     (initial-gc-time (gctime))
	     (initial-consed (total-bytes-allocated))           
	     (initial-overhead (integer-size-in-bytes initial-consed)))
	(%%rusage start)
	(multiple-value-prog1 (funcall thunk)
	  (%%rusage stop)	  
	  (let* ((s *trace-output*)
		 (new-consed (total-bytes-allocated))		     
		 (bytes-consed
		  (- new-consed (+ initial-overhead initial-consed)))
		 (elapsed-real-time
		  (- (get-internal-real-time) initial-real-time))
		 (elapsed-gc-time (- (gctime) initial-gc-time))
		 (elapsed-user-time
		  (progn
		    (%sub-timevals timediff
				   (pref stop :rusage.ru_utime)
				   (pref start :rusage.ru_utime))
		    (timeval->milliseconds timediff)))
		 (elapsed-system-time
		  (progn
		    (%sub-timevals timediff
				   (pref stop :rusage.ru_stime)
				   (pref start :rusage.ru_stime))
		    (timeval->milliseconds timediff)))
		 (elapsed-run-time (+ elapsed-user-time elapsed-system-time))
		 (elapsed-mf-time (- elapsed-real-time elapsed-run-time))
		 (elapsed-minor (- (pref stop :rusage.ru_minflt)
				   (pref start :rusage.ru_minflt)))
		 (elapsed-major (- (pref stop :rusage.ru_majflt)
				   (pref start :rusage.ru_majflt)))
		 (elapsed-swaps (- (pref stop :rusage.ru_nswap)
				   (pref start :rusage.ru_nswap))))
	    (format s "~&~S took ~:D milliseconds (~,3F seconds) to run."
		    form elapsed-real-time (/ elapsed-real-time internal-time-units-per-second))
	    (format s "~&Of that, ~:D milliseconds (~,3F seconds) were spent in user mode" elapsed-user-time (/ elapsed-user-time internal-time-units-per-second))
	    (format s "~&         ~:D milliseconds (~,3F seconds) were spent in system mode" elapsed-system-time (/ elapsed-system-time internal-time-units-per-second))
	    (when (> elapsed-mf-time 0)
	      (format s "~%         ~:D milliseconds (~,3F seconds) were spent executing other OS processes."
		      elapsed-mf-time (/ elapsed-mf-time internal-time-units-per-second)))
	    (unless (eql elapsed-gc-time 0)
	      (format s
		      "~%~:D milliseconds (~,3F seconds) was spent in GC."
		      elapsed-gc-time (/ elapsed-gc-time internal-time-units-per-second)))
	    (unless (eql 0 bytes-consed)
	      (format s "~% ~:D bytes of memory allocated." bytes-consed))
	    (when (or (> elapsed-minor 0)
		      (> elapsed-major 0)
		      (> elapsed-swaps 0))
	      (format s
		      "~% ~:D minor page faults, ~:D major page faults, ~:D swaps."
		      elapsed-minor elapsed-major elapsed-swaps)
	      (format s "~&"))))))))

;(rm:heading 2 "Other Environment Inquiries")


; site names and machine-instance is in the init file.

(defun add-feature (symbol)
  "Not CL but should be."
  (if (symbolp symbol)
      (if (not (memq symbol *features*))
          (setq *features* (cons symbol *features*)))))

; (dotimes (i 5000) (declare (fixnum i)) (add-feature 'junk))



; ed is a CL fn so it must get loaded before tools, but it
; should be able to take advantage of tools if tools are loaded. - what tools?



; Misc string functions


(defun string-left-trim (char-bag string &aux end)
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  left end."
  (setq string (string string))
  (setq end (length string))
  (do ((index 0 (%i+ index 1)))
      ((or (eq index end) (not (find (aref string index) char-bag)))
       (subseq string index end))))

(defun string-right-trim (char-bag string &aux end)
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  right end."
  (setq string (string string))
  (setq end (length string))
  (do ((index (%i- end 1) (%i- index 1)))
      ((or (%i< index 0) (not (find (aref string index) char-bag)))
       (subseq string 0 (%i+ index 1)))))

(defun string-trim (char-bag string &aux end)
  "Given a set of characters (a list or string) and a string, returns a
  copy of the string with the characters in the set removed from both
  ends."
  (setq string (string string))
  (setq end (length string))
  (let ((left-end) (right-end))
     (do ((index 0 (%i+ index 1)))
	 ((or (eq index end) (not (find (aref string index) char-bag)))
	  (setq left-end index)))
     (do ((index (%i- end 1) (%i- index 1)))
	 ((or (%i< index left-end) (not (find (aref string index) char-bag)))
	  (setq right-end index)))
      (subseq string left-end (%i+ right-end 1))))



(defun copy-symbol (symbol &optional (copy-props nil) &aux new-symbol def)
  "Make and return a new uninterned symbol with the same print name
  as SYMBOL. If COPY-PROPS is false, the new symbol is neither bound
  nor fbound and has no properties, else it has a copy of SYMBOL's
  function, value and property list."
  (setq new-symbol (make-symbol (symbol-name symbol)))
  (when copy-props
      (when (boundp symbol)
            (set new-symbol (symbol-value symbol)))
      (when (setq def (fboundp symbol))
            ;Shouldn't err out on macros/special forms.
            (%fhave new-symbol def))
      (set-symbol-plist new-symbol (copy-list (symbol-plist symbol))))
  new-symbol)


(defvar %gentemp-counter 0
  "Counter for generating unique GENTEMP symbols.")

(defun gentemp (&optional (prefix "T") (package *package*))
  "Creates a new symbol interned in package PACKAGE with the given PREFIX."
  (loop
    (let* ((new-pname (%str-cat (ensure-simple-string prefix) 
                                (%integer-to-string %gentemp-counter)))
           (sym (find-symbol new-pname package)))
      (if sym
        (setq %gentemp-counter (%i+ %gentemp-counter 1))
        (return (values (intern new-pname package))))))) ; 1 value.




(defun add-gc-hook (hook-function &optional (which-hook :pre-gc))
  (ecase which-hook
    (:pre-gc
     (pushnew hook-function *pre-gc-hook-list*)
     (setq *pre-gc-hook* #'(lambda ()
                             (dolist (hook *pre-gc-hook-list*)
                               (funcall hook)))))
    (:post-gc
     (pushnew hook-function *post-gc-hook-list*)
     (setq *post-gc-hook* #'(lambda ()
                             (dolist (hook *post-gc-hook-list*)
                               (funcall hook))))))
  hook-function)

(defun remove-gc-hook (hook-function &optional (which-hook :pre-gc))
  (ecase which-hook
    (:pre-gc
     (unless (setq *pre-gc-hook-list* (delq hook-function *pre-gc-hook-list*))
       (setq *pre-gc-hook* nil)))
    (:post-gc
     (unless (setq *post-gc-hook-list* (delq hook-function *post-gc-hook-list*))
       (setq *post-gc-hook* nil)))))






(defun find-method-by-names (name qualifiers specializers)
  (let ((gf (fboundp name)))
    (when gf
      (if (not (standard-generic-function-p gf))
        (error "~S is not a generic-function." gf)
        (let ((methods (%gf-methods gf)))
          (when methods
            (let* ((spec-len (length (%method-specializers (car methods))))
                   (new-specs (make-list spec-len :initial-element (find-class t))))
              (declare (dynamic-extent new-specs))
              (do ((specs specializers (cdr specs))
                   (nspecs new-specs (cdr nspecs)))
                  ((or (null specs) (null nspecs)))
                (let ((s (car specs)))
                  (rplaca nspecs (if (consp s) s (find-class s nil)))))
              (find-method gf qualifiers new-specs nil))))))))



(defun get-string-from-user (prompt)
  (with-terminal-input
      (format *query-io* "~&~a " prompt)
    (force-output *query-io*)
    (clear-input *query-io*)
    (values (read-line *query-io*))))


(defun select-item-from-list (list &key (window-title "Select one of the following")
				   (table-print-function #'prin1)
				   &allow-other-keys)
  (with-terminal-input
      (format *query-io* "~a:~%" window-title)
    (do* ((l list (cdr l))
	  (i 0 (1+ i))
	  (item (car l) (car l)))
	 ((null l))
      (declare (fixnum i))
      (format *query-io* "~&~d: " i)
      (funcall table-print-function item *query-io*)
      (loop
	  (fresh-line *query-io*)
	  (let* ((string (get-string-from-user "Selection:"))
		 (value (ignore-errors
			  (let* ((*package* *keyword-package*))
			    (read-from-string string nil)))))
	    (cond ((eq value :q) (throw :cancel t))
		  ((and (typep value 'unsigned-byte)
			(< value (length list)))
		   (return (nth value list)))))))))

;;; There should ideally be some way to override the UI (such as
;;; it is ...) here.
;;; More generally, this either
;;;   a) shouldn't exist, or
;;;   b) should do more sanity-checking
(defun choose-file-dialog (&key file-types (prompt "File name:"))
  (%choose-file-dialog t prompt file-types))

(defun choose-new-file-dialog (&key prompt)
  (%choose-file-dialog nil prompt nil))

(defun %choose-file-dialog (must-exist prompt file-types)
  (loop
      (let* ((namestring (get-string-from-user prompt))
	     (pathname (ignore-errors (pathname namestring)))
	     (exists (and pathname (probe-file pathname))))
	(when (and (if must-exist exists)
		   (or (null file-types)
		       (member (pathname-type pathname)
			       file-types :test #'equal)))
	  (return pathname))
	(if (not exists)
	  (format *query-io* "~&~s does not exist." namestring)
	  (format *query-io* "~&Type of ~s is not one of ~{~a~}"
		  namestring file-types)))))

(defparameter *overwrite-dialog-hook* nil)
(defun overwrite-dialog (filename prompt)
  (if *overwrite-dialog-hook*
    (funcall *overwrite-dialog-hook* filename prompt)
    t))

