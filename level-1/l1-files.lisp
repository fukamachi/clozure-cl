;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   Portions copyright (C) 2001 Clozure Associates
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

;; L1-files.lisp - Object oriented file stuff

(defconstant $paramErr -50)   ; put this with the rest when we find the rest

(defconstant pathname-case-type '(member :common :local :studly))
(defconstant pathname-arg-type '(or string pathname stream))

(defmacro signal-file-error (err-num &optional pathname &rest args)
  `(%signal-file-error ,err-num
    ,@(if pathname (list pathname))
              ,@(if args args)))

(defun %signal-file-error (err-num &optional pathname args)
  (declare (fixnum err-num))
  (let* ((err-code (logior (ash 2 16) (the fixnum (logand #xffff (the fixnum err-num))))))
    (funcall (if (< err-num 0) '%errno-disp '%err-disp)
	     err-code
	     pathname
	     args)))


(defvar %logical-host-translations% '())
(defvar *load-pathname* nil)
(defvar *load-truename* nil)


(defparameter *default-pathname-defaults* (%cons-pathname nil nil nil))

;Right now, the only way it's used is that an explicit ";" expands into it.
;Used to merge with it before going to ROM.  Might be worth to bring that back,
;it doesn't hurt anything if you don't set it.
;(defparameter *working-directory* (%cons-pathname nil nil nil))

;These come in useful...  We should use them consistently and then document them,
;thereby earning the eternal gratitude of any users who find themselves with a
;ton of "foo.CL" files...
(defparameter *.fasl-pathname* (%cons-pathname nil nil
					       #+linuxppc-target "pfsl"
					       #+darwinppc-target "dfsl"
					       #+sparc-target "sfsl"
					       #-(or sparc-target ppc-target) "fasl"))
(defparameter *.lisp-pathname* (%cons-pathname nil nil "lisp"))

(defun if-exists (if-exists filename &optional (prompt "Create ..."))
  (case if-exists
    (:error (signal-file-error (- #$EEXIST) filename))
    ((:dialog :new-version :rename) (overwrite-dialog filename prompt))
    ((nil) nil)
    ((:ignored :overwrite :append :supersede :rename-and-delete) filename)
    (t (report-bad-arg if-exists '(member :error :dialog nil :ignored :overwrite :append :supersede :rename-and-delete)))))

(defun if-does-not-exist (if-does-not-exist filename)
  (case if-does-not-exist 
    (:error (signal-file-error (- #$ENOENT) filename)) ; (%err-disp $err-no-file filename))
    (:create filename)
    ((nil) (return-from if-does-not-exist nil))
    (t (report-bad-arg if-does-not-exist '(member :error :create nil)))))


(defun native-translated-namestring (path)
  (let ((name (translated-namestring path)))
    ;; Check that no quoted /'s
    (when (%path-mem-last-quoted "/" name)
      (signal-file-error $xbadfilenamechar name #\/))
    ;; Check that no unquoted wildcards.
    (when (%path-mem-last "*" name)
      (signal-file-error $xillwild name))
    (namestring-unquote name)))

;; Reverse of above, take native namestring and make a Lisp pathname.
(defun native-to-pathname (name)
  (pathname (%path-std-quotes name nil "*")))

(defun native-to-directory-pathname (name)
  (make-directory-pathname  :device nil :directory (%path-std-quotes name nil "*")))

;;; Make a pathname which names the specified directory; use
;;; explict :NAME, :TYPE, and :VERSION components of NIL.
(defun make-directory-pathname (&key host device directory)
  (make-pathname :host host
		 :device device
		 :directory directory
                 :name nil
                 :type nil
                 :version nil))

		   

(defun namestring-unquote (name)
  (let ((esc *pathname-escape-character*))
    (if (position esc name)
      (multiple-value-bind (sstr start end) (get-sstring name)
	(let ((result (make-string (%i- end start) :element-type 'base-char))
	      (dest 0))
	  (loop
	    (let ((pos (or (position esc sstr :start start :end end) end)))
	      (while (%i< start pos)
		(setf (%schar result dest) (%schar sstr start)
		      start (%i+ start 1)
		      dest (%i+ dest 1)))
	      (when (eq pos end)
		(return nil))
	      (setq start (%i+ pos 1))))
	  (shrink-vector result dest)))
      name)))

(defun translated-namestring (path)
  (namestring (translate-logical-pathname (merge-pathnames path))))

(defun truename (path)
  (or (probe-file path)
      (signal-file-error $err-no-file path)))

(defun probe-file (path)
  (let* ((native (native-translated-namestring path))
	 (realpath (%realpath native))
	 (kind (if realpath (%unix-file-kind realpath))))
    ;; Darwin's #_realpath will happily return non-nil for
    ;; files that don't exist.  I don't think that
    ;; %UNIX-FILE-KIND would do so.
    (when kind
      (when (and realpath
		 (> (length native) 0)
		 (eq (aref native (1- (length native))) #\/))
	;; If started with a directory, return a directory (%realpath
	;; always strips off the trailing /).
	;; E.g. (truename "/foo/bar/") should return "/foo/bar/".
	(if (eq kind :directory)
	  (unless (eq (aref realpath (1- (length realpath))) #\/)
	    (setq realpath (%str-cat realpath "/")))
	  (setq realpath nil)))
      (when realpath
	(native-to-pathname realpath)))))

(defun cwd (path)  
  (multiple-value-bind (realpath kind) (%probe-file-x (native-translated-namestring path))
    (if kind
      (if (eq kind :directory)
	(let* ((error (%chdir realpath)))
	  (if (eql error 0)
	    (mac-default-directory)
	    (signal-file-error error path)))
	(error "~S is not a directory pathname." path))
      (error "Invalid pathname : ~s." path))))

(defun create-file (path &key (if-exists :error) (create-directory t))
  (native-to-pathname (%create-file path :if-exists if-exists
				      :create-directory create-directory)))
(defun %create-file (path &key
			 (if-exists :error)
			 (create-directory t))
  (when create-directory
    (create-directory path))
  (when (directory-pathname-p path)
    (return-from %create-file (probe-file-x path)))
  (assert (or (eql if-exists :overwrite) (not (probe-file path))) ()
	  "~s ~s not implemented yet" :if-exists if-exists)
  (let* ((unix-name (native-translated-namestring path))
	 (fd (fd-open unix-name (logior #$O_WRONLY #$O_CREAT #$O_TRUNC))))
    (if (< fd 0)
      (signal-file-error fd path)
      (fd-close fd))
    (%realpath unix-name)))


#|
(defstruct (pattern
	    (:constructor make-pattern (pieces)))
  (pieces nil :type list))

(defmethod make-load-form ((p pattern))
  (make-load-form-saving-slots p))

(defmethod print-object ((p pattern) stream)
  (print-unreadable-object (p stream :type t)
    (let* ((pieces (pattern-pieces p)))
      (if *print-pretty*
        (let ((*print-escape* t))
	  (pprint-fill stream pieces nil))
	(prin1 pieces stream)))))

(defvar *ignore-wildcards* nil)


(defun remove-backslashes (namestr start end)
  "Remove and occurences of \\ from the string because we've already
   checked for whatever they may have been backslashed."
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((result (make-string (- end start) :element-type 'base-char))
	 (dst 0)
	 (quoted nil))
    (do ((src start (1+ src)))
	((= src end))
      (cond (quoted
	     (setf (schar result dst) (schar namestr src))
	     (setf quoted nil)
	     (incf dst))
	    (t
	     (let ((char (schar namestr src)))
	       (cond ((char= char #\\)
		      (setq quoted t))
		     (t
		      (setf (schar result dst) char)
		      (incf dst)))))))
    (when quoted
      (error 'namestring-parse-error
	     :complaint "Backslash in bad place."
	     :namestring namestr
	     :offset (1- end)))
    (shrink-vector result dst)))

(defun maybe-make-pattern (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (if *ignore-wildcards*
      (subseq namestr start end)
      (let* ((pattern ()))
	(let* ((quoted nil)
	      (any-quotes nil)
	      (last-regular-char nil)
	      (index start))
	  (flet ((flush-pending-regulars ()
		   (when last-regular-char
		     (push (if any-quotes
			       (remove-backslashes namestr
						   last-regular-char
						   index)
			       (subseq namestr last-regular-char index)) 
			   pattern)
		     (setf any-quotes nil)
		     (setf last-regular-char nil))))
	    (loop
	      (when (>= index end)
		(return))
	      (let ((char (schar namestr index)))
		(cond (quoted
		       (incf index)
		       (setf quoted nil))
		      ((char= char #\\)
		       (setf quoted t)
		       (setf any-quotes t)
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index))
		      ((char= char #\?)
		       (flush-pending-regulars)
		       (push :single-char-wild pattern)
		       (incf index))
		      ((char= char #\*)
		       (flush-pending-regulars)
		       (push :multi-char-wild pattern)
		       (incf index))
		      ((char= char #\[)
		       (flush-pending-regulars)
		       (let ((close-bracket
			      (position #\] namestr :start index :end end)))
			 (unless close-bracket
			   (error 'namestring-parse-error
				  :complaint "``['' with no corresponding ``]''"
				  :namestring namestr
				  :offset index))
			 (push (list :character-set
					(subseq namestr
						(1+ index)
						close-bracket))
			  pattern)
			 (setf index (1+ close-bracket))))
		      (t
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index)))))
	    (flush-pending-regulars)))
	(cond ((null (setq pattern (nreverse pattern)))
	       "")
	      ((null (cdr pattern))
	       (let ((piece (first pattern)))
		 (typecase piece
		   ((member :multi-char-wild) :wild)
		   (simple-string piece)
		   (t
		    (make-pattern pattern)))))
	      (t
	       (make-pattern pattern))))))

(defun extract-name-type-and-version (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((last-dot (position #\. namestr :start (1+ start) :end end
			     :from-end t))
	 (second-to-last-dot (and last-dot
				  (position #\. namestr :start (1+ start)
					    :end last-dot :from-end t)))
	 (version nil))
    ;; If there is a second-to-last dot, check to see if there is a valid
    ;; version after the last dot.
    (when second-to-last-dot
      (cond ((and (= (+ last-dot 2) end)
		  (char= (schar namestr (1+ last-dot)) #\*))
	     (setf version :wild))
	    ((and (< (1+ last-dot) end)
		  (do ((index (1+ last-dot) (1+ index)))
		      ((= index end) t)
		    (unless (char<= #\0 (schar namestr index) #\9)
		      (return nil))))
	     (setf version
		   (parse-integer namestr :start (1+ last-dot) :end end)))
	    (t
	     (setf second-to-last-dot nil))))
    (cond (second-to-last-dot
	   (values (maybe-make-pattern namestr start second-to-last-dot)
		   (maybe-make-pattern namestr
				       (1+ second-to-last-dot)
				       last-dot)
		   version))
	  (last-dot
	   (values (maybe-make-pattern namestr start last-dot)
		   (maybe-make-pattern namestr (1+ last-dot) end)
		   version))
	  (t
	   (values (maybe-make-pattern namestr start end)
		   nil
		   version)))))

;;; Take a string and return a list of cons cells that mark the char
;;; separated subseq. The first value t if absolute directories location.
;;;
(defun split-at-slashes (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((absolute (and (/= start end)
		       (char= (schar namestr start) #\/))))
    (when absolute
      (incf start))
    ;; Next, split the remainder into slash seperated chunks.
    (let* ((pieces ()))
      (loop
	(let ((slash (position #\/ namestr :start start :end end)))
	  (push (cons start (or slash end)) pieces)
	  (unless slash
	    (return))
	  (setf start (1+ slash))))
      (values absolute (nreverse pieces)))))

(defun parse-unix-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (multiple-value-bind
      (absolute pieces)
      (split-at-slashes namestr start end)
    (multiple-value-bind
	  (name type)
	(let* ((tail (car (last pieces)))
	       (tail-start (car tail))
	       (tail-end (cdr tail)))
	  (unless (= tail-start tail-end)
	    (setf pieces (butlast pieces))
	    (extract-name-type-and-version namestr tail-start tail-end)))
      ;; Now we have everything we want.  So return it.
      (values nil ; no host for unix namestrings.
	      (let* ((dirs ()))
		(dolist (piece pieces)
		  (let* ((piece-start (car piece))
			 (piece-end (cdr piece)))
		    (unless (= piece-start piece-end)
		      (cond ((string= namestr ".." :start1 piece-start
				      :end1 piece-end)
			     (push :up dirs))
			    ((string= namestr "**" :start1 piece-start
				      :end1 piece-end)
			     (push :wild-inferiors dirs))
			    (t
			     (push (maybe-make-pattern namestr
						       piece-start
						       piece-end) 
			      dirs))))))
		(setq dirs (nreverse dirs))
		(cond (absolute
		       (cons :absolute dirs))
		      (dirs
		       (cons :relative dirs))
		      (t
		       nil)))
	      name
	      type
	      nil))))

(defun unparse-unix-piece (thing)
  (etypecase thing
    ((member :wild) "*")
    (simple-string
     (let* ((srclen (length thing))
	    (dstlen srclen))
       (dotimes (i srclen)
	 (case (schar thing i)
	   ((#\* #\? #\[)
	    (incf dstlen))))
       (let ((result (make-string dstlen :element-type 'base-char))
	     (dst 0))
	 (dotimes (src srclen)
	   (let ((char (schar thing src)))
	     (case char
	       ((#\* #\? #\[)
		(setf (schar result dst) #\\)
		(incf dst)))
	     (setf (schar result dst) char)
	     (incf dst)))
	 result)))
    (pattern
     (let* ((strings ()))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string
	    (push piece strings))
	   (symbol
	    (ecase piece
	      (:multi-char-wild
	       (push "*" strings))
	      (:single-char-wild
	       (push "?" strings))))
	   (cons
	    (case (car piece)
	      (:character-set
	       (push "[" strings)
	       (push (cdr piece) strings)
	       (push "]" strings))
	      (t
	       (error "Invalid pattern piece: ~S" piece))))))
       (apply #'concatenate
	      'simple-string
	      (nreverse strings))))))

(defun unparse-unix-directory-list (directory)
  (declare (type list directory))
  (let* ((pieces ()))
    (when directory
      (ecase (pop directory)
	(:absolute
	 (push "/" pieces))
	(:relative
	 ;; Nothing special.
	 ))
      (dolist (dir directory)
	(typecase dir
	  ((member :up)
	   (push "../" pieces))
	  ((member :back)
	   (error ":BACK cannot be represented in namestrings."))
	  ((member :wild-inferiors)
	   (push "**/" pieces))
	  ((or simple-string pattern)
	   (push (unparse-unix-piece dir) pieces)
	   (push "/" pieces))
	  (t
	   (error "Invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (nreverse pieces))))

(defun %pathname-host (pathname)
  (if (logical-pathname-p pathname)
      (%logical-pathname-host pathname)
      :unspecific))

(defun %pathname-version (pathname)
  (if (logical-pathname-p pathname)
      (%logical-pathname-version pathname)
      :newest))

(defun unparse-unix-file (pathname)
  (declare (type pathname pathname))
  (let* ((strings ()))
    (let* ((name (%pathname-name pathname))
	   (type (%pathname-type pathname))
	   (type-supplied (not (or (null type) (eq type :unspecific))))
	   (version (%pathname-version pathname))
	   (version-supplied (not (or (null version) (eq version :newest)))))
      (when name
	(push (unparse-unix-piece name) strings))
      (when type-supplied
	(push "." strings)
	(push (unparse-unix-piece type) strings))
      (when version-supplied
	(unless type-supplied
	  (error "Cannot specify the version without a type: ~S" pathname))
	(push (if (eq version :wild)
	     ".*"
	     (format nil ".~D" version)) 
	 strings)))
    (apply #'concatenate 'simple-string (nreverse strings))))

(defun unparse-unix-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
	       (unparse-unix-directory-list (%pathname-directory pathname))
	       (unparse-unix-file pathname)))

(defun unparse-logical-piece (thing)
  (etypecase thing
    (simple-string thing)
    (pattern
     (let* ((strings ()))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string (push piece strings))
	   (keyword
	    (cond ((eq piece :wild-inferiors)
		   (push "**" strings))
		  ((eq piece :multi-char-wild)
		   (push "*" strings))
		  (t (error "Invalid keyword: ~S" piece))))))
       (apply #'concatenate 'simple-string (nreverse strings))))))

(defun unparse-logical-directory (pathname)
  (declare (type pathname pathname))
  (let* ((pieces ()))
    (let ((directory (%pathname-directory pathname)))
      (when directory
	(ecase (pop directory)
	  (:absolute)	 ;; Nothing special.
	  (:relative (push ";" pieces)))
	(dolist (dir directory)
	  (cond ((or (stringp dir) (pattern-p dir))
		 (push (unparse-logical-piece dir) pieces)
		 (push ";" pieces))
		((eq dir :wild)
		 (push "*;" pieces))
		((eq dir :wild-inferiors)
		 (push "**;" pieces))
		(t
		 (error "Invalid directory component: ~S" dir))))))
    (apply #'concatenate 'simple-string (nreverse pieces))))

(defun unparse-logical-namestring (pathname)
  (declare (type logical-pathname pathname))
  (concatenate 'simple-string
	       (%pathname-host pathname) ":"
	       (unparse-logical-directory pathname)
	       (unparse-unix-file pathname)))

(defun maybe-make-logical-pattern (namestring chunks)
  (let* ((chunk (caar chunks)))
    (let* ((pattern ()))
      (let* ((last-pos 0)
	     (len (length chunk)))
	(declare (fixnum last-pos))
	(loop
	  (when (= last-pos len) (return))
	  (let ((pos (or (position #\* chunk :start last-pos) len)))
	    (if (= pos last-pos)
		(when pattern
		  (error 'namestring-parse-error
			 :complaint "Double asterisk inside of logical ~
				     word: ~S"
			 :arguments (list chunk)
			 :namestring namestring
			 :offset (+ (cdar chunks) pos)))
		(push (subseq chunk last-pos pos) pattern))
	    (if (= pos len)
		(return)
		(push :multi-char-wild pattern))
	    (setq last-pos (1+ pos)))))
      (setq pattern (nreverse pattern))
	(if (cdr pattern)
	    (make-pattern pattern)
	    (let ((x (car pattern)))
	      (if (eq x :multi-char-wild)
		  :wild
		  x))))))

(defun logical-chunkify (namestr start end)
  (let* ((chunks nil))
    (do ((i start (1+ i))
	 (prev 0))
	((= i end)
	 (when (> end prev)
	    (push (cons (subseq namestr prev end) prev) chunks)))
      (let ((ch (schar namestr i)))
	(unless (or (alpha-char-p ch) (digit-char-p ch)
		    (member ch '(#\- #\*)))
	  (when (> i prev)
	    (push (cons (subseq namestr prev i) prev) chunks))
	  (setq prev (1+ i))
	  (unless (member ch '(#\; #\: #\.))
	    (error 'namestring-parse-error
		   :complaint "Illegal character for logical pathname:~%  ~S"
		   :arguments (list ch)
		   :namestring namestr
		   :offset i))
	  (push (cons ch i) chunks))))
    (nreverse chunks)))

(defun find-logical-host (hostname &optional (errorp t))
  (let* ((found (%str-assoc hostname %logical-host-translations%)))
    (unless (or found (not errorp))
      (error "Logical host not yet defined: ~S" hostname))
    (car found)))

(defun parse-logical-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((directory ()))
    (let* ((host nil)
	   (name nil)
	   (type nil)
	   (version nil))
      (labels ((expecting (what chunks)
		 (unless (and chunks (simple-string-p (caar chunks)))
		   (error 'namestring-parse-error
			  :complaint "Expecting ~A, got ~:[nothing~;~S~] ."
			  :arguments (list what (caar chunks) (caar chunks))
			  :namestring namestr
			  :offset (if chunks (cdar chunks) end)))
		 (caar chunks))
	       (parse-host (chunks)
		 (case (caadr chunks)
		   (#\:
		    (setq host
			  (find-logical-host (expecting "a host name" chunks)))
		    (parse-relative (cddr chunks)))
		   (t
		    (parse-relative chunks))))
	       (parse-relative (chunks)
		 (case (caar chunks)
		   (#\;
		    (push :relative directory)
		    (parse-directory (cdr chunks)))
		   (t
		    (push :absolute directory) ; Assumption! Maybe revoked later.
		    (parse-directory chunks))))
	       (parse-directory (chunks)
		 (case (caadr chunks)
		   (#\;
		    (push
		     (let ((res (expecting "a directory name" chunks)))
		       (cond ((string= res "..") :up)
			     ((string= res "**") :wild-inferiors)
			     (t
			      (maybe-make-logical-pattern namestr chunks))))
		     directory)
		    (parse-directory (cddr chunks)))
		   (t
		    (parse-name chunks))))
	       (parse-name (chunks)
		 (when chunks
		   (expecting "a file name" chunks)
		   (setq name (maybe-make-logical-pattern namestr chunks))
		   (expecting-dot (cdr chunks))))
	       (expecting-dot (chunks)
		 (when chunks
		   (unless (eql (caar chunks) #\.)
		     (error 'namestring-parse-error
			    :complaint "Expecting a dot, got ~S."
			    :arguments (list (caar chunks))
			    :namestring namestr
			    :offset (cdar chunks)))
		   (if type
		     (parse-version (cdr chunks))
		     (parse-type (cdr chunks)))))
	       (parse-type (chunks)
		 (expecting "a file type" chunks)
		 (setq type (maybe-make-logical-pattern namestr chunks))
		 (expecting-dot (cdr chunks)))
	       (parse-version (chunks)
		 (let ((str (expecting "a positive integer, * or NEWEST"
				       chunks)))
		   (cond
		     ((string= str "*") (setq version :wild))
		     ((string= str "NEWEST") (setq version :newest))
		     (t
		      (multiple-value-bind
			  (res pos)
			  (parse-integer str :junk-allowed t)
			(unless (and res (plusp res))
			  (error 'namestring-parse-error
				 :complaint "Expected a positive integer, ~
    got ~S"
				 :arguments (list str)
				 :namestring namestr
				 :offset (+ pos (cdar chunks))))
			(setq version res)))))
		 (when (cdr chunks)
		   (error 'namestring-parse-error
			  :complaint "Extra stuff after end of file name."
			  :namestring namestr
			  :offset (cdadr chunks)))))
	(parse-host (logical-chunkify namestr start end)))
      (setq directory (nreverse directory))
      (values host
	      (and (not (equal directory '(:absolute)))directory)
	      name type version))))

(defun extract-logical-host-prefix (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end)
	   (values (or logical-host null)))
  (let ((colon-pos (position #\: namestr :start start :end end)))
    (if colon-pos
	(%str-assoc (subseq namestr start colon-pos)
		    %logical-host-translations%)
	nil)))

(defun %parse-namestring (namestr host defaults start end junk-allowed)
  (declare  (type string namestr)
	   (type index start) (type (or index null) end))
  (if junk-allowed
      (handler-case
	  (%parse-namestring namestr host defaults start end nil)
	(namestring-parse-error (condition)
	  (values nil (namestring-parse-error-offset condition))))
      (let* ((end (or end (length namestr)))
	     (logical-host (car (extract-logical-host-prefix namestr start end)))
	     (parse-host (or host logical-host (pathname-host defaults))))
	(unless parse-host
	  (error "When Host arg is not supplied, Defaults arg must ~
		  have a non-null PATHNAME-HOST."))
	(if (eq host :unspecific)
	    (setq host nil))
	(setq host (or host logical-host))
	(multiple-value-bind
	    (new-host directory name type version)
	    (if host
		(parse-logical-namestring namestr start end)
		(parse-unix-namestring namestr start end))
	  (when (and host new-host (not (equal new-host host)))
	    (error "Host in namestring: ~S~@
		    does not match explicit host argument: ~S"
		   (subseq namestr start end)
		   host))
	  (cons-pathname directory name type host version)))))


(defun parse-namestring (thing
			 &optional host (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  (declare (type pathname defaults)
	   (type index start)
	   (type (or index null) end)
	   (type (or t null) junk-allowed)
	   (values (or null pathname) (or null index)))
    (typecase thing
      (simple-string
       (%parse-namestring thing host defaults start end junk-allowed))
      (string
       (%parse-namestring (coerce thing 'simple-string)
			  host defaults start end junk-allowed))
      (pathname
       (let ((host (if host host (%pathname-host defaults))))
	 (unless (equal host (%pathname-host thing))
	   (error "Hosts do not match: ~S and ~S."
		  host (%pathname-host thing))))
       (values thing start))
      (stream
       (let ((name (stream-pathname thing)))
	 (unless name
	   (error "Can't figure out the file associated with stream:~%  ~S"
		  thing))
	 name))))

(defun namestring (path &key)
  (typecase path
    (logical-pathname (unparse-logical-namestring path))
    (pathname (unparse-unix-namestring path))
    (t (namestring (pathname path)))))

|#

;; The following assumptions are deeply embedded in all our pathname code:
;; (1) Non-logical pathname host is always :unspecific.
;; (2) Logical pathname host is never :unspecific.
;; (3) Logical pathname host can however be NIL, e.g. "foo;bar;baz".

(defun %pathname-host (pathname)
  (if (logical-pathname-p pathname)
      (%logical-pathname-host pathname)
      :unspecific))

(defun %pathname-version (pathname)
  (if (logical-pathname-p pathname)
      (%logical-pathname-version pathname)
      :newest))


(defun logical-pathname-p (thing) (istruct-typep thing 'logical-pathname))

(defun pathname-host (thing)  ; redefined later in this file
  (declare (ignore thing))
  :unspecific)

(defun pathname-version (thing)  ; redefined later in this file
  (declare (ignore thing))
  :unspecific)

(defmethod print-object ((pathname pathname) stream)
  (let ((flags (if (logical-pathname-p pathname) 4
                   (%i+ (if (eq (%pathname-type pathname) ':unspecific) 1 0)
                        (if (equal (%pathname-name pathname) "") 2 0))))
        (name (namestring pathname)))
    (if (and (not *print-readably*) (not *print-escape*))
      (write-string name stream)
      (progn
        (format stream (if (eql flags 0) "#P" "#~DP") flags)
        (write-escaped-string name stream #\")))))


(defun mac-default-directory ()
  (let* ((native-name (current-directory-name))
	 (len (length native-name)))
    (declare (fixnum len))
    (when (and (> len 1)
	       (not (eq #\/ (schar native-name (1- len)))))
      (setq native-name (%str-cat native-name "/")))
    (native-to-pathname native-name)))


; I thought I wanted to call this from elsewhere but perhaps not
(defun absolute-directory-list (dirlist)
  ; just make relative absolute and remove ups where possible
  (when (eq (car dirlist) :relative)
    (let ((default (mac-default-directory)) default-dir)
      (when default
        (setq default-dir (%pathname-directory default))
        (when default-dir
          (setq dirlist (append default-dir (cdr dirlist)))))))
  (when (memq :up dirlist)
    (setq dirlist (remove-up (copy-list dirlist))))
  dirlist)

; ? is (:absolute "a" :up "b") = (:absolute "b") - seems reasonable
; destructively mungs dir
(defun remove-up (dir)
  (let ((n 0)
        (last nil)
        (sub dir)
        has-abs kept-up)
    ;; from %std-directory-component we get dir with :relative/:absolute stripped
    (when (memq :up dir)
      (when (memq (car dir) '(:relative :absolute))
	(setq sub (cdr dir) n 1 has-abs t))
      (do () ((null sub))
	(cond ((eq (car sub) :up)
	       (cond ((or (eq n 0)
			  (and (stringp last)(string= last "**"))
			  (eq last :wild-inferiors)
			  kept-up
			  (and has-abs (eq n 1)))
		      ;; up after "**" stays, initial :up stays, how bout 2 :ups
		      (setq kept-up t)
		      )
		     ((eq n 1) (setq dir (cddr dir) kept-up nil n -1))
		     (t (rplacd (nthcdr (- n 2) dir) (cdr sub))
			(setq n (- n 2) kept-up nil))))
	      (t (setq kept-up nil)))
	(setq last (car sub)
	      n (1+ n) 
	      sub (cdr sub))))
    dir))

(defun namestring (path)
  (%str-cat (host-namestring path)
	    (directory-namestring path)
	    (file-namestring path)))

(defun host-namestring (path)
  (let ((host (pathname-host path)))
    (if (and host (neq host :unspecific)) (%str-cat host ":") "")))

(defun directory-namestring (path)
  (%directory-list-namestring (pathname-directory path)
			      (neq (pathname-host path) :unspecific)))

(defun %directory-list-namestring (list &optional logical-p)
  (if (null list)
    ""
    (let ((len (if (eq (car list) (if logical-p :relative :absolute)) 1 0))
          (type 'base-character)
          result)
      (declare (fixnum len)(optimize (speed 3)(safety 0)))
      (dolist (s (%cdr list))
        (case s
          (:wild (setq len (+ len 2)))
          (:wild-inferiors (setq len (+ len 3)))
          (:up (setq len (+ len 3)))
          (t ;This assumes that special chars in dir components are escaped,
	     ;otherwise would have to pre-scan for escapes here.
	   (setq len (+ len 1 (length s)))
	   (when (extended-string-p s) (setq type 'extended-character)))))
      (setq result
	    ;; Avoid an out-of-line make-string call during bootstrapping
	    (if (eq type 'base-character)
	      (make-string len :element-type 'base-character)
	      (make-string len :element-type type)))
      (let ((i 0)
            (sep (if logical-p #\; #\/)))
        (declare (fixnum i))
        (when (eq (%car list) (if logical-p :relative :absolute))
          (setf (%schar result 0) sep)
          (setq i 1))
        (dolist (s (%cdr list))
	  (case s
	    (:wild (setq s "*"))
	    (:wild-inferiors (setq s "**"))
	    ;; There is no :up in logical pathnames, so this must be native
	    (:up (setq s "..")))
	  (let ((len (length s)))
	    (declare (fixnum len))
	    (move-string-bytes s result 0 i len)
	    (setq i (+ i len)))
	  (setf (%schar result i) sep)
	  (setq i (1+ i))))
      result)))

(defun file-namestring (path)
  (let* ((name (pathname-name path))
         (type (pathname-type path))
         (version (pathname-version path)))
    (file-namestring-from-parts name type version)))

(defun file-namestring-from-parts (name type version)
  (when (eq version :unspecific) (setq version nil))
  (when (eq type :unspecific) (setq type nil))
  (%str-cat (case name
	      ((nil :unspecific) "")
	      (:wild "*")
	      (t name))
	    (if (or type version)
	      (%str-cat (case type
			  ((nil) ".")
			  (:wild ".*")
			  (t (%str-cat "." type)))
			(case version
			  ((nil) "")
			  (:newest ".newest")
			  (:wild ".*")
			  (t (%str-cat "." (if (fixnump version)
					     (%integer-to-string version)
					     version)))))
	      "")))

(defun enough-namestring (path &optional (defaults *default-pathname-defaults*))
  (if (null defaults)
    (namestring path)
      (let* ((dir (pathname-directory path))
             (nam (pathname-name path))
             (typ (pathname-type path))
	     (ver (pathname-version path))
             (host (pathname-host path))
	     (logical-p (neq host :unspecific))
             (default-dir (pathname-directory defaults)))
	;; enough-host-namestring
        (setq host (if (and host
			    (neq host :unspecific)
			    (not (equalp host (pathname-host defaults))))
                     (%str-cat host ":")
                     ""))
	;; enough-directory-namestring
        (cond ((equalp dir default-dir)
               (setq dir '(:relative)))
              ((and dir default-dir
                    (eq (car dir) :absolute) (eq (car default-dir) :absolute))
               ; maybe make it relative to defaults
	       (do ((p1 (cdr dir) (cdr p1))
		    (p2 (cdr default-dir) (cdr p2)))
		   ((or (null p2) (null p1) (not (equalp (car p1) (car p2))))
		    (when (and (null p2) (neq p1 (cdr dir)))
		      (setq dir (cons :relative p1)))))))
	(setq dir (%directory-list-namestring dir logical-p))
	;; enough-file-namestring
	(when (equalp ver (pathname-version defaults))
	  (setq ver nil))
	(when (and (null ver) (equalp typ (pathname-type defaults)))
	  (setq typ nil))
	(when (and (null typ) (equalp nam (pathname-name defaults)))
	  (setq nam nil))
	(setq nam (file-namestring-from-parts nam typ ver))
	(%str-cat host dir nam))))

(defun cons-pathname (dir name type &optional host version)
  (if (neq host :unspecific)
    (%cons-logical-pathname dir name type host version)
    (%cons-pathname dir name type)))

(defun pathname (path)
  (etypecase path
    (pathname path)
    (stream (%path-from-stream path))
    (string (string-to-pathname path))))

(defun %path-from-stream (stream)
  (or (stream-filename stream) (error "Can't determine pathname of ~S ." stream)))      ; ???

;Like (pathname stream) except returns NIL rather than error when there's no
;filename associated with the stream.
(defun stream-pathname (stream &aux (path (stream-filename stream)))
  (when path (pathname path)))

(defun string-to-pathname (string &optional (start 0) (end (length string))
                                            (reference-host nil)
                                            (defaults *default-pathname-defaults*))
  (require-type reference-host '(or null string))
  (multiple-value-bind (sstr start end) (get-sstring string start end)
    (let (directory name type host version (start-pos start) (end-pos end) has-slashes)
      (multiple-value-setq (host start-pos has-slashes) (pathname-host-sstr sstr start-pos end-pos))
      (cond ((and host (neq host :unspecific))
             (when (and reference-host (not (string-equal reference-host host)))
               (error "Host in ~S does not match requested host ~S"
                      (%substr sstr start end) reference-host)))
            ((or reference-host
		 (and defaults
		      (neq (setq reference-host (pathname-host defaults)) :unspecific)))
	     ;;If either a reference-host is specified or defaults is a logical pathname
	     ;; then the string must be interpreted as a logical pathname.
	     (when has-slashes
	       (error "Illegal logical namestring ~S" (%substr sstr start end)))
             (setq host reference-host)))
      (multiple-value-setq (directory start-pos) (pathname-directory-sstr sstr start-pos end-pos host))
      (unless (eq host :unspecific)
	(multiple-value-setq (version end-pos) (pathname-version-sstr sstr start-pos end-pos)))
      (multiple-value-setq (type end-pos) (pathname-type-sstr sstr start-pos end-pos))
      ; now everything else is the name
      (unless (eq start-pos end-pos)
        (setq name (%std-name-component (%substr sstr start-pos end-pos))))
      (if (eq host :unspecific)
	(%cons-pathname directory name type)
        (%cons-logical-pathname directory name type host version)))))

(defun parse-namestring (thing &optional host (defaults *default-pathname-defaults*)
                               &key (start 0) end junk-allowed)
  (declare (ignore junk-allowed))
  (unless (typep thing 'string)
    (let* ((path (pathname thing))
	   (pathname-host (pathname-host path)))
      (when (and host pathname-host
		 (or (eq pathname-host :unspecific) ;physical
		     (not (string-equal host pathname-host))))
	(error "Host in ~S does not match requested host ~S" path host))
      (return-from parse-namestring (values path start))))
  (when host
    (verify-logical-host-name host))
  (setq end (check-sequence-bounds thing start end))
  (values (string-to-pathname thing start end host defaults) end))



(defun make-pathname (&key (host nil host-p) 
                           device
                           (directory nil directory-p)
                           (name nil name-p)
                           (type nil type-p)
                           (version nil version-p)
                           (defaults nil defaults-p) case
                           &aux path)
  (declare (ignore device))
  (when case (setq case (require-type case pathname-case-type)))
  (if (null host-p)
    (let ((defaulted-defaults (if defaults-p defaults *default-pathname-defaults*)))
      (setq host (if defaulted-defaults
		   (pathname-host defaulted-defaults)
		   :unspecific)))
    (unless host (setq host :unspecific)))
  (if directory-p 
    (setq directory (%std-directory-component directory host)))
  (if (and defaults (not directory))
    (setq directory (pathname-directory defaults)))
  (setq name
        (if name-p
             (%std-name-component name)
             (and defaults (pathname-name defaults))))
  (setq type
        (if type-p
             (%std-type-component type)
             (and defaults (pathname-type defaults))))
  (setq version (if version-p
                  (%logical-version-component version)
		  (if name-p
		    nil
		    (and defaults (pathname-version defaults)))))
  (setq path
        (if (eq host :unspecific)
          (%cons-pathname directory name type)
          (%cons-logical-pathname
	   (or directory
	       (unless directory-p '(:absolute)))
	   name type host version)))
  (when (and case (neq case :local))
    (setf (%pathname-directory path) (%reverse-component-case (%pathname-directory path) case)
          (%pathname-name path) (%reverse-component-case (%pathname-name path) case)
          (%pathname-type path) (%reverse-component-case (%pathname-type path) case)))
  path)

;  In portable CL, if the :directory argument to make pathname is a string, it should
;  be the name of a top-level directory and should not contain any punctuation characters
;  such as "/" or ";".  In MCL a string :directory argument with slashes or semi-colons
;  will be parsed as a directory in the obvious way.
(defun %std-directory-component (directory host)
  (cond ((null directory) nil)
        ((eq directory :wild) '(:absolute :wild-inferiors))
        ((stringp directory) (%directory-string-list directory 0 (length directory) host))
        ((listp directory)
         ;Standardize the directory list, taking care not to cons if nothing
         ;needs to be changed.
         (let ((names (%cdr directory)) (new-names ()))
           (do ((nn names (%cdr nn)))
               ((null nn) (setq new-names (if new-names (nreverse new-names) names)))
             (let* ((name (car nn))
                    (new-name (%std-directory-part name)))
               (unless (eq name new-name)
                 (unless new-names
                   (do ((new-nn names (%cdr new-nn)))
                       ((eq new-nn nn))
                     (push (%car new-nn) new-names))))
               (when (or new-names (neq name new-name))
                 (push new-name new-names))))
           (when (memq :up (or new-names names))
             (setq new-names (remove-up (copy-list (or new-names names)))))
           (ecase (%car directory)
             (:relative           
                  (cond (new-names         ; Just (:relative) is the same as NIL. - no it isnt
                         (if (eq new-names names)
                           directory
                           (cons ':relative new-names)))
                        (t directory)))
             (:absolute
                  (cond ((null new-names) directory)  ; But just (:absolute) IS the same as NIL
                        ((eq new-names names) directory)
                        (t (cons ':absolute new-names)))))))
        (t (report-bad-arg directory '(or string list (member :wild))))))

(defun %std-directory-part (name)
  (case name
    ((:wild :wild-inferiors :up) name)
    (:back :up)
    (t (cond ((string= name "*") :wild)
             ((string= name "**") :wild-inferiors)
	     ((string= name "..") :up)
             (t (%path-std-quotes name "/:;*" "/:;"))))))

; this will allow creation of garbage pathname "foo:bar;bas:" do we care?
(defun merge-pathnames (path &optional (defaults *default-pathname-defaults*)
                                       (default-version :newest))
  ;(declare (ignore default-version))
  (when (not (pathnamep path))(setq path (pathname path)))
  (when (and defaults (not (pathnamep defaults)))(setq defaults (pathname defaults)))
  (let* ((path-dir (pathname-directory path))
         (path-host (pathname-host path))
         (path-name (pathname-name path))
	 (path-type (pathname-type path))
         (default-dir (and defaults (pathname-directory defaults)))
         (default-host (and defaults (pathname-host defaults)))
         ; take host from defaults iff path-dir is logical or absent - huh? 
         (host (cond ((or (null path-host)  ; added 7/96
                          (and (eq path-host :unspecific)
                               (or (null path-dir)
                                   (null (cdr path-dir))
                                   (and (eq :relative (car path-dir))
                                        (not (memq default-host '(nil :unspecific)))))))
                          
                      default-host)
                     (t  path-host)))
         (dir (cond ((null path-dir) default-dir)
                    ((null default-dir) path-dir)
                    ((eq (car path-dir) ':relative)
                     (let ((the-dir (append default-dir (%cdr path-dir))))
                       (when (memq ':up the-dir)(setq the-dir (remove-up (copy-list the-dir))))
                       the-dir))
                    (t path-dir)))
         (nam (or path-name
                  (and defaults (pathname-name defaults))))
         (typ (or path-type
                  (and defaults (pathname-type defaults))))
         (version (or (pathname-version path)
		      (cond ((not path-name)
			     (and defaults (pathname-version defaults)))
			    (t default-version)))))
    (if (and (pathnamep path)
             (eq dir (%pathname-directory path))
             (eq nam path-name)
             (eq typ (%pathname-type path))
             (eq host path-host)
             (eq version (pathname-version path)))
      path 
      (cons-pathname dir nam typ host version))))

(defun directory-pathname-p (path)
  (let ((name (pathname-name path))(type (pathname-type path)))
    (and  (or (null name) (eq name :unspecific) (%izerop (length name)))
          (or (null type) (eq type :unspecific)))))

;In CCL, a pathname is logical if and only if pathname-host is not :unspecific.
(defun pathname-host (thing &key case)
  (when (streamp thing)(setq thing (%path-from-stream thing)))
  (when case (setq case (require-type case pathname-case-type)))
  (let ((name
         (typecase thing    
           (logical-pathname (%logical-pathname-host thing))
           (pathname :unspecific)
           (string (multiple-value-bind (sstr start end) (get-sstring thing) 
                     (pathname-host-sstr sstr start end)))
           (t (report-bad-arg thing pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) (neq name :unspecific)) (setq case :logical))
	(%reverse-component-case name case))
      name)))

(defun pathname-host-sstr (sstr start end &optional no-check)
  ;; A pathname with any (unescaped) /'s is always a physical pathname.
  ;; Otherwise, if the pathname has either a : or a ;, then it's always logical.
  ;; Otherwise, it's probably physical.
  ;; Return :unspecific for physical, host string or nil for a logical.
  (let* ((slash (%path-mem "/" sstr start end))
	 (pos (and (not slash) (%path-mem ":;" sstr start end)))
	 (pos-char (and pos (%schar sstr pos)))
	 (host (and (eql pos-char #\:) (%substr sstr start pos))))
    (cond (host
	   (unless (or no-check (logical-host-p host))
	     (error "~S is not a defined logical host" host))
	   (values host (%i+ pos 1) nil))
	  ((eql pos-char #\;) ; logical pathname with missing host
	   (values nil start nil))
	  (t ;else a physical pathname.
	   (values :unspecific start slash)))))


(defun pathname-device (thing &key case)
  (declare (ignore case thing))
  :unspecific)


;A directory is either NIL or a (possibly wildcarded) string ending in "/" or ";"
;Quoted /'s are allowed at this stage, though will get an error when go to the
;filesystem.
(defun pathname-directory (path &key case)
  (when (streamp path) (setq path (%path-from-stream path)))
  (when case (setq case (require-type case pathname-case-type)))
  (let* ((logical-p nil)
	 (names (typecase path
		  (logical-pathname (setq logical-p t) (%pathname-directory path))
		  (pathname (%pathname-directory path))
		  (string
		   (multiple-value-bind (sstr start end) (get-sstring path)
		     (multiple-value-bind (host pos2) (pathname-host-sstr sstr start end)
		       (unless (eq host :unspecific) (setq logical-p t))
                      (pathname-directory-sstr sstr pos2 end host))))
		  (t (report-bad-arg path pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) logical-p) (setq case :logical))
	(%reverse-component-case names case))
      names)))

;; Must match pathname-directory-end below
(defun pathname-directory-sstr (sstr start end host)
  (let ((pos (%path-mem-last (if (eq host :unspecific) "/" ";") sstr start end)))
    (if pos
      (values 
       (%directory-string-list sstr start (setq pos (%i+ pos 1)) host)
       pos)
      (values (and (neq host :unspecific)
		   (neq start end)
		   '(:absolute))
	      start))))

;; Must match pathname-directory-sstr above
(defun pathname-directory-end (sstr start end)
  (multiple-value-bind (host pos2) (pathname-host-sstr sstr start end)
    (let ((pos (%path-mem-last (if (eq host :unspecific) "/" ";") sstr pos2 end)))
      (if pos
	(values (%i+ pos 1) host)
	(values pos2 host)))))

(defun %directory-string-list (sstr start &optional (end (length sstr)) host)
  ;; Should use host to split by / vs. ; but for now suport both for either host,
  ;; like the mac version. It means that ';' has to be quoted in unix pathnames.
  (declare (ignore host))
  ;This must cons up a fresh list, %expand-logical-directory rplacd's it.
  (labels ((std-part (sstr start end)
             (%std-directory-part (if (and (eq start 0) (eq end (length sstr)))
                                    sstr (%substr sstr start end))))
           (split (sstr start end)
	     (unless (eql start end)
	       (let ((pos (%path-mem "/;" sstr start end)))
		 (if (eq pos start)
		   (split sstr (%i+ start 1) end) ;; treat multiple ////'s as one.
                   (cons (std-part sstr start (or pos end))
                         (when pos
                           (split sstr (%i+ pos 1) end))))))))
    (unless (eq start end)
      (let* ((slash-pos (%path-mem "/" sstr start end))
	     (semi-pos (%path-mem ";" sstr start end))
	     (pos (or slash-pos semi-pos)))
	; this never did anything sensible but did not signal an error
        (when (and slash-pos semi-pos)
	  (error "Illegal directory string ~s" (%substr sstr start end)))
        (if (null pos)
	  (list :relative (std-part sstr start end))
	  (let ((pos-char (%schar sstr pos)))
	    (cons (if (eq pos start)
		    (if (eq pos-char #\/) ':absolute ':relative)
		    (if (eq pos-char #\/) ':relative ':absolute))
		  (split sstr start end))))))))

(defun pathname-version (path)
  (when (streamp path) (setq path (%path-from-stream path)))
  (typecase path
    (logical-pathname (%logical-pathname-version path))
    (pathname :unspecific)
    (string
     (multiple-value-bind (sstr start end) (get-sstring path)
       (multiple-value-bind (newstart host) (pathname-directory-end sstr start end)
	 (if (eq host :unspecific)
	   :unspecific
	   (pathname-version-sstr sstr newstart end)))))
    (t (report-bad-arg path pathname-arg-type))))

(defun pathname-version-sstr (sstr start end)
  (declare (fixnum start end))
  (let ((pos (%path-mem-last "." sstr start end)))
    (if (and pos (%i> pos start) (%path-mem "." sstr start (%i- pos 1)))
      (values (%std-version-component (%substr sstr (%i+ pos 1) end)) pos)
      (values nil end))))

(defun %std-version-component (v)
  (cond ((or (null v) (eq v :unspecific)) v)
	((eq v :wild) "*")
	((string= v "") :unspecific)
	((string-equal v "newest") :newest)
	((every #'digit-char-p v) (parse-integer v))
	(t (%path-std-quotes v "./:;*" "./:;"))))


;A name is either NIL or a (possibly wildcarded, possibly empty) string.
;Quoted /'s are allowed at this stage, though will get an error if go to the
;filesystem.
(defun pathname-name (path &key case)
  (when (streamp path) (setq path (%path-from-stream path)))
  (when case (setq case (require-type case pathname-case-type)))
  (let* ((logical-p nil)
	 (name (typecase path
		 (logical-pathname (setq logical-p t) (%pathname-name path))
		 (pathname (%pathname-name path))
		 (string
		  (multiple-value-bind (sstr start end) (get-sstring path)
		    (multiple-value-bind (newstart host) (pathname-directory-end sstr start end)
		      (setq start newstart)
		      (unless (eq host :unspecific)
			(setq logical-p t)
			(setq end (nth-value 1 (pathname-version-sstr sstr start end))))
		      ;; TODO: -->> Need to make an exception so that ".emacs" is name with no type.
		      ;;   -->> Need to make an exception so that foo/.. is a directory pathname,
		      ;; for native.
		      (setq end (or (%path-mem-last "." sstr start end) end));; strip off type
		      (unless (eq start end)
			(%std-name-component (%substr sstr start end))))))
		 (t (report-bad-arg path pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) logical-p) (setq case :logical))
	(%reverse-component-case name case))
      name)))

(defun %std-name-component (name)
  (cond ((or (null name) (eq name :unspecific)) name)
        ((eq name :wild) "*")
        (t (%path-std-quotes name "/:;*" "/:;"))))

;A type is either NIL or a (possibly wildcarded, possibly empty) string.
;Quoted :'s are allowed at this stage, though will get an error if go to the
;filesystem.
(defun pathname-type (path &key case)
  (when (streamp path) (setq path (%path-from-stream path)))
  (when case (setq case (require-type case pathname-case-type)))
  (let* ((logical-p nil)
	 (name (typecase path
		 (logical-pathname (setq logical-p t) (%pathname-type path))
		 (pathname (%pathname-type path))
		 (string
		  (multiple-value-bind (sstr start end) (get-sstring path)
		    (multiple-value-bind (newstart host) (pathname-directory-end sstr start end)
		      (setq start newstart)
		      (unless (eq host :unspecific)
			(setq logical-p t)
			(setq end (nth-value 1 (pathname-version-sstr sstr start end))))
		      ;; TODO: -->> Need to make an exception so that ".emacs" is name with no type.
		      ;;   -->> Need to make an exception so that foo/.. is a directory pathname,
		      ;; for native.
		      (pathname-type-sstr sstr start end))))
		 (t (report-bad-arg path pathname-arg-type)))))
    (if (and case (neq case :local))
      (progn
	(when (and (eq case :common) logical-p) (setq case :logical))
	(%reverse-component-case name case))
      name)))

; assumes dir & version if any has been stripped away
(defun pathname-type-sstr (sstr start end)
  (let ((pos (%path-mem-last "." sstr start end)))
    (if pos
      (values (%std-type-component (%substr sstr (%i+ 1 pos) end)) pos)
      (values nil end))))

(defun %std-type-component (type)
  (cond ((or (null type) (eq type :unspecific)) type)
        ((eq type :wild) "*")
        (t (%path-std-quotes type "./:;*" "./:;"))))

(defun %std-name-and-type (native)
  (let* ((end (length native))
	 (pos (position #\. native :from-end t))
	 (type (and pos
		    (%path-std-quotes (%substr native (%i+ 1 pos) end)
				      nil "/:;*")))
	 (name (unless (eq (or pos end) 0)
		 (%path-std-quotes (if pos (%substr native 0 pos) native)
				   nil "/:;*"))))
    (values name type)))

(defun %reverse-component-case (name case)
  (cond ((not (stringp name))
         (if (listp name)
           (mapcar #'(lambda (name) (%reverse-component-case name case))  name)
           name))
        #+advanced-studlification-feature
        ((eq case :studly) (string-studlify name))
	((eq case :logical)
	 (if (every #'(lambda (ch) (not (lower-case-p ch))) name)
	   name
	   (string-upcase name)))
        (t ; like %read-idiocy but non-destructive - need it be?
         (let ((which nil)
               (len (length name)))
           (dotimes (i len)
             (let ((c (%schar name i)))
               (if (alpha-char-p c)
                 (if (upper-case-p c)
                   (progn
                     (when (eq which :lower)(return-from %reverse-component-case name))
                     (setq which :upper))
                   (progn
                     (when (eq which :upper)(return-from %reverse-component-case name))
                     (setq which :lower))))))
           (case which
             (:lower (string-upcase name))
             (:upper (string-downcase name))
             (t name))))))

;;;;;;; String-with-quotes utilities
(defun %path-mem-last-quoted (chars sstr &optional (start 0) (end (length sstr)))
  (while (%i< start end)
    (when (and (%%str-member (%schar sstr (setq end (%i- end 1))) chars)
               (%path-quoted-p sstr end start))
      (return-from %path-mem-last-quoted end))))

(defun %path-mem-last (chars sstr &optional (start 0) (end (length sstr)))
  (while (%i< start end)
    (when (and (%%str-member (%schar sstr (setq end (%i- end 1))) chars)
               (not (%path-quoted-p sstr end start)))
      (return-from %path-mem-last end))))

(defun %path-mem (chars sstr &optional (start 0) (end (length sstr)))
  (let ((one-char (when (eq (length chars) 1) (%schar chars 0))))
    (while (%i< start end)
      (let ((char (%schar sstr start)))
        (when (if one-char (eq char one-char)(%%str-member char chars))
          (return-from %path-mem start))
        (when (eq char *pathname-escape-character*)
          (setq start (%i+ start 1)))
        (setq start (%i+ start 1))))))

; these for \:  meaning this aint a logical host. Only legal for top level dir
 
(defun %path-unquote-one-quoted (chars sstr &optional (start 0)(end (length sstr)))
  (let ((pos (%path-mem-last-quoted chars sstr start end)))
    (when (and pos (neq pos 1))
      (cond ((or (%path-mem chars sstr start (1- pos))
                 (%path-mem-last-quoted chars sstr start (1- pos)))
             nil)
            (t (%str-cat (%substr sstr start (1- pos))(%substr sstr  pos end)))))))

(defun %path-one-quoted-p (chars sstr &optional (start 0)(end (length sstr)))
  (let ((pos (%path-mem-last-quoted chars sstr start end)))
    (when (and pos (neq pos 1))
      (not (or (%path-mem-last-quoted chars sstr start (1- pos))
               (%path-mem chars sstr start (1- pos)))))))
 
(defun %path-quoted-p (sstr pos start &aux (esc *pathname-escape-character*) (q nil))
  (while (and (%i> pos start) (eq (%schar sstr (setq pos (%i- pos 1))) esc))
    (setq q (not q)))
  q)



;Standardize pathname quoting, so can do EQUAL.
;; Note that this can't be used to remove quotes because it
;; always keeps the escape character quoted.
(defun %path-std-quotes (arg keep-quoted make-quoted)
  (when (symbolp arg)
    (error "Invalid pathname component ~S" arg))
  (let* ((str arg)
         (esc *pathname-escape-character*)
         (end (length str))
         res-str char)
    (multiple-value-bind (sstr start)(array-data-and-offset str)
      (setq end (+ start end))
      (let ((i start))
        (until (eq i end)
          (setq char (%schar sstr i))
          (cond ((or (%%str-member char make-quoted)
                     (and (null keep-quoted) (eq char esc)))
                 (unless res-str
                   (setq res-str (make-array (%i- end start)
                                             :element-type (array-element-type sstr)
                                             :adjustable t :fill-pointer 0))
                   (do ((j start (%i+ j 1))) ((eq j i))
                     (vector-push-extend (%schar sstr j) res-str)))
                 (vector-push-extend esc res-str))
                ((neq char esc) nil)
                ((eq (setq i (%i+ i 1)) end)
                 (error "Malformed pathname component string ~S" str))
                ((or (eq (setq char (%schar sstr i)) esc)
                     (%%str-member char keep-quoted))
                 (when res-str (vector-push-extend esc res-str)))
                (t
                 (unless res-str
                   (setq res-str (make-array (%i- end start)
                                             :element-type (array-element-type sstr)
                                             :adjustable t :fill-pointer 0))
                   (do ((j start (%i+ j 1)) (end (%i- i 1))) ((eq j end))
                     (vector-push-extend (%schar sstr j) res-str)))))
          (when res-str (vector-push-extend char res-str))
          (setq i (%i+ i 1)))
        (ensure-simple-string (or res-str str))))))



(defun %%str-member (char string)
  (locally (declare (optimize (speed 3)(safety 0)))
    (dotimes (i (the fixnum (length string)))
      (when (eq (%schar string i) char)
        (return i)))))


(defun file-write-date (path)
  (%file-write-date (native-translated-namestring path)))

(defun file-author (path)
  (%file-author (native-translated-namestring path)))

(defun touch (path)
  (if (not (probe-file path))
    (progn
      (ensure-directories-exist path)
      (if (or (pathname-name path)
              (pathname-type path))
        (create-file path)))
    (%utimes (native-translated-namestring path)))
  t)


;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
; load, require, provide

(defun find-load-file (file-name)
  (let ((full-name (full-pathname file-name :no-error nil)))
    (when full-name
      (let ((file-type (pathname-type full-name)))
        (if (and file-type (neq file-type :unspecific))
          (values (probe-file full-name) file-name file-name)
          (let* ((source (merge-pathnames file-name *.lisp-pathname*))
                 (fasl   (merge-pathnames file-name *.fasl-pathname*))
                 (true-source (probe-file source))
                 (true-fasl   (probe-file fasl)))
            (cond (true-source
                   (if (and true-fasl
                            (> (file-write-date true-fasl)
                               (file-write-date true-source)))
                     (values true-fasl fasl source)
                     (values true-source source source)))
                  (true-fasl
                   (values true-fasl fasl fasl))
                  ((setq full-name (probe-file full-name))
                   (values full-name file-name file-name)))))))))



(defun compile-load (file-name &rest options &key
                               fasl-file force-compile ignore-compiler-warnings
                               &allow-other-keys)
  (declare (dynamic-extent options))
  (remf options :fasl-file)
  (remf options :force-compile)
  (remf options :ignore-compiler-warnings)
  (let* ((file (merge-pathnames file-name *.lisp-pathname*))
         (fasl (merge-pathnames (if fasl-file (merge-pathnames fasl-file *.fasl-pathname*) *.fasl-pathname*)
                                file)))
    (when (probe-file file)
      (when (or force-compile
                (not (probe-file fasl))
                (< (file-write-date fasl) (file-write-date file)))
        (multiple-value-bind (compiled ignore bad-warnings)
                             (apply #'compile-file file :output-file fasl options)
          (declare (ignore ignore))
          (unless compiled              ; skip compile to compiler error
            (return-from compile-load nil))
          (unless (or ignore-compiler-warnings (not bad-warnings))
            (restart-case
              (error "Compiling ~s produced warnings." file)
              (retry-compile ()
                     :report (lambda (s) (format s "Retry compiling ~s" file))
                     (return-from compile-load (apply #'compile-load file-name options)))
              (load-anyway ()
                     :report (lambda (s) (format s "Load ~s despite compiler warnings" fasl)))
              (skip-load ()
                     :report (lambda (s) (format s "Skip loading ~s" fasl))
                     (return-from compile-load nil)))))))
    (if (probe-file fasl)
      (restart-case
        (apply #'load fasl options)
        (retry-compile ()
               :report (lambda (s) (format s "Retry compiling ~s" file))
               (apply #'compile-load file-name options))
        (skip-load ()
               :report (lambda (s) (format s "Skip loading ~s" fasl))
               nil))
      (restart-case
        (signal-file-error $err-no-file file)
        (retry-compile ()
               :report (lambda (s) (format s "Retry compiling ~s" file))
               (apply #'compile-load file-name options))))))

(defun load (file-name &key (verbose *load-verbose*)
                       (print *load-print*)
                       (if-does-not-exist :error)
		       (external-format :default))
  "Extension: :PRINT :SOURCE means print source as well as value"
  (loop
    (restart-case
      (return (%load file-name verbose print if-does-not-exist external-format))
      (retry-load ()
                  :report (lambda (stream) (format stream "Retry loading ~s" file-name)))
      (skip-load ()
                 :report (lambda (stream) (format stream "Skip loading ~s" file-name))
                 (return nil))
      (load-other ()
                  :report (lambda (stream) (format stream "Load other file instead of ~s" file-name))
                  (return
                   (load (choose-file-dialog)
                         :verbose verbose
                         :print print
                         :if-does-not-exist if-does-not-exist))))))


(defun %load (file-name verbose print if-does-not-exist external-format)
  (let ((*load-pathname* file-name)
        (*load-truename* file-name)
        (source-file file-name)
        constructed-source-file)
    (declare (special *load-pathname* *load-truename*))
    (unless (streamp file-name)
      (multiple-value-setq (*load-truename* *load-pathname* source-file)
        (find-load-file (merge-pathnames file-name)))
      (when (not *load-truename*)
        (return-from %load (if if-does-not-exist
                             (signal-file-error $err-no-file file-name))))
      (setq file-name *load-truename*))
    (let* ((*package* *package*)
           (*readtable* *readtable*)
           (*loading-files* (cons file-name (specialv *loading-files*)))
           (*loading-file-source-file* (namestring source-file))) ;reset by fasload to logical name stored in the file?
      (declare (special *loading-files* *loading-file-source-file*))
      (unwind-protect
        (progn
          (when verbose
            (format t "~&;Loading ~S..." *load-pathname*)
            (force-output))
          (cond ((fasl-file-p file-name)
                 (flet ((attempt-load (file-name)
                          (multiple-value-bind (winp err) 
                              (%fasload (native-translated-namestring file-name))
                            (if (not winp) 
                              (%err-disp err)))))
                   (let ((*fasload-print* print))
                     (declare (special *fasload-print*))
                     (setq constructed-source-file (make-pathname :defaults file-name :type (pathname-type *.lisp-pathname*)))
                     (when (equalp source-file *load-truename*)
                       (when (probe-file constructed-source-file)
                         (setq source-file constructed-source-file)))
                     (if (and source-file
                              (not (equalp source-file file-name))
                              (probe-file source-file))
                       ;;really need restart-case-if instead of duplicating code below
                       (restart-case
                         (attempt-load file-name)
                         #+ignore
                         (load-other () :report (lambda (x) (format s "load other file"))
                                     (return-from
                                       %load
                                       (%load (choose-file-dialog) verbose print if-does-not-exist)))
                         (load-source 
                          ()
                          :report (lambda (s) 
                                    (format s "Attempt to load ~s instead of ~s" 
                                            source-file *load-pathname*))
                          (return-from 
                            %load
                            (%load source-file verbose print if-does-not-exist  external-format))))
                       ;;duplicated code
                       (attempt-load file-name)))))
                (t 
                 (with-open-file (stream file-name
					 :element-type 'base-char
					 :external-format external-format)
                   (load-from-stream stream print))))))))
  file-name)

(defun load-from-stream (stream print &aux (eof-val (list ())) val)
  (with-compilation-unit (:override nil) ; try this for included files
    (let ((env (new-lexical-environment (new-definition-environment 'eval))))
      (%rplacd (defenv.type (lexenv.parent-env env)) *outstanding-deferred-warnings*)
      (while (neq eof-val (setq val (read stream nil eof-val)))
        (when (eq print :source) (format t "~&Source: ~S~%" val))
        (setq val (cheap-eval-in-environment val env))
        (when print
          (format t "~&~A~S~%" (if (eq print :source) "Value: " "") val))))))

(defun include (filename)
  (load
   (if (null *loading-files*)
     filename
     (merge-pathnames filename (directory-namestring (car *loading-files*))))))

(%fhave '%include #'include)

(defun delete-file (path)
  (let* ((namestring (native-translated-namestring path)))
    (when (%realpath namestring)
      (let* ((err (%delete-file namestring)))
        (or (eql 0 err) (signal-file-error err path))))))

(defvar *known-backends* ())

(defun fasl-file-p (pathname)
  (let* ((type (pathname-type pathname)))
    (or (and (null *known-backends*)
	     (equal type (pathname-type *.fasl-pathname*)))
	(dolist (b *known-backends*)
	  (when (equal type (pathname-type (backend-target-fasl-pathname b)))
	    (return t))))))

(defun provide (module &aux (str (string module)))
  (when (null module) (report-bad-arg module '(not (member nil))))
  (setq *modules* (adjoin str *modules* :test #'string-equal))
  (let* ((cell (assoc str *module-file-alist* :test #'string-equal))
         (path *loading-file-source-file*))
    (if cell
      (setf (cdr cell) path)
      (push (cons str path) *module-file-alist*)))
  str)

(defparameter *loading-modules* () "Internal. Prevents circularity")

(defun require (module &optional pathname &aux (str (string module)))
  (when (null module) (report-bad-arg module '(not null)))
  (when (and (not (member str *modules* :test #'string-equal))
             (not (member str *loading-modules* :test #'string-equal))
             (or pathname
                 (setq pathname (find-module-pathnames str))
                 (progn
                   (cerror "If ~S still hasn't been provided,
you will be asked to choose a file."
                           "The module ~S was required while loading ~S.
No file could be found for that module."
                           str *loading-file-source-file*)
                   (unless (member str *modules* :test #'string-equal)
                     (catch-cancel
                       (setq pathname (choose-file-dialog
                                       :file-types '(#+sparc-target "sfsl"
                                                     #+ppc-target "pfsl"
                                                     #-(or ppc-target sparc-target) "fasl"
                                                        "text")))))
                   pathname)))
    (let ((*loading-modules* (cons str *loading-modules*)))
      (if (consp pathname)
        (dolist (path pathname) (load path))
        (load pathname)))
    (setq *modules* (adjoin str *modules* :test #'string-equal)))
  str)

(defun find-module-pathnames (module)
  "Returns the file or list of files making up the module"
  (or (cdr (assoc module *module-file-alist* :test #'string-equal))
      (let ((mod-path (make-pathname :name (string-downcase module) :defaults nil)) path)
        (dolist (path-cand *module-search-path* nil)
          (when (setq path (find-load-file (merge-pathnames mod-path path-cand)))
            (return path))))))

