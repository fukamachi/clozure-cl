; -*- Mode: Lisp; Package: CCL; -*-

(in-package "CCL")

(defvar *grep-program* "grep")

(defclass cocoa-edit-grep-line-request (ns:ns-object)
  ((file-id :foreign-type :int)
   (line-num :foreign-type :int))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithFile:line:
		((self cocoa-edit-grep-line-request) (file :int) (line :int))
  (#/init self)
  (setf (slot-value self 'file-id) file
	(slot-value self 'line-num) line)
  self)

(objc:defmethod (#/editGrepLine: :void)
    ((self hemlock-document-controller) request)
  (let* ((file (id-map-free-object *edit-definition-id-map* (slot-value request 'file-id)))
	 (line-num (slot-value request 'line-num))
	 (namestring (native-translated-namestring file))
	 (url (#/initFileURLWithPath:
	       (#/alloc ns:ns-url)
	       (%make-nsstring namestring)))
	 (document (#/openDocumentWithContentsOfURL:display:error:
		    self
		    url
		    nil
		    +null-ptr+)))
    (unless (%null-ptr-p document)
      (when (= (#/count (#/windowControllers document)) 0)
	(#/makeWindowControllers document))
      (let* ((buffer (hemlock-document-buffer document))
	     (hi::*current-buffer* buffer)
	     (hi::*buffer-gap-context* (hi::buffer-gap-context buffer)))
	(edit-grep-line-in-buffer line-num))
      (#/updateHemlockSelection (slot-value document 'textstorage))
      (#/showWindows document))))

(defun edit-grep-line-in-buffer (line-num)
  (let ((point (hi::current-point-collapsing-selection)))
    (hi::buffer-start point)
    (unless (hi::line-offset point line-num)
      (hi::buffer-end point))))

(defun request-edit-grep-line (line)
  (let* ((pos1 (position #\: line))
	 (pos2 (and pos1 (position #\: line :start (1+ pos1))))
	 (num (and pos2 (ignore-errors
			 (parse-integer line :start (1+ pos1) :end pos2
					:junk-allowed nil))))
	 (file (and num (subseq line 0 pos1))))
    (when file
      (let* ((request (make-instance 'cocoa-edit-grep-line-request
				     :with-file (assign-id-map-id *edit-definition-id-map* file)
				     :line num)))
	(#/performSelectorOnMainThread:withObject:waitUntilDone:
	 (#/sharedDocumentController ns:ns-document-controller)
	 (@selector #/editGrepLine:)
	 request
	 t)))))

(defun split-grep-lines (output)
  (loop with end = (length output)
    for start = 0 then (1+ pos)
    as pos = (or (position #\Newline output :start start :end end) end)
    when (< start pos) collect (subseq output start pos)
    while (< pos end)))

(defun grep (pattern directory &key ignore-case (include "*.lisp") (exclude "*~.lisp"))
  (with-output-to-string (stream)
    (let* ((proc (run-program *grep-program*
			      (nconc (and include (list "--include" include))
				     (and exclude (list "--exclude" exclude))
				     (and ignore-case (list "--ignore-case"))
				     (list "--recursive"
					   "--with-filename"
					   "--line-number"
					   "-e" pattern
					   (native-untranslated-namestring directory)))
			      :input nil
			      :output stream)))
      (multiple-value-bind (status exit-code) (external-process-status proc)
	(let ((output (get-output-stream-string stream)))
	  (if (and (eq :exited status) (= exit-code 0))
	      (make-instance 'sequence-window-controller
			     :sequence (split-grep-lines output)
			     :result-callback #'request-edit-grep-line
			     :display #'princ
			     :title (format nil "~a in ~a" pattern directory))
	      (hi:editor-error "Error in grep status ~s code ~s: ~a" status exit-code output)))))))


(hi:defhvar "Grep Directory"
  "The directory searched by \"Grep\".  NIL means to use the directory of the buffer."
  :value nil)

(hi:defcommand "Grep" (p)
  "Prompts for a pattern and invokes grep, searching recursively through .lisp
   files in \"Grep Directory\".
   With an argument, prompts for a directory to search, and sets \"Grep Directory\"
   for the next time."
  ""
  (let* ((default (make-pathname :name :unspecific
				 :type :unspecific
				 :defaults (or (hi:value hemlock::grep-directory)
					       (hi:buffer-pathname hi::*current-buffer*)
					       "ccl:")))
	 (directory (if p
			(setf (hi:value hemlock::grep-directory)
			      (hi:prompt-for-file :must-exist t
						  :default default
						  :default-string (namestring default)
						  :prompt "Directory: "))
			default))
	 (pattern (hi:prompt-for-string
		   :prompt "Pattern: "
		   :help "Pattern to search for")))
    (grep pattern directory)))
