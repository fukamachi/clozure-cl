(in-package :asdf-install)

(pushnew :asdf-install *features*)

#+:digitool
(defparameter *home-volume-name*
  (second (pathname-directory (truename (user-homedir-pathname))))
  "Digitool MCL retains the OS 9 convention that ALL volumes have a
name which includes the startup volume. OS X doesn't know about this.
This figures in the home path and in the normalization for system
namestrings.")

(defvar *proxy* (get-env-var "http_proxy"))
(defvar *cclan-mirror*
  (or (get-env-var "CCLAN_MIRROR")
      "http://ftp.linux.org.uk/pub/lisp/cclan/"))

(defun directorify (name)
  ;; input name may or may not have a trailing #\/, but we know we
  ;; want a directory
  (let ((path (pathname name)))
    (if (pathname-name path)
	(merge-pathnames
	 (make-pathname :directory `(:relative ,(pathname-name path))
			:name "")
	 path)
	path)))

(defvar *sbcl-home* (directorify (or (get-env-var "SBCL_HOME")
                                     (get-env-var "ASDF_INSTALL_DIR")
                                     (make-pathname :directory
                                                    `(:absolute
                                                      #+digitool ,*home-volume-name*
                                                      "usr" "local" "asdf-install")))))

(defvar *dot-sbcl*
  #+:sbcl
  (merge-pathnames (make-pathname :directory '(:relative ".sbcl"))
		   (truename (user-homedir-pathname)))
  #-:sbcl
  (cond ((get-env-var "PRIVATE_ASDF_INSTALL_DIR")
          (directorify (get-env-var "PRIVATE_ASDF_INSTALL_DIR")))
        (t
          (merge-pathnames (make-pathname :directory '(:relative ".asdf-install-dir"))
                           (truename (user-homedir-pathname))))))

(defvar *trusted-uids* nil)

(defvar *verify-gpg-signatures* t)

(defvar *safe-url-prefixes* nil)

(defvar *preferred-location* nil)

(defun verify-gpg-signatures-p (url)
  (labels ((prefixp (prefix string)
	     (let ((m (mismatch prefix string)))
	       (or (not m) (>= m (length prefix))))))
    (case *verify-gpg-signatures*
      ((nil) nil)
      ((:unknown-locations)
       (notany
	(lambda (x) (prefixp x url))
	*safe-url-prefixes*))
      (t t))))
	  
(defvar *locations*
  `((,(merge-pathnames (make-pathname :directory '(:relative "site")) *sbcl-home*)
     ,(merge-pathnames (make-pathname :directory '(:relative "site-systems")) *sbcl-home*)
     "System-wide install")
    (,(merge-pathnames (make-pathname :directory '(:relative "site")) *dot-sbcl*)
     ,(merge-pathnames (make-pathname :directory '(:relative "systems")) *dot-sbcl*)
     "Personal installation")))

#-:sbcl
(pushnew `(merge-pathnames ,(make-pathname :directory '(:relative "site-systems")) ,*sbcl-home*)
         asdf:*central-registry*
         :test #'equal)

#-:sbcl
(pushnew `(merge-pathnames ,(make-pathname :directory '(:relative "systems")) ,*dot-sbcl*)
         asdf:*central-registry*
         :test #'equal)

(let* ((*package* (find-package :asdf-install-customize))
       (file (probe-file (merge-pathnames
			  (make-pathname :name ".asdf-install")
			  (truename (user-homedir-pathname))))))
  (when file (load file)))

(define-condition download-error (error)
  ((url :initarg :url :reader download-url)
   (response :initarg :response :reader download-response))
  (:report (lambda (c s)
	     (format s "Server responded ~A for GET ~A"
		     (download-response c) (download-url c)))))

(define-condition signature-error (error)
  ((cause :initarg :cause :reader signature-error-cause))
  (:report (lambda (c s)
	     (format s "Cannot verify package signature:  ~A"
		     (signature-error-cause c)))))

(define-condition gpg-error (error)
  ((message :initarg :message :reader gpg-error-message))
  (:report (lambda (c s)
	     (format s "GPG failed with error status:~%~S"
		     (gpg-error-message c)))))

(define-condition no-signature (gpg-error) ())
(define-condition key-not-found (gpg-error)
  ((key-id :initarg :key-id :reader key-id))
  (:report (lambda (c s)
	     (format s "No key found for key id 0x~A.  Try some command like ~%  gpg  --recv-keys 0x~A"
		     (key-id c) (key-id c)))))

(define-condition key-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "GPG warns that the key id 0x~A (~A) is not fully trusted"
		     (key-id c) (key-user-name c)))))
(define-condition author-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "~A (key id ~A) is not on your package supplier list"
		     (key-user-name c) (key-id c)))))
  
(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
	 (host-end (min (or (position #\/ url :start 7) (length url))
			(or port-start (length url)))))
    (subseq url 7 host-end)))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((port-start (position #\: url :start 7)))
    (if port-start (parse-integer url :start port-start :junk-allowed t) 80)))

(defun url-connection (url)
  (let ((stream (make-stream-from-url url))
        (host (url-host url)))
    (format stream "GET ~A HTTP/1.0~C~CHost: ~A~C~CCookie: CCLAN-SITE=~A~C~C~C~C"
            url #\Return #\Linefeed
            host #\Return #\Linefeed
            *cclan-mirror* #\Return #\Linefeed #\Return #\Linefeed)
    (force-output stream)
    (flet (#-:digitool
           (read-header-line ()
             (read-line stream))
           #+:digitool
           (read-header-line (&aux (line (make-array 16
                                                     :element-type 'character
                                                     :adjustable t
                                                     :fill-pointer 0))
                                   (byte nil))
             (print (multiple-value-bind (reader arg)
                        (ccl::stream-reader stream)
                      (loop (setf byte (funcall reader arg))
                            (case byte
                              ((nil)
                                (return))
                              ((#.(char-code #\Return)
                                  #.(char-code #\Linefeed))
                                (case (setf byte (funcall reader arg))
                                  ((nil #.(char-code #\Return) #.(char-code #\Linefeed)))
                                  (t (ccl:stream-untyi stream byte)))
                                (return))
                              (t
                                (vector-push-extend (code-char byte) line))))
                      (when (or byte (plusp (length line)))
                        line)))))
      (list
       (let* ((l (read-header-line))
              (space (position #\Space l)))
         (parse-integer l :start (1+ space) :junk-allowed t))
       (loop for line = (read-header-line)
             until (or (null line)
                       (zerop (length line))
                       (eql (elt line 0) (code-char 13)))
             collect
             (let ((colon (position #\: line)))
               (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                     (string-trim (list #\Space (code-char 13))
                                  (subseq line (1+ colon))))))
       stream))))

(defun download-files-for-package (package-name-or-url file-name)
  (let ((url
	 (if (= (mismatch package-name-or-url "http://") 7)
	     package-name-or-url
	     (format nil "http://www.cliki.net/~A?download"
		     package-name-or-url))))
    (destructuring-bind (response headers stream)
	(block got
	  (loop
	   (destructuring-bind (response headers stream) (url-connection url)
	     (unless (member response '(301 302))	       
	       (return-from got (list response headers stream)))
	     (close stream)
	     (setf url (cdr (assoc :location headers))))))
      (if (>= response 400)
	(error 'download-error :url url :response response))
      (let ((length (parse-integer
		     (or (cdr (assoc :content-length headers)) "")
		     :junk-allowed t)))
	(format t "Downloading ~A bytes from ~A to ~A ..."
		(or length "some unknown number of") url file-name)
	(force-output)
        #+:clisp (setf (stream-element-type stream)
                         '(unsigned-byte 8))
	(with-open-file (o file-name :direction :output
                                     #+(or :clisp :digitool (and :lispworks :win32))
                                     :element-type
                                     #+(or :clisp :digitool (and :lispworks :win32))
                                     '(unsigned-byte 8)
                                     :if-exists :supersede)
          #+(or :cmu :digitool)
          (copy-stream stream o)
          #-(or :cmu :digitool)
	  (if length
	      (let ((buf (make-array length
				     :element-type
				     (stream-element-type stream))))
		#-:clisp (read-sequence buf stream)
		#+:clisp (ext:read-byte-sequence buf stream :no-hang nil)
		(write-sequence buf o))
	      (copy-stream stream o))))
      (close stream)
      (terpri)
      (restart-case 
	  (verify-gpg-signature/url url file-name)
	(skip-gpg-check (&rest rest)
	  :report "Don't ckeck GPG signature for this package"
          (declare (ignore rest))
	  nil)))))

(defun read-until-eof (stream)
  (with-output-to-string (o)
    (copy-stream stream o)))
  
(defun verify-gpg-signature/string (string file-name)
  (let ((gpg-stream (make-stream-from-gpg-command string file-name))
        tags)
    (unwind-protect
      (loop for l = (read-line gpg-stream nil nil)
            while l
            do (print l)
            when (> (mismatch l "[GNUPG:]") 6)
            do (destructuring-bind (_ tag &rest data)
                   (asdf::split l)
	       (declare (ignore _))
               (pushnew (cons (intern tag :keyword)
			      data) tags)))
      (ignore-errors
        (close gpg-stream)))
    ;; test for obvious key/sig problems
    (let ((errsig (assoc :errsig tags)))
      (and errsig (error 'key-not-found :key-id (second errsig))))
    (let ((badsig (assoc :badsig tags)))
      (and badsig (error 'key-not-found :key-id (second badsig))))
    (let* ((good (assoc :goodsig tags))
	   (id (second good))
	   (name (format nil "~{~A~^ ~}" (nthcdr 2 good))))
      ;; good signature, but perhaps not trusted
      (unless (or (assoc :trust_ultimate tags)
		  (assoc :trust_fully tags))
	(cerror "Install the package anyway"
		'key-not-trusted
		:key-user-name name
		:key-id id))
      (loop
       (when
	   (restart-case
	       (or (assoc id *trusted-uids* :test #'equal)
		   (error 'author-not-trusted
			  :key-user-name name
			  :key-id id))
	     (add-key (&rest rest)
	       :report "Add to package supplier list"
               (declare (ignore rest))
	       (pushnew (list id name) *trusted-uids*)))
	 (return))))))

(defun verify-gpg-signature/url (url file-name)
  (when (verify-gpg-signatures-p url)
    (destructuring-bind (response headers stream)
        (url-connection (concatenate 'string url ".asc"))
      (unwind-protect
        (flet (#-:digitool
               (read-signature (data stream)
                 (read-sequence data stream))
               #+:digitool
               (read-signature (data stream)
                 (multiple-value-bind (reader arg)
                     (ccl:stream-reader stream)
                   (let ((byte 0))
                     (dotimes (i (length data))
                       (unless (setf byte (funcall reader arg))
                         (error 'download-error :url  (concatenate 'string url ".asc")
                                :response 200))
                       (setf (char data i) (code-char byte)))))))
          (if (= response 200)
            (let ((data (make-string (parse-integer
                                      (cdr (assoc :content-length headers))
                                      :junk-allowed t))))
              (read-signature data stream)
              (verify-gpg-signature/string data file-name))
            (error 'download-error :url  (concatenate 'string url ".asc")
                   :response response)))
        (close stream)))))

(defun where ()
  (let ((response (or *preferred-location*             
                      (progn
                        (format t "Install where?~%")
                        (loop for (source system name) in *locations*
                              for i from 1
                              do (format t "~A) ~A: ~%   System in ~A~%   Files in ~A ~%"
                                         i name system source))
                        (format t " --> ")
                        (force-output)
                        (read)))))
    (when (> response 0)
      (elt *locations* (1- response)))))

(defun install-package (source system packagename)
  "Returns a list of asdf system names for installed asdf systems"
  (ensure-directories-exist source)
  (ensure-directories-exist system)
  (let* ((tar
           (or
             #-(or :win32 :mswindows)
	     (return-output-from-program "tar"
                                         (list "-C" (system-namestring source)
                                               "-xzvf" (system-namestring packagename)))
             #+(or :win32 :mswindows)
	     (return-output-from-program "sh"
                                         (list "-c"
                                               (format nil "\"tar -C \\\"`cygpath '~A'`\\\" -xzvf \\\"`cygpath '~A'`\\\"\""
                                                       (namestring (truename source))
                                                       (namestring (truename packagename)))))
	     (error "can't untar")))
         (pos-slash (position-if #'(lambda (c)
                                     (find c #(#\/ #\Return #\Linefeed)))
                                 tar))
	 (*default-pathname-defaults*
	  (merge-pathnames
	   (make-pathname :directory
			  `(:relative ,(subseq tar 0 pos-slash)))
	   source)))
    (princ tar)
    (loop for asd in (directory
		      (make-pathname :defaults (print *default-pathname-defaults*)
                                     :name :wild :type "asd"))
          #-(or :win32 :mswindows)
          do
	  #-(or :win32 :mswindows)
          (let ((target (merge-pathnames
                         (make-pathname :name (pathname-name asd)
                                        :type (pathname-type asd))
                         system)))
            (when (probe-file target)
              (unlink-file target))
            (symlink-files asd target))
	  collect (pathname-name asd))))

(defun temp-file-name (p)
  (let* ((pos-slash (position #\/ p :from-end t))
	 (pos-dot (position #\. p :start (or pos-slash 0))))
    (merge-pathnames
     (make-pathname
      :name (subseq p (if pos-slash (1+ pos-slash) 0) pos-dot)
      :type "asdf-install-tmp")
     #+:clisp (user-homedir-pathname))))

;; this is the external entry point
(defun install (&rest packages)
  (let ((*temporary-files* nil)
	(*trusted-uids*
	 (let ((p (merge-pathnames "trusted-uids.lisp" *dot-sbcl*)))
	   (when (probe-file p)
	     (with-open-file (f p) (read f))))))
    (unwind-protect
	 (destructuring-bind (source system name) (where)
           (declare (ignore name))
	   (labels ((one-iter (packages)
		      (dolist (asd
				(loop for p in (mapcar 'string packages)
				      unless #+(or :sbcl :alisp)
                                             (probe-file p)
                                             #-(or :sbcl :alisp)
                                             (and (/= (mismatch p "http://") 7)
                                                  (probe-file p))
				      do (let ((tmp (temp-file-name p)))
					   (pushnew tmp *temporary-files*)
					   (download-files-for-package p tmp)
					   (setf p tmp))
				      end
				      do (format t "Installing ~A in ~A,~A~%"
						 p source system)
				      append (install-package source system p)))
			(handler-bind
			    ((asdf:missing-dependency
			      (lambda (c) 
				(format t
					"Downloading package ~A, required by ~A~%"
					(asdf::missing-requires c)
					(asdf:component-name
					 (asdf::missing-required-by c)))
				(one-iter (list
					   (symbol-name
					    (asdf::missing-requires c))))
				(invoke-restart 'retry))))
			  (loop
			   (multiple-value-bind (ret restart-p)
			       (with-simple-restart
				   (retry "Retry installation")
				 (asdf:operate 'asdf:load-op asd))
                             (declare (ignore ret))
			     (unless restart-p (return))))))))
	     (one-iter packages)))
      (let ((p (merge-pathnames "trusted-uids.lisp" *dot-sbcl*)))
	(with-open-file (out p :direction :output
                               :if-exists :supersede)
	  (with-standard-io-syntax
	    (prin1 *trusted-uids* out))))
      (dolist (l *temporary-files*)
	(when (probe-file l) (delete-file l))))))

(defun uninstall (system &optional (prompt t))
  (let* ((asd (asdf:system-definition-pathname system))
	 (system (asdf:find-system system))
	 (dir (asdf::pathname-sans-name+type
	       (asdf::resolve-symlinks asd))))
    (when (or (not prompt)
	      (y-or-n-p
	       "Delete system ~A~%asd file: ~A~%sources: ~A~%Are you sure?"
	       system asd dir))
      #-(or :win32 :mswindows)
      (delete-file asd)
      (asdf:run-shell-command "rm -r '~A'" (system-namestring dir)))))
      
;;; some day we will also do UPGRADE, but we need to sort out version
;;; numbering a bit better first

#+(or :win32 :mswindows)
(defun sysdef-source-dir-search (system)
  (let ((name (asdf::coerce-name system)))
    (dolist (location *locations*)
      (let* ((dir (first location))
             (files (directory (merge-pathnames
                                (make-pathname :name name
                                               :type "asd"
                                               :version :newest
                                               :directory '(:relative :wild)
                                               :host nil
                                               :device nil)
                                dir))))
        (dolist (file files)
          (when (probe-file file)
            (return-from sysdef-source-dir-search file)))))))
