(in-package "CCL")

;;; We need to be able to point the CoreFoundation and Cocoa libraries
;;; at some bundle very early in the process, so do that before anything
;;; else.
;;;
;;; If you're using this file to load something other than the IDE,
;;; you might want to change create-ide-bundle...

(defvar *cocoa-application-path* "ccl:temp bundle.app;")
(defvar *cocoa-application-copy-headers-p* nil)

(defun create-ide-bundle (bundle-path &key (source "ccl:cocoa-ide;ide-contents;")
				           (source-ignore '(".svn" "cvs" ".cvsignore"))
					   (copy-headers *cocoa-application-copy-headers-p*)
					   (if-exists :overwrite))
  ;; TODO: Right now if the bundle exists, we leave alone any files that we don't replace.
  ;; I'd like :if-exists :supersede mean to remove such files, for clean builds, but
  ;; recursive-copy-directory doesn't support :if-exists :supersede yet...
  (flet ((subdir (dir sub)
	   (ensure-directory-pathname (make-pathname :name sub :defaults dir)))
	 (ignore-test (p)
	   (flet ((backup-p (name)
		    (and (stringp name)
			 (let ((len (length name)))
			   (and (> len 0)
				(or (eql (aref name (1- len)) #\~)
				    (eql (aref name 0) #\#)))))))
	     (not (or (member (car (last (pathname-directory p))) source-ignore :test #'equalp)
		      (backup-p (pathname-name p))
		      (backup-p (pathname-type p))
		      (member (pathname-name p) source-ignore :test #'equalp))))))
    (let* ((source-dir (ensure-directory-pathname source))
	   (target-dir (ensure-directory-pathname bundle-path))
	   (contents-dir (subdir target-dir "Contents")))
      (recursive-copy-directory source-dir contents-dir :if-exists if-exists :test #'ignore-test)
      (when copy-headers
	(let* ((subdirs (cdb-subdirectory-path))
	       (ccl-headers (make-pathname :host "ccl" :directory `(:absolute ,@subdirs)))
	       (dest-headers (make-pathname :host (pathname-host contents-dir)
					    :directory (append (pathname-directory contents-dir)
							       (cons "Resources" subdirs)))))
	  (recursive-copy-directory ccl-headers dest-headers :if-exists if-exists :test #'ignore-test)))
      ;; Is this necessary?
      (let* ((image-name (standard-kernel-name))
	     (ccl-image (make-pathname :name image-name :host "ccl"))
	     (dest-image (make-pathname :name image-name
					:defaults (subdir contents-dir "MacOS"))))
	(ensure-directories-exist dest-image)
	(copy-file ccl-image dest-image :if-exists :supersede :preserve-attributes t))
      (touch target-dir))))

#+darwin-target
(progn
  (require "FAKE-CFBUNDLE-PATH")
  (create-ide-bundle *cocoa-application-path*)
  (fake-cfbundle-path *cocoa-application-path* "ccl:cocoa-ide;Info.plist-proto" "com.clozure"))


(require "OBJC-SUPPORT")

(if (< #&NSAppKitVersionNumber 824)
  (error "This application requires features introduced in OSX 10.4."))

(defparameter *standalone-cocoa-ide* nil)

  
(require "COCOA-UTILS")
(require "COCOA-WINDOW")
(require "COCOA-DOC")
(require "COCOA-LISTENER")
(require "COCOA-GREP")
(require "COCOA-BACKTRACE")
(require "COCOA-INSPECTOR")
(require "APP-DELEGATE")


(def-cocoa-default *ccl-directory* :string "" nil #'(lambda (old new)
						      (when (equal new "") (setq new nil))
						      (unless (and new (equal old new))
							(init-interfaces-root)
							(replace-base-translation "ccl:"
										  (or new (find-ccl-directory))))))

;; If there are interfaces inside the bundle, use those rather than the ones
;; in CCL:, since they're more likely to be valid.  CCL: could be some random
;; sources we're just using just for meta-.
(defun init-interfaces-root ()
  (let* ((subpath (cdb-subdirectory-path))
	 (path (pathname-directory (ccl-directory))))
    (when (and *standalone-cocoa-ide*
	       (equalp (last path 2) '("Contents" "MacOS")))
      (setq path (butlast path))
      (when (or (probe-file (make-pathname :directory (append path subpath)))
		(probe-file (make-pathname :directory (append (setq path `(,@path "Resources")) subpath))))
	(setq *interfaces-root* (make-pathname :directory path))))))

(defun find-ccl-directory ()
  (let* ((path (ccl-directory))
	 (dir (pathname-directory path)))
    (if (equalp (last dir 2) '("Contents" "MacOS"))
	(make-pathname :directory (butlast dir 3))
	path)))

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   operation
                                   &rest args)
  (declare (ignore operation args))
  ;; Do nothing.  Would it be better to warn and/or log this ?
  )

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :note-current-package))
                                   &rest args)
  (ui-object-note-package o (car args)))

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :eval-selection))
                                   &rest args)
  (ui-object-eval-selection o (car args)))

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :enter-backtrace-context))
                                   &rest args)
  (ui-object-enter-backtrace-context o (car args)))

(defmethod ui-object-do-operation ((o ns:ns-application)
                                   (operation (eql :exit-backtrace-context))
                                   &rest args)
  (ui-object-exit-backtrace-context o (car args)))




(start-cocoa-application)

