;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

;;; Before loading any Cocoa code which depends on CFBundle/NSBundle
;;; being able to find an application bundle, it -may- be neccessary
;;; to point the environment variable "CFProcessPath" to some file
;;; that's where the bundle's executable would be.
;;; This should only be necessary if the current application isn't
;;; already "inside a bundle".  If it is necessary, it has to happen
;;; before the CoreFoundation library's initialized.

(defun fake-cfbundle-path (executable-path)
  (when executable-path
    (unless (probe-file executable-path)
      (cerror "Create an empty file."
	      "The specified executable path (~s) doesn't exist"
	      executable-path)
      (create-file executable-path))
    (let* ((fakepath
	    (native-translated-namestring executable-path)))
      (setenv "CFProcessPath" fakepath))))
