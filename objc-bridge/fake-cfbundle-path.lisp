;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

;;; Before loading any Cocoa code which depends on CFBundle/NSBundle
;;; being able to find an application bundle, it -may- be neccessary
;;; to point the environment variable "CFProcessPath" to some file
;;; that's where the bundle's executable would be.
;;; This should only be necessary if the current application isn't
;;; already "inside a bundle".  If it is necessary, it has to happen
;;; before the CoreFoundation library's initialized.

(defun fake-cfbundle-path (bundle-root info-plist-proto-path)
  (let* ((kernel-name (standard-kernel-name))
         (needle "OPENMCL-KERNEL")
         (translated-root (translate-logical-pathname bundle-root))
         (executable-path (merge-pathnames
                           (make-pathname :directory "Contents/MacOS/"
                                          :name kernel-name)
                           translated-root)))
    (unless (probe-file info-plist-proto-path)
      (error "Can't find Info.plist prototype in ~s" info-plist-proto-path))
    (with-open-file (in info-plist-proto-path 
                        :direction :input
                        :external-format :utf-8)
      (with-open-file (out (merge-pathnames
                            (make-pathname :directory "Contents/"
                                           :name "Info"
                                           :type "plist")
                            translated-root)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede
                           :external-format :utf-8)
        (do* ((line (read-line in nil nil) (read-line in nil nil)))
             ((null line))
          (let* ((pos (search needle line)))
            (when pos
              (setq line
                    (concatenate 'string
                                 (subseq line 0 pos)
                                 kernel-name
                                 (subseq line (+ pos (length needle)))))))
          (write-line line out))))
    (touch executable-path)
    (setenv "CFProcessPath" (native-translated-namestring executable-path))))
