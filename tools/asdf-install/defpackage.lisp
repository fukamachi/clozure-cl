(cl:in-package :cl-user)

(defpackage :asdf-install
  (:use "CL")
  (:export
   ;; customizable variables
   #:*proxy* #:*cclan-mirror* #:*sbcl-home*
   #:*verify-gpg-signatures* #:*locations*
   #:*safe-url-prefixes*
   #:*preferred-location*
   #+(or :win32 :mswindows) #:sysdef-source-dir-search
   ;; external entry points
   #:uninstall #:install))

(defpackage :asdf-install-customize
  (:use "CL" "ASDF-INSTALL"))
