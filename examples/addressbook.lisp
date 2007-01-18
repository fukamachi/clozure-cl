;;; This code is adapted from the webkit example and with help 
;;; from Richard Cook and Gary Byers on the OpenMCL list.

(in-package ccl)

;;; We need to be able to point the CoreFoundation and Cocoa libraries
;;; at some bundle very early in the process.  If you want to use some
;;; other bundle path, you may need to change the call to FAKE-CFBUNDLE-PATH
;;; below.

#+darwin-target
(progn
  (require "FAKE-CFBUNDLE-PATH")
  (fake-cfbundle-path "ccl:OpenMCL.app;Contents;MacOS;dppccl"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OBJC-SUPPORT")
  (use-interface-dir :addressbook)
  (reset-objc-class-count)
  (update-objc-method-info))

(let* ((checked-for-addressbook nil)
       (addressbook-loaded nil))
  (defun reset-checked-for-addressbook ()
    (setq checked-for-addressbook nil
	  addressbook-loaded nil))
  (defun check-for-addressbook ()
    (if checked-for-addressbook
	addressbook-loaded
	(with-autorelease-pool
	 (let* ((bundle
		 (send
		  (@class "NSBundle")
		  :bundle-with-path
		  #@"/System/Library/Frameworks/Addressbook.framework")))
	   (setq checked-for-addressbook t
		 addressbook-loaded (unless (%null-ptr-p bundle)
				 (send (the ns:ns-bundle bundle) 'load)))
	   ;; Process class, method decls
	   (map-objc-classes)
	   addressbook-loaded)))))

(defun require-addressbook () 
  (or (check-for-addressbook)
      (error "The Addressbook framework doesn't seem to be installed on this machine.~&
	      It's available as part of Safari 1.0.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require-addressbook))
