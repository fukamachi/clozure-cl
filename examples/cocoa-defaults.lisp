;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2004 Clozure Associates
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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :cocoa)
  (use-interface-dir :carbon))

(require "OBJC-SUPPORT")

(defstruct cocoa-default
  symbol                                ; a lisp special variable
  string                                ; an NSConstantString
  type                                  ; a keyword
  value                                 ; the "standard" initial value
  doc                                   ; a doc string
  )

(let* ((cocoa-defaults ()))
  (defun %get-cocoa-default (name)
    (find name cocoa-defaults :key #'cocoa-default-symbol))
  (defun %put-cocoa-default (default)
    (push default cocoa-defaults))
  (defun cocoa-defaults () cocoa-defaults)
  (defun %remove-cocoa-default (name)
    (setq cocoa-defaults
          (delete name cocoa-defaults :key #'cocoa-default-symbol)))
  (defun %clear-cocoa-defaults () (setq cocoa-defaults nil)))

(defun set-cocoa-default (name string type value doc)
  (check-type name symbol)
  (check-type string objc-constant-string)
  (check-type type keyword)
  (check-type doc (or null string))
  (%remove-cocoa-default name)
  (%put-cocoa-default (make-cocoa-default :symbol name
                                          :string string
                                          :type type
                                          :value value
                                          :doc doc))
  value)

(defun %define-cocoa-default (name type value doc)
  (proclaim `(special name))
  (record-source-file name 'variable)
  (setf (documentation name 'variable) doc)
  (set name (set-cocoa-default name (ns-constant-string (string name)) type value doc))
  name)
  
  

(defmacro def-cocoa-default (name type value &optional doc)
  `(progn
    (declaim (special ,name))
    (%define-cocoa-default ',name  ',type ',value ',doc)))

    
(defun update-cocoa-defaults ()
  (let* ((domain (send (@class "NSUserDefaults") 'standard-user-defaults)))
    (dolist (d (cocoa-defaults))
      (let* ((name (cocoa-default-symbol d))
             (key (objc-constant-string-nsstringptr (cocoa-default-string d))))
	(if (%null-ptr-p (send domain :object-for-key key))
	  (send domain
		:set-object (%make-nsstring (format nil "~a" (cocoa-default-value d)))
		:for-key key)
	  (case (cocoa-default-type d)
	    (:int
	     (set name (send domain :integer-for-key key)))
	    (:float
	     (set name (send domain :float-for-key key)))
	    (:string
	     (let* ((nsstring (send domain :string-for-key key)))
	       (unless (%null-ptr-p nsstring)
		 (set name (lisp-string-from-nsstring nsstring)))))))))
    (send domain 'synchronize)
    (send domain 'dictionary-representation)))


  
                                   
    

                       