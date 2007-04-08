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
  constraint                            ; an optional type constraint.
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

(defun set-cocoa-default (name string type value doc &optional constraint)
  (check-type name symbol)
  (check-type string objc-constant-string)
  (check-type type keyword)
  (check-type doc (or null string))
  (%remove-cocoa-default name)
  (%put-cocoa-default (make-cocoa-default :symbol name
                                          :string string
                                          :type type
                                          :value value
                                          :doc doc
                                          :constraint constraint))
  value)

(defun %define-cocoa-default (name type value doc &optional constraint)
  (proclaim `(special ,name))
  ;; Make the variable "GLOBAL": its value can be changed, but it can't
  ;; have a per-thread binding.
  (%symbol-bits name (logior (ash 1 $sym_vbit_global)
                             (the fixnum (%symbol-bits name))))
  (record-source-file name 'variable)
  (setf (documentation name 'variable) doc)
  (set name (set-cocoa-default name (ns-constant-string (string name)) type value doc constraint))
  name)
  
  

(defmacro def-cocoa-default (name type value  doc &optional constraint &environment env)
  `(progn
     (eval-when (:compile-toplevel)
       (note-variable-info ',name :global ,env))
    (declaim (special ,name))
    (%define-cocoa-default ',name  ',type ',value ',doc ,@(when constraint `((specifier-type ',constraint))))))

    
(defun update-cocoa-defaults ()
  (update-cocoa-defaults-vector
   (#/standardUserDefaults ns:ns-user-defaults)
   (apply #'vector (reverse (cocoa-defaults)))))

(defun update-cocoa-defaults-vector (domain defaults-vector)
  (let* ((need-synch nil))
    (dotimes (i (length defaults-vector))
      (let* ((d (svref defaults-vector i))
             (name (cocoa-default-symbol d))
             (key (objc-constant-string-nsstringptr (cocoa-default-string d))))
	(if (%null-ptr-p (#/objectForKey:  domain key))
          (progn
            (#/setObject:forKey: domain (%make-nsstring (format nil "~a" (cocoa-default-value d))) key)
            (setq need-synch t))
	  (case (cocoa-default-type d)
	    (:int
	     (set name (#/integerForKey: domain key)))
	    (:float
	     (set name (#/floatForKey: domain key)))
	    (:string
	     (let* ((nsstring (#/stringForKey: domain key)))
	       (unless (%null-ptr-p nsstring)
		 (set name (lisp-string-from-nsstring nsstring)))))))))
    (when need-synch (#/synchronize domain))))
