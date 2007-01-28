;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  (require "PPCENV")
  (require "PPC32-ARCH"))


#+eabi-target
(defun define-ppc32-eabi-callback (name args body env)
  (let* ((stack-word (gensym))
         (stack-ptr (gensym))
         (result-type-spec :void)
         (args args)
         (woi nil)
	 (monitor nil)
         (need-struct-arg)
         (struct-return-arg-name)
         (error-return nil))
    (collect ((arg-names)
              (arg-specs))
      (let* ((spec (car (last args)))
             (rtype (ignore-errors (parse-foreign-type spec))))
        (setq need-struct-arg (typep rtype 'foreign-record-type))
        (if rtype
          (setq result-type-spec spec args (butlast args))))
      
    (loop
      (when (null args) (return))
      (if (eq (car args) :without-interrupts)
        (setq woi (cadr args) args (cddr args))
	(if (eq (car args) :monitor-exception-ports)
	  (setq monitor (cadr args) args (cddr args))
          (if (eq (car args) :error-return)
            (setq error-return
                  (warn "~s not yet implemented on this platform"
                        :error-return)
                  args (cddr args))
            (if need-struct-arg
              (setq struct-return-arg-name (pop args) need-struct-arg nil)
            (progn
              (arg-specs (pop args))
              (arg-names (pop args))))))))
      (multiple-value-bind (rlets lets dynamic-extent-names inits foreign-return-type)
          (funcall (ftd-callback-bindings-function *target-ftd*)
                   stack-ptr (arg-names) (arg-specs) result-type-spec struct-return-arg-name)
        (multiple-value-bind (body decls doc) (parse-body body env t)
          `(progn
            (declaim (special ,name))
            (define-callback-function
                (nfunction ,name
                 (lambda (,stack-word)
                   (declare (ignorable ,stack-word))
                   (block ,name
                     (with-macptrs ((,stack-ptr))
                       (%setf-macptr-to-object ,stack-ptr ,stack-word)
                       ,(defcallback-body  stack-ptr
                                           lets
                                           rlets
                                           inits
                                           `(declare (dynamic-extent ,@dynamic-extent-names))
                                           decls
                                           body
                                           foreign-return-type
                                           struct-return-arg-name
                                           error-return
                                           0
                                           )))))
                ,doc
              ,woi
              ,monitor)))))))

#+eabi-target
(defun defcallback-body-ppc32-eabi (stack-ptr lets rlets inits dynamic-extent-decls other-decls body return-type struct-return-arg error-return error-delta)
  (let* ((result (gensym))
         (condition-name (if (atom error-return) 'error (car error-return)))
         (error-return-function (if (atom error-return) error-return (cadr error-return)))
         (body
   	  `(rlet ,rlets
            (let ,lets
              ,dynamic-extent-decls
              ,@other-decls
              ,@inits
              (let ((,result (progn ,@body)))
                (declare (ignorable ,result))
                ,@(progn
                   ;; Coerce SINGLE-FLOAT result to DOUBLE-FLOAT
                   (when (typep return-type 'foreign-single-float-type)
                     (setq result `(float ,result 0.0d0)))
                   nil)
                ,(funcall (ftd-callback-return-value-function *target-ftd*)
                          stack-ptr
                          result
                          return-type
                          struct-return-arg))))))
    (if error-return
      (let* ((cond (gensym)))
        `(handler-case ,body
          (,condition-name (,cond) (,error-return-function ,cond ,stack-ptr (%inc-ptr ,stack-ptr ,error-delta)))))
      body)))

#+poweropen-target
(defun define-ppc32-poweropen-callback (name args body env)
  (let* ((stack-word (gensym))
         (stack-ptr (gensym))
         (fp-arg-regs (gensym))
         (fp-arg-num 0)
         (arg-names ())
         (arg-types ())
         (return-type :void)
         (args args)
         (woi nil)
	 (monitor nil)
         (dynamic-extent-names ())
         (error-return nil))
    (loop
      (when (null args) (return))
      (when (null (cdr args))
        (setq return-type (car args))
        (return))
      (if (eq (car args) :without-interrupts)
        (setq woi (cadr args) args (cddr args))
	(if (eq (car args) :monitor-exception-ports)
	  (setq monitor (cadr args) args (cddr args))
          (if (eq (car args) :error-return)
            (setq error-return
                  (cadr args)
                  args (cddr args))
            (progn
              (push (foreign-type-to-representation-type (pop args)) arg-types)
              (push (pop args) arg-names))))))
    (setq arg-names (nreverse arg-names)
          arg-types (nreverse arg-types))
    (setq return-type (foreign-type-to-representation-type return-type))
    (when (eq return-type :void)
      (setq return-type nil))
    (let* ((offset 0)
           (need-stack-pointer (or arg-names return-type error-return))
           (lets
             (mapcar
	      #'(lambda (name type)
		  (let* ((delta 4)
			 (bias 0)
                         (use-fp-args nil))
		    (prog1
			(list name
			      `(,
				(if (typep type 'unsigned-byte)
				  (progn (setq delta (* 4 type)) '%inc-ptr)
				  (ecase type
				    (:single-float
                                     (if (< (incf fp-arg-num) 14)
                                       (progn
                                         (setq use-fp-args t)
                                         '%get-single-float-from-double-ptr)
                                       '%get-single-float))
				    (:double-float
                                     (setq delta 8)
                                     (if (< (incf fp-arg-num) 14)
                                       (setq use-fp-args t))
                                     '%get-double-float)
				    (:signed-doubleword (setq delta 8) '%%get-signed-longlong)
				    (:signed-fullword
                                     (setq bias 0)
                                     '%get-signed-long)
				    (:signed-halfword (setq bias 2)
                                                      '%get-signed-word)
				    (:signed-byte (setq bias 3)
                                                  '%get-signed-byte)
				    (:unsigned-doubleword (setq delta 8) '%%get-unsigned-longlong)
				    (:unsigned-fullword
                                     (setq bias 0)
                                     '%get-unsigned-long)
				    (:unsigned-halfword
                                     (setq bias 2)
                                     '%get-unsigned-word)
				    (:unsigned-byte
                                     (setq bias 3)
                                     '%get-unsigned-byte)
				    (:address '%get-ptr)))
				,(if use-fp-args fp-arg-regs stack-ptr)
				,(if use-fp-args (* 8 (1- fp-arg-num))
                                     `(+ ,offset ,bias))))
		      (when (or (eq type :address)
				(typep type 'unsigned-byte))
			(push name dynamic-extent-names))
		      (incf offset delta))))
	      arg-names arg-types)))
      (multiple-value-bind (body decls doc) (parse-body body env t)
        `(progn
           (declaim (special ,name))
           (define-callback-function
             (nfunction ,name
                        (lambda (,stack-word)
                          (declare (ignorable ,stack-word))
                          (block ,name
                            (with-macptrs (,@(and need-stack-pointer (list `(,stack-ptr))))
                              ,(when need-stack-pointer
                                 `(%setf-macptr-to-object ,stack-ptr ,stack-word))
                              ,(defcallback-body  stack-ptr lets dynamic-extent-names
                                                 decls body return-type error-return
                                                 (- ppc32::c-frame.savelr ppc32::c-frame.param0)
                                                 fp-arg-regs
                                                 )))))
             ,doc
             ,woi
	     ,monitor))))))

#+poweropen-target
(defun defcallback-body-ppc32-poweropen (stack-ptr lets dynamic-extent-names decls body return-type error-return error-delta #+poweropen-target fp-arg-ptr)
  (let* ((result (gensym))
         (return-ptr (case return-type
                       ((:single-float :double-float)
                        fp-arg-ptr)
                       (t stack-ptr)))
         (condition-name (if (atom error-return) 'error (car error-return)))
         (error-return-function (if (atom error-return) error-return (cadr error-return)))
         (body
   	  `(with-macptrs ((,fp-arg-ptr))
            (%setf-macptr ,fp-arg-ptr (%get-ptr ,stack-ptr (- ppc32::c-frame.unused-1 ppc32::c-frame.param0)))
            (let ,lets
              (declare (dynamic-extent ,@dynamic-extent-names))
              ,@decls

              (let ((,result (progn ,@body)))
                (declare (ignorable ,result))
                ,@(progn
                   ;; Coerce SINGLE-FLOAT result to DOUBLE-FLOAT
                   (when (eq return-type :single-float)
                     (setq result `(float ,result 0.0d0)))
                   nil)

                ,(when return-type
                       `(setf (,
                               (case return-type
                                 (:address '%get-ptr)
                                 (:signed-doubleword '%%get-signed-longlong)
                                 (:unsigned-doubleword '%%get-unsigned-longlong)
                                 ((:double-float :single-float) '%get-double-float)
                                 (t  '%get-long)) ,return-ptr 0) ,result)))))))
    (if error-return
      (let* ((cond (gensym)))
        `(handler-case ,body
          (,condition-name (,cond) (,error-return-function ,cond ,stack-ptr (%inc-ptr ,stack-ptr ,error-delta)))))
      body)))

(defvar *ppc32-vinsn-templates* (make-hash-table :test #'eq))




(defvar *known-ppc32-backends* ())


#+linuxppc-target
(defvar *linuxppc32-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
                :define-vinsn 'define-ppc-vinsn
                :platform-syscall-mask (logior platform-os-linux platform-cpu-ppc)
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc32-vinsn-templates*
		:p2-template-hash-name '*ppc32-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :eabi-target :linux-target :linuxppc-target :ppc32-target :32-bit-target :big-endian-target)
		:target-fasl-pathname (make-pathname :type "pfsl")
		:target-platform (logior platform-word-size-32
                                         platform-cpu-ppc
                                         platform-os-linux)
		:target-os :linuxppc
		:name :linuxppc32
		:target-arch-name :ppc32
		:target-foreign-type-data nil
                :target-arch ppc32::*ppc32-target-arch*
                :define-callback 'define-ppc32-eabi-callback
                :defcallback-body 'defcallback-body-ppc32-eabi))


#+darwinppc-target
(defvar *darwinppc32-backend*
  (make-backend :lookup-opcode #'lookup-ppc-opcode
		:lookup-macro #'ppc::ppc-macro-function
		:lap-opcodes ppc::*ppc-opcodes*
                :define-vinsn 'define-ppc-vinsn
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-ppc)                
		:p2-dispatch *ppc2-specials*
		:p2-vinsn-templates *ppc32-vinsn-templates*
		:p2-template-hash-name '*ppc32-vinsn-templates*
		:p2-compile 'ppc2-compile
		:target-specific-features
		'(:powerpc :ppc-target :darwin-target :darwinppc-target :ppc32-target :32-bit-target :big-endian-target)
		:target-fasl-pathname (make-pathname :type "dfsl")
		:target-platform (logior platform-word-size-32
                                         platform-cpu-ppc
                                         platform-os-darwin)
		:target-os :darwinppc
		:name :darwinppc32
		:target-arch-name :ppc32
		:target-foreign-type-data nil
                :target-arch ppc32::*ppc32-target-arch*
                :define-callback 'define-ppc32-poweropen-callback
                :defcallback-body 'defcallback-body-ppc32-poweropen))

#+linuxppc-target
(pushnew *linuxppc32-backend* *known-ppc32-backends* :key #'backend-name)


#+darwinppc-target
(pushnew *darwinppc32-backend* *known-ppc32-backends* :key #'backend-name)

(defvar *ppc32-backend* (car *known-ppc32-backends*))

(defun fixup-ppc32-backend ()
  (dolist (b *known-ppc32-backends*)
    (setf (backend-lap-opcodes b) ppc::*ppc-opcodes*
	  (backend-p2-dispatch b) *ppc2-specials*
	  (backend-p2-vinsn-templates b)  *ppc32-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-ppc32-backend)

#+ppc32-target
(setq *host-backend* *ppc32-backend* *target-backend* *ppc32-backend*)
#-ppc32-target
(unless (backend-target-foreign-type-data *ppc32-backend*)
  (let* ((ftd (make-ftd
               :interface-db-directory
               #+darwinppc-target "ccl:darwin-headers;"
               #+linuxppc-target "ccl:headers;"
               :interface-package-name
               #+darwinppc-target "DARWIN32"
               #+linuxppc-target "LINUX32"
               :attributes
               #+darwinppc-target
               '(:signed-char t
                 :struct-by-value t
                 :prepend-underscores t
                 :bits-per-word  32
                 :poweropen-alignment t)
               #+linuxppc-target
               '(:bits-per-word 32)
               :ff-call-expand-function
               #+linuxppc-target
               'linux32::expand-ff-call
               #+darwinppc-target
               'darwin32::expand-ff-call
               :ff-call-struct-return-by-implicit-arg-function
               #+linuxppc-target
               linux32::record-type-returns-structure-as-first-arg
               #+darwinppc-target
               darwin32::record-type-returns-structure-as-first-arg
               :callback-bindings-function
               #+linuxppc-target
               linux32::generate-callback-bindings
               #+darwinppc-target
               darwin32::generate-callback-bindings
               :callback-return-value-function
               #+linuxppc-target
               linux32::generate-callback-return-value
               #+darwinppc-target
               darwin32::generate-callback-return-value
               )))
    (install-standard-foreign-types ftd)
    (use-interface-dir :libc ftd)
    (setf (backend-target-foreign-type-data *ppc32-backend*) ftd)))

(pushnew *ppc32-backend* *known-backends* :key #'backend-name)

#+ppc32-target
(require "PPC32-VINSNS")
(provide "PPC32-BACKEND")
