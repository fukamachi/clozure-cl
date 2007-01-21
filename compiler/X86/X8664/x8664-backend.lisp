;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
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
  (require "X8664ENV"))


(defun define-x8664-callback (name args body env)
  (let* ((stack-word (gensym))
         (stack-ptr (gensym))
         (gpr-arg-num 0)
         (gpr-arg-offset -8)
         (fpr-arg-num 0)
         (fpr-arg-offset -56)
         (memory-arg-offset 16)
         (arg-names ())
         (arg-types ())
         (return-type :void)
         (args args)
         (woi nil)
         (dynamic-extent-names ()))
    (loop
      (when (null args) (return))
      (when (null (cdr args))
        (setq return-type (car args))
        (return))
      (if (eq (car args) :without-interrupts)
        (setq woi (cadr args) args (cddr args))
        (if (eq (car args) :error-return)
          (error ":error-return not yet supported on this target")
          (progn
            (push (foreign-type-to-representation-type (pop args)) arg-types)
            (push (pop args) arg-names)))))
    (setq arg-names (nreverse arg-names)
          arg-types (nreverse arg-types))
    (setq return-type (foreign-type-to-representation-type return-type))
    (when (eq return-type :void)
      (setq return-type nil))
    (let* ((lets
            (flet ((next-gpr ()
                     (if (<= (incf gpr-arg-num) 6)
                       (prog1
                           gpr-arg-offset
                         (decf gpr-arg-offset 8))
                       (prog1
                           memory-arg-offset
                         (incf memory-arg-offset 8))))
                   (next-fpr ()
                       (if (<= (incf fpr-arg-num) 8)
                         (prog1
                             fpr-arg-offset
                           (decf fpr-arg-offset 8))
                         (prog1
                             memory-arg-offset
                           (incf memory-arg-offset 8)))))
              (mapcar
               #'(lambda (name type)
                   (let* ((fp nil))
                     (list name
                           `(,
                             (ecase type
                               (:single-float (setq fp t) '%get-single-float)
                               (:double-float (setq fp t) '%get-double-float)
                               (:signed-doubleword  '%%get-signed-longlong)
                               (:signed-fullword '%get-signed-long)
                               (:signed-halfword '%get-signed-word)
                               (:signed-byte '%get-signed-byte)
                               (:unsigned-doubleword '%%get-unsigned-longlong)
                               (:unsigned-fullword '%get-unsigned-long)
                               (:unsigned-halfword '%get-unsigned-word)
                               (:unsigned-byte '%get-unsigned-byte)
                               (:address
                                (push name dynamic-extent-names)
                                '%get-ptr))
                             ,stack-ptr
                             ,(if fp (next-fpr) (next-gpr))))))
	      arg-names arg-types))))
      (multiple-value-bind (body decls doc) (parse-body body env t)
        `(progn
           (declaim (special ,name))
           (define-callback-function
             (nfunction ,name
                        (lambda (,stack-word)
                          (declare (ignorable ,stack-word))
                            (with-macptrs (,stack-ptr)
                              (%setf-macptr-to-object ,stack-ptr ,stack-word)
                              ,(defcallback-body  stack-ptr lets dynamic-extent-names
                                                 decls `((block ,name ,@body)) return-type))))
             ,doc
             ,woi))))))

(defun defcallback-body-x8664 (stack-ptr lets dynamic-extent-names decls body return-type)
  (let* ((result (gensym))
         (result-offset -8)
         (body
   	  `(let ,lets
              (declare (dynamic-extent ,@dynamic-extent-names))
              ,@decls

              (let ((,result (progn ,@body)))
                (declare (ignorable ,result))
                ,(when return-type
                       `(setf (,
                               (case return-type
                                 (:address '%get-ptr)
                                 (:signed-doubleword '%%get-signed-longlong)
                                 (:unsigned-doubleword '%%get-unsigned-longlong)
                                 (:double-float '%get-double-float)
                                 (:single-float '%get-single-float)
                                 (t  '%%get-signed-longlong))
                               ,stack-ptr ,result-offset) ,result))))))
      body))


(defvar *x8664-vinsn-templates* (make-hash-table :test #'eq))



(defvar *known-x8664-backends* ())


#+(or linuxx86-target (not x86-target))
(defvar *linuxx8664-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
                :lookup-macro #'false
                :lap-opcodes x86::*x8664-opcode-templates*
                :define-vinsn 'define-x86-vinsn
                :platform-syscall-mask (logior platform-os-linux platform-cpu-x86 platform-word-size-64) 
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :linux-target :linuxx86-target :x8664-target
                  :linuxx8664-target
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "lx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-linux
                                         platform-word-size-64)
		:target-os :linuxx86
		:name :linuxx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil


                :target-arch x8664::*x8664-target-arch*
                :define-callback 'define-x8664-callback
                :defcallback-body 'defcallback-body-x8664
                :lisp-context-register x8664::gs
                ))


#+darwinx86-target
(defvar *darwinx8664-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
		:lookup-macro #'false
                :lap-opcodes x86::*x8664-opcode-templates*
                :define-vinsn 'define-x86-vinsn
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) 
		:target-specific-features
		'(:x8664 :x86-target :darwin-target :darwinx86-target :x8664-target
                  :darwinx8664-target
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "dx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-darwin
                                         platform-word-size-64)
		:target-os :darwinx86
		:name :darwinx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*
                :define-callback 'define-x8664-callback
                :defcallback-body 'defcallback-body-x8664
                ;; Overload %gs until Apple straightens things out.
                :lisp-context-register x8664::gs
                ))

#+freebsdx86-target
(defvar *freebsdx8664-backend*
  (make-backend :lookup-opcode 'lookup-x86-opcode
		:lookup-macro #'false
                :lap-opcodes x86::*x8664-opcode-templates*
                :define-vinsn 'define-x86-vinsn
		:p2-dispatch *x862-specials*
		:p2-vinsn-templates *x8664-vinsn-templates*
		:p2-template-hash-name '*x8664-vinsn-templates*
		:p2-compile 'x862-compile
		:target-specific-features
		'(:x8664 :x86-target :freebsd-target :freebsdx86-target :x8664-target
                  :freebsdx8664-target                  
                  :little-endian-target
                  :64-bit-target)
		:target-fasl-pathname (make-pathname :type "fx64fsl")
		:target-platform (logior platform-cpu-x86
                                         platform-os-freebsd
                                         platform-word-size-64)
		:target-os :freebsdx86
		:name :freebsdx8664
		:target-arch-name :x8664
		:target-foreign-type-data nil
                :target-arch x8664::*x8664-target-arch*
                :define-callback 'define-x8664-callback
                :defcallback-body 'defcallback-body-x8664
                :platform-syscall-mask (logior platform-os-freebsd platform-cpu-x86 platform-word-size-64)
                :lisp-context-register x8664::gs
                ))

#+(or linuxx86-target (not x86-target))
(pushnew *linuxx8664-backend* *known-x8664-backends* :key #'backend-name)


#+darwinx86-target
(pushnew *darwinx8664-backend* *known-x8664-backends* :key #'backend-name)

#+freebsdx86-target
(pushnew *freebsdx8664-backend* *known-x8664-backends* :key #'backend-name)

(defvar *x8664-backend* (car *known-x8664-backends*))

(defun fixup-x8664-backend ()
  (dolist (b *known-x8664-backends*)
    (setf #| (backend-lap-opcodes b) x86::*x86-opcodes* |#
	  (backend-p2-dispatch b) *x862-specials*
	  (backend-p2-vinsn-templates b)  *x8664-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-x8664-backend)

#+x8664-target
(setq *host-backend* *x8664-backend* *target-backend* *x8664-backend*)

(defun setup-x8664-ftd (backend)
  (or (backend-target-foreign-type-data backend)
      (let* ((name (backend-name backend))
             (ftd
              (case name
                (:linuxx8664
                 (make-ftd :interface-db-directory "ccl:x86-headers64;"
                          :interface-package-name "X86-LINUX64"
                          :attributes '(:bits-per-word  64
                                        :struct-by-value t)
                          :ff-call-expand-function
                          (intern "EXPAND-FF-CALL" "X86-LINUX64")))
                (:darwinx8664
                  (make-ftd :interface-db-directory "ccl:darwin-x86-headers64;"
                          :interface-package-name "X86-DARWIN64"
                          :attributes '(:bits-per-word  64
                                        :signed-char t
                                        :struct-by-value t
                                        :prepend-underscore t)
                          :ff-call-expand-function
                          (intern "EXPAND-FF-CALL" "X86-DARWIN64")))
                 (:freebsdx8664
                  (make-ftd :interface-db-directory "ccl:freebsd-headers64;"
                            :interface-package-name "X86-FREEBSD64"
                            :attributes '(:bits-per-word  64
                                          :struct-by-value t)
                          :ff-call-expand-function
                          (intern "EXPAND-FF-CALL" "X86-FREEBSD64"))))))
        (install-standard-foreign-types ftd)
    (use-interface-dir :libc ftd)
    (setf (backend-target-foreign-type-data backend) ftd))))

#-x8664-target
(setup-x8664-ftd *x8664-backend*)

(pushnew *x8664-backend* *known-backends* :key #'backend-name)

#+x8664-target
(require "X8664-VINSNS")

(provide "X8664-BACKEND")
