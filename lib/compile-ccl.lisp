;;;-*-Mode: LISP; Package: CCL -*-
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



(in-package :ccl)




(require 'systems)


; Interim PPC support
; sequences is here since l1-typesys REQUIREs it
(defparameter *level-1-modules*
  '(level-1
    l1-cl-package
    l1-boot-1 l1-boot-2 l1-boot-3
    l1-utils l1-init l1-symhash l1-numbers l1-aprims 
    l1-sort l1-dcode l1-clos-boot l1-clos
    l1-streams l1-files l1-io 
    l1-format l1-readloop l1-reader
    l1-sysio l1-pathnames l1-events
    l1-boot-lds  l1-readloop-lds 
    l1-lisp-threads  l1-processes
    l1-typesys sysutils l1-error-system
    l1-error-signal version l1-callbacks
    l1-sockets

    ))

(defparameter *compiler-modules*
      '(nx optimizers dll-node arch vreg vinsn 
	reg subprims risc-lap backend))


(defparameter *ppc-compiler-modules*
      '(ppc32-arch
	ppc-arch
        ppcenv
        ppc-asm
        ppc32-vinsns
        ppc-lap
	ppc-backend
	ppc32-backend
        ppc2
))


(defparameter *sparc-compiler-modules*
      '(sparc-arch
        sparcenv
        sparc-asm
        sparc-vinsns
        sparc-lap
	sparc-backend
        sparc2
        sparc-disassemble
        sparc-lapmacros
))

(defparameter *sparc-bootstrap-modules* '(sparc-bootstrap))

(defparameter *ppc-xload-modules* '(xppcfasload xfasload heap-image ))


(defparameter *sparc-xload-modules* '(xsparcfasload xfasload heap-image))

;; Not too OS-specific.
(defparameter *ppc-xdev-modules* '(ppc2 ppc-lapmacros ppc-disassemble nxenv ))
(defun target-xdev-modules (&optional (target
				       (backend-target-arch-name
					*host-backend*)))
  (case target
    (:ppc32 *ppc-xdev-modules*)))

(defun target-xload-modules (&optional (target
					(backend-target-arch-name *host-backend*)))
  (case target
    (:ppc32 *ppc-xload-modules*)))






(defparameter *env-modules*
      '(hash backquote lispequ  level-2 macros
        defstruct-macros lists chars setf setf-runtime
        defstruct defstruct-lds 
	foreign-types
	db-io
	nfcomp
	eval
	))

(defun target-env-modules (&optional (target
				      (backend-target-arch-name
				       *host-backend*)))
  (append *env-modules*
	  (case target
	    (:ppc32 '( ppc-lapmacros)))))

(defun target-compiler-modules (&optional (target
					   (backend-target-arch-name
					    *host-backend*)))
  (case target
    (:ppc32 *ppc-compiler-modules*)))

(defparameter *other-lib-modules*
      '(streams pathnames backtrace
        apropos
        numbers 
        dumplisp   source-files))

(defun target-other-lib-modules (&optional (target
					    (backend-target-arch-name
					     *host-backend*)))
  (append *other-lib-modules*
	  (case target
	    (:ppc32 '(ppc-disassemble)))))

(defun target-compiler-modules (&optional
				(target
				 (backend-target-arch-name *target-backend*)))
  (case target
    (:ppc32 *ppc-compiler-modules*)))
	  

(defun target-lib-modules (&optional (target
				      (backend-target-arch-name *target-backend*)))
  (append (target-env-modules target) (target-other-lib-modules target)))


(defparameter *code-modules*
      '(encapsulate
        read misc  arrays-fry
        sequences sort 
        method-combination
        case-error pprint 
        format time 
        eval step
        backtrace-lds  ccl-export-syms prepare-mcl-environment))



(defparameter *aux-modules*
      '(systems compile-ccl 
        lisp-package
        number-macros number-case-macro
        loop
	runtime
	mcl-compat
	arglist
	edit-callers
        describe
))







(defun target-level-1-modules (&optional (target (backend-name *host-backend*)))
  (append *level-1-modules*
	  (case target
	    ((:linuxppc32 :darwinppc32)
	     '(linux-files ppc-error-signal ppc-trap-support
	       ppc-threads-utils ppc-callback-support)))))
		  




;





; Needed to cross-dump an image



(unless (fboundp 'xload-level-0)
  (%fhave 'xload-level-0
          #'(lambda (&rest rest)
	      (in-development-mode
	       (require-modules *ppc-xload-modules*))
              (apply 'xload-level-0 rest))))

(defun find-module (module &optional (target (backend-name *host-backend*))  &aux data fasl sources)
  (if (setq data (assoc module *ccl-system*))
    (let* ((backend (or (find-backend target) *host-backend*)))
      (setq fasl (cadr data) sources (caddr data))      
      (setq fasl (merge-pathnames (backend-target-fasl-pathname
				   backend) fasl))
      (values fasl (if (listp sources) sources (list sources))))
    (error "Module ~S not defined" module)))

;compile if needed.
(defun target-compile-modules (modules target force-compile)
  (if (not (listp modules)) (setq modules (list modules)))
  (in-development-mode
   (dolist (module modules t)
     (multiple-value-bind (fasl sources) (find-module module target)
      (if (needs-compile-p fasl sources force-compile)
        (progn
          (require'nfcomp)
          (compile-file (car sources)
			:output-file fasl
			:verbose t
			:target target)))))))






(defun needs-compile-p (fasl sources force-compile)
  (if fasl
    (if (eq force-compile t) t
        (if (not (probe-file fasl)) t
            (let ((fasldate (file-write-date fasl)))
              (if (if (integerp force-compile) (> force-compile fasldate)) t
                  (dolist (source sources nil)
                    (if (> (file-write-date source) fasldate)
                      (return t)))))))))



;compile if needed, load if recompiled.

(defun update-modules (modules &optional force-compile)
  (if (not (listp modules)) (setq modules (list modules)))
  (in-development-mode
   (dolist (module modules t)
     (multiple-value-bind (fasl sources) (find-module module)
       (if (needs-compile-p fasl sources force-compile)
	 (progn
	   (require'nfcomp)
	   (let* ((*warn-if-redefine* nil))
	     (compile-file (car sources) :output-file fasl :verbose t :load t))
	   (provide module)))))))

(defun compile-modules (modules &optional force-compile)
  (target-compile-modules modules (backend-name *host-backend*) force-compile)
)

(defun compile-ccl (&optional force-compile)
  (compile-modules 'nxenv force-compile)
  (update-modules *compiler-modules* force-compile)
  (update-modules (target-compiler-modules) force-compile)
  (update-modules (cdr (target-xdev-modules)) force-compile)
  (update-modules (target-xload-modules)  force-compile)
  (let* ((env-modules (target-env-modules))
	 (other-lib (target-other-lib-modules)))
    (require-modules env-modules)
    (update-modules env-modules force-compile)
    (compile-modules (target-level-1-modules)  force-compile)
    (update-modules other-lib force-compile)
    (require-modules other-lib)
    (require-update-modules *code-modules* force-compile))
  (compile-modules *aux-modules* force-compile))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun require-env (&optional force-load)
  (require-modules  (target-env-modules)
                   force-load))

(defun compile-level-1 (&optional force-compile)
  (require-env)
  (compile-modules (target-level-1-modules (backend-name *host-backend*))
                   force-compile))





(defun compile-lib (&optional force-compile)
  (compile-modules (target-lib-modules)
                   force-compile))

(defun compile-code (&optional force-compile)
  (compile-modules *code-modules* force-compile))



#+ppc-target
(defun load-ccl ()
  (ppc-load-ccl))




;Compile but don't load

#+ppc-target
(defun xcompile-ccl (&optional force)
  (ppc-xcompile-ccl force))

(defun require-update-modules (modules &optional force-compile)
  (if (not (listp modules)) (setq modules (list modules)))
  (in-development-mode
    (dolist (module modules)
    (require-modules module)
    (update-modules module force-compile))))

(defun compile-level-1 (&optional force-compile)
  (compile-modules (target-level-1-modules (backend-name *host-backend*))
		   force-compile))

(defun compile-compiler (&optional force-compile)
  (update-modules 'ppcenv force-compile)
  (compile-modules 'nxenv force-compile)
  (compile-modules 'nx-base-app force-compile) ; for appgen
  (compile-modules *compiler-modules* force-compile)
  (compile-modules *ppc-compiler-modules* force-compile))

(defun ppc-xcompile-ccl (&optional force)
  (compile-compiler force) ; ??
  ;(compile-modules *ppc-xdev-modules* force)
  ; These won't compile correctly unless they're loaded 
  (update-modules *ppc-xload-modules* force)
  (compile-modules (target-level-1-modules :ppc32) force)
  (compile-modules (target-lib-modules :ppc32)  force)
  (compile-modules *aux-modules* force)
  (compile-modules *code-modules* force)
  )

(defun target-xcompile-ccl (target &optional force)
  (let* ((backend (or (find-backend target) *target-backend*))
	 (arch (backend-target-arch-name backend))
	 (*defstruct-share-accessor-functions* nil))
    (target-compile-modules 'nxenv target force)
    (target-compile-modules *compiler-modules* target force)
    (target-compile-modules (target-compiler-modules arch) target force)
    (target-compile-modules (target-level-1-modules target) target force)
    (target-compile-modules (target-lib-modules arch) target force)
    (target-compile-modules *aux-modules* target force)
    (target-compile-modules *code-modules* target force)))

(defun ppc-load-ccl ()
  (require-modules *ppc-compiler-modules*)
  (require-modules (target-lib-modules))
  (require-modules *code-modules*)
  ;(require-modules *ppc-xload-modules*)
  )

(defun ppc-require-module (module force-load)
  (multiple-value-bind (fasl source) (find-module module)
      (setq source (car source))
      (if (if fasl (probe-file fasl))
        (if force-load
          (progn
            (load fasl)
            (provide module))
          (require module fasl))
        (if (probe-file source)
          (progn
            (if fasl (format t "~&Can't find ~S so requiring ~S instead"
                             fasl source))
            (if force-load
              (progn
                (load source)
                (provide module))
              (require module source)))
          (error "Can't find ~S or ~S" fasl source)))))

(defun require-modules (modules &optional force-load)
  (if (not (listp modules)) (setq modules (list modules)))
  (let ((*package* (find-package :ccl)))
    (dolist (m modules t)
      (ppc-require-module m force-load))))


(defun target-xcompile-level-1 (target &optional force)
  (target-compile-modules (target-level-1-modules target) target force))

