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

;; l1-boot-2.lisp
;; Second part of l1-boot



(macrolet ((l1-load (name)
	     (let* ((namestring
		     (concatenate 'simple-base-string
				  #+linuxppc-target "./l1-pfsls/"
				  #+darwinppc-target "./l1-dfsls/"
				  (string name)
				  #+(and linuxppc-target ppc64-target) ".p64fsl"
                                  #+(and linuxppc-target ppc32-target) ".pfsl"
				  #+(and darwinppc-target ppc64-target) ".d64fsl"
                                  #+(and darwinppc-target ppc32-target) ".dfsl")))
	       `(let* ((*loading-file-source-file* *loading-file-source-file*))
                 (%fasload ,namestring))))
	   (bin-load (name)
	     (let* ((namestring
		     (concatenate 'simple-base-string
				  #+linuxppc-target "./binppc/"
				  #+darwinppc-target "./bindarwin/"
				  (string name)
				  #+(and linuxppc-target ppc64-target) ".p64fsl"
                                  #+(and linuxppc-target ppc32-target) ".pfsl"
				  #+(and darwinppc-target ppc64-target) ".d64fsl"
                                  #+(and darwinppc-target ppc32-target) ".dfsl")))
               `(let* ((*loading-file-source-file* *loading-file-source-file*))
                 (%fasload ,namestring)))))


(catch :toplevel
    (l1-load "l1-typesys")
    (l1-load "sysutils")
    #+ppc-target
    (l1-load "ppc-error-signal")
    (l1-load "l1-error-signal")
    (l1-load "l1-sockets")
    (setq *LEVEL-1-LOADED* t))

(defun altivec-available-p ()
  (not (eql (%get-kernel-global 'ppc::altivec-present) 0)))

(defloadvar *altivec-available* (altivec-available-p))

       
(defglobal *auto-flush-streams* ())
(def-ccl-pointers *auto-flush-streams* () (setq *auto-flush-streams* nil))
(defglobal *auto-flush-streams-lock* (make-lock))


(defloadvar *batch-flag* (not (eql (%get-kernel-global 'ppc::batch-flag) 0)))
(defloadvar *quiet-flag* nil)
(defvar *terminal-input* ())
(defvar *terminal-output* ())
(defvar *stdin* ())
(defvar *stdout* ())
(defvar *stderr* ())

;;; The hard parts here have to do with setting up *TERMINAL-IO*.
;;; If file descriptors 0 and 1 denote the same TTY, that's
;;; pretty easy: we make an echoing-two-way stream in that case.
;;; If we're being run under ILISP, fds 0 and 1 may be pipes
;;; (depending on whether the IPC package (comint) uses pipes
;;; or ptys.  It's not entirely clear whether or not we can
;;; reliably tell that these pipes are peers.
;;; When we're run as a subprocess, we can probably open /dev/tty,
;;; but that doesn't mean that we can read from and write to it.

(def-ccl-pointers fd-streams ()
  (setq *stdin*	(make-fd-stream 0
                                :direction :input
                                :interactive (not *batch-flag*)))
  (setq *stdout* (make-fd-stream 1 :direction :output))

  (setq *stderr* (make-fd-stream 2 :direction :output))
  (if *batch-flag*
    (let* ((tty-fd (fd-open "/dev/tty" #$O_RDWR))
           (can-use-tty (and tty-fd (eql (tcgetpgrp tty-fd) (getpid)))))
      (if can-use-tty
        (setq
         *terminal-input* (make-fd-stream tty-fd
                                          :direction :input
                                          :interactive t)
         *terminal-output* (make-fd-stream tty-fd :direction :output)
         *terminal-io* (make-echoing-two-way-stream
                        *terminal-input* *terminal-output*))
        (progn
          (when tty-fd (fd-close tty-fd))
          (setq *terminal-input* *stdin*
                *terminal-output* *stdout*
                *terminal-io* (make-two-way-stream
                               *terminal-input* *terminal-output*))))
      (setq *standard-input* *stdin*
            *standard-output* *stdout*))
    (progn
      (setq *terminal-input* *stdin*
            *terminal-output* *stdout*
            *terminal-io* (make-echoing-two-way-stream
                           *terminal-input* *terminal-output*))
      (setq *standard-input* (make-synonym-stream '*terminal-io*)
            *standard-output* (make-synonym-stream '*terminal-io*))))
  (setq *error-output* (if *batch-flag*
                         (make-synonym-stream '*stderr*)
                         (make-synonym-stream '*terminal-io*)))
  (setq *query-io* (make-synonym-stream '*terminal-io*))
  (setq *debug-io* *query-io*)
  (setq *trace-output* *standard-output*)
  (push *stdout* *auto-flush-streams*)
  (setf (input-stream-shared-resource *terminal-input*)
	(make-shared-resource "Shared Terminal Input")))





(catch :toplevel
    (macrolet ((l1-load-provide (module path)
		 `(let* ((*package* *package*))
		   (l1-load ,path)
		   (provide ,module)))
	       (bin-load-provide (module path)
		 `(let* ((*package* *package*))
		   (bin-load ,path)
		   (provide ,module))))
      (bin-load-provide "SORT" "sort")
      (bin-load-provide "NUMBERS" "numbers")
      (bin-load-provide "HASH" "hash")
      (bin-load-provide "DLL-NODE" "dll-node")
      
      #+ppc-target
      (bin-load-provide "PPC32-ARCH" "ppc32-arch")
      (bin-load-provide "VREG" "vreg")
      
      #+ppc-target
      (bin-load-provide "PPC-ASM" "ppc-asm")
      
      (bin-load-provide "VINSN" "vinsn")
      (bin-load-provide "REG" "reg")
      (bin-load-provide "SUBPRIMS" "subprims")
      
      #+ppc-target
      (bin-load-provide "PPC-LAP" "ppc-lap")
      
      (bin-load-provide "BACKEND" "backend")
     
      #+ppc-target
      (provide "PPC2")                  ; Lie, load the module manually
      
      (l1-load-provide "NX" "nx")
      
      #+ppc-target
      (bin-load "ppc2")
      
      (bin-load-provide "LEVEL-2" "level-2")
      (bin-load-provide "MACROS" "macros")
      (bin-load-provide "SETF" "setf")
      (bin-load-provide "SETF-RUNTIME" "setf-runtime")
      (bin-load-provide "FORMAT" "format")
      (bin-load-provide "STREAMS" "streams")
      (bin-load-provide "OPTIMIZERS" "optimizers")      
      (bin-load-provide "DEFSTRUCT-MACROS" "defstruct-macros")        ;  ... but this file thinks it does.
      (bin-load-provide "DEFSTRUCT-LDS" "defstruct-lds")
      (bin-load-provide "NFCOMP" "nfcomp")
      (bin-load-provide "BACKQUOTE" "backquote")
      (bin-load-provide "BACKTRACE-LDS" "backtrace-lds")
      (bin-load-provide "BACKTRACE" "backtrace")
      (bin-load-provide "READ" "read")
      (bin-load-provide "ARRAYS-FRY" "arrays-fry")
      (bin-load-provide "APROPOS" "apropos")
      
      #+ppc-target
      (progn
	(bin-load-provide "PPC-DISASSEMBLE" "ppc-disassemble")
	(bin-load-provide "PPC-LAPMACROS" "ppc-lapmacros"))

      (bin-load-provide "FOREIGN-TYPES" "foreign-types")
      (bin-load-provide "DB-IO" "db-io")
      
      (bin-load-provide "CASE-ERROR" "case-error")
      (bin-load-provide "ENCAPSULATE" "encapsulate")
      (bin-load-provide "METHOD-COMBINATION" "method-combination")
      (bin-load-provide "MISC" "misc")
      (bin-load-provide "PPRINT" "pprint")
      (bin-load-provide "DUMPLISP" "dumplisp")
      (bin-load-provide "PATHNAMES" "pathnames")
      (bin-load-provide "TIME" "time")
      (bin-load-provide "COMPILE-CCL" "compile-ccl")
      (bin-load-provide "ARGLIST" "arglist")
      (bin-load-provide "EDIT-CALLERS" "edit-callers")
      (bin-load-provide "DESCRIBE" "describe")
      (bin-load-provide "SOURCE-FILES" "source-files")
      (bin-load-provide "MCL-COMPAT" "mcl-compat")
      (require "LOOP")
      (bin-load-provide "CCL-EXPORT-SYMS" "ccl-export-syms")
      (l1-load-provide "VERSION" "version")
      (progn  ; Shouldn't need this at load time ...
	(%fasload #+linuxppc-target
                  (progn
                    #+ppc32-target "./library/lispequ.pfsl"
                    #+ppc64-target "./library/lispequ.p64fsl")
                  #+darwinppc-target
                  (progn
                    #+ppc32-target  "./library/lispequ.dfsl"
                    #+ppc64-target  "./library/lispequ.d64fsl"))
	(provide "LISPEQU"))
      )
    (setq *%fasload-verbose* nil)
    )
)






