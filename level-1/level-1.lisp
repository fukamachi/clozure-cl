;;-*-Mode: LISP; Package: CCL -*-
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

; Level-1.lisp

(in-package "CCL")

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
	       `(%fasload ,namestring)))
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
	       `(%fasload ,namestring))))

  (l1-load "l1-cl-package")
  (l1-load "l1-utils")
  (l1-load "l1-init")
  (l1-load "l1-symhash")
  (l1-load "l1-numbers")
  (l1-load "l1-aprims")
  #+ppc-target
  (l1-load "ppc-callback-support")
  (l1-load "l1-callbacks")
  (l1-load "l1-sort")
  (bin-load "lists")
  (bin-load "sequences")
  (l1-load "l1-dcode")
  (l1-load "l1-clos-boot")
  (bin-load "hash")
  (l1-load "l1-clos")
  (bin-load "defstruct")
  (bin-load "dll-node")
  (l1-load "l1-streams")
  (l1-load "linux-files")
  (bin-load "chars")
  (l1-load "l1-files")
  (provide "SEQUENCES")
  (provide "DEFSTRUCT")
  (provide "CHARS")
  (provide "LISTS")
  (provide "DLL-NODE")
  (l1-load "ppc-threads-utils")
  (l1-load "l1-lisp-threads")
  (l1-load "l1-application")
  (l1-load "l1-processes")
  (l1-load "l1-io")
  (l1-load "l1-reader")
  (l1-load "l1-readloop")
  (l1-load "l1-readloop-lds")
  (l1-load "l1-error-system")

  (l1-load "l1-events")
  (l1-load "ppc-trap-support")
  (l1-load "l1-format")
  (l1-load "l1-sysio")
  (l1-load "l1-pathnames")
  (l1-load "l1-boot-lds")

  (l1-load "l1-boot-1")
  (l1-load "l1-boot-2")
  (l1-load "l1-boot-3")
  )

(require "PREPARE-MCL-ENVIRONMENT")
(progn (%set-toplevel #'toplevel-loop) (set-user-environment t) (toplevel))




