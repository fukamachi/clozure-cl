;;; -*- Mode: Lisp; Package: CCL -*-
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

; level-0;ppc;ppc-debug.lisp - PPC debugging functions!


#+allow-in-package
(in-package "CCL")



; Write a simple-base-string to stderr's file descriptor (2).
(defppclapfunction %string-to-stderr ((str arg_z))
  (check-nargs 1)
  (save-lisp-context)
  (:regsave save2 0)
  (vpush save0)
  (vpush save1)
  (vpush save2)
  (trap-unless-typecode= str ppc32::subtag-simple-base-string)
  (let ((size imm0)
        (header imm1)
        (length save1)
        (ptr save0)
        (string save2))
    (mr string str)
    (vector-size size string header)
    (box-fixnum length size)
    ; we need 8 bytes for tsp header, 8 bytes for macptr, and need to
    ; round size up to a dword boundary.
    (la imm2 (+ 8 8 7) size)
    (clrrwi imm2 imm2 3)                ; align to dword-boundary
    (neg imm2 imm2)
    (stwux tsp tsp imm2)
    (stw tsp 4 tsp)                     ; not-lisp
    (li imm2 (logior (ash 1 ppc32::num-subtag-bits) ppc32::subtag-macptr))
    (la imm3 16 tsp)
    (stw imm2 8 tsp)
    (stw imm3 12 tsp)
    (la ptr (+ 8 ppc32::fulltag-misc) tsp)
    (vpush string)                      ; source ivector
    (vpush rzero)                       ; source-byte-offset
    (mr arg_x ptr)                      ; dest macptr
    (li arg_y 0)                        ; dest-byte-offset
    (mr arg_z length)                   ; nbytes
    (set-nargs 5)
    (call-symbol %copy-ivector-to-ptr)
    (li arg_x '2)
    (mr arg_y ptr)
    (mr arg_z length)
    (set-nargs 3)
    (call-symbol fd-write)
    (lwz tsp 0 tsp)
    (lwz save2 0 vsp)
    (lwz save1 4 vsp)
    (lwz save0 8 vsp)
    (restore-full-lisp-context)
    (blr)))





; Alice's cuter name
(defppclapfunction dbg-paws ()
  (blr))

; end
