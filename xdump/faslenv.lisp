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

;; Compile-time environment for fasl dumper/loader.

; loader state istruct
(def-accessors (faslstate) %svref
  ()
  faslstate.faslfname
  faslstate.faslevec
  faslstate.faslecnt
  faslstate.faslfd
  faslstate.faslval
  faslstate.faslstr
  faslstate.oldfaslstr
  faslstate.faslerr
  faslstate.iobuffer
  faslstate.bufcount
  faslstate.faslversion
  faslstate.faslepush
  faslstate.faslgsymbols
  faslstate.fasldispatch)

; loader framework istruct
(def-accessors (faslapi) %svref
  ()
  ;; these represent all users of faslstate.iobuffer, .bufcount, and
  ;; .faslfd -- I think these are all the important file- and
  ;; buffer-IO-specific slots in faslstate; encapsulating these allows
  ;; sophisticated users to load fasl data from nonstandard sources
  ;; without too much trouble
  faslapi.fasl-open
  faslapi.fasl-close
  faslapi.fasl-init-buffer
  faslapi.fasl-set-file-pos
  faslapi.fasl-get-file-pos
  faslapi.fasl-read-buffer
  faslapi.fasl-read-byte
  faslapi.fasl-read-n-bytes)

(defconstant numfaslops 50 "Number of fasl file opcodes, roughly")
(defconstant $fasl-epush-bit 7)
(defconstant $fasl-file-id #xff00)
(defconstant $fasl-file-id1 #xff01)
(defconstant $fasl-vers #x40)
(defconstant $fasl-min-vers #x40)
(defconstant $faslend #xff)
(defconstant $fasl-buf-len 2048)
(defmacro deffaslop (n arglist &body body)
  `(setf (svref *fasl-dispatch-table* ,n)
         #'(lambda ,arglist ,@body)))


(defconstant $fasl-noop 0)              ;<nada:zilch>.  
;(defconstant $fasl-obsolete 1)
(defconstant $fasl-code-vector 2)       ;<count> words of code
(defconstant $fasl-svar 3)              ;<expr> Make SVAR for special symbol
(defconstant $fasl-lfuncall 4)          ;<lfun:expr> funcall the lfun.
(defconstant $fasl-globals 5)           ;<expr> global symbols vector
(defconstant $fasl-char 6)              ;<char:byte> Make a char
(defconstant $fasl-fixnum 7)            ;<value:long> Make a (4-byte) fixnum
(defconstant $fasl-dfloat 8)            ;<hi:long><lo:long> Make a DOUBLE-FLOAT
(defconstant $fasl-unused-9 9)          ;
(defconstant $fasl-word-fixnum 10)      ;<value:word> Make a fixnum
(defconstant $fasl-unused-11 11)        ;
(defconstant $fasl-unused-12 12)        ;
(defconstant $fasl-unused-14 13)        ;
(defconstant $fasl-unused-15 14)        ;
(defconstant $fasl-cons 15)             ;<car:expr><cdr:expr> Make a cons
(defconstant $fasl-unused-16 16)        ;
(defconstant $fasl-unused-17 17)        ;
(defconstant $fasl-nil 18)              ; Make nil
(defconstant $fasl-timm 19)             ;<n:long>
(defconstant $fasl-function 20)         ;<count> Make function
(defconstant $fasl-vstr 21)             ;<vstring> Make a string
(defconstant $fasl-vmksym 22)           ;<vstring> Make an uninterned symbol
(defconstant $fasl-arch 23)             ;<n:byte> Ensure that file's loadable on arch n.
(defconstant $fasl-vetab-alloc 24)      ;<count:count> Make a new expression table
                                        ; with count slots.  Current etab gets lost.
(defconstant $fasl-veref 25)            ;<index:count> Get the value from an etab slot.
(defconstant $fasl-fixnum8 26)          ;<high:long><low:long> Make an 8-byte fixnum.
(defconstant $fasl-symfn 27)            ;<sym:expr> returns #'sym.
(defconstant $fasl-eval 28)             ;<expr> Eval <expr> and return value.
(defconstant $fasl-unused-29 29)        ;
(defconstant $fasl-unused-30 30)        ;
(defconstant $fasl-vintern 31)          ;<vstring> Intern in current pkg.
(defconstant $fasl-vpkg-intern 32)      ;<pkg:expr><vstring> Make a sym in pkg.
(defconstant $fasl-vpkg 33)             ;<string> Returns the package of given name
(defconstant $fasl-vgvec 34)            ;<subtype:byte><n:count><n exprs>
(defconstant $fasl-defun 35)            ;<fn:expr><doc:expr>
(defconstant $fasl-macro 37)            ;<fn:expr><doc:expr>
(defconstant $fasl-defconstant 38)      ;<sym:expr><val:expr><doc:expr>
(defconstant $fasl-defparameter 39)     ;<sym:expr><val:expr><doc:expr>
(defconstant $fasl-defvar 40)           ;<sym:expr>
(defconstant $fasl-defvar-init 41)      ;<sym:expr><val:expr><doc:expr>
(defconstant $fasl-vivec 42)            ;<subtype:byte><n:count><n data bytes>
(defconstant $fasl-prog1 43)            ;<expr><expr> - Second <expr> is for side-affects only
(defconstant $fasl-vlist 44)            ;<n:count> <data: n+1 exprs> Make a list
(defconstant $fasl-vlist* 45)           ;<n:count> <data:n+2 exprs> Make an sexpr
(defconstant $fasl-sfloat 46)           ;<long> Make SINGLE-FLOAT from bits
(defconstant $fasl-src 47)              ;<expr> - Set *loading-file-source-file * to <expr>.
(defconstant $fasl-unused-48 48)        ;
(defconstant $fasl-provide 49)          ;<string:expr>

; <string> means <size><size bytes>
; <size> means either <n:byte> with n<#xFF, or <FF><n:word> with n<#xFFFF or
;   <FFFF><n:long>

(defconstant $fasl-end #xFF)    ;Stop reading.

(defconstant $fasl-epush-mask #x80)  ;Push value on etab if this bit is set in opcode.

(defmacro fasl-epush-op (op) `(%ilogior2 ,$fasl-epush-mask ,op))

(provide "FASLENV")
