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

(defconstant array-total-size-limit 
  #.(expt 2 (- ppc32::nbits-in-word ppc32::num-subtag-bits))
  "the exclusive upper bound on the total number of elements in an array")


;Features for #+/- conditionalization:
; #+:CORAL = common to ccl and beanie
; #+:COMMON-LISP = not MacLisp, not Scheme...
; #+:CCL = this particular lisp implementation
(defparameter *features*
  '(:ccl :ccl-2 :ccl-3 :ccl-4
    :coral 
    :common-lisp :mcl
    :openmcl
    :ansi-cl :processes
    :unix
    :openmcl-native-threads
    :openmcl-partial-mop
    :mcl-common-mop-subset
    :openmcl-mop-2
    #+eabi-target :eabi-target
    #+interfaces-2 :interfaces-2
    #+ppc-target :powerpc
    #+ppc-target :ppc-target
    #+ppc-target :ppc-clos              ; used in encapsulate
    #+ppc32-target :ppc32-target
    #+ppc32-target :ppc32-host
    #+linux-target :linux-host
    #+linux-target :linux-target
    #+linuxppc-target :linuxppc-target
    #+darwinppc-target :darwinppc-target
    #+darwinppc-target :darwinppc-host
    #+darwinppc-target :darwin
    #+darwinppp-target :darwin-target
    #+darwinppc-target :poweropen-target
    )
  "a list of symbols that describe features provided by the
   implementation")
(defparameter *load-verbose* nil
  "the default for the :VERBOSE argument to LOAD")

;All Lisp package variables... Dunno if this still matters, but it
;used to happen in the kernel...
(dolist (x '(* ** *** *APPLYHOOK* *DEBUG-IO*
             *DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *EVALHOOK*
             *FEATURES* *LOAD-VERBOSE* *MACROEXPAND-HOOK* *MODULES*
             *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE* *PRINT-CIRCLE*
             *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL*
             *PRINT-PRETTY* *PRINT-RADIX* *QUERY-IO* *RANDOM-STATE* *READ-BASE*
             *READ-DEFAULT-FLOAT-FORMAT* *READ-SUPPRESS* *READTABLE*
             *STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT*
             + ++ +++ - / // /// ARRAY-DIMENSION-LIMIT ARRAY-RANK-LIMIT
             ARRAY-TOTAL-SIZE-LIMIT BOOLE-1 BOOLE-2 BOOLE-AND BOOLE-ANDC1
             BOOLE-ANDC2 BOOLE-C1 BOOLE-C2 BOOLE-CLR BOOLE-EQV BOOLE-IOR
             BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2 BOOLE-SET BOOLE-XOR
             CALL-ARGUMENTS-LIMIT CHAR-CODE-LIMIT
             DOUBLE-FLOAT-EPSILON DOUBLE-FLOAT-NEGATIVE-EPSILON
             INTERNAL-TIME-UNITS-PER-SECOND LAMBDA-LIST-KEYWORDS
             LAMBDA-PARAMETERS-LIMIT LEAST-NEGATIVE-DOUBLE-FLOAT
             LEAST-NEGATIVE-LONG-FLOAT LEAST-NEGATIVE-SHORT-FLOAT
             LEAST-NEGATIVE-SINGLE-FLOAT LEAST-POSITIVE-DOUBLE-FLOAT
             LEAST-POSITIVE-LONG-FLOAT LEAST-POSITIVE-SHORT-FLOAT
             LEAST-POSITIVE-SINGLE-FLOAT LONG-FLOAT-EPSILON
             LONG-FLOAT-NEGATIVE-EPSILON MOST-NEGATIVE-DOUBLE-FLOAT
             MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-LONG-FLOAT
             MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT
             MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-FIXNUM
             MOST-POSITIVE-LONG-FLOAT MOST-POSITIVE-SHORT-FLOAT
             MOST-POSITIVE-SINGLE-FLOAT MULTIPLE-VALUES-LIMIT PI
             SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON
             SINGLE-FLOAT-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON))
  (%symbol-bits x (%ilogior2 (%symbol-bits x) (ash 1 $sym_bit_special))))

(defparameter *loading-file-source-file* nil)

; end
