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



#+allow-in-package
(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "PPC32-ARCH" )
  (require "PPC-LAPMACROS")
  (require "LISPEQU")
  )

(defmacro %make-sfloat ()
  `(%alloc-misc ppc32::single-float.element-count ppc32::subtag-single-float))

(defmacro %make-dfloat ()
  `(%alloc-misc ppc32::double-float.element-count ppc32::subtag-double-float))

(defmacro require-null-or-double-float-sym (sym)
  (setq sym (require-type sym 'symbol))
  `(when (and ,sym (not (double-float-p ,sym)))
     (setq ,sym (require-type ,sym 'double-float))))




(defmacro %numerator (x)
  `(%svref ,x ppc32::ratio.numer-cell))

(defmacro %denominator (x)
  `(%svref ,x ppc32::ratio.denom-cell))

(defmacro %realpart (x)
  `(%svref ,x ppc32::complex.realpart-cell))

(defmacro %imagpart (x)
  `(%svref ,x ppc32::complex.imagpart-cell))


(defmacro with-stack-double-floats (specs &body body)
    (collect ((binds)
	      (inits)
	      (names))
      (dolist (spec specs)
        (let ((name (first spec)))
          (binds `(,name (%alloc-misc ppc32::double-float.element-count ppc32::subtag-double-float)))
          (names name)
          (let ((init (second spec)))
            (when init
              (inits `(%double-float ,init ,name))))))
      `(let* ,(binds)
         (declare (dynamic-extent ,@(names))
                  (double-float ,@(names)))
         ,@(inits)
         ,@body)))

(setf (macro-function 'with-ppc-stack-double-floats) (macro-function 'with-stack-double-floats))

(defmacro with-stack-short-floats (specs &body body)
  (collect ((binds)
	    (inits)
	    (names))
    (dolist (spec specs)
      (let ((name (first spec)))
	(binds `(,name (%alloc-misc ppc32::single-float.element-count ppc32::subtag-single-float)))
	(names name)
	(let ((init (second spec)))
	  (when init
	    (inits `(%short-float ,init ,name))))))
    `(let* ,(binds)
      (declare (dynamic-extent ,@(names))
	       (short-float ,@(names)))
      ,@(inits)
      ,@body)))

(setf (macro-function 'with-ppc-stack-short-floats) (macro-function 'with-stack-short-floats))


#| ; its in ppc-float - so there
; see "Optimizing PowerPC Code" p. 156
(defppclapmacro int-to-freg (int freg temp-freg immx immy)
  `(let ((temp -8)
         (temp.h -8)
         (temp.l -4)
         (zero -16)
         (zero.h -16)
         (zero.l -12))
     (lwi ,immx #x43300000)  ; 1075 = 1022+53 
     (lwi ,immy #x80000000)
     (stw ,immx temp.h sp) 
     (stw ,immx zero.h sp)
     (stw ,immy zero.l sp)     
     (unbox-fixnum ,immx ,int)
     (xor ,immx ,immx ,immy)          ; invert sign of int
     (stw ,immx temp.l sp)
     (lfd ,freg temp sp)
     (lfd ,temp-freg zero sp)
     (fsub ,freg ,freg ,temp-freg)))
|#

 ;;; WITH-BIGNUM-BUFFERS  --  Internal.
  ;;;
  ;;; Could do freelisting someday. NAH
  ;;;
(defmacro with-bignum-buffers (specs &body body)  ; <<
  "WITH-BIGNUM-BUFFERS ({(var size [init])}*) Form*"
  (collect ((binds)
	    (inits)
	    (names))
    (dolist (spec specs)
      (let ((name (first spec))
            (size (second spec)))
        (binds `(,name (%alloc-misc ,size ppc32::subtag-bignum)))
        (names name)          
        (let ((init (third spec)))
          (when init
            (inits `(bignum-replace ,name ,init))))))
    `(let* ,(binds)
       (declare (dynamic-extent ,@(names)))
       ,@(inits)
       ,@body)))

;;; call fn on possibly stack allocated negative of a and/or b
;;; args better be vars - we dont bother with once-only
(defmacro with-negated-bignum-buffers (a b fn)
  `(let* ((len-a (%bignum-length ,a))
          (len-b (%bignum-length ,b))
          (a-plusp (bignum-plusp ,a))
          (b-plusp (bignum-plusp ,b)))
     (declare (type bignum-index len-a len-b))
     (if (and a-plusp b-plusp)
       (,fn ,a ,b )
       (if (not a-plusp)
         (with-bignum-buffers ((a1 (1+ len-a)))
           (negate-bignum ,a nil a1)
           (if b-plusp
             (,fn a1 ,b)
             (with-bignum-buffers ((b1 (1+ len-b)))
               (negate-bignum ,b nil b1)
               (,fn a1 b1))))
         (with-bignum-buffers ((b1 (1+ len-b)))
           (negate-bignum ,b nil b1)
           (,fn ,a b1))))))

(defmacro with-one-negated-bignum-buffer (a fn)
  `(if (bignum-plusp ,a)
    (,fn ,a)
    (with-bignum-buffers ((a1 (1+ (%bignum-length ,a))))
      (negate-bignum ,a nil a1)
      (,fn a1))))


(defmacro fixnum-to-bignum-set (big fix)
  `(%fixnum-to-bignum-set ,big ,fix))

(defmacro with-small-bignum-buffers (specs &body body)
  (collect ((binds)
	    (inits)
	    (names))
    (dolist (spec specs)
      (let ((name (first spec)))
	(binds `(,name (%alloc-misc 1 ppc32::subtag-bignum)))
	(names name)
	(let ((init (second spec)))
	  (when init
	    (inits `(fixnum-to-bignum-set ,name ,init))))))
    `(let* ,(binds)
      (declare (dynamic-extent ,@(names)))
      ,@(inits)
      ,@body)))

(provide "NUMBER-MACROS")

; end of number-macros.lisp
