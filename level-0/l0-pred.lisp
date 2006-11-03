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

;; Non-portable type-predicates & such.


;; bootstrapping defs - real ones in l1-typesys, l1-clos, sysutils

(defun find-builtin-cell (type &optional create)
  (declare (ignore create))
  (cons type nil))

(defun find-class-cell (type create?)
  (declare (ignore create?))
  (cons type nil))

(defun builtin-typep (form cell)
  (typep form (car cell)))

(defun class-cell-typep (arg class-cell)
  (typep arg (car class-cell)))

(defun class-cell-find-class (class-cell errorp)
  (declare (ignore errorp)) ; AARGH can't be right
  ;(dbg-paws #x100)
  (let ((class (cdr class-cell)))
    (or class 
        (if  (fboundp 'find-class)
          (find-class (car class-cell) nil)))))

(defun %require-type-builtin (form foo)
  (declare (ignore foo))
  form)

(defun %require-type-class-cell (form cell)
  (declare (ignore cell))
  form)
  
(defun non-nil-symbol-p (x)
  (if (symbolp x) x))

(defun pathnamep (thing)
  (or (istruct-typep thing 'pathname) (istruct-typep thing 'logical-pathname)))

(defun compiled-function-p (form)
  "Return true if OBJECT is a COMPILED-FUNCTION, and NIL otherwise."
  (and (functionp form)
       (not (logbitp $lfbits-trampoline-bit (the fixnum (lfun-bits form))))))

;;; all characters are base-chars.
(defun extended-char-p (c)
  (declare (ignore c)))


;;; Some of these things are probably open-coded.
;;; The functions have to exist SOMEWHERE ...
(defun fixnump (x)
  (= (the fixnum (lisptag x)) target::tag-fixnum))

(defun bignump (x)
  (= (the fixnum (typecode x)) target::subtag-bignum))

(defun integerp (x)
  "Return true if OBJECT is an INTEGER, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode target::tag-fixnum)
        (= typecode target::subtag-bignum))))

(defun ratiop (x)
  (= (the fixnum (typecode x)) target::subtag-ratio))


(defun rationalp (x)
  "Return true if OBJECT is a RATIONAL, and NIL otherwise."
  (or (fixnump x)
      (let* ((typecode (typecode x)))
        (declare (fixnum typecode))
        #+ppc32-target
        (and (>= typecode ppc32::min-numeric-subtag)
             (<= typecode ppc32::max-rational-subtag))
        #+(or ppc64-target x8664-target)
        (cond ((= typecode target::subtag-bignum) t)
              ((= typecode target::subtag-ratio) t)))))

(defun short-float-p (x)
  (= (the fixnum (typecode x)) target::subtag-single-float))


(defun double-float-p (x)
  (= (the fixnum (typecode x)) target::subtag-double-float))

(defun floatp (x)
  "Return true if OBJECT is a FLOAT, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-single-float)
        (= typecode target::subtag-double-float))))

(defun realp (x)
  "Return true if OBJECT is a REAL, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    #+ppc32-target
    (or (= typecode ppc32::tag-fixnum)
        (and (>= typecode ppc32::min-numeric-subtag)
             (<= typecode ppc32::max-real-subtag)))
    #+ppc64-target
    (if (<= typecode ppc64::subtag-double-float)
      (logbitp (the (integer 0 #.ppc64::subtag-double-float) typecode)
               (logior (ash 1 ppc64::tag-fixnum)
                       (ash 1 ppc64::subtag-single-float)
                       (ash 1 ppc64::subtag-double-float)
                       (ash 1 ppc64::subtag-bignum)
                       (ash 1 ppc64::subtag-ratio))))
    #+x8664-target
    (if (< typecode x8664::nbits-in-word)
      (logbitp (the (integer 0 #.x8664::subtag-double-float) typecode)
               (logior (ash 1 x8664::tag-fixnum)
                       (ash 1 x8664::subtag-bignum)
                       (ash 1 x8664::tag-single-float)
                       (ash 1 x8664::subtag-double-float)
                       (ash 1 x8664::subtag-ratio))))))

(defun complexp (x)
  "Return true if OBJECT is a COMPLEX, and NIL otherwise."
  (= (the fixnum (typecode x)) target::subtag-complex))

(defun numberp (x)
  "Return true if OBJECT is a NUMBER, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    #+ppc32-target
    (or (= typecode ppc32::tag-fixnum)
        (and (>= typecode ppc32::min-numeric-subtag)
             (<= typecode ppc32::max-numeric-subtag)))
    #+ppc64-target
    (if (<= typecode ppc64::subtag-double-float)
      (logbitp (the (integer 0 #.ppc64::subtag-double-float) typecode)
               (logior (ash 1 ppc64::tag-fixnum)
                       (ash 1 ppc64::subtag-bignum)
                       (ash 1 ppc64::subtag-single-float)
                       (ash 1 ppc64::subtag-double-float)
                       (ash 1 ppc64::subtag-ratio)
                       (ash 1 ppc64::subtag-complex))))
    #+x8664-target
    (if (< typecode x8664::nbits-in-word)
      (logbitp (the (integer 0 #.x8664::subtag-double-float) typecode)
               (logior (ash 1 x8664::tag-fixnum)
                       (ash 1 x8664::subtag-bignum)
                       (ash 1 x8664::tag-single-float)
                       (ash 1 x8664::subtag-double-float)
                       (ash 1 x8664::subtag-ratio)
                       (ash 1 x8664::subtag-complex))))
    
    ))

(defun arrayp (x)
  "Return true if OBJECT is an ARRAY, and NIL otherwise."
  (>= (the fixnum (typecode x)) target::min-array-subtag))

(defun vectorp (x)
  "Return true if OBJECT is a VECTOR, and NIL otherwise."
  (>= (the fixnum (typecode x)) target::min-vector-subtag))


(defun stringp (x)
  "Return true if OBJECT is a STRING, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-vectorH)
      (setq typecode (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref x target::arrayH.flags-cell)))))
    (= typecode target::subtag-simple-base-string)))


(defun simple-base-string-p (x)
  (= (the fixnum (typecode x)) target::subtag-simple-base-string))

(defun simple-string-p (x)
  "Return true if OBJECT is a SIMPLE-STRING, and NIL otherwise."
  (= (the fixnum (typecode x)) target::subtag-simple-base-string))

(defun complex-array-p (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
            (= typecode target::subtag-vectorH))
      (not (%array-header-simple-p x)))))

(defun simple-array-p (thing)
  "Returns T if the object is a simple array, else returns NIL.
   That's why it's called SIMPLE-ARRAY-P.  Get it ?
   A simple-array may have no fill-pointer, may not be displaced,
   and may not be adjustable."
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
            (= typecode target::subtag-vectorH))
      (%array-header-simple-p thing)
      (> typecode target::subtag-vectorH))))

(defun macptrp (x)
  (= (the fixnum (typecode x)) target::subtag-macptr))


;;; Note that this is true of symbols and functions and many other
;;; things that it wasn't true of on the 68K.
(defun gvectorp (x)
  #+ppc32-target
  (= (the fixnum (logand (the fixnum (typecode x)) ppc32::fulltagmask)) ppc32::fulltag-nodeheader)
  #+ppc64-target
  (= (the fixnum (logand (the fixnum (typecode x)) ppc64::lowtagmask)) ppc64::lowtag-nodeheader)
  #+x8664-target
  (let* ((fulltag (logand (the fixnum (typecode x)) x8664::fulltagmask)))
    (declare (fixnum fulltag))
    (or (= fulltag x8664::fulltag-nodeheader-0)
        (= fulltag x8664::fulltag-nodeheader-1)))
  )


(setf (type-predicate 'gvector) 'gvectorp)

(defun ivectorp (x)
  #+ppc32-target
    (= (the fixnum (logand (the fixnum (typecode x)) ppc32::fulltagmask))
       ppc32::fulltag-immheader)
  #+ppc64-target
  (= (the fixnum (logand (the fixnum (typecode x)) ppc64::lowtagmask)) ppc64::lowtag-immheader)
  #+x8664-target
  (let* ((fulltag (logand (the fixnum (typecode x)) x8664::fulltagmask)))
    (declare (fixnum fulltag))
    (or (= fulltag x8664::fulltag-immheader-0)
        (= fulltag x8664::fulltag-immheader-1)
        (= fulltag x8664::fulltag-immheader-2)))
  )

(setf (type-predicate 'ivector) 'ivectorp)

(defun miscobjp (x)
  #+(or ppc32-target x8664-target)
  (= (the fixnum (lisptag x)) target::tag-misc)
  #+ppc64-target
  (= (the fixnum (fulltag x)) ppc64::fulltag-misc)
  )

(defun simple-vector-p (x)
  "Return true if OBJECT is a SIMPLE-VECTOR, and NIL otherwise."
  (= (the fixnum (typecode x)) target::subtag-simple-vector))

(defun base-string-p (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-simple-base-string)
        (and (= typecode target::subtag-vectorh)
             (= (the fixnum 
                  (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing target::arrayH.flags-cell))))
                target::subtag-simple-base-string)))))

(defun simple-bit-vector-p (form)
  "Return true if OBJECT is a SIMPLE-BIT-VECTOR, and NIL otherwise."
  (= (the fixnum (typecode form)) target::subtag-bit-vector))

(defun bit-vector-p (thing)
  "Return true if OBJECT is a BIT-VECTOR, and NIL otherwise."
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-bit-vector)
        (and (= typecode target::subtag-vectorh)
             (= (the fixnum 
                  (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing target::arrayH.flags-cell))))
                target::subtag-bit-vector)))))

(defun displaced-array-p (array)
  (if (%array-is-header array)
    (do* ((disp (%svref array target::arrayH.displacement-cell)
		(+ disp (the fixnum (%svref target target::arrayH.displacement-cell))))
	  (target (%svref array target::arrayH.data-vector-cell)
		  (%svref target target::arrayH.data-vector-cell)))
	 ((not (%array-is-header target))
	  (values target disp)))
    (values nil 0)))



(defun eq (x y)
  "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."
  (eq x y))


(defun cons-equal (x y)
  (declare (cons x y))
  (if (equal (car x) (car y))
    (equal (cdr x) (cdr y))))

(defun hairy-equal (x y)
  (declare (optimize (speed 3)))
  ;; X and Y are not EQL, and are both of tag target::fulltag-misc.
  (let* ((x-type (typecode x))
	 (y-type (typecode y)))
    (declare (fixnum x-type y-type))
    (if (and (>= x-type target::subtag-vectorH)
	     (>= y-type target::subtag-vectorH))
	(let* ((x-simple (if (= x-type target::subtag-vectorH)
			     (ldb target::arrayH.flags-cell-subtag-byte 
				  (the fixnum (%svref x target::arrayH.flags-cell)))
			     x-type))
	       (y-simple (if (= y-type target::subtag-vectorH)
			     (ldb target::arrayH.flags-cell-subtag-byte 
				  (the fixnum (%svref y target::arrayH.flags-cell)))
			     y-type)))
	  (declare (fixnum x-simple y-simple))
	  (if (= x-simple target::subtag-simple-base-string)
	      (if (= y-simple target::subtag-simple-base-string)
		  (locally
                      (declare (optimize (speed 3) (safety 0)))
		    (let* ((x-len (if (= x-type target::subtag-vectorH) 
                                      (%svref x target::vectorH.logsize-cell)
                                      (uvsize x)))
			   (x-pos 0)
			   (y-len (if (= y-type target::subtag-vectorH) 
                                      (%svref y target::vectorH.logsize-cell)
                                      (uvsize y)))
			   (y-pos 0))
		      (declare (fixnum x-len x-pos y-len y-pos))
		      (when (= x-type target::subtag-vectorH)
			(multiple-value-setq (x x-pos) (array-data-and-offset x)))
		      (when (= y-type target::subtag-vectorH)
			(multiple-value-setq (y y-pos) (array-data-and-offset y)))
		      (%simple-string= x y x-pos y-pos (the fixnum (+ x-pos x-len)) (the fixnum (+ y-pos y-len))))))
	      ;;Bit-vector case or fail.
	      (and (= x-simple target::subtag-bit-vector)
		   (= y-simple target::subtag-bit-vector)
		   (locally
		       (declare (optimize (speed 3) (safety 0)))
		     (let* ((x-len (if (= x-type target::subtag-vectorH) 
				       (%svref x target::vectorH.logsize-cell)
				       (uvsize x)))
			    (x-pos 0)
			    (y-len (if (= y-type target::subtag-vectorH) 
				       (%svref y target::vectorH.logsize-cell)
				       (uvsize y)))
			    (y-pos 0))
		       (declare (fixnum x-len x-pos y-len y-pos))
		       (when (= x-len y-len)
			 (when (= x-type target::subtag-vectorH)
			   (multiple-value-setq (x x-pos) (array-data-and-offset x)))
			 (when (= y-type target::subtag-vectorH)
			   (multiple-value-setq (y y-pos) (array-data-and-offset y)))
			 (do* ((i 0 (1+ i)))
			      ((= i x-len) t)
			   (declare (fixnum i))
			   (unless (= (the bit (sbit x x-pos)) (the bit (sbit y y-pos)))
			     (return))
			   (incf x-pos)
			   (incf y-pos))))))))
	(if (= x-type y-type)
	    (if (= x-type target::subtag-istruct)
		(and (let* ((structname (%svref x 0)))
		       (and (eq structname (%svref y 0))
			    (or (eq structname 'pathname)
				(eq structname 'logical-pathname))))
		     (locally
                         (declare (optimize (speed 3) (safety 0)))
		       (let* ((x-size (uvsize x)))
			 (declare (fixnum x-size))
			 (if (= x-size (the fixnum (uvsize y)))
                             (do* ((i 1 (1+ i)))
                                  ((= i x-size) t)
                               (declare (fixnum i))
                               (unless (equal (%svref x i) (%svref y i))
                                 (return))))))))))))

#+ppc32-target
(progn
(defparameter *nodeheader-types*
  #(bogus                               ; 0
    ratio                               ; 1
    bogus                               ; 2
    complex                             ; 3
    catch-frame                         ; 4
    function                            ; 5
    lisp-thread                         ; 6
    symbol                              ; 7
    lock                                ; 8
    hash-table-vector                   ; 9
    pool                                ; 10
    population                          ; 11
    package                             ; 12
    slot-vector				; 13
    standard-instance                   ; 14
    structure                           ; 15
    internal-structure                  ; 16
    value-cell                          ; 17
    xfunction                           ; 18
    array-header                        ; 19
    vector-header                       ; 20
    simple-vector                       ; 21
    bogus                               ; 22
    bogus                               ; 23
    bogus                               ; 24
    bogus                               ; 25
    bogus                               ; 26
    bogus                               ; 27
    bogus                               ; 28
    bogus                               ; 29
    bogus                               ; 30
    bogus                               ; 31
    ))


(defparameter *immheader-types*
  #(bignum                              ; 0
    short-float                         ; 1
    double-float                        ; 2
    macptr                              ; 3
    dead-macptr                         ; 4
    code-vector                         ; 5
    creole-object                       ; 6
    ;; 8-19 are unused
    xcode-vector                        ; 7
    bogus                               ; 8
    bogus                               ; 9
    bogus                               ; 10
    bogus                               ; 11
    bogus                               ; 12
    bogus                               ; 13
    bogus                               ; 14
    bogus                               ; 15
    bogus                               ; 16
    bogus                               ; 17
    bogus                               ; 18
    bogus                               ; 19
    simple-short-float-vector           ; 20
    simple-unsigned-long-vector         ; 21
    simple-signed-long-vector           ; 22
    simple-fixnum-vector                ; 23
    simple-base-string                  ; 24
    simple-unsigned-byte-vector         ; 25
    simple-signed-byte-vector           ; 26
    bogus                               ; 27
    simple-unsigned-word-vector         ; 28
    simple-signed-word-vector           ; 29
    simple-double-float-vector          ; 30
    simple-bit-vector                   ; 31
    ))

(defun %type-of (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (if (= typecode ppc32::tag-fixnum)
      'fixnum
      (if (= typecode ppc32::tag-list)
        (if thing 'cons 'null)
        (if (= typecode ppc32::tag-imm)
          (if (base-char-p thing)
            'base-char
            'immediate)
	  (if (= typecode ppc32::subtag-macptr)
	    (if (classp thing)
	      (class-name thing)
	      'macptr)
	    (let* ((tag-type (logand typecode ppc32::full-tag-mask))
		   (tag-val (ash typecode (- ppc32::ntagbits))))
	      (declare (fixnum tag-type tag-val))
	      (if (/= tag-type ppc32::fulltag-nodeheader)
		(%svref *immheader-types* tag-val)
		(let ((type (%svref *nodeheader-types* tag-val)))
		  (if (eq type 'function)
		    (let ((bits (lfun-bits thing)))
		      (declare (fixnum bits))
		      (if (logbitp $lfbits-trampoline-bit bits)
			(let ((inner-fn (closure-function thing)))
                          (if (neq inner-fn thing)
                            (let ((inner-bits (lfun-bits inner-fn)))
                              (if (logbitp $lfbits-method-bit inner-bits)
                                'compiled-lexical-closure
                                (if (logbitp $lfbits-gfn-bit inner-bits)
                                  'standard-generic-function ; not precisely - see class-of
                                  (if (logbitp  $lfbits-cm-bit inner-bits)
                                    'combined-method
                                    'compiled-lexical-closure))))
                            'compiled-lexical-closure))
                        (if (logbitp  $lfbits-method-bit bits)
                          'method-function          
                          'compiled-function)))
		    (if (eq type 'lock)
		      (or (uvref thing ppc32::lock.kind-cell)
			  type)
		      type)))))))))))

);#+ppc32-target

#+ppc64-target
(progn
(defparameter *immheader-types*
  #(bogus
    bogus
    code-vector
    bogus
    bogus
    bogus
    xcode-vector
    macptr
    bogus
    bogus
    bignum
    dead-macptr
    bogus
    bogus
    double-float
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-signed-byte-vector
    simple-signed-word-vector
    simple-signed-long-vector
    simple-signed-doubleword-vector
    simple-unsigned-byte-vector
    simple-unsigned-word-vector
    simple-unsigned-long-vector
    simple-unsigned-doubleword-vector
    bogus
    bogus
    simple-short-float-vector
    simple-fixnum-vector
    bogus
    bogus
    bogus
    simple-double-float-vector
    bogus
    bogus
    simple-base-string
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-bit-vector
    bogus
    bogus))

(defparameter *nodeheader-types*
    #(function
      catch-frame
      slot-vector
      ratio
      symbol
      lisp-thread
      standard-instance
      complex
      bogus
      lock
      structure
      bogus
      bogus
      hash-vector
      internal-structure
      bogus
      bogus
      pool
      value-cell
      bogus
      bogus
      population
      xfunction
      bogus
      bogus
      package
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      array-header
      vector-header
      simple-vector
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      )
  )


(defun %type-of (thing)
  (if (null thing)
    'null
    (let* ((typecode (typecode thing)))
      (declare (fixnum typecode))
      (cond ((= typecode ppc64::tag-fixnum) 'fixnum)
            ((= typecode ppc64::fulltag-cons) 'cons)
            ((= typecode ppc64::subtag-character) 'character)
            ((= typecode ppc64::subtag-single-float) 'short-float)
            (t (let* ((lowtag (logand typecode ppc64::lowtagmask)))
                 (declare (fixnum lowtag))
                 (cond ((= lowtag ppc64::lowtag-immheader)
                        (%svref *immheader-types* (ash typecode -2)))
                       ((= lowtag ppc64::lowtag-nodeheader)
                        (let* ((type (%svref *nodeheader-types*
                                             (ash typecode -2))))
                          (cond ((eq type 'function)
                                 (let ((bits (lfun-bits thing)))
                                   (declare (fixnum bits))
                                   (if (logbitp $lfbits-trampoline-bit bits)
                                     (let ((inner-fn (closure-function thing)))
                                         (if (neq inner-fn thing)
                                           (let ((inner-bits (lfun-bits inner-fn)))
                                             (if (logbitp $lfbits-method-bit inner-bits)
                                               'compiled-lexical-closure
                                               (if (logbitp $lfbits-gfn-bit inner-bits)
                                                 'standard-generic-function ; not precisely - see class-of
                                                 (if (logbitp  $lfbits-cm-bit inner-bits)
                                                   'combined-method
                                                   'compiled-lexical-closure))))
                                           'compiled-lexical-closure))
                                     (if (logbitp  $lfbits-method-bit bits)
                                       'method-function          
                                       'compiled-function))))
                                ((eq type 'lock)
                                 (or (uvref thing ppc64::lock.kind-cell)
                                     type))
                                (t type))))
                       (t 'immediate))))))))
);#+ppc64-target



#+x8664-target
(progn
(defparameter *nodeheader-0-types*
  #(bogus
    symbol-vector
    catch-frame
    hash-vector
    pool
    population
    package
    slot-vector
    lisp-thread
    function-vector                                        ;8
    array-header
    bogus
    bogus
    bogus
    bogus
    bogus
    ))

(defparameter *nodeheader-1-types*
  #(bogus
    ratio
    complex
    structure
    istruct
    value-cell
    xfunction
    lock
    instance
    bogus
    vector-header
    simple-vector
    bogus
    bogus
    bogus
    bogus
    ))

(defparameter *immheader-0-types*
  #(bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-signed-word-vector
    simple-unsigned-word-vector
    bogus
    simple-signed-byte-vector
    simple-unsigned-byte-vector
    bit-vector))

(defparameter *immheader-1-types*
  #(bogus
    bignum
    double-float
    xcode-vector
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-base-string
    simple-signed-long-vector
    simple-unsigned-long-vector
    single-float-vector))

(defparameter *immheader-2-types*
  #(bogus
    macptr
    dead-macptr
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-fixnum-vector
    simple-signed-doubleword-vector
    simple-unsigned-doubleword-vector
    double-float-vector))


(defparameter *x8664-%type-of-functions* nil)

(let* ((fixnum (lambda (x) (declare (ignore x)) 'fixnum))
       (tra (lambda (x) (declare (ignore x)) 'tagged-return-address))
       (bogus (lambda (x) (declare (ignore x)) 'bogus)))
  (setq *x8664-%type-of-functions*
        (vector
         fixnum                         ;0
         (lambda (x) (declare (ignore x)) 'short-float) ;1
         (lambda (x) (if (characterp x) 'character 'immediate)) ;2
         (lambda (x) (declare (ignore x)) 'cons) ;3
         tra                            ;4
         bogus                          ;5
         bogus                          ;6
         bogus                          ;7
         fixnum                         ;8
         bogus                          ;9
         bogus                          ;10
         (lambda (x) (declare (ignore x)) 'null) ;11
         tra                            ;12
         (lambda (x) (let* ((typecode (typecode x)) 
                            (low4 (logand typecode x8664::fulltagmask))
                            (high4 (ash typecode (- x8664::ntagbits))))
                       (declare (type (unsigned-byte 8) typecode)
                                (type (unsigned-byte 4) low4 high4))
                       (let* ((name
                               (cond ((= low4 x8664::fulltag-immheader-0)
                                      (%svref *immheader-0-types* high4))
                                     ((= low4 x8664::fulltag-immheader-1)
                                      (%svref *immheader-1-types* high4))
                                     ((= low4 x8664::fulltag-immheader-2)
                                      (%svref *immheader-2-types* high4))
                                     ((= low4 x8664::fulltag-nodeheader-0)
                                      (%svref *nodeheader-0-types* high4))
                                     ((= low4 x8664::fulltag-nodeheader-1)
                                      (%svref *nodeheader-1-types* high4))
                                     (t 'bogus))))
                         (or (and (eq name 'lock)
                                  (uvref x x8664::lock.kind-cell))
                             name)))) ;13
         (lambda (x) (declare (ignore x)) 'symbol) ;14
         (lambda (thing)
           (let ((bits (lfun-bits thing)))
             (declare (fixnum bits))
             (if (logbitp $lfbits-trampoline-bit bits)
               (let ((inner-fn (closure-function thing)))
                 (if (neq inner-fn thing)
                   (let ((inner-bits (lfun-bits inner-fn)))
                     (if (logbitp $lfbits-method-bit inner-bits)
                       'compiled-lexical-closure
                       (if (logbitp $lfbits-gfn-bit inner-bits)
                         'standard-generic-function ; not precisely - see class-of
                         (if (logbitp  $lfbits-cm-bit inner-bits)
                           'combined-method
                           'compiled-lexical-closure))))
                   'compiled-lexical-closure))
               (if (logbitp  $lfbits-method-bit bits)
                 'method-function          
                 'compiled-function))))))) ;15
                                      
       


  
(defun %type-of (thing)
  (let* ((f (fulltag thing)))
    (funcall (%svref *x8664-%type-of-functions* f) thing)))

        

);#+x8664-target
      

;;; real machine specific huh
(defun consp (x)
  "Return true if OBJECT is a CONS, and NIL otherwise."
  (consp x))

(defun characterp (arg)
  "Return true if OBJECT is a CHARACTER, and NIL otherwise."
  (characterp arg))

(defun base-char-p (c)
  (base-char-p c))




(defun structurep (form)
  "True if the given object is a named structure, Nil otherwise."
  (= (the fixnum (typecode form)) target::subtag-struct))

(defun istructp (form)
  (= (the fixnum (typecode form)) target::subtag-istruct))

(defun structure-typep (thing type)
  (if (= (the fixnum (typecode thing)) target::subtag-struct)
    (if (memq type (%svref thing 0))
      t)))


(defun istruct-typep (thing type)
  (if (= (the fixnum (typecode thing)) target::subtag-istruct)
    (eq (%svref thing 0) type)))

(defun symbolp (thing)
  "Return true if OBJECT is a SYMBOL, and NIL otherwise."
  #+ppc32-target
  (if thing
    (= (the fixnum (typecode thing)) ppc32::subtag-symbol)
    t)
  #+ppc64-target
  (= (the fixnum (typecode thing)) ppc64::subtag-symbol)
  #+x8664-target
  (if thing
    (= (the fixnum (lisptag thing)) x8664::tag-symbol)
    t)
  )
      
(defun packagep (thing)
  (= (the fixnum (typecode thing)) target::subtag-package))

;;; 1 if by land, 2 if by sea.
(defun sequence-type (x)
  (unless (>= (the fixnum (typecode x)) target::min-vector-subtag)
    (or (listp x)
        (report-bad-arg x 'sequence))))

(defun uvectorp (x)
  (= (the fixnum (fulltag x)) target::fulltag-misc))

(setf (type-predicate 'uvector) 'uvectorp)
