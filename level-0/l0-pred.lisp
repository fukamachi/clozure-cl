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
  (= (the fixnum (lisptag x)) ppc32::tag-fixnum))

(defun bignump (x)
  (= (the fixnum (typecode x)) ppc32::subtag-bignum))

(defun integerp (x)
  "Return true if OBJECT is an INTEGER, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode ppc32::tag-fixnum)
        (= typecode ppc32::subtag-bignum))))

(defun ratiop (x)
  (= (the fixnum (typecode x)) ppc32::subtag-ratio))


(defun rationalp (x)
  "Return true if OBJECT is a RATIONAL, and NIL otherwise."
  (or (fixnump x)
      (let* ((typecode (typecode x)))
        (declare (fixnum typecode))
        (and (>= typecode ppc32::min-numeric-subtag)
             (<= typecode ppc32::max-rational-subtag)))))



(defun short-float-p (x)
  (= (the fixnum (typecode x)) ppc32::subtag-single-float))


(defun double-float-p (x)
  (= (the fixnum (typecode x)) ppc32::subtag-double-float))

(defun floatp (x)
  "Return true if OBJECT is a FLOAT, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (and (>= typecode ppc32::min-float-subtag)
         (<= typecode ppc32::max-float-subtag))))

(defun realp (x)
  "Return true if OBJECT is a REAL, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode ppc32::tag-fixnum)
        (and (>= typecode ppc32::min-numeric-subtag)
             (<= typecode ppc32::max-real-subtag)))))

(defun complexp (x)
  "Return true if OBJECT is a COMPLEX, and NIL otherwise."
  (= (the fixnum (typecode x)) ppc32::subtag-complex))

(defun numberp (x)
  "Return true if OBJECT is a NUMBER, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode ppc32::tag-fixnum)
        (and (>= typecode ppc32::min-numeric-subtag)
             (<= typecode ppc32::max-numeric-subtag)))))

(defun arrayp (x)
  "Return true if OBJECT is an ARRAY, and NIL otherwise."
  (>= (the fixnum (typecode x)) ppc32::min-array-subtag))

(defun vectorp (x)
  "Return true if OBJECT is a VECTOR, and NIL otherwise."
  (>= (the fixnum (typecode x)) ppc32::min-vector-subtag))


(defun stringp (x)
  "Return true if OBJECT is a STRING, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (= typecode ppc32::subtag-vectorH)
      (setq typecode (ldb ppc32::arrayH.flags-cell-subtag-byte (the fixnum (%svref x ppc32::arrayH.flags-cell)))))
    (= typecode ppc32::subtag-simple-base-string)))


(defun simple-base-string-p (x)
  (= (the fixnum (typecode x)) ppc32::subtag-simple-base-string))

(defun simple-string-p (x)
  "Return true if OBJECT is a SIMPLE-STRING, and NIL otherwise."
  (= (the fixnum (typecode x)) ppc32::subtag-simple-base-string))

(defun complex-array-p (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (or (= typecode ppc32::subtag-arrayH)
            (= typecode ppc32::subtag-vectorH))
      (not (%array-header-simple-p x)))))

(defun simple-array-p (thing)
  "Returns T if the object is a simple array, else returns NIL.
   That's why it's called SIMPLE-ARRAY-P.  Get it ?
   A simple-array may have no fill-pointer, may not be displaced,
   and may not be adjustable."
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (if (or (= typecode ppc32::subtag-arrayH)
            (= typecode ppc32::subtag-vectorH))
      (%array-header-simple-p thing)
      (> typecode ppc32::subtag-vectorH))))

(defun macptrp (x)
  (= (the fixnum (typecode x)) ppc32::subtag-macptr))


;;; Note that this is true of symbols and functions and many other
;;; things that it wasn't true of on the 68K.
(defun gvectorp (x)
  (= (the fixnum (logand (the fixnum (typecode x)) ppc32::fulltagmask)) ppc32::fulltag-nodeheader))

(setf (type-predicate 'gvector) 'gvectorp)

(defun ivectorp (x)
  (= (the fixnum (logand (the fixnum (typecode x)) ppc32::fulltagmask))
     ppc32::fulltag-immheader))

(setf (type-predicate 'ivector) 'ivectorp)

(defun miscobjp (x)
  #+ppc32-target
  (= (the fixnum (lisptag x)) ppc32::tag-misc)
  #+ppc64-target
  (= (the fixnum (fulltag x)) ppc64::fulltag-misc)
  )

(defun simple-vector-p (x)
  "Return true if OBJECT is a SIMPLE-VECTOR, and NIL otherwise."
  (= (the fixnum (typecode x)) ppc32::subtag-simple-vector))

(defun base-string-p (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode ppc32::subtag-simple-base-string)
        (and (= typecode ppc32::subtag-vectorh)
             (= (the fixnum 
                  (ldb ppc32::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing ppc32::arrayH.flags-cell))))
                ppc32::subtag-simple-base-string)))))

(defun simple-bit-vector-p (form)
  "Return true if OBJECT is a SIMPLE-BIT-VECTOR, and NIL otherwise."
  (= (the fixnum (typecode form)) ppc32::subtag-bit-vector))

(defun bit-vector-p (thing)
  "Return true if OBJECT is a BIT-VECTOR, and NIL otherwise."
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode ppc32::subtag-bit-vector)
        (and (= typecode ppc32::subtag-vectorh)
             (= (the fixnum 
                  (ldb ppc32::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing ppc32::arrayH.flags-cell))))
                ppc32::subtag-bit-vector)))))

(defun displaced-array-p (array)
  (if (%array-is-header array)
    (do* ((disp (%svref array ppc32::arrayH.displacement-cell)
		(+ disp (the fixnum (%svref target ppc32::arrayH.displacement-cell))))
	  (target (%svref array ppc32::arrayH.data-vector-cell)
		  (%svref target ppc32::arrayH.data-vector-cell)))
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
    svar                                ; 19
    array-header                        ; 20
    vector-header                       ; 21
    simple-vector                       ; 22
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
    ;; 8-20 are unused
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
    bogus                               ; 20
    simple-short-float-vector           ; 21
    simple-unsigned-long-vector         ; 22
    simple-signed-long-vector           ; 23
    simple-unsigned-byte-vector         ; 24
    simple-signed-byte-vector           ; 25
    simple-base-string                  ; 26
    *unused*                            ; 27
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
			(if (logbitp $lfbits-evaluated-bit bits)
			  'interpreted-lexical-closure
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
			      'compiled-lexical-closure)))
			(if (logbitp $lfbits-evaluated-bit bits)
			  (if (logbitp $lfbits-method-bit bits)
			    'interpreted-method-function
			    'interpreted-function)
			  (if (logbitp  $lfbits-method-bit bits)
			    'method-function          
			    'compiled-function))))
		    (if (eq type 'lock)
		      (or (uvref thing ppc32::lock.kind-cell)
			  type)
		      type)))))))))))


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
  (= (the fixnum (typecode thing)) ppc64::subtag-symbol))
      
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
