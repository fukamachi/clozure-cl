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
  (and (functionp form)
       (not (logbitp $lfbits-trampoline-bit (the fixnum (lfun-bits form))))))

; all characters are base-chars.
(defun extended-char-p (c)
  (declare (ignore c)))


; Some of these things are probably open-coded.
; The functions have to exist SOMEWHERE ...
(defun fixnump (x)
  (= (the fixnum (lisptag x)) arch::tag-fixnum))

(defun bignump (x)
  (= (the fixnum (typecode x)) arch::subtag-bignum))

(defun integerp (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode arch::tag-fixnum)
        (= typecode arch::subtag-bignum))))

(defun ratiop (x)
  (= (the fixnum (typecode x)) arch::subtag-ratio))


(defun rationalp (x)
  (or (fixnump x)
      (let* ((typecode (typecode x)))
        (declare (fixnum typecode))
        (and (>= typecode arch::min-numeric-subtag)
             (<= typecode arch::max-rational-subtag)))))



(defun short-float-p (x)
  (= (the fixnum (typecode x)) arch::subtag-single-float))


(defun double-float-p (x)
  (= (the fixnum (typecode x)) arch::subtag-double-float))

(defun floatp (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (and (>= typecode arch::min-float-subtag)
         (<= typecode arch::max-float-subtag))))

(defun realp (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode arch::tag-fixnum)
        (and (>= typecode arch::min-numeric-subtag)
             (<= typecode arch::max-real-subtag)))))

(defun complexp (x)
  (= (the fixnum (typecode x)) arch::subtag-complex))

(defun numberp (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode arch::tag-fixnum)
        (and (>= typecode arch::min-numeric-subtag)
             (<= typecode arch::max-numeric-subtag)))))

(defun arrayp (x)
  (>= (the fixnum (typecode x)) arch::min-array-subtag))

(defun vectorp (x)
  (>= (the fixnum (typecode x)) arch::min-vector-subtag))


(defun stringp (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (= typecode arch::subtag-vectorH)
      (setq typecode (ldb arch::arrayH.flags-cell-subtag-byte (the fixnum (%svref x arch::arrayH.flags-cell)))))
    (= typecode arch::subtag-simple-base-string)))


(defun simple-base-string-p (x)
  (= (the fixnum (typecode x)) arch::subtag-simple-base-string))

(defun simple-string-p (x)
  (= (the fixnum (typecode x)) arch::subtag-simple-base-string))

(defun complex-array-p (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (or (= typecode arch::subtag-arrayH)
            (= typecode arch::subtag-vectorH))
      (not (%array-header-simple-p x)))))

(setf (type-predicate 'complex-array) 'complex-array-p)

(defun simple-array-p (thing)
  "Returns T if the object is a simple array, else returns NIL.
   That's why it's called SIMPLE-ARRAY-P.  Get it ?
   A simple-array may have no fill-pointer, may not be displaced,
   and may not be adjustable."
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (if (or (= typecode arch::subtag-arrayH)
            (= typecode arch::subtag-vectorH))
      (%array-header-simple-p thing)
      (> typecode arch::subtag-vectorH))))

(defun macptrp (x)
  (= (the fixnum (typecode x)) arch::subtag-macptr))


; Note that this is true of symbols and functions and many other
; things that it wasn't true of on the 68K.
(defun gvectorp (x)
  (= (the fixnum (logand (the fixnum (typecode x)) arch::fulltagmask)) arch::fulltag-nodeheader))

(setf (type-predicate 'gvector) 'gvectorp)

(defun miscobjp (x)
  (= (the fixnum (lisptag x)) arch::tag-misc))

(defun simple-vector-p (x)
  (= (the fixnum (typecode x)) arch::subtag-simple-vector))

(defun base-string-p (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode arch::subtag-simple-base-string)
        (and (= typecode arch::subtag-vectorh)
             (= (the fixnum 
                  (ldb arch::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing arch::arrayH.flags-cell))))
                arch::subtag-simple-base-string)))))

(defun simple-bit-vector-p (form)
  (= (the fixnum (typecode form)) arch::subtag-bit-vector))

(defun bit-vector-p (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode arch::subtag-bit-vector)
        (and (= typecode arch::subtag-vectorh)
             (= (the fixnum 
                  (ldb arch::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing arch::arrayH.flags-cell))))
                arch::subtag-bit-vector)))))

(defun displaced-array-p (array)
  (if (%array-is-header array)
    (values (%svref array arch::arrayH.data-vector-cell)
            (%svref array arch::arrayH.displacement-cell))
    (values nil 0)))

(setf (type-predicate 'displaced-array) 'displaced-array-p)


(defun eq (x y) (eq x y))


(defun cons-equal (x y)
  (declare (cons x y))
  (if (equal (car x) (car y))
    (equal (cdr x) (cdr y))))

(defun hairy-equal (x y)
  (declare (optimize (speed 3)))
  ; X and Y are not EQL, and are both of tag arch::fulltag-misc.
  (let* ((x-type (typecode x))
	 (y-type (typecode y)))
    (declare (fixnum x-type y-type))
    (if (and (>= x-type arch::subtag-vectorH)
	     (>= y-type arch::subtag-vectorH))
	(let* ((x-simple (if (= x-type arch::subtag-vectorH)
			     (ldb arch::arrayH.flags-cell-subtag-byte 
				  (the fixnum (%svref x arch::arrayH.flags-cell)))
			     x-type))
	       (y-simple (if (= y-type arch::subtag-vectorH)
			     (ldb arch::arrayH.flags-cell-subtag-byte 
				  (the fixnum (%svref y arch::arrayH.flags-cell)))
			     y-type)))
	  (declare (fixnum x-simple y-simple))
	  (if (= x-simple arch::subtag-simple-base-string)
	      (if (= y-simple arch::subtag-simple-base-string)
		  (locally
                      (declare (optimize (speed 3) (safety 0)))
		    (let* ((x-len (if (= x-type arch::subtag-vectorH) 
                                      (%svref x arch::vectorH.logsize-cell)
                                      (uvsize x)))
			   (x-pos 0)
			   (y-len (if (= y-type arch::subtag-vectorH) 
                                      (%svref y arch::vectorH.logsize-cell)
                                      (uvsize y)))
			   (y-pos 0))
		      (declare (fixnum x-len x-pos y-len y-pos))
		      (when (= x-type arch::subtag-vectorH)
			(multiple-value-setq (x x-pos) (array-data-and-offset x)))
		      (when (= y-type arch::subtag-vectorH)
			(multiple-value-setq (y y-pos) (array-data-and-offset y)))
		      (%simple-string= x y x-pos y-pos (the fixnum (+ x-pos x-len)) (the fixnum (+ y-pos y-len))))))
	      ;;Bit-vector case or fail.
	      (and (= x-simple arch::subtag-bit-vector)
		   (= y-simple arch::subtag-bit-vector)
		   (locally
		       (declare (optimize (speed 3) (safety 0)))
		     (let* ((x-len (if (= x-type arch::subtag-vectorH) 
				       (%svref x arch::vectorH.logsize-cell)
				       (uvsize x)))
			    (x-pos 0)
			    (y-len (if (= y-type arch::subtag-vectorH) 
				       (%svref y arch::vectorH.logsize-cell)
				       (uvsize y)))
			    (y-pos 0))
		       (declare (fixnum x-len x-pos y-len y-pos))
		       (when (= x-len y-len)
			 (when (= x-type arch::subtag-vectorH)
			   (multiple-value-setq (x x-pos) (array-data-and-offset x)))
			 (when (= y-type arch::subtag-vectorH)
			   (multiple-value-setq (y y-pos) (array-data-and-offset y)))
			 (do* ((i 0 (1+ i)))
			      ((= i x-len) t)
			   (declare (fixnum i))
			   (unless (= (the bit (sbit x x-pos)) (the bit (sbit y y-pos)))
			     (return))
			   (incf x-pos)
			   (incf y-pos))))))))
	(if (= x-type y-type)
	    (if (= x-type arch::subtag-istruct)
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
    xcode-vecor                         ; 7
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
    (if (= typecode arch::tag-fixnum)
      'fixnum
      (if (= typecode arch::tag-list)
        (if thing 'cons 'null)
        (if (= typecode arch::tag-imm)
          (if (base-char-p thing)
            'base-char
            'immediate)
          (let* ((tag-type (logand typecode arch::full-tag-mask))
                 (tag-val (ash typecode (- arch::ntagbits))))
            (declare (fixnum tag-type tag-val))
            ;; When we get to the point that we can differentiate between
            ;; different types of functions, do so.
            (if (/= tag-type arch::fulltag-nodeheader)
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
                    (or (uvref thing arch::lock.kind-cell)
                        type)
                    type))))))))))


; real machine specific huh
(defun consp (x) (consp x))

(defun characterp (arg)
  (characterp arg))

(defun base-char-p (c)
  (base-char-p c))




(defun structurep (form)
  "True if the given object is a named structure, Nil otherwise."
  (= (the fixnum (typecode form)) arch::subtag-struct))

(defun istructp (form)
  (= (the fixnum (typecode form)) arch::subtag-istruct))

(defun structure-typep (thing type)
  (if (= (the fixnum (typecode thing)) arch::subtag-struct)
    (if (memq type (%svref thing 0))
      t)))


(defun istruct-typep (thing type)
  (if (= (the fixnum (typecode thing)) arch::subtag-istruct)
    (eq (%svref thing 0) type)))

(defun symbolp (thing)
  (if thing
    (= (the fixnum (typecode thing)) arch::subtag-symbol)
    t))

(defun packagep (thing)
  (= (the fixnum (typecode thing)) arch::subtag-package))

; 1 if by land, 2 if by sea.
(defun sequence-type (x)
  (unless (>= (the fixnum (typecode x)) arch::min-vector-subtag)
    (or (listp x)
        (report-bad-arg x 'sequence))))

;; I'm really skeptical about anything that calls UVECTORP
;; (in that I'm afraid that it thinks that it knows what's
;; a "uvector" and what isn't.
(defun uvectorp (x)
  (= (the fixnum (lisptag x)) arch::tag-misc))
