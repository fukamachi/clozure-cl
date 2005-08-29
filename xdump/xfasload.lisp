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


(eval-when (:compile-toplevel :execute)
(require "FASLENV" "ccl:xdump;faslenv")



(defmacro defxloadfaslop (n arglist &body body)
  `(setf (svref *xload-fasl-dispatch-table* ,n)
         #'(lambda ,arglist ,@body)))

(defmacro xload-copy-faslop (n)
  `(let* ((n ,n))
     (setf (svref *xload-fasl-dispatch-table* n)
           (svref *fasl-dispatch-table* n))))
)


;;; I'm not sure that there's a better way to do this.

(defparameter *xload-show-cold-load-functions* nil "Set to T when debugging")

(defparameter *xload-target-nil* nil)
(defparameter *xload-target-fixnumshift* nil)
(defparameter *xload-target-fulltag-cons* nil)
(defparameter *xload-target-fulltag-misc* nil)
(defparameter *xload-target-misc-data-offset* nil)
(defparameter *xload-target-fulltagmask* nil)
(defparameter *xload-target-fulltag-cons* nil)
(defparameter *xload-target-cons-size* nil)
(defparameter *xload-target-car-offset* nil)
(defparameter *xload-target-cdr-offset* nil)
(defparameter *xload-target-misc-header-offset* nil)
(defparameter *xload-target-misc-subtag-offset* nil)
(defparameter *xload-target-unbound-marker* nil)
(defparameter *xload-target-subtag-char* nil)
(defparameter *xload-target-charcode-shift* nil)

(defvar *xload-backends* nil)
(defvar *xload-default-backend*)
(defvar *xload-target-backend*)

(defparameter *xload-image-base-address* nil)

(defparameter *xload-purespace-reserve* #x04000000)
(defparameter *xload-static-space-address* (ash 1 12))
(defparameter *xload-static-space-size* (ash 8 10))
(defparameter *xload-readonly-space-address* nil)
(defparameter *xload-readonly-space-size* (ash 1 18))
(defparameter *xload-dynamic-space-address* nil)
(defparameter *xload-dynamic-space-size* (ash 1 18))

(defstruct backend-xload-info
  name
  macro-apply-code-function
  closure-trampoline-code
  udf-code
  default-image-name
  default-startup-file-name
  subdirs
  compiler-target-name
  image-base-address
)
(defun setup-xload-target-parameters ()
  (setq *xload-image-base-address*
        (backend-xload-info-image-base-address
         *xload-target-backend*))
  (setq *xload-readonly-space-address* *xload-image-base-address*)
  (setq *xload-dynamic-space-address*
        (+ *xload-image-base-address*
           *xload-purespace-reserve*))
  (setq *xload-target-nil*
          (target-arch-case
           (:ppc32 ppc32::nil-value)
           (:ppc64 ppc64::nil-value)))
  (setq *xload-target-unbound-marker*
        (target-arch-case
         (:ppc32 ppc32::unbound-marker)
         (:ppc64 ppc64::unbound-marker)))
  (setq *xload-target-misc-header-offset*
        (target-arch-case
         (:ppc32 ppc32::misc-header-offset)
         (:ppc64 ppc64::misc-header-offset)))
  (setq *xload-target-misc-subtag-offset*
        (target-arch-case
         (:ppc32 ppc32::misc-subtag-offset)
         (:ppc64 ppc64::misc-subtag-offset)))
  (setq *xload-target-fixnumshift*
        (target-arch-case
         (:ppc32 ppc32::fixnumshift)
         (:ppc64 ppc64::fixnumshift)))
  (setq *xload-target-fulltag-cons*
        (target-arch-case
         (:ppc32 ppc32::fulltag-cons)
         (:ppc64 ppc64::fulltag-cons)))
  (setq *xload-target-car-offset*
        (target-arch-case
         (:ppc32 ppc32::cons.car)
         (:ppc64 ppc64::cons.car)))
  (setq *xload-target-cdr-offset*
        (target-arch-case
         (:ppc32 ppc32::cons.cdr)
         (:ppc64 ppc64::cons.cdr)))
  (setq *xload-target-cons-size*
        (target-arch-case
         (:ppc32 ppc32::cons.size)
         (:ppc64 ppc64::cons.size)))
  (setq *xload-target-fulltagmask*
        (target-arch-case
         (:ppc32 ppc32::fulltagmask)
         (:ppc64 ppc64::fulltagmask)))
  (setq *xload-target-misc-data-offset*
        (target-arch-case
         (:ppc32 ppc32::misc-data-offset)
         (:ppc64 ppc64::misc-data-offset)))
  (setq *xload-target-fulltag-misc*
        (target-arch-case
         (:ppc32 ppc32::fulltag-misc)
         (:ppc64 ppc64::fulltag-misc)))
  (setq *xload-target-subtag-char*
        (target-arch-case
         (:ppc32 ppc32::subtag-character)
         (:ppc64 ppc64::subtag-character)))
  (setq *xload-target-charcode-shift*
        (target-arch-case
         (:ppc32 ppc32::charcode-shift)
         (:ppc64 ppc64::charcode-shift)))
                 
  )







(defun xload-target-consp (addr)
  (target-arch-case
   (:ppc32 (= ppc32::fulltag-cons (logand addr ppc32::fulltagmask)))
   (:ppc64 (= ppc64::fulltag-cons (logand addr ppc64::fulltagmask)))))


(defun xload-target-listp (addr)
  (or (= addr *xload-target-nil*)
      (xload-target-consp addr)))





(defun find-xload-backend (target)
  (find target *xload-backends* :key #'backend-xload-info-name))

(defun add-xload-backend (b)
  (let* ((already (find-xload-backend (backend-xload-info-name b))))
    (when already
      (setq *xload-backends* (remove already *xload-backends*)))
    (push b *xload-backends*)))



(defun make-xload-header (element-count subtag)
  (logior (ash element-count target::num-subtag-bits) subtag))



(defparameter *xload-record-source-file-p* t)

(defun xload-symbol-header ()
  (target-arch-case
   (:ppc32 (make-xload-header ppc32::symbol.element-count
                              ppc32::subtag-symbol))
   (:ppc64 (make-xload-header ppc64::symbol.element-count
                              ppc64::subtag-symbol))))

(defparameter *xload-fasl-dispatch-table* (make-array (length *fasl-dispatch-table*)
                                                     :initial-element #'%bad-fasl))



(defun u32-ref (u32v byte-offset)
  (declare (type (simple-array (unsigned-byte 32) (*)) u32v)
           (fixnum byte-offset))
  (locally (declare (optimize (speed 3) (safety 0)))
    (aref u32v (ash byte-offset -2))))

(defun (setf u32-ref) (new u32v byte-offset)
  (declare (type (simple-array (unsigned-byte 32) (*)) u32v)
           (fixnum byte-offset))
  (locally (declare (optimize (speed 3) (safety 0)))
    (setf (aref u32v (ash byte-offset -2))
          (logand new #xffffffff))))

(defun u16-ref (u16v byte-offset)
  (declare (type (simple-array (unsigned-byte 16) (*)) u16v)
           (fixnum byte-offset))
  (locally (declare (optimize (speed 3) (safety 0)))
    (aref u16v (ash byte-offset -1))))

(defun (setf u16-ref) (new u16v byte-offset)
  (declare (type (simple-array (unsigned-byte 16) (*)) u16v)
           (fixnum byte-offset))
  (locally (declare (optimize (speed 3) (safety 0)))
    (setf (aref u16v (ash byte-offset -1))
          new)))

(defun u8-ref (u8v byte-offset)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8v)
           (fixnum byte-offset))
  (locally (declare (optimize (speed 3) (safety 0)))
    (aref u8v byte-offset)))

(defun (setf u8-ref) (new u8v byte-offset)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8v)
           (fixnum byte-offset))
  (locally (declare (optimize (speed 3) (safety 0)))
    (setf (aref u8v byte-offset) new)))

(defun natural-ref (u32v byte-offset)
  (target-arch-case
   (:ppc32 (u32-ref u32v byte-offset))
   (:ppc64 (dpb (u32-ref u32v byte-offset)
                (byte 32 32)
                (u32-ref u32v (+ byte-offset 4))))))

(defun (setf natural-ref) (new u32v byte-offset)
  (target-arch-case
   (:ppc32 (setf (u32-ref u32v byte-offset) new))
   (:ppc64 (setf (u32-ref u32v byte-offset) (ldb (byte 32 32) new))
           (setf (u32-ref u32v (+ byte-offset 4)) (ldb (byte 32 0) new))
           new)))


(defun xload-aligned-uvector-size (nbytes)
  (target-arch-case
   (:ppc32 (logand (lognot 7) (+ 4 7 nbytes )))
   (:ppc64 (logand (lognot 15) (+ 15 8 nbytes)))))

(defparameter *xload-spaces* nil)
(defparameter *xload-image-file* nil)
(defvar *xload-image-file-name*)
(defvar *xload-startup-file*)
(defvar *xload-svar-alist* nil)


(defstruct xload-space
  (vaddr 0)
  (size (ash 1 18))
  (lowptr 0)
  (data nil)
  (code 0))

(defmethod print-object ((s xload-space) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a @#x~8,'0x len = ~d" (xload-space-code s) (xload-space-vaddr s) (xload-space-lowptr s))))

;;; :constructor ... :constructor ... <gasp> ... must remember ... :constructor

(defun init-xload-space (vaddr size code)
  (let* ((nfullwords (ash (+ size 3) -2))
         (space (make-xload-space :vaddr vaddr
                                 :size size
                                 :data (make-array nfullwords
                                                   :element-type '(unsigned-byte 32)
                                                   :initial-element 0)
				 :code code)))
    (push space *xload-spaces*)
    space))

; Nilreg-relative symbols.

(defparameter %builtin-functions%
  #(+-2 --2 *-2 /-2 =-2 /=-2 >-2 >=-2 <-2 <=-2 eql length sequence-type
        assq memq logbitp logior-2 logand-2 ash 
        %negate logxor-2 %aref1 %aset1
        ; add more
        )
  "Symbols naming fixed-arg, single-valued functions")
        
(defparameter *xload-nrs*
  (mapcar
   #'(lambda (s)
       (or (assq s '((nil) (%pascal-functions%) (*all-metered-functions*)
		      (*post-gc-hook*) (%handlers%) 
		     (%finalization-alist%) (%closure-code%)))
	   s))
   ppc::*ppc-nilreg-relative-symbols*))



(defun  %xload-unbound-function% ()
  (+ *xload-dynamic-space-address* *xload-target-fulltag-misc*))

(defparameter *xload-dynamic-space* nil)
(defparameter *xload-readonly-space* nil)
(defparameter *xload-static-space* nil)
(defparameter *xload-symbols* nil)
(defparameter *xload-package-alist* nil)         ; maps real package to clone
(defparameter *xload-aliased-package-addresses* nil)     ; cloned package to address
(defparameter *xload-cold-load-functions* nil)
(defparameter *xload-cold-load-documentation* nil)
(defparameter *xload-loading-file-source-file* nil)

(defparameter *xload-pure-code-p* t)     ; when T, subprims are copied to readonly space
                                        ; and code vectors are allocated there, reference subprims
                                        ; pc-relative.


        
(defun xload-lookup-symbol (sym)
  (gethash (%symbol->symptr sym) *xload-symbols*))

(defun (setf xload-lookup-symbol) (addr sym)
  (setf (gethash (%symbol->symptr sym) *xload-symbols*) addr))

(defun xload-lookup-address (address)
  (dolist (space *xload-spaces* (error "Address #x~8,'0x not found in defined address spaces ." address))
    (let* ((vaddr (xload-space-vaddr space)))
      (if (and (<= vaddr address)
               (< address (+ vaddr (the fixnum (xload-space-size space)))))
        (return (values (xload-space-data space) (- address vaddr)))))))

(defun xload-u32-at-address (address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (u32-ref v o)))

(defun (setf xload-u32-at-address) (new address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (setf (u32-ref v o) new)))

(defun xload-natural-at-address (address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (natural-ref v o)))

(defun (setf xload-natural-at-address) (new address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (setf (natural-ref v o) new)))
    
(defun xload-u16-at-address (address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (u16-ref v o)))

(defun (setf xload-u16-at-address) (new address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (setf (u16-ref v o) new)))

(defun xload-u8-at-address (address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (u8-ref v o)))

(defun (setf xload-u8-at-address) (new address)
  (multiple-value-bind (v o) (xload-lookup-address address)
    (setf (u8-ref v o) new)))

(defun xload-integer (imm &optional (nwords 1))
  (let* ((arch (backend-target-arch *target-backend*))
         (most-negative (arch::target-most-negative-fixnum arch))
         (most-positive (arch::target-most-positive-fixnum arch)))
  (if (and (typep imm 'integer)
           (<= most-negative imm most-positive))
    (ash imm (arch::target-fixnum-shift arch))
    (let* ((bignum (xload-make-ivector
                    *xload-dynamic-space*
                    :bignum
                    nwords)))
      (dotimes (i nwords bignum)
        (setf (xload-%fullword-ref bignum i) (ldb (byte 32 0) imm)
              imm (ash imm -32)))))))

;;; "grow" the space: make a new data vector. Copy old data 
;;;  to new data vector.  Update size and data fields.
;;; Grow (arbitrarily) by 64K bytes, or as specified by caller.
(defun xload-more-space (space &optional (delta (ash 1 16)))
  (declare (fixnum delta))
  (setq delta (logand (lognot 3) (the fixnum (+ delta 3))))
  (let* ((old-size (xload-space-size space))
         (old-data (xload-space-data space))
         (old-nfullwords (ash old-size -2))
         (delta-nfullwords (ash delta -2))
         (new-size (+ old-size delta))
         (new-nfullwords (+ old-nfullwords delta-nfullwords))
         (new-data (make-array (the fixnum new-nfullwords)
                               :element-type '(unsigned-byte 32)
                               :initial-element 0)))
    (declare (fixnum old-size old-nfullwords delta-nfullwords))
    (declare (type (simple-array (unsigned-byte 32) (*)) old-data new-data))
    (dotimes (i old-nfullwords)
      (declare (optimize (speed 3) (safety 0)))
      (setf (aref new-data i) (aref old-data i)))
    (setf (xload-space-size space) new-size
          (xload-space-data space) new-data)
    new-size))
                               

(defun xload-alloc (space tag nbytes)
  (declare (fixnum tag nbytes))
  (when (logtest 7 nbytes) (error "~d not a multiple of 8 ." nbytes))
  (let* ((free (xload-space-lowptr space)))
    (if (> nbytes (the fixnum (- (the fixnum (xload-space-size space)) free)))
      (xload-more-space space (the fixnum (+ nbytes (ash 1 16)))))
    (setf (xload-space-lowptr space) (the fixnum (+ free nbytes)))
    (let* ((offset (+ free tag)))
      (declare (fixnum offset))
      (values 
       (the fixnum (+ (xload-space-vaddr space) offset))
       (xload-space-data space)
       offset))))

;;; element-count doesn't include header
(defun xload-alloc-fullwords (space tag nelements)
  (xload-alloc space tag (xload-aligned-uvector-size (ash nelements 2))))

(defun xload-alloc-halfwords (space tag nelements)
  (xload-alloc space tag (xload-aligned-uvector-size (ash nelements 1))))

(defun xload-alloc-bytes (space tag nelements)
  (xload-alloc space tag (xload-aligned-uvector-size nelements)))

(defun xload-alloc-doublewords (space tag nelements)
  (xload-alloc space tag (xload-aligned-uvector-size (ash nelements 3))))

(defun xload-alloc-bits (space tag nelements)
  (xload-alloc space tag (logand (lognot 7) (+ 7 4 (ash (+ 7 nelements) -3)))))


(defun xload-make-cons (car cdr &optional (space *xload-dynamic-space*))
  (multiple-value-bind (cell-addr data offset) (xload-alloc space  *xload-target-fulltag-cons* *xload-target-cons-size*)
    (setf (natural-ref data (the fixnum (+ offset *xload-target-car-offset*))) car)
    (setf (natural-ref data (the fixnum (+ offset *xload-target-cdr-offset*))) cdr)
    cell-addr))

;;; This initializes the gvector's contents to 0.  Might want to
;;; consider initializing it to NIL for the benefit of package and
;;; hashtable code.
(defun xload-make-gvector (subtag len)
  (unless (typep subtag 'fixnum)
    (setq subtag (type-keyword-code subtag)))
  (locally
      (declare (fixnum subtype len))
      (multiple-value-bind (cell-addr data offset)
          (target-arch-case
           (:ppc32 (xload-alloc-fullwords *xload-dynamic-space* *xload-target-fulltag-misc* len))
           (:ppc64 (xload-alloc-doublewords *xload-dynamic-space* *xload-target-fulltag-misc* len)))
        (declare (fixnum offset))
        (setf (natural-ref data (+ offset *xload-target-misc-header-offset*)) (make-xload-header len subtag))
        cell-addr)))

(defun xload-make-word-ivector (subtag len space)
  (declare (fixnum subtype len))
    (multiple-value-bind (cell-addr data offset) (xload-alloc-fullwords space  *xload-target-fulltag-misc* len)
      (declare (fixnum offset))
      (setf (natural-ref data (+ offset *xload-target-misc-header-offset*)) (make-xload-header len subtag))
      cell-addr))

(defun xload-package->addr (p)
  (or (cdr (assq (or (cdr (assq p *xload-package-alist*)) 
                     (error "Package ~s not cloned ." p))
                 *xload-aliased-package-addresses*))
      (error "Cloned package ~s: no assigned address . " p)))

(defun xload-addr->package (a)
  (or (car (rassoc (or (car (rassoc a *xload-aliased-package-addresses* :test #'eq))
                       (error "Address ~d: no cloned package ." a))
                   *xload-package-alist*))
      (error "Package at address ~d not cloned ." a)))

(defun xload-make-symbol (pname-address &optional
					(package-address *xload-target-nil*)
					(space *xload-dynamic-space*))
  (let* ((sym
          (target-arch-case
           (:ppc32 (xload-alloc-fullwords space *xload-target-fulltag-misc* target::symbol.element-count))
           (:ppc64 (xload-alloc-doublewords space *xload-target-fulltag-misc* target::symbol.element-count)))))
    (setf (xload-%svref sym -1)  (xload-symbol-header))
    (setf (xload-%svref sym target::symbol.flags-cell) 0)
    ;; On PPC64, NIL's pname must be NIL.
    (setf (xload-%svref sym target::symbol.pname-cell)
          (if (and (target-arch-case (:ppc64 t) (:ppc32 nil))
                   (= sym *xload-target-nil*))
            *xload-target-nil*
            pname-address))
    (setf (xload-%svref sym target::symbol.vcell-cell) *xload-target-unbound-marker*)
    (setf (xload-%svref sym target::symbol.package-plist-cell) package-address)
    (setf (xload-%svref sym target::symbol.fcell-cell) (%xload-unbound-function%))
    ;;(break "Made symbol at #x~x (#x~x)" cell-addr offset)
    sym))

;;; No importing or shadowing can (easily) happen during the cold
;;; load; a symbol is present in no package other than its home
;;; package.
;;; This -just- finds or adds the symbol in the "clone" package's itab/etab.
;;; Somebody else has to copy the symbol to the image ...
(defun xload-intern (symbol)
  (let* ((pname (symbol-name symbol))
         (namelen (length pname))
         (package (symbol-package symbol))
         (clone (cdr (assq package *xload-package-alist*))))
    (unless (nth-value 1 (%find-package-symbol pname clone namelen))    ; already there
      (without-interrupts
       (let* ((htab (if (%get-htab-symbol pname namelen (pkg.etab package)) 
                      (pkg.etab clone) 
                      (pkg.itab clone))))
         (%htab-add-symbol symbol htab (nth-value 2 (%get-htab-symbol pname namelen htab))))))
    t))
     

(defun xload-dnode-align (nbytes)
  (target-arch-case
   (:ppc32 (logand (lognot 7) (+ nbytes 7 4)))
   (:ppc64 (logand (lognot 15) (+ nbytes 15 8)))))

(defun xload-subtag-bytes (subtag element-count)
  (funcall (arch::target-array-data-size-function
            (backend-target-arch *target-backend*))
           subtag element-count))

    
(defun xload-make-dfloat (space high low)
  (target-arch-case
   (:ppc32
    (multiple-value-bind (dfloat-addr v o) (xload-alloc-fullwords space ppc32::fulltag-misc ppc32::double-float.element-count)
      (declare (fixnum o))
      (setf (u32-ref v (the fixnum (+ o ppc32::double-float.header))) 
            (make-xload-header ppc32::double-float.element-count ppc32::subtag-double-float))
      (setf (u32-ref v (the fixnum (+ o ppc32::double-float.value))) high)
      (setf (u32-ref v (the fixnum (+ o ppc32::double-float.val-low))) low)
      dfloat-addr))
   (:ppc64
    (multiple-value-bind (dfloat-addr v o) (xload-alloc-fullwords space ppc64::fulltag-misc ppc64::double-float.element-count)
      (declare (fixnum o))
      (setf (natural-ref v (the fixnum (+ o ppc64::misc-header-offset))) 
            (make-xload-header ppc64::double-float.element-count ppc64::subtag-double-float))
      (setf (u32-ref v (the fixnum (+ o ppc64::double-float.value))) high)
      (setf (u32-ref v (the fixnum (+ o ppc64::double-float.val-low))) low)
      dfloat-addr))))

(defun xload-make-sfloat (space bits)
  (target-arch-case
   (:ppc32
    (multiple-value-bind (sfloat-addr v o) (xload-alloc-fullwords space ppc32::fulltag-misc ppc32::single-float.element-count)
      (declare (fixnum o))
      (setf (u32-ref v (the fixnum (+ o ppc32::single-float.header))) 
            (make-xload-header ppc32::single-float.element-count ppc32::subtag-single-float))
      (setf (u32-ref v (the fixnum (+ o ppc32::single-float.value))) bits)
      sfloat-addr))
   (:ppc64
    (logior (ash bits 32) ppc64::subtag-single-float))))
        
(defun xload-make-ivector (space subtag nelements)
  (unless (typep subtag 'fixnum)
    (setq subtag (type-keyword-code subtag)))
  (locally
      (declare (fixnum subtype nelements))
    (multiple-value-bind (addr v o) (xload-alloc space *xload-target-fulltag-misc* (xload-dnode-align (xload-subtag-bytes subtag nelements)))
      (declare (fixnum o))
      (setf (natural-ref v (the fixnum (- o *xload-target-fulltag-misc*))) (make-xload-header nelements subtag))
      (values addr v o))))

(defun xload-%svref (addr i)
  (declare (fixnum i))
  (target-arch-case
   (:ppc32
    (if (= (the fixnum (logand addr ppc32::fulltagmask)) ppc32::fulltag-misc)
      (multiple-value-bind (v offset) (xload-lookup-address addr)
        (declare (fixnum offset))
        (u32-ref v (the fixnum (+ offset (the fixnum (+ *xload-target-misc-data-offset* (the fixnum (ash i 2))))))))
      (error "Not a vector: #x~x" addr)))
   (:ppc64
    (if (= (the fixnum (logand addr ppc64::fulltagmask)) ppc64::fulltag-misc)
      (multiple-value-bind (v offset) (xload-lookup-address addr)
        (declare (fixnum offset))
        (natural-ref v (the fixnum (+ offset (the fixnum (+ *xload-target-misc-data-offset* (the fixnum (ash i 3))))))))
      (error "Not a vector: #x~x" addr)))))

(defun (setf xload-%svref) (new addr i)
  (declare (fixnum i))
  (target-arch-case
   (:ppc32
    (if (= (the fixnum (logand addr ppc32::fulltagmask)) ppc32::fulltag-misc)
      (multiple-value-bind (v offset) (xload-lookup-address addr)
        (declare (fixnum offset))
        (setf (u32-ref v (the fixnum (+ offset (the fixnum (+ *xload-target-misc-data-offset* (the fixnum (ash i 2))))))) new))
      (error "Not a vector: #x~x" addr)))
   (:ppc64
    (if (= (the fixnum (logand addr ppc64::fulltagmask)) ppc64::fulltag-misc)
      (multiple-value-bind (v offset) (xload-lookup-address addr)
        (declare (fixnum offset))
        (setf (natural-ref v (the fixnum (+ offset (the fixnum (+ *xload-target-misc-data-offset* (the fixnum (ash i 3))))))) new))
      (error "Not a vector: #x~x" addr)))))


(defun xload-%fullword-ref (addr i)
  (declare (fixnum i))
  (if (= (the fixnum (logand addr *xload-target-fulltagmask*))
           *xload-target-fulltag-misc*)
      (multiple-value-bind (v offset) (xload-lookup-address addr)
        (declare (fixnum offset))
        (u32-ref v (the fixnum (+ offset (the fixnum (+ *xload-target-misc-data-offset* (the fixnum (ash i 2))))))))
      (error "Not a vector: #x~x" addr)))

(defun (setf xload-%fullword-ref) (new addr i)
  (declare (fixnum i))
  (if (= (the fixnum (logand addr *xload-target-fulltagmask*))
         *xload-target-fulltag-misc*)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (u32-ref v (the fixnum (+ offset (the fixnum (+ *xload-target-misc-data-offset* (the fixnum (ash i 2))))))) new))
    (error "Not a vector: #x~x" addr)))

(defun xload-car (addr)
  (if (xload-target-listp addr)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (natural-ref v (the fixnum (+ offset *xload-target-car-offset*))))
    (error "Not a list: #x~x" addr)))

(defun (setf xload-car) (new addr)
  (if (xload-target-consp addr)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (natural-ref v (the fixnum (+ offset *xload-target-car-offset*))) new))
    (error "Not a cons: #x~x" addr)))

(defun xload-cdr (addr)
  (if (xload-target-listp addr)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (natural-ref v (the fixnum (+ offset *xload-target-cdr-offset*))))
    (error "Not a list: #x~x" addr)))

(defun (setf xload-cdr) (new addr)
  (if (xload-target-consp addr)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (natural-ref v (the fixnum (+ offset *xload-target-cdr-offset*))) new))
    (error "Not a cons: #x~x" addr)))

(defun xload-symbol-value (addr)
  (if (= (xload-%svref addr -1) (xload-symbol-header))
    (xload-%svref addr target::symbol.vcell-cell)
    (error "Not a symbol: #x~x" addr)))
  

(defun (setf xload-symbol-value) (new addr)
  (if (= (xload-%svref addr -1) (xload-symbol-header))
    (setf (xload-%svref addr target::symbol.vcell-cell) new)
    (error "Not a symbol: #x~x" addr)))

(defun xload-set (sym val)
  (check-type sym symbol)
  (check-type val integer)
  (let* ((symaddr (xload-lookup-symbol sym)))
    (unless symaddr (error "Symbol address not found: ~s ." sym))
    (setf (xload-symbol-value symaddr) val)))

(defun xload-fset (addr def)
  (if (= (xload-%svref addr -1) (xload-symbol-header))
    (setf (xload-%svref addr target::symbol.fcell-cell) def)
    (error "Not a symbol: #x~x" addr)))

(defun (setf xload-symbol-plist) (new addr)
  (let* ((package-plist (xload-%svref addr target::symbol.package-plist-cell)))
    (if (xload-target-consp package-plist)
      (let* ((str (xload-get-string (xload-%svref addr target::symbol.pname-cell))))
        (break "str = ~s" str)
        (warn "Symbol at #x~x: plist already set." addr))
      (setf (xload-%svref addr target::symbol.package-plist-cell)
            (xload-make-cons package-plist new)))
    new))
      
  
;;; This handles constants set to themselves.  Unless
;;; PRESERVE-CONSTANTNESS is true, the symbol's $sym_vbit_const bit is
;;; cleared.  (This is done because the kernel tries to call EQUALP if
;;; constants are "redefined", and EQUALP may not be defined early
;;; enough.)
(defun xload-copy-symbol (symbol &key
				 (preserve-constantness (keywordp symbol))
				 (space *xload-dynamic-space*))
  (or (xload-lookup-symbol symbol)
      (let* ((pname (symbol-name symbol))
             (home-package (symbol-package symbol))
             (addr (xload-make-symbol (xload-save-string pname (length pname))
                                      (if home-package 
                                        (xload-package->addr home-package)
                                        *xload-target-nil*)
                                      space)))
        (xload-intern symbol)
        (let* ((bits (logandc2 (%symbol-bits symbol)
                               (ash 1 $sym_vbit_typeppred))))
          (setf (xload-%svref addr target::symbol.flags-cell)
                (ash (if preserve-constantness
                       bits
                       (logand (lognot (ash 1 $sym_vbit_const)) bits))
                     *xload-target-fixnumshift*)))
        (if (and (constantp symbol)
                 (eq (symbol-value symbol) symbol))
          (setf (xload-symbol-value addr) addr))
        (setf (xload-lookup-symbol symbol) addr))))


;;; Write a list to dynamic space.  No detection of circularity or
;;; structure sharing.  The cdr of the final cons can be nil (treated
;;; as *xload-target-nil*.  All cars must be addresses.

(defun xload-save-list (l)
  (if (atom l)
    (or l *xload-target-nil*)
    (xload-make-cons (car l) (xload-save-list (cdr l)))))

(defun xload-save-string (str &optional (n (length str)))
  (declare (fixnum n))
  (let* ((subtag (type-keyword-code :simple-string)))
    (multiple-value-bind (addr v offset) (xload-make-ivector *xload-readonly-space* subtag n)
      (do* ((p (+ offset *xload-target-misc-data-offset*)
               (1+ p))
              (i 0 (1+ i)))
             ((= i n) str)
          (declare (fixnum i p))
          (setf (u8-ref v p) (char-code (schar str i))))
        addr)))

;;; Read a string from fasl file, save it to readonly-space.
(defun %xload-fasl-vreadstr (s)
  (multiple-value-bind (str n new-p) (%fasl-vreadstr s)
    (declare (fixnum n subtype))
    (values (xload-save-string str n) str n new-p)))

(defun xload-clone-packages (packages)
  (let* ((alist (mapcar #'(lambda (p)
                            (cons p
                                  (gvector :package
                                            (cons (make-array (the fixnum (length (car (uvref p 0))))
                                                              :initial-element 0)
                                                  (cons 0 (cddr (pkg.itab p))))
                                            (cons (make-array
                                                   (the fixnum
                                                     (length
                                                      (car
                                                       (pkg.etab p))))
                                                   :initial-element 0)
                                                  (cons 0 (cddr (pkg.etab p))))
                                            nil                         ; used
                                            nil                         ; used-by
                                            (copy-list (pkg.names p))     ; names
                                            nil ;shadowed
                                            nil ;lock
                                            )))
                        packages)))
    (flet ((lookup-clone (p) (or (cdr (assq p alist)) (error "Package ~S not cloned ." p))))
      (dolist (pair alist alist)
        (let* ((orig (car pair))
               (dup (cdr pair)))
          (setf (pkg.used dup) (mapcar #'lookup-clone (pkg.used orig))
                (pkg.used-by dup) (mapcar #'lookup-clone (pkg.used-by orig))))))))

;;; Dump each cloned package into dynamic-space; return an alist
(defun xload-assign-aliased-package-addresses (alist)
  (let* ((addr-alist (mapcar #'(lambda (pair)
                                 (let* ((p (cdr pair))
                                        (v (xload-make-gvector :package (uvsize p))))
                                   (setf (xload-%svref v pkg.names)
                                         (xload-save-list (mapcar #'(lambda (n) (xload-save-string n))
                                                                 (pkg.names p))))
                                   (cons p v)))
                             alist)))
    (flet ((clone->addr (clone)
             (or (cdr (assq clone addr-alist)) (error "cloned package ~S not found ." clone))))
      (dolist (pair addr-alist addr-alist)
        (let* ((p (car pair))
               (v (cdr pair)))
          (setf (xload-%svref v pkg.used)
                (xload-save-list (mapcar #'clone->addr (pkg.used p)))
                (xload-%svref v pkg.used-by)
                (xload-save-list (mapcar #'clone->addr (pkg.used-by p)))
                (xload-%svref v pkg.shadowed) 
                (xload-save-list (mapcar #'xload-copy-symbol (pkg.shadowed p)))))))))



(defun xload-fasload (pathnames)
  (dolist (path pathnames)
    (multiple-value-bind (*load-pathname* *load-truename* source-file) (find-load-file (merge-pathnames path))
      (unless *load-truename*
        (return (signal-file-error $err-no-file path)))
      (setq path *load-truename*)
      (let* ((*readtable* *readtable*)
             (*package* *ccl-package*)   ; maybe just *package*
             (*loading-files* (cons path *loading-files*))
             (*xload-loading-file-source-file* nil)
             (*loading-file-source-file* (namestring source-file)))
        (when *load-verbose*
	  (format t "~&;Loading ~S..." *load-pathname*)
	  (force-output))
        (multiple-value-bind (winp err) (%fasload (native-translated-namestring path) *xload-fasl-dispatch-table*)
          (if (not winp) (%err-disp err)))))))
  



(defun xload-save-htab (htab)
  (let* ((htvec (car htab))
         (len (length htvec))
         (xvec (xload-make-gvector :simple-vector len))
         (deleted-marker *xload-target-unbound-marker*))
    (dotimes (i len)
      (let* ((s (%svref htvec i)))
        (setf (xload-%svref xvec i)
              (if s
                (if (symbolp s) (xload-lookup-symbol s) deleted-marker)
                *xload-target-nil*))))
    (xload-make-cons  
     xvec 
     (xload-make-cons
      (xload-integer (cadr htab))
      (xload-integer (cddr htab))))))

(defun xload-finalize-packages ()
  (dolist (pair *xload-aliased-package-addresses*)
    (let* ((p (car pair))
           (q (cdr pair)))
      (setf (xload-%svref q pkg.etab) (xload-save-htab (pkg.etab p)))
      (setf (xload-%svref q pkg.itab) (xload-save-htab (pkg.itab p))))))

(defun xload-get-string (address)
    (multiple-value-bind (v o) (xload-lookup-address address)
      (let* ((header (natural-ref v (+ o *xload-target-misc-header-offset*)))
             (len (ash header (- target::num-subtag-bits)))
             (str (make-string len))
             (p (+ o *xload-target-misc-data-offset*)))
        (dotimes (i len str)
          (setf (schar str i) (code-char (u8-ref v (+ p i))))))))

               
(defun xload-save-code-vector (code)
  (let* ((read-only-p *xload-pure-code-p*)
         (vlen (uvsize code))
         (prefix (arch::target-code-vector-prefix (backend-target-arch
                                                   *target-backend*)))
         (n (+ (length prefix) vlen)))
    (declare (fixnum n))
    (let* ((vector (xload-make-ivector 
                    (if read-only-p
                      *xload-readonly-space*
                      *xload-dynamic-space*)
                    :code-vector
                    n))
           (j -1))
      (declare (fixnum j))
      (dotimes (i n)
        (setf (xload-%fullword-ref vector i)
              (if prefix
                (pop prefix)
                (uvref code (incf j)))))
      vector)))
                          
;;; For debugging
(defun xload-show-list (l)
  (labels ((show-list (l)
             (unless (= l *xload-target-nil*)
               (format t "#x~x" (xload-car l))
               (setq l (xload-cdr l))
               (unless (= l *xload-target-nil*)
                 (format t " ")
                 (show-list l)))))
    (format t "~&(")
    (show-list l)
    (format t ")")))


(defun xfasload (output-file &rest pathnames)
  (let* ((*xload-symbols* (make-hash-table :test #'eq))
         (*xload-spaces* nil)
         (*xload-readonly-space* (init-xload-space *xload-readonly-space-address* *xload-readonly-space-size* ppc::area-readonly))
         (*xload-dynamic-space* (init-xload-space *xload-dynamic-space-address* *xload-dynamic-space-size* ppc::area-dynamic))
	 (*xload-static-space* (init-xload-space *xload-static-space-address* *xload-static-space-size* ppc::area-static))
						 
         (*xload-package-alist* (xload-clone-packages %all-packages%))
         (*xload-cold-load-functions* nil)
         (*xload-cold-load-documentation* nil)
         (*xload-loading-file-source-file* nil)
         (*xload-svar-alist* nil)
         (*xload-aliased-package-addresses* nil))
    (target-arch-case
     (:ppc32
      (xload-make-word-ivector ppc32::subtag-u32-vector 1027 *xload-static-space*)
      ;; Make NIL.  Note that NIL is sort of a misaligned cons (it straddles two doublewords.)
      (xload-make-cons *xload-target-nil* 0 *xload-static-space*)
      (xload-make-cons 0 *xload-target-nil* *xload-static-space*))
     (:ppc64
      (xload-make-ivector *xload-static-space* ppc64::subtag-u64-vector
                         (1- (/ 4096 8)))))
    ;; Create %unbound-function% and the package objects in dynamic space,
    ;; then fill in the nilreg-relative symbols in static space.
    ;; Then start consing ..
    ;; The undefined-function object is a 1-element simple-vector (not
    ;; a function vector).  The code-vector in its 0th element should
    ;; report the appropriate error.
    (let* ((udf-object (xload-make-gvector :simple-vector 1)))
      (setf (xload-%svref udf-object 0) (xload-save-code-vector
					 (backend-xload-info-udf-code
					  *xload-target-backend*))))
    (setq *xload-aliased-package-addresses* (xload-assign-aliased-package-addresses *xload-package-alist*))
    (dolist (pair *xload-nrs*)
      (let* ((val-p (consp pair))
	     (val (if val-p (or (cdr pair) *xload-target-nil*)))
	     (sym (if val-p (car pair) pair)))
	(xload-copy-symbol sym
			   :preserve-constantness t
			   :space *xload-static-space*)
	(when val-p (xload-set sym val))))
    ; This could be a little less ... procedural.
    (xload-set '*package* (xload-package->addr *ccl-package*))
    (xload-set '*keyword-package* (xload-package->addr *keyword-package*))
    (xload-set '%all-packages% (xload-save-list (mapcar #'cdr *xload-aliased-package-addresses*)))
    (xload-set '%unbound-function% (%xload-unbound-function%))
    (xload-set '*gc-event-status-bits* (xload-integer 0 #|(ash 1 $gc-integrity-check-bit)|#))
    (xload-set '%toplevel-catch% (xload-copy-symbol :toplevel))
    (xload-set '%closure-code% (xload-save-code-vector
				(backend-xload-info-closure-trampoline-code
				 *xload-target-backend*)))
    (xload-set '%macro-code% (xload-save-code-vector
			      (funcall
                               (backend-xload-info-macro-apply-code-function
                                *xload-target-backend*))))
    (let* ((len (length %builtin-functions%))
           (v (xload-make-gvector :simple-vector len)))
      (dotimes (i len)
        (setf (xload-%svref v i) (xload-copy-symbol (svref %builtin-functions% i))))
      (xload-set '%builtin-functions% v))
    (xload-copy-symbol '*xload-startup-file*)
    (xload-fasload pathnames)
    (xload-set '*xload-startup-file*
              (xload-save-string *xload-startup-file*))
    (let* ((toplevel (xload-symbol-value (xload-lookup-symbol '%toplevel-function%))))      
      (when (= toplevel *xload-target-unbound-marker*)
	(warn "~S not set in loading ~S ." '%toplevel-function pathnames)))
    (setf (xload-symbol-value (xload-copy-symbol '*xload-cold-load-functions*))
          (xload-save-list (setq *xload-cold-load-functions*
                                (nreverse *xload-cold-load-functions*))))
    (when *xload-show-cold-load-functions*
      (format t "~&cold-load-functions list:")
      (xload-show-list (xload-symbol-value (xload-copy-symbol '*xload-cold-load-functions*))))
    (setf (xload-symbol-value (xload-copy-symbol '*xload-cold-load-documentation*))
          (xload-save-list (setq *xload-cold-load-documentation*
                                 (nreverse *xload-cold-load-documentation*))))
                              
    (xload-finalize-packages)
    (xload-dump-image output-file *xload-image-base-address*)))

(defun xload-dump-image (output-file heap-start)
  (declare (ftype (function (t t list)) write-image-file))
  (write-image-file output-file
		    heap-start
		    (list *xload-readonly-space*
			  *xload-static-space*
			  *xload-dynamic-space*)))
		    





;;; The xloader

(xload-copy-faslop $fasl-noop)
(xload-copy-faslop $fasl-vetab-alloc)
(xload-copy-faslop $fasl-veref)

; Should error if epush bit set, else push on *xload-cold-load-functions* or something.
(defxloadfaslop $fasl-lfuncall (s)
  (let* ((fun (%fasl-expr-preserve-epush s)))
    (when (faslstate.faslepush s)
      (error "Can't call function for value : ~s" fun))
    (when *xload-show-cold-load-functions*
      (format t "~& cold-load function: #x~x" fun))
    (push fun *xload-cold-load-functions*)))

(xload-copy-faslop $fasl-globals)        ; what the hell did this ever do ?

;;; fasl-char: maybe epush, return target representation of BASE-CHARACTER
(defxloadfaslop $fasl-char (s)
  (let* ((code (%fasl-read-byte s))
         (target-char (logior *xload-target-subtag-char*
                              (ash code *xload-target-charcode-shift*))))
    (%epushval s target-char)))



(defxloadfaslop $fasl-dfloat (s)
  (%epushval s (xload-make-dfloat *xload-readonly-space* (%fasl-read-long s) (%fasl-read-long s))))

(defxloadfaslop $fasl-sfloat (s)
  (%epushval s (xload-make-sfloat *xload-readonly-space* (%fasl-read-long s))))

(defxloadfaslop $fasl-vstr (s)
  (let* ((n (%fasl-read-count s)))
    (multiple-value-bind (str v o) (xload-make-ivector *xload-readonly-space* :simple-string n)
      (%epushval s str)
      (%fasl-read-n-bytes s v (+ o *xload-target-misc-data-offset*) n)
      str)))


(defxloadfaslop $fasl-fixnum (s)
  (%epushval s (xload-integer
                ;; This nonsense converts unsigned %fasl-read-long
                ;; result to signed
                (rlet ((long :long))
                  (setf (%get-long long) (%fasl-read-long s))
                  (%get-long long)))))

(defxloadfaslop $fasl-word-fixnum (s)
  (%epushval s (xload-integer (%word-to-int (%fasl-read-word s)))))

(defxloadfaslop $fasl-s32 (s)
  (%stack-block ((n 4))
    (setf (%get-unsigned-word n 0) (%fasl-read-word s)
          (%get-unsigned-word n 2) (%fasl-read-word s))
    (%epushval s (xload-integer (%get-signed-long n)))))

(defxloadfaslop $fasl-s64 (s)
  (%stack-block ((n 8))
    (setf (%get-unsigned-word n 0) (%fasl-read-word s)
          (%get-unsigned-word n 2) (%fasl-read-word s)
          (%get-unsigned-word n 4) (%fasl-read-word s)
          (%get-unsigned-word n 6) (%fasl-read-word s))
    (%epushval s (xload-integer (%%get-signed-longlong n 0) 2))))

(defun %xload-fasl-vmake-symbol (s)
  (%epushval s (xload-make-symbol (%xload-fasl-vreadstr s))))

(defxloadfaslop $fasl-vmksym (s)
  (%xload-fasl-vmake-symbol s))

(defun %xload-fasl-vintern (s package)
  (multiple-value-bind (str len new-p) (%fasl-vreadstr s)
    (without-interrupts
     (multiple-value-bind (cursym access internal external) (%find-symbol str len package)
       (unless access
         (unless new-p (setq str (%fasl-copystr str len)))
         (setq cursym (%add-symbol str package internal external)))
       ; cursym now exists in the load-time world; make sure that it exists
       ; (and is properly "interned" in the world we're making as well)
       (%epushval s (xload-copy-symbol cursym))))))


(defxloadfaslop $fasl-vintern (s)
  (%xload-fasl-vintern s *package*))


(defxloadfaslop $fasl-vpkg-intern (s)
  (let* ((addr (%fasl-expr-preserve-epush  s))
         (pkg (xload-addr->package addr)))
    (%xload-fasl-vintern s pkg)))


(defun %xload-fasl-vpackage (s)
  (multiple-value-bind (str len new-p) (%fasl-vreadstr s)
    (let* ((p (%find-pkg str len)))
      (%epushval s (xload-package->addr 
                    (or p (%kernel-restart $XNOPKG (if new-p str (%fasl-copystr str len)))))))))


(defxloadfaslop $fasl-vpkg (s)
  (%xload-fasl-vpackage s))

(defxloadfaslop $fasl-cons (s)
  (let* ((cons (%epushval s (xload-make-cons *xload-target-nil* *xload-target-nil*))))
    (setf (xload-car cons) (%fasl-expr s)
          (xload-cdr cons) (%fasl-expr s))
    (setf (faslstate.faslval s) cons)))
    

(defun %xload-fasl-vlistX (s dotp)
  (let* ((len (%fasl-read-count s)))
    (declare (fixnum len))
    (let* ((val (%epushval s (xload-make-cons *xload-target-nil* *xload-target-nil*)))
           (tail val))
      (setf (xload-car val) (%fasl-expr s))
      (dotimes (i len)
        (setf (xload-cdr tail) (setq tail (xload-make-cons  (%fasl-expr s) *xload-target-nil*))))
      (if dotp
        (setf (xload-cdr tail) (%fasl-expr s)))
      (setf (faslstate.faslval s) val))))

(defxloadfaslop $fasl-vlist (s)
  (%xload-fasl-vlistX s nil))

(defxloadfaslop $fasl-vlist* (s)
  (%xload-fasl-vlistX s t))

(defxloadfaslop $fasl-nil (s)
  (%epushval s *xload-target-nil*))

(defxloadfaslop $fasl-timm (s)
  (let* ((val (%fasl-read-long s)))
    #+paranoid (unless (= (logand $typemask val) $t_imm) 
                 (error "Bug: expected immediate-tagged object, got ~s ." val))
    (%epushval s val)))


(defxloadfaslop $fasl-arch (s)
  (%cant-epush s)
  (let* ((arch (%fasl-expr s))
	 (backend-name (backend-xload-info-compiler-target-name
				 *xload-target-backend*))
	 (backend (find-backend backend-name)))
    (declare (fixnum arch))
    (unless (= arch (ash (backend-target-architecture backend)
			 *xload-target-fixnumshift*))
      (error "Not a ~A fasl file : ~s" backend-name (faslstate.faslfname s)))))


(defxloadfaslop $fasl-symfn (s)
  (let* ((symaddr (%fasl-expr-preserve-epush s))
         (fnobj (xload-%svref symaddr target::symbol.fcell-cell)))
    (if (and (= *xload-target-fulltag-misc*
                (logand fnobj *xload-target-fulltagmask*))
             (= (type-keyword-code :function) (xload-u8-at-address (+ fnobj *xload-target-misc-subtag-offset*))))
      (%epushval s fnobj)
      (error "symbol at #x~x is unfbound . " symaddr))))

(defxloadfaslop $fasl-eval (s)
  (let* ((expr (%fasl-expr-preserve-epush s)))
    (error "Can't evaluate expression ~s in cold load ." expr)
    (%epushval s (eval expr))))         ; could maybe evaluate symbols, constants ...


(defun xload-target-subtype (name)
  (or
   (cdr (assoc name (arch::target-uvector-subtags (backend-target-arch *target-backend*))))
   (error "Unknown uvector type name ~s" name)))

(defxloadfaslop $fasl-vivec (s)
  (let* ((subtag (%fasl-read-byte s))
         (element-count (%fasl-read-count s)))
    (declare (fixnum subtag))
    (multiple-value-bind (vector v o)
                         (xload-make-ivector 
                          *xload-readonly-space*
                          subtag 
                          element-count)
      (%epushval s vector)
      (%fasl-read-n-bytes s v (+ o  *xload-target-misc-data-offset*) (xload-subtag-bytes subtag element-count))
      vector)))

(defun xfasl-read-ivector (s subtag)
  (let* ((element-count (%fasl-read-count s)))
    (multiple-value-bind (vector v o)
                         (xload-make-ivector 
                          *xload-readonly-space*
                          subtag 
                          element-count)
      (%epushval s vector)
      (%fasl-read-n-bytes s
                          v
                          (+ o *xload-target-misc-data-offset*)
                          (xload-subtag-bytes subtag element-count))
      vector)))

(defxloadfaslop $fasl-u8-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :unsigned-8-bit-vector)))

(defxloadfaslop $fasl-s8-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :signed-8-bit-vector)))

(defxloadfaslop $fasl-u16-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :unsigned-16-bit-vector)))

(defxloadfaslop $fasl-s16-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :signed-16-bit-vector)))

(defxloadfaslop $fasl-u32-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :unsigned-32-bit-vector)))

(defxloadfaslop $fasl-s32-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :signed-32-bit-vector)))


;;; We really can't compile 64-bit vectors on a 32-bit host.
#+ppc64-target
(defxloadfaslop $fasl-u64-vector (s)
  (xfasl-read-ivector s ppc64::subtag-u64-vector))

#+ppc64-target
(defxloadfaslop $fasl-u64-vector (s)
  (xfasl-read-ivector s ppc64::subtag-s64-vector))

(defxloadfaslop $fasl-bit-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :bit-vector)))

(defxloadfaslop $fasl-bignum32 (s)
  (xfasl-read-ivector s (xload-target-subtype :bignum)))

(defxloadfaslop $fasl-single-float-vector (s)
  (xfasl-read-ivector s (xload-target-subtype :single-float-vector)))

(defxloadfaslop $fasl-double-float-vector (s)
  (target-arch-case
   (:ppc64 (xfasl-read-ivector s ppc64::subtag-double-float-vector))
   (:ppc32
    (let* ((element-count (%fasl-read-count s)))
      (multiple-value-bind (vector v o)
          (xload-make-ivector 
           *xload-readonly-space*
           ppc32::subtag-double-float-vector
           element-count)
        (%epushval s vector)
        (%fasl-read-n-bytes s v (+ o  ppc32::misc-dfloat-offset) (xload-subtag-bytes ppc32::subtag-double-float-vector  element-count))
        vector)))))

(defxloadfaslop $fasl-code-vector (s)
  (let* ((element-count (%fasl-read-count s))
         (subtag (xload-target-subtype :code-vector)))
    (multiple-value-bind (vector v o)
                         (xload-make-ivector 
                          (if (not *xload-pure-code-p*)
                            *xload-dynamic-space* 
                            *xload-readonly-space*)
                          subtag 
                          element-count)
      (%epushval s vector)
      (%fasl-read-n-bytes s v (+ o
                                 *xload-target-misc-data-offset*)
                          (xload-subtag-bytes subtag element-count))
      vector)))

(defun xfasl-read-gvector (s subtype)
  (declare (fixnum subtype))
  (let* ((n (%fasl-read-count s))
         (vector (xload-make-gvector subtype n)))
    (%epushval s vector)
    (dotimes (i n (setf (faslstate.faslval s) vector))
      (setf (xload-%svref vector i) (%fasl-expr s)))))
  
(defxloadfaslop $fasl-vgvec (s)
  (let* ((subtype (%fasl-read-byte s)))
    (xfasl-read-gvector s subtype)))

(defxloadfaslop $fasl-vector-header (s)
  (xfasl-read-gvector s (xload-target-subtype :vector-header)))

(defxloadfaslop $fasl-array-header (s)
  (xfasl-read-gvector s (xload-target-subtype :array-header)))

(defxloadfaslop $fasl-ratio (s)
  (let* ((r (xload-make-gvector (xload-target-subtype :ratio)
                                target::ratio.element-count)))
    (%epushval s r)
    (setf (xload-%svref r target::ratio.numer-cell) (%fasl-expr s)
          (xload-%svref r target::ratio.denom-cell) (%fasl-expr s))
    (setf (faslstate.faslval s) r)))

(defxloadfaslop $fasl-complex (s)
  (let* ((c (xload-make-gvector (xload-target-subtype :complex)
                                target::complex.element-count)))
    (%epushval s c)
    (setf (xload-%svref c target::complex.realpart-cell) (%fasl-expr s)
          (xload-%svref c target::complex.imagpart-cell) (%fasl-expr s))
    (setf (faslstate.faslval s) c)))

;;; About all that we do with svars is to canonicalize them.
(defxloadfaslop $fasl-svar (s)
  (let* ((epush (faslstate.faslepush s))
         (ecount (faslstate.faslecnt s)))
    (when epush
      (%epushval s 0))
    (let* ((sym (%fasl-expr s))
           (vector (cdr (assq sym *xload-svar-alist*))))
      (unless vector
        (setq vector (xload-make-gvector :svar target::svar.element-count))
        (setf (xload-%svref vector target::svar.symbol-cell) sym)
        (push (cons sym vector) *xload-svar-alist*))
      (when epush
        (setf (svref (faslstate.faslevec s) ecount) vector))
      (setf (faslstate.faslval s) vector))))

(defxloadfaslop $fasl-t-vector (s)
  (xfasl-read-gvector s (xload-target-subtype :simple-vector)))

(defxloadfaslop $fasl-function (s)
  (xfasl-read-gvector s (xload-target-subtype :function)))

(defxloadfaslop $fasl-istruct (s)
  (xfasl-read-gvector s (xload-target-subtype :istruct)))

(defun xload-lfun-name (lf)
  (let* ((header (xload-%svref lf -1)))
    (unless (= (type-keyword-code :function)
               (logand header (1- (ash 1 target::num-subtag-bits))))
      (error "Not a function address: ~s" lf))
    (let* ((n (ash header (- target::num-subtag-bits))))
      (if (> n 2)
        (let* ((bits (ash (xload-%svref lf (1- n))
                          (- *xload-target-fixnumshift*))))
          (unless (logbitp $lfbits-noname-bit bits)
            (xload-%svref lf (- n 2))))
        (error "Teeny, tiny, little function : ~s" lf)))))


(defun xload-record-source-file (symaddr indicator)
  (when *xload-record-source-file-p*
    (when (or (eq indicator 'function)
              (eq indicator 'variable))
      (let* ((keyaddr (xload-copy-symbol 'bootstrapping-source-files))
             (pathaddr (or *xload-loading-file-source-file*
                           (if *loading-file-source-file*
                             (setq *xload-loading-file-source-file* (xload-save-string *loading-file-source-file*))))))
        (when pathaddr
          (let* ((keyval (if (eq indicator 'function)
                           (xload-make-cons  pathaddr *xload-target-nil*)
                           (xload-make-cons
                            (xload-make-cons 
                             (xload-make-cons  (xload-copy-symbol indicator) pathaddr)
                             *xload-target-nil*)
                            *xload-target-nil*))))
            (setf (xload-symbol-plist symaddr) (xload-make-cons keyaddr keyval))))))))

(defun xload-set-documentation (symaddr indicator doc)
  ;; Should maybe check further that it's a string
  ;; and it would hurt for whatever processes *xload-cold-load-documentation*
  ;; to do some checking there as well.
  (when (= (the fixnum (logand doc *xload-target-fulltagmask*))
           *xload-target-fulltag-misc*)
    (push (xload-save-list
           (list symaddr
                 (xload-copy-symbol indicator)
                 doc))
          *xload-cold-load-documentation*)))



(defxloadfaslop $fasl-defun (s)
  (%cant-epush s)
  (let* ((fun (%fasl-expr s))
         (doc (%fasl-expr s)))
    (let* ((sym (xload-lfun-name fun)))
      (unless (= doc *xload-target-nil*)
        (xload-set-documentation sym 'function doc))
      (xload-record-source-file sym 'function)
      (xload-fset sym fun))))

(defxloadfaslop $fasl-macro (s)
  (%cant-epush s)
  (let* ((fun (%fasl-expr s))
         (doc (%fasl-expr s)))
    (let* ((sym (xload-lfun-name fun))
           (vector (xload-make-gvector :simple-vector 2)))
      (setf (xload-%svref vector 0) (xload-symbol-value (xload-lookup-symbol '%macro-code%))
            (xload-%svref vector 1) fun)
      (unless (= doc *xload-target-nil*)
        (xload-set-documentation sym 'function doc))
      (xload-record-source-file sym 'function)
      (xload-fset sym vector))))

(defxloadfaslop $fasl-defconstant (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s))
         (doc (%fasl-expr s)))
    (unless (= doc *xload-target-nil*)
      (xload-set-documentation sym 'variable doc))
    (xload-record-source-file sym 'variable)
    (setf (xload-symbol-value sym) val)
    (setf (xload-%svref sym target::symbol.flags-cell)
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash 1 $sym_vbit_const) 
                   (ash (xload-%svref sym target::symbol.flags-cell)
                        (- *xload-target-fixnumshift*)))
           *xload-target-fixnumshift*))))

(defxloadfaslop $fasl-defparameter (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s))
         (doc (%fasl-expr s)))
    (unless (= doc *xload-target-nil*)
      (xload-set-documentation sym 'variable doc))
    (xload-record-source-file sym 'variable)
    (setf (xload-symbol-value sym) val)
    (setf (xload-%svref sym target::symbol.flags-cell)
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash (xload-%svref sym target::symbol.flags-cell)
                        (- *xload-target-fixnumshift*)))
           *xload-target-fixnumshift*))))

(defxloadfaslop $fasl-defvar (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s)))
    (xload-record-source-file sym 'variable)
    (setf (xload-%svref sym target::symbol.flags-cell)
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash (xload-%svref sym target::symbol.flags-cell)
                         (- *xload-target-fixnumshift*)))
           *xload-target-fixnumshift*))))

(defxloadfaslop $fasl-defvar-init (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s))
         (doc (%fasl-expr s)))
    (unless (= doc *xload-target-nil*)
      (xload-set-documentation sym 'variable doc))
    (when (= *xload-target-unbound-marker*
             (xload-symbol-value sym))
      (setf (xload-symbol-value sym) val))
    (xload-record-source-file sym 'variable)
    (setf (xload-%svref sym target::symbol.flags-cell)
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash (xload-%svref sym target::symbol.flags-cell)
                        (- *xload-target-fixnumshift*)))
           *xload-target-fixnumshift*))))


(xload-copy-faslop $fasl-prog1)

(defxloadfaslop $fasl-src (s)
  (%cant-epush s)
  (let* ((path (%fasl-expr s)))
    (setq *xload-loading-file-source-file* path)))


(defparameter *xcompile-features* nil)



(defun target-Xcompile-directory (target dir &optional force)
  (let* ((backend (find-backend target))
	 (any (not (null force)))
         (outpath (merge-pathnames dir (backend-target-fasl-pathname backend))))
    (in-development-mode
     (dolist (src (sort (directory (merge-pathnames dir "*.lisp"))
			#'string< :key #'namestring)
	      any)
       (let* ((fasl (merge-pathnames outpath  src)))
	 (when (or force
		   (not (probe-file fasl))
		   (> (file-write-date src)
		      (file-write-date fasl)))
	   (setq any t)
	   (compile-file src :target target
			 :features *xcompile-features*
			 :output-file  fasl 
			 :verbose t)))))))

(defun target-xcompile-level-0 (target &optional force)
  (let* ((backend (or (find-xload-backend target)
		      (error "Unknown xload backend: ~s" target)))
         ;; Saving doc-strings doesn't work in level-0 (yet.)
         (*save-doc-strings* t)
         (*fasl-save-doc-strings* t)
	 (a (target-xcompile-directory target "ccl:level-0;" force))
	 (b
          (dolist (d (backend-xload-info-subdirs backend))
            (target-xcompile-directory target d force))))
    (or a b)))

(defun cross-compile-level-0 (target &optional (recompile t))
  (with-cross-compilation-target (target)
    (target-xcompile-level-0 target recompile)))
    
(defun target-Xload-level-0 (target &optional (recompile t))
  (let* ((*xload-target-backend* (or (find-xload-backend target)
				     *xload-default-backend*))
	 (*xload-startup-file* (backend-xload-info-default-startup-file-name
				*xload-target-backend*)))
    ;; This just undoes the CLtL1 compatability stuff in
    ;; "ccl:library;lisp-package".  If someone's created LISP and/or
    ;; USER packages, nuke 'em.
    (let* ((user-pkg (find-package "USER"))
	   (lisp-pkg (find-package "LISP")))
      (when (and user-pkg (not (eq user-pkg (find-package "CL-USER"))))
	(delete-package user-pkg))
      (when (and lisp-pkg (not (eq lisp-pkg (find-package "CL"))))
	(delete-package lisp-pkg)))
    (in-development-mode
     (when recompile
       (target-Xcompile-level-0 target (eq recompile :force)))
     (setup-xload-target-parameters)
     (let* ((*load-verbose* t)
	    (compiler-backend (find-backend
			       (backend-xload-info-compiler-target-name
				*xload-target-backend*)))
	    (wild-fasls (concatenate 'simple-string
				     "*."
				     (pathname-type
				      (backend-target-fasl-pathname
				       compiler-backend))))
	    (wild-root (merge-pathnames "ccl:level-0;" wild-fasls))
	    (wild-subdirs
             (mapcar #'(lambda (d)
                         (merge-pathnames d wild-fasls))
			  (backend-xload-info-subdirs *xload-target-backend*)))
	    (*xload-image-file-name* (backend-xload-info-default-image-name *xload-target-backend*)))
       (apply #'xfasload *xload-image-file-name*
              (append
               (apply #'append
                      (mapcar #'(lambda (d)
                                  (sort (directory d) #'string< :key #'namestring))
                              wild-subdirs))
               (sort (directory wild-root) #'string< :key #'namestring)))
       (format t "~&;Wrote bootstrapping image: ~s" (truename *xload-image-file-name*))))))

(defun cross-xload-level-0 (target &optional (recompile t))
  (with-cross-compilation-target (target)
    (let* ((*target-backend* (find-backend target)))
      (target-xload-level-0 target recompile))))

(provide "XFASLOAD")
