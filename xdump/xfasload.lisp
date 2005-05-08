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



(defstruct backend-xload-info
  name
  application-entry-code
  macro-apply-code
  closure-trampoline-code
  udf-code
  excised-code				;for calls to function in excised libs
  default-image-name
  default-startup-file-name
  relativize-subprims-hook
  prepend-subprims-hook
  subdir
  compiler-target-name
)

(defvar *xload-backends* nil)
(defvar *xload-default-backend*)
(defvar *xload-target-backend*)

(defun find-xload-backend (target)
  (find target *xload-backends* :key #'backend-xload-info-name))

(defun add-xload-backend (b)
  (let* ((already (find-xload-backend (backend-xload-info-name b))))
    (when already
      (setq *xload-backends* (remove already *xload-backends*)))
    (push b *xload-backends*)))



(defun make-xload-header (element-count subtag)
  (logior (ash element-count ppc32::num-subtag-bits) subtag))


(defparameter *xload-record-source-file-p* t)

(defvar *xload-symbol-header* (make-xload-header ppc32::symbol.element-count ppc32::subtag-symbol))

(defparameter *xload-fasl-dispatch-table* (make-array (length *fasl-dispatch-table*)
                                                     :initial-element #'%bad-fasl))


; Maybe we should use this version for both targets.
; It's only marginally slower than the lap (a couple of extra shift instructions)

(progn

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

) ; end of #-ppc-target

(defun untagged-addr (addr)
  (logand (lognot 7) addr))

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

; :constructor ... :constructor ... <gasp> ... must remember ... :constructor

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

;;; This should probably be a function of the target backend.
(defparameter *xload-image-base-address*
  #+darwinppc-target #x02000000
  #+linuxppc-target #x31000000)

(defparameter *xload-static-reserve* (ash 1 15))
(defparameter *xload-purespace-reserve* #x04000000)
(defparameter *xload-static-space-address* (ash 1 12))
(defparameter *xload-static-space-size* (ash 8 10))
(defparameter *xload-readonly-space-address* *xload-image-base-address*)
(defparameter *xload-readonly-space-size* (ash 1 18))
(defparameter *xload-dynamic-space-address* (+ *xload-image-base-address*
					       *xload-purespace-reserve*))
(defparameter *xload-dynamic-space-size* (ash 1 18))
(defparameter *xload-nil* (+ *xload-static-space-address* 16 (* 1024 4) ppc32::fulltag-nil))
(defparameter *xload-T* (+ *xload-nil* ppc32::t-offset))
(defparameter *xload-nilsym* (+ *xload-T* ppc32::symbol.size))
(defparameter %xload-unbound-function% (+ *xload-dynamic-space-address* ppc32::fulltag-misc))
(defparameter *xload-dynamic-space* nil)
(defparameter *xload-readonly-space* nil)
(defparameter *xload-static-space* nil)
(defparameter *xload-symbols* nil)
(defparameter *xload-package-alist* nil)         ; maps real package to clone
(defparameter *xload-aliased-package-addresses* nil)     ; cloned package to address
(defparameter *xload-cold-load-functions* nil)
(defparameter *xload-cold-load-documentation* nil)
(defparameter *xload-fcell-refs* nil)    ; function cells referenced, for warnings
(defparameter *xload-vcell-refs* nil)    ; value cells, for warnings
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

(defun xload-immval (imm)
  (if (or (typep imm 'fixnum)
          (and (<= #.(ash (- (expt 2 31)) (- ppc32::fixnum-shift))
                   imm
                   #.(1- (ash (expt 2 31) (- ppc32::fixnum-shift))))))
    (ash imm ppc32::fixnumshift)
    (error "Bad idea.")))

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

; element-count doesn't include header
(defun xload-alloc-fullwords (space tag nelements)
  (xload-alloc space tag (logand (lognot 7) (+ 7 4 (ash nelements 2)))))

(defun xload-alloc-halfwords (space tag nelements)
  (xload-alloc space tag (logand (lognot 7) (+ 7 4 (ash nelements 1)))))

(defun xload-alloc-bytes (space tag nelements)
  (xload-alloc space tag (logand (lognot 7) (+ 7 4 nelements))))

(defun xload-alloc-doublewords (space tag nelements)
  (xload-alloc space tag (logand (lognot 7) (+ 7 4 (ash nelements 3)))))

(defun xload-alloc-bits (space tag nelements)
  (xload-alloc space tag (logand (lognot 7) (+ 7 4 (ash (+ 7 nelements) -3)))))


(defun xload-make-cons (car cdr &optional (space *xload-dynamic-space*))
  (multiple-value-bind (cell-addr data offset) (xload-alloc space  ppc32::fulltag-cons ppc32::cons.size)
    (setf (u32-ref data (the fixnum (+ offset ppc32::cons.car))) car)
    (setf (u32-ref data (the fixnum (+ offset ppc32::cons.cdr))) cdr)
    cell-addr))

; This initializes the gvector's contents to 0.  Might want to
; consider initializing it to NIL for the benefit of package and
; hashtable code.
(defun xload-make-gvector (subtag len)
  (declare (fixnum subtype len))
  (multiple-value-bind (cell-addr data offset) (xload-alloc-fullwords *xload-dynamic-space* ppc32::fulltag-misc len)
    (declare (fixnum offset))
    (setf (u32-ref data (+ offset ppc32::misc-header-offset)) (make-xload-header len subtag))
    cell-addr))

(defun xload-make-word-ivector (subtag len space)
  (declare (fixnum subtype len))
  (multiple-value-bind (cell-addr data offset) (xload-alloc-fullwords space  ppc32::fulltag-misc len)
    (declare (fixnum offset))
    (setf (u32-ref data (+ offset ppc32::misc-header-offset)) (make-xload-header len subtag))
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
					(package-address *xload-nil*)
					(space *xload-dynamic-space*))
  (multiple-value-bind (cell-addr data offset) (xload-alloc-fullwords space ppc32::fulltag-misc ppc32::symbol.element-count)
    (declare (fixnum offset))
      (setf (u32-ref data (+ offset ppc32::symbol.header)) *xload-symbol-header*)
      (setf (u32-ref data (+ offset ppc32::symbol.flags)) 0)
      (setf (u32-ref data (+ offset ppc32::symbol.pname)) pname-address)
      (setf (u32-ref data (+ offset ppc32::symbol.vcell)) ppc32::unbound-marker)
      (setf (u32-ref data (+ offset ppc32::symbol.package-plist)) package-address)
      (setf (u32-ref data (+ offset ppc32::symbol.fcell)) %xload-unbound-function%)
      ;(break "Made symbol at #x~x (#x~x)" cell-addr offset)
      cell-addr))

; No importing or shadowing can (easily) happen during the cold load; a symbol is present
; in no package other than its home package.
; This -just- finds or adds the symbol in the "clone" package's itab/etab.
; Somebody else has to copy the symbol to the image ...
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
     

(defun xload-dword-align (nbytes &optional (header-p t))
  (logand (lognot 7) (+ nbytes 7 (if header-p 4 0))))

(defun xload-subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (= ppc32::fulltag-immheader (logand subtag ppc32::fulltagmask))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((element-bit-shift
          (if (<= subtag ppc32::max-32-bit-ivector-subtag)
            5
            (if (<= subtag ppc32::max-8-bit-ivector-subtag)
              3
              (if (<= subtag ppc32::max-16-bit-ivector-subtag)
                4
                (if (= subtag ppc32::subtag-double-float-vector)
                  6
                  1)))))
         (total-bits (ash element-count element-bit-shift)))
    (ash (+ 7 total-bits) -3)))    ; byte count

    
(defun xload-make-dfloat (space high low)
  (multiple-value-bind (dfloat-addr v o) (xload-alloc-fullwords space ppc32::fulltag-misc ppc32::double-float.element-count)
    (declare (fixnum o))
    (setf (u32-ref v (the fixnum (+ o ppc32::double-float.header))) 
          (make-xload-header ppc32::double-float.element-count ppc32::subtag-double-float))
    (setf (u32-ref v (the fixnum (+ o ppc32::double-float.value))) high)
    (setf (u32-ref v (the fixnum (+ o ppc32::double-float.val-low))) low)
    dfloat-addr))

(defun xload-make-sfloat (space bits)
  (multiple-value-bind (sfloat-addr v o) (xload-alloc-fullwords space ppc32::fulltag-misc ppc32::single-float.element-count)
    (declare (fixnum o))
    (setf (u32-ref v (the fixnum (+ o ppc32::single-float.header))) 
          (make-xload-header ppc32::single-float.element-count ppc32::subtag-single-float))
    (setf (u32-ref v (the fixnum (+ o ppc32::single-float.value))) bits)
    sfloat-addr))
        
(defun xload-make-ivector (space subtag nelements)
  (declare (fixnum subtype nelements))
  (multiple-value-bind (addr v o) (xload-alloc space ppc32::fulltag-misc (xload-dword-align (xload-subtag-bytes subtag nelements) t))
    (declare (fixnum o))
    (setf (u32-ref v (the fixnum (- o ppc32::fulltag-misc))) (make-xload-header nelements subtag))
    (values addr v o)))

(defun xload-%svref (addr i)
  (declare (fixnum i))
  (if (= (the fixnum (logand addr ppc32::fulltagmask)) ppc32::fulltag-misc)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (u32-ref v (the fixnum (+ offset (the fixnum (+ ppc32::misc-data-offset (the fixnum (ash i 2))))))))
    (error "Not a vector: #x~x" addr)))

(defun (setf xload-%svref) (new addr i)
  (declare (fixnum i))
  (if (= (the fixnum (logand addr ppc32::fulltagmask)) ppc32::fulltag-misc)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (u32-ref v (the fixnum (+ offset (the fixnum (+ ppc32::misc-data-offset (the fixnum (ash i 2))))))) new))
    (error "Not a vector: #x~x" addr)))

(defun xload-car (addr)
  (if (= (the fixnum (logand addr ppc32::tagmask)) ppc32::tag-list)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (u32-ref v (the fixnum (+ offset ppc32::cons.car))))
    (error "Not a list: #x~x" addr)))

(defun (setf xload-car) (new addr)
  (if (= (the fixnum (logand addr ppc32::tagmask)) ppc32::tag-list)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (u32-ref v (the fixnum (+ offset ppc32::cons.car))) new))
    (error "Not a list: #x~x" addr)))

(defun xload-cdr (addr)
  (if (= (the fixnum (logand addr ppc32::tagmask)) ppc32::tag-list)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (u32-ref v (the fixnum (+ offset ppc32::cons.cdr))))
    (error "Not a list: #x~x" addr)))

(defun (setf xload-cdr) (new addr)
  (if (= (the fixnum (logand addr ppc32::tagmask)) ppc32::tag-list)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (u32-ref v (the fixnum (+ offset ppc32::cons.cdr))) new))
    (error "Not a list: #x~x" addr)))

(defun xload-symbol-value (addr)
  (if (= (xload-%svref addr -1) *xload-symbol-header*)
    (xload-%svref addr ppc32::symbol.vcell-cell)
    (error "Not a symbol: #x~x" addr)))
  
(defun xload-symbol-name (addr)
  (if (= addr *xload-nil*) (incf addr (+ ppc32::t-offset ppc32::symbol.size)))
  (let* ((header (xload-%svref addr -1)))
    (if (= header *xload-symbol-header*)
      (xload-%svref addr ppc32::symbol.pname-cell)
      (error "Not a symbol: #x~x" addr))))

(defun (setf xload-symbol-value) (new addr)
  (if (= (xload-%svref addr -1) *xload-symbol-header*)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (u32-ref v (the fixnum (+ offset ppc32::symbol.vcell))) new))
    (error "Not a symbol: #x~x" addr)))

(defun xload-set (sym val)
  (check-type sym symbol)
  (check-type val integer)
  (let* ((symaddr (xload-lookup-symbol sym)))
    (unless symaddr (error "Symbol address not found: ~s ." sym))
    (setf (xload-symbol-value symaddr) val)))

(defun xload-fset (addr def)
  (if (= (xload-%svref addr -1) *xload-symbol-header*)
    (multiple-value-bind (v offset) (xload-lookup-address addr)
      (declare (fixnum offset))
      (setf (u32-ref v (the fixnum (+ offset ppc32::symbol.fcell))) def))
    (error "Not a symbol: #x~x" addr)))

(defun (setf xload-symbol-plist) (new addr)
  (let* ((plist-addr (+ addr ppc32::symbol.package-plist))
         (package-plist (xload-u32-at-address plist-addr)))
    (if (= ppc32::fulltag-cons (logand package-plist ppc32::fulltagmask))
      (let* ((pname-addr (+ addr ppc32::symbol.pname))
	     (pname (xload-u32-at-address pname-addr))
	     (header (xload-u32-at-address (+ pname ppc32::misc-header-offset)))
	     (len (ash header -8))
	     (str (make-string len)))
	(multiple-value-bind (v o) (xload-lookup-address pname)
	  (incf o ppc32::misc-data-offset)
	  (dotimes (i len) (setf (schar str i)
				 (code-char (u8-ref v (+ i o)))))
	  (break "str = ~s" str)
	  (warn "Symbol at #x~x: plist already set." addr)))
      (setf (xload-u32-at-address plist-addr)
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
                                       *xload-nil*)
				     space)))
        (xload-intern symbol)
        (let* ((bits (logandc2 (%symbol-bits symbol) (ash 1 $sym_vbit_typeppred))))
          (setf (xload-u32-at-address (+ addr ppc32::symbol.flags))
                (ash (if preserve-constantness
                       bits
                       (logand (lognot (ash 1 $sym_vbit_const)) bits)) 
                     ppc32::fixnumshift)))
        (if (and (constantp symbol)
                 (eq (symbol-value symbol) symbol))
          (setf (xload-symbol-value addr) addr))
        (setf (xload-lookup-symbol symbol) addr))))


;;; Write a list to dynamic space.  No detection of circularity or
;;; structure sharing.  The cdr of the final cons can be nil (treated
;;; as *xload-nil*.)  All cars must be addresses.

(defun xload-save-list (l)
  (if (atom l)
    (or l *xload-nil*)
    (xload-make-cons (car l) (xload-save-list (cdr l)))))

(defun xload-save-string (str &optional (n (length str)))
  (declare (fixnum n))
  (let* ((subtag ppc32::subtag-simple-base-string))
    (multiple-value-bind (addr v offset) (xload-make-ivector *xload-readonly-space* subtag n)
      (do* ((p (+ offset ppc32::misc-data-offset) (1+ p))
              (i 0 (1+ i)))
             ((= i n) str)
          (declare (fixnum i p))
          (setf (u8-ref v p) (char-code (schar str i))))
        addr)))

;;; Read a string from fasl file, save it to readonly-space.
(defun %xload-fasl-readstr (s)
  (multiple-value-bind (str n new-p) (%fasl-readstr s)
    (declare (fixnum n subtype))
    (values (xload-save-string str n) str n new-p)))

(defun %xload-fasl-vreadstr (s)
  (multiple-value-bind (str n new-p) (%fasl-vreadstr s)
    (declare (fixnum n subtype))
    (values (xload-save-string str n) str n new-p)))

(defun xload-clone-packages (packages)
  (let* ((alist (mapcar #'(lambda (p)
                            (cons p
                                  (gvector :package
                                            (cons (make-array (the fixnum (length (car (uvref p 0))))
                                                              :initial-element nil)
                                                  (cons 0 (cddr (uvref p 0))))   ; itab
                                            (cons (make-array (the fixnum (length (car (uvref p 1))))
                                                              :initial-element nil)
                                                  (cons 0 (cddr (uvref p 1))))   ; etab
                                            nil                         ; used
                                            nil                         ; used-by
                                            (copy-list (uvref p 4))     ; names
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

; Dump each cloned package into dynamic-space; return an alist
(defun xload-assign-aliased-package-addresses (alist)
  (let* ((addr-alist (mapcar #'(lambda (pair)
                                 (let* ((p (cdr pair))
                                        (v (xload-make-gvector ppc32::subtag-package (uvsize p))))
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
  
(defun xload-check-symbols ()
  )


(defun xload-save-htab (htab)
  (let* ((htvec (car htab))
         (len (length htvec))
         (xvec (xload-make-gvector ppc32::subtag-simple-vector len))
         (deleted-marker ppc32::unbound-marker))
    (dotimes (i len)
      (let* ((s (%svref htvec i)))
        (setf (xload-%svref xvec i)
              (if s
                (if (symbolp s) (xload-lookup-symbol s) deleted-marker)
                *xload-nil*))))
    (xload-make-cons  
     xvec 
     (xload-make-cons
      (xload-immval (cadr htab))
      (xload-immval (cddr htab))))))

(defun xload-finalize-packages ()
  (dolist (pair *xload-aliased-package-addresses*)
    (let* ((p (car pair))
           (q (cdr pair)))
      (setf (xload-%svref q pkg.etab) (xload-save-htab (pkg.etab p)))
      (setf (xload-%svref q pkg.itab) (xload-save-htab (pkg.itab p))))))

(defun xload-get-string (address)
    (multiple-value-bind (v o) (xload-lookup-address address)
      (let* ((header (u32-ref v (+ o ppc32::misc-header-offset)))
             (len (ash header (- ppc32::num-subtag-bits)))
             (str (make-string len :element-type 'base-char))
             (p (+ o ppc32::misc-data-offset)))
        (dotimes (i len str)
          (setf (schar str i) (code-char (u8-ref v (+ p i))))))))

(defun xload-note-undefined-references ()
  (let* ((unbound-vcells nil)
         (undefined-fcells nil))
    (dolist (vcell *xload-vcell-refs*)
      (when (=  (xload-u32-at-address (+ vcell ppc32::symbol.vcell))
               ppc32::unbound-marker)
        (push vcell unbound-vcells)))
    (dolist (fcell *xload-fcell-refs*)
      (declare (fixnum fcell))
      (when (= (xload-u32-at-address (+ fcell ppc32::symbol.fcell))
               %xload-unbound-function%)
        (push fcell undefined-fcells)))
    (flet ((xload-symbol-name-string (addr) (xload-get-string (xload-symbol-name addr))))
      (when undefined-fcells
        (warn "Function names referenced but not defined: ~a" (mapcar #'xload-symbol-name-string undefined-fcells)))
      (when unbound-vcells
        (warn "Variable names referenced but not defined: ~a" (mapcar #'xload-symbol-name-string unbound-vcells))))))
  
(defparameter *xload-use-trampoline-subprims-code* t)


               
(defun xload-save-code-vector (code)
  (let* ((read-only-p *xload-pure-code-p*)
         (n (uvsize code)))
    (declare (fixnum n))
    (multiple-value-bind (vector v o) 
                         (xload-make-ivector 
                          (if read-only-p
                            *xload-readonly-space*
                            *xload-dynamic-space*)
                          ppc32::subtag-code-vector
                          n)
      (dotimes (i n)
        (setf (xload-%svref vector i) (uvref code i)))
      (when read-only-p
	(funcall
	 (backend-xload-info-relativize-subprims-hook *xload-target-backend*)
	 v (+ o ppc32::misc-data-offset) n))
      vector)))
                          
                          

(defun xfasload (output-file &rest pathnames)
  (let* ((*xload-symbols* (make-hash-table :test #'eq))
         (*xload-spaces* nil)
         (*xload-readonly-space* (init-xload-space *xload-readonly-space-address* *xload-readonly-space-size* ppc32::area-readonly))
         (*xload-dynamic-space* (init-xload-space *xload-dynamic-space-address* *xload-dynamic-space-size* ppc32::area-dynamic))
	 (*xload-static-space* (init-xload-space *xload-static-space-address* *xload-static-space-size* ppc32::area-static))
						 
         (*xload-package-alist* (xload-clone-packages %all-packages%))
         (*xload-cold-load-functions* nil)
         (*xload-cold-load-documentation* nil)
         (*xload-loading-file-source-file* nil)
         (*xload-vcell-refs* nil)
         (*xload-fcell-refs* nil)
         (*xload-svar-alist* nil)
         (*xload-aliased-package-addresses* nil))
    (xload-make-word-ivector ppc32::subtag-u32-vector 1027 *xload-static-space*)
    ; Make NIL.  Note that NIL is sort of a misaligned cons (it straddles two doublewords.)
    (xload-make-cons *xload-nil* 0 *xload-static-space*)
    (xload-make-cons 0 *xload-nil* *xload-static-space*)
    ;; Create %unbound-function% and the package objects in dynamic space,
    ;; then fill in the nilreg-relative symbols in static space.
    ;; Then start consing ..
    ;; The undefined-function object is a 1-element simple-vector (not a function vector).
    ;; The code-vector in its 0th element should report the appropriate error.
    (let* ((udf-object (xload-make-gvector ppc32::subtag-simple-vector 1)))
      (setf (xload-%svref udf-object 0) (xload-save-code-vector
					 (backend-xload-info-udf-code
					  *xload-target-backend*))))
    (setq *xload-aliased-package-addresses* (xload-assign-aliased-package-addresses *xload-package-alist*))
    (dolist (pair *xload-nrs*)
      (let* ((val-p (consp pair))
	     (val (if val-p (or (cdr pair) *xload-nil*)))
	     (sym (if val-p (car pair) pair)))
	(xload-copy-symbol sym
			   :preserve-constantness t
			   :space *xload-static-space*)
	(when val-p (xload-set sym val))))
    ; This could be a little less ... procedural.
    (xload-set '*package* (xload-package->addr *ccl-package*))
    (xload-set '*keyword-package* (xload-package->addr *keyword-package*))
    (xload-set '%all-packages% (xload-save-list (mapcar #'cdr *xload-aliased-package-addresses*)))
    (xload-set '%unbound-function% %xload-unbound-function%)
    (xload-set '*gc-event-status-bits* (xload-immval 0 #|(ash 1 $gc-integrity-check-bit)|#))
    (xload-set '%toplevel-catch% (xload-copy-symbol :toplevel))
    (xload-set '%closure-code% (xload-save-code-vector
				(backend-xload-info-closure-trampoline-code
				 *xload-target-backend*)))
    (xload-set '%macro-code% (xload-save-code-vector
			      (backend-xload-info-macro-apply-code
			       *xload-target-backend*)))
    (let* ((len (length %builtin-functions%))
           (v (xload-make-gvector ppc32::subtag-simple-vector len)))
      (dotimes (i len)
        (setf (xload-%svref v i) (xload-copy-symbol (svref %builtin-functions% i))))
      (xload-set '%builtin-functions% v))
    (xload-copy-symbol '*xload-startup-file*)
    (xload-fasload pathnames)
    (xload-set '*xload-startup-file*
              (xload-save-string *xload-startup-file*))
    (xload-check-symbols)                ; Make sure interned symbols are.
    (let* ((toplevel (xload-symbol-value (xload-lookup-symbol '%toplevel-function%))))      
      (when (= toplevel
	       ppc32::undefined)
	(warn "~S not set in loading ~S ." '%toplevel-function pathnames)))
    (setf (xload-symbol-value (xload-copy-symbol '*xload-cold-load-functions*))
          (xload-save-list (setq *xload-cold-load-functions*
                                (nreverse *xload-cold-load-functions*))))
    (setf (xload-symbol-value (xload-copy-symbol '*xload-cold-load-documentation*))
          (xload-save-list (setq *xload-cold-load-documentation*
                                 (nreverse *xload-cold-load-documentation*))))
                              
    (xload-note-undefined-references)
    (xload-finalize-packages)
    (xload-dump-image output-file *xload-image-base-address*)))

(defun xload-dump-image (output-file heap-start)
  (declare (ftype (function (t t list)) write-image-file))
  ;; Write the 0th and first elements of the kernel_globals vectors as
  ;; untagged pointers to the beginning and end of "static space".
  (setf (xload-u32-at-address (+ *xload-static-space-address* 16)) *xload-static-space-address*)
  (setf (xload-u32-at-address (+ *xload-static-space-address* 20)) (+ *xload-static-space-address* (xload-space-lowptr *xload-static-space*)))

  ;; The next word is supposed to be a pointer to some other static
  ;; section in the application.  The following two words describe the
  ;; start and end of the readonly section.
  (setf (xload-u32-at-address (+ *xload-static-space-address* 28)) *xload-readonly-space-address*)
  (setf (xload-u32-at-address (+ *xload-static-space-address* 32)) (+ *xload-readonly-space-address* (xload-space-lowptr *xload-readonly-space*)))
  (write-image-file output-file
		    heap-start
		    (list *xload-readonly-space*
			  *xload-static-space*
			  *xload-dynamic-space*)))
		    





;;; The xloader

(xload-copy-faslop $fasl-noop)
(xload-copy-faslop $fasl-etab-alloc)
(xload-copy-faslop $fasl-eref)
(xload-copy-faslop $fasl-vetab-alloc)
(xload-copy-faslop $fasl-veref)

; Should error if epush bit set, else push on *xload-cold-load-functions* or something.
(defxloadfaslop $fasl-lfuncall (s)
  (let* ((fun (%fasl-expr-preserve-epush s)))
    (when (faslstate.faslepush s)
      (error "Can't call function for value : ~s" fun))
    (format t "~& cold-load function:")
    (push fun *xload-cold-load-functions*)))

(xload-copy-faslop $fasl-globals)        ; what the hell did this ever do ?

; fasl-char: maybe epush, return 32-bit representation of BASE-CHARACTER

(defxloadfaslop $fasl-char (s)
  (%epushval s (xload-immval (code-char (%fasl-read-byte s)))))

(defxloadfaslop $fasl-fixnum (s)
  (%epushval s (xload-immval
                ;; This nonsense converts unsigned %fasl-read-long result to signed
                (rlet ((long :long))
                  (setf (%get-long long) (%fasl-read-long s))
                  (%get-long long)))))

(defxloadfaslop $fasl-dfloat (s)
  (%epushval s (xload-make-dfloat *xload-readonly-space* (%fasl-read-long s) (%fasl-read-long s))))

(defxloadfaslop $fasl-sfloat (s)
  (%epushval s (xload-make-sfloat *xload-readonly-space* (%fasl-read-long s))))

(defxloadfaslop $fasl-str (s)
  (let* ((n (%fasl-read-size s)))
    (multiple-value-bind (str v o) (xload-make-ivector *xload-readonly-space* ppc32::subtag-simple-base-string n)
      (%epushval s str)
      (%fasl-read-n-bytes s v (+ o  ppc32::misc-data-offset) n)
      str)))

(defxloadfaslop $fasl-vstr (s)
  (let* ((n (%fasl-read-count s)))
    (multiple-value-bind (str v o) (xload-make-ivector *xload-readonly-space* ppc32::subtag-simple-base-string n)
      (%epushval s str)
      (%fasl-read-n-bytes s v (+ o  ppc32::misc-data-offset) n)
      str)))

(defxloadfaslop $fasl-word-fixnum (s)
  (%epushval s (xload-immval (%word-to-int (%fasl-read-word s)))))

(defun %xload-fasl-make-symbol (s)
  (%epushval s (xload-make-symbol (%xload-fasl-readstr s))))

(defun %xload-fasl-vmake-symbol (s)
  (%epushval s (xload-make-symbol (%xload-fasl-vreadstr s))))

(defxloadfaslop $fasl-mksym (s)
  (%xload-fasl-make-symbol s))

(defxloadfaslop $fasl-vmksym (s)
  (%xload-fasl-vmake-symbol s))

(defun %xload-fasl-intern (s package)
  (multiple-value-bind (str len new-p) (%fasl-readstr s)
    (without-interrupts
     (multiple-value-bind (cursym access internal external) (%find-symbol str len package)
       (unless access
         (unless new-p (setq str (%fasl-copystr str len)))
         (setq cursym (%add-symbol str package internal external)))
       ; cursym now exists in the load-time world; make sure that it exists
       ; (and is properly "interned" in the world we're making as well)
       (%epushval s (xload-copy-symbol cursym))))))

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

(defxloadfaslop $fasl-intern (s)
  (%xload-fasl-intern s *package*))

(defxloadfaslop $fasl-vintern (s)
  (%xload-fasl-vintern s *package*))

(defxloadfaslop $fasl-pkg-intern (s)
  (let* ((addr (%fasl-expr-preserve-epush  s))
         (pkg (xload-addr->package addr)))
    (%xload-fasl-intern s pkg)))

(defxloadfaslop $fasl-vpkg-intern (s)
  (let* ((addr (%fasl-expr-preserve-epush  s))
         (pkg (xload-addr->package addr)))
    (%xload-fasl-vintern s pkg)))

(defun %xload-fasl-package (s)
  (multiple-value-bind (str len new-p) (%fasl-readstr s)
    (let* ((p (%find-pkg str len)))
      (%epushval s (xload-package->addr 
                    (or p (%kernel-restart $XNOPKG (if new-p str (%fasl-copystr str len)))))))))

(defun %xload-fasl-vpackage (s)
  (multiple-value-bind (str len new-p) (%fasl-vreadstr s)
    (let* ((p (%find-pkg str len)))
      (%epushval s (xload-package->addr 
                    (or p (%kernel-restart $XNOPKG (if new-p str (%fasl-copystr str len)))))))))

(defxloadfaslop $fasl-pkg (s)
  (%xload-fasl-package s))

(defxloadfaslop $fasl-vpkg (s)
  (%xload-fasl-vpackage s))

(defxloadfaslop $fasl-cons (s)
  (let* ((cons (%epushval s (xload-make-cons *xload-nil* *xload-nil*))))
    (setf (xload-car cons) (%fasl-expr s)
          (xload-cdr cons) (%fasl-expr s))
    (setf (faslstate.faslval s) cons)))
    
(defun %xload-fasl-listX (s dotp)
  (let* ((len (%fasl-read-word s)))
    (declare (fixnum len))
    (let* ((val (%epushval s (xload-make-cons *xload-nil* *xload-nil*)))
           (tail val))
      (setf (xload-car val) (%fasl-expr s))
      (dotimes (i len)
        (setf (xload-cdr tail) (setq tail (xload-make-cons  (%fasl-expr s) *xload-nil*))))
      (if dotp
        (setf (xload-cdr tail) (%fasl-expr s)))
      (setf (faslstate.faslval s) val))))

(defun %xload-fasl-vlistX (s dotp)
  (let* ((len (%fasl-read-count s)))
    (declare (fixnum len))
    (let* ((val (%epushval s (xload-make-cons *xload-nil* *xload-nil*)))
           (tail val))
      (setf (xload-car val) (%fasl-expr s))
      (dotimes (i len)
        (setf (xload-cdr tail) (setq tail (xload-make-cons  (%fasl-expr s) *xload-nil*))))
      (if dotp
        (setf (xload-cdr tail) (%fasl-expr s)))
      (setf (faslstate.faslval s) val))))

(defxloadfaslop $fasl-list (s)
  (%xload-fasl-listX s nil))

(defxloadfaslop $fasl-vlist (s)
  (%xload-fasl-vlistX s nil))

(defxloadfaslop $fasl-list* (s)
  (%xload-fasl-listX s t))

(defxloadfaslop $fasl-vlist* (s)
  (%xload-fasl-vlistX s t))

(defxloadfaslop $fasl-nil (s)
  (%epushval s *xload-nil*))

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
			 ppc32::fixnumshift))
      (error "Not a ~A fasl file : ~s" backend-name (faslstate.faslfname s)))))

(defxloadfaslop $fasl-symfn (s)
  (let* ((symaddr (%fasl-expr-preserve-epush s))
         (fnobj (xload-u32-at-address (+ symaddr ppc32::symbol.fcell))))
    (if (and (= ppc32::fulltag-misc (logand fnobj ppc32::fulltagmask))
             (= ppc32::subtag-function (xload-u8-at-address (+ fnobj ppc32::misc-subtag-offset))))
      (%epushval s fnobj)
      (error "symbol at #x~x is unfbound . " symaddr))))

(defxloadfaslop $fasl-eval (s)
  (let* ((expr (%fasl-expr-preserve-epush s)))
    (error "Can't evaluate expression ~s in cold load ." expr)
    (%epushval s (eval expr))))         ; could maybe evaluate symbols, constants ...

; Whether or not it's a good idea to allocate other things there
; is hard to say.  Let's assume that it is, for the time being.
(defxloadfaslop $fasl-ivec (s)
  (let* ((subtag (%fasl-read-byte s))
         (element-count (%fasl-read-size s))
         (read-only-code (and (= subtag ppc32::subtag-code-vector) *xload-pure-code-p*)))
    (declare (fixnum subtag))
    (when (= subtag ppc32::subtag-xcode-vector)
      (setq subtag ppc32::subtag-code-vector))
    (multiple-value-bind (vector v o)
                         (xload-make-ivector 
                          (if (and (= subtag ppc32::subtag-code-vector) (not *xload-pure-code-p*))
                            *xload-dynamic-space* 
                            *xload-readonly-space*)
                          subtag 
                          element-count)
      (%epushval s vector)
      (%fasl-read-n-bytes s v (+ o  ppc32::misc-data-offset) (xload-subtag-bytes subtag element-count))
      (when read-only-code
	(funcall
	 (backend-xload-info-relativize-subprims-hook *xload-target-backend*)
         v (+ o ppc32::misc-data-offset) element-count))
      vector)))

(defxloadfaslop $fasl-vivec (s)
  (let* ((subtag (%fasl-read-byte s))
         (element-count (%fasl-read-count s))
         (read-only-code (and (= subtag ppc32::subtag-code-vector) *xload-pure-code-p*)))
    (declare (fixnum subtag))
    (when (= subtag ppc32::subtag-xcode-vector)
      (setq subtag ppc32::subtag-code-vector))
    (multiple-value-bind (vector v o)
                         (xload-make-ivector 
                          (if (and (= subtag ppc32::subtag-code-vector) (not *xload-pure-code-p*))
                            *xload-dynamic-space* 
                            *xload-readonly-space*)
                          subtag 
                          element-count)
      (%epushval s vector)
      (%fasl-read-n-bytes s v (+ o  ppc32::misc-data-offset) (xload-subtag-bytes subtag element-count))
      (when read-only-code
	(funcall
	 (backend-xload-info-relativize-subprims-hook *xload-target-backend*)
         v (+ o ppc32::misc-data-offset) element-count))
      vector)))

;;; About all we do with svars is to canonicalize them.
(defxloadfaslop $fasl-gvec (s)
  (let* ((subtype (%fasl-read-byte s))
         (n (%fasl-read-size s)))
    (declare (fixnum subtype n))
    (if (and (= subtype ppc32::subtag-svar)
             (= n ppc32::svar.element-count))
      (let* ((epush (faslstate.faslepush s))
             (ecount (faslstate.faslecnt s)))
        (when epush
          (%epushval s 0))
        (let* ((sym (%fasl-expr s))
               (ignore (%fasl-expr s))
               (vector (cdr (assq sym *xload-svar-alist*))))
          (declare (ignore ignore))
          (unless vector
            (setq vector (xload-make-gvector subtype n))
            (setf (xload-%svref vector ppc32::svar.symbol-cell) sym)
            (push (cons sym vector) *xload-svar-alist*))
          (when epush
            (setf (svref (faslstate.faslevec s) ecount) vector))
          (setf (faslstate.faslval s) vector)))
      (let* ((vector (xload-make-gvector subtype n)))
        (%epushval s vector)
        (dotimes (i n (setf (faslstate.faslval s) vector))
          (setf (xload-%svref vector i) (%fasl-expr s)))))))

(defxloadfaslop $fasl-vgvec (s)
  (let* ((subtype (%fasl-read-byte s))
         (n (%fasl-read-count s)))
    (declare (fixnum subtype n))
    (if (and (= subtype ppc32::subtag-svar)
             (= n ppc32::svar.element-count))
      (let* ((epush (faslstate.faslepush s))
             (ecount (faslstate.faslecnt s)))
        (when epush
          (%epushval s 0))
        (let* ((sym (%fasl-expr s))
               (ignore (%fasl-expr s))
               (vector (cdr (assq sym *xload-svar-alist*))))
          (declare (ignore ignore))
          (unless vector
            (setq vector (xload-make-gvector subtype n))
            (setf (xload-%svref vector ppc32::svar.symbol-cell) sym)
            (push (cons sym vector) *xload-svar-alist*))
          (when epush
            (setf (svref (faslstate.faslevec s) ecount) vector))
          (setf (faslstate.faslval s) vector)))
      (let* ((vector (xload-make-gvector subtype n)))
        (%epushval s vector)
        (dotimes (i n (setf (faslstate.faslval s) vector))
          (setf (xload-%svref vector i) (%fasl-expr s)))))))

(defun xload-note-cell-ref (loc imm)
  (if (= loc ppc32::symbol.fcell)
    (pushnew imm *xload-fcell-refs*)
    (if (= loc ppc32::symbol.vcell)
      (pushnew imm *xload-vcell-refs*)))
  (+ loc imm))


(defun xload-lfun-name (lf)
  (let* ((header (xload-%svref lf -1)))
    (unless (= ppc32::subtag-function (logand header (1- (ash 1 ppc32::num-subtag-bits))))
      (error "Not a function address: ~s" lf))
    (let* ((n (ash header (- ppc32::num-subtag-bits))))
      (if (> n 2)
        (let* ((bits (ash (xload-%svref lf (1- n)) (- ppc32::fixnumshift))))
          (unless (logbitp 29 bits)
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
                           (xload-make-cons  pathaddr *xload-nil*)
                           (xload-make-cons
                            (xload-make-cons 
                             (xload-make-cons  (xload-copy-symbol indicator) pathaddr)
                             *xload-nil*)
                            *xload-nil*))))
            (setf (xload-symbol-plist symaddr) (xload-make-cons keyaddr keyval))))))))

(defun xload-set-documentation (symaddr indicator doc)
  (push (xload-save-list
         (list symaddr
               (xload-copy-symbol indicator)
               doc))
        *xload-cold-load-documentation*))



(defxloadfaslop $fasl-defun (s)
  (%cant-epush s)
  (let* ((fun (%fasl-expr s))
         (doc (%fasl-expr s)))
    (let* ((sym (xload-lfun-name fun)))
      (unless (= doc *xload-nil*)
        (xload-set-documentation sym 'function doc))
      (xload-record-source-file sym 'function)
      (xload-fset sym fun))))

(defxloadfaslop $fasl-macro (s)
  (%cant-epush s)
  (let* ((fun (%fasl-expr s))
         (doc (%fasl-expr s)))
    (let* ((sym (xload-lfun-name fun))
           (vector (xload-make-gvector ppc32::subtag-simple-vector 2)))
      (setf (xload-%svref vector 0) (xload-symbol-value (xload-lookup-symbol '%macro-code%))
            (xload-%svref vector 1) fun)
      (unless (= doc *xload-nil*)
        (xload-set-documentation sym 'function doc))
      (xload-record-source-file sym 'function)
      (xload-fset sym vector))))

(defxloadfaslop $fasl-defconstant (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s))
         (doc (%fasl-expr s)))
    (unless (= doc *xload-nil*)
      (xload-set-documentation sym 'variable doc))
    (xload-record-source-file sym 'variable)
    (setf (xload-symbol-value sym) val)
    (setf (xload-u32-at-address (+ sym ppc32::symbol.flags))
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash 1 $sym_vbit_const) 
                   (ash (xload-u32-at-address (+ sym ppc32::symbol.flags)) 
                        (- ppc32::fixnumshift)))
           ppc32::fixnumshift))))

(defxloadfaslop $fasl-defparameter (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s))
         (doc (%fasl-expr s)))
    (unless (= doc *xload-nil*)
      (xload-set-documentation sym 'variable doc))
    (xload-record-source-file sym 'variable)
    (setf (xload-symbol-value sym) val)
    (setf (xload-u32-at-address (+ sym ppc32::symbol.flags))
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash (xload-u32-at-address (+ sym ppc32::symbol.flags)) 
                        (- ppc32::fixnumshift)))
           ppc32::fixnumshift))))

(defxloadfaslop $fasl-defvar (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s)))
    (xload-record-source-file sym 'variable)
    (setf (xload-u32-at-address (+ sym ppc32::symbol.flags))
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash (xload-u32-at-address (+ sym ppc32::symbol.flags)) 
                        (- ppc32::fixnumshift)))
           ppc32::fixnumshift))))

(defxloadfaslop $fasl-defvar-init (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s))
         (doc (%fasl-expr s)))
    (unless (= doc *xload-nil*)
      (xload-set-documentation sym 'variable doc))
    (when (= ppc32::unbound-marker (xload-symbol-value sym))
      (setf (xload-symbol-value sym) val))
    (xload-record-source-file sym 'variable)
    (setf (xload-u32-at-address (+ sym ppc32::symbol.flags))
          (ash 
           (logior (ash 1 $sym_vbit_special) 
                   (ash (xload-u32-at-address (+ sym ppc32::symbol.flags)) 
                        (- ppc32::fixnumshift)))
           ppc32::fixnumshift))))


(xload-copy-faslop $fasl-prog1)



(defxloadfaslop $fasl-src (s)
  (%cant-epush s)
  (let* ((path (%fasl-expr s)))
    (setq *xload-loading-file-source-file* path)))


(defparameter *pef-image-file-type* "APPL")
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
	 (b (target-xcompile-directory target
				       (backend-xload-info-subdir backend)
				       force)))
    (or a b)))

(defun target-Xload-level-0 (target &optional (recompile t))
  (let* ((*xload-target-backend* (or (find-xload-backend target)
				     *xload-default-backend*))
	 (*xload-startup-file* (backend-xload-info-default-startup-file-name
				*xload-target-backend*)))
    ;; This just undoes the CLtL1 compatability stuff in "ccl:library;lisp-package".
    ;; If someone's created LISP and/or USER packages, nuke 'em.
    (let* ((user-pkg (find-package "USER"))
	   (lisp-pkg (find-package "LISP")))
      (when (and user-pkg (not (eq user-pkg (find-package "CL-USER"))))
	(delete-package user-pkg))
      (when (and lisp-pkg (not (eq lisp-pkg (find-package "CL"))))
	(delete-package lisp-pkg)))
    (in-development-mode
     (when recompile
       (target-Xcompile-level-0 target (eq recompile :force)))
     (let* ((*load-verbose* t)
	    (compiler-backend (find-backend
			       (backend-xload-info-compiler-target-name
				*xload-target-backend*)))
	    (wild-fasls (concatenate 'simple-base-string
				     "*."
				     (pathname-type
				      (backend-target-fasl-pathname
				       compiler-backend))))
	    (wild-root (merge-pathnames "ccl:level-0;" wild-fasls))
	    (wild-subdir (merge-pathnames
			  (backend-xload-info-subdir *xload-target-backend*)
			  wild-fasls))
	    (*xload-image-file-name* (backend-xload-info-default-image-name *xload-target-backend*)))
       (apply #'xfasload *xload-image-file-name*
	      (append (sort (directory wild-subdir) #'string< :key #'namestring)
		      (sort (directory wild-root) #'string< :key #'namestring)))))))

(provide "XFASLOAD")
