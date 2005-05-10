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

; No error checking, no interrupts, no protect_caller, no nuthin.
; No error, no cons.  No problem.
(defun %progvrestore (saved)
  (declare (optimize (speed 3) (safety 0)))
  (dolist (pair saved)
    (%set-sym-value (car pair) (cdr pair))))

;; Check that something that's supposed to be a proper list of
;; symbols is; error otherwise.
;; This is called only by the compiler output of a PROGV form.
;; It checks for the maximum length that the progvsave subprim
;; can handle.
(defun svar-check-symbol-list (l &optional (max-length
                                        (floor (- 4096 20) (* 4 3))
                                       ))
  (let ((len (list-length l)))
    (if (and len
             (or (null max-length)
                 (< len max-length))
             (dolist (s l t) 
               (unless (and (symbolp s)
                            (not (constant-symbol-p s))
                            (not (logbitp $sym_vbit_global (the fixnum (%symbol-bits s)))))
                 (return nil))))
      (mapcar #'ensure-svar l)
      (error "~s is not a proper list of bindable symbols~@[ of length < ~s~]." l max-length))))

(defun check-symbol-list (l &optional (max-length
                                        (floor (- 4096 20) (* 4 3))
                                       ))
  (let ((len (list-length l)))
    (if (and len
             (or (null max-length)
                 (< len max-length))
             (dolist (s l t) 
               (unless (and (symbolp s)
                            (not (constant-symbol-p s))
                            (not (logbitp $sym_vbit_global (the fixnum (%symbol-bits s)))))
                 (return nil))))
      l
      (error "~s is not a proper list of bindable symbols~@[ of length < ~s~]." l max-length))))

;;; The type-checking done on the "plist" arg shouldn't be removed.
(defun set-symbol-plist (sym plist)
  (let* ((pp (%symbol-package-plist sym))
         (newpp pp))
    (let* ((len (list-length plist)))
      (unless (and len (evenp len))
        (error "Bad plist: ~s" plist)))
    (if (atom pp)
      (setq newpp (cons pp plist))
      (let* ((bits (%symbol-bits sym)))
        (declare (fixnum bits))
        (if (logbitp $sym_vbit_typeppred bits)
          (setf (cdr (cdr pp)) plist)
          (setf (cdr pp) plist))))
    (%set-symbol-package-plist sym newpp)
    plist))


(defun %pl-search (ok-plist key)
  (do* ((l ok-plist (cdr (the list (cdr l)))))
       ((or (null l) (eq (car l) key)) l)
    (declare (list l))))

(defun symbol-plist (sym)
  "Return SYMBOL's property list."
  (let* ((pp (%symbol-package-plist sym)))
    (if (consp pp)
      (let* ((bits (%symbol-bits sym)))
        (declare (fixnum bits))
        (if (logbitp $sym_vbit_typeppred bits)
          (cddr pp)
          (cdr pp))))))


(defun get (sym key &optional default)
  "Look on the property list of SYMBOL for the specified INDICATOR. If this
  is found, return the associated value, else return DEFAULT."
  (let* ((tail (%pl-search (symbol-plist sym) key)))
    (if tail (%cadr tail) default)))

(defun put (sym key value)
  (let* ((plist (symbol-plist sym))
         (tail (%pl-search plist key)))
    (if tail 
      (%rplaca (%cdr tail) value)
      (set-symbol-plist sym (cons key (cons value plist))))
    value))

; This is how things have traditionally been defined: if %sym_vbit_typeppred 
; is set in the symbol's %SYMBOL-BITS, it's assumed that it's safe to
; take the %CADR of the %SYMBOL-PACKAGE-PLIST.
(defun get-type-predicate (name)
  (let* ((bits (%symbol-bits name)))
    (declare (fixnum bits))
    (if (logbitp $sym_vbit_typeppred bits)
      (%cadr (%symbol-package-plist name)))))

(defun set-type-predicate (name function)
  (let* ((bits (%symbol-bits name))
         (spp (%symbol-package-plist name)))
    (declare (fixnum bits))
    (if (logbitp $sym_vbit_typeppred bits)
      (%rplaca (%cdr (%symbol-package-plist name)) function)
      (without-interrupts
        (%symbol-bits name (the fixnum (bitset $sym_vbit_typeppred bits)))
        (if (atom spp)
          (%set-symbol-package-plist name (cons spp (cons function nil)))
          (%rplacd spp (cons function (cdr spp))))))
    function))

(defun symbol-value (sym)
  "Return SYMBOL's current bound value."
  (let* ((val (%sym-value sym)))
    (if (eq val (%unbound-marker))
      (%kernel-restart $xvunbnd sym)
      val)))

(defun set (sym value)
  "Set SYMBOL's value cell to NEW-VALUE."
  (let* ((bits (%symbol-bits sym)))
    (declare (fixnum bits))
    (if (logbitp $sym_vbit_const bits)
      (%err-disp $XCONST sym)
      (%set-sym-value sym value))))

(defun constant-symbol-p (sym)
  (and (symbolp sym)
       (%ilogbitp $sym_vbit_const (%symbol-bits sym))))

; This leaves the SPECIAL and INDIRECT bits alone, clears the others.
(defun makunbound (sym)
  "Make SYMBOL unbound, removing any value it may currently have."
  (if (and *warn-if-redefine-kernel*
           (constant-symbol-p sym))
    (cerror "Make ~S be unbound anyway."
            "~S is a constant; making it unbound might be a bad idea." sym))
  (%symbol-bits sym (the fixnum (logand (logior #xff00 (ash 1 $sym_bit_special) (ash 1 $sym_vbit_bound))
                                        (the fixnum (%symbol-bits sym)))))
  (%set-sym-value sym (%unbound-marker))
  sym)

(defun non-nil-symbolp (x)
  "Returns symbol if true"
  (if (symbolp x) x))

(defun symbol-package (sym)
  "Return the package SYMBOL was interned in, or NIL if none."
  (let* ((pp (%symbol-package-plist sym)))
    (if (consp pp) (car pp) pp)))

(defun boundp (sym)
  "Return non-NIL if SYMBOL is bound to a value."
  (not (eq (%sym-value sym) (%unbound-marker))))

(defun make-symbol (name)
  "Make and return a new symbol with the NAME as its print name."
  (%gvector ppc32::subtag-symbol
                (ensure-simple-string name) ; pname
                (%unbound-marker)       ; value cell
                %unbound-function%      ; function cell
                nil                     ; package&plist
                0))                     ; flags

(defun %symbol-bits (sym &optional new)
  (let* ((p (%symbol->symptr sym))
         (bits (%svref p ppc32::symbol.flags-cell)))
    (if new
      (setf (%svref p ppc32::symbol.flags-cell) new))
    bits))

(defun %sym-value (name)
  (%svar-sym-value (%ensure-svar (%symbol->symptr name))))

(defun %set-sym-value (name val)
  (%svar-set-sym-value (%ensure-svar (%symbol->symptr name)) val))

(defun %sym-global-value (name)
  (%svref (%symbol->symptr name) ppc32::symbol.vcell-cell))

(defun %set-sym-global-value (name val)
  (setf (%svref (%symbol->symptr name) ppc32::symbol.vcell-cell) val))

(defun %symbol-package-plist (sym)
  (%svref (%symbol->symptr sym) ppc32::symbol.package-plist-cell))

(defun %set-symbol-package-plist (sym new)
  (setf (%svref (%symbol->symptr sym) ppc32::symbol.package-plist-cell) new))



(defun symbol-name (sym)
  "Return SYMBOL's name as a string."
  #+ppc32-target
  (%svref (%symbol->symptr sym) ppc32::symbol.pname-cell)
  #+ppc64-target
  (if sym                               ;NIL's pname is implicit
    (%svref (%symbol->symptr sym) ppc64::symbol.pname-cell)
    "NIL")
  )




(defun %global-macro-function (symbol)
  (let* ((fbinding (fboundp symbol)))
    (if (and (typep fbinding 'simple-vector)
             (= (the fixnum (uvsize fbinding)) 2))
      (let* ((fun (%svref fbinding 1)))
        (if (functionp fun) fun)))))

(defun %symbol-binding-address (sym)
  (%svar-binding-address (ensure-svar sym)))

(let* ((svar-lock (make-lock))
       (svar-hash (make-hash-table :test #'eq :weak t))
       (svar-idx-map (make-hash-table :test #'eq :weak :value))
       (svar-index 0))
  (defun %set-svar-hash (hash)
    (unless svar-hash
      (setq svar-hash hash)))
  (defun %find-svar (symptr)
    (gethash symptr svar-hash))
  (defun find-svar (sym)
    (%find-svar (%symbol->symptr sym)))
  (defun ensure-svar-idx (svar)
    (let* ((sym (%svref svar ppc32::svar.symbol-cell))
           (idx (%svref svar ppc32::svar.idx-cell))
           (bits (%symbol-bits sym)))
      (declare (fixnum idx bits))
      (if (or (logbitp $sym_vbit_global bits)
              (logbitp $sym_vbit_const bits))
        (unless (zerop idx)
	  (remhash idx svar-idx-map)
          (setf (%svref svar ppc32::svar.idx-cell) 0))
        (if (zerop idx)
	  (let* ((new-idx (incf svar-index)))
	    (setf (%svref svar ppc32::svar.idx-cell) new-idx)
	    (setf (gethash new-idx svar-idx-map) svar))))
      svar))
  (defun %ensure-svar (symptr)
    (with-lock-grabbed (svar-lock)
      (let* ((svar (or (%find-svar symptr)
                       (setf (gethash symptr svar-hash)
                             (gvector :svar symptr 0)))))
        (ensure-svar-idx svar))))
  (defun ensure-svar (sym)
    (%ensure-svar (%symbol->symptr sym)))
  (defun cold-load-svar (svar)
    (with-lock-grabbed (svar-lock)
      (let* ((symptr (%symbol->symptr (%svref svar ppc32::svar.symbol-cell))))
        (setf (gethash symptr svar-hash) svar)
        (setf (%svref svar ppc32::svar.idx-cell) 0)
        (ensure-svar-idx svar))))
  (defun svar-index ()
    svar-index)
  (defun index-svar (index)
    (gethash index svar-idx-map))
  )
