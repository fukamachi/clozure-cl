;;-*- Mode: Lisp; Package: CCL -*-
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

(eval-when (:compile-toplevel :execute)

(require "FASLENV" "ccl:xdump;faslenv")
#+ppc-target
(require "PPC-LAPMACROS")
#+sparc-target
(require "SPARC-LAPMACROS")


(defconstant $primsizes (make-array 23
                                    :element-type 'fixnum
                                    :initial-contents
                                    '(41 61 97 149 223 337 509 769 887 971 1153 1559 1733
                                      2609 2801 3917 5879 8819 13229 19843 24989 29789 32749)))
(defconstant $hprimes (make-array 8 
                                  :element-type '(unsigned-byte 16)
                                  :initial-contents '(5 7 11 13 17 19 23 29)))

; Symbol hash tables: (htvec . (hcount . hlimit))

(defmacro htvec (htab) `(%car ,htab))
(defmacro htcount (htab) `(%cadr ,htab))
(defmacro htlimit (htab) `(%cddr ,htab))
)

(eval-when (:execute :compile-toplevel)
  (assert (= 50 numfaslops)))

(defvar *fasl-dispatch-table* #50(%bad-fasl))

(defun %bad-fasl (s)
  (error "bad opcode in FASL file ~s" (faslstate.faslfname s)))

(defun %cant-epush (s)
  (if (faslstate.faslepush s)
    (%bad-fasl s)))

(defun %epushval (s val)
  (setf (faslstate.faslval s) val)
  (when (faslstate.faslepush s)
    (setf (svref (faslstate.faslevec s) (faslstate.faslecnt s)) val)
    (incf (the fixnum (faslstate.faslecnt s))))
  val)

(defun %simple-fasl-read-buffer (s)
  (let* ((fd (faslstate.faslfd s))
         (buffer (faslstate.iobuffer s))
         (bufptr (%get-ptr buffer)))
    (declare (dynamic-extent bufptr)
             (type macptr buffer bufptr pb))
    (%setf-macptr bufptr (%inc-ptr buffer 4))
    (setf (%get-ptr buffer) bufptr)
    (let* ((n (fd-read fd bufptr $fasl-buf-len)))
      (declare (fixnum n))
      (if (> n 0)
        (setf (faslstate.bufcount s) n)
        (error "Fix this: look at errno, EOF")))))

 
(defun %simple-fasl-read-byte (s)
  (loop
    (let* ((buffer (faslstate.iobuffer s))
           (bufptr (%get-ptr buffer)))
      (declare (dynamic-extent bufptr)
               (type macptr buffer bufptr))
      (if (>= (the fixnum (decf (the fixnum (faslstate.bufcount s))))
              0)
        (return
         (prog1
           (%get-unsigned-byte bufptr)
           (setf (%get-ptr buffer)
                 (%incf-ptr bufptr))))
        (%fasl-read-buffer s)))))

(defun %fasl-read-word (s)
  (the fixnum 
    (logior (the fixnum (ash (the fixnum (%fasl-read-byte s)) 8))
            (the fixnum (%fasl-read-byte s)))))



; This does something much like what COMPOSE-DIGIT does (in the PPC/CMU-bignum
; code), only we HOPE that compose-digit disappears REAL SOON
#+ppc-target
(progn
  (defppclapfunction %compose-unsigned-fullword ((high arg_y) (low arg_z))
    (rlwinm imm0 low (- 32 arch::fixnumshift) 16 31)
    (rlwimi imm0 high (- 16 arch::fixnumshift) 0 15)
    ; Now have an unsigned fullword in imm0.  Box it.
    (clrrwi. imm1 imm0 (- arch::least-significant-bit arch::nfixnumtagbits))
    (box-fixnum arg_z imm0)             ; assume no high bits set.
    (beqlr+)
    (ba .SPmakeu32))

  
  (defppclapfunction %compose-signed-fixnum ((high arg_y) (low arg_z))
    (rlwinm imm0 low (- 32 arch::fixnumshift) 16 31)
    (rlwimi imm0 high (- 16 arch::fixnumshift) 0 15)
    ; Now have an unsigned fullword in imm0.  Box it.
    (box-fixnum arg_z imm0)
    (blr))
)

#+sparc-target
(progn
  (defsparclapfunction %compose-unsigned-fullword ((%high %arg_y) (%low %arg_z))
    (set #xffff %imm1)
    (unbox-fixnum %low %imm0)
    (and %imm1 %imm0 %imm0)
    (sll %high (- 16 arch::fixnumshift) %imm1)
    (or %imm1 %imm0 %imm0)
    (box-unsigned-byte-32 %imm0 %imm1 %arg_z)
    (retl)
     (nop))

  (defsparclapfunction %compose-signed-fixnum ((%high %arg_y) (%low %arg_z))
    (set #xffff %imm1)
    (unbox-fixnum %low %imm0)
    (and %imm1 %imm0 %imm0)
    (sll %high (- 16 arch::fixnumshift) %imm1)
    (or %imm1 %imm0 %imm0)
    (retl)
     (box-fixnum %imm0 %arg_z))
)

(defun %fasl-read-long (s)
  (%compose-unsigned-fullword (%fasl-read-word s) (%fasl-read-word s)))



(defun %fasl-read-size (s)
  (let* ((size (%fasl-read-byte s)))
    (declare (integer size))
    (when (= size #xFF)
      (setq size (%fasl-read-word s))
      (if (= size #xFFFF)
        (setq size (%fasl-read-long s))))
    size))

(defun %simple-fasl-read-n-bytes (s ivector byte-offset n)
  (declare (fixnum byte-offset n))
  (do* ()
       ((= n 0))
    (let* ((count (faslstate.bufcount s))
           (buffer (faslstate.iobuffer s))
           (bufptr (%get-ptr buffer))
           (nthere (if (< count n) count n)))
      (declare (dynamic-extent bufptr)
               (type macptr buffer bufptr)
               (fixnum count nthere))
      (if (= nthere 0)
        (%fasl-read-buffer s)
        (progn
          (decf n nthere)
          (decf (the fixnum (faslstate.bufcount s)) nthere)
          (%copy-ptr-to-ivector bufptr 0 ivector byte-offset nthere)
          (incf byte-offset nthere)
          (setf (%get-ptr buffer)
                (%incf-ptr bufptr nthere)))))))
        

(defun %fasl-readstr (s &optional ignore)
  (declare (fixnum subtype) (ignore ignore))
  (let* ((nbytes (%fasl-read-size s))
         (copy t)
         (n nbytes)
         (str (faslstate.faslstr s)))
    (declare (fixnum n nbytes))
    (if (> n (length str))
        (setq str (make-string n :element-type 'base-char))
        (setq copy nil))
    (%fasl-read-n-bytes s str 0 nbytes)
    (values str n copy)))

(defun %fasl-copystr (str len)
  ; IS THIS OK?
  (declare (fixnum len))
  (let* ((new (make-string len :element-type 'base-char)))
    (declare (simple-base-string new))
    (declare (optimize (speed 3)(safety 0)))
    (dotimes (i len new)
      (setf (schar new i) (schar str i)))))

(defun %fasl-dispatch (s op)
  (declare (fixnum op))
  (setf (faslstate.faslepush s) (logbitp $fasl-epush-bit op))
  ;(format t "~& dispatch: op = ~d" (logand op (lognot (ash 1 $fasl-epush-bit))))
  (funcall (svref (faslstate.fasldispatch s) (logand op (lognot (ash 1 $fasl-epush-bit)))) 
           s))

(defun %fasl-expr (s)
  (%fasl-dispatch s (%fasl-read-byte s))
  (faslstate.faslval s))

(defun %fasl-expr-preserve-epush (s)
  (let* ((epush (faslstate.faslepush s))
         (val (%fasl-expr s)))
    (setf (faslstate.faslepush s) epush)
    val))

(defun %fasl-make-symbol (s ignore)
  (declare (fixnum subtype) (ignore ignore))
  (let* ((n (%fasl-read-size s))
         (str (make-string n :element-type 'base-char)))
    (declare (fixnum n))
    (%fasl-read-n-bytes s str 0 n)
    (%epushval s (make-symbol str))))

(defun %fasl-intern (s package ignore)
  (declare (ignore ignore))
  (multiple-value-bind (str len new-p) (%fasl-readstr s)
    (with-package-lock (package)
     (multiple-value-bind (symbol access internal-offset external-offset)
                          (%find-symbol str len package)
       (unless access
         (unless new-p (setq str (%fasl-copystr str len)))
         (setq symbol (%add-symbol str package internal-offset external-offset)))
       (%epushval s symbol)))))

(defun %find-pkg (name &optional (len (length name)))
  (declare (fixnum len)
           (optimize (speed 3) (safety 0)))
  (with-package-list-read-lock
      (dolist (p %all-packages%)
        (if (dolist (pkgname (pkg.names p))
              (when (and (= (length pkgname) len)
                         (dotimes (i len t)
                           ;; Aref: allow non-simple strings
                           (unless (eq (aref name i) (schar pkgname i))
                             (return))))
                (return t)))
          (return p)))))



(defun pkg-arg (thing &optional deleted-ok)
  (let* ((xthing (if (or (symbolp thing)
                         (typep thing 'character))
                   (string thing)
                   thing)))
    (let* ((typecode (typecode xthing)))
        (declare (fixnum typecode))
        (cond ((= typecode arch::subtag-package)
               (if (or deleted-ok (pkg.names xthing))
                 xthing
                 (error "~S is a deleted package ." thing)))
              ((or (= typecode arch::subtag-simple-base-string)
                   (= typecode arch::subtag-simple-general-string))
               (or (%find-pkg xthing)
                   (%kernel-restart $xnopkg xthing)))
              (t (report-bad-arg thing 'simple-string))))))

(defun %fasl-package (s &optional ignore)
  (declare (ignore ignore))
  (multiple-value-bind (str len new-p) (%fasl-readstr s)
    (let* ((p (%find-pkg str len)))
      (%epushval s (or p (%kernel-restart $XNOPKG (if new-p str (%fasl-copystr str len))))))))

(defun %fasl-listX (s dotp)
  (let* ((len (%fasl-read-word s)))
    (declare (fixnum len))
    (when (= len 0)
      (setq len (%fasl-read-long s)))
    (let* ((val (%epushval s (cons nil nil)))
           (tail val))
      (declare (type cons val tail))
      (setf (car val) (%fasl-expr s))
      (dotimes (i len)
          (setf (cdr tail) (setq tail (cons (%fasl-expr s) nil))))
      (if dotp
        (setf (cdr tail) (%fasl-expr s)))
      (setf (faslstate.faslval s) val))))



(deffaslop $fasl-noop (s)
  (%cant-epush s))

(deffaslop $fasl-etab-alloc (s)
  (%cant-epush s)
  (setf (faslstate.faslevec s) (make-array (the fixnum (%fasl-read-long s)))
        (faslstate.faslecnt s) 0))

(deffaslop $fasl-arch (s)
  (%cant-epush s)
  (let* ((arch (%fasl-expr s)))
    (declare (fixnum arch))
    #+linuxppc-target
    (unless (= arch 1) (error "Not a LinuxPPC fasl file : ~s" (faslstate.faslfname s)))
    #+sparc-target
    (unless (= arch 16) (error "Not a SPARC fasl file : ~s" (faslstate.faslfname s)))
    #+darwinppc-target
    (unless (= arch 3) (error "Not a Darwin PPC fasl file : ~s" (faslstate.faslfname s)))
    ))

(deffaslop $fasl-eref (s)
  (let* ((idx (%fasl-read-word s)))     ; 16 bit limit ? why ?
    (declare (fixnum idx))
    (if (>= idx (the fixnum (faslstate.faslecnt s)))
      (%bad-fasl s))
    (%epushval s (svref (faslstate.faslevec s) idx))))

(deffaslop $fasl-lfuncall (s)
  (let* ((fun (%fasl-expr-preserve-epush s)))
    ;(break "fun = ~s" fun)
     (%epushval s (funcall fun))))

(deffaslop $fasl-globals (s)
  (setf (faslstate.faslgsymbols s) (%fasl-expr s)))

(deffaslop $fasl-char (s)
  (%epushval s (code-char (%fasl-read-byte s))))



(deffaslop $fasl-fixnum (s)
  (%epushval
   s
    (%compose-signed-fixnum (%fasl-read-word s) (%fasl-read-word s))))




(deffaslop $fasl-float (s)
  ;; A double-float is a 3-element "misc" object.
  ;; Element 0 is always 0 and exists solely to keep elements 1 and 2
  ;; aligned on a 64-bit boundary.
  (let* ((df (%alloc-misc arch::double-float.element-count
                          arch::subtag-double-float)))
    (setf (%misc-ref df arch::double-float.value-cell)
          (%fasl-read-long s))
    (setf (%misc-ref df arch::double-float.val-low-cell)
          (%fasl-read-long s))
    (%epushval s df)))

(deffaslop $fasl-str (s)
  (let* ((n (%fasl-read-size s))
         (str (make-string (the fixnum n) :element-type 'base-char)))
    (%epushval s str)
    (%fasl-read-n-bytes s str 0 n)))

(deffaslop $fasl-word-fixnum (s)
  (%epushval s (%word-to-int (%fasl-read-word s))))

(deffaslop $fasl-mksym (s)
  (%fasl-make-symbol s nil))

(deffaslop $fasl-intern (s)
  (%fasl-intern s *package* nil))

(deffaslop $fasl-pkg-intern (s)
  (let* ((pkg (%fasl-expr-preserve-epush s)))
    #+paranoia
    (setq pkg (pkg-arg pkg))
    (%fasl-intern s pkg nil)))

(deffaslop $fasl-pkg (s)
  (%fasl-package s nil))

(deffaslop $fasl-cons (s)
  (let* ((cons (%epushval s (cons nil nil))))
    (declare (type cons cons))
    (setf (car cons) (%fasl-expr s)
          (cdr cons) (%fasl-expr s))
    (setf (faslstate.faslval s) cons)))

(deffaslop $fasl-list (s)
  (%fasl-listX s nil))

(deffaslop $fasl-list* (s)
  (%fasl-listX s t))

(deffaslop $fasl-nil (s)
  (%epushval s nil))



(deffaslop $fasl-timm (s)
  (rlet ((p :long))
    (setf (%get-long p) (%fasl-read-long s))
    (%epushval s (%get-unboxed-ptr p))))




(deffaslop $fasl-symfn (s)
  (%epushval s (%function (%fasl-expr-preserve-epush s))))
    
(deffaslop $fasl-eval (s)
  (%epushval s (eval (%fasl-expr-preserve-epush s))))

; For bootstrapping. The real version is cheap-eval in l1-readloop
(when (not (fboundp 'eval))
  (defun eval (form)
    (if (and (listp form)
             (let ((f (%car form)))
               (and (symbolp f)
                    (functionp (fboundp f)))))
      (apply (%car form) (%cdr form))
      (error "Can't eval yet: ~s" form))))


(deffaslop $fasl-ivec (s)
  (let* ((subtag (%fasl-read-byte s))
         (element-count (%fasl-read-size s))
         (size-in-bytes (subtag-bytes subtag element-count))
         (vector (%alloc-misc element-count subtag)))
    (declare (fixnum subtag element-count size-in-bytes))
    (%epushval s vector)
    (%fasl-read-n-bytes s vector 0 size-in-bytes)
    (when (= subtag arch::subtag-code-vector)
      (%make-code-executable vector))
    vector))


(deffaslop $fasl-gvec (s)
  (let* ((subtype (%fasl-read-byte s))
         (n (%fasl-read-size s))
         (vector (%alloc-misc n subtype)))
    (declare (fixnum n))
    (%epushval s vector)
    (dotimes (i n (setf (faslstate.faslval s) vector))
      (setf (%svref vector i) (%fasl-expr s)))))




          
(deffaslop $fasl-xchar (s)
  (%epushval s (code-char (%fasl-read-word s))))
    
(deffaslop $fasl-mkxsym (s)
  (%fasl-make-symbol s t))

(deffaslop $fasl-defun (s)
  (%cant-epush s)
  (%defun (%fasl-expr s) (%fasl-expr s)))

(deffaslop $fasl-macro (s)
  (%cant-epush s)
  (%macro (%fasl-expr s) (%fasl-expr s)))

(deffaslop $fasl-defconstant (s)
  (%cant-epush s)
  (%defconstant (%fasl-expr s) (%fasl-expr s) (%fasl-expr s)))

(deffaslop $fasl-defparameter (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s)))
    (%defvar sym (%fasl-expr s))
    (set sym val)))

; (defvar var)
(deffaslop $fasl-defvar (s)
  (%cant-epush s)
  (%defvar (%fasl-expr s)))

; (defvar var initfom doc)
(deffaslop $fasl-defvar-init (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s)))
    (unless (%defvar sym (%fasl-expr s))
      (set sym val))))

(deffaslop $fasl-skip (s)
  (%fasl-expr s)
  (%fasl-expr s))

(deffaslop $fasl-prog1 (s)
  (let* ((val (%fasl-expr s)))
    (%fasl-expr s)
    (setf (faslstate.faslval s) val)))

(deffaslop $fasl-xintern (s)
  (%fasl-intern s *package* t))

(deffaslop $fasl-pkg-xintern (s)
  (let* ((pkg (%fasl-expr-preserve-epush s)))
    #+paranoia
    (setq pkg (pkg-arg pkg))
    (%fasl-intern s pkg t)))

(deffaslop $fasl-xpkg (s)
  (%fasl-package s t))

(deffaslop $fasl-src (s)
  (%cant-epush s)
  (let* ((source-file (%fasl-expr s)))
    ; (format t "~& source-file = ~s" source-file)
    (setq *loading-file-source-file* source-file)))

#-openmcl
(deffaslop $fasl-library-pointer (s)
  (setf (faslstate.faslval s)
        (pfsl-shared-library-offset s)))

(defvar *modules* nil)

; Bootstrapping version
(defun provide (module-name)
  (push module-name *modules*))

(deffaslop $fasl-provide (s)
  (provide (%fasl-expr s)))    


;;; The loader itself

(defun %simple-fasl-set-file-pos (s new)
  (let* ((fd (faslstate.faslfd s))
         (posoffset (fd-tell fd)))
    (if (>= (decf posoffset new) 0)
      (let* ((count (faslstate.bufcount s)))
        (if (>= (decf count posoffset ) 0)
          (progn
            (setf (faslstate.bufcount s) posoffset)
            (incf (%get-long (faslstate.iobuffer s)) count)
            (return-from %simple-fasl-set-file-pos nil)))))
    (progn
      (setf (faslstate.bufcount s) 0)
      (fd-lseek fd new #$SEEK_SET))))

(defun %simple-fasl-get-file-pos (s)
  (- (fd-tell (faslstate.faslfd s)) (faslstate.bufcount s)))

(defparameter *%fasload-verbose* t)

;; the default fasl file opener sets up the fasl state and checks the header
(defun %simple-fasl-open (string s)
  (let* ((ok nil)
	 (fd (fd-open string #$O_RDONLY))
	 (err 0))
    (declare (fixnum fd))
    (if (>= fd 0)
	(if (< (fd-lseek fd 0 #$SEEK_END) 4)
	    (setq err $xnotfasl)
	  (progn
	    (setq err 0)
	    (setf (faslstate.bufcount s) 0
		  (faslstate.faslfd s) fd)
	    (fd-lseek fd 0 #$SEEK_SET)
	    (multiple-value-setq (ok err) (%fasl-check-header s))))
      (setq err fd))
    (unless (eql err 0) (setf (faslstate.faslerr s) err))
    ok))

;; once the fasl state is set up, this checks the fasl header
;; and returns (values ok err)
(defun %fasl-check-header (s)
  (let* ((signature (%fasl-read-word s)))
    (declare (fixnum signature))
    (if (= signature $fasl-file-id)
	(values t 0)
      (if (= signature $fasl-file-id1)
	  (progn
	    (%fasl-set-file-pos s (%fasl-read-long s))
	    (values t 0))
	(values nil $xnotfasl)))))

(defun %simple-fasl-close (s)
  (let* ((fd (faslstate.faslfd s)))
    (when fd (fd-close fd))))

(defun %simple-fasl-init-buffer (s)
  (declare (ignore s))
  nil)

(defvar *fasl-api* nil)
(setf *fasl-api* (%istruct 'faslapi
			   #'%simple-fasl-open
			   #'%simple-fasl-close
			   #'%simple-fasl-init-buffer
			   #'%simple-fasl-set-file-pos
			   #'%simple-fasl-get-file-pos
			   #'%simple-fasl-read-buffer
			   #'%simple-fasl-read-byte
			   #'%simple-fasl-read-n-bytes))

(defun %fasl-open (string s)
  (funcall (faslapi.fasl-open *fasl-api*) string s))
(defun %fasl-close (s)
  (funcall (faslapi.fasl-close *fasl-api*) s))
(defun %fasl-init-buffer (s)
  (funcall (faslapi.fasl-init-buffer *fasl-api*) s))
(defun %fasl-set-file-pos (s new)
  (funcall (faslapi.fasl-set-file-pos *fasl-api*) s new))
(defun %fasl-get-file-pos (s)
  (funcall (faslapi.fasl-get-file-pos *fasl-api*) s))
(defun %fasl-read-buffer (s)
  (funcall (faslapi.fasl-read-buffer *fasl-api*) s))
(defun %fasl-read-byte (s)
  (funcall (faslapi.fasl-read-byte *fasl-api*) s))
(defun %fasl-read-n-bytes (s ivector byte-offset n)
  (funcall (faslapi.fasl-read-n-bytes *fasl-api*) s ivector byte-offset n))

(defun %fasload (string &optional (table *fasl-dispatch-table*)
                        start-faslops-function
                        stop-faslops-function)
  ;(dbg string) 
  (when (and *%fasload-verbose*
	     (not *load-verbose*))
    (%string-to-stderr ";Loading ") (pdbg string))
  (let* ((s (%istruct
             'faslstate
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil)))
    (declare (dynamic-extent s))
    (setf (faslstate.faslfname s) string)
    (setf (faslstate.fasldispatch s) table)
    (setf (faslstate.faslversion s) 0)
    (%stack-block ((buffer (+ 4 $fasl-buf-len)))
      (setf (faslstate.iobuffer s) buffer)
      (%fasl-init-buffer s)
      (let* ((parse-string (make-string 255 :element-type 'base-char)))
        (declare (dynamic-extent parse-string))
        (setf (faslstate.oldfaslstr s) nil
              (faslstate.faslstr s) parse-string)
	(unwind-protect
          (when (%fasl-open string s)
	    (let* ((nblocks (%fasl-read-word s))
		   (*pfsl-library-base* nil)
		   (*pfsl-library* nil))
	      (declare (fixnum nblocks))
	      (declare (special *pfsl-library-base* *pfsl-library*))
	      (unless (= nblocks 0)
		(let* ((pos (%fasl-get-file-pos s)))
		  (dotimes (i nblocks)
		    (%fasl-set-file-pos s pos)
		    (%fasl-set-file-pos s (%fasl-read-long s))
		    (incf pos 8)
		    (when start-faslops-function (funcall start-faslops-function s))
		    (let* ((version (%fasl-read-word s)))
		      (declare (fixnum version))
		      (if (or (> version (+ #xff00 $fasl-vers))
			      (< version (+ #xff00 $fasl-min-vers)))
                          (%err-disp (if (>= version #xff00) $xfaslvers $xnotfasl))
			(progn
			  (setf (faslstate.faslversion s) version)
			  (%fasl-read-word s) 
			  (%fasl-read-word s)       ; Ignore kernel version stuff
			  (setf (faslstate.faslevec s) nil
				(faslstate.faslecnt s) 0)
			  (do* ((op (%fasl-read-byte s) (%fasl-read-byte s)))
			       ((= op $faslend))
			    (declare (fixnum op))
			    (%fasl-dispatch s op))
			  (when stop-faslops-function (funcall stop-faslops-function s))
			  ))))))))
	  (%fasl-close s))
	(let* ((err (faslstate.faslerr s)))
	  (if err
	      (progn
		(when *%fasload-verbose*
		  (let* ((herald ";!!Error loading ")
			 (hlen (length herald))
			 (len (length string))
			 (msg (make-string (+ hlen len))))
		    (declare (dynamic-extent msg))
		    (%copy-ivector-to-ivector herald 0 msg 0 hlen)
		    (%copy-ivector-to-ivector string 0 msg hlen len)
		    (bug msg))
		  (values nil err)))
	    (values t nil)))))))


(defun %new-package-hashtable (size)
  (%initialize-htab (cons nil (cons 0 0)) size))

(defun %initialize-htab (htab size)
  (declare (fixnum size))
  ; Ensure that "size" is relatively prime to all secondary hash values.
  ; If it's small enough, pick the next highest known prime out of the
  ; "primsizes" array.  Otherwize, iterate through all all of "hprimes"
  ; until we find something relatively prime to all of them.
  (setq size
        (if (> size 32749)
          (do* ((nextsize (logior 1 size) (+ nextsize 2)))
               ()
            (declare (fixnum nextsize))
            (when (dotimes (i 8 t)
                    (unless (eql 1 (gcd nextsize (uvref #.$hprimes i)))
                      (return)))
              (return nextsize)))
          (dotimes (i (the fixnum (length #.$primsizes)))
            (let* ((psize (uvref #.$primsizes i)))
              (declare (fixnum psize))
              (if (>= psize size) 
                (return psize))))))
  (setf (htvec htab) (make-array size :initial-element nil))
  (setf (htcount htab) 0)
  (setf (htlimit htab) (the fixnum (- size (the fixnum (ash size -3)))))
  htab)

(defun %resize-htab (htab)
  (declare (optimize (speed 3) (safety 0)))
  (without-interrupts
   (let* ((old-vector (htvec htab))
          (old-len (length old-vector)))
     (declare (fixnum old-len)
              (simple-vector old-vector))
     (let* ((nsyms 0))
       (declare (fixnum nsyms))
       (dovector (s old-vector)
         (and s (symbolp s) (incf nsyms)))
       (%initialize-htab htab 
                         (the fixnum (+ 
                                      (the fixnum 
                                        (+ nsyms (the fixnum (ash nsyms -2))))
                                      2)))
       (let* ((new-vector (htvec htab))
              (nnew 0))
         (declare (fixnum nnew)
                  (simple-vector new-vector))
         (dotimes (i old-len (setf (htcount htab) nnew))
           (let* ((s (svref old-vector i)))
             (when s
               (setf (svref old-vector i) nil)       ; in case old-vector was static
               (if (symbolp s)
                 (let* ((pname (symbol-name s)))
                   (setf (svref 
                          new-vector 
                          (nth-value 
                           2
                           (%get-htab-symbol 
                            pname
                            (length pname)
                            htab)))
                         s)
                   (incf nnew))))))
         htab)))))

#+ppc-target
(defppclapfunction %pname-hash ((str arg_y) (len arg_z))
  (let ((nextw imm1)
        (accum imm0)
        (offset imm2)
        (tag imm3))
    (extract-subtag tag str)
    (cmpwi cr0 len 0)
    (li offset arch::misc-data-offset)
    (li accum 0)
    (beqlr- cr0)    
    @loop8
    (cmpwi cr1 len '1)
    (subi len len '1)
    (lbzx nextw str offset)
    (addi offset offset 1)
    (rotlwi accum accum 5)
    (xor accum accum nextw)
    (bne cr1 @loop8)
    (slwi accum accum 5)
    (srwi arg_z accum (- 5 arch::fixnumshift))
    (blr)))

#+sparc-target
(defsparclapfunction %pname-hash ((%str %arg_y) (%len %arg_z))
  (let ((%nextw %imm1)
        (%accum %imm0)
        (%offset %imm2)
	(%tag %imm3)
        (%accum1 %imm4))
    (tst %len)
    (mov arch::misc-data-offset %offset)
    (be @done)
     (mov 0 %accum)
    @loop8
    (subcc %len '1 %len)
    (ldub (%str %offset) %nextw)
    (add %offset 1 %offset)
    (srl %accum 27 %accum1)
    (sll %accum 5 %accum)
    (or %accum %accum1 %accum)
    (bne @loop8)
     (xor %accum %nextw %accum)
    (sll %accum 5 %accum)
    @done
    (retl)
     (srl %accum (- 5 arch::fixnumshift) %arg_z)))

        
(defun hash-pname (str len)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((primary (%pname-hash str len)))
    (declare (fixnum primary))
    (values primary (aref (the (simple-array (unsigned-byte 16) (8)) $hprimes) (logand primary 7)))))


    


(defun %get-hashed-htab-symbol (str len htab primary secondary)
  (declare (optimize (speed 3) (safety 0))
           (fixnum primary secondary len))
  (let* ((vec (htvec htab))
         (vlen (length vec)))
    (declare (fixnum vlen))
    (do* ((idx (fast-mod primary vlen) (+ i secondary))
          (i idx (if (>= idx vlen) (- idx vlen) idx))
          (elt (svref vec i) (svref vec i)))
         ((null elt) (values nil nil i))
      (declare (fixnum i idx))
      (when (and elt (symbolp elt))
        (let* ((pname (symbol-name elt)))
          (if (and 
               (= (the fixnum (length pname)) len)
               (dotimes (j len t)
                 (unless (eq (aref str j) (schar pname j))
                   (return))))
            (return (values t (%symptr->symbol elt) i))))))))

(defun %get-htab-symbol (string len htab)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (p s) (hash-pname string len)
    (%get-hashed-htab-symbol string len htab p s)))

(defun %find-symbol (string len package)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (found-p sym internal-offset)
                       (%get-htab-symbol string len (pkg.itab package))
    (if found-p
      (values sym :internal internal-offset nil)
      (multiple-value-bind (found-p sym external-offset)
                           (%get-htab-symbol string len (pkg.etab package))
        (if found-p
          (values sym :external internal-offset external-offset)
          (dolist (p (pkg.used package) (values nil nil internal-offset external-offset))
            (multiple-value-bind (found-p sym)
                                 (%get-htab-symbol string len (pkg.etab p))
              (when found-p
                (return (values sym :inherited internal-offset external-offset))))))))))
          
(defun %htab-add-symbol (symbol htab idx)
  (declare (optimize (speed 3) (safety 0)))
  (setf (svref (htvec htab) idx) (%symbol->symptr symbol))
  (if (>= (incf (the fixnum (htcount htab)))
          (the fixnum (htlimit htab)))
    (%resize-htab htab))
  symbol)

(defun %set-symbol-package (symbol package-or-nil)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((old-pp (%symbol-package-plist symbol)))
    (if (consp old-pp)
      (setf (car old-pp) package-or-nil)
      (%set-symbol-package-plist symbol package-or-nil))))

(defun %insert-symbol (symbol package internal-idx external-idx &optional force-export)
  (let* ((package-plist (%symbol-package-plist symbol))
         (keyword-package (eq package *keyword-package*)))
    ; Set home package
    (if package-plist
      (if (listp package-plist)
        (unless (%car package-plist) (%rplaca package-plist package)))
      (%set-symbol-package-plist symbol package))
    (if (or force-export keyword-package)
      (progn
        (%htab-add-symbol symbol (pkg.etab package) external-idx)
        (if keyword-package
          ;(define-constant symbol symbol)
          (progn
            (%set-sym-value symbol symbol)
            (%symbol-bits symbol 
                          (logior (ash 1 $sym_vbit_special) 
                                  (ash 1 $sym_vbit_const)
                                  (the fixnum (%symbol-bits symbol)))))))
      (%htab-add-symbol symbol (pkg.itab package) internal-idx))
    symbol))

; PNAME must be a simple string!
(defun %add-symbol (pname package internal-idx external-idx &optional force-export)
  (let* ((sym (make-symbol pname)))
    (%insert-symbol sym package internal-idx external-idx force-export)))


; The initial %toplevel-function% sets %toplevel-function% to NIL;
; if the %fasload call fails, the lisp should exit (instead of repeating
; the process endlessly ...



(defvar %toplevel-function%
  #'(lambda ()
      (declare (special *xload-cold-load-functions*
                        *xload-startup-file*))
      (%set-tcr-toplevel-function (%current-tcr) nil)   ; should get reset by l1-boot.
     (dolist (f (prog1 *xload-cold-load-functions* (setq *xload-cold-load-functions* nil)))
        (funcall f))
      (%fasload *xload-startup-file*)))

