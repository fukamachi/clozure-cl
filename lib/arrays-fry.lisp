; -*- Mode:Lisp; Package:CCL; -*-
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

(defun bit (bit-array &rest subscripts)
  "Returns the bit from the Bit-Array at the specified Subscripts."
  (declare (dynamic-extent subscripts))
  (unless (eq (array-element-type bit-array) 'bit)
    (report-bad-arg bit-array '(array bit)))
  (apply #'aref bit-array subscripts))

(defun %bitset (bit-array &rest stuff)
  (declare (dynamic-extent stuff))
  (unless (eq (array-element-type bit-array) 'bit)
    (report-bad-arg bit-array '(array bit)))
  (apply #'aset bit-array stuff))

(defun sbit (v &optional sub0 &rest others)
  (declare (dynamic-extent others))
  (if (or (null sub0) others)
    (apply #'bit v sub0 others)
     ( sbit (require-type v 'simple-bit-vector) sub0)))

(defun %sbitset (v sub0 &optional (newval nil newval-p) &rest newval-was-really-sub1)
  (declare (dynamic-extent newval-was-really-sub1))
  (if newval-p
    (if newval-was-really-sub1
      (apply #'%bitset v sub0 newval newval-was-really-sub1)
      (progn
        (unless (typep v 'simple-bit-vector)
          (report-bad-arg v 'simple-bit-vector))
        (uvset v sub0 newval)))
    (%bitset v)))

(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical AND on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
   (bit-boole boole-and bit-array1 bit-array2 result-bit-array))

(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical IOR on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-boole  boole-ior bit-array1 bit-array2 result-bit-array))

(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical XOR on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
   (bit-boole  boole-xor bit-array1 bit-array2 result-bit-array))

(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical EQV  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
   (bit-boole boole-eqv bit-array1 bit-array2 result-bit-array))

(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical NAND  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-boole boole-nand bit-array1 bit-array2 result-bit-array))

(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical NOR  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-boole boole-nor bit-array1 bit-array2 result-bit-array))

(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ANDC1 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-boole boole-andc1 bit-array1 bit-array2 result-bit-array))

(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ANDC2 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-boole boole-andc2 bit-array1 bit-array2 result-bit-array))

(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ORC1 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-boole boole-orc1 bit-array1 bit-array2 result-bit-array))

(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ORC2 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-boole boole-orc2 bit-array1 bit-array2 result-bit-array))

(defun bit-not (bit-array &optional result-bit-array)
  "Performs a bit-wise logical NOT in the elements of the Bit-Array putting
  the results into the Result-Bit-Array."
  (bit-boole boole-nor bit-array bit-array result-bit-array))

(defun result-bit-array (bit-array-1 bit-array-2 result)
  ; Check that the two bit-array args are bit-arrays with
  ; compatible dimensions.  If "result" is specified as T,
  ; return bit-array-1.  If result is unspecified, return
  ; a new bit-array of the same dimensions as bit-array-2.
  ; Otherwise, make sure that result is a bit-array of the
  ; same dimensions as the other two arguments and return
  ; it.
  (let* ((typecode-1 (typecode bit-array-1))
         (typecode-2 (typecode bit-array-2)))
    (declare (fixnum typecode-1 typecode-2))
    (flet ((bit-array-dimensions (bit-array typecode)
             (declare (fixnum typecode))
             (if (= typecode arch::subtag-bit-vector)
               (uvsize bit-array)
               (let* ((array-p (= typecode arch::subtag-arrayH))
                      (vector-p (= typecode arch::subtag-vectorH)))
                 (if (and (or array-p vector-p) 
                          (= (the fixnum (%array-header-subtype bit-array)) arch::subtag-bit-vector))
                   (if vector-p
                     (length bit-array)
                     (array-dimensions bit-array))
                   (report-bad-arg bit-array '(array bit))))))
           (check-matching-dimensions (a1 d1 a2 d2)
             (unless (equal d1 d2)
               (error "%s and %s have different dimensions."  a1 a2))
             a2))
      (let* ((dims-1 (bit-array-dimensions bit-array-1 typecode-1))
             (dims-2 (bit-array-dimensions bit-array-2 typecode-2)))
        (check-matching-dimensions bit-array-1 dims-1 bit-array-2 dims-2)
        (if result
          (if (eq result t)
            bit-array-1
            (check-matching-dimensions bit-array-2 dims-2 result (bit-array-dimensions result (typecode result))))
          (make-array dims-2 :element-type 'bit :initial-element 0))))))

; If the bit-arrays are all simple-bit-vectorp, we can do the operations
; 32 bits at a time.  (other case have to worry about alignment/displacement.)
#+ppc-target
(defppclapfunction %simple-bit-boole ((op 0) (b1 arg_x) (b2 arg_y) (result arg_z))
  (la imm0 4 vsp)
  (save-lisp-context imm0)
  (vector-size imm4 result imm4)
  (srwi. imm3 imm4 5)
  (clrlwi imm4 imm4 27)
  (bl @get-dispatch)
  (cmpwi cr1 imm4 0)
  (mflr loc-pc)
  (lwz temp0 op vsp)
  (add loc-pc loc-pc temp0)
  (add loc-pc loc-pc temp0)
  (mtctr loc-pc)
  (li imm0 arch::misc-data-offset)
  (b @testw)
  @nextw
  (cmpwi cr0 imm3 1)
  (subi imm3 imm3 1)
  (lwzx imm1 b1 imm0)
  (lwzx imm2 b2 imm0)
  (bctrl)
  (stwx imm1 result imm0)
  (addi imm0 imm0 4)
  @testw
  (bne cr0 @nextw)
  (beq cr1 @done)
  ; Not sure if we need to make this much fuss about the partial word
  ; in this simple case, but what the hell.
  (lwzx imm1 b1 imm0)
  (lwzx imm2 b2 imm0)
  (bctrl)
  (lwzx imm2 result imm0)
  (slw imm2 imm2 imm4)
  (srw imm2 imm2 imm4)
  (subfic imm4 imm4 32)
  (srw imm1 imm1 imm4)
  (slw imm1 imm1 imm4)
  (or imm1 imm1 imm2)
  (stwx imm1 result imm0)
  @done
  (restore-full-lisp-context)
  (blr)

  @get-dispatch 
  (blrl)
  @disptach
  (li imm1 0)                           ; boole-clr
  (blr)
  (li imm1 -1)                          ; boole-set
  (blr)
  (blr)                                 ; boole-1
  (blr)                             
  (mr imm1 imm2)                        ; boole-2
  (blr)
  (not imm1 imm1)                       ; boole-c1
  (blr)
  (not imm1 imm2)                       ; boole-c2
  (blr)
  (and imm1 imm1 imm2)                  ; boole-and
  (blr)
  (or imm1 imm1 imm2)                   ; boole-ior
  (blr)
  (xor imm1 imm1 imm2)                  ; boole-xor
  (blr)
  (eqv imm1 imm1 imm2)                  ; boole-eqv
  (blr)
  (nand imm1 imm1 imm2)                 ; boole-nand
  (blr)
  (nor imm1 imm1 imm2)                  ; boole-nor
  (blr)
  (andc imm1 imm2 imm1)                 ; boole-andc1
  (blr)
  (andc imm1 imm1 imm2)                 ; boole-andc2
  (blr)
  (orc imm1 imm2 imm1)                  ; boole-orc1
  (blr)
  (orc imm1 imm1 imm2)                  ; boole-orc2
  (blr))

#+sparc-target
(defsparclapfunction %simple-bit-boole ((op 0) (b1 %arg_x) (b2 %arg_y) (result %arg_z))
  (add %vsp 4 %imm0)
  (save-lisp-context %imm0)
  (vector-size result %imm4 %imm4)
  (srl %imm4 5 %imm3)
  (call @get-dispatch)
   (and %imm4 (1- (ash 1 5)) %imm4)
  (ld (%vsp op) %temp0)
  (add %temp0 %ra1 %ra1)
  (add %temp0 %ra1 %ra1)
  (mov arch::misc-data-offset %imm0)
  (b @testw)
    (tst %imm3)
  @nextw
  (deccc %imm3)
  (ld (b1 %imm0) %imm1)
  (jmpl %ra1 8 %ra0)
    (ld (b2 %imm0) %imm2)
  (st %imm1 (result %imm0))
  (inc 4 %imm0)
  @testw
  (tst %imm3)
  (bne  @nextw)
    (tst %imm4)
  (be @done)
    (nop)
  ; Not sure if we need to make this much fuss about the partial word
  ; in this simple case, but what the hell.
  (ld (b1 %imm0) %imm1)
  (jmpl %ra1 8 %ra0)
    (ld (b2 %imm0) %imm2)
  (ld (result %imm0) %imm2)
  (sll %imm2 %imm4 %imm2)
  (srl %imm2 %imm4 %imm2)
  (mov 32 %imm3)
  (sub %imm3 %imm4 %imm4)
  (sll %imm1 %imm4 %imm1)
  (srl %imm1 %imm4 %imm1)
  (or %imm2 %imm1 %imm1)
  (st %imm1 (result %imm0))
  @done
  (restore-full-lisp-context)
  (retl)
    (nop)

  @get-dispatch 
  (jmpl %ra0 8 %ra1)
   (nop)
  @disptach
  (retl)                           ; boole-clr
   (mov 0 %imm1)
   
  (retl)                          ; boole-set
   (orn %rzero 0 %imm1)

  (retl)                                 ; boole-1
  (nop)                             

  (retl)				; boole-2
  (mov %imm2 %imm1)
  @boole-c1
  (retl)                       ; boole-c1
  (xnor %imm1 %rzero %imm1)
  
  (retl)                       ; boole-c2
  (xnor %imm2 %rzero %imm1)
  
  (retl)                  ; boole-and
  (and %imm2 %imm1 %imm1)
  
  (retl)                   ; boole-ior
  (or %imm2 %imm1 %imm1)
  
  (retl)                  ; boole-xor
  (xor %imm1 %imm2 %imm1)
  
  (retl)                  ; boole-eqv
  (xnor %imm1 %imm2 %imm1)
  
  (b @boole-c1)                 ; boole-nand
  (and %imm1 %imm2 %imm1)
  
  (b @boole-c1)                  ; boole-nor
  (or %imm1 %imm2 %imm1)
  
  (retl)                 ; boole-andc1
  (andn %imm2 %imm1 %imm1)
  
  (retl)                 ; boole-andc2
  (andn %imm1 %imm2 %imm1)
  
  (retl)                  ; boole-orc1
  (orn %imm2 %imm1 %imm1)
  
  (retl)                  ; boole-orc2
  (orn %imm1 %imm2 %imm1))
  
(defun bit-boole (opcode array1 array2 result-array)
  (unless (eql opcode (logand 15 opcode))
    (setq opcode (require-type opcode '(mod 16))))
  (let* ((result (result-bit-array array1 array2 result-array)))
    (if (and (typep array1 'simple-bit-vector)
             (typep array2 'simple-bit-vector)
             (typep result 'simple-bit-vector))
      (%simple-bit-boole opcode array1 array2 result)
      (multiple-value-bind (v1 i1) (array-data-and-offset array1)
        (declare (simple-bit-vector v1) (fixnum i1))
        (multiple-value-bind (v2 i2) (array-data-and-offset array2)
          (declare (simple-bit-vector v2) (fixnum i2))
          (multiple-value-bind (v3 i3) (array-data-and-offset result)
            (declare (simple-bit-vector v3) (fixnum i3))
            (let* ((e3 (+ i3 (the fixnum (array-total-size result)))))
              (declare (fixnum e3))
              (do* ( )
                   ((= i3 e3) result)
                (setf (sbit v3 i3) 
                      (logand (boole opcode (sbit v1 i1) (sbit v2 i2)) 1))
                (incf i1)
                (incf i2)
                (incf i3)))))))))


          
          




; shrink-vector is called only in sequences-2. None of the calls depend on
; the side affect of setting the passed-in symbol to the [possibly new]
; returned vector
; Since there hasn't been such a thing as sequences-2 in about 7 years,
; this is especially puzzling.
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro shrink-vector (vector to-size)
    `(setq ,vector (%shrink-vector ,vector ,to-size)))
  )


; new and faulty def
(defun %shrink-vector (vector to-size)
  (cond ((eq (length vector) to-size)
         vector)
        ((array-has-fill-pointer-p vector)
         (setf (fill-pointer vector) to-size)
         vector)
        (t (subseq vector 0 to-size))))



; this could be put into print-db as it was in ccl-pr-4.2
; Or it (and print-db) could just be flushed ... tough one.
(defun multi-dimension-array-to-list (array)
  "Produces a nested list of the elements in array."
  (mdal-aux array (array-dimensions array) nil 
            (array-dimensions array)))

(defun mdal-aux (array all-dimensions use-dimensions 
                       remaining-dimensions)
  (if (= (length all-dimensions) (length use-dimensions))
    (apply 'aref array use-dimensions)
    (do ((index 0 (1+ index))
         (d-length (car remaining-dimensions))
         (result nil))
        ((= d-length index) result)
      (setq result 
            (append result (list (mdal-aux array all-dimensions
                                           (append use-dimensions 
                                                   (list index))
                                           (cdr remaining-dimensions))))))))

(defun adjust-array (array dims
                     &key (element-type nil element-type-p)
                          (initial-element nil initial-element-p)
                          (initial-contents nil initial-contents-p)
                          (fill-pointer nil fill-pointer-p)
                          displaced-to
                          displaced-index-offset
                     &aux (subtype (array-element-subtype array)))
  (when (and element-type-p
             (neq (element-type-subtype element-type) subtype))
    (error "~S is not of element type ~S" array element-type))
  (when (integerp dims)(setq dims (list dims))) ; because %displace-array wants the list
  (if (neq (list-length dims)(array-rank array))
    (error "~S has wrong rank for adjusting to dimensions ~S" array dims))
  (let ((size 1))    
    (dolist (dim dims)
      (when (< dim 0)(report-bad-arg dims '(integer 0 *)))
      (setq size (* size dim)))
    (when (and (neq fill-pointer t)
               (array-has-fill-pointer-p array)
               (< size (or fill-pointer (fill-pointer array))))
      (error "Cannot adjust array ~S to size less than fill pointer ~S"
             array (or fill-pointer (fill-pointer array))))
    (when (and fill-pointer (not (array-has-fill-pointer-p array)))
        (error "~S does not have a fill pointer" array))
    (when (and displaced-index-offset (null displaced-to))
      (error "Cannot specify ~S without ~S" :displaced-index-offset :displaced-to))
    (when (and initial-element-p initial-contents-p)
        (error "Cannot specify both ~S and ~S" :initial-element :initial-contents))
    (cond 
     ((not (adjustable-array-p array))
      (let ((new-array (make-array-1  dims 
                                       (array-element-type array) T
                                       displaced-to
                                       displaced-index-offset
                                       nil
                                       fill-pointer
                                       initial-element initial-element-p
                                       initial-contents initial-contents-p
                                       size)))
                     
        (when (and (null initial-contents-p)
                   (null displaced-to))
          (multiple-value-bind (array-data offs) (array-data-and-offset array)
            (let ((new-array-data (array-data-and-offset new-array))) 
              (cond ((null dims)
                     (uvset new-array-data 0 (uvref array-data offs)))
                    (T
                   (init-array-data array-data offs (array-dimensions array) 
                                    new-array-data 0 dims))))))
        (setq array new-array)))
     (T (cond 
         (displaced-to
          (if (and displaced-index-offset 
                   (or (not (fixnump displaced-index-offset))
                       (< displaced-index-offset 0)))
            (report-bad-arg displaced-index-offset '(integer 0 #.most-positive-fixnum)))
          (when (or initial-element-p initial-contents-p)
            (error "Cannot specify initial values for displaced arrays"))
          (unless (eq subtype (array-element-subtype displaced-to))
            (error "~S is not of element type ~S"
                   displaced-to (array-element-type array)))
          (do* ((vec displaced-to (displaced-array-p vec)))
               ((null vec) ())
            (when (eq vec array)
              (error "Array cannot be displaced to itself."))))
         (T
          (setq displaced-to ( %alloc-misc size subtype))      
          (cond (initial-element-p
                 (dotimes (i (the fixnum size)) (uvset displaced-to i initial-element)))
                (initial-contents-p
                 (if (null dims) (uvset displaced-to 0 initial-contents)
                     (init-uvector-contents displaced-to 0 dims initial-contents))))
          (cond ((null dims)
                 (uvset displaced-to 0 (aref array)))
                ((not initial-contents-p)
                 (multiple-value-bind (vec offs) (array-data-and-offset array)
                   (init-array-data vec offs (array-dimensions array) displaced-to 0 dims))))))
        (%displace-array array dims size displaced-to (or displaced-index-offset 0))))
    (when fill-pointer-p
      (cond
        ((eq fill-pointer t)
         (set-fill-pointer array size))
        (fill-pointer
         (set-fill-pointer array fill-pointer))))
    array))

(defun array-dims-sizes (dims)
   (if (or (atom dims) (null (%cdr dims))) dims
     (let ((ndims (array-dims-sizes (%cdr dims))))
       (cons (* (%car dims) (%car ndims)) ndims))))

(defun init-array-data (vec off dims nvec noff ndims)
   (init-array-data-aux vec off dims (array-dims-sizes (cdr dims))
                        nvec noff ndims (array-dims-sizes (cdr ndims))))

(defun init-array-data-aux (vec off dims siz nvec noff ndims nsiz)
   (when (null siz)
      (return-from init-array-data-aux
         (init-vector-data vec off (car dims) nvec noff (car ndims))))
   (let ((count (pop dims))
         (size (pop siz))
         (ncount (pop ndims))
         (nsize (pop nsiz)))
     (dotimes (i (if (%i< count ncount) count ncount))
        (declare (fixnum i))
        (init-array-data-aux vec off dims siz nvec noff ndims nsiz)
        (setq off (%i+ off size) noff (%i+ noff nsize)))))

(defun init-vector-data (vec off len nvec noff nlen)
  (dotimes (i (if (%i< len nlen) len nlen))
     (declare (fixnum i))
     (uvset nvec noff (uvref vec off))
     (setq off (%i+ off 1) noff (%i+ noff 1))))

; only caller is adjust-array

(defun %displace-array (array dims size data offset)
  (let* ((typecode (typecode array))
         (array-p (eql typecode arch::subtag-arrayH))
         (vector-p (eql typecode arch::subtag-vectorH)))
    (unless (or array-p vector-p)
      (error "Array ~S cannot be displaced" array))
    (unless (fixnump offset) (report-bad-arg offset '(integer 0 #.most-positive-fixnum)))
    (unless (adjustable-array-p data)
      (multiple-value-bind (ndata noffset) (displaced-array-p data)
        (if ndata (setq data ndata offset (%i+ offset noffset)))))
    (unless (and (fixnump size) (%i<= (%i+ offset size) (array-total-size data)))
      (error "Offset ~S + size ~S must be less than size of array displaced-to" offset size))
    (let* ((flags (%svref array arch::vectorH.flags-cell)))
      (declare (fixnum flags))
      (setf (%svref array arch::vectorH.flags-cell)
            (if (> (the fixnum (typecode data)) arch::subtag-vectorH)
              (bitclr $arh_disp_bit flags)
              (bitset $arh_disp_bit flags)))
      (setf (%svref array arch::arrayH.data-vector-cell) data)
      (if array-p
        (progn
          (do ((i arch::arrayH.dim0-cell (1+ i)))
              ((null dims))
            (declare (fixnum i))
            (setf (%svref array i) (pop dims)))
          (setf (%svref array arch::arrayH.physsize-cell) size)
          (setf (%svref array arch::arrayH.displacement-cell) offset))
        (progn
          (if (or (not (logbitp $arh_fill_bit flags))
                  (> (the fixnum (%svref array arch::vectorH.logsize-cell)) size))
            (setf (%svref array arch::vectorH.logsize-cell) size))
          (setf (%svref array arch::vectorH.physsize-cell) size)
          (setf (%svref array arch::vectorH.displacement-cell) offset)))
      array)))



(defun array-row-major-index (array &lexpr subscripts)
  (let ((rank  (array-rank array))
        (nsubs (%lexpr-count subscripts))
        (sum 0))
    (declare (fixnum sum rank))
    (unless (eql rank nsubs)
      (%err-disp $xndims array nsubs))    
      (if (eql 0 rank)
        0
        (do* ((i (1- rank) (1- i))
              (dim (array-dimension array i) (array-dimension array i))
              (last-size 1 size)
              (size dim (* dim size)))
             (nil)
          (declare (fixnum i last-size size))
          (let ((s (%lexpr-ref subscripts nsubs i)))
            (unless (fixnump s)
              (setq s (require-type s 'fixnum)))
            (when (or (< s 0) (>= s dim))
              (%err-disp $XARROOB (%apply-lexpr 'list subscripts) array))
            (incf sum (the fixnum (* s last-size)))
            (when (eql i 0) (return sum)))))))

(defun array-in-bounds-p (array &lexpr subscripts)
  (let ((rank  (array-rank array))
        (nsubs (%lexpr-count subscripts)))
    (declare (fixnum sum rank))    
    (if (not (eql nsubs rank))
      (%err-disp $xndims array nsubs)
      (if (eql 0 rank)
        0
        (do* ((i (1- rank) (1- i))
              (dim (array-dimension array i) (array-dimension array i)))
             (nil)
          (declare (fixnum i))
          (let ((s  (%lexpr-ref subscripts nsubs i)))
            (require-type s 'fixnum)
            (if (or (< s 0)(>= s dim)) (return nil))
            (when (eql i 0) (return t))))))))

(defun row-major-aref (array index)
  (multiple-value-bind (displaced-to offset) (displaced-array-p array)
    (aref (or displaced-to array) (+ index offset))))

(defun row-major-aset (array index new)
  (multiple-value-bind (displaced-to offset) (displaced-array-p array)
    (setf (aref (or displaced-to array) (+ index offset)) new)))

(defsetf row-major-aref row-major-aset)
             


; end
