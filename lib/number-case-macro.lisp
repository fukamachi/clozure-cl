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


(in-package :ccl)



;;;;;;;;;
;; support fns and vars for number-case

(defvar *type-to-typecode* `((fixnum . ,ppc32::tag-fixnum)(bignum . ,ppc32::subtag-bignum)
                           (double-float . ,ppc32::subtag-double-float) 
                           (short-float . ,ppc32::subtag-single-float)
			   (single-float . ,ppc32::subtag-single-float)
                           (ratio . ,ppc32::subtag-ratio)(complex . ,ppc32::subtag-complex)))

(defun type-name-to-code (name)
  (let ((res (cdr (assq name *type-to-typecode*))))
    (when (not res) (error "illegal numeric type name ~s" name))
    res))

(defvar nd-onions `((integer fixnum bignum) (rational fixnum bignum ratio)
                    (float double-float short-float)
                    (real fixnum bignum ratio double-float short-float)
                    (number fixnum bignum ratio double-float short-float complex)))

(defun nd-diff (x y) ; things in x that are not in y
  (let ((res))
    (dolist (e x)
      (when (not (memq e y))(push e res)))
    res))

(defun nd-type-compose (selectors)
  ; this could do better but probably not worth the trouble - only for require-type error
  (or (dolist (union nd-onions)
        (if (when (eq (length selectors)(length (cdr union)))
              (dolist (e selectors t)(if (not (memq e (cdr union)))(return))))
          (return (car union))))
      (cons 'or selectors)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simpler number dispatch. Syntax is just like case.
;;
;; (number-case x                 =>         (case (typecode x)
;;     (fixnum (print 4))		        (ppc32::tag-fixnum (print 4)) ; actually tag value
;;     ((bignum ratio)(print 5)))		((ppc32::tag-bignum ppc32::tag-ratio)(print 5))
;;	                      			(t (require-type x 'rational)))) 
;;						  

(defmacro number-case (var &rest cases)
  (let ((selectors-so-far)
        (t-case nil)
        (tag (gensym))
        (block (gensym)))
    (flet ((maybe-compound (selector)
             (let ((compound (cdr (assq selector nd-onions))))
               (when compound
                 (setq compound (nd-diff compound selectors-so-far))
                 (when (not compound)(error "Unreachable case ~s" selector))
                 (setq selectors-so-far
                       (append compound selectors-so-far))
                 compound))))
      (declare (dynamic-extent maybe-compound))
      `(block ,block
         (tagbody 
           ,tag
           (return-from ,block              
             (case (typecode ,var)
               ,@(mapcar 
                  #'(lambda (case)
                      (let ((selector (car case)))
                        (if (atom selector)
                          (cond ((eq selector t)(setq t-case t))
                                ((memq selector selectors-so-far)(error "Unreachable case ~s" selector))
                                ((let ((compound (maybe-compound selector)))
                                   (when compound
                                     (setq selector compound))))
                                (t (push selector selectors-so-far)))
                          (progn
                            (setq selector
                                  (mapcan #'(lambda (item)
                                              (cond ((memq item selectors-so-far))
                                                    ((let ((compound (maybe-compound item)))
                                                       (when compound
                                                         (setq item compound))))
                                                    (t (push item selectors-so-far)))
                                              (if (listp item) item (list item)))
                                          selector))))
                        (setq selector (if (listp selector)
                                         (mapcar #'type-name-to-code selector)
                                         (if (eq selector t) t
                                             (type-name-to-code selector))))
                        `(,selector ,@(cdr case))))
                  cases)
               ,@(if (not t-case)
                   `((t (setq ,var (%kernel-restart $xwrongtype ,var ',(nd-type-compose selectors-so-far)))
                        (go ,tag)))))))))))

(provide "NUMBER-CASE-MACRO")
