;;;-*-Mode: LISP; Package: CCL -*-
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

(eval-when (eval compile)
  (require 'backquote)
  (require 'level-2))



;;; These functions perform basic list operations:

#|
(defun caar (list) (car (car list)))
(defun cadr (list) (car (cdr list)))
(defun cdar (list) (cdr (car list)))
(defun cddr (list) (cdr (cdr list)))

(defun caaar (list) (car (caar list)))
(defun caadr (list) (car (cadr list)))
(defun cadar (list) (car (cdar list)))
(defun caddr (list) (car (cddr list)))
(defun cdaar (list) (cdr (caar list)))
(defun cdadr (list) (cdr (cadr list)))
(defun cddar (list) (cdr (cdar list)))
(defun cdddr (list) (cdr (cddr list)))
|#


(defun caaaar (list) (car (caaar list)))
(defun caaadr (list) (car (caadr list)))
(defun caadar (list) (car (cadar list)))
(defun caaddr (list) (car (caddr list)))
(defun cadaar (list) (car (cdaar list)))
(defun cadadr (list) (car (cdadr list)))
(defun caddar (list) (car (cddar list)))
(defun cdaaar (list) (cdr (caaar list)))
(defun cdaadr (list) (cdr (caadr list)))
(defun cdadar (list) (cdr (cadar list)))
(defun cdaddr (list) (cdr (caddr list)))
(defun cddaar (list) (cdr (cdaar list)))
(defun cddadr (list) (cdr (cdadr list)))
(defun cdddar (list) (cdr (cddar list)))
(defun cddddr (list) (cdr (cdddr list)))

(defun tree-equal (x y &key (test (function eql)) test-not)
  "Returns T if X and Y are isomorphic trees with identical leaves."
  (if test-not
      (tree-equal-test-not x y test-not)
      (tree-equal-test x y test)))

(defun tree-equal-test-not (x y test-not)
  (cond ((and (atom x) (atom y))
         (if (and (not x) (not y)) ;must special case end of both lists.
           t
           (if (not (funcall test-not x y)) t)))
	((consp x)
	 (and (consp y)
	      (tree-equal-test-not (car x) (car y) test-not)
	      (tree-equal-test-not (cdr x) (cdr y) test-not)))
	(t ())))

(defun tree-equal-test (x y test)
  (if (atom x)
    (if (atom y)
      (if (funcall test x y) t))
    (and (consp y)
         (tree-equal-test (car x) (car y) test)
         (tree-equal-test (cdr x) (cdr y) test))))

(defun first (list)
  (car list))

(defun second (list)
  (cadr list))

(defun third (list)
  (caddr list))

(defun fourth (list)
  (cadddr list))

(defun fifth (list)
  (car (cddddr list)))

(defun sixth (list)
  (cadr (cddddr list)))

(defun seventh (list)
  (caddr (cddddr list)))

(defun eighth (list)
  (cadddr (cddddr list)))

(defun ninth (list)
  (car (cddddr (cddddr list))))

(defun tenth (list)
  (cadr (cddddr (cddddr list))))

(defun rest (list)
  (cdr list))
;;; List* is done the same as list, except that the last cons is made a
;;; dotted pair


;;; List Copying Functions

;;; The list is copied correctly even if the list is not terminated by ()
;;; The new list is built by cdr'ing splice which is always at the tail
;;; of the new list


(defun copy-alist (alist)
  "Returns a new association list equal to alist, constructed in space"
  (if (atom alist)
    (if alist
      (report-bad-arg alist 'list))
    (let ((result
           (cons (if (atom (car alist))
                   (car alist)
                   (cons (caar alist) (cdar alist)) )
                 '() )))	      
      (do ((x (cdr alist) (cdr x))
           (splice result
                   (cdr (rplacd splice
                                (cons
                                 (if (atom (car x)) 
                                   (car x)
                                   (cons (caar x) (cdar x)))
                                 '() ))) ))
          ;;; Non-null terminated alist done here.
          ((atom x) (unless (null x)
                      (rplacd splice x))
           result)))))

;;; More Commonly-used List Functions

(defun revappend (x y)
  "Returns (append (reverse x) y)"
  (dolist (a x y) (push a y)))

;;; The outer loop finds the first non-null list.  Starting with this list
;;; the inner loop tacks on the remaining lists in the arguments


(defun butlast (list &optional n)
  "Returns a new list the same as List without the N last elements."
  (setq list (require-type list 'list))
  (if (and n (or (not (fixnump n)) (< n 0))) (report-bad-arg n '(fixnump 0 *)))
  (let* ((count (if n (- (alt-list-length list) n) most-positive-fixnum))
         it tail)
    (declare (fixnum count))
    (do ((l list (cdr l)))
        ((if n (<= count 0)(not (consp (cdr l)))) it)
        (declare (list l))
        (let ((newtail (list (car l))))
          (cond (tail (rplacd (the cons tail) newtail))
                (t (setq it newtail)))
          (setq tail newtail)
          (setq count (1- count))))))

(defun alt-list-length (list)
  (let ((n 0))
    (declare (fixnum n))
    (while 
      (consp list)
      (setq list (cdr (the list list)))
      (setq n (1+ n)))
    n))      

(defun nbutlast (list &optional n)
  "Modifies List to remove the last N elements."
  (setq list (require-type list 'list))
  (if (and n (or (not (fixnump n)) (< n 0))) (report-bad-arg n '(fixnum 0 *)))
  (let* ((count (if n (- (alt-list-length list) n) most-positive-fixnum)))

    (declare (fixnum count))
    (do ((last list l)
         (l list (cdr l)))
        ((if n (<= count 0)(not (consp (cdr l))))
         (if (eq l list) nil (progn (rplacd (the cons last) nil) list)))
      (declare (list l))
      (setq count (1- count)))))

      

(defun ldiff (list object) 
  "Returns a new list, whose elements are those of List that appear before
   Sublist.  If Sublist is not a tail of List, a copy of List is returned."
  (do* ((list (require-type list 'list) (cdr list)) 
        (result (cons nil nil))
        (splice result))
       ((atom list) 
        (if (eql list object) 
	  (cdr result) 
	  (progn (rplacd splice list) (cdr result))))
    (declare (dynamic-extent result)
	     (cons splice result))
    (if (eql list object) 
      (return (cdr result)) 
      (setq splice (cdr (rplacd splice (list (car list))))))))


;;; Functions to alter list structure

;;; The following are for use by SETF.

(defun %setnth (n list newval)
  "Sets the Nth element of List (zero based) to Newval."
  (if (%i< n 0)
      (error "~S is an illegal N for SETF of NTH." n)
      (do ((count n (%i- count 1)))
          ((%izerop count) (rplaca list newval) newval)
        (if (endp (cdr list))
            (error "~S is too large an index for SETF of NTH." n)
            (setq list (cdr list))))))

(defun test-not-error (test test-not)
  (%err-disp $xkeyconflict :test test :test-not test-not))

;;; Use this with the following keyword args:
;;;  (&key (key #'identity) (test #'eql testp) (test-not nil notp))

(eval-when (eval compile #-bccl load)
 (defmacro with-set-keys (funcall)
   `(cond (notp ,(append funcall '(:key key :test-not test-not)))
          (t ,(append funcall '(:key key :test test)))))

;;; Works with the above keylist.  We do three clauses so that if only test-not
;;; is supplied, then we don't test eql.  In each case, the args should be 
;;; multiply evaluable.

(defmacro elements-match-p (elt1 elt2)
  `(or (and testp
	    (funcall test (funcall key ,elt1) (funcall key ,elt2)))
       (and notp
	    (not (funcall test-not (funcall key ,elt1) (funcall key ,elt2))))
       (eql (funcall key ,elt1) (funcall key ,elt2))))



)
;;; Substitution of expressions

;subst that doesn't call labels
(defun subst (new old tree &key key
		           (test #'eql testp) (test-not nil notp))   
  "Substitutes new for subtrees matching old."
  (if (and testp notp)
    (test-not-error test test-not))
  (subst-aux new old tree key test test-not))

(defun subst-aux (new old subtree key test test-not)
  (flet ((satisfies-the-test (item elt)
           (let* ((val (if key (funcall key elt) elt)))
             (if test-not
               (not (funcall test-not item val))
               (funcall test item val)))))
    (declare (inline satisfies-the-test))
    (cond ((satisfies-the-test old subtree) new)
          ((atom subtree) subtree)
          (t (let ((car (subst-aux new old (car subtree)
                                   key test test-not ))
                   (cdr (subst-aux new old (cdr subtree)
                                   key test test-not)))
               (if (and (eq car (car subtree))
                        (eq cdr (cdr subtree)))
                 subtree
                 (cons car cdr)))))))

;;;subst-if without a call to labels
;;; I've always wondered how those calls to a special operator
;;; should best be avoided.  Clearly, the answer involves
;;; lots of recursion.
(defun subst-if (new test tree &key key)
  "Substitutes new for subtrees for which test is true."
  (unless key (setq key #'identity))
  (cond ((funcall test (funcall key tree)) new)
        ((atom tree) tree)
        (t (let ((car (subst-if new test (car tree) :key key))
                 (cdr (subst-if new test (cdr tree) :key key)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
               tree
               (cons car cdr))))))

;subst-if-not without a call to labels
(defun subst-if-not (new test tree &key key)
  "Substitutes new for subtrees for which test is false."
  "replace with above def when labels works."
  (unless key (setq key #'identity))
  (cond ((not (funcall test (funcall key tree))) new)
        ((atom tree) tree)
        (t (let ((car (subst-if-not new test (car tree) :key key))
                 (cdr (subst-if-not new test (cdr tree) :key key)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
               tree
               (cons car cdr))))))

(defun nsubst (new old tree &key key
                   (test #'eql testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  "replace with above def when labels works"
  (if (and testp notp)
    (test-not-error test test-not))
  (nsubst-aux new old tree (or key #'identity) test test-not))

(defun nsubst-aux (new old subtree key test test-not)
  (flet ((satisfies-the-test (item elt)
           (let* ((val (if key (funcall key elt) elt)))
             (if test-not
               (not (funcall test-not item val))
               (funcall test item val)))))
    (declare (inline satisfies-the-test))
    (cond ((satisfies-the-test old subtree) new)
          ((atom subtree) subtree)
          (t (do* ((last nil subtree)
                   (subtree subtree (cdr subtree)))
                  ((atom subtree)
                   (if (satisfies-the-test old subtree)
                     (set-cdr last new)))
               (if (satisfies-the-test old subtree)
                 (return (set-cdr last new))
                 (set-car subtree 
                          (nsubst-aux new old (car subtree)
                                      key test test-not))))
             subtree))))

(defun nsubst-if (new test tree &key key)
  "Substitutes new for subtrees of tree for which test is true."
  "replace with above def when labels works."
  (unless key (setq key #'identity))
  (cond ((funcall test (funcall key tree)) new)
        ((atom tree) tree)
        (t (do* ((last nil tree)
                 (tree tree (cdr tree)))
                ((atom tree)
                 (if (funcall test (funcall key tree))
                   (set-cdr last new)))
             (if (funcall test (funcall key tree))
               (return (set-cdr last new))
               (set-car tree 
                        (nsubst-if new test (car tree) :key key))))
           tree)))

(defun nsubst-if-not (new test tree &key key)
  "Substitutes new for subtrees of tree for which test is false."
  "Replace with above def when labels works."
  (unless key (setq key #'identity))
  (cond ((not (funcall test (funcall key tree))) new)
        ((atom tree) tree)
        (t (do* ((last nil tree)
                 (tree tree (cdr tree)))
                ((atom tree)
                 (if (not (funcall test (funcall key tree)))
                   (set-cdr last new)))
             (if (not (funcall test (funcall key tree)))
               (return (set-cdr (cdr last) new))
               (set-car tree 
                        (nsubst-if-not new test (car tree) :key key))))
           tree)))

(defun sublis (alist tree &key key
                     (test #'eql testp) (test-not nil notp))
  "Substitutes from alist into tree nondestructively."
  (if (and testp notp)
    (test-not-error test test-not))
  (sublis-aux alist tree (or key #'identity) test test-not notp))

(defun sublis-aux  (alist subtree key test test-not notp) 
  (let ((assoc (if notp
                 (assoc (funcall key subtree) alist :test-not test-not)
                 (assoc (funcall key subtree) alist :test test))))
    (cond (assoc (cdr assoc))
          ((atom subtree) subtree)
          (t (let ((car (sublis-aux alist (car subtree)
                                    key test test-not notp))
                   (cdr (sublis-aux alist (cdr subtree)
                                    key test test-not notp)))
               (if (and (eq car (car subtree))
                        (eq cdr (cdr subtree)))
                 subtree
                 (cons car cdr)))))))

(eval-when (compile eval)
  (defmacro nsublis-macro ()
    '(if notp
       (assoc (funcall key subtree) alist :test-not test-not)
       (assoc (funcall key subtree) alist :test test)))
  )

(defun nsublis (alist tree &key key
                      (test #'eql testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (if (and testp notp)
    (test-not-error test test-not))
  (nsublis-aux alist tree (or key #'identity) test test-not notp))

(defun nsublis-aux (alist subtree key test test-not notp &optional temp)
  (cond ((setq temp (nsublis-macro))
         (cdr temp))
        ((atom subtree) subtree)
        (t (do*  ((last nil subtree)
                  (subtree subtree (cdr subtree)))
                 ((atom subtree)
                  (if (setq temp (nsublis-macro))
                    (set-cdr last (cdr temp))))
             (if (setq temp (nsublis-macro))
               (return (set-cdr last (cdr temp)))
               (set-car subtree 
                        (nsublis-aux alist (car subtree) key test
                                     test-not notp temp))))
           subtree)))

;;; Functions for using lists as sets


(defun member-if (test list &key key )
  "Returns tail of list beginning with first element satisfying test(element)"
  (unless key (setq key #'identity))
  (do ((list list (Cdr list)))
      ((endp list) nil)
    (if (funcall test (funcall key (car list)))
      (return list))))

(defun member-if-not (test list &key key)
  "Returns tail of list beginning with first element not satisfying test(el)"
  (unless key (setq key #'identity))
  (do ((list list (cdr list)))
      ((endp list) ())
    (if (not (funcall test (funcall key (car list))))
      (return list))))

(defun tailp (sublist list)                  ;Definition "B"
  (do ((list list (%cdr list)))
      ((atom list) (eql list sublist))
    (if (eq sublist list)
      (return t))))


 
(defun union (list1 list2  &key
                    key
                    (test #'eql testp)
                    (test-not nil notp))
  "Returns the union of List1 and List2."
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (let ((res list2))
    (dolist (elt list1)
      (if (not (with-set-keys (member (funcall key elt) list2)))
        (push elt res)))
    res))






(eval-when (eval compile #-bccl load)
;;; Destination and source are setf-able and many-evaluable.
;;; Sets the source to the cdr, and "conses" the 1st elt of 
;;; source to destination.
(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
           (cdr temp) ,destination
           ,destination temp)))
)

(defun nunion (list1 list2 &key key
                     (test #'eql testp) (test-not nil notp))
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (let ((res list2))
    (do ()
        ((endp list1))
      (if (not (with-set-keys (member (funcall key (car list1)) list2)))
        (steve-splice list1 res)
        (setq list1 (cdr list1))))
    res))




(defun intersection (list1 list2  &key key
                           (test #'eql testp) (test-not nil notp))
  "Returns the intersection of List1 and List2."
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (funcall key elt) list2))
        (push elt res)))
    res))

(defun nintersection (list1 list2 &key key
                            (test #'eql testp) (test-not nil notp))
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (let ((res nil))
    (do () ((endp list1))
      (if (with-set-keys (member (funcall key (car list1)) list2))
        (steve-splice list1 res)
        (setq list1 (Cdr list1))))
    res))

(defun set-difference (list1 list2 &key key
                             (test #'eql testp) (test-not nil notp))
  "Returns a lsit of the elements in LIST1 which are not in LIST2."
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (let ((res nil))
    (dolist (elt list1)
      (if (not (with-set-keys (member (funcall key elt) list2)))
        (push elt res)))
    res))

(defun nset-difference (list1 list2 &key key
                              (test #'eql testp) (test-not nil notp))
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (let ((res nil))
    (do () ((endp list1))
      (if (not (with-set-keys (member (funcall key (car list1)) list2)))
	  (steve-splice list1 res)
          (setq list1 (cdr list1))))
    res))

#| spice version
(defun set-exclusive-or (list1 list2 &key (key #'identity)
                               (test #'eql testp) (test-not nil notp))
  "Returns new list of elements appearing exactly  once in List1 and List2.
  If an element appears > once in a list and does not appear at all in the
  other list, that element will appear >1 in the output list."
  (let ((result nil))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall key elt) list2))
        (setq result (cons elt result))))
    (dolist (elt list2)
      (unless (with-set-keys (member (funcall key elt) list1))
        (setq result (cons elt result))))
    result))
|#

(defun set-exclusive-or (list1 list2 &key key
                               (test #'eql testp) (test-not nil notp)
                               &aux result elt1-compare elt2-compare)
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (dolist (elt1 list1)
    (setq elt1-compare (funcall key elt1))
    (if (if notp
           (dolist (elt2 list2 t)
            (if (not (funcall test-not elt1-compare (funcall key elt2)))
              (return nil)))
          (dolist (elt2 list2 t)
            (if (funcall test elt1-compare (funcall key elt2))
              (return nil))))
      (push elt1 result)))
  (dolist (elt2 list2)
    (setq elt2-compare (funcall key elt2))
    (if (if notp
          (dolist (elt1 list1 t)
            (if (not (funcall test-not (funcall key elt1) elt2-compare))
              (return nil)))
          (dolist (elt1 list1 t)
            (if (funcall test (funcall key elt1) elt2-compare)
              (return nil))))
      (push elt2 result)))
  result)

#| the description of the below SpiceLisp algorthm used for implementing
 nset-exclusive-or sounds counter to CLtL. Furthermore, it fails 
on the example (nset-exclusive-or (list 1 1) (list 1))
  [returns (1) but should return NIL.] ... fry

;;; The outer loop examines list1 while the inner loop examines list2. If an
;;; element is found in list2 "equal" to the element in list1, both are
;;; spliced out. When the end of list1 is reached, what is left of list2 is
;;; tacked onto what is left of list1.  The splicing operation ensures that
;;; the correct operation is performed depending on whether splice is at the
;;; top of the list or not

(defun nset-exclusive-or (list1 list2 &key (test #'eql) (test-not nil notp)
                                (key #'identity))
  "Return a list with elements which appear but once in List1 and List2."
  (do ((x list1 (cdr x))
       (splicex ()))
      ((endp x)
       (if (null splicex)
         (setq list1 list2)
         (rplacd splicex list2))
       list1)
    (do ((y list2 (cdr y))
         (splicey ()))
        ((endp y) (setq splicex x))
      (cond ((if notp 
               (not (funcall test-not (funcall key (car x))
                             (funcall key (car y))))
               (funcall test (funcall key (car x)) 
                        (funcall key (car y))))
             (if (null splicex)
               (setq list1 (cdr x))
               (rplacd splicex (cdr x)))
             (if (null splicey) 
               (setq list2 (cdr y))
               (rplacd splicey (cdr y)))
             (return ()))			; assume lists are really sets
            (t (setq splicey y))))))
|#

(defun nset-exclusive-or (list1 list2 &key key
                               (test #'eql testp) (test-not nil notp))
   (if (and testp notp)
     (test-not-error test test-not))
   (unless key (setq key #'identity))
   (if notp
     (set-exclusive-or list1 list2 :key key :test-not test-not)
     (set-exclusive-or list1 list2 :key key :test test)
     ))
 
(defun subsetp (list1 list2 &key key
                      (test #'eql testp) (test-not nil notp))
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key (setq key #'identity))
  (dolist (elt list1)
    (unless (with-set-keys (member (funcall key elt) list2))
      (return-from subsetp nil)))
  T)
    

;;; Functions that operate on association lists

(defun acons (key datum a-list)
  (cons (cons key datum) a-list))

(defun pairlis (keys data &optional (alist '()))
  "Construct an association list from keys and data (adding to alist)"
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) alist)
    (if (or (endp x) (endp y)) 
      (error "The lists of keys and data are of unequal length."))
    (setq alist (acons (car x) (car y) alist))))

(defun default-identity-key (key)
  (and key (neq key 'identity) (neq key #'identity) (coerce-to-function key)))

(defun assoc-if (predicate alist &key key)
  "Returns the first cons in alist whose car satisfies the Predicate."
  (setq key (default-identity-key key))
  (dolist (pair alist)
    (when (and pair
               (funcall predicate 
                        (if key (funcall key (car pair))
                            (car pair))))
      (return pair))))

(defun assoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose car does not satisfy the Predicate."
  (setq key (default-identity-key key))
  (dolist (pair alist)
    (when (and pair
               (not (funcall predicate 
                        (if key (funcall key (car pair))
                            (car pair)))))
      (return pair))))

(defun rassoc-if (predicate alist &key key)
  "Returns the first cons in alist whose cdr satisfies the Predicate."
  (setq key (default-identity-key key))
  (dolist (pair alist)
    (when (and pair
               (funcall predicate 
                        (if key (funcall key (cdr pair))
                            (cdr pair))))
      (return pair))))

(defun rassoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose cdr does not satisfy the Predicate."
  (setq key (default-identity-key key))
  (dolist (pair alist)
    (when (and pair
               (not (funcall predicate 
                        (if key (funcall key (cdr pair))
                            (cdr pair)))))
      (return pair))))


(defun map1 (function original-arglists accumulate take-car)
 "This function is called by mapc, mapcar, mapcan, mapl, maplist, and mapcon.
 It Maps function over the arglists in the appropriate way. It is done when any
 of the arglists runs out.  Until then, it CDRs down the arglists calling the
 function and accumulating results as desired."
  (let* ((length (length original-arglists))
         (arglists (make-list length))
         (args (make-list length))
         (ret-list (list nil))
         (temp ret-list))
    (declare (dynamic-extent arglists args ret-list))
    (let ((argstail arglists))
      (declare (cons argstail))
      (dolist (arg original-arglists)
        (setf (car argstail) arg)
        (pop argstail)))
    (do ((res nil)
         (argstail args args))
        ((memq nil arglists)	        
         (if accumulate
             (cdr ret-list)
             (car original-arglists)))
      (declare (cons argstail))
      (do ((l arglists (cdr l)))
          ((not l))
        (setf (car argstail) (if take-car (car (car l)) (car l)))
        (rplaca l (cdr (car l)))
        (pop argstail))
      (setq res (apply function args))
      (case accumulate
        (:nconc 
         (setq temp (last (nconc temp res))))
        (:list  (rplacd temp (list res))
                (setq temp (cdr temp)))))))

(defun mapc (function list &rest more-lists)
  "Applies fn to successive elements of lists, returns LIST,
  ie the 2nd arg to mapc."
  (declare (dynamic-extent more-lists))
  (let ((arglists (cons list more-lists)))
    (declare (dynamic-extent arglists))
    (values (map1 function arglists nil t))))

(defun mapcar (function list &rest more-lists)
  "Applies fn to successive elements of list, returns list of results."
  (declare (dynamic-extent more-lists))
  (let ((arglists (cons list more-lists)))
    (declare (dynamic-extent arglists))
    (values (map1 function arglists :list t))))

(defun mapcan (function list &rest more-lists)
  "Applies fn to successive elements of list, returns NCONC of results."
  (declare (dynamic-extent more-lists))
  (let ((arglists (cons list more-lists)))
    (declare (dynamic-extent arglists))
    (values (map1 function arglists :nconc t))))

(defun mapl (function list &rest more-lists)
  "Applies fn to successive CDRs of list, returns LIST."
  (declare (dynamic-extent more-lists))
  (let ((arglists (cons list more-lists)))
    (declare (dynamic-extent arglists))
    (values (map1 function arglists nil nil))))

(defun maplist (function list &rest more-lists)
  "Applies fn to successive CDRs of list, returns list of results."
  (declare (dynamic-extent more-lists))
  (let ((arglists (cons list more-lists)))
    (declare (dynamic-extent arglists))
    (values (map1 function arglists :list nil))))

(defun mapcon (function list &rest more-lists)
  "Applies fn to successive CDRs of lists, returns NCONC of results."
  (declare (dynamic-extent more-lists))
  (let ((arglists (cons list more-lists)))
    (declare (dynamic-extent arglists))
    (values (map1 function arglists :nconc nil))))

;;; Functions for compatibility sake:

(defun delq (item a-list &optional (n 0 np))  
  "Returns list with all (up to n) elements with all elements EQ to ITEM
   deleted"
   ;(%print "a-list = " a-list) 
  (declare (type list a-list) (type integer n))
  ;(%print "a-list = " a-list) 
  (do ((x a-list (cdr x))
       (splice '()))
      ((or (endp x)
           (and np (zerop n))) 
       a-list)
    ; (%print "a-list = " a-list)
    (cond ((eq item (car x))
           (setq n (- n 1))
           (if (null splice) 
             (setq a-list (cdr x))
             (rplacd splice (cdr x))))
          (T (setq splice x)))))	; move splice along to include element

(defun list-length-and-final-cdr (list)
  "First value reutrned is length of regular list.
    [for (a b . c), returns 2]
    [for circular lists, returns NIL]
   Second value is the final cdr.
    [ for (a b), returns NIL
      for (a b . c), returns c
      for circular lists, returns NIL]
   Third value only returned if we have a circular list. It is
   the MAX possible length of the list until the repeat."
   (do* ((n 0 (+ n 2))
         (fast list (cddr fast))
         (slow list (cdr slow)))
        ()
     (declare (fixnum n))
     (cond ((null fast)
            (return (values n nil)))
           ((not (consp fast))
            (return (values n fast)))
           ((null (cdr fast))
            (return (values (1+ n) nil)))
           ((and (eq fast slow) (> n 0)) ;circular list
            (return (values nil nil n)))          
           ((not (consp (cdr fast)))
            (return (values (1+ n) (cdr fast)))))))

(provide 'lists)
