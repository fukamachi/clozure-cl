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

; Backquote.lisp

(in-package "CCL")

#+nil
(progn
;;; Common Lisp backquote implementation, written in Common Lisp.
;;; Author: Guy L. Steele Jr.     Date: 27 December 1985
;;; Texted under Symbolics Common Lisp and Lucid Common Lisp.
;;; This software is in the public domain.

;;; The following are unique tokens used during processing
;;; They need not be symbols; they need not even be atoms.

(defvar *comma* (make-symbol "`,"))
(defvar *comma-atsign* (make-symbol "`,@"))
(defvar *comma-dot* (make-symbol "`,."))
(defvar *bq-list* (make-symbol "BQ-LIST"))
(defvar *bq-append* (make-symbol "BQ-APPEND"))
(defvar *bq-list** (make-symbol "BQ-LIST*"))
(defvar *bq-nconc* (make-symbol "BQ-NCONC"))
(defvar *bq-clobberable* (make-symbol "BQ-CLOBBERABLE"))
(defvar *bq-quote* (make-symbol "BQ-QUOTE"))
(defvar *bq-quote-nil* (list *bq-quote* nil))

;;; Reader macro characters:
;;;    `foo is read in as (BACKQUOTE foo)
;;;    ,foo is read in as (#:COMMA foo)
;;;    ,@foo is read in as (#:COMMA-ATSIGN foo)
;;;    ,.foo is read in as (#:COMMA-DOT foo)
;;; where #:COMMA is the value of the variable *COMMA* etc.

;;; BACKQUOTE is an ordinary macro (not a read-macro) that
;;; processes the expression foo, looking for occurrences of
;;; #:COMMA, #:COMMA-ATSIGN, and #:COMMA-DOT.  It constructs code
;;; in strict accordance with the rules on pages 349-350 of
;;; of the first edition (pages 528-529 of this second edition).
;;; It then optionally applies a code simplifier.

(set-macro-character #\`
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (list 'backquote (read stream t nil t))))

(set-macro-character #\,
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (case (peek-char nil stream t nil t)
                           (#\@ (read-char stream t nil t)
                            (list *comma-atsign* (read stream t nil t)))
                           (#\. (read-char stream t nil t)
                            (list *comma-dot* (read stream t nil t)))
                           (otherwise (list *comma* (read stream t nil t))))))

;;; if the value of *BQ-SIMPLIFY* is non-nil, then BACKQUOTE
;;; processing applies the code simplifier.  If the value is NIL,
;;; then the code resulting from BACKQUOTE is exactly that
;;; specified by the official rules.

(defvar *bq-simplify* t)

(defmacro backquote (x)
  (bq-completely-process x))

;;; Backquote processing proceeds in three stages:
;;;
;;; (1) BQ-PROCESS applies the rules to remove occurrences of
;;; #:COMMA, #:COMMA-ATSIGN, and #:COMMA-DOT corresponding to
;;; this level of BACKQUOTE.  (It also causes embedded calls to
;;; BACKQUOTE to be expanded so that nesting is properly handled.)
;;; Code is produced that is expressed in terms of functions
;;; #:BQ-LIST, #:BQ-APPEND, and #:BQ-CLOBBERABLE.  This is done
;;; so that the simplifier will simplify only list construction
;;; functions actually generated by backquote and will not involve
;;; any user code in the simplification.   #:BQ-LIST means LIST,
;;; #:BQ-APPEND means APPEND, and #:BQ-CLOBBERABLE means IDENTITY
;;; but indicates places where ",." was used and where NCONC may
;;; therefore be introduced by the simplifier for efficiency.
;;;
;;; (2) BQ-SIMPLIFY, if used, rewrites the code produced by
;;; BQ-PROCESS to produce equivalent but faster code.  The
;;; additional functions #:BQ-LIST* and #:BQ-NCONC may be
;;; introduced into the code.
;;;
;;; (3) BQ-REMOVE-TOKENS goes through the code and replaces
;;; #:BQ-LIST with LIST, #:BQ-APPEND with APPEND, and so on.
;;; #:BQ-CLOBBERABLE is simply eliminated (a call to it being
;;; replaced by its argument).  #:BQ-LIST* is replaced by either
;;; LIST* or CONS (the latter is used in the two-argument case,
;;; purely to make the resulting code a tad more readable).

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens (if *bq-simplify*
                        (bq-simplify raw-result)
                        raw-result))))

; Portable code could just say (coerce list 'vector)
(defun list-to-vector (list)
  (unless (listp list)
    (setq list (require-type list 'list)))
  (%list-to-uvector nil list))

(define-compiler-macro list-to-vector (&whole whole form)
  (if (quoted-form-p form)
    (list-to-vector (cadr form))
    whole))

(defun bq-process (x)
  (cond ((atom x)
         (if (simple-vector-p x)
           (list 'list-to-vector (bq-process (coerce x 'list)))
           (list *bq-quote* x)))
        ((eq (car x) 'backquote)
         (bq-process (bq-completely-process (cadr x))))
        ((eq (car x) *comma*) (cadr x))
        ((eq (car x) *comma-atsign*)
         (error ",@~S after `" (cadr x)))
        ((eq (car x) *comma-dot*)
         (error ",.~S after `" (cadr x)))
        (t (do ((p x (cdr p))
                (q '() (cons (bracket (car p)) q)))
               ((atom p)
                (cons *bq-append*
                      (nreconc q (list (list *bq-quote* p)))))
             (when (eq (car p) *comma*)
               (unless (null (cddr p)) (error "Malformed ,~S" p))
               (return (cons *bq-append*
                             (nreconc q (list (cadr p))))))
             (when (eq (car p) *comma-atsign*)
               (error "Dotted ,@~S" p))
             (when (eq (car p) *comma-dot*)
               (error "Dotted ,.~S" p))))))

;;; This implements the bracket operator of the formal rules

(defun bracket (x)
  (cond ((atom x)
         (list *bq-list* (bq-process x)))
        ((eq (car x) *comma*)
         (list *bq-list* (cadr x)))
        ((eq (car x) *comma-atsign*)
         (cadr x))
        ((eq (car x) *comma-dot*)
         (list *bq-clobberable* (cadr x)))
        (t (list *bq-list* (bq-process x)))))

;;; This auxiliary function is like MAPCAR but has two extra
;;; purpoess: (1) it handles dotted lists; (2) it tries to make
;;; the result share with the argument x as much as possible.

(defun maptree (fn x)
  (if (atom x)
    (funcall fn x)
    (let ((a (funcall fn (car x)))
          (d (maptree fn (cdr x))))
      (if (and (eql a (car x)) (eql d (cdr x)))
        x
        (cons a d)))))

;;; This predicate is true of a form that when read looked
;;; like ,@foo or ,.foo

(defun bq-splicing-frob (x)
  (and (consp x)
       (or (eq (car x) *comma-atsign*)
           (eq (car x) *comma-dot*))))

;;; This predicate is true of a form that when read
;;; looked like ,@foo or just plain ,foo.

(defun bq-frob (x)
  (and (consp x)
       (or (eq (car x) *comma*)
           (eq (car x) *comma-atsign*)
           (eq (car x) *comma-dot*))))

;;; The simplifier essentially looks for calls to #:BQ-APPEND and
;;; tries to simplify them.  The arguments to #:BQ-APPEND are
;;; processed from right to left, building up a replacement for.
;;; At each step a number of special cases are handled that,
;;; loosely speaking, look like this:
;;;
;;; (APPEND (LIST a b c) foo) => (LIST* a b c foo)
;;;   provided a, b, c are not splicing frobs
;;; (APPEND (LIST* a b c) foo) => (LIST* a b (APPEND c foo))
;;;   provided a, b, c are not splicing frobs
;;; (APPEND (QUOTE (x)) foo) => (LIST* (QUOTE x) foo)
;;; (APPEND (CLOBBERABLE x) foo) => (NCONC x foo)

(defun bq-simplify (x)
  (if (atom x)
    x
    (let ((x (if (eq (car x) *bq-quote*)
               x
               (maptree #'bq-simplify x))))
      (if (not (eq (car x) *bq-append*))
        x
        (bq-simplify-args x)))))

(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result
        nil
        (cond ((atom (car args))
               (bq-attach-append *bq-append* (car args) result))
              ((and (eq (caar args) *bq-list*)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses (cdar args) result))
              ((and (eq (caar args) *bq-list**)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses
                (reverse (cdr (reverse (cdar args))))
                (bq-attach-append *bq-append*
                                  (car (last (car args)))
                                  result)))
              ((and (eq (caar args) *bq-quote*)
                    (consp (cadar args))
                    (not (bq-frob (cadar args)))
                    (null (cddar args)))
               (bq-attach-conses (list (list *bq-quote*
                                             (caadar args)))
                                 result))
              ((eq (caar args) *bq-clobberable*)
               (bq-attach-append *bq-nconc* (cadar args) result))
              (t (bq-attach-append *bq-append*
                                   (car args)
                                   result)))))
      ((null args) result)))

(defun null-or-quoted (x)
  (or (null x) (and (consp x) (eq (car x) *bq-quote*))))

;;; When BQ-ATTACH-APPEND is called, the OP should be #:BQ-APPEND
;;; or #:BQ-NCONC.  This produces a form (op item result) but
;;; some simplifications are done on the fly:
;;;
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g)
;;;  (op item 'nil) => item, provided item is not a splicable frob
;;;  (op item 'nil) => (op item), if item is a splicable frob
;;;  (op item (op a b c)) => (op item a b c)

(defun bq-attach-append (op item result)
  (cond ((and (null-or-quoted item) (null-or-quoted result))
         (list *bq-quote* (append (cadr item) (cadr result))))
        ((or (null result) (equal result *bq-quote-nil*))
         (if (bq-splicing-frob item) (list op item) item))
        ((and (consp result) (eq (car result) op))
         (list* (car result) item (cdr result)))
        (t (list op item result))))

;;; The effec tof BQ-ATTACH-CONSES is to produce a form as if by
;;; `(LIST* ,@items ,result) but some simplifications are done
;;; on the fly.
;;;
;;;  (LIST* 'a 'b 'c 'd) => '(a b c . d)
;;;  (LIST* a b c 'nil) => (LIST a b c)
;;;  (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g)
;;;  (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)

(defun bq-attach-conses (items result)
  (cond ((and (every #'null-or-quoted items)
              (null-or-quoted result))
         (list *bq-quote*
               (append (mapcar #'cadr items) (cadr result))))
        ((or (null result) (equal result *bq-quote-nil*))
         (cons *bq-list* items))
        ((and (consp result)
              (or (eq (car result) *Bq-list*)
                  (eq (car result) *bq-list**)))
         (cons (car result) (append items (cdr result))))
        (t (cons *bq-list** (append items (list result))))))

;;; Removes funny toeksn and changes (#:BQ-LIST* a b) into
;;; (CONS a b) instead of (LIST* a b), purely for readability.

(defun bq-remove-tokens (x)
  (cond ((eq x *bq-list*) 'list)
        ((eq x *bq-append*) 'append)
        ((eq x *bq-nconc*) 'nconc)
        ((eq x *bq-list**) 'list*)
        ((eq x *bq-quote*) 'quote)
        ((atom x) x)
        ((eq (car x) *bq-clobberable*)
         (bq-remove-tokens (cadr x)))
        ((and (eq (car x) *bq-list**)
              (consp (cddr x))
              (null (cdddr x)))
         (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
        (t (maptree #'bq-remove-tokens x))))

)

#-nil
(progn
(declaim (special *|`,|* *|`,.|* *|`,@|*))

;;;Backquote reads in as a call to the BACKQUOTE-EXPANDER macro.
;;;This makes it a little obscure to look at raw, but makes it possible for
;;;the pretty-printer to print things read in with backquote.

(defvar *backquote-expand* t "If non-NIL, expand at read-time")

(defmacro backquote-expander (*|`,|* *|`,.|* *|`,@|* form)
   (declare (special *|`,|* *|`,.|* *|`,@|*))
   (multiple-value-bind (form constantp) (backquote-aux form)
     (backq-form form constantp)))

(defun backquote-aux (form)
  ;;Doesn't try to optimize multiple CONS's into LIST/LIST*'s, leaving it up
  ;;to the compiler.  The code here is mainly concerned with folding
  ;;constants, since the compiler is not allowed to do that in general.
  (cond
   ((simple-vector-p form)
    (let ((elts ()) (i (length form)))
      (until (%izerop i) (push (svref form (setq i (%i- i 1))) elts))
      (multiple-value-bind (elts quotedp) (backquote-aux elts)
        (if quotedp
          (values (list-to-vector elts) t)
          (list 'list-to-vector elts)))))
   ((self-evaluating-p form) (values form t))
   ((atom form) (values form t))
   ((eq (%car form) 'backquote-expander) (backquote-aux (macroexpand-1 form)))
   ((eq (%car form) *|`,|*) (%cdr form))
   ((eq (%car form) *|`,@|*) (error "Misplaced ,@~S after backquote" (%cdr form)))
   ((eq (%car form) *|`,.|*) (error "Misplaced ,.~S after backquote" (%cdr form)))
   (t (let* ((car (%car form))
             (splice (and (consp car) (if (eq (%car car) *|`,@|*) 'append
                                        (if (eq (%car car) *|`,.|*) 'nconc)))))
        (multiple-value-bind (cdr qd) (backquote-aux (%cdr form))
          (if splice
            (cond ((null (%cdr car)) (values cdr qd))
                  ((null cdr) (values (%cdr car) (self-evaluating-p (%cdr car))))
                  (t (list splice (%cdr car) (backq-form cdr qd))))
            (multiple-value-bind (car qa) (backquote-aux car)
              (cond ((and qa qd) (values (cons car cdr) t))
                    ((null cdr) (list 'list car))
                    (t (list 'list*     ; was CONS
                             (backq-form car qa) (backq-form cdr qd)))))))))))

(defun backq-form (form constantp)
  (if (and constantp (not (self-evaluating-p form))) (list 'quote form) form))

(defparameter *backquote-stack* ())

(set-macro-character 
 #\`
 (nfunction 
  |` reader|
  (lambda (stream char &aux form)
    (declare (ignore char))
    (setq form
          (let* ((|`,| (make-symbol "`,"))
                 (|`,.| (make-symbol "`,."))
                 (|`,@| (make-symbol "`,@")))
            (list 'backquote-expander |`,| |`,.| |`,@|
                  (let ((*backquote-stack* (list* |`,| |`,.| |`,@| *backquote-stack*)))
                    (read stream t nil t)))))
    (if *backquote-expand* (values (macroexpand-1 form)) form))))

(set-macro-character 
 #\, 
 (nfunction
  |, reader| 
  (lambda (stream char &aux (stack *backquote-stack*))
    (when (null stack)
      (signal-reader-error stream "Comma not inside backquote"))
    (let ((*backquote-stack* (cdddr stack)))
      (setq char (tyi stream))
      (cond ((eq char #\@)
             (cons (%caddr stack) (read stream t nil t)))
            ((eq char #\.)
             (cons (%cadr stack) (read stream t nil t)))
            (t
             (untyi char stream)
             (cons (%car stack) (read stream t nil t))))))))
)

(provide 'backquote)
