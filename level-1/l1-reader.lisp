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

;;; READ and related functions.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (defconstant readtable-case-keywords '((:upcase . 1) (:downcase . 2) (:preserve . 0)
                                         (:invert . -1) (:studly . -2)))
  (defmacro readtable-case-keywords () `',readtable-case-keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *name-char-alist*
  '(;; Standard character names
    ("Newline" .  #\012) ("Space" . #\040)
    ;; Semi-standard character names
    ("Rubout" . #\177) ("Page" . #\014) ("Tab" . #\011)
    ("Backspace" . #\010) ("Return" . #\015) ("Linefeed" . #\012)
    ;; Other character names.  (When available, standard names
    ;; should be used for printing in preference to any non-standard
    ;; names.)
    ("Null" . #\000) ("Nul" . #\000)
    ("Bell"  . #\007) ; ^G , used by Franz (and others with bells.)
    ("Delete" . #\010) ("BS" . #\010)
    ("LF" . #\012)
    ("PageUp" . #\013)
    ("PageDown" . #\014)("Formfeed" . #\014) ("FF" . #\014)
    ("CR" . #\015)
    ("ESC" .  #\033) ("Escape" . #\033) ("Clear" .  #\033)
    ("Altmode" .  #\033) ("ALT" .  #\033)
    ("Fs" . #\034)
    ("Gs" . #\035)
    ("Rs" . #\036)
    ("Us" . #\037)
    ("DEL" . #\177)("ForwardDelete" . #\177) 
    ))

;;; Character names are stored in *NAME-CHAR-ALIST* which consists of
;;; (name . char) where name must be a simple string and char must be
;;; a character.

;;;(NAME-CHAR name)
;;;If name has an entry in the *NAME-CHAR-ALIST*, return first such entry.
;;;Otherwise, if it consists of one char, return it.
;;;Otherwise, if it consists of two chars, the first of which  is ^,
;;; return %code-char(c xor 64), where c is the uppercased second char.
;;;Otherwise, if it consists of octal digits, the digits are
;;; interpreted as the (mod 256) ascii code of a character.
;;;Otherwise return NIL.

(defun name-char (name)
  "Given an argument acceptable to STRING, NAME-CHAR returns a character
  whose name is that string, if one exists. Otherwise, NIL is returned."
  (if (characterp name)
    name
    (let* ((name (string name)))
      (let* ((namelen (length name)))
        (declare (fixnum namelen))
        (or (cdr (assoc name *name-char-alist* :test #'string-equal))
         (if (= namelen 1)
           (char name 0)
           (if (and (= namelen 2) (eq (char name 0) #\^))
             (code-char (the fixnum (logxor (the fixnum (char-code (char-upcase (char name 1)))) #x40)))
             (let* ((n 0))
               (dotimes (i namelen (code-char (logand n (1- char-code-limit))))
                 (let* ((code (the fixnum (- (the fixnum (char-code (char name i)))
                                             (char-code #\0)))))
                   (declare (fixnum code))
                   (if (and (>= code 0)
                            (<= code 7))
                     (setq n (logior code (the fixnum (ash n 3))))
                     (return))))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant wsp #.(let ((str (make-string 6  :element-type 'base-char)))
                      (set-schar str 0 #\Space)
                      (set-schar str 1 #\^I)
                      (set-schar str 2 #\^L)
                      (set-schar str 3 #\^@)
                      (set-schar str 4 #\^J)
                      (set-schar str 5 (%code-char #xCA))
                      str))

(defconstant wsp&cr #.(let ((str (make-string 7 :element-type 'base-char)))
                        (set-schar str 0 #\Space)
                        (set-schar str 1 #\^M)
                        (set-schar str 2 #\^I)
                        (set-schar str 3 #\^L)
                        (set-schar str 4 #\^@)
                        (set-schar str 5 #\^J)
                        (set-schar str 6 (%code-char #xCA))
                        str))
)

(defun whitespacep (char)
  (eql $cht_wsp (%character-attribute char (rdtab.ttab *readtable*))))
	   
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Readtables					;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Readtable = istructure with data [1] type-table and [2] macro-char-alist
;;; Type-table is a 256 byte ivector with a type byte for each char.
;;; macro-char-alist is a list of (char . defn).  The defn is either a
;;; cons of (#'read-dispatch . macro-char-alist) for
;;; dispatch macros, or it is a function or a symbol to call for simple macros.

(defun readtablep (object) (istruct-typep object 'readtable)) 

(defun readtable-arg (object)
  (if (null object) (setq object *readtable*))
  (unless (istruct-typep object 'readtable)
    (report-bad-arg object 'readtable))
  object)

(eval-when (:compile-toplevel :execute)
(def-accessors %svref
  token.string
  token.ipos
  token.opos
  token.len
)

(defmacro with-token-buffer ((name) &body body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    `(let* ((,name (vector (%get-token-string 16) 0 0 16 nil)))
       (declare (dynamic-extent ,name))
       (unwind-protect
         (locally ,@decls ,@body)
         (%return-token-string ,name)))))
)

(defun read-dispatch (stream char)
  (let* ((info (cdr (assq char (rdtab.alist *readtable*)))))
    (with-token-buffer (tb)
      (let* ((subchar nil)
             (numarg nil))
        (loop
            (if (digit-char-p (setq subchar (%read-char-no-eof stream)))
                (%add-char-to-token subchar tb)
                (return (setq subchar (char-upcase subchar) 
                              numarg (%token-to-number tb 10)))))
        (let* ((dispfun (cdr (assq subchar (cdr info)))))     ; <== WAS char
          (if dispfun
              (funcall dispfun stream subchar numarg)
              (signal-reader-error stream "Undefined character ~S in a ~S dispatch macro." subchar char)))))))

;;; This -really- gets initialized later in the file
(defvar %initial-readtable%
  (let* ((ttab (make-array 256 :element-type '(signed-byte 8)))
         (macs `((#\# . (,#'read-dispatch))))
         (case :upcase))
    (dotimes (i 256) (declare (fixnum i))(uvset ttab i $cht_cnst))
    (dotimes (ch (1+ (char-code #\Space)))
      (uvset ttab ch $cht_wsp))
    (uvset ttab (char-code #\\) $cht_sesc)
    (uvset ttab (char-code #\|) $cht_mesc)
    (uvset ttab (char-code #\#) $cht_ntmac)
    (uvset ttab (char-code #\Backspace) $cht_ill)
    (uvset ttab (char-code #\Rubout) $cht_ill)
    (%istruct 'readtable ttab macs case)))

(setq *readtable* %initial-readtable%)
(def-standard-initial-binding *readtable* )
(queue-fixup (setq %initial-readtable% (copy-readtable *readtable*)))

(defun copy-readtable (&optional (from *readtable*) to)
  (setq from (if from (readtable-arg from)  %initial-readtable%))
  (setq to (if to 
             (readtable-arg to)
             (%istruct 'readtable
                        (make-array 256 :element-type '(signed-byte 8))
                         nil (rdtab.case from))))
  (setf (rdtab.alist to) (copy-tree (rdtab.alist from)))
  (setf (rdtab.case to) (rdtab.case from))
  (let* ((fttab (rdtab.ttab from))
         (tttab (rdtab.ttab to)))
    (dotimes (i 256 to)
      (setf (uvref tttab i) (uvref fttab i)))))

(declaim (inline %character-attribute))

(defun %character-attribute (char attrtab)
  (declare (character char)
           (type (simple-array (signed-byte 8) (*)) attrtab)
           (optimize (speed 3) (safety 0)))
  (let* ((code (char-code char)))
    (declare (fixnum code))
    (aref attrtab code)))

;;; returns: (values attrib <aux-info>), where
;;;           <aux-info> = (char . fn), if terminating macro
;;;                      = (char . (fn . dispatch-alist)), if dispatching macro
;;;                      = nil otherwise


(defun %get-readtable-char (char &optional (readtable *readtable*))
  (setq char (require-type char 'character))
  (let* ((attr (%character-attribute char (rdtab.ttab readtable))))
    (declare (fixnum attr))
    (values attr (if (logbitp $cht_macbit attr) (assoc char (rdtab.alist readtable))))))


(defun set-syntax-from-char (to-char from-char &optional to-readtable from-readtable)
  "Causes the syntax of TO-CHAR to be the same as FROM-CHAR in the
  optional readtable (defaults to the current readtable). The
  FROM-TABLE defaults to the standard Lisp readtable when NIL."
  (setq to-char (require-type to-char 'base-char))
  (setq from-char (require-type from-char 'base-char))
  (setq to-readtable (readtable-arg to-readtable))
  (setq from-readtable (readtable-arg (or from-readtable %initial-readtable%)))
  (multiple-value-bind (from-attr from-info) (%get-readtable-char from-char from-readtable)
    (let* ((new-tree (copy-tree (cdr from-info)))
           (old-to-info (nth-value 1 (%get-readtable-char to-char to-readtable))))
      (without-interrupts
       (if from-info
         (if old-to-info
           (setf (cdr old-to-info) new-tree)
           (push (cons to-char new-tree) (rdtab.alist to-readtable)))
         (if old-to-info
           (setf (rdtab.alist to-readtable) (delq old-to-info (rdtab.alist to-readtable)))))
       (if (and (= from-attr $cht_cnst)
                (member to-char '(#\Newline #\Linefeed #\Page #\Return
                                  #\Space #\Tab #\Backspace #\Rubout)))
           (setf (uvref (rdtab.ttab to-readtable) (char-code to-char)) $cht_ill)
           (setf (uvref (rdtab.ttab to-readtable) (char-code to-char)) from-attr)))
      t)))

(defun get-macro-character (char &optional readtable)
  "Return the function associated with the specified CHAR which is a macro
  character, or NIL if there is no such function. As a second value, return
  T if CHAR is a macro character which is non-terminating, i.e. which can
  be embedded in a symbol name."
  (setq readtable (readtable-arg readtable))
  (multiple-value-bind (attr info) (%get-readtable-char char readtable)
    (declare (fixnum attr) (list info))
    (let* ((def (cdr info)))
      (values (if (consp def) (car def) def)
              (= attr $cht_ntmac)))))

(defun set-macro-character (char fn &optional non-terminating-p readtable)
  "Causes CHAR to be a macro character which invokes FUNCTION when seen
   by the reader. The NON-TERMINATINGP flag can be used to make the macro
   character non-terminating, i.e. embeddable in a symbol name."
  (setq char (require-type char 'base-char))
  (setq readtable (readtable-arg readtable))
  (when fn
    (unless (or (symbolp fn) (functionp fn))
      (setq fn (require-type fn '(or symbol function)))))
  (let* ((info (nth-value 1 (%get-readtable-char char readtable))))
    (declare (list info))
    (without-interrupts
     (setf (uvref (rdtab.ttab readtable) (char-code char))
           (if (null fn) $cht_cnst (if non-terminating-p $cht_ntmac $cht_tmac)))
     (if (and (null fn) info)
       (setf (rdtab.alist readtable) (delete info (rdtab.alist readtable) :test #'eq)) 
       (if (null info)
         (push (cons char fn) (rdtab.alist readtable))
         (let* ((def (cdr info)))
           (if (atom def)
             (setf (cdr info) fn)         ; Non-dispatching
             (setf (car def) fn))))))     ; Dispatching
    t))

(defun readtable-case (readtable)
  (unless (istruct-typep readtable 'readtable)
    (report-bad-arg readtable 'readtable))
  (let* ((case (rdtab.case (readtable-arg readtable))))
    (if (symbolp case)
      case
      (%car (rassoc case (readtable-case-keywords) :test #'eq)))))

(defun %set-readtable-case (readtable case)
  (unless (istruct-typep readtable 'readtable)
    (report-bad-arg readtable 'readtable))
  (check-type case (member :upcase :downcase :preserve :invert))
  (setf (rdtab.case (readtable-arg readtable)) case))
  
(defsetf readtable-case %set-readtable-case)

(defun make-dispatch-macro-character (char &optional non-terminating-p readtable)
  "Cause CHAR to become a dispatching macro character in readtable (which
   defaults to the current readtable). If NON-TERMINATING-P, the char will
   be non-terminating."
  (setq readtable (readtable-arg readtable))
  (setq char (require-type char 'base-char))
  (let* ((info (nth-value 1 (%get-readtable-char char readtable))))
    (declare (list info))
    (without-interrupts
     (setf (uvref (rdtab.ttab readtable) (char-code char))
           (if non-terminating-p $cht_ntmac $cht_tmac))
     (if info
       (rplacd (cdr info) nil)
       (push (cons char (cons #'read-dispatch nil)) (rdtab.alist readtable)))))
  t)

(defun get-dispatch-macro-character (disp-ch sub-ch &optional (readtable *readtable*))
  "Return the macro character function for SUB-CHAR under DISP-CHAR
   or NIL if there is no associated function."
  (setq readtable (readtable-arg (or readtable %initial-readtable%)))
  (setq disp-ch (require-type disp-ch 'base-char))
  (setq sub-ch (char-upcase (require-type sub-ch 'base-char)))
  (unless (digit-char-p sub-ch 10)
    (let* ((def (cdr (nth-value 1 (%get-readtable-char disp-ch readtable)))))
      (if (consp (cdr def))
        (cdr (assq sub-ch (cdr def)))
        (error "~A is not a dispatching macro character in ~s ." disp-ch readtable)))))

(defun set-dispatch-macro-character (disp-ch sub-ch fn &optional readtable)
  "Cause FUNCTION to be called whenever the reader reads DISP-CHAR
   followed by SUB-CHAR."
  (setq readtable (readtable-arg readtable))
  (setq disp-ch (require-type disp-ch 'base-char))
  (setq sub-ch (char-upcase (require-type sub-ch 'base-char)))
  (when (digit-char-p sub-ch 10)
    (error "subchar can't be a decimal digit - ~a ." sub-ch))
  (let* ((info (nth-value 1 (%get-readtable-char disp-ch readtable)))
         (def (cdr info)))
    (declare (list info))
    (unless (consp def)
      (error "~A is not a dispatching macro character in ~s ." disp-ch readtable))
    (let* ((alist (cdr def))
           (pair (assq sub-ch alist)))
      (if pair
        (setf (cdr pair) fn)
        (push (cons sub-ch fn) (cdr def))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Reader					;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *read-eval* t "When nil, #. signals an error.")
(defvar %read-objects% nil)
(defvar %keep-whitespace% nil)




(def-standard-initial-binding %token-strings% (%cons-pool nil))


(defun %return-token-string (token)
  (let* ((str (token.string token))
         (pool %token-strings%))
    (setf (token.string token) nil)
    (without-interrupts
     (setf (pool.data pool)
           (cheap-cons str (pool.data pool))))))

;;;Look for an exact match, else create a simple-string.
(defun %get-token-string (len)
  (declare (fixnum len))
  (without-interrupts
   (do* ((pool %token-strings%)
         (head (cons nil (pool.data pool)))
         (prev head next)
         (next (cdr prev) (cdr next)))
        ((null next)
         (make-string len :element-type 'base-char))
     (declare (dynamic-extent head)
              (list head prev next))
     (let* ((s (car next)))
       (when (= len (length s))
         (rplacd prev (cdr next))
         (setf (pool.data pool) (cdr head))
         (free-cons next)
         (return s))))))

(defun %extend-token-string (token)
  (let* ((old-string (token.string token))
         (old-length (token.len token)))
    (declare (fixnum old-length))
    (let* ((new-length (the fixnum (ash old-length 1)))
           (new-string (%get-token-string new-length)))
      (dotimes (i old-length)
        (setf (%schar new-string i)
              (%schar old-string i)))
      (%return-token-string token)
      (setf (token.string token) new-string
            (token.len token) new-length)
      token)))

(defun %add-char-to-token (char token)
  (let* ((len (token.len token))
         (opos (token.opos token)))
    (declare (fixnum len opos))
    (when (= opos len)
      (%extend-token-string token))
    (setf (token.opos token) (the fixnum (1+ opos))
          (%schar (token.string token) opos) char)))

(defun %string-from-token (token)
  (let* ((opos (token.opos token))
         (ipos (token.ipos token))
         (tstr (token.string token))
         (len (the fixnum (- opos ipos)))
         (string (make-string len :element-type 'base-char)))
    (do* ((k 0 (1+ k))
          (i ipos (1+ i)))
         ((= i opos) string)
      (declare (fixnum i k))
      (setf (%schar string k) (%schar tstr i)))))

(defun %next-token-char (token)
  (let* ((ipos (token.ipos token)))
    (declare (fixnum ipos))
    (when (< ipos (the fixnum (token.opos token)))
      (setf (token.ipos token) (the fixnum (1+ ipos)))
      (%schar (token.string token) ipos))))

      
(defun input-stream-arg (stream)
  (cond ((null stream) *standard-input*)
        ((eq stream t) *terminal-io*)
        ;Otherwise, let ASK complain...
        (t stream)))

(defun %read-char-no-eof (stream)
  (read-char stream))

(defun %next-char-and-attr (stream &optional (attrtab (rdtab.ttab *readtable*)))
  (let* ((ch (read-char stream nil :eof)))
    (if (eq ch :eof)
      (values nil nil)
      (values ch (%character-attribute ch attrtab)))))

(defun %next-non-whitespace-char-and-attr (stream)
  (let* ((attrtab (rdtab.ttab *readtable*)))
    (loop
      (multiple-value-bind (ch attr) (%next-char-and-attr stream attrtab)
        (if (null ch)
          (return (values nil nil))
          (unless (eql attr $cht_wsp)
            (return (values ch attr))))))))

(defun %next-char-and-attr-no-eof (stream &optional (attrtab (rdtab.ttab *readtable*)))
  (let* ((ch (%read-char-no-eof stream)))
    (values ch (%character-attribute ch attrtab))))

(defun %next-non-whitespace-char-and-attr-no-eof (stream)
  (let* ((attrtab (rdtab.ttab *readtable*)))
    (loop
      (multiple-value-bind (ch attr) (%next-char-and-attr-no-eof stream attrtab)
        (declare (fixnum attr))
        (unless (= attr $cht_wsp)
          (return (values ch attr)))))))

;;; "escapes" is a list of escaped character positions, in reverse order
(defun %casify-token (token escapes)
  (let* ((case (readtable-case *readtable*))
         (opos (token.opos token))
         (string (token.string token)))
    (declare (fixnum opos))
    (if (and (null escapes) (eq case :upcase))          ; Most common case, pardon the pun
      ; %strup is faster - boot probs tho
      (dotimes (i opos)
        (setf (%schar string i) (char-upcase (%schar string i))))
      (unless (eq case :preserve)
        (when (eq case :invert)
          (let* ((lower-seen nil)
                 (upper-seen nil))
            (do* ((i (1- opos) (1- i))
                  (esclist escapes)
                  (nextesc (if esclist (pop esclist) -1)))
                 ((< i 0) (if upper-seen (unless lower-seen (setq case :downcase))
                                         (when lower-seen (setq case :upcase))))
              (declare (fixnum i nextesc))
              (if (= nextesc i)
                (setq nextesc (if esclist (pop esclist) -1))
                (let* ((ch (%schar string i)))
                  (if (upper-case-p ch)
                    (setq upper-seen t)
                    (if (lower-case-p ch)
                      (setq lower-seen t))))))))
        (if (eq case :upcase)
          (do* ((i (1- opos) (1- i))
                  (nextesc (if escapes (pop escapes) -1)))
               ((< i 0))
            (declare (fixnum i nextesc))
            (if (= nextesc i)
                (setq nextesc (if escapes (pop escapes) -1))
                (setf (%schar string i) (char-upcase (%schar string i)))))
          (if (eq case :downcase)
            (do* ((i (1- opos) (1- i))
                  (nextesc (if escapes (pop escapes) -1)))
               ((< i 0))
            (declare (fixnum i nextesc))
            (if (= nextesc i)
                (setq nextesc (if escapes (pop escapes) -1))
                (setf (%schar string i) (char-downcase (%schar string i)))))))))))

;;; MCL's reader has historically treated ||:foo as a reference to the
;;; symbol FOO in the package which has the null string as its name.
;;; Some other implementations treat it as a keyword.  This takes an
;;; argument indicating whether or not something was "seen" before the
;;; first colon was read, even if that thing caused no characters to
;;; be added to the token.

(defun %token-package (token colonpos seenbeforecolon)
  (if colonpos
    (if (and (eql colonpos 0) (not seenbeforecolon))
      *keyword-package*
      (let* ((string (token.string token)))
        (or (%find-pkg string colonpos)
            (%kernel-restart $XNOPKG (subseq string 0 colonpos)))))
    *package*))

;;; Returns 4 values: reversed list of escaped character positions,
;;; explicit package (if unescaped ":" or "::") or nil, t iff any
;;; non-dot, non-escaped chars in token, and t if either no explicit
;;; package or "::"

(defun %collect-xtoken (token stream 1stchar)
  (let* ((escapes ())
         (nondots nil)
         (explicit-package *read-suppress*)
         (double-colon t)
         (multi-escaped nil))
    (do* ((attrtab (rdtab.ttab *readtable*))
          (char 1stchar (read-char stream nil :eof )))
         ((eq char :eof))
      (flet ((add-note-escape-pos (char token escapes)
               (push (token.opos token) escapes)
               (%add-char-to-token char token)
               escapes))
        (let* ((attr (%character-attribute char attrtab)))
          (declare (fixnum attr))
          (when (or (= attr $cht_tmac)
                    (= attr $cht_wsp))
            (when (or (not (= attr $cht_wsp)) %keep-whitespace%)
              (unread-char char stream))
            (return ))
          (if (= attr $cht_ill)
              (signal-reader-error stream "Illegal character ~S." char)
              (if (= attr $cht_sesc)
                  (setq nondots t 
                        escapes (add-note-escape-pos (%read-char-no-eof stream) token escapes))
                  (if (= attr $cht_mesc)
                      (progn 
                        (setq nondots t)
                        (loop
                            (multiple-value-bind (nextchar nextattr) (%next-char-and-attr-no-eof stream attrtab)
                              (declare (fixnum nextattr))
                              (if (= nextattr $cht_mesc) 
                                  (return (setq multi-escaped t))
                                  (if (= nextattr $cht_sesc)
                                      (setq escapes (add-note-escape-pos (%read-char-no-eof stream) token escapes))
                            (setq escapes (add-note-escape-pos nextchar token escapes)))))))
                  (let* ((opos (token.opos token)))         ; Add char to token, note 1st colonpos
                    (declare (fixnum opos))
                    (if (and (eq char #\:)       ; (package-delimiter-p char ?)
                             (not explicit-package))
                      (let* ((nextch (%read-char-no-eof stream)))
                        (if (eq nextch #\:)
                          (setq double-colon t)
                          (progn
			    (unread-char nextch stream)
                            (setq double-colon nil)))
                        (%casify-token token escapes)
                        (setq explicit-package (%token-package token opos nondots)
                              nondots t
                              escapes nil)
                        (setf (token.opos token) 0))
                      (progn
                        (unless (eq char #\.) (setq nondots t))
                        (%add-char-to-token char token))))))))))
        (values (or escapes multi-escaped) (if *read-suppress* nil explicit-package) nondots double-colon)))
          
(defun %validate-radix (radix)
  (if (and (typep radix 'fixnum)
           (>= (the fixnum radix) 2)
           (<= (the fixnum radix) 36))
    radix
    (progn
      (check-type radix (integer 2 36))
      radix)))

(defun %token-to-number (token radix &optional no-rat)
  (new-numtoken (token.string token) (token.ipos token) (token.opos token) radix no-rat))

;;; If we're allowed to have a single "." in this context, DOT-OK is some distinguished
;;; value that's returned to the caller when exactly one dot is present.
(defun %parse-token (stream firstchar dot-ok)
  (with-token-buffer (tb)
    (multiple-value-bind (escapes explicit-package nondots double-colon) (%collect-xtoken tb stream firstchar)
      (unless *read-suppress* 
        (let* ((string (token.string tb))
               (len (token.opos tb)))
          (declare (fixnum len ndots nondots))
          (if (not nondots)
            (if (= len 1)
              (or dot-ok
                  (signal-reader-error stream "Dot context error."))
              (signal-reader-error stream "Illegal symbol syntax."))
            ; Something other than a buffer full of dots.  Thank god.
            (let* ((num (if (null escapes)
                            (handler-case
                                (%token-to-number tb (%validate-radix *read-base*))
                              (arithmetic-error (c)
                                (error 'impossible-number
                                       :stream stream
                                       :token (%string-from-token tb)
                                       :condition c))))))
              (if (and num (not explicit-package))
                num
                (if (and (zerop len) (null escapes))
                  (%err-disp $XBADSYM)
                  (progn                  ; Muck with readtable case of extended token.
                    (%casify-token tb (unless (atom escapes) escapes))
                    (let* ((pkg (or explicit-package *package*)))
                      (if (or double-colon (eq pkg *keyword-package*))
                        (without-interrupts
                         (multiple-value-bind (symbol access internal-offset external-offset)
                                              (%find-symbol string len pkg)
                           (if access
                             symbol
                             (%add-symbol (%string-from-token tb) pkg internal-offset external-offset))))
                        (multiple-value-bind (found symbol) (%get-htab-symbol string len (pkg.etab pkg))
                          (if found
                            symbol
                            (%err-disp $XNOESYM (%string-from-token tb) pkg)))))))))))))))
                    
#|
(defun %parse-token-test (string &key dot-ok (case (readtable-case *readtable*)))
  (let* ((stream (make-string-input-stream string))
         (oldcase (readtable-case *readtable*)))
    (unwind-protect
      (progn
        (setf (readtable-case *readtable*) case) 
        (%parse-token stream (read-char stream t) dot-ok))
      (setf (readtable-case *readtable*) oldcase))))

(%parse-token-test "ABC")
(%parse-token-test "TRAPS::_DEBUGGER")
(%parse-token-test "3.14159")
(ignore-errors (%parse-token-test "BAD-PACKAGE:WORSE-SYMBOL"))
(ignore-errors (%parse-token-test "CCL::"))
(%parse-token-test "TRAPS::_debugger" :case :preserve)
(%parse-token-test ":foo")
|#

;;; firstchar must not be whitespace.
;;; People who think that there's so much overhead in all of
;;; this (multiple-value-list, etc.) should probably consider
;;; rewriting those parts of the CLOS and I/O code that make
;;; using things like READ-CHAR impractical ...
(defun %parse-expression (stream firstchar dot-ok)
  (let* ((readtable *readtable*)
         (attrtab (rdtab.ttab readtable)))
    (let* ((attr (%character-attribute firstchar attrtab)))
      (declare (fixnum attr))
      (if (= attr $cht_ill)
          (signal-reader-error stream "Illegal character ~S." firstchar))
      (let* ((vals (multiple-value-list 
                    (if (not (logbitp $cht_macbit attr))
                      (%parse-token stream firstchar dot-ok)
                      (let* ((def (cdr (assq firstchar (rdtab.alist readtable)))))
                        (cond ((null def))
                              ((atom def)
                               (funcall def stream firstchar))
                              #+no ; include if %initial-readtable% broken (see above)
                              ((and (consp (car def))
                                    (eq (caar def) 'function))
                               (funcall (cadar def) stream firstchar))
                              ((functionp (car def))
                               (funcall (car def) stream firstchar))
                              (t (break "Bogus default dispatch fn: ~S" (car def)) nil)))))))
        (declare (dynamic-extent vals)
                 (list vals))
        (if (null vals)
            (values nil nil)
            (values (car vals) t))))))


#|
(defun %parse-expression-test (string)
  (let* ((stream (make-string-input-stream string)))
    (%parse-expression stream (read-char stream t) nil)))

(%parse-expression-test ";hello")
(%parse-expression-test "#'cdr")
(%parse-expression-test "#+foo 1 2")

|#

(defun %read-list-expression (stream dot-ok &optional (termch #\)))
  (loop
      (let* ((firstch (%next-non-whitespace-char-and-attr-no-eof stream)))
        (if (eq firstch termch)
            (return (values nil nil))
            (multiple-value-bind (val val-p) (%parse-expression stream firstch dot-ok)
              (if val-p
                  (return (values val t))))))))


(defun read-list (stream &optional nodots (termch #\)))
  (let* ((dot-ok (cons nil nil))
         (head (cons nil nil))
         (tail head))
    (declare (dynamic-extent dot-ok head)
             (list head tail))
    (if nodots (setq dot-ok nil))
    (multiple-value-bind (firstform firstform-p)
        (%read-list-expression stream dot-ok termch)
      (when firstform-p
        (if (and dot-ok (eq firstform dot-ok))       ; just read a dot
            (signal-reader-error stream "Dot context error."))
        (rplacd tail (setq tail (cons firstform nil)))
        (loop
          (multiple-value-bind (nextform nextform-p)
              (%read-list-expression stream dot-ok termch)
            (if (not nextform-p) (return))
            (if (and dot-ok (eq nextform dot-ok))    ; just read a dot
                (if (multiple-value-bind (lastform lastform-p)
                        (%read-list-expression stream nil termch)
                      (and lastform-p
                           (progn (rplacd tail lastform) 
                                  (not (nth-value 1 (%read-list-expression stream nil termch))))))
                    (return)
                    (signal-reader-error stream "Dot context error."))
                (rplacd tail (setq tail (cons nextform nil))))))))
    (cdr head)))

#|
(defun read-list-test (string &optional nodots)
  (read-list (make-string-input-stream string) nodots))

(read-list-test ")")
(read-list-test "a b c)" t)
(read-list-test "a b ;hello
c)" t)

|#

(set-macro-character
 #\(
 #'(lambda (stream ignore)
     (declare (ignore ignore))
     (read-list stream nil #\))))

(set-macro-character 
 #\' 
 (nfunction |'-reader| 
            (lambda (stream ignore)
              (declare (ignore ignore))
              `(quote ,(read stream t nil t)))))

(defparameter *alternate-line-terminator*
    #+darwinppc-target #\Return
    #-darwinppc-target nil
    "This variable is currently only used by the standard reader macro
function for #\; (single-line comments); that function reads successive
characters until EOF, a #\NewLine is read, or a character EQL to the value
of *alternate-line-terminator* is read. In OpenMCL for Darwin, the value
of this variable is initially #\Return ; in OpenMCL for LinuxPPC, it's
initially NIL.")
	     
(set-macro-character
 #\;
 (nfunction |;-reader|
            (lambda (stream ignore)
              (declare (ignore ignore))
              (let* ((ch nil))
                (loop 
                    (if (or (eq :eof (setq ch (read-char stream nil :eof)))
                            (eq ch #\NewLine)
			    (eq ch *alternate-line-terminator*))
                        (return (values))))))))

(defun read-string (stream termch)
  (with-token-buffer (tb)
    (do* ((attrs (rdtab.ttab *readtable*))
          (ch (%read-char-no-eof stream)
              (%read-char-no-eof stream)))
         ((eq ch termch)
          (%string-from-token tb))
      (if (= (the fixnum (%character-attribute ch attrs)) $CHT_SESC)
          (setq ch (%read-char-no-eof stream)))
      (%add-char-to-token ch tb))))

(set-macro-character #\" #'read-string)

(defparameter *ignore-extra-close-parenthesis* nil)

(set-macro-character 
 #\)
 #'(lambda (stream ch)
     (let* ((pos (if (typep stream 'file-stream)
                     (file-position stream))))
       (if *ignore-extra-close-parenthesis*
           (warn "Ignoring extra \"~c\" ~@[near position ~d~] on ~s ." ch pos stream)
           (signal-reader-error stream "Unmatched ')' ~@[near position ~d~]." pos)))))




(eval-when (:load-toplevel)             ; But not when mousing around!
  (make-dispatch-macro-character #\# t))


(set-dispatch-macro-character
 #\#
 #\(
 (nfunction 
  |#(-reader| 
  (lambda (stream subchar numarg)
    (declare (ignore subchar))
    (if (or (null numarg) *read-suppress*)
      (let* ((lst (read-list stream t))
             (len (length lst))
             (vec (make-array len)))
        (declare (list lst) (fixnum len) (simple-vector vec))
        (dotimes (i len vec)
          (setf (svref vec i) (pop lst))))
      (locally
        (declare (fixnum numarg))
        (do* ((vec (make-array numarg))
              (lastform)
              (i 0 (1+ i)))
             ((multiple-value-bind (form form-p) (%read-list-expression stream nil)
                (if form-p
                  (setq lastform form)
                  (unless (= i numarg)
                      (if (= i 0) 
                        (%err-disp $XARROOB -1 vec)
                        (do* ((j i (1+ j)))
                             ((= j numarg))
                          (declare (fixnum j))
                          (setf (svref vec j) lastform)))))
                (not form-p))
              vec)
          (declare (fixnum i))
          (setf (svref vec i) lastform)))))))

(defun %read-rational (stream subchar radix)
  (declare (ignore subchar))
  (with-token-buffer (tb)
      (multiple-value-bind (escapes xpackage)
                           (%collect-xtoken tb stream (%next-non-whitespace-char-and-attr-no-eof stream))
        (unless *read-suppress*
          (let* ((val (%token-to-number tb radix)))
          (or (and (null escapes)
                   (null xpackage)
                   (typep val 'rational)
                   val)
              (%err-disp $xbadnum)))))))

(defun require-numarg (subchar numarg)
  (or numarg *read-suppress*
      (error "Numeric argument required for #~A reader macro ." subchar)))

(defun require-no-numarg (subchar numarg)
  (if (and numarg (not *read-suppress*))
      (error "Spurious numeric argument in #~D~A reader macro ." numarg subchar)))

(defun read-eval (stream subchar numarg)
  (require-no-numarg subchar numarg)
  (if *read-eval*
    (let* ((exp (%read-list-expression stream nil)))
      (unless *read-suppress*
        (eval exp)))
    (signal-reader-error stream "#. reader macro invoked when ~S is false ."
                         '*read-eval*)))

(set-dispatch-macro-character 
 #\# 
 #\C
 #'(lambda (stream char arg &aux form)
     (require-no-numarg char arg )
     (setq form (read stream t nil t))
     (unless *read-suppress* (apply #'complex form))))

(set-dispatch-macro-character 
 #\#
 #\.
 #'read-eval)

;;; This has been deprecated.  Why not nuke it ?
#-ansi-cl
(set-dispatch-macro-character
 #\#
 #\,
 #'(lambda (stream subchar numarg)
     (let* ((sharp-comma-token *reading-for-cfasl*))
       (if (or *read-suppress* (not *compiling-file*) (not sharp-comma-token))
         (read-eval stream subchar numarg)
         (progn
           (require-no-numarg subchar numarg)
           (list sharp-comma-token (read stream t nil t)))))))

(set-dispatch-macro-character
 #\#
 #\:
 #'(lambda (stream subchar numarg)
     (require-no-numarg subchar numarg)
     (if (not *read-suppress*)
         (multiple-value-bind (firstch attr) (%next-non-whitespace-char-and-attr-no-eof stream)
           (declare (fixnum attr))
           (with-token-buffer (tb)
             (if (or (= attr $CHT_ILL)
                     (logbitp $cht_macbit attr)
                     (multiple-value-bind (escapes explicit-package nondots) (%collect-xtoken tb stream firstch)
                       (declare (ignore nondots))
                       (%casify-token tb (unless (atom escapes) escapes))
                       (or explicit-package
                           (and (not escapes)
                                (%token-to-number tb (%validate-radix *read-base*))))))
               (%err-disp $XBADSYM)
               (make-symbol (%string-from-token tb)))))
         (progn
           (%read-list-expression stream nil)
           nil))))

(set-dispatch-macro-character 
 #\# 
 #\b
 #'(lambda (stream subchar numarg)
     (require-no-numarg subchar numarg)
     (%read-rational stream subchar 2)))

(set-dispatch-macro-character 
 #\# 
 #\o
 #'(lambda (stream subchar numarg)
     (require-no-numarg subchar numarg)
     (%read-rational stream subchar 8)))

(set-dispatch-macro-character 
 #\# 
 #\x
 #'(lambda (stream subchar numarg)
     (require-no-numarg subchar numarg)
     (%read-rational stream subchar 16)))

(set-dispatch-macro-character 
 #\# 
 #\r
 #'(lambda (stream subchar numarg)
     (unless *read-suppress*
       (require-numarg subchar numarg)
       (check-type numarg (integer 2 36)))
     (%read-rational stream subchar numarg)))

(set-dispatch-macro-character
 #\#
 #\'
 (nfunction |#'-reader| 
            (lambda (stream subchar numarg)
              (require-no-numarg subchar numarg)
              `(function ,(read stream t nil t)))))

(set-dispatch-macro-character
 #\#
 #\|
 (nfunction |#\|-reader| 
            (lambda (stream subchar numarg)
              (require-no-numarg subchar numarg)
              (do* ((lastch nil ch)
                    (ch )
                    (level 1))
                   ((= level 0) (values))
                (declare (fixnum level))
                (setq ch (%read-char-no-eof stream))
                (if (and (eq ch #\|)
                         (eq lastch #\#))
                    (progn 
                      (setq ch nil)
                      (incf level))
                    (if (and (eq ch #\#)
                             (eq lastch #\|))
                        (progn 
                          (setq ch nil)
                          (decf level))))))))

(defun %unreadable (stream description)
  (signal-reader-error stream "~S encountered." description))

(set-dispatch-macro-character
 #\#
 #\<
 #'(lambda (stream &rest ignore)
     (declare (ignore ignore))
     (%unreadable stream "#<")))

(dolist (ch '(#\null #\tab #\linefeed #\page #\return #\space #\312))
  (set-dispatch-macro-character
   #\#
   ch
   #'(lambda (stream &rest ignore)
       (declare (ignore ignore))
       (%unreadable stream "#<whitespace>"))))

(set-dispatch-macro-character
 #\#
 #\)
 #'(lambda (stream &rest ignore)
     (declare (ignore ignore))
     (%unreadable stream "#)")))

(set-dispatch-macro-character
 #\#
 #\\
 #'(lambda (stream subchar numarg)
     (require-no-numarg subchar numarg)
     (with-token-buffer (tb)
       (%collect-xtoken tb stream #\\)
       (unless *read-suppress*
         (let* ((str (%string-from-token tb)))
           (or (name-char str)
               (error "Unknown character name - \"~a\" ." str)))))))


     
;;;Since some built-in read macros used to use internal reader entry points
;;;for efficiency, we couldn't reliably offer a protocol for stream-dependent
;;;recursive reading.  So recursive reads always get done via tyi's, and streams
;;;only get to intercept toplevel reads.

(defun read (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (resident))
  (setq stream (input-stream-arg stream))
  (if recursive-p
    (%read-form stream 0 nil)
    (let ((%read-objects% nil) (%keep-whitespace% nil))
      (%read-form stream (if eof-error-p 0) eof-value))))

(defun read-preserving-whitespace (&optional stream (eof-error-p t) eof-value recursive-p)
  "Read from STREAM and return the value read, preserving any whitespace
   that followed the object."
  (setq stream (input-stream-arg stream))
  (if recursive-p
    (%read-form stream 0 nil)
    (let ((%read-objects% nil) (%keep-whitespace% t))
      (%read-form stream (if eof-error-p 0) eof-value))))


(defun read-delimited-list (char &optional stream recursive-p)
  "Read Lisp values from INPUT-STREAM until the next character after a
   value's representation is ENDCHAR, and return the objects as a list."
  (setq char (require-type char 'character))
  (setq stream (input-stream-arg stream))
  (let ((%keep-whitespace% nil))
    (if recursive-p
      (%read-form stream char nil)
      (let ((%read-objects% nil))
        (%read-form stream char nil)))))

(defun read-conditional (stream subchar int)
  (declare (ignore int))
  (cond ((eq subchar (read-feature stream)) (read stream t nil t))
        (t (let* ((*read-suppress* t))
             (read stream t nil t)
             (values)))))

(defun read-feature (stream)
  (let* ((f (let* ((*package* *keyword-package*))
              (read stream t nil t))))
    (labels ((eval-feature (form)
               (cond ((atom form) 
                      (member form *features*))
                     ((eq (car form) :not) 
                      (not (eval-feature (cadr form))))
                     ((eq (car form) :and) 
                      (dolist (subform (cdr form) t)
                        (unless (eval-feature subform) (return))))
                     ((eq (car form) :or) 
                      (dolist (subform (cdr form) nil)
                        (when (eval-feature subform) (return t))))
                     (t (%err-disp $XRDFEATURE form)))))
      (if (eval-feature f) #\+ #\-))))

(set-dispatch-macro-character #\# #\+ #'read-conditional)
(set-dispatch-macro-character #\# #\- #'read-conditional)




;;;arg=0 : read form, error if eof
;;;arg=nil : read form, eof-val if eof.
;;;arg=char : read delimited list
(defun %read-form (stream arg eof-val)
  (declare (resident))
  (check-type *readtable* readtable)
  (check-type *package* package)
  (if (and arg (not (eq arg 0)))
      (read-list stream nil arg)
      (loop
          (let* ((ch (%next-non-whitespace-char-and-attr stream)))
          (if (null ch)
            (if arg 
              (error 'end-of-file :stream stream)
              (return eof-val))
            (multiple-value-bind (form form-p) (%parse-expression stream ch nil)
              (if form-p
                 (if *read-suppress*
                     (return nil)
                     (return form)))))))))






;;;Until load backquote...
(set-macro-character #\`
  #'(lambda (stream char) (declare (ignore stream)) (%err-disp $xbadmac char)))
(set-macro-character #\, (get-macro-character #\`))



(set-dispatch-macro-character #\# #\P
 (qlfun |#P-reader| (stream char flags &aux path (invalid-string "Invalid flags (~S) for pathname ~S"))
   (declare (ignore char))
   (when (null flags) (setq flags 0))
   (unless (memq flags '(0 1 2 3 4))
     (unless *read-suppress* (report-bad-arg flags '(integer 0 4))))
   (setq path (read stream t nil t))
   (unless *read-suppress*
     (unless (stringp path) (report-bad-arg path 'string))
     (setq path (pathname path))
     (when (%ilogbitp 0 flags)
       (when (%pathname-type path) (error invalid-string flags path))
       (setf (%pathname-type path) :unspecific))
     (when (%ilogbitp 1 flags)
       (when (%pathname-name path) (error invalid-string flags path))
       (setf (%pathname-name path) ""))
     path)))






