;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2006 Clozure Associates and contributors.
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


;;; Unicode translation stuff, mostly in support of I/O.

(in-package "CCL")


(defvar *character-encodings* (make-hash-table :test #'eq))

(defun get-character-encoding (name)
  (gethash name *character-encodings*))

(defun (setf get-character-encoding) (new name)
  (setf (gethash name *character-encodings*) new))

(defstruct character-encoding
  (name ())                             ;canonical name
  (code-unit-size 8)                    ;in bits: 8, 16, 32
  (native-endianness t)                 ;if nil, need to swap 16,32-bit units
  (max-units-per-char 1)                ;usually 1-4

  ;; Returns NIL if the character can't be encoded, else writes it
  ;; to the stream and returns the number of units written.
  stream-encode-function                ;(CHAR WRITE-FUNCTION STREAM)
  
  ;; Returns a charcter or NIL, possibly calling a function to
  ;; obtain the next unit from a stream-like argument
  stream-decode-function                ;(1ST-UNIT NEXT-UNIT STREAM)

  ;; Returns NIL if the character can't be encoded, else sets 1 or
  ;; more units in a vector argument and returns a value 1 greater
  ;; than the index of the last unit written to the vector
  vector-encode-function                ;(CHAR VECTOR INDEX)
  
  ;; Returns a character and a value 1 greater than the last unit
  ;; index consumed from the vector argument, or NIL and the
  ;; argument index if the character can't be decoded.
  vector-decode-function                ;(VECTOR INDEX)
  
  ;; Sets one or more units in memory at the address denoted by
  ;; the pointer and idx arguments and returns (+ idx number of
  ;; units written to memory), else returns NIL if the character
  ;; can't be encoded.
  memory-encode-function                ;(CHAR POINTER INDEX)
  
  ;; Returns (as multiple values) the character encoded in memory
  ;; at the address denoted by the address and index args and the
  ;; sum of the index arg and the number of units consumed, else
  ;; NIL and the incoming index arg if the character can't be
  ;; encoded.  (Note that the index args are octet offsets and
  ;; the return values should be scaled appropriately.)
  memory-decode-function                ;(POINTER INDEX)
  
  ;; Returns the number of units needed to encode STRING between START and END.
  ;; Might return NIL if any character can't be encoded.
  units-in-string-function              ;(STRING &optional (START 0) (END (LENGTH STRING)))
  ;; Might return NIL if the encoding's bogus
  length-of-vector-encoding-function    ;(VECTOR &optional (START 0) (END (LENGTH VECTOR))) 
  ;; Might return NIL if the encoding's bogus
  length-of-memory-encoding-function    ;(POINTER NUNITS &optional (START 0))

  ;; Code units and character codes less than this value map to themselves
  (literal-char-code-limit 0)
  )


;;; N.B.  (ccl:nfunction <name> (lambda (...) ...)) is just  like
;;;       (cl:function (lambda (...) ...)), except that the resulting
;;; function will have "name" <name> (this is often helpful when debugging.)

(defmacro define-character-encoding (name &rest args &key &allow-other-keys)
  (setq name (intern (string name) "KEYWORD"))
  `(progn
    (setf (get-character-encoding ,name)
     (make-character-encoding :name ,name  ,@args))))

(defun encoding-name (encoding)
  (character-encoding-name (or encoding (get-character-encoding nil))))

;;; ISO-8859-1 is trivial, though of course it can't really encode characters
;;; whose CHAR-CODE is >= 256

(define-character-encoding :iso-8859-1
  :stream-encode-function
  (nfunction
   iso-8859-1-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (when (< code 256)
         (funcall write-function stream code)
         1))))
  :stream-decode-function
  (nfunction
   iso-8859-1-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (code-char 1st-unit)))
  :vector-encode-function
  (nfunction
   iso-8859-1-vector-encode
   (lambda (char vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (when (and (< code 256)
                  (< idx (the fixnum (length vector))))
         (setf (aref vector idx) code)
         1))))
  :vector-decode-function
  (nfunction
   iso-8859-1-vector-decode
   (lambda (vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (if (< idx (length vector))
       (values (code-char (aref vector idx))
               (the fixnum (1+ (the fixnum idx))))
       (values nil idx))))
  :memory-encode-function
  (nfunction
   iso-8859-1-memory-encode
   (lambda (char pointer idx)
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (when (< code 256)
         (setf (%get-unsigned-byte pointer idx) code)
         (1+ idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-1-memory-decode
   (lambda (pointer idx)
     (values (code-char (%get-unsigned-byte pointer idx))
             (the fixnum (1+ (the fixnum idx))))))
  :units-in-string-function
  (nfunction
   iso-8859-1-units-in-string
   (lambda (string &optional (start 0) (end (length string)))
     (when (>= end start)
       (do* ((i start (1+ i)))
            ((= i end) (- end start))
         (let* ((code (char-code (schar string i))))
           (declare (type (mod #x110000) code))
           (unless (< code 256) (return nil)))))))
  :length-of-vector-encoding-function
  (nfunction
   iso-8859-1-length-of-vector-encoding
   (lambda (vector &optional (start 0) (end (length vector)))
     (when (>= end start)
       (- end start))))
  :length-of-memory-encoding-function 
  (nfunction
   iso-8859-1-length-of-memory-encoding
   (lambda (pointer nunits &optional start)
     (declare (ignore pointer start))
     nunits))
  :literal-char-code-limit 256
  )

;;; Make :ISO-8859-1 the "null" encoding (not necessarily the default).
(setf (get-character-encoding nil)
      (get-character-encoding :iso-8859-1))




;;; Other 1-byte, fixed-width encodings.  Typically, codes in the range
;;; #x00-#x9f maps straight through, while codes #xa0-#xff select arbitrary
;;; Unicode characters that are commonly used in some locale.  (Sometimes
;;; the break is at #x80 instead of #xa0).


;;; Later.

;;; UTF-8.  Decoding checks for malformed sequences; it might be faster (and
;;; would certainly be simpler) if it didn't.
(define-character-encoding :utf-8
    :max-units-per-char 4
    :stream-encode-function
    (nfunction
     utf-8-stream-encode
     (lambda (char write-function stream)
       (let* ((code (char-code char)))
         (declare (type (mod #x110000) code))
         (cond ((< code #x80)
                (funcall write-function stream code)
                1)
               ((< code #x800)
                (let* ((y (ldb (byte 5 6) code))
                       (z (ldb (byte 6 0) code)))
                  (declare (fixnum y z))
                  (funcall write-function stream (logior #xc0 y))
                  (funcall write-function stream (logior #x80 z))
                  2))
               ((< code #x10000)
                (let* ((x (ldb (byte 4 12) code))
                       (y (ldb (byte 6 6) code))
                       (z (ldb (byte 6 0) code)))
                  (declare (fixnum x y z))
                  (funcall write-function stream (logior #xe0 x))
                  (funcall write-function stream (logior #x80 y))
                  (funcall write-function stream (logior #x80 z))
                  3))
               (t
                (let* ((w (ldb (byte 3 18) code))
                       (x (ldb (byte 6 12) code))
                       (y (ldb (byte 6 6) code))
                       (z (ldb (byte 6 0) code)))
                  (declare (fixnum w x y z))
                  (funcall write-function stream (logior #xf0 w))
                  (funcall write-function stream (logior #x80 x))
                  (funcall write-function stream (logior #x80 y))
                  (funcall write-function stream (logior #x80 z))
                  4))))))
    :stream-decode-function
    (nfunction
     utf-8-stream-decode
     (lambda (1st-unit next-unit-function stream)
       (declare (type (unsigned-byte 8) 1st-unit))
       (if (< 1st-unit #x80)
         (code-char 1st-unit)
         (when (>= 1st-unit #xc2)
           (let* ((s1 (funcall next-unit-function stream)))
             (if (eq s1 :eof)
               s1
               (locally
                   (declare (type (unsigned-byte 8) s1))
                 (if (< 1st-unit #xe0)
                   (if (< (the fixnum (logxor s1 #x80)) #x40)
                     (code-char
                      (logior
                       (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                       (the fixnum (logxor s1 #x80)))))
                   (let* ((s2 (funcall next-unit-function stream)))                 
                     (if (eq s2 :eof)
                       s2
                       (locally
                           (declare (type (unsigned-byte 8) s2))
                         (if (< s2 #xf0)
                           (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                                    (< (the fixnum (logxor s2 #x80)) #x40)
                                    (or (>= 1st-unit #xe1)
                                        (>= s1 #xa0)))
                             (code-char (the fixnum
                                          (logior (the fixnum
                                                    (ash (the fixnum (logand 1st-unit #xf))
                                                         12))
                                                  (the fixnum
                                                    (logior
                                                     (the fixnum
                                                       (ash (the fixnum (logand s1 #x3f))
                                                            6))
                                                     (the fixnum (logand s2 #x3f))))))))
                           (if (< 1st-unit #xf8)
                             (let* ((s3 (funcall next-unit-function stream)))
                               (if (eq s3 :eof)
                                 s3
                                 (locally
                                     (declare (type (unsigned-byte 8) s3))
                                   (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                                            (< (the fixnum (logxor s2 #x80)) #x40)
                                            (< (the fixnum (logxor s3 #x80)) #x40)
                                            (or (>= 1st-unit #xf1)
                                                (>= s1 #x90)))
                                     (code-char
                                      (logior
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logand 1st-unit 7)) 18))
                                          (the fixnum
                                            (ash (the fixnum (logxor s1 #x80)) 12))))
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logxor s2 #x80)) 6))
                                          (the fixnum (logxor s3 #x80)))))))))))))))))))))))
    :vector-encode-function
    (nfunction
     utf-8-vector-encode
     (lambda (char vector index)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (type index index)
                (optimize (speed 3) (safety 0)))
       (let* ((len (length vector))
              (code (char-code char)))
         (declare (type index len)
                  (type (mod #x110000) code))
         (if (< code #x80)
           (when (< index len)
             (setf (aref vector index) code)
             (the fixnum (+ index 1))
             (let* ((i1 (1+ index)))
               (declare (fixnum i1))
               (if (< code #x800)
                 (when (< i1 len)
                   (setf (aref vector index)
                         (logior #xc0 (the fixnum (ash code -6)))
                         (aref vector i1)
                         (logior #x80 (the fixnum (logand code #x3f))))
                   (the fixnum (+ i1 1)))
                 (let* ((i2 (1+ i1)))
                   (declare (fixnum i2))
                   (if (< code #x10000)
                     (when (< i2 len)
                       (setf (aref vector index)
                             (logior #xe0 (the fixnum (ash code -12)))
                             (aref vector i1)
                             (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6)))))
                             (aref vector i2)
                             (logior #x80 (the fixnum (logand code #x3f))))
                       (the fixnum (+ i2 1)))
                     (let* ((i3 (1+ i2)))
                       (declare (fixnum i3))
                       (when (< i3 len)
                         (setf (aref vector index)
                               (logior #xf0
                                       (the fixnum (logand #x7 (the fixnum (ash code -18)))))
                               (aref vector i1)
                               (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -12)))))
                               (aref vector i2)
                               (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6)))))
                               (aref vector i3)
                               (logand #x3f code))
                         (the fixnum (+ i3 1)))))))))))))
    :vector-decode-function
    (nfunction
     utf-8-vector-decode
     (lambda (vector idx)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (type index idx))
       (let* ((len (length vector)))
         (declare (fixnum len))
         (if (>= idx len)
           (values nil idx)
           (let* ((1st-unit (aref vector idx)))
             (declare (type (unsigned-byte 8) 1st-unit))
             (if (< 1st-unit #x80)
               (values (code-char 1st-unit) (the fixnum (1+ idx)))
               (if (>= 1st-unit #xc2)
                 (let* ((i1 (1+ idx)))
                   (declare (fixnum i1))
                   (if (>= i1 len)
                     (values nil idx)
                     (let* ((s1 (aref vector i1)))
                       (declare (type (unsigned-byte 8) s1))
                       (if (< 1st-unit #xe0)
                         (if (< (the fixnum (logxor s1 #x80)) #x40)
                           (values
                            (code-char
                             (logior
                              (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                              (the fixnum (logxor s1 #x80))))
                            (the fixnum (1+ i1)))
                           (values nil i1))
                         (let* ((i2 (1+ i1)))
                           (declare (fixnum i2))
                           (if (>= i2 len)
                             (values nil idx)
                             (let* ((s2 (aref vector i2)))
                               (declare (type (unsigned-byte 8) s2))
                               (if (< 1st-unit #xf0)
                                 (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                                          (< (the fixnum (logxor s2 #x80)) #x40)
                                          (or (>= 1st-unit #xe1)
                                              (>= s1 #xa0)))
                                   (values
                                    (code-char (the fixnum
                                                 (logior (the fixnum
                                                           (ash (the fixnum (logand 1st-unit #xf))
                                                                12))
                                                         (the fixnum
                                                           (logior
                                                            (the fixnum
                                                              (ash (the fixnum (logand s1 #x3f))
                                                                   6))
                                                            (the fixnum (logand s2 #x3f)))))))
                                    (the fixnum (1+ i2)))
                                   (values nil idx))
                                 (if (>= 1st-unit #xf8)
                                   (values nil idx)
                                   (let* ((i3 (1+ i2)))
                                     (declare (fixnum i3))
                                     (if (>= i3 len)
                                       (values nil idx)
                                       (let* ((s3 (aref vector i3)))
                                         (declare (type (unsigned-byte 8) s3))
                                         (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                                                  (< (the fixnum (logxor s2 #x80)) #x40)
                                                  (< (the fixnum (logxor s3 #x80)) #x40)
                                                  (or (>= 1st-unit #xf1)
                                                      (>= s1 #x90)))
                                           (values
                                            (code-char
                                             (logior
                                              (the fixnum
                                                (logior
                                                 (the fixnum
                                                   (ash (the fixnum (logand 1st-unit 7)) 18))
                                                 (the fixnum
                                                   (ash (the fixnum (logxor s1 #x80)) 12))))
                                              (the fixnum
                                                (logior
                                                 (the fixnum
                                                   (ash (the fixnum (logxor s2 #x80)) 6))
                                                 (the fixnum (logxor s3 #x80))))))
                                            (the fixnum (1+ i3)))
                                           (values nil idx))))))))))))))
                 (values nil idx))))))))
    :memory-encode-function
    (nfunction
     utf-8-memory-encode
     (lambda (char pointer idx)
       (declare (fixnum idx))
       (let* ((code (char-code char))
              (i1 (1+ idx))
              (i2 (1+ i1))
              (i3 (1+ i2)))
         (declare (type (mod #x110000) code i1 i2 i3))
         (cond ((< code #x80)
                (setf (%get-unsigned-byte pointer idx) code)
                i1)
               ((< code #x800)
                (setf (%get-unsigned-byte pointer idx)
                      (logior #xc0 (the fixnum (ash code -6)))
                      (%get-unsigned-byte pointer i1)
                      (logior #x80 (the fixnum (logand code #x3f))))
                i2)
               ((< code #x10000)
                (setf (%get-unsigned-byte pointer idx)
                      (logior #xe0 (the fixnum (ash code -12)))
                      (%get-unsigned-byte pointer i1)
                      (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6)))))
                      (%get-unsigned-byte pointer i2)
                      (logior #x80 (the fixnum (logand code #x3f))))
                i3)
               (t
                (setf (%get-unsigned-byte pointer idx)
                      (logior #xf0
                              (the fixnum (logand #x7 (the fixnum (ash code -18)))))
                      (%get-unsigned-byte pointer i1)
                      (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -12)))))
                      (%get-unsigned-byte pointer i2)
                      (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6)))))
                      (%get-unsigned-byte pointer i3)
                      (logand #x3f code))
                (the fixnum (1+ i3)))))))
    :memory-decode-function
    (nfunction
     utf-8-memory-decode
     (lambda (pointer idx)
       (declare (fixnum idx))
       (let* ((1st-unit (%get-unsigned-byte pointer idx))
              (i1 (1+ idx))
              (i2 (1+ i1))
              (i3 (1+ i2)))
         (declare (type (unsigned-byte 8) 1st-unit)
                  (fixnum i1 i2 i3))
         (if (< 1st-unit #x80)
           (values (code-char 1st-unit) (the fixnum (1+ idx)))
           (if (< 1st-unit #xc2)
             (values nil idx)
             (let* ((s1 (%get-unsigned-byte pointer i1)))
               (declare (type (unsigned-byte 8) s1))
               (if (< 1st-unit #xe0)
                 (if (< (the fixnum (logxor s1 #x80)) #x40)
                   (values
                    (code-char
                     (logior
                      (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                      (the fixnum (logxor s1 #x80))))
                    (the fixnum (1+ i1)))
                   (values nil i1))
                 (let* ((s2 (%get-unsigned-byte pointer i2)))
                   (declare (type (unsigned-byte 8) s2))
                   (if (< 1st-unit #xf0)
                     (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                              (< (the fixnum (logxor s2 #x80)) #x40)
                              (or (>= 1st-unit #xe1)
                                  (>= s1 #xa0)))
                       (values
                        (code-char (the fixnum
                                     (logior (the fixnum
                                               (ash (the fixnum (logand 1st-unit #xf))
                                                    12))
                                             (the fixnum
                                               (logior
                                                (the fixnum
                                                  (ash (the fixnum (logand s1 #x3f))
                                                       6))
                                                (the fixnum (logand s2 #x3f)))))))
                        i3)
                       (values nil idx))
                     (if (>= 1st-unit #xf8)
                       (values nil idx)
                       (let* ((s3 (%get-unsigned-byte pointer i3)))
                         (declare (type (unsigned-byte 8) s3))
                         (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                                  (< (the fixnum (logxor s2 #x80)) #x40)
                                  (< (the fixnum (logxor s3 #x80)) #x40)
                                  (or (>= 1st-unit #xf1)
                                      (>= s1 #x90)))
                           (values
                            (code-char
                             (logior
                              (the fixnum
                                (logior
                                 (the fixnum
                                   (ash (the fixnum (logand 1st-unit 7)) 18))
                                 (the fixnum
                                   (ash (the fixnum (logxor s1 #x80)) 12))))
                              (the fixnum
                                (logior
                                 (the fixnum
                                   (ash (the fixnum (logxor s2 #x80)) 6))
                                 (the fixnum (logxor s3 #x80))))))
                            (the fixnum (1+ i3)))
                           (values nil idx)))))))))))))
    :units-in-string-function
    (nfunction
     utf-8-units-in-string
     (lambda (string &optional (start 0) (end (length string)))
       (when (>= end start)
         (do* ((nunits 0)
               (i start (1+ i)))
              ((= i end) nunits)
           (declare (fixnum nunits))
           (let* ((code (char-code (schar string i))))
             (declare (type (mod #x110000) code))
             (incf nunits
                   (if (< code #x80)
                     1
                     (if (< code #x800)
                       2
                       (if (< code #x10000)
                         3
                         4)))))))))
    :length-of-vector-encoding-function
    (nfunction
     utf-8-length-of-vector-encoding
     (lambda (vector &optional (start 0) (end (length vector)))
       (declare (type (simple-array (unsigned-byte 8) (*)) vector))
       (do* ((i start)
             (nchars 0 (1+ nchars)))
            ((>= i end)
             (if (= i end) nchars))
         (let* ((code (aref vector i)))
           (declare (type (unsigned-byte 8) code))
           (incf i
                 (cond ((< code #x80) 1)
                       ((< code #xe0) 2)
                       ((< code #xf0) 3)
                       (t 4)))))))
    :length-of-memory-encoding-function
    (nfunction
     utf-8-length-of-memory-encoding
     (lambda (pointer nunits &optional (start 0))
       (do* ((i start)
             (nchars 0 (1+ nchars)))
            ((>= i nunits)
             (if (= i nunits) nchars))
         (let* ((code (%get-unsigned-byte pointer i)))
           (declare (type (unsigned-byte 8) code))
           (incf i
                 (cond ((< code #x80) 1)
                       ((< code #xe0) 2)
                       ((< code #xf0) 3)
                       (t 4)))))))
    :literal-char-code-limit #x80
    )

