;;;-*-Mode: LISP; Package: CCL -*-
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

(defun lookup-character-encoding (name)
  (gethash name *character-encodings*))

(defun get-character-encoding (name)
  (or (lookup-character-encoding name)
      (error "Unknown character encoding: ~s." name)))

(defun (setf get-character-encoding) (new name)
  (setf (gethash name *character-encodings*) new))

(defun ensure-character-encoding (thing)
  (if (typep thing 'character-encoding)
    thing
    (or (lookup-character-encoding thing)
        (error "~s is not a character-encoding or the name of a character-encoding."
               thing))))


(defstruct character-encoding
  (name ())                             ;canonical name
  (code-unit-size 8)                    ;in bits: 8, 16, 32
  (native-endianness t)                 ;if nil, need to swap 16,32-bit units
  (max-units-per-char 1)                ;usually 1-4

  ;; Writes CHAR (or a replacement character if CHAR can't be encoded)
  ;; to STREAM and returns the number of code-units written.
  stream-encode-function                ;(CHAR WRITE-FUNCTION STREAM)
  
  ;; Returns a charcter (possibly #\Replacement_Character) or :EOF.
  stream-decode-function                ;(1ST-UNIT NEXT-UNIT STREAM)

  ;; Sets 1 or more units in a vector argument and returns a value 1
  ;; greater than the index of the last octet written to the vector
  vector-encode-function                ;(STRING VECTOR INDEX START END)
  
  ;; Returns a value 1 greater than the last octet index consumed from
  ;; the vector argument.
  vector-decode-function                ;(VECTOR INDEX NOCTETS STRING)
  
  ;; Sets one or more units in memory at the address denoted by
  ;; the pointer and idx arguments and returns (+ idx number of
  ;; units written to memory), else returns NIL if any character
  ;; can't be encoded.
  memory-encode-function                ;(STRING POINTER INDEX START END)

  
  ;; Returns (as multiple values) the  string encoded in memory
  ;; at the address denoted by the address and index args and the
  ;; sum of the index arg and the number of octets consumed.
  memory-decode-function                ;(POINTER NOCTETS INDEX STRING)
  
  ;; Returns the number of octets needed to encode STRING between START and END
  octets-in-string-function              ;(STRING START END)

  ;; Returns the number of (full) characters encoded in VECTOR, and the index
  ;; of the first octet not used to encode them. (The second value may be less than END).
  length-of-vector-encoding-function    ;(VECTOR START END) 

  ;; Returns the number of (full) characters encoded in memort at (+ POINTER START)
  ;; and the number of octets used to encode them.  (The second value may be less
  ;; than NOCTETS.)
  length-of-memory-encoding-function    ;(POINTER NOCTETS START)

  ;; Code units and character codes less than this value map to themselves
  (literal-char-code-limit 0)

  ;; Does a byte-order-mark determine the endianness of input ?
  ;; Should we prepend a BOM to output ?
  ;; If non-nil, the value should be the name of the an encoding
  ;; that implements this encoding with swapped byte order.
  (use-byte-order-mark nil)
  ;; What alternate line-termination conventions can be encoded ?  (This basically
  ;; means "can #\Line_Separator be encoded?", since :CR and :CRLF can always
  ;; be encoded.)
  (alternate-line-termination-conventions '(:cr :crlf))
  ;; By what other MIME names is this encoding known ?
  (aliases nil)
  (documentation nil)
  )

(defconstant byte-order-mark #\u+feff)
(defconstant byte-order-mark-char-code (char-code byte-order-mark))
(defconstant swapped-byte-order-mark #\u+fffe)
(defconstant swapped-byte-order-mark-char-code (char-code swapped-byte-order-mark))


(defun decode-character-encoded-vector (encoding vector start-index noctets string)
  (setq encoding (ensure-character-encoding encoding))
  (unless (= (the (unsigned-byte 8) (typecode vector))
             target::subtag-u8-vector)
    (report-bad-arg vector '(simple-array (unsigned-byte 8) (*))))
  (unless (= (the (unsigned-byte 8) (typecode string))
             target::subtag-simple-base-string)
    (report-bad-arg vector 'simple-string))
  (let* ((len (length vector)))
    (declare (type index len))
    (unless (and (typep start-index 'fixnum)
                 (>= (the fixnum start-index) 0)
                 (< (the fixnum start-index) len))
      (error "~s is an invalid start index for ~s" start-index vector))
    (unless (and (typep noctets 'fixnum)
                 (>= (the fixnum noctets) 0)
                 (<= (+ (the fixnum start-index) (the fixnum noctets)) len))
      (error "~S is an invalid octet count for ~s at ~s" noctets vector start-index))
    (funcall (character-encoding-vector-decode-function encoding)
             vector
             start-index
             noctets
             string)))


(defmethod print-object ((ce character-encoding) stream)
  (print-unreadable-object (ce stream :type t :identity t)
    (format stream "~a" (character-encoding-name ce))))

;;; N.B.  (ccl:nfunction <name> (lambda (...) ...)) is just  like
;;;       (cl:function (lambda (...) ...)), except that the resulting
;;; function will have "name" <name> (this is often helpful when debugging.)

(defmacro define-character-encoding (name doc &rest args &key &allow-other-keys)
  (setq name (intern (string name) "KEYWORD"))
  (let* ((encoding (gensym))
         (alias (gensym)))
  `(let* ((,encoding (make-character-encoding :name ,name :documentation ,doc ,@args)))
    (setf (get-character-encoding ,name) ,encoding)
    (dolist (,alias (character-encoding-aliases ,encoding))
      (setf (get-character-encoding ,alias) ,encoding))
    ',name)))

(defun encoding-name (encoding)
  (character-encoding-name (or encoding (get-character-encoding nil))))

;;; ISO-8859-1 is trivial, though of course it can't really encode characters
;;; whose CHAR-CODE is >= 256

(defun 8-bit-fixed-width-octets-in-string (string start end)
  (declare (ignore string))
  (if (>= end start)
    (- end start)
    0))

(defun 8-bit-fixed-width-length-of-vector-encoding (vector start end)
  (declare (ignore vector))
  (if (>= end start)
    (values (- end start) (- end start))
    (values 0 0)))

(defun 8-bit-fixed-width-length-of-memory-encoding (pointer noctets start)
  (declare (ignore pointer start))
  noctets)

(define-character-encoding :iso-8859-1
  "An 8-bit, fixed-width character encoding in which all character
codes map to their Unicode equivalents. Intended to support most
characters used in most Western European languages."

  ;; The NIL alias is used internally to mean that ISO-8859-1 is
  ;; the "null" 8-bit encoding
  :aliases '(nil :iso_8859-1 :latin1 :l1 :ibm819 :cp819 :csISOLatin1)
  :stream-encode-function
  (nfunction
   iso-8859-1-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (if (>= code 256)
         (setq code (char-code #\Sub)))
       (funcall write-function stream code)
       1)))
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
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (if (>= code 256)
           (setq code (char-code #\Sub)))
         (progn
           (setf (aref vector idx) code)
           (incf idx))))))
  :vector-decode-function
  (nfunction
   iso-8859-1-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (setf (schar string i) (code-char (the (unsigned-byte 8)
                                             (aref vector index)))))))
  :memory-encode-function
  (nfunction
   iso-8859-1-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (if (>= code 256)
           (setq code (char-code #\Sub)))
         (setf (%get-unsigned-byte pointer idx) code)
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-1-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
         (setf (schar string i) (code-char (the (unsigned-byte 8)
                                             (%get-unsigned-byte pointer index)))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :literal-char-code-limit 256
  )

(define-character-encoding :us-ascii
  "An 7-bit, fixed-width character encoding in which all character
codes map to their Unicode equivalents. "

  :aliases '(:csASCII :cp637 :IBM637 :us :ISO646-US :ascii :ISO-ir-6)
  :stream-encode-function
  (nfunction
   ascii-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (when (>= code 128)
         (setq code (char-code #\Sub)))
       (funcall write-function stream code)
       1)))
  :stream-decode-function
  (nfunction
   ascii-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit 128)
       (code-char 1st-unit)
       #\Replacement_Character)))
  :vector-encode-function
  (nfunction
   ascii-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (if (>= code 128)
           (setq code (char-code #\Sub)))
         (setf (aref vector idx) code)
         (incf idx)))))
  :vector-decode-function
  (nfunction
   ascii-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((code (aref vector index)))
         (declare (type (unsigned-byte 8) code))
         (when (>= code 128)
           (setq code (char-code #\Sub)))
         (setf (schar string i) code)))))
  :memory-encode-function
  (nfunction
   ascii-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (if (>= code 128)
           (setq code (char-code #\Sub)))
         (setf (%get-unsigned-byte pointer idx) code)
         (incf idx)))))
  :memory-decode-function
  (nfunction
   ascii-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((code (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) code))
         (if (>= code 128)
           (setf (schar string i) #\sub)
           (setf (schar string i) (code-char code)))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :literal-char-code-limit 128
  )



;;; Other 1-byte, fixed-width encodings.  Typically, codes in the range
;;; #x00-#x9f maps straight through, while codes #xa0-#xff select arbitrary
;;; Unicode characters that are commonly used in some locale.  (Sometimes
;;; the break is at #x80 instead of #xa0).

(defparameter *iso-8859-2-to-unicode*
  #(
  ;; #xa0
  #\u+00a0 #\u+0104 #\u+02d8 #\u+0141 #\u+00a4 #\u+013d #\u+015a #\u+00a7
  #\u+00a8 #\u+0160 #\u+015e #\u+0164 #\u+0179 #\u+00ad #\u+017d #\u+017b
  ;; #xb0 
  #\u+00b0 #\u+0105 #\u+02db #\u+0142 #\u+00b4 #\u+013e #\u+015b #\u+02c7
  #\u+00b8 #\u+0161 #\u+015f #\u+0165 #\u+017a #\u+02dd #\u+017e #\u+017c
  ;; #xc0 
  #\u+0154 #\u+00c1 #\u+00c2 #\u+0102 #\u+00c4 #\u+0139 #\u+0106 #\u+00c7
  #\u+010c #\u+00c9 #\u+0118 #\u+00cb #\u+011a #\u+00cd #\u+00ce #\u+010e
  ;; #xd0 
  #\u+0110 #\u+0143 #\u+0147 #\u+00d3 #\u+00d4 #\u+0150 #\u+00d6 #\u+00d7
  #\u+0158 #\u+016e #\u+00da #\u+0170 #\u+00dc #\u+00dd #\u+0162 #\u+00df
  ;; #xe0 
  #\u+0155 #\u+00e1 #\u+00e2 #\u+0103 #\u+00e4 #\u+013a #\u+0107 #\u+00e7
  #\u+010d #\u+00e9 #\u+0119 #\u+00eb #\u+011b #\u+00ed #\u+00ee #\u+010f
  ;; #xf0 
  #\u+0111 #\u+0144 #\u+0148 #\u+00f3 #\u+00f4 #\u+0151 #\u+00f6 #\u+00f7
  #\u+0159 #\u+016f #\u+00fa #\u+0171 #\u+00fc #\u+00fd #\u+0163 #\u+02d9
))

(defparameter *unicode-00a0-0180-to-iso8859-2*
  #(
    #xa0 nil nil nil #xa4 nil nil #xa7 ; #xa0-#xa7 
    #xa8 nil nil nil nil #xad nil nil ; #xa8-#xaf 
    #xb0 nil nil nil #xb4 nil nil nil ; #xb0-#xb7 
    #xb8 nil nil nil nil nil nil nil  ; #xb8-#xbf 
    nil #xc1 #xc2 nil #xc4 nil nil #xc7 ; #xc0-#xc7 
    nil #xc9 nil #xcb nil #xcd #xce nil ; #xc8-#xcf 
    nil nil nil #xd3 #xd4 nil #xd6 #xd7 ; #xd0-#xd7 
    nil nil #xda nil #xdc #xdd nil #xdf ; #xd8-#xdf 
    nil #xe1 #xe2 nil #xe4 nil nil #xe7 ; #xe0-#xe7 
    nil #xe9 nil #xeb nil #xed #xee nil ; #xe8-#xef 
    nil nil nil #xf3 #xf4 nil #xf6 #xf7 ; #xf0-#xf7 
    nil nil #xfa nil #xfc #xfd nil nil ; #xf8-#xff 
    ;; #x0100 
    nil nil #xc3 #xe3 #xa1 #xb1 #xc6 #xe6 ; #x100-#x107 
    nil nil nil nil #xc8 #xe8 #xcf #xef ; #x108-#x10f 
    #xd0 #xf0 nil nil nil nil nil nil ; #x110-#x117 
    #xca #xea #xcc #xec nil nil nil nil ; #x118-#x11f 
    nil nil nil nil nil nil nil nil     ; #x120-#x127 
    nil nil nil nil nil nil nil nil     ; #x128-#x12f 
    nil nil nil nil nil nil nil nil     ; #x130-#x137 
    nil #xc5 #xe5 nil nil #xa5 #xb5 nil ; #x138-#x13f 
    nil #xa3 #xb3 #xd1 #xf1 nil nil #xd2 ; #x140-#x147 
    #xf2 nil nil nil nil nil nil nil  ; #x148-#x14f 
    #xd5 #xf5 nil nil #xc0 #xe0 nil nil ; #x150-#x157 
    #xd8 #xf8 #xa6 #xb6 nil nil #xaa #xba ; #x158-#x15f 
    #xa9 #xb9 #xde #xfe #xab #xbb nil nil ; #x160-#x167 
    nil nil nil nil nil nil #xd9 #xf9 ; #x168-#x16f 
    #xdb #xfb nil nil nil nil nil nil ; #x170-#x177 
    nil #xac #xbc #xaf #xbf #xae #xbe nil ; #x178-#x17f 
    ))

(defparameter *unicode-00c0-00e0-to-iso8859-2*
  #(
    nil nil nil nil nil nil nil #xb7  ; #xc0-#xc7 
    nil nil nil nil nil nil nil nil     ; #xc8-#xcf 
    nil nil nil nil nil nil nil nil     ; #xd0-#xd7 
    #xa2 #xff nil #xb2 nil #xbd nil nil ; #xd8-#xdf
    ))

(define-character-encoding :iso-8859-2
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Central/Eastern Europe."
  :aliases '(:iso_8859-2 :latin-2 :l2 :csISOLatin2)
  :stream-encode-function
  (nfunction
   iso-8859-2-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-00a0-0180-to-iso8859-2*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2c0) (< code #x2e0))
                       (svref *unicode-00c0-00e0-to-iso8859-2*
                                      (the fixnum (- code #x2c0)))))))
                      
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-2-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                          ((< code #x180)
                           (svref *unicode-00a0-0180-to-iso8859-2*
                                  (the fixnum (- code #xa0))))
                          ((and (>= code #x2c0) (< code #x2e0))
                           (svref *unicode-00c0-00e0-to-iso8859-2*
                                  (the fixnum (- code #x2c0)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-2-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (setf (schar string i)
            (if (< 1st-unit #xa0)
              (code-char 1st-unit)
              (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-2-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-00a0-0180-to-iso8859-2*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2c0) (< code #x2e0))
                         (svref *unicode-00c0-00e0-to-iso8859-2*
                                (the fixnum (- code #x2c0)))))))
       (declare (type (mod #x110000) code))
       (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
       (1+ idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-2-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :literal-char-code-limit #xa0
  )

(defparameter *iso-8859-3-to-unicode*
  #(
    ;; #xa0 
    #\u+00a0 #\u+0126 #\u+02d8 #\u+00a3 #\u+00a4 #\u+fffd #\u+0124 #\u+00a7
    #\u+00a8 #\u+0130 #\u+015e #\u+011e #\u+0134 #\u+00ad #\u+fffd #\u+017b
    ;; #xb0 
    #\u+00b0 #\u+0127 #\u+00b2 #\u+00b3 #\u+00b4 #\u+00b5 #\u+0125 #\u+00b7
    #\u+00b8 #\u+0131 #\u+015f #\u+011f #\u+0135 #\u+00bd #\u+fffd #\u+017c
    ;; #xc0 
    #\u+00c0 #\u+00c1 #\u+00c2 #\u+fffd #\u+00c4 #\u+010a #\u+0108 #\u+00c7
    #\u+00c8 #\u+00c9 #\u+00ca #\u+00cb #\u+00cc #\u+00cd #\u+00ce #\u+00cf
    ;; #xd0 
    #\u+fffd #\u+00d1 #\u+00d2 #\u+00d3 #\u+00d4 #\u+0120 #\u+00d6 #\u+00d7
    #\u+011c #\u+00d9 #\u+00da #\u+00db #\u+00dc #\u+016c #\u+015c #\u+00df
    ;; #xe0 
    #\u+00e0 #\u+00e1 #\u+00e2 #\u+fffd #\u+00e4 #\u+010b #\u+0109 #\u+00e7
    #\u+00e8 #\u+00e9 #\u+00ea #\u+00eb #\u+00ec #\u+00ed #\u+00ee #\u+00ef
    ;; #xf0 
    #\u+fffd #\u+00f1 #\u+00f2 #\u+00f3 #\u+00f4 #\u+0121 #\u+00f6 #\u+00f7
    #\u+011d #\u+00f9 #\u+00fa #\u+00fb #\u+00fc #\u+016d #\u+015d #\u+02d9
    ))

(defparameter *unicode-a0-100-to-iso8859-3*
  #(
    #xa0 nil nil #xa3 #xa4 nil nil #xa7 ; #xa0-#xa7 
    #xa8 nil nil nil nil #xad nil nil   ; #xa8-#xaf 
    #xb0 nil #xb2 #xb3 #xb4 #xb5 nil #xb7 ; #xb0-#xb7 
    #xb8 nil nil nil nil #xbd nil nil   ; #xb8-#xbf 
    #xc0 #xc1 #xc2 nil #xc4 nil nil #xc7 ; #xc0-#xc7 
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf 
    nil #xd1 #xd2 #xd3 #xd4 nil #xd6 #xd7 ; #xd0-#xd7 
    nil #xd9 #xda #xdb #xdc nil nil #xdf ; #xd8-#xdf 
    #xe0 #xe1 #xe2 nil #xe4 nil nil #xe7 ; #xe0-#xe7 
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef 
    nil #xf1 #xf2 #xf3 #xf4 nil #xf6 #xf7 ; #xf0-#xf7 
    nil #xf9 #xfa #xfb #xfc nil nil nil ; #xf8-#xff 
    ))

(defparameter *unicode-108-180-to-iso8859-3*
  #(
    #xc6 #xe6 #xc5 #xe5 #x00 #x00 #x00 #x00 ; #x108-#x10f 
    nil nil nil nil nil nil nil nil     ; #x110-#x117 
    nil nil nil nil #xd8 #xf8 #xab #xbb ; #x118-#x11f 
    #xd5 #xf5 nil nil #xa6 #xb6 #xa1 #xb1 ; #x120-#x127 
    nil nil nil nil nil nil nil nil     ; #x128-#x12f 
    #xa9 #xb9 nil nil #xac #xbc nil nil ; #x130-#x137 
    nil nil nil nil nil nil nil nil     ; #x138-#x13f 
    nil nil nil nil nil nil nil nil     ; #x140-#x147 
    nil nil nil nil nil nil nil nil     ; #x148-#x14f 
    nil nil nil nil nil nil nil nil     ; #x150-#x157 
    nil nil nil nil #xde #xfe #xaa #xba ; #x158-#x15f 
    nil nil nil nil nil nil nil nil     ; #x160-#x167 
    nil nil nil nil #xdd #xfd nil nil   ; #x168-#x16f 
    nil nil nil nil nil nil nil nil     ; #x170-#x177 
    nil nil nil #xaf #xbf nil nil nil   ; #x178-#x17f 
    ))

(defparameter *unicode-2d8-2e0-to-iso8859-3*
  #(
    #xa2 #xff nil nil nil nil nil nil   ; #x2d8-#x2df 
    ))


    
(define-character-encoding :iso-8859-3
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Southern Europe."

  :aliases '(:iso_8859-3 :latin3 :l3 :csisolatin3)
  :stream-encode-function
  (nfunction
   iso-8859-3-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso8859-3*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x108) (< code #x180))
                       (svref *unicode-108-180-to-iso8859-3*
                              (the fixnum (- code #x108))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2d8-2e0-to-iso8859-3*
                              (the fixnum (- code #x2d8)))))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-3-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-3-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x100)
                         (svref *unicode-a0-100-to-iso8859-3*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x108) (< code #x180))
                         (svref *unicode-108-180-to-iso8859-3*
                                (the fixnum (- code #x108))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2d8-2e0-to-iso8859-3*
                 
               (the fixnum (- code #x2d8)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-3-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
         (let* ((1st-unit (aref vector index)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (setf (schar string i)
                 (if (< 1st-unit #xa0)
                   (code-char 1st-unit)
                   (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-3-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x100)
                         (svref *unicode-a0-100-to-iso8859-3*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x108) (< code #x180))
                         (svref *unicode-108-180-to-iso8859-3*
                                (the fixnum (- code #x108))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2d8-2e0-to-iso8859-3*
                                (the fixnum (- code #x2d8)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-3-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :literal-char-code-limit #xa0
  )


(defparameter *iso-8859-4-to-unicode*
  #(
    ;; #xa0 
    #\u+00a0 #\u+0104 #\u+0138 #\u+0156 #\u+00a4 #\u+0128 #\u+013b #\u+00a7
    #\u+00a8 #\u+0160 #\u+0112 #\u+0122 #\u+0166 #\u+00ad #\u+017d #\u+00af
    ;; #xb0 
    #\u+00b0 #\u+0105 #\u+02db #\u+0157 #\u+00b4 #\u+0129 #\u+013c #\u+02c7
    #\u+00b8 #\u+0161 #\u+0113 #\u+0123 #\u+0167 #\u+014a #\u+017e #\u+014b
    ;; #xc0 
    #\u+0100 #\u+00c1 #\u+00c2 #\u+00c3 #\u+00c4 #\u+00c5 #\u+00c6 #\u+012e
    #\u+010c #\u+00c9 #\u+0118 #\u+00cb #\u+0116 #\u+00cd #\u+00ce #\u+012a
    ;; #xd0 
    #\u+0110 #\u+0145 #\u+014c #\u+0136 #\u+00d4 #\u+00d5 #\u+00d6 #\u+00d7
    #\u+00d8 #\u+0172 #\u+00da #\u+00db #\u+00dc #\u+0168 #\u+016a #\u+00df
    ;; #xe0 
    #\u+0101 #\u+00e1 #\u+00e2 #\u+00e3 #\u+00e4 #\u+00e5 #\u+00e6 #\u+012f
    #\u+010d #\u+00e9 #\u+0119 #\u+00eb #\u+0117 #\u+00ed #\u+00ee #\u+012b
    ;; #xf0 
    #\u+0111 #\u+0146 #\u+014d #\u+0137 #\u+00f4 #\u+00f5 #\u+00f6 #\u+00f7
    #\u+00f8 #\u+0173 #\u+00fa #\u+00fb #\u+00fc #\u+0169 #\u+016b #\u+02d9
    ))


(defparameter *unicode-a0-180-to-iso8859-4*
  #(
    #xa0 nil nil nil #xa4 nil nil #xa7  ; #xa0-#xa7 
    #xa8 nil nil nil nil #xad nil #xaf  ; #xa8-#xaf 
    #xb0 nil nil nil #xb4 nil nil nil   ; #xb0-#xb7 
    #xb8 nil nil nil nil nil nil nil    ; #xb8-#xbf 
    nil #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 nil ; #xc0-#xc7 
    nil #xc9 nil #xcb nil #xcd #xce nil ; #xc8-#xcf 
    nil nil nil nil #xd4 #xd5 #xd6 #xd7 ; #xd0-#xd7 
    #xd8 nil #xda #xdb #xdc nil nil #xdf ; #xd8-#xdf 
    nil #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 nil ; #xe0-#xe7 
    nil #xe9 nil #xeb nil #xed #xee nil ; #xe8-#xef 
    nil nil nil nil #xf4 #xf5 #xf6 #xf7 ; #xf0-#xf7 
    #xf8 nil #xfa #xfb #xfc nil nil nil ; #xf8-#xff 
    #xc0 #xe0 nil nil #xa1 #xb1 nil nil ; #x100-#x107 
    nil nil nil nil #xc8 #xe8 nil nil   ; #x108-#x10f 
    #xd0 #xf0 #xaa #xba nil nil #xcc #xec ; #x110-#x117 
    #xca #xea nil nil nil nil nil nil   ; #x118-#x11f 
    nil nil #xab #xbb nil nil nil nil   ; #x120-#x127 
    #xa5 #xb5 #xcf #xef nil nil #xc7 #xe7 ; #x128-#x12f 
    nil nil nil nil nil nil #xd3 #xf3   ; #x130-#x137 
    #xa2 nil nil #xa6 #xb6 nil nil nil  ; #x138-#x13f 
    nil nil nil nil nil #xd1 #xf1 nil   ; #x140-#x147 
    nil nil #xbd #xbf #xd2 #xf2 nil nil ; #x148-#x14f 
    nil nil nil nil nil nil #xa3 #xb3   ; #x150-#x157 
    nil nil nil nil nil nil nil nil     ; #x158-#x15f 
    #xa9 #xb9 nil nil nil nil #xac #xbc ; #x160-#x167 
    #xdd #xfd #xde #xfe nil nil nil nil ; #x168-#x16f 
    nil nil #xd9 #xf9 nil nil nil nil   ; #x170-#x177 
    nil nil nil nil nil #xae #xbe nil   ; #x178-#x17f 
    ))

(defparameter *unicode-2c0-2e0-to-iso8859-4*
  #(
    nil nil nil nil nil nil nil #xb7    ; #x2c0-#x2c7
    nil nil nil nil nil nil nil nil     ; #x2c8-#x2cf
    nil nil nil nil nil nil nil nil     ; #x2d0-#x2d7
    nil #xff nil #xb2 nil nil nil nil   ; #x2d8-#x2df
    ))



(define-character-encoding :iso-8859-4
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Northern Europe."

  :aliases '(:iso_8859-4 :latin4 :l4 :csisolatin4)
  :stream-encode-function
  (nfunction
   iso-8859-4-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso8859-4*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-iso8859-4*
                              (the fixnum (- code #x2c0)))))))
                      
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-4-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-4-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-a0-180-to-iso8859-4*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2c0-2e0-to-iso8859-4*
                                (the fixnum (- code #x2c0)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-4-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-4-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-a0-180-to-iso8859-4*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2c0-2e0-to-iso8859-4*
                                (the fixnum (- code #x2c0)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-4-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :literal-char-code-limit #xa0
  )

;;; UTF-8.  Decoding checks for malformed sequences; it might be faster (and
;;; would certainly be simpler) if it didn't.
(define-character-encoding :utf-8
    "An 8-bit, variable-length character encoding in which characters
with CHAR-CODEs in the range #x00-#x7f can be encoded in a single
octet; characters with larger code values can be encoded in 2 to 4
bytes."
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
         (if (>= 1st-unit #xc2)
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
                       (the fixnum (logxor s1 #x80))))
                     #\Replacement_Character)
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
                                                     (the fixnum (logand s2 #x3f)))))))
                             #\Replacement_Character)
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
                                          (the fixnum (logxor s3 #x80))))))
                                     #\Replacement_Character))))
                             #\Replacement_Character)))))))))
           #\Replacement_Character))))
    :vector-encode-function
    (nfunction
     utf-8-vector-encode
     (lambda (string vector idx start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (fixnum idx))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (let* ((char (schar string i))
                (code (char-code char)))
           (declare (type (mod #x110000) code))
           (cond ((< code #x80)
                  (setf (aref vector idx) code)
                  (incf idx))
                 ((< code #x800)
                  (setf (aref vector idx)
                        (logior #xc0 (the fixnum (ash code -6))))
                  (setf (aref vector (the fixnum (1+ idx)))
                        (logior #x80 (the fixnum (logand code #x3f))))
                  (incf idx 2))
                 ((< code #x10000)
                  (setf (aref vector idx)
                        (logior #xe0 (the fixnum (ash code -12))))
                  (setf (aref vector (the fixnum (1+ idx)))
                        (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
                  (setf (aref vector (the fixnum (+ idx 2)))
                        (logior #x80 (the fixnum (logand code #x3f))))
                  (incf idx 3))
                 (t
                   (setf (aref vector idx)
                         (logior #xf0
                                 (the fixnum (logand #x7 (the fixnum (ash code -18))))))
                   (setf (aref vector (the fixnum (1+ idx)))
                         (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -12))))))
                   (setf (aref vector (the fixnum (+ idx 2)))
                         (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
                   (setf (aref vector (the fixnum (+ idx 3))) (logand #x3f code))
                   (incf idx 4)))))))
    :vector-decode-function
    (nfunction
     utf-8-vector-decode
     (lambda (vector idx noctets string)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (type index idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((= index end) index)
           (let* ((1st-unit (aref vector index)))
             (declare (type (unsigned-byte 8) 1st-unit))
             (let* ((char 
                     (if (< 1st-unit #x80)
                       (code-char 1st-unit)
                       (if (>= 1st-unit #xc2)
                           (let* ((2nd-unit (aref vector (incf index))))
                             (declare (type (unsigned-byte 8) 2nd-unit))
                             (if (< 1st-unit #xe0)
                               (if (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                 (code-char
                                  (logior
                                   (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                                   (the fixnum (logxor 2nd-unit #x80)))))
                               (let* ((3rd-unit (aref vector (incf index))))
                                 (declare (type (unsigned-byte 8) 3rd-unit))
                                 (if (< 1st-unit #xf0)
                                   (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                            (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                            (or (>= 1st-unit #xe1)
                                                (>= 2nd-unit #xa0)))
                                     (code-char (the fixnum
                                                  (logior (the fixnum
                                                            (ash (the fixnum (logand 1st-unit #xf))
                                                                 12))
                                                          (the fixnum
                                                            (logior
                                                             (the fixnum
                                                               (ash (the fixnum (logand 2nd-unit #x3f))
                                                                    6))
                                                             (the fixnum (logand 3rd-unit #x3f))))))))
                                   (let* ((4th-unit (aref vector (incf index))))
                                     (declare (type (unsigned-byte 8) 4th-unit))
                                     (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                              (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                              (< (the fixnum (logxor 4th-unit #x80)) #x40)
                                              (or (>= 1st-unit #xf1)
                                                  (>= 2nd-unit #x90)))
                                       (code-char
                                        (logior
                                         (the fixnum
                                           (logior
                                            (the fixnum
                                              (ash (the fixnum (logand 1st-unit 7)) 18))
                                            (the fixnum
                                              (ash (the fixnum (logxor 2nd-unit #x80)) 12))))
                                         (the fixnum
                                           (logior
                                            (the fixnum
                                              (ash (the fixnum (logxor 3rd-unit #x80)) 6))
                                            (the fixnum (logxor 4th-unit #x80))))))))))))))))
               (setf (schar string i) (or char #\Replacement_Character)))))))
    :memory-encode-function
    (nfunction
     utf-8-memory-encode
     (lambda (string pointer idx start end)
       (declare (fixnum idx))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (let* ((code (char-code (schar string i))))
           (declare (type (mod #x110000) code))
           (cond ((< code #x80)
                  (setf (%get-unsigned-byte pointer idx) code)
                  (incf idx))
                 ((< code #x800)
                  (setf (%get-unsigned-byte pointer idx)
                        (logior #xc0 (the fixnum (ash code -6))))
                  (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                        (logior #x80 (the fixnum (logand code #x3f))))
                  (incf idx 2))
                 ((< code #x10000)
                  (setf (%get-unsigned-byte pointer idx)
                        (logior #xe0 (the fixnum (ash code -12))))
                  (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                        (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
                  (setf (%get-unsigned-byte pointer (the fixnum (+ idx 2)))
                        (logior #x80 (the fixnum (logand code #x3f))))
                  (incf idx 3))
                 (t
                  (setf (%get-unsigned-byte pointer idx)
                        (logior #xf0
                                (the fixnum (logand #x7 (the fixnum (ash code -18))))))
                  (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                        (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -12))))))
                  (setf (%get-unsigned-byte pointer (the fixnum (+ idx 2)))
                        (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
                  (setf (%get-unsigned-byte pointer (the fixnum (+ idx 3)))
                        (logand #x3f code))
                  (incf idx 4)))))))
    :memory-decode-function
    (nfunction
     utf-8-memory-decode
     (lambda (pointer noctets idx string)
       (declare (fixnum noctets idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((>= index end) (if (= index end) index 0))
         (let* ((1st-unit (%get-unsigned-byte pointer index)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (let* ((char (if (< 1st-unit #x80)
                          (code-char 1st-unit)
                          (if (>= 1st-unit #xc2)
                            (let* ((2nd-unit (%get-unsigned-byte pointer (incf index))))
                              (declare (type (unsigned-byte 8) 2nd-unit))
                              (if (< 1st-unit #xe0)
                                (if (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                  (code-char
                                   (logior
                                    (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                                    (the fixnum (logxor 2nd-unit #x80)))))
                                (let* ((3rd-unit (%get-unsigned-byte pointer (incf index))))
                                  (declare (type (unsigned-byte 8) 3rd-unit))
                                  (if (< 1st-unit #xf0)
                                    (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                             (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                             (or (>= 1st-unit #xe1)
                                                 (>= 2nd-unit #xa0)))
                                      (code-char (the fixnum
                                                   (logior (the fixnum
                                                             (ash (the fixnum (logand 1st-unit #xf))
                                                                  12))
                                                           (the fixnum
                                                             (logior
                                                              (the fixnum
                                                                (ash (the fixnum (logand 2nd-unit #x3f))
                                                                     6))
                                                              (the fixnum (logand 3rd-unit #x3f))))))))
                                    (if (< 1st-unit #xf8)
                                      (let* ((4th-unit (%get-unsigned-byte pointer (incf index))))
                                        (declare (type (unsigned-byte 8) 4th-unit))
                                        (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                                 (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                                 (< (the fixnum (logxor 4th-unit #x80)) #x40)
                                                 (or (>= 1st-unit #xf1)
                                                     (>= 2nd-unit #x90)))
                                          (code-char
                                           (logior
                                            (the fixnum
                                              (logior
                                               (the fixnum
                                                 (ash (the fixnum (logand 1st-unit 7)) 18))
                                               (the fixnum
                                                 (ash (the fixnum (logxor 2nd-unit #x80)) 12))))
                                            (the fixnum
                                              (logior
                                               (the fixnum
                                                 (ash (the fixnum (logxor 3rd-unit #x80)) 6))
                                               (the fixnum (logxor 4th-unit #x80)))))))))))))))))
             (setf (schar string i) (or char #\Replacement_Character)))))))
    :octets-in-string-function
    (nfunction
     utf-8-octets-in-string
     (lambda (string start end)
       (if (>= end start)
         (do* ((noctets 0)
               (i start (1+ i)))
              ((= i end) noctets)
           (declare (fixnum noctets))
           (let* ((code (char-code (schar string i))))
             (declare (type (mod #x110000) code))
             (incf noctets
                   (if (< code #x80)
                     1
                     (if (< code #x800)
                       2
                       (if (< code #x10000)
                         3
                         4))))))
         0)))
    :length-of-vector-encoding-function
    (nfunction
     utf-8-length-of-vector-encoding
     (lambda (vector start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector))
       (do* ((i start)
             (nchars 0))
            ((>= i end)
             (if (= i end) (values nchars i)))
         (declare (fixnum i))
         (let* ((code (aref vector i))
                (nexti (+ i (cond ((< code #x80) 1)
                                  ((< code #xe0) 2)
                                  ((< code #xf0) 3)
                                  (t 4)))))
           (declare (type (unsigned-byte 8) code))
           (if (> nexti end)
             (return (values nchars i))
             (setq nchars (1+ nchars) i nexti))))))
    :length-of-memory-encoding-function
    (nfunction
     utf-8-length-of-memory-encoding
     (lambda (pointer noctets start)
       (do* ((i start)
             (end (+ start noctets))
             (nchars 0 (1+ nchars)))
            ((= i end) (values nchars i))
         (let* ((code (%get-unsigned-byte pointer i))
                (nexti (+ i (cond ((< code #x80) 1)
                                  ((< code #xe0) 2)
                                  ((< code #xf0) 3)
                                  (t 4)))))
           (declare (type (unsigned-byte 8) code))
           (if (> nexti end)
             (return (values nchars i))
             (setq nchars (1+ nchars) i nexti))))))
    :literal-char-code-limit #x80
    )


;;; For a code-unit-size greater than 8: the stream-encode function's write-function
;;; accepts a code-unit in native byte order and swaps it if necessary and the
;;; stream-decode function receives a first-unit in native byte order and its
;;; next-unit-function returns a unit in native byte order.  The memory/vector
;;; functions have to do their own byte swapping.


(defmacro utf-16-combine-surrogate-pairs (a b)
  `(code-char
    (the (unsigned-byte 21)
      (+ #x10000
         (the (unsigned-byte 20)
           (logior
            (the (unsigned-byte 20) (ash (the (unsigned-byte 10)
                                           (- ,a #xd800))
                                         10))
            (the (unsigned-byte 10) (- ,b #xdc00))))))))
    
(defun utf-16-stream-encode (char write-function stream)
  (let* ((code (char-code char))
         (highbits (- code #x10000)))
    (declare (type (mod #x110000) code)
             (fixnum highbits))
    (if (< highbits 0)
      (progn
        (funcall write-function stream code)
        1)
      (progn
        (funcall write-function stream (logior #xd800 (the fixnum (ash highbits -10))))
        (funcall write-function (logior #xdc00 (the fixnum (logand highbits #x3ff))))
        2))))

(defun utf-16-stream-decode (1st-unit next-unit-function stream)
  (declare (type (unsigned-byte 16) 1st-unit))
  (if (or (< 1st-unit #xd800)
          (>= 1st-unit #xe000))
    (code-char 1st-unit)
    (if (< 1st-unit #xdc00)
      (let* ((2nd-unit (funcall next-unit-function stream)))
        (if (eq 2nd-unit :eof)
          2nd-unit
          (locally (declare (type (unsigned-byte 16) 2nd-unit))
            (if (and (>= 2nd-unit #xdc00)
                     (< 2nd-unit #xe000))
              (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)
              #\Replacement_Character))))
      #\Replacement_Character)))


(defun utf-16-octets-in-string (string start end)
  (if (>= end start)
    (do* ((noctets 0)
          (i start (1+ i)))
         ((= i end) noctets)
      (declare (fixnum noctets))
      (let* ((code (char-code (schar string i))))
        (declare (type (mod #x110000) code))
        (incf noctets
              (if (< code #x10000)
                2
                4))))
    0))


(declaim (inline %big-endian-u8-ref-u16 %little-endian-u8-ref-u16))
(defun %big-endian-u8-ref-u16 (u8-vector idx)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (logior (the (unsigned-byte 16) (ash (the (unsigned-byte 8) (aref u8-vector idx)) 8))
          (the (unsigned-byte 8) (aref u8-vector (the fixnum (1+ idx))))))

(defun %little-endian-u8-ref-u16 (u8-vector idx)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (logior (the (unsigned-byte 16) (ash (the (unsigned-byte 8)
                                         (aref u8-vector (the fixnum (1+ idx)))) 8))
          (the (unsigned-byte 8) (aref u8-vector idx))))

#+big-endian-target
(progn
(defmacro %native-u8-ref-u16 (vector idx)
  `(%big-endian-u8-ref-u16 ,vector ,idx))

(defmacro %reversed-u8-ref-u16 (vector idx)
  `(%little-endian-u8-ref-u16 ,vector ,idx))
)

#+little-endian-target
(progn
(defmacro %native-u8-ref-u16 (vector idx)
  `(%little-endian-u8-ref-u16 ,vector ,idx))

(defmacro %reversed-u8-ref-u16 (vector idx)
  `(%big-endian-u8-ref-u16 ,vector ,idx))
)


(declaim (inline (setf %big-endian-u8-ref-u16) (setf %little-endian-u8-ref-u16)))
(defun (setf %big-endian-u8-ref-u16) (val u8-vector idx)
  (declare (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (setf (aref u8-vector idx) (ldb (byte 8 8) val)
        (aref u8-vector (the fixnum (1+ idx))) (ldb (byte 8 0) val))
  val)

(defun (setf %little-endian-u8-ref-u16) (val u8-vector idx)
  (declare (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (setf (aref u8-vector idx) (ldb (byte 8 0) val)
        (aref u8-vector (the fixnum (1+ idx))) (ldb (byte 8 8) val))
  val)


;;; utf-16, native byte order.
(define-character-encoding #+big-endian-target :utf-16be #-big-endian-target :utf-16le
    #+big-endian-target
    "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
big-endian word and characters with larger codes can be encoded in a
pair of 16-bit big-endian words.  The endianness of the encoded data
is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
    #+little-endian-target
    "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word and characters with larger codes can be encoded in
a pair of 16-bit little-endian words.  The endianness of the encoded
data is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
    :max-units-per-char 2
    :code-unit-size 16
    :native-endianness t
    :stream-encode-function
    #'utf-16-stream-encode
    :stream-decode-function
    #'utf-16-stream-decode
    :vector-encode-function
    (nfunction
     native-utf-16-vector-encode
     (lambda (string vector idx start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (fixnum idx start end))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (declare (fixnum i))
         (let* ((char (schar string i))
                (code (char-code char))
                (highbits (- code #x10000)))
           (declare (type (mod #x110000) code)
                    (fixnum highbits))
           (cond ((< highbits 0)
                  (setf (%native-u8-ref-u16 vector idx) code)
                  (incf idx 2))
                 (t
                  (let* ((firstword (logior #xd800 (the fixnum (ash highbits -10))))
                         (secondword (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                    (declare (type (unsigned-byte 16) firstword secondword))
                    (setf (%native-u8-ref-u16 vector idx) firstword
                          (%native-u8-ref-u16 vector (the fixnum (+ idx 2))) secondword)
                    (incf idx 4))))))))
    :vector-decode-function
    (nfunction
     native-utf-16-vector-decode
     (lambda (vector idx noctets string)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (type index idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx))
            ((= index end) index)
         (declare (fixnum i len index))
         (let* ((1st-unit (%native-u8-ref-u16 vector index)))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (%native-u8-ref-u16 vector index)))
                         (declare (type (unsigned-byte 16) 2nd-unit))
                         (incf index 2)
                         (if (and (>= 2nd-unit #xdc00)
                                  (< 2nd-unit #xe000))
                           (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
             (setf (schar string i) (or char #\Replacement_Character)))))))
    :memory-encode-function
    (nfunction
     native-utf-16-memory-encode
     (lambda (string pointer idx start end)
       (declare (fixnum idx))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (let* ((code (char-code (schar string i)))
                (highbits (- code #x10000)))
           (declare (type (mod #x110000) code)
                  (fixnum  highbits))
         (cond ((< highbits 0)
                (setf (%get-unsigned-word pointer idx) code)
                (incf idx 2))
               (t
                (setf (%get-unsigned-word pointer idx) (logior #xd800 (the fixnum (ash highbits -10))))
                (incf idx 2)
                (setf (%get-unsigned-word pointer idx) (logior #xdc00 (the fixnum (logand highbits #x3ff))))
                (incf idx 2)))))))
    :memory-decode-function
    (nfunction
     native-utf-16-memory-decode
     (lambda (pointer noctets idx string)
       (declare (fixnum noctets idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx))
            ((>= index end) index)
         (declare (fixnum i index p))
         (let* ((1st-unit (%get-unsigned-word pointer index)))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (%get-unsigned-word pointer index)))
                           (declare (type (unsigned-byte 16) 2nd-unit))
                           (incf index)
                           (if (and (>= 2nd-unit #xdc00)
                                    (< 2nd-unit #xe000))
                             (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
            (setf (schar string i) (or char #\Replacement_Character)))))))
    :octets-in-string-function
    #'utf-16-octets-in-string
    :length-of-vector-encoding-function
    (nfunction
     native-utf-16-length-of-vector-encoding
     (lambda (vector start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector))
       (declare (fixnum start end))
       (do* ((i start)
             (j (+ 2 i) (+ 2 i))
             (nchars 0))
            ((> j end) (values nchars i))
         (declare (fixnum i j nchars))
         (let* ((code (%native-u8-ref-u16 vector i))
                (nexti (+ i (if (or (< code #xd800)
                                    (>= code #xdc00))
                              2
                              4))))
           (declare (type (unsigned-byte 16) code)
                    (fixnum nexti))
           (if (> nexti end)
             (return (values nchars i))
             (setq i nexti nchars (1+ nchars)))))))
    :length-of-memory-encoding-function
    (nfunction
     native-utf-16-length-of-memory-encoding
     (lambda (pointer noctets start)
       (do* ((i start)
             (j (+ i 2) (+ i 2))
             (end (+ start noctets))
             (nchars 0))
            ((> j end) (values nchars i))
         (let* ((code (%get-unsigned-word pointer i))
                (nexti (+ i (if (or (< code #xd800)
                                    (>= code #xdc00))
                              2
                              4))))
           (declare (type (unsigned-byte 16) code)
                    (fixnum nexti))
           (if (> nexti end)
             (return (values nchars i))
             (setq i nexti nchars (1+ nchars)))))))
    :literal-char-code-limit #x10000
    )

;;; utf-16, reversed byte order
(define-character-encoding #+big-endian-target :utf-16le #-big-endian-target :utf-16be
   #+little-endian-target
   "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
big-endian word and characters with larger codes can be encoded in a
pair of 16-bit big-endian words.  The endianness of the encoded data
is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
  #+big-endian-target
  "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word and characters with larger codes can be encoded in
a pair of 16-bit little-endian words.  The endianness of the encoded
data is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness nil
  :stream-encode-function
  #'utf-16-stream-encode
  :stream-decode-function
  #'utf-16-stream-decode
  :vector-encode-function
  (nfunction
   reversed-utf-16-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx start end))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (declare (fixnum i))
       (let* ((char (schar string i))
              (code (char-code char))
              (highbits (- code #x10000)))
         (declare (type (mod #x110000) code)
                  (fixnum highbits))
         (cond ((< highbits 0)
                (setf (%reversed-u8-ref-u16 vector idx) code)
                (incf idx 2))
               (t
                (let* ((firstword (logior #xd800 (the fixnum (ash highbits -10))))
                       (secondword (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                  (declare (type (unsigned-byte 16) firstword secondword))
                  (setf (%reversed-u8-ref-u16 vector idx) firstword
                        (%reversed-u8-ref-u16 vector (the fixnum (+ idx 2))) secondword)
                  (incf idx 4))))))))
  :vector-decode-function
  (nfunction
   reversed-utf-16-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx))
          ((= index end) index)
       (declare (fixnum i len index))
       (let* ((1st-unit (%reversed-u8-ref-u16 vector index)))
         (declare (type (unsigned-byte 16) 1st-unit))
         (incf index 2)
         (let* ((char
                 (if (or (< 1st-unit #xd800)
                         (>= 1st-unit #xe000))
                   (code-char 1st-unit)
                   (if (< 1st-unit #xdc00)
                     (let* ((2nd-unit (%reversed-u8-ref-u16 vector index)))
                       (declare (type (unsigned-byte 16) 2nd-unit))
                       (incf index 2)
                       (if (and (>= 2nd-unit #xdc00)
                                (< 2nd-unit #xe000))
                         (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
           (setf (schar string i) (or char #\Replacement_Character)))))))
  :memory-encode-function
  (nfunction
   reversed-utf-16-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (highbits (- code #x10000)))
         (declare (type (mod #x110000) code)
                  (fixnum  highbits))
         (cond ((< highbits 0)
                (setf (%get-unsigned-word pointer idx) (%swap-u16 code))
                (incf idx 2))
               (t
                (setf (%get-unsigned-word pointer idx) (%swap-u16 (logior #xd800 (the fixnum (ash highbits -10)))))
                (incf idx 2)
                (setf (%get-unsigned-word pointer idx) (%swap-u16 (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                (incf idx 2)))))))
  :memory-decode-function
  (nfunction
   reversed-utf-16-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx))
          ((>= index end) index)
       (declare (fixnum i index p))
       (let* ((1st-unit (%swap-u16 (%get-unsigned-word pointer index))))
         (declare (type (unsigned-byte 16) 1st-unit))
         (incf index 2)
         (let* ((char
                 (if (or (< 1st-unit #xd800)
                         (>= 1st-unit #xe000))
                   (code-char 1st-unit)
                   (if (< 1st-unit #xdc00)
                     (let* ((2nd-unit (%swap-u16 (%get-unsigned-word pointer index))))
                       (declare (type (unsigned-byte 16) 2nd-unit))
                       (incf index)
                       (if (and (>= 2nd-unit #xdc00)
                                (< 2nd-unit #xe000))
                         (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
           (setf (schar string i) (or char #\Replacement_Character)))))))
  :octets-in-string-function
  #'utf-16-octets-in-string
  :length-of-vector-encoding-function
  (nfunction
   reversed-utf-16-length-of-vector-encoding
   (lambda (vector start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (declare (fixnum start end))
     (do* ((i start)
           (j (+ 2 i) (+ 2 i))
           (nchars 0))
          ((> j end) (values nchars i))
       (declare (fixnum i j nchars))
       (let* ((code (%reversed-u8-ref-u16 vector i))
              (nexti (+ i (if (or (< code #xd800)
                                  (>= code #xdc00))
                            2
                            4))))
         (declare (type (unsigned-byte 16) code)
                  (fixnum nexti))
         (if (> nexti end)
           (return (values nchars i))
           (setq i nexti nchars (1+ nchars)))))))
  :length-of-memory-encoding-function
  (nfunction
   reversed-utf-16-length-of-memory-encoding
   (lambda (pointer noctets start)
     (do* ((i start)
           (j (+ i 2) (+ i 2))
           (end (+ start noctets))
           (nchars 0))
          ((> j end) (values nchars i))
       (let* ((code (%swap-u16 (%get-unsigned-word pointer i)))
              (nexti (+ i (if (or (< code #xd800)
                                  (>= code #xdc00))
                            2
                            4))))
         (declare (type (unsigned-byte 16) code)
                  (fixnum nexti))
         (if (> nexti end)
           (return (values nchars i))
           (setq i nexti nchars (1+ nchars)))))))
  :literal-char-code-limit #x10000
  )

;;; UTF-16.  Memory and vector functions determine endianness of
;;; input by the presence of a byte-order mark (or swapped BOM)
;;; at the beginning of input, and assume big-endian order
;;; if this mark is missing; on output, a BOM is prepended and
;;; things are written in native byte order.
;;; The endianness of stream-io operations is determined by
;;; stream content; new output streams are written in native
;;; endianness with a BOM character prepended.  Input streams
;;; are read in native byte order if the initial character is
;;; a BOM, in reversed byte order if the initial character is
;;; a swapped BOM, and in big-endian order (per RFC 2781) if
;;; there is no BOM.

(define-character-encoding :utf-16
    "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
word and characters with larger codes can be encoded in a
pair of 16-bit words.  The endianness of the encoded data is
indicated by the endianness of a byte-order-mark character (#\u+feff)
prepended to the data; in the absence of such a character on input,
the data is assumed to be in big-endian order."    
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness t                  ;not necessarily true.
  :stream-encode-function
  #'utf-16-stream-encode
  :stream-decode-function
  #'utf-16-stream-decode
  :vector-encode-function
  (nfunction
   utf-16-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (when (> end start)
       (setf (%native-u8-ref-u16 vector idx) byte-order-mark-char-code)
       (incf idx 2))
            (do* ((i start (1+ i)))
            ((>= i end) idx)
         (declare (fixnum i))
         (let* ((char (schar string i))
                (code (char-code char))
                (highbits (- code #x10000)))
           (declare (type (mod #x110000) code)
                    (fixnum highbits))
           (cond ((< highbits 0)
                  (setf (%native-u8-ref-u16 vector idx) code)
                  (incf idx 2))
                 (t
                  (let* ((firstword (logior #xd800 (the fixnum (ash highbits -10))))
                         (secondword (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                    (declare (type (unsigned-byte 16) firstword secondword))
                    (setf (%native-u8-ref-u16 vector idx) firstword
                          (%native-u8-ref-u16 vector (the fixnum (+ idx 2))) secondword)
                    (incf idx 4))))))))
  :vector-decode-function
  (nfunction
   utf-16-vector-decode 
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 16) (*)) vector)
              (type index idx))
     (let* ((swap (if (>= noctets 2)
                    (case (%native-u8-ref-u16 vector idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2) nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2) t)
                      (t #+little-endian-target t)))))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx))
            ((= index end) index)
         (declare (fixnum i len index))
         (let* ((1st-unit (if swap
                            (%reversed-u8-ref-u16 vector index)
                            (%native-u8-ref-u16 vector index))))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (if swap
                                          (%reversed-u8-ref-u16 vector index)
                                          (%native-u8-ref-u16 vector index))))
                         (declare (type (unsigned-byte 16) 2nd-unit))
                         (incf index 2)
                         (if (and (>= 2nd-unit #xdc00)
                                  (< 2nd-unit #xe000))
                           (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
             (setf (schar string i) (or char #\Replacement_Character))))))))
  :memory-encode-function
  (nfunction
   utf-16-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (when (> end start)
       (setf (%get-unsigned-word pointer idx)
             byte-order-mark-char-code)
       (incf idx 2))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (highbits (- code #x10000)))
         (declare (type (mod #x110000) code)
                  (fixnum p highbits))
         (cond ((< highbits 0)
                (setf (%get-unsigned-word pointer idx) code)
                (incf idx 2))
               (t
                (setf (%get-unsigned-word pointer idx) (logior #xd800 (the fixnum (ash highbits -10))))

                (setf (%get-unsigned-word pointer (the fixnum (+ idx 2))) (logior #xdc00 (the fixnum (logand highbits #x3ff))))
                (incf idx 4)))))))
  :memory-decode-function
  (nfunction
   utf-16-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum nunits idx))
     (let* ((swap (when (> noctets 1)
                    (case (%get-unsigned-word pointer idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx ))
            ((>= index end) index)
         (declare (fixnum i index p))
         (let* ((1st-unit (%get-unsigned-word pointer index)))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (if swap (setq 1st-unit (%swap-u16 1st-unit)))
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (%get-unsigned-byte pointer index)))
                         (declare (type (unsigned-byte 16) 2nd-unit))
                         (if swap (setq 2nd-unit (%swap-u16 2nd-unit)))
                         (incf index 2)
                         (if (and (>= 2nd-unit #xdc00)
                                  (< 2nd-unit #xe000))
                           (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
             (setf (schar string i) (or char #\Replacement_Character))))))))
  :octets-in-string-function
  #'(lambda (&rest args)
      (declare (dynamic-extent args))
      ;; Add two for the BOM.
      (+ 2 (apply #'utf-16-octets-in-string args)))
  :length-of-vector-encoding-function
  (nfunction
   utf-16-length-of-vector-encoding
   (lambda (vector start end)
     (declare (type (simple-array (unsigned-byte 16) (*)) vector))
     (let* ((swap (when (> end start)
                    (case (%native-u8-ref-u16 vector start)
                      (#.byte-order-mark-char-code
                       (incf start 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf start 2)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i start)
             (j (+ 2 i) (+ 2 i))
             (nchars 0))
            ((> j end)
             (if (= i end) (values nchars i))
             (let* ((code (if swap
                            (%reversed-u8-ref-u16 vector i)
                            (%native-u8-ref-u16 vector i)))
                    (nexti (+ i (if (or (< code #xd800)
                                        (>= code #xdc00))
                                  2
                                  4))))
               (declare (type (unsigned-byte 16) code)
                        (fixnum nexti))
               (if (> nexti end)
                 (return (values nchars i))
                 (setq i nexti nchars (1+ nchars)))))))))
  :length-of-memory-encoding-function
  (nfunction
   utf-16-length-of-memory-encoding
   (lambda (pointer noctets start)
     (let* ((swap (when (>= noctets 2)
                    (case (%get-unsigned-word pointer (+ start start))
                      (#.byte-order-mark-char-code
                       (incf start 2)
                       (decf noctets 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf start 2)
                       (decf noctets 2)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i start)
             (nchars 0 (1+ nchars)))
            ((>= i noctets)
             (if (= i noctets) nchars))
         (let* ((code (%get-unsigned-word pointer i)))
           (declare (type (unsigned-byte 16) code))
           (if swap (setq code (%swap-u16 code)))
           (incf i
                 (if (or (< code #xd800)
                         (>= code #xdc00))
                   2
                   4)))))))
  :literal-char-code-limit #x10000
  :use-byte-order-mark
  #+big-endian-target :utf-16le
  #+little-endian-target :utf-16be
  )


(defun ucs-2-stream-encode (char write-function stream)
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (>= code #x10000)
      (setq code (char-code #\Replacement_Character)))
    (funcall write-function stream code)
    1))

(defun ucs-2-stream-decode (1st-unit next-unit-function stream)
  (declare (type (unsigned-byte 16) 1st-unit)
           (ignore next-unit-function stream))
  ;; CODE-CHAR returns NIL on either half of a surrogate pair.
  (or (code-char 1st-unit)
      #\Replacement_Character))


(defun ucs-2-octets-in-string (string start end)
  (declare (ignore string))
  (if (>= end start)
    (* 2 (- end start))
    0))


;;; UCS-2, native byte order
(define-character-encoding #+big-endian-target :ucs-2be #-big-endian-target :ucs-2le
  #+big-endian-target
  "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word. The encoded data is implicitly big-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  #+little-endian-target
  "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word. The encoded data is implicitly little-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  :max-units-per-char 1
  :code-unit-size 16
  :native-endianness t
  :stream-encode-function
  #'ucs-2-stream-encode
  :stream-decode-function
  #'ucs-2-stream-decode
  :vector-encode-function
  (nfunction
   native-ucs-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (when (>= code #x10000)
           (setq code (char-code #\Replacement_Character)))
         (setf (%native-u8-ref-u16 vector idx) code)
         (incf idx 2)))))
  :vector-decode-function
  (nfunction
   native-ucs-2-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx (+ 2 index)))
          ((>= index end) index)
       (declare (fixnum i len index))
       (setf (schar string i)
             (or (code-char (%native-u8-ref-u16 vector index))
                 #\Replacement_Character)))))
  :memory-encode-function
  (nfunction
   native-ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
                      (if (>= code #x10000)
                        (char-code #\Replacement_Character)
                        code))
         (incf idx 2)))))
  :memory-decode-function
  (nfunction
   native-ucs-2-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (index idx (+ index 2)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%get-unsigned-word pointer index)))
         (declare (type (unsigned-byte 16) 1st-unit))
         (setf (schar string i) (or (char-code 1st-unit) #\Replacement_Character))))))
  :octets-in-string-function
  #'ucs-2-octets-in-string
  :length-of-vector-encoding-function
  (nfunction
   native-ucs-2-length-of-vector-encoding
   (lambda (vector start end)
     (declare (ignore vector))
     (do* ((i start (1+ i))
           (j (+ i 2) (+ i 2))
           (nchars 0 (1+ nchars)))
          ((> j end) (values nchars i)))))
  :length-of-memory-encoding-function
  (nfunction
   native-ucs-2-length-of-memory-encoding
   (lambda (pointer noctets start)
     (declare (ignore pointer))
     (values (floor noctets 2) (+ start noctets))))
  :literal-char-code-limit #x10000
  )

;;; UCS-2, reversed byte order
(define-character-encoding #+big-endian-target :ucs-2le #-big-endian-target :ucs-2be
  #+little-endian-target
  "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word. The encoded data is implicitly big-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  #+big-endian-target
  "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word. The encoded data is implicitly little-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  :max-units-per-char 1
  :code-unit-size 16
  :native-endianness nil
  :stream-encode-function
  #'ucs-2-stream-encode
  :stream-decode-function
  #'ucs-2-stream-decode
  :vector-encode-function
  (nfunction
   reversed-ucs-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (when (>= code #x10000)
           (setq code (char-code #\Replacement_Character)))
         (setf (%reversed-u8-ref-u16 vector idx) code)
         (incf idx 2)))))
  :vector-decode-function
  (nfunction
   reversed-ucs-2-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx (+ 2 index)))
          ((>= index end) index)
       (declare (fixnum i len index))
       (setf (schar string i)
             (or (code-char (%reversed-u8-ref-u16 vector index))
                 #\Replacement_Character)))))
  :memory-encode-function
  (nfunction
   reversed-ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
               (if (>= code #x10000)
                 (%swap-u16 (char-code #\Replacement_Character))
                 (%swap-u16 code)))
         (incf idx 2)))))
  :memory-decode-function
  (nfunction
   reversed-ucs-2-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (index idx (+ index 2)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%swap-u16 (%get-unsigned-word pointer index))))
         (declare (type (unsigned-byte 16) 1st-unit))
         (setf (schar string i) (or (char-code 1st-unit) #\Replacement_Character))))))
  :octets-in-string-function
  #'ucs-2-octets-in-string
  :length-of-vector-encoding-function
  (nfunction
   reversed-ucs-2-length-of-vector-encoding
   (lambda (vector start end)
     (declare (ignore vector))
     (do* ((i start (1+ i))
           (j (+ i 2) (+ i 2))
           (nchars 0 (1+ nchars)))
          ((> j end) (values nchars i)))))
  :length-of-memory-encoding-function
  (nfunction
   reversed-ucs-2-length-of-memory-encoding
   (lambda (pointer noctets start)
     (declare (ignore pointer))
     (values (floor noctets 2) (+ start noctets))))
  :literal-char-code-limit #x10000
  )

(define-character-encoding :ucs-2
    "A 16-bit, fixed-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit words.
The endianness of the encoded data is indicated by the endianness of a
byte-order-mark character (#\u+feff) prepended to the data; in the
absence of such a character on input, the data is assumed to be in
big-endian order."
  :max-units-per-char 1
  :code-unit-size 16
  :native-endianness t                  ;not necessarily true.
  :stream-encode-function
  #'ucs-2-stream-encode
  :stream-decode-function
  #'ucs-2-stream-decode
  :vector-encode-function
  (nfunction
   ucs-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (when (> end start)
       (setf (%native-u8-ref-u16 vector idx) byte-order-mark-char-code)
       (incf idx 2))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (when (>= code #x10000)
           (setq code (char-code #\Replacement_Character)))
         (setf (%native-u8-ref-u16 vector idx) code)
         (incf idx 2)))))
  :vector-decode-function
  (nfunction
   ucs-2-vector-decode 
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx)
              (fixnum noctets))
     (let* ((swap (if (> noctets 1)
                    (case (%native-u8-ref-u16 vector idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2) (decf noctets 2) nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2) (decf noctets 2) t)
                       (t #+little-endian-target t)))))

       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((>= index end) index)
         (declare (fixnum i len index))
         (let* ((1st-unit (if swap
                            (%reversed-u8-ref-u16 vector index)
                            (%native-u8-ref-u16 vector index))))
             (declare (type (unsigned-byte 16) 1st-unit))
             (setf (schar string i) (or (code-char 1st-unit) #\Replacement_Character)))))))
  :memory-encode-function
  (nfunction
   ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (when (> end start)
       (setf (%get-unsigned-word pointer idx)
             byte-order-mark-char-code)
       (incf idx 2))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
                      (if (>= code #x10000)
                        (char-code #\Replacement_Character)
                        code))
         (incf idx 2)))))
  :memory-decode-function
  (nfunction
   ucs-2-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (let* ((swap (when (> noctets 1)
                    (case (%get-unsigned-word pointer idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i 0 (1+ i))
           (index idx (+ index 2)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%get-unsigned-word pointer index)))
         (declare (type (unsigned-byte 16) 1st-unit))
         (if swap (setq 1st-unit (%swap-u16 1st-unit)))
         (setf (schar string i) (or (code-char 1st-unit) #\Replacement_Character)))))))
  :octets-in-string-function
  #'(lambda (&rest args)
      (declare (dynamic-extent args))
      ;; Add two for the BOM.
      (+ 2 (apply #'ucs-2-octets-in-string args)))
  :length-of-vector-encoding-function
  (nfunction
   ucs-2-length-of-vector-encoding
   (lambda (vector start end)
     (declare (ignore vector))
     (do* ((i start (1+ i))
           (j (+ i 2) (+ i 2))
           (nchars 0 (1+ nchars)))
          ((> j end) (values nchars i)))))
  :length-of-memory-encoding-function
  (nfunction
   ucs-2-length-of-memory-encoding
   (lambda (pointer noctets start)
     (when (> noctets 1)
       (case (%get-unsigned-word pointer )
         (#.byte-order-mark-char-code
          (incf start 2)
          (decf noctets 2))
         (#.swapped-byte-order-mark-char-code
          (incf start)
          (decf noctets))))
     (values (floor noctets 2) (+ start noctets))))
  :literal-char-code-limit #x10000
  :use-byte-order-mark
  #+big-endian-target :ucs-2le
  #+little-endian-target :ucs-2be
  )

(defun describe-character-encoding (name)
  (let* ((enc (lookup-character-encoding name)))
    (when enc
      (let* ((name (character-encoding-name enc))
             (doc (character-encoding-documentation enc))
             (aliases (character-encoding-aliases enc)))
        (format t "~&~s" name)
        (when (null (car aliases))
          (pop aliases))
        (when aliases
          (format t " [Aliases:~{ ~s~}]" aliases))
        (format t "~&~a~%~%"  doc)
        (values)))))
      
(defun describe-character-encodings ()
  (let* ((names nil))
    (maphash #'(lambda (name enc)
                 (when (eq name (character-encoding-name enc))
                   (push name names)))
             *character-encodings*)
    (dolist (name (sort names #'string<) (values))
      (describe-character-encoding name))))

(defmethod make-load-form ((c character-encoding) &optional environment)
  (declare (ignore environment))
  `(get-character-encoding ,(character-encoding-name c)))

(defun cstring-encoded-length-in-bytes (encoding string start end)
  (+ 1                             ; NULL terminator
     (funcall (character-encoding-octets-in-string-function encoding)
              string
              (or start 0)
              (or end (length string)))))

(defun encode-string-to-memory (encoding pointer offset string start end)
  (funcall (character-encoding-memory-encode-function encoding)
           string pointer offset (or start 0) (or end (length string))))
