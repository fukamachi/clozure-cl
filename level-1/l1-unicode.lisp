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

(defun lookup-character-encoding (name)
  (gethash name *character-encodings*))

(defun get-character-encoding (name)
  (or (lookup-character-encoding name)
      (error "Unknown character encoding: ~s." name)))

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
  ;; encoded.  (Note that the index args are and return value
  ;; are "code unit indices", not "byte offsets".)
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

  ;; Does a byte-order-mark determine the endianness of input ?
  ;; Should we prepend a BOM to output ?
  ;; If non-nil, the value should be a cons:
  ;; (native-byte-order-encoding . swapped-byte-order-encoding)
  (use-byte-order-mark nil)
  )

(defconstant byte-order-mark #\u+feff)
(defconstant byte-order-mark-char-code (char-code byte-order-mark))
(defconstant swapped-byte-order-mark #\u+fffe)
(defconstant swapped-byte-order-mark-char-code (char-code swapped-byte-order-mark))



(defmethod print-object ((ce character-encoding) stream)
  (print-unreadable-object (ce stream :type t :identity t)
    (format stream "~a" (character-encoding-name ce))))

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
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (when (and (< code 256)
                  (< idx (the fixnum (length vector))))
         (setf (aref vector idx) code)
         (the fixnum (1+ idx))))))
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
       (when c2
         (funcall write-function stream code)
         1))))
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
   (lambda (char vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (let* ((code (char-code char))
            (c2 (when (< idx (the fixnum (length vector)))
                  (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-00a0-0180-to-iso8859-2*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2c0) (< code #x2e0))
                         (svref *unicode-00c0-00e0-to-iso8859-2*
                                (the fixnum (- code #x2c0))))))))
       (declare (type (mod #x110000) code))
       (when c2
         (setf (aref vector idx) c2)
         (the fixnum (1+ idx))))))
  :vector-decode-function
  (nfunction
   iso-8859-2-vector-decode
   (lambda (vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (if (< idx (length vector))
       (let* ((1st-unit (aref vector idx)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (values
          (if (< 1st-unit #xa0)
            (code-char 1st-unit)
            (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0))))
          (the fixnum (1+ (the fixnum idx)))))
       (values nil idx))))
  :memory-encode-function
  (nfunction
   iso-8859-2-memory-encode
   (lambda (char pointer idx)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-00a0-0180-to-iso8859-2*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2c0) (< code #x2e0))
                         (svref *unicode-00c0-00e0-to-iso8859-2*
                                (the fixnum (- code #x2c0)))))))
       (declare (type (mod #x110000) code))
       (when c2
         (setf (%get-unsigned-byte pointer idx) c2)
         (1+ idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-2-memory-decode
   (lambda (pointer idx)
     (let* ((1st-unit (%get-unsigned-byte pointer idx)))
       (declare (type (unsigned-byte 8) 1st-unit))
       (values (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0))))
               (the fixnum (1+ (the fixnum idx)))))))
  :units-in-string-function
  (nfunction
   iso-8859-2-units-in-string
   (lambda (string &optional (start 0) (end (length string)))
     (when (>= end start)
       (do* ((i start (1+ i)))
            ((= i end) (- end start))
         (let* ((code (char-code (schar string i)))
                (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-00a0-0180-to-iso8859-2*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2c0) (< code #x2e0))
                         (svref *unicode-00c0-00e0-to-iso8859-2*
                                (the fixnum (- code #x2c0)))))))
           (declare (type (mod #x110000) code))
           (unless c2 (return nil)))))))
  :length-of-vector-encoding-function
  (nfunction
   iso-8859-2-length-of-vector-encoding
   (lambda (vector &optional (start 0) (end (length vector)))
     (when (>= end start)
       (- end start))))
  :length-of-memory-encoding-function 
  (nfunction
   iso-8859-2-length-of-memory-encoding
   (lambda (pointer nunits &optional start)
     (declare (ignore pointer start))
     nunits))
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
       (when c2
         (funcall write-function stream code)
         1))))
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
   (lambda (char vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (let* ((code (char-code char))
            (c2 (when (< idx (the fixnum (length vector)))
                  (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso8859-3*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x108) (< code #x180))
                       (svref *unicode-108-180-to-iso8859-3*
                              (the fixnum (- code #x108))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2d8-2e0-to-iso8859-3*
                              (the fixnum (- code #x2d8))))))))
       (declare (type (mod #x110000) code))
       (when c2
         (setf (aref vector idx) c2)
         (the fixnum (1+ idx))))))
  :vector-decode-function
  (nfunction
   iso-8859-3-vector-decode
   (lambda (vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (if (< idx (length vector))
       (let* ((1st-unit (aref vector idx)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (values
          (if (< 1st-unit #xa0)
            (code-char 1st-unit)
            (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0))))
          (the fixnum (1+ (the fixnum idx)))))
       (values nil idx))))
  :memory-encode-function
  (nfunction
   iso-8859-3-memory-encode
   (lambda (char pointer idx)
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
       (when c2
         (setf (%get-unsigned-byte pointer idx) c2)
         (1+ idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-3-memory-decode
   (lambda (pointer idx)
     (let* ((1st-unit (%get-unsigned-byte pointer idx)))
       (declare (type (unsigned-byte 8) 1st-unit))
       (values (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0))))
               (the fixnum (1+ (the fixnum idx)))))))
  :units-in-string-function
  (nfunction
   iso-8859-1-units-in-string
   (lambda (string &optional (start 0) (end (length string)))
     (when (>= end start)
       (do* ((i start (1+ i)))
            ((= i end) (- end start))
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
           (unless c2 (return nil)))))))
  :length-of-vector-encoding-function
  (nfunction
   iso-8859-3-length-of-vector-encoding
   (lambda (vector &optional (start 0) (end (length vector)))
     (when (>= end start)
       (- end start))))
  :length-of-memory-encoding-function 
  (nfunction
   iso-8859-3-length-of-memory-encoding
   (lambda (pointer nunits &optional start)
     (declare (ignore pointer start))
     nunits))
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
       (when c2
         (funcall write-function stream code)
         1))))
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
   (lambda (char vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (let* ((code (char-code char))
            (c2 (when (< idx (the fixnum (length vector)))
                  (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso8859-4*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-iso8859-4*
                              (the fixnum (- code #x2c0))))))))
       (declare (type (mod #x110000) code))
       (when c2
         (setf (aref vector idx) c2)
         (the fixnum (1+ idx))))))
  :vector-decode-function
  (nfunction
   iso-8859-4-vector-decode
   (lambda (vector idx)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (if (< idx (length vector))
       (let* ((1st-unit (aref vector idx)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (values
          (if (< 1st-unit #xa0)
            (code-char 1st-unit)
            (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0))))
          (the fixnum (1+ (the fixnum idx)))))
       (values nil idx))))
  :memory-encode-function
  (nfunction
   iso-8859-4-memory-encode
   (lambda (char pointer idx)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso8859-4*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-iso8859-4*
                              (the fixnum (- code #x2c0)))))))
       (declare (type (mod #x110000) code))
       (when c2
         (setf (%get-unsigned-byte pointer idx) c2)
         (1+ idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-4-memory-decode
   (lambda (pointer idx)
     (let* ((1st-unit (%get-unsigned-byte pointer idx)))
       (declare (type (unsigned-byte 8) 1st-unit))
       (values (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0))))
               (the fixnum (1+ (the fixnum idx)))))))
  :units-in-string-function
  (nfunction
   iso-8859-1-units-in-string
   (lambda (string &optional (start 0) (end (length string)))
     (when (>= end start)
       (do* ((i start (1+ i)))
            ((= i end) (- end start))
         (let* ((code (char-code (schar string i)))
                (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso8859-4*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-iso8859-4*
                              (the fixnum (- code #x2c0))))) ))
           (declare (type (mod #x110000) code))
           (unless c2 (return nil)))))))
  :length-of-vector-encoding-function
  (nfunction
   iso-8859-4-length-of-vector-encoding
   (lambda (vector &optional (start 0) (end (length vector)))
     (when (>= end start)
       (- end start))))
  :length-of-memory-encoding-function 
  (nfunction
   iso-8859-4-length-of-memory-encoding
   (lambda (pointer nunits &optional start)
     (declare (ignore pointer start))
     nunits))
  :literal-char-code-limit #xa0
  )

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
             (the fixnum (+ index 1)))
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
                       (the fixnum (+ i3 1))))))))))))
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
         (declare (type (mod #x110000) code)
                  (fixnum i1 i2 i3))
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

;;; For a code-unit-size greater than 8: the stream-encode function's write-function
;;; accepts a code-unit in native byte order and swaps it if necessary and the
;;; stream-decode function receives a first-unit in native byte order and its
;;; next-unit-function returns a unit in native byte order.  The memory/vector
;;; functions have to do their own byte swapping.


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
              (code-char (the (unsigned-byte 21)
                           (logior
                            (the (unsigned-byte 20) (ash (the (unsigned-byte 10)
                                                           (- 1st-unit #xd800))
                                                         10))
                            (the (unsigned-byte 10) (- 2nd-unit #xdc00))))))))))))


(defun utf-16-units-in-string (string &optional (start 0) (end (length string)))
  (when (>= end start)
    (do* ((nunits 0)
          (i start (1+ i)))
         ((= i end) nunits)
      (declare (fixnum nunits))
      (let* ((code (char-code (schar string i))))
        (declare (type (mod #x110000) code))
        (incf nunits
              (if (< code #x10000)
                1
                2))))))

;;; utf-16, native byte order.
(define-character-encoding
    #+big-endian-target :utf-16be #-big-endian-target :utf-16le
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
     (lambda (char vector index)
       (declare (type (simple-array (unsigned-byte 16) (*)) vector)
                (type index index)
                (optimize (speed 3) (safety 0)))
       (let* ((len (length vector))
              (code (char-code char))
              (highbits (- code #x10000)))
         (declare (type index len)
                  (type (mod #x110000) code)
                  (fixnum highbits))
         (if (< highbits 0)
           (when (< index len)
             (setf (aref vector index) code)
             (the fixnum (+ index 1)))           
           (let* ((i1 (1+ index)))
             (declare (fixnum i1))
             (when (< i1 len)
               (setf (aref vector index) (logior #xd800 (the fixnum (ash highbits -10)))
                     (aref vector i1) (logior #xdc00 (the fixnum (logand highbits #x3ff))))
               (the fixnum (1+ i1))))))))
    :vector-decode-function
    (nfunction
     native-utf-16-vector-decode
     (lambda (vector idx)
       (declare (type (simple-array (unsigned-byte 16) (*)) vector)
                (type index idx))
       (let* ((len (length vector)))
         (declare (fixnum len))
         (if (>= idx len)
           (values nil idx)
           (let* ((1st-unit (aref vector idx)))
             (declare (type (unsigned-byte 16) 1st-unit))
             (if (or (< 1st-unit #xd800)
                     (>= 1st-unit #xe000))
               (values (code-char 1st-unit)
                       (the fixnum (1+ idx)))
               (if (>= 1st-unit #xdc00)
                 (values nil idx)
                 (let* ((i1 (1+ idx)))
                   (declare (fixnum i1))
                   (if (>= i1 len)
                     (values nil idx)
                     (let* ((2nd-unit (aref vector i1)))
                       (declare (type (unsigned-byte 16) 2nd-unit))
                       (if (and (>= 2nd-unit #xdc00)
                                (< 2nd-unit #xe000))
                         (values
                          (code-char (the (unsigned-byte 21)
                                       (logior
                                        (the (unsigned-byte 20) (ash (the (unsigned-byte 10)
                                                                       (- 1st-unit #xd800))
                                                                     10))
                                        (the (unsigned-byte 10) (- 2nd-unit #xdc00)))))
                          (the fixnum (1+ i1)))
                         (values nil idx))))))))))))
    :memory-encode-function
    (nfunction
     native-utf-16-memory-encode
     (lambda (char pointer idx)
       (declare (fixnum idx))
       (let* ((code (char-code char))
              (highbits (- code #x10000))
              (i0 (+ idx idx))
              (i1 (+ i0 2)))
         (declare (type (mod #x110000) code)
                  (fixnum i0 i1 highbits))
         (cond ((< highbits 0)
                (setf (%get-unsigned-word pointer i0) code)
                (the fixnum (1+ idx)))

               (t
                (setf (%get-unsigned-word pointer i0) (logior #xd800 (the fixnum (ash highbits -10)))
                      (%get-unsigned-word pointer i1) (logior #xdc00 (the fixnum (logand highbits #x3ff))))

                (the fixnum (+ idx 2)))))))
    :memory-decode-function
    (nfunction
     native-utf-16-memory-decode
     (lambda (pointer idx)
       (declare (fixnum idx))
       (let* ((i0 (+ idx idx))
              (1st-unit (%get-unsigned-word pointer i0))
              (i1 (+ i0 2)))
         (declare (type (unsigned-byte 16) 1st-unit)
                  (fixnum i1 i2 i3))
         (if (or (< 1st-unit #xd800)
                 (>= 1st-unit #xe000))
           (values (code-char 1st-unit) (the fixnum (1+ idx)))
           (if (< 1st-unit #xdc00)
             (let* ((2nd-unit (%get-unsigned-word pointer i1)))
               (declare (type (unsigned-byte 16) 2nd-unit))
               (if (and (>= 2nd-unit #xdc00)
                        (< 2nd-unit #xe000))
                 (values
                  (code-char (the (unsigned-byte 21)
                               (logior
                                (the (unsigned-byte 20) (ash (the (unsigned-byte 10)
                                                               (- 1st-unit #xd800))
                                                             10))
                                (the (unsigned-byte 10) (- 2nd-unit #xdc00)))))
                  (the fixnum (+ idx 2))))))))))
    :units-in-string-function
    #'utf-16-units-in-string
    :length-of-vector-encoding-function
    (nfunction
     native-utf-16-length-of-vector-encoding
     (lambda (vector &optional (start 0) (end (length vector)))
       (declare (type (simple-array (unsigned-byte 16) (*)) vector))
       (do* ((i start)
             (nchars 0 (1+ nchars)))
            ((>= i end)
             (if (= i end) nchars))
         (let* ((code (aref vector i)))
           (declare (type (unsigned-byte 8) code))
           (incf i
                 (if (or (< code #xd800)
                         (>= code #xe000))
                   1
                   2))))))
    :length-of-memory-encoding-function
    (nfunction
     native-utf-8-length-of-memory-encoding
     (lambda (pointer nunits &optional (start 0))
       (do* ((i start)
             (p (+ start start) (+ p 2))
             (nchars 0 (1+ nchars)))
            ((>= i nunits)
             (if (= i nunits) nchars))
         (let* ((code (%get-unsigned-word pointer p)))
           (declare (type (unsigned-byte 16) code))
           (incf i
                 (incf i
                       (if (or (< code #xd800)
                               (>= code #xe000))
                         1
                         2)))))))
    :literal-char-code-limit #x10000
    )

;;; utf-16, reversed byte order
(define-character-encoding
    #+big-endian-target :utf-16le #-big-endian-target :utf-16be
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
     (lambda (char vector index)
       (declare (type (simple-array (unsigned-byte 16) (*)) vector)
                (type index index)
                (optimize (speed 3) (safety 0)))
       (let* ((len (length vector))
              (code (char-code char))
              (highbits (- code #x10000)))
         (declare (type index len)
                  (type (mod #x110000) code)
                  (fixnum highbits))
         (if (< highbits 0)
           (when (< index len)
             (setf (aref vector index) (%swap-u16 code))
             (the fixnum (+ index 1)))           
           (let* ((i1 (1+ index)))
             (declare (fixnum i1))
             (when (< i1 len)
               (setf (aref vector index)
                     (%swap-u16 (logior #xd800 (the fixnum (ash highbits -10))))
                     (aref vector i1)
                     (%swap-u16 (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
               (the fixnum (1+ i1))))))))
    :vector-decode-function
    (nfunction
     reversed-utf-16-vector-decode
     (lambda (vector idx)
       (declare (type (simple-array (unsigned-byte 16) (*)) vector)
                (type index idx))
       (let* ((len (length vector)))
         (declare (fixnum len))
         (if (>= idx len)
           (values nil idx)
           (let* ((1st-unit (%swap-u16 (aref vector idx))))
             (declare (type (unsigned-byte 16) 1st-unit))
             (if (or (< 1st-unit #xd800)
                     (>= 1st-unit #xe000))
               (values (code-char 1st-unit)
                       (the fixnum (1+ idx)))
               (if (>= 1st-unit #xdc00)
                 (values nil idx)
                 (let* ((i1 (1+ idx)))
                   (declare (fixnum i1))
                   (if (>= i1 len)
                     (values nil idx)
                     (let* ((2nd-unit (%swap-u16 (aref vector i1))))
                       (declare (type (unsigned-byte 16) 2nd-unit))
                       (if (and (>= 2nd-unit #xdc00)
                                (< 2nd-unit #xe000))
                         (values
                          (code-char (the (unsigned-byte 21)
                                       (logior
                                        (the (unsigned-byte 20) (ash (the (unsigned-byte 10)
                                                                       (- 1st-unit #xd800))
                                                                     10))
                                        (the (unsigned-byte 10) (- 2nd-unit #xdc00)))))
                          (the fixnum (1+ i1)))
                         (values nil idx))))))))))))
    :memory-encode-function
    (nfunction
     reversed-utf-16-memory-encode
     (lambda (char pointer idx)
       (declare (fixnum idx))
       (let* ((code (char-code char))
              (highbits (- code #x10000))
              (i0 (+ idx idx))
              (i1 (+ i0 2)))
         (declare (type (mod #x110000) code)
                  (fixnum i0 i1 highbits))
         (cond ((< highbits 0)
                (setf (%get-unsigned-word pointer i0) (%swap-u16 code))
                (the fixnum (1+ idx)))
               (t
                (setf (%get-unsigned-word pointer i0)
                      (%swap-u16 (logior #xd800 (the fixnum (ash highbits -10))))
                      (%get-unsigned-word pointer i1)
                      (%swap-u16 (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                (the fixnum (+ idx 2)))))))
    :memory-decode-function
    (nfunction
     reversed-utf-16-memory-decode
     (lambda (pointer idx)
       (declare (fixnum idx))
       (let* ((i0 (+ idx idx))
              (1st-unit (%swap-u16 (%get-unsigned-word pointer i0)))
              (i1 (+ i0 2)))
         (declare (type (unsigned-byte 16) 1st-unit)
                  (fixnum i1 i2 i3))
         (if (or (< 1st-unit #xd800)
                 (>= 1st-unit #xe000))
           (values (code-char 1st-unit) (the fixnum (1+ idx)))
           (if (< 1st-unit #xdc00)
             (let* ((2nd-unit (%swap-u16 (%get-unsigned-word pointer i1))))
               (declare (type (unsigned-byte 16) 2nd-unit))
               (if (and (>= 2nd-unit #xdc00)
                        (< 2nd-unit #xe000))
                 (values
                  (code-char (the (unsigned-byte 21)
                               (logior
                                (the (unsigned-byte 20) (ash (the (unsigned-byte 10)
                                                               (- 1st-unit #xd800))
                                                             10))
                                (the (unsigned-byte 10) (- 2nd-unit #xdc00)))))
                  (the fixnum (+ idx 2))))))))))
    :units-in-string-function
    #'utf-16-units-in-string
    :length-of-vector-encoding-function
    (nfunction
     reversed-utf-16-length-of-vector-encoding
     (lambda (vector &optional (start 0) (end (length vector)))
       (declare (type (simple-array (unsigned-byte 16) (*)) vector))
       (do* ((i start)
             (nchars 0 (1+ nchars)))
            ((>= i end)
             (if (= i end) nchars))
         (let* ((code (%swap-u16 (aref vector i))))
           (declare (type (unsigned-byte 8) code))
           (incf i
                 (if (or (< code #xd800)
                         (>= code #xe000))
                   1
                   2))))))
    :length-of-memory-encoding-function
    (nfunction
     reversed-utf-8-length-of-memory-encoding
     (lambda (pointer nunits &optional (start 0))
       (do* ((i start)
             (p (+ start start) (+ p 2))
             (nchars 0 (1+ nchars)))
            ((>= i nunits)
             (if (= i nunits) nchars))
         (let* ((code (%swap-u16 (%get-unsigned-word pointer p))))
           (declare (type (unsigned-byte 8) code))
           (incf i
                 (incf i
                       (if (or (< code #xd800)
                               (>= code #xe000))
                         1
                         2)))))))
    :literal-char-code-limit #x10000
    )

;;; UTF-16.
