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

(defstruct (file-ioblock (:include ioblock))
  (octet-pos 0 :type fixnum)		; current io position in octets
  (fileeof 0 :type fixnum)		; file length in elements
  (input-filter #'false)
  (output-filter #'false))


;;; The file-ioblock-octet-pos field is the (octet) position
;;; at which the next I/O operation will begin (e.g., where the
;;; input came from and/or where the output's going.)  There are
;;; cases (e.g., after a STREAM-CLEAR-INPUT) when this can't be
;;; determined (based on its previous value and the logical size
;;; of the buffer) so we'll have to ask the OS.

(defun file-octet-filepos (file-ioblock)
  (fd-tell (file-ioblock-device file-ioblock)))

(defun synch-file-octet-filepos (file-ioblock)
  (setf (file-ioblock-octet-pos file-ioblock)
	(file-octet-filepos file-ioblock)))

(defun translate-cr-to-lf (file-ioblock)
  (let* ((inbuf (file-ioblock-inbuf file-ioblock))
	 (string (io-buffer-buffer inbuf))
	 (n (io-buffer-count inbuf)))
    (declare (simple-base-string string)
	     (fixnum n))
    (dotimes (i n n)
      (if (eq (schar string i) #\Return)
	(setf (schar string i) #\Linefeed)))))

(defun translate-lf-to-cr (file-ioblock n)
  (declare (fixnum n))
  (let* ((outbuf (file-ioblock-outbuf file-ioblock))
	 (string (io-buffer-buffer outbuf)))
    (declare (simple-base-string string))
    (dotimes (i n n)
      (if (eq (schar string i) #\Linefeed)
	(setf (schar string i) #\Return)))))

(defun infer-external-format (file-stream)
  (with-stream-ioblock-input (ioblock file-stream :speedy t)
    (setf (file-stream-external-format file-stream)
	  (if (eq (funcall (ioblock-peek-char-function ioblock) ioblock)
                  :eof)
	    :unix
	    (let* ((inbuf (ioblock-inbuf ioblock))
		   (string (io-buffer-buffer inbuf))
		   (n (io-buffer-count inbuf)))
	      (declare (simple-base-string string)
		       (fixnum n))
	      (dotimes (i n :unix)
		(let* ((ch (schar string i)))
		  (if (eq ch #\Linefeed)
		    (return :unix))
		  (when (eq ch #\Return)
		    (translate-cr-to-lf ioblock)
		    (return :macos)))))))))

(defvar *default-external-format* :unix
  "The value of this variable is used when :EXTERNAL-FORMAT is
unspecified or specified as :DEFAULT. It can meaningfully be given any
of the values :UNIX, :MACOS, or :INFERRED, each of which is
interpreted as described in the documentation.

Because there's some risk that unsolicited newline translation could have
undesirable consequences, the initial value of this variable in OpenMCL
is :UNIX.")

(defparameter *external-format-translations*
  '((:unix nil nil)
    (:macos translate-cr-to-lf translate-lf-to-cr))
  "an alist: external-format-name, input-translation-function (or NIL),
   output-translation-function (or NIL)")

(defun file-stream-force-output (stream ioblock count finish-p)
  (let* ((filter (caddr (assoc (file-stream-external-format stream)
			       *external-format-translations*
			       :test #'eq))))
    (when filter
      (funcall filter ioblock count))
    (fd-stream-force-output stream ioblock count finish-p)))

;;; Establish a new position for the specified file-stream.
(defun file-ioblock-seek (file-ioblock newoctetpos)
  (let* ((result (fd-lseek
		  (file-ioblock-device file-ioblock) newoctetpos #$SEEK_SET)))
    (if (< result 0)
      (error 'simple-stream-error
	     :stream (file-ioblock-stream file-ioblock)
	     :format-control (format nil "Can't set file position to ~d: ~a"
				     newoctetpos (%strerror result)))
      newoctetpos)))

;;; For input streams, getting/setting the position is fairly simple.
;;; Getting the position is a simple matter of adding the buffer
;;; origin to the current position within the buffer.
;;; Setting the position involves either adjusting the buffer index
;;; (if the new position is within the current buffer) or seeking
;;; to a new position.

(defun %ioblock-input-file-position (file-ioblock newpos)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (inbuf (file-ioblock-inbuf file-ioblock))
	 (curpos (+ element-base (io-buffer-idx inbuf))))
    (if (null newpos)
      (if (file-ioblock-untyi-char file-ioblock)
	(1- curpos)
	curpos)
      (progn
	(setf (file-ioblock-untyi-char file-ioblock) nil)
	(if (and (>= newpos element-base)
		 (< newpos (+ element-base (io-buffer-count inbuf))))
	  (setf (io-buffer-idx inbuf) (- newpos element-base))
	  (file-ioblock-seek-and-reset file-ioblock
				       (ioblock-elements-to-octets
					file-ioblock
					newpos)))
	newpos))))

;;; For (pure) output streams, it's a little more complicated.  If we
;;; have to seek to a new origin, we may need to flush the buffer
;;; first.

(defun %ioblock-output-file-position (file-ioblock newpos)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (outbuf (file-ioblock-outbuf file-ioblock))
	 (curpos (+ element-base (io-buffer-idx outbuf)))
	 (maxpos (+ element-base (io-buffer-count outbuf))))
    (if (null newpos)
      curpos
      (progn
	(if (and (>= newpos element-base)
		 (<= newpos maxpos))
	  ;; Backing up is easy.  Skipping forward (without flushing
	  ;; and seeking) would be hard, 'cause we can't tell what
	  ;; we're skipping over.
	  (let* ((newidx (- newpos element-base)))
	    (setf (io-buffer-idx outbuf) newidx))
	  (progn
	    (when (file-ioblock-dirty file-ioblock)
	      (file-stream-force-output (file-ioblock-stream file-ioblock)
					file-ioblock
					(io-buffer-count outbuf)
					nil)
	      ;; May have just extended the file; may need to update
	      ;; fileeof.
	      (when (> maxpos (file-ioblock-fileeof file-ioblock))
		(setf (file-ioblock-fileeof file-ioblock) maxpos)))
	    (file-ioblock-seek-and-reset file-ioblock
					 (ioblock-elements-to-octets
					  file-ioblock
					  newpos))))
	newpos))))

;;; For I/O file streams, there's an additional complication: if we
;;; back up within the (shared) buffer and the old position was beyond
;;; the buffer's input count, we have to set the input count to the
;;; old position.  (Consider the case of writing a single element at
;;; the end-of-file, backing up one element, then reading the element
;;; we wrote.)  We -can- skip forward over stuff that's been read;
;;; if the buffer's dirty, we'll eventually write it back out.

(defun %ioblock-io-file-position (file-ioblock newpos)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (outbuf (file-ioblock-outbuf file-ioblock)) ; outbuf = inbuf
	 (curidx (io-buffer-idx outbuf))
	 (curpos (+ element-base curidx)))
    (if (null newpos)
      (if (file-ioblock-untyi-char file-ioblock)
	(1- curpos)
	curpos)
      (let* ((incount (io-buffer-count outbuf)))
	(when (file-ioblock-untyi-char file-ioblock)
	  (setf (file-ioblock-untyi-char file-ioblock) nil)
	  (if (> curidx 0)
	    (decf curpos)))
	(cond 
	  ((and (>= newpos element-base)
		(<= newpos curpos))
	   ;; If we've read less than we've written, make what's
	   ;; been written available for subsequent input.
	   (when (> curidx incount)
	     (setf (io-buffer-count outbuf) curidx))
	   (setf (io-buffer-idx outbuf) (- newpos element-base)))
	  ((and (>= newpos element-base)
		(< newpos (+ element-base incount)))
	   (setf (io-buffer-idx outbuf) (- newpos element-base)))
	  (t
	   (let* ((maxpos (+ element-base (io-buffer-count outbuf))))
	     (when (> maxpos (file-ioblock-fileeof file-ioblock))
	       (setf (file-ioblock-fileeof file-ioblock) maxpos)))
	   (when (file-ioblock-dirty file-ioblock)
	     (file-ioblock-seek file-ioblock octet-base)
	     (file-stream-force-output (file-ioblock-stream file-ioblock)
				       file-ioblock
				       (io-buffer-count outbuf)
				       nil))
	   (file-ioblock-seek-and-reset file-ioblock
					(ioblock-elements-to-octets
					 file-ioblock newpos))))
	newpos))))

;;; Again, it's simplest to define this in terms of the stream's direction.
;;; Note that we can't change the size of file descriptors open for input
;;; only.

(defun %ioblock-input-file-length (file-ioblock newlen)
  (unless newlen
    (file-ioblock-fileeof file-ioblock)))
 
(defun %ioblock-output-file-length (file-ioblock newlen)
  (let* ((octet-base (file-ioblock-octet-pos file-ioblock))
	 (element-base (ioblock-octets-to-elements file-ioblock octet-base))
	 (outbuf (file-ioblock-outbuf file-ioblock)) 
	 (curidx (io-buffer-idx outbuf))
	 (maxpos (+ element-base (io-buffer-count outbuf)))
	 (curlen (file-ioblock-fileeof file-ioblock)))
    (if (> maxpos curlen)
      (setf (file-ioblock-fileeof file-ioblock) (setq curlen maxpos)))
    (if (null newlen)
      curlen
      (let* ((fd (file-ioblock-device file-ioblock))
	     (new-octet-eof (ioblock-elements-to-octets file-ioblock newlen))
	     (cur-octet-pos (fd-tell fd)))
	(cond ((> newlen curlen)
	       ;; Extend the file; maintain the current position.
	       ;; ftruncate isn't guaranteed to extend a file past
	       ;; its current EOF.  Seeking to the new EOF, then
	       ;; writing, is guaranteed to do so.  Seek to the
	       ;; new EOF, write a random byte, truncate to the
	       ;; specified length, then seek back to where we
	       ;; were and pretend that nothing happened.
	       (file-ioblock-seek file-ioblock new-octet-eof)
	       (%stack-block ((buf 1))
			     (fd-write fd buf 1))
	       (fd-ftruncate fd new-octet-eof)
	       (file-ioblock-seek file-ioblock cur-octet-pos))
	      ((> newlen maxpos)
	       ;; Make the file shorter.  Doesn't affect
	       ;; our position or anything that we have buffered.
	       (fd-ftruncate fd new-octet-eof))
	      ((< newlen element-base)
	       ;; Discard any buffered output.  Truncate the
	       ;; file, then seek to the new EOF.
	       (fd-ftruncate fd new-octet-eof)
	       (setf (file-ioblock-untyi-char file-ioblock) nil)
	       (file-ioblock-seek-and-reset file-ioblock new-octet-eof))
	      (t
	       (fd-ftruncate fd new-octet-eof)
	       (let* ((newidx (- newlen element-base)))
		 (when (> maxpos newlen)
		   (setf (io-buffer-count outbuf) newidx))
		 (when (> curidx newidx)
		   (setf (io-buffer-idx outbuf) newidx)))))
	(setf (file-ioblock-fileeof file-ioblock) newlen)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass fundamental-file-stream (fd-stream file-stream)
    ((filename :initform nil :initarg :filename :accessor file-stream-filename)
     (actual-filename :initform nil :initarg :actual-filename)
     (external-format :initform :default :initarg :external-format
		      :accessor file-stream-external-format)))

  

(defmethod stream-filename ((s fundamental-file-stream))
  (file-stream-filename s))

(defmethod stream-actual-filename ((s file-stream))
  (slot-value s 'actual-filename))

(defmethod (setf stream-filename) (new (s fundamental-file-stream))
  (setf (file-stream-filename s) new))

(defmethod (setf stream-actual-filename) (new (s fundamental-file-stream))
  (setf (slot-value s 'actual-filename) new))

(defun print-file-stream (s out)
  (print-unreadable-object (s out :type t :identity t)
    (let* ((file-ioblock (stream-ioblock s nil)))
      (format out "(~s/" (stream-filename s))
      (if file-ioblock
	(format out "~d ~a)" (file-ioblock-device file-ioblock) (encoding-name (ioblock-encoding file-ioblock)))
	(format out ":closed")))))
    
(defmethod print-object ((s fundamental-file-stream) out)
  (print-file-stream s out))

(make-built-in-class 'basic-file-stream 'basic-stream 'file-stream)

(defmethod stream-filename ((s basic-file-stream))
  (basic-file-stream.filename s))

(defmethod stream-actual-filename ((s basic-file-stream))
  (basic-file-stream.actual-filename s))

(defmethod (setf stream-filename) (new (s basic-file-stream))
  (setf (basic-file-stream.filename s) new))

(defmethod (setf stream-actual-filename) (new (s basic-file-stream))
  (setf (basic-file-stream.actual-filename s) new))

(defmethod print-object ((s basic-file-stream) out)
  (print-file-stream s out))

(defmethod stream-create-ioblock ((stream fundamental-file-stream) &rest args &key)
  (declare (dynamic-extent args))
  (apply #'make-file-ioblock :stream stream args))

(defmethod stream-create-ioblock ((stream basic-file-stream) &rest args &key)
  (declare (dynamic-extent args))
  (apply #'make-file-ioblock :stream stream args))

(defclass fundamental-file-input-stream (fundamental-file-stream fd-input-stream)
    ())

(make-built-in-class 'basic-file-input-stream 'basic-file-stream 'basic-input-stream)


(defclass fundamental-file-output-stream (fundamental-file-stream fd-output-stream)
    ())

(make-built-in-class 'basic-file-output-stream 'basic-file-stream 'basic-output-stream)

(defclass fundamental-file-io-stream (fundamental-file-stream fd-io-stream)
    ())

(make-built-in-class 'basic-file-io-stream 'basic-file-stream 'basic-io-stream)


(defclass fundamental-file-character-input-stream (fundamental-file-input-stream
					  fd-character-input-stream)
    ())

(make-built-in-class 'basic-file-character-input-stream 'basic-file-input-stream 'basic-character-input-stream)


(defclass fundamental-file-character-output-stream (fundamental-file-output-stream
                                                    fd-character-output-stream)
    ())

(make-built-in-class 'basic-file-character-output-stream 'basic-file-output-stream 'basic-character-output-stream)

(defclass fundamental-file-character-io-stream (fundamental-file-io-stream
				       fd-character-io-stream)
    ())

(make-built-in-class 'basic-file-character-io-stream 'basic-file-io-stream 'basic-character-io-stream)

(defclass fundamental-file-binary-input-stream (fundamental-file-input-stream
                                                fd-binary-input-stream)
    ())

(make-built-in-class 'basic-file-binary-input-stream 'basic-file-input-stream 'basic-binary-input-stream)

(defclass fundamental-file-binary-output-stream (fundamental-file-output-stream
                                                 fd-binary-output-stream)
    ())

(make-built-in-class 'basic-file-binary-output-stream 'basic-file-output-stream 'basic-binary-output-stream)

(defclass fundamental-file-binary-io-stream (fundamental-file-io-stream fd-binary-io-stream)
    ())

(make-built-in-class 'basic-file-binary-io-stream 'basic-file-io-stream 'basic-binary-io-stream)


;;; This stuff is a lot simpler if we restrict the hair to the
;;; case of file streams opened in :io mode (which have to worry
;;; about flushing the shared buffer before filling it, and things
;;; like that.)

(defmethod stream-clear-input ((f fundamental-file-input-stream))
  (with-stream-ioblock-input (file-ioblock f :speedy t)
    (call-next-method)
    (synch-file-octet-filepos file-ioblock)
    nil))


(defmethod stream-clear-input ((f basic-file-input-stream))
  (let* ((file-ioblock (basic-stream-ioblock f)))
    (with-ioblock-input-locked (file-ioblock)
      (call-next-method)
      (synch-file-octet-filepos file-ioblock)
      nil)))

    
(defmethod stream-clear-input ((f fundamental-file-io-stream))
  (with-stream-ioblock-input (file-ioblock f :speedy t)
    (stream-force-output f)		
    (call-next-method)
    (synch-file-octet-filepos file-ioblock)
    nil))

(defmethod stream-clear-input ((f basic-file-io-stream))
  (let* ((file-ioblock (basic-stream-ioblock f)))
    (with-ioblock-input-locked (file-ioblock)
      (call-next-method)
      (synch-file-octet-filepos file-ioblock)
      nil)))

(defmethod stream-clear-output ((f fundamental-file-output-stream))
  (with-stream-ioblock-output (file-ioblock f :speedy t)
    (call-next-method)
    (synch-file-octet-filepos file-ioblock)
    nil))

(defmethod stream-clear-output ((f basic-file-output-stream))
  (let* ((file-ioblock (basic-stream-ioblock f)))
    (with-ioblock-input-locked (file-ioblock)
      (call-next-method)
      (synch-file-octet-filepos file-ioblock)
      nil)))

;;; Fill the input buffer, possibly doing newline translation.
(defun file-stream-advance (stream file-ioblock read-p)
  (let* ((n (fd-stream-advance stream file-ioblock read-p))
	   (filter (cadr (assoc (stream-external-format stream)
				*external-format-translations*
				:test #'eq))))
      (if filter
	(funcall filter file-ioblock))
      n))
  
;;; If we've been reading, the file position where we're going
;;; to read this time is (+ where-it-was-last-time what-we-read-last-time.)
(defun input-file-ioblock-advance (stream file-ioblock read-p)
  (let* ((newpos (+ (file-ioblock-octet-pos file-ioblock)
		    (io-buffer-count (file-ioblock-inbuf file-ioblock))))
	 (curpos (ioblock-octets-to-elements
		  file-ioblock
		  (file-octet-filepos file-ioblock))))
    (unless (eql newpos curpos)
      (break "Expected newpos to be ~d, fd is at ~d" newpos curpos))
    (setf (file-ioblock-octet-pos file-ioblock) newpos)
    (file-stream-advance stream file-ioblock read-p)))

;;; If the buffer's dirty, we have to back up and rewrite it before
;;; reading in a new buffer.
(defun io-file-ioblock-advance (stream file-ioblock read-p)
  (let* ((curpos (file-ioblock-octet-pos file-ioblock))
	 (count (io-buffer-count (file-ioblock-inbuf file-ioblock)))
	 (newpos (+ curpos 
		    (ioblock-elements-to-octets file-ioblock count))))
    (when (ioblock-dirty file-ioblock)
      (file-ioblock-seek file-ioblock curpos)
      (file-stream-force-output stream file-ioblock count nil))
    (unless (eql newpos (file-octet-filepos file-ioblock))
      (break "Expected newpos to be ~d, fd is at ~d"
	     newpos (file-octet-filepos file-ioblock)))
    (setf (file-ioblock-octet-pos file-ioblock) newpos)
    (file-stream-advance stream file-ioblock read-p)))

		    
(defun output-file-force-output (stream file-ioblock count finish-p)
  ;; Check to see if we're where we think we should be.
  (let* ((curpos (file-ioblock-octet-pos file-ioblock)))
    (unless (eql curpos (file-octet-filepos file-ioblock))
      (break "Expected newpos to be ~d, fd is at ~d"
	     curpos (file-octet-filepos file-ioblock)))
    (let* ((n (file-stream-force-output stream file-ioblock count finish-p)))
      (incf (file-ioblock-octet-pos file-ioblock) (or n 0))
      n)))

;;; Can't be sure where the underlying fd is positioned, so seek first.
(defun io-file-force-output (stream file-ioblock count finish-p)
  (file-ioblock-seek file-ioblock (file-ioblock-octet-pos file-ioblock))
  (output-file-force-output stream file-ioblock count finish-p))


;;; Invalidate both buffers and seek to the new position.  The output
;;; buffer's been flushed already if it needed to be.

(defun file-ioblock-seek-and-reset (file-ioblock newoctetpos)
  (let* ((inbuf (file-ioblock-inbuf file-ioblock))
	 (outbuf (file-ioblock-outbuf file-ioblock)))
    (setf (file-ioblock-untyi-char file-ioblock) nil)
    (setf (file-ioblock-dirty file-ioblock) nil)
    (when inbuf
      (setf (io-buffer-count inbuf) 0
	    (io-buffer-idx inbuf) 0))
    (when outbuf
      (setf (io-buffer-count outbuf) 0
	    (io-buffer-idx outbuf) 0))
    (setf (file-ioblock-octet-pos file-ioblock) newoctetpos)
    (file-ioblock-seek file-ioblock newoctetpos)))

(defmethod stream-position ((stream fundamental-file-input-stream) &optional newpos)
  (with-stream-ioblock-input (file-ioblock stream :speedy t)
    (%ioblock-input-file-position file-ioblock newpos)))


(defmethod stream-position ((stream basic-file-input-stream) &optional newpos)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (%ioblock-input-file-position file-ioblock newpos))))

(defmethod stream-position ((stream fundamental-file-output-stream) &optional newpos)
  (with-stream-ioblock-output (file-ioblock stream :speedy t)
    (%ioblock-output-file-position file-ioblock newpos)))

(defmethod stream-position ((stream basic-file-output-stream) &optional newpos)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (file-ioblock)
      (%ioblock-output-file-position file-ioblock newpos))))


(defmethod stream-position ((stream fundamental-file-io-stream) &optional newpos)
  (with-stream-ioblock-input (file-ioblock stream :speedy t)
    (%ioblock-io-file-position file-ioblock newpos)))

(defmethod stream-position ((stream basic-file-io-stream) &optional newpos)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (%ioblock-io-file-position file-ioblock newpos))))


(defmethod stream-length ((stream fundamental-file-input-stream) &optional newlen)
  (with-stream-ioblock-input (file-ioblock stream :speedy t)
    (%ioblock-input-file-length file-ioblock newlen)))

(defmethod stream-length ((stream basic-file-input-stream) &optional newlen)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (%ioblock-input-file-length file-ioblock newlen))))


(defmethod stream-length ((s fundamental-file-output-stream) &optional newlen)
  (with-stream-ioblock-output (file-ioblock s :speedy t)
    (%ioblock-output-file-length file-ioblock newlen)))


(defmethod stream-length ((stream basic-file-output-stream) &optional newlen)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-output-locked (file-ioblock)
      (%ioblock-output-file-length file-ioblock newlen))))

(defmethod stream-length ((s fundamental-file-io-stream) &optional newlen)
  (with-stream-ioblock-input (file-ioblock s :speedy t)
    (%ioblock-output-file-length file-ioblock newlen)))

(defmethod stream-length ((stream basic-file-io-stream) &optional newlen)
  (let* ((file-ioblock (basic-stream-ioblock stream)))
    (with-ioblock-input-locked (file-ioblock)
      (%ioblock-output-file-length file-ioblock newlen))))

(defun close-file-stream (s abort)
  (when (open-stream-p s)
    (let* ((ioblock (stream-ioblock s t))
	   (filename (stream-filename s))
	   (actual-filename (stream-actual-filename s)))
      (when actual-filename
	(if abort
	  (progn
	    (setf (ioblock-dirty ioblock) nil)
	    (fd-stream-close s ioblock)
	    (unix-rename (namestring actual-filename) (probe-file-x filename)))
	  (delete-file actual-filename)))
      (setq *open-file-streams* (nremove s *open-file-streams*)))))


(defmethod close ((s fundamental-file-stream) &key abort)
  (close-file-stream s abort)
  (call-next-method))

(defmethod close ((s basic-file-stream) &key abort)
  (close-file-stream s abort)
  (call-next-method))

(defmethod select-stream-class ((class fundamental-file-stream) in-p out-p char-p)
  (if char-p
    (if (and in-p out-p)
      'fundamental-file-character-io-stream
      (if in-p
	'fundamental-file-character-input-stream
	(if out-p
	  'fundamental-file-character-output-stream
	  'fundamental-file-stream)))
    (if (and in-p out-p)
      'fundamental-file-binary-io-stream
      (if in-p
	'fundamental-file-binary-input-stream
	(if out-p
	  'fundamental-file-binary-output-stream
	  'fundamental-file-stream)))))

(defmethod select-stream-class ((class file-stream) in-p out-p char-p)
  (if char-p
    (if (and in-p out-p)
      'fundamental-file-character-io-stream
      (if in-p
	'fundamental-file-character-input-stream
	(if out-p
	  'fundamental-file-character-output-stream
	  'fundamental-file-stream)))
    (if (and in-p out-p)
      'fundamental-file-binary-io-stream
      (if in-p
	'fundamental-file-binary-input-stream
	(if out-p
	  'fundamental-file-binary-output-stream
	  'fundamental-file-stream)))))

(defmethod map-to-basic-stream-class-name ((name (eql 'fundamental-file-stream)))
  'basic-file-stream)

(defmethod map-to-basic-stream-class-name ((name (eql 'file-stream)))
  'basic-file-stream)

(defmethod select-stream-class ((class (eql 'basic-file-stream)) in-p out-p char-p)
  (if char-p
    (if (and in-p out-p)
      'basic-file-character-io-stream
      (if in-p
	'basic-file-character-input-stream
	(if out-p
	  'basic-file-character-output-stream
	  'basic-file-stream)))
    (if (and in-p out-p)
      'basic-file-binary-io-stream
      (if in-p
	'basic-file-binary-input-stream
	(if out-p
	  'basic-file-binary-output-stream
	  'basic-file-stream)))))

(defun make-file-stream (filename
			 direction
			 element-type
			 if-exists
			 if-does-not-exist
			 elements-per-buffer
			 class
			 external-format
                         sharing
                         basic)

  (let* ((temp-name nil)
         (dir (pathname-directory filename))
         (filename (if (eq (car dir) :relative)
                     (full-pathname filename)
                     filename))
         (pathname (pathname filename))) 
    (block open
      (if (or (memq element-type '(:default character base-char))
	      (subtypep element-type 'character))
	(if (eq element-type :default)(setq element-type 'character))
	(progn
	  (setq element-type (type-expand element-type))
	  (cond ((equal element-type '#.(type-expand 'signed-byte))
		 (setq element-type '(signed-byte 8)))
		((equal element-type '#.(type-expand 'unsigned-byte))
		 (setq element-type '(unsigned-byte 8))))))
      (case direction
	(:probe (setq if-exists :ignored))
	(:input (setq if-exists :ignored))
	((:io :output) nil)
	(t (report-bad-arg direction '(member :input :output :io :probe))))
      (multiple-value-bind (native-truename kind)(probe-file-x filename)
	(if native-truename
	  (if (eq kind :directory)
	    (if (eq direction :probe)
	      (return-from open nil)
	      (signal-file-error (- #$EISDIR)  filename))
	    (if (setq filename (if-exists if-exists filename "Open ..."))
	      (progn
		(multiple-value-setq (native-truename kind) (probe-file-x filename))
		(cond 
		  ((not native-truename)
		   (setq native-truename (%create-file filename)))
		  ((memq direction '(:output :io))
		   #|;;
                   ;; this prevents us from writing a file that is open for anything            
                   ;;l but does not protect against reading a file that is open for :output
		   (when (and bits (eq direction :output)(neq 0 (logand bits #x81)))
		   (signal-file-error EBUSY filename))
		   |#
		   (when (eq if-exists :supersede)
		     (let ((truename (native-to-pathname native-truename)))
		       (setq temp-name (gen-file-name truename))
		       (unix-rename native-truename (namestring temp-name))
		       (%create-file native-truename))))))
	      (return-from open nil)))
	  (if (setq filename (if-does-not-exist if-does-not-exist filename))
	    (setq native-truename (%create-file filename))
	    (return-from open nil)))
	(let* ((fd (fd-open native-truename (case direction
					      ((:probe :input) #$O_RDONLY)
					      (:output #$O_WRONLY)
					      (:io #$O_RDWR)))))
	  (when (< fd 0)  (signal-file-error fd filename))
          (let* ((fd-kind (%unix-fd-kind fd)))
            (if (not (eq fd-kind :file))
              (make-fd-stream fd :direction direction
                              :element-type element-type
                              :elements-per-buffer elements-per-buffer
                              :sharing sharing
                              :basic basic)
              (progn
                (when basic
                  (setq class (map-to-basic-stream-class-name class))
                  (setq basic (subtypep (find-class class) 'basic-stream)))
                (let* ((in-p (member direction '(:io :input)))
                     (out-p (member direction '(:io :output)))
                     (io-p (eq direction :io))
                     (char-p (or (eq element-type 'character)
                                 (subtypep element-type 'character)))
                     (infer nil)
                     (real-external-format
                      (if (and char-p in-p)
                        (progn
                          (if (eq external-format :default)
                            (setq external-format *default-external-format*))
                          (if (eq external-format :inferred)
                            (setq infer t external-format :unix)
                            (unless (assoc external-format
                                           *external-format-translations*
                                           :test #'eq)
                              (setq external-format :unix)))
                          external-format)
                        :binary))
                     (fstream (make-ioblock-stream
                               (select-stream-class class in-p out-p char-p)
                               :insize (if in-p elements-per-buffer)
                               :outsize (if (and out-p (not io-p))
                                          elements-per-buffer)
                               :share-buffers-p io-p
                               :interactive nil
                               :direction direction
                               :element-type element-type
                               :direction direction
                               :listen-function 'fd-stream-listen
                               :close-function 'fd-stream-close
                               :advance-function
                               (if io-p
                                 'io-file-ioblock-advance
                                 (if in-p
                                   'input-file-ioblock-advance))
                               :force-output-function
                               (if io-p
                                 'io-file-force-output
                                 (if out-p
                                   'output-file-force-output))
                               :device fd
                               :external-format real-external-format
                               :sharing sharing
                               :character-p (or (eq element-type 'character)
                                                (subtypep element-type 'character))))
                     (ioblock (stream-ioblock fstream t)))
                (setf (stream-filename fstream) (namestring pathname)
                      (stream-actual-filename fstream) temp-name)
                (setf (file-ioblock-fileeof ioblock)
                      (ioblock-octets-to-elements ioblock (fd-size fd)))
                (if infer
                  (infer-external-format fstream))
                (cond ((eq if-exists :append)
                       (file-position fstream :end))
                      ((and (memq direction '(:io :output))
                            (neq if-exists :overwrite))
                       (stream-length fstream 0)))
                (if (eq direction :probe)
                  (close fstream)
                  (push fstream *open-file-streams*))
                fstream)))))))))

(defmethod stream-external-format ((s fundamental-file-stream))
  (file-stream-external-format s))

(defmethod stream-external-format ((s basic-file-stream))
  (basic-file-stream.external-format s))

(defmethod file-stream-external-format ((s basic-file-stream))
  (basic-file-stream.external-format s))

(defmethod (setf file-stream-external-format) (new (s basic-file-stream))
  (setf (basic-file-stream.external-format s) new))


(defmethod stream-external-format ((s broadcast-stream))
  (let* ((last (last-broadcast-stream s)))
    (if last
        (stream-external-format s)
        :default)))

;;; Under the circumstances, this is a very slow way of saying
;;; "we don't support EXTENDED-CHARs".
(defun file-string-length (stream object)
  "Return the delta in STREAM's FILE-POSITION that would be caused by writing
   OBJECT to STREAM. Non-trivial only in implementations that support
   international character sets."
  (if (typep stream 'broadcast-stream)
    (let* ((last (last-broadcast-stream stream)))
      (if last
	(file-string-length last object)
	1))
    (progn
      (unless (and (typep stream 'file-stream)
		   (let* ((eltype (stream-element-type stream)))
		     (or (eq 'character eltype)
			 (eq 'base-char eltype)
			 (subtypep eltype 'character))))
	(error "~S is not a file stream capable of character output" stream))
      (etypecase object
	(character 1)
	(string (length object))))))
  
