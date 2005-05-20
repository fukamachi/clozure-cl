;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
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



;;; basic socket API
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(MAKE-SOCKET
	    ACCEPT-CONNECTION
	    DOTTED-TO-IPADDR
	    IPADDR-TO-DOTTED
	    IPADDR-TO-HOSTNAME
	    LOOKUP-HOSTNAME
	    LOOKUP-PORT
	    ;;with-pending-connect
	    RECEIVE-FROM
	    SEND-TO
	    SHUTDOWN
	    ;;socket-control
	    SOCKET-OS-FD
	    REMOTE-HOST
	    REMOTE-PORT
	    REMOTE-FILENAME
	    LOCAL-HOST
	    LOCAL-PORT
	    LOCAL-FILENAME
	    SOCKET-ADDRESS-FAMILY
	    SOCKET-CONNECT
	    SOCKET-FORMAT
	    SOCKET-TYPE
	    SOCKET-ERROR
	    SOCKET-ERROR-CODE
	    SOCKET-ERROR-IDENTIFIER
	    SOCKET-ERROR-SITUATION
	    WITH-OPEN-SOCKET)))

;;; The PPC is big-endian (uses network byte order), which makes
;;; things like #_htonl and #_htonl no-ops.  These functions aren't
;;; necessarily defined as functions in some header files (I'm sure
;;; that that either complies with or violates some C standard), and
;;; it doesn't seem to make much sense to fight that to do ff-calls
;;; to a couple of identity functions.

#+ppc-target
(progn
  (defmacro HTONL (x) x)
  (defmacro HTONS (x) x)
  (defmacro NTOHL (x) x)
  (defmacro NTOHS (x) x))
  

;;; On some (hypothetical) little-endian platform, we might want to
;;; define HTONL and HTONS to actually swap bytes around.

(defpackage "OPENMCL-SOCKET"
  (:use "CL")
  (:import-from "CCL"
		"MAKE-SOCKET"
		"ACCEPT-CONNECTION"
		"DOTTED-TO-IPADDR"
		"IPADDR-TO-DOTTED"
		"IPADDR-TO-HOSTNAME"
		"LOOKUP-HOSTNAME"
		"LOOKUP-PORT"
		;;with-pending-connect
		"RECEIVE-FROM"
		"SEND-TO"
		"SHUTDOWN"
		;;socket-control
		"SOCKET-OS-FD"
		"REMOTE-HOST"
		"REMOTE-PORT"
		"REMOTE-FILENAME"
		"LOCAL-HOST"
		"LOCAL-PORT"
		"LOCAL-FILENAME"
		"SOCKET-ADDRESS-FAMILY"
		"SOCKET-CONNECT"
		"SOCKET-FORMAT"
		"SOCKET-TYPE"
		"SOCKET-ERROR"
		"SOCKET-ERROR-CODE"
		"SOCKET-ERROR-IDENTIFIER"
		"SOCKET-ERROR-SITUATION"
		"WITH-OPEN-SOCKET")
  (:export  "MAKE-SOCKET"
	    "ACCEPT-CONNECTION"
	    "DOTTED-TO-IPADDR"
	    "IPADDR-TO-DOTTED"
	    "IPADDR-TO-HOSTNAME"
	    "LOOKUP-HOSTNAME"
	    "LOOKUP-PORT"
	    ;;with-pending-connect
	    "RECEIVE-FROM"
	    "SEND-TO"
	    "SHUTDOWN"
	    ;;socket-control
	    "SOCKET-OS-FD"
	    "REMOTE-HOST"
	    "REMOTE-PORT"
	    "REMOTE-FILENAME"
	    "LOCAL-HOST"
	    "LOCAL-PORT"
	    "LOCAL-FILENAME"
	    "SOCKET-ADDRESS-FAMILY"
	    "SOCKET-CONNECT"
	    "SOCKET-FORMAT"
	    "SOCKET-TYPE"
	    "SOCKET-ERROR"
	    "SOCKET-ERROR-CODE"
	    "SOCKET-ERROR-IDENTIFIER"
	    "SOCKET-ERROR-SITUATION"
	    "WITH-OPEN-SOCKET"))

(eval-when (:compile-toplevel :execute)
  #+linuxppc-target
  (require "LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWIN-SYSCALLS"))

(define-condition socket-error (simple-stream-error)
  ((code :initarg :code :reader socket-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-error-identifier)
   (Situation :initarg :situation :reader socket-error-situation)))

(define-condition socket-creation-error (simple-error)
  ((code :initarg :code :reader socket-creation-error-code)
   (identifier :initform :unknown :initarg :identifier :reader socket-creationg-error-identifier)
   (situation :initarg :situation :reader socket-creation-error-situation)))

(defvar *socket-error-identifiers*
  (list #$EADDRINUSE :address-in-use
	#$ECONNABORTED :connection-aborted
	#$ENOBUFS :no-buffer-space
	#$ENOMEM :no-buffer-space
	#$ENFILE :no-buffer-space
	#$ETIMEDOUT :connection-timed-out
	#$ECONNREFUSED :connection-refused
	#$ENETUNREACH :host-unreachable
	#$EHOSTUNREACH :host-unreachable
	#$EHOSTDOWN :host-down
	#$ENETDOWN :network-down
	#$EADDRNOTAVAIL :address-not-available
	#$ENETRESET :network-reset
	#$ECONNRESET :connection-reset
	#$ESHUTDOWN :shutdown
	#$EACCES :access-denied
	#$EPERM :access-denied))


(declaim (inline socket-call))
(defun socket-call (stream where res)
  (if (< res 0)
    (socket-error stream where res)
    res))

(defun %hstrerror (h_errno)
  (with-macptrs ((p (#_hstrerror (abs h_errno))))
    (if p
      (%get-cstring p)
      (format nil "Nameserver error ~d" (abs h_errno)))))
    



(defun socket-error (stream where errno &optional nameserver-p)
  "Creates and signals (via error) one of two socket error 
conditions, based on the state of the arguments."
  (when (< errno 0)
    (setq errno (- errno)))
  (if stream
    (error (make-condition 'socket-error
			   :stream stream
			   :code errno
			   :identifier (getf *socket-error-identifiers* errno :unknown)
			   :situation where
			   ;; TODO: this is a constant arg, there is a way to put this
			   ;; in the class definition, just need to remember how...
			   :format-control "~a (error #~d) on ~s in ~a"
			   :format-arguments (list
					      (if nameserver-p
						(%hstrerror errno)
						(%strerror errno))
					      errno stream where)))
    (error (make-condition 'socket-creation-error
			   :code errno
			   :identifier (getf *socket-error-identifiers* errno :unknown)
			   :situation where
			   ;; TODO: this is a constant arg, there is a way to put this
			   ;; in the class definition, just need to remember how...
			   :format-control "~a (error #~d) on ~s in ~a"
			   :format-arguments (list
					      (if nameserver-p
						(%hstrerror errno)
						(%strerror errno))
					      errno stream where)))))
    


;; If true, this will try to allow other processes to run while
;; socket io is happening.
(defvar *multiprocessing-socket-io* t)

(defclass socket ()
  ())

(defmacro with-open-socket ((var . args) &body body
			    &aux (socket (make-symbol "socket"))
			         (done (make-symbol "done")))
  `(let (,socket ,done)
     (unwind-protect
	 (multiple-value-prog1
	   (let ((,var (setq ,socket (make-socket ,@args))))
	     ,@body)
	   (setq ,done t))
       (when ,socket (close ,socket :abort (not ,done))))))

(defclass ip-socket (socket)
  ())

(defmethod SOCKET-ADDRESS-FAMILY ((socket ip-socket)) :internet)

(defclass file-socket (socket)
  ())

(defmethod SOCKET-ADDRESS-FAMILY ((socket file-socket)) :file)

(defclass tcp-socket (ip-socket)
  ())

(defmethod SOCKET-TYPE ((socket tcp-socket)) :stream)

(defclass stream-file-socket (file-socket)
  ())

(defmethod SOCKET-TYPE ((socket stream-file-socket)) :stream)


;; An active TCP socket is an honest-to-goodness stream.
(defclass tcp-stream (tcp-socket fd-stream
				 buffered-binary-io-stream-mixin
				 buffered-character-io-stream-mixin)
  ())

(defmethod SOCKET-CONNECT ((stream tcp-stream)) :active)

(defmethod SOCKET-FORMAT ((stream tcp-stream))
  (if (eq (stream-element-type stream) 'character)
    :text
    ;; Should distinguish between :binary and :bivalent, but hardly
    ;; seems worth carrying around an extra slot just for that.
    :bivalent))

(defmethod socket-device ((stream tcp-stream))
  (let ((ioblock (stream-ioblock stream)))
    (and ioblock (ioblock-device ioblock))))

(defmethod select-stream-class ((class tcp-stream) in-p out-p char-p)
  (declare (ignore char-p)) ; TODO: is there any real reason to care about this?
  (assert (and in-p out-p) () "Non-bidirectional tcp stream?")
  'tcp-stream)

;; A FILE-SOCKET-STREAM is also honest. To goodness.
(defclass file-socket-stream (stream-file-socket
                              fd-stream
                              buffered-binary-io-stream-mixin
                              buffered-character-io-stream-mixin)
  ())

(defmethod select-stream-class ((class file-socket-stream) in-p out-p char-p)
  (declare (ignore char-p)) ; TODO: is there any real reason to care about this?
  (assert (and in-p out-p) () "Non-bidirectional tcp stream?")
  'file-socket-stream)

(defclass unconnected-socket (socket)
  ((device :initarg :device :accessor socket-device)
   (keys :initarg :keys :reader socket-keys)))

(defmethod SOCKET-FORMAT ((socket unconnected-socket))
  (or (getf (socket-keys socket) :format) :text))

(defmethod CLOSE ((socket unconnected-socket) &key abort)
  (declare (ignore abort))
  (when (socket-device socket)
    (fd-close (socket-device socket))
    (setf (socket-device socket) nil)
    t))

;; A passive tcp socket just generates connection streams
(defclass listener-socket (tcp-socket unconnected-socket) ())

(defmethod SOCKET-CONNECT ((stream listener-socket)) :passive)

(defclass file-listener-socket (stream-file-socket unconnected-socket) ())

(defmethod SOCKET-CONNECT ((stream file-listener-socket)) :passive)

;;; A FILE-LISTENER-SOCKET should try to delete the filesystem
;;; entity when closing.

(defmethod close :before ((s file-listener-socket) &key abort)
  (declare (ignore abort))
  (let* ((path (local-socket-filename (socket-device s) s)))
    (when path (%delete-file path))))


;; A udp socket just sends and receives packets.
(defclass udp-socket (ip-socket unconnected-socket) ())

(defmethod SOCKET-TYPE ((stream udp-socket)) :datagram)
(defmethod SOCKET-CONNECT ((stream udp-socket)) nil)

;; Returns nil for closed stream...
(defmethod SOCKET-OS-FD ((socket socket))
  (socket-device socket))

;; Returns nil for closed stream
(defun local-socket-info (fd type socket)
  (and fd
       (rlet ((sockaddr :sockaddr_in)
	      (namelen :signed))
	     (setf (pref namelen :signed) (record-length :sockaddr_in))
	     (socket-call socket "getsockname" (c_getsockname fd sockaddr namelen))
	     (when (= #$AF_INET (pref sockaddr :sockaddr_in.sin_family))
	       (ecase type
		 (:host (ntohl (pref sockaddr :sockaddr_in.sin_addr.s_addr)))
		 (:port (ntohs (pref sockaddr :sockaddr_in.sin_port))))))))

(defun path-from-unix-address (addr)
  (when (= #$AF_LOCAL (pref addr :sockaddr_un.sun_family))
    #+darwinppc-target
    (%str-from-ptr (pref addr :sockaddr_un.sun_path)
		   (- (pref addr :sockaddr_un.sun_len) 2))
    #-darwinppc-target
    (%get-cstring (pref addr :sockaddr_un.sun_path))))

(defun local-socket-filename (fd socket)
  (and fd
       (rlet ((addr :sockaddr_un)
              (namelen :signed))
         (setf (pref namelen :signed) (record-length :sockaddr_un))
         (socket-call socket "getsockname" (c_getsockname fd addr namelen))
	 (path-from-unix-address addr))))

(defmacro with-if ((var expr) &body body)
  `(let ((,var ,expr))
     (if ,var
	 (progn
	   ,@body))))     

(defun remote-socket-info (socket type)
  (with-if (fd (socket-device socket))
    (rlet ((sockaddr :sockaddr_in)
	   (namelen :signed))
	  (setf (pref namelen :signed) (record-length :sockaddr_in))
	  (let ((err (c_getpeername fd sockaddr namelen)))
	    (cond ((eql err (- #$ENOTCONN)) nil)
		  ((< err 0) (socket-error socket "getpeername" err))
		  (t
		   (when (= #$AF_INET (pref sockaddr :sockaddr_in.sin_family))
		     (ecase type
		       (:host (ntohl (pref sockaddr :sockaddr_in.sin_addr.s_addr)))
		       (:port (ntohs  (pref sockaddr :sockaddr_in.sin_port)))))))))))

(defun remote-socket-filename (socket)
  (with-if (fd (socket-device socket))
    (rlet ((addr :sockaddr_un)
	   (namelen :signed))
	  (setf (pref namelen :signed) (record-length :sockaddr_un))
	  (let* ((err (c_getsockname fd addr namelen)))
	    (cond ((eql err (- #$ENOTCONN)) nil)
		  ((< err 0) (socket-error socket "getpeername" err))
		  (t (path-from-unix-address addr)))))))

(defmethod LOCAL-PORT ((socket socket))
  (local-socket-info (socket-device socket) :port socket))

(defmethod LOCAL-HOST ((socket socket))
  (local-socket-info (socket-device socket) :host socket))

(defmethod LOCAL-FILENAME ((socket socket))
  (local-socket-filename socket))

;; Returns NIL if socket is not connected
(defmethod REMOTE-HOST ((socket socket))
  (remote-socket-info socket :host))

(defmethod REMOTE-PORT ((socket socket))
  (remote-socket-info socket :port))

(defmethod REMOTE-FILENAME ((socket socket))
  (remote-socket-filename socket))
  
(defun set-socket-options (fd-or-socket &key 
			   keepalive
			   reuse-address
			   nodelay
			   broadcast
			   linger
			   address-family
			   local-port
			   local-host
			   local-filename
			   type
			   connect
			   out-of-band-inline
			   &allow-other-keys)
  ;; see man socket(7) tcp(7) ip(7)
  (multiple-value-bind (socket fd) (etypecase fd-or-socket
				     (socket (values fd-or-socket (socket-device fd-or-socket)))
				     (integer (values nil fd-or-socket)))
    
    (if (null address-family)
	(setq address-family :internet))
    (when keepalive
      (int-setsockopt fd #$SOL_SOCKET #$SO_KEEPALIVE 1))
    (when reuse-address
      (int-setsockopt fd #$SOL_SOCKET #$SO_REUSEADDR 1))
    (when broadcast
      (int-setsockopt fd #$SOL_SOCKET #$SO_BROADCAST 1))
    (when out-of-band-inline
      (int-setsockopt fd #$SOL_SOCKET #$SO_OOBINLINE 1))
    (rlet ((plinger :linger))
	  (setf (pref plinger :linger.l_onoff) (if linger 1 0)
		(pref plinger :linger.l_linger) (or linger 0))
	  (socket-call socket "setsockopt"
		       (c_setsockopt fd #$SOL_SOCKET #$SO_LINGER plinger 8)))
    (when (eq address-family :internet)
      (when nodelay
	(int-setsockopt fd
			#+linuxppc-target #$SOL_TCP
			#+darwinppc-target #$IPPROTO_TCP
			#$TCP_NODELAY 1))
      (when (or local-port local-host)
	(let* ((proto (if (eq type :stream) "tcp" "udp"))
	       (port-n (if local-port (port-as-inet-port local-port proto) 0))
	       (host-n (if local-host (host-as-inet-host local-host) #$INADDR_ANY)))
	  ;; Darwin includes the SIN_ZERO field of the sockaddr_in when
	  ;; comparing the requested address to the addresses of configured
	  ;; interfaces (as if the zeros were somehow part of either address.)
	  ;; "rletz" zeros out the stack-allocated structure, so those zeros
	  ;; will be 0.
	  (rletz ((sockaddr :sockaddr_in))
		 (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET
		       (pref sockaddr :sockaddr_in.sin_port) port-n
		       (pref sockaddr :sockaddr_in.sin_addr.s_addr) host-n)
		 (socket-call socket "bind" (c_bind fd sockaddr (record-length :sockaddr_in)))))))
    (when (and (eq address-family :file)
	       (eq connect :passive)
	       local-filename)
      (bind-unix-socket fd local-filename))    
    (when *multiprocessing-socket-io*
      (socket-call socket "fcntl" (fd-set-flag fd #$O_NONBLOCK)))))

;; I hope the inline declaration makes the &rest/apply's go away...
(declaim (inline make-ip-socket))
(defun make-ip-socket (&rest keys &key type &allow-other-keys)
  (declare (dynamic-extent keys))
  (ecase type
    ((nil :stream) (apply #'make-tcp-socket keys))
    ((:datagram) (apply #'make-udp-socket keys))))

(declaim (inline make-file-socket))
(defun make-file-socket (&rest keys &key type &allow-other-keys)
  (declare (dynamic-extent keys))
  (ecase type
    ((nil :stream) (apply #'make-stream-file-socket keys))
    (:datagram (apply #'make-datagram-file-socket keys))))

(defun MAKE-SOCKET (&rest keys
		    &key address-family
		    ;; List all keys here just for error checking...
		    ;; &allow-other-keys
		    type connect remote-host remote-port eol format
		    keepalive reuse-address nodelay broadcast linger
		    local-port local-host backlog class out-of-band-inline
		    local-filename remote-filename)
  (declare (dynamic-extent keys))
  (declare (ignore type connect remote-host remote-port eol format
		   keepalive reuse-address nodelay broadcast linger
		   local-port local-host backlog class out-of-band-inline
		   local-filename remote-filename))
  (ecase address-family
    ((:file) (apply #'make-file-socket keys))
    ((nil :internet) (apply #'make-ip-socket keys))))



(defun make-udp-socket (&rest keys &aux (fd -1))
  (unwind-protect
    (let (socket)
      (setq fd (socket-call nil "socket"
			    (c_socket #$AF_INET #$SOCK_DGRAM #$IPPROTO_UDP)))
      (apply #'set-socket-options fd keys)
      (setq socket (make-instance 'udp-socket
				  :device fd
				  :keys keys))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-tcp-socket (&rest keys &key connect &allow-other-keys &aux (fd -1))
  (unwind-protect
    (let (socket)
      (setq fd (socket-call nil "socket"
			    (c_socket #$AF_INET #$SOCK_STREAM #$IPPROTO_TCP)))
      (apply #'set-socket-options fd keys)
      (setq socket
	    (ecase connect
	      ((nil :active) (apply #'make-tcp-stream-socket fd keys))
	      ((:passive) (apply #'make-tcp-listener-socket fd keys))))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun make-stream-file-socket (&rest keys &key connect &allow-other-keys &aux (fd -1))
  (unwind-protect
    (let (socket)
      (setq fd (socket-call nil "socket" (c_socket #$PF_LOCAL #$SOCK_STREAM 0)))
      (apply #'set-socket-options fd keys)
      (setq socket
	    (ecase connect
	      ((nil :active) (apply #'make-file-stream-socket fd keys))
	      ((:passive) (apply #'make-file-listener-socket fd keys))))
      (setq fd -1)
      socket)
    (unless (< fd 0)
      (fd-close fd))))

(defun %socket-connect (fd addr addrlen)
  (let* ((err (c_connect fd addr addrlen)))
    (declare (fixnum err))
    (when (eql err (- #$EINPROGRESS))
      (process-output-wait fd)
      (setq err (- (int-getsockopt fd #$SOL_SOCKET #$SO_ERROR))))
    (unless (eql err 0) (socket-error nil "connect" err))))
    
(defun inet-connect (fd host-n port-n)
  (rlet ((sockaddr :sockaddr_in))
    (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET
          (pref sockaddr :sockaddr_in.sin_port) port-n
          (pref sockaddr :sockaddr_in.sin_addr.s_addr) host-n)
    (%socket-connect fd sockaddr (record-length :sockaddr_in))))
               
(defun file-socket-connect (fd remote-filename)
  (rletz ((sockaddr :sockaddr_un))
    (init-unix-sockaddr sockaddr remote-filename)
    (%socket-connect fd sockaddr (record-length :sockaddr_un))))
         
  
(defun make-tcp-stream-socket (fd &key remote-host
				  remote-port
				  eol
				  format
				  (class 'tcp-stream)
				  &allow-other-keys)
  (inet-connect fd
		(host-as-inet-host remote-host)
		(port-as-inet-port remote-port "tcp"))
  (make-tcp-stream fd :format format :eol eol :class class))

(defun make-file-stream-socket (fd &key remote-filename
                                   eol
                                   format
                                   (class 'file-socket-stream)
                                   &allow-other-keys)
  (file-socket-connect fd remote-filename)
  (make-file-socket-stream fd :format format :eol eol :class class))


(defun make-tcp-stream (fd &key format eol (class 'tcp-stream)  &allow-other-keys)
  (declare (ignore eol))		;???
  (let ((element-type (ecase format
			((nil :text) 'character)
			((:binary :bivalent) '(unsigned-byte 8)))))
    ;; TODO: check out fd-stream-advance, -listen, -eofp, -force-output, -close
    ;; See if should specialize any of 'em.
    (make-fd-stream fd
		    :class class
		    :direction :io
		    :element-type element-type)))

(defun make-file-socket-stream (fd &key format eol (class 'file-socket-stream)  &allow-other-keys)
  (declare (ignore eol))		;???
  (let ((element-type (ecase format
			((nil :text) 'character)
			((:binary :bivalent) '(unsigned-byte 8)))))
    ;; TODO: check out fd-stream-advance, -listen, -eofp, -force-output, -close
    ;; See if should specialize any of 'em.
    (make-fd-stream fd
		    :class class
		    :direction :io
		    :element-type element-type)))

(defun make-tcp-listener-socket (fd &rest keys &key backlog &allow-other-keys)
  (socket-call nil "listen" (c_listen fd (or backlog 5)))
  (make-instance 'listener-socket
		 :device fd
		 :keys keys))

(defun make-file-listener-socket (fd &rest keys &key backlog &allow-other-keys)
  (socket-call nil "listen" (c_listen fd (or backlog 5)))
  (make-instance 'file-listener-socket
		 :device fd
		 :keys keys))

(defun socket-accept (fd wait socket)
  (flet ((_accept (fd async)
	   (let ((res (c_accept fd (%null-ptr) (%null-ptr))))
	     (declare (fixnum res))
	     ;; See the inscrutable note under ERROR HANDLING in
	     ;; man accept(2). This is my best guess at what they mean...
	     (if (and async (< res 0)
		      (or (eql res (- #$ENETDOWN))
			  (eql res (- #+linuxppc-target #$EPROTO
				      #+darwinppc-target #$EPROTOTYPE))
			  (eql res (- #$ENOPROTOOPT))
			  (eql res (- #$EHOSTDOWN))
			  (eql res (- #+linuxppc-target #$ENONET
				      #+darwinppc-target #$ENETDOWN))
			  (eql res (- #$EHOSTUNREACH))
			  (eql res (- #$EOPNOTSUPP))
			  (eql res (- #$ENETUNREACH))))
	       (- #$EAGAIN)
	       res))))
    (cond (wait
	    (with-eagain fd :input
	      (_accept fd *multiprocessing-socket-io*)))
	  (*multiprocessing-socket-io*
	    (_accept fd t))
	  (t
	    (let ((old (socket-call socket "fcntl" (fd-get-flags fd))))
	      (unwind-protect
		  (progn
		    (socket-call socket "fcntl" (fd-set-flags fd (logior old #$O_NONBLOCK)))
		    (_accept fd t))
		(socket-call socket "fcntl" (fd-set-flags fd old))))))))

(defun accept-socket-connection (socket wait stream-create-function)
  (let ((listen-fd (socket-device socket))
	(fd -1))
    (unwind-protect
      (progn
	(setq fd (socket-accept listen-fd wait socket))
	(cond ((>= fd 0)
	       (prog1 (apply stream-create-function fd (socket-keys socket))
		 (setq fd -1)))
	      ((eql fd (- #$EAGAIN)) nil)
	      (t (socket-error socket "accept" fd))))
      (when (>= fd 0)
	(fd-close fd)))))

(defmethod ACCEPT-CONNECTION ((socket listener-socket) &key (wait t))
  (accept-socket-connection socket wait #'make-tcp-stream))

(defmethod ACCEPT-CONNECTION ((socket file-listener-socket) &key (wait t))
  (accept-socket-connection socket wait #'make-file-socket-stream))

(defun verify-socket-buffer (buf offset size)
  (unless offset (setq offset 0))
  (unless (<= (+ offset size) (length buf))
    (report-bad-arg size `(integer 0 ,(- (length buf) offset))))
  (multiple-value-bind (arr start) (array-data-and-offset buf)
    (setq buf arr offset (+ offset start)))
  ;; TODO: maybe should allow any raw vector
  (let ((subtype (typecode buf)))
    (unless (and (<= ppc32::min-8-bit-ivector-subtag subtype)
		 (<= subtype ppc32::max-8-bit-ivector-subtag))
      (report-bad-arg buf `(or (array character)
			       (array (unsigned-byte 8))
			       (array (signed-byte 8))))))
  (values buf offset))

(defmethod SEND-TO ((socket udp-socket) msg size
		    &key remote-host remote-port offset)
  (let ((fd (socket-device socket)))
    (multiple-value-setq (msg offset) (verify-socket-buffer msg offset size))
    (unless remote-host
      (setq remote-host (or (getf (socket-keys socket) :remote-host)
			    (remote-socket-info socket :host))))
    (unless remote-port
      (setq remote-port (or (getf (socket-keys socket) :remote-port)
			    (remote-socket-info socket :port))))
    (rlet ((sockaddr :sockaddr_in))
      (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET)
      (setf (pref sockaddr :sockaddr_in.sin_addr.s_addr)
	    (if remote-host (host-as-inet-host remote-host) #$INADDR_ANY))
      (setf (pref sockaddr :sockaddr_in.sin_port)
	    (if remote-port (port-as-inet-port remote-port "udp") 0))
      (%stack-block ((bufptr size))
        (%copy-ivector-to-ptr msg offset bufptr 0 size)
	(socket-call socket "sendto"
	  (with-eagain fd :output
	    (c_sendto fd bufptr size 0 sockaddr (record-length :sockaddr_in))))))))

(defmethod RECEIVE-FROM ((socket udp-socket) size &key buffer extract offset)
  (let ((fd (socket-device socket))
	(vec-offset offset)
	(vec buffer)
	(ret-size -1))
    (when vec
      (multiple-value-setq (vec vec-offset)
	(verify-socket-buffer vec vec-offset size)))
    (rlet ((sockaddr :sockaddr_in)
	   (namelen :signed))
      (setf (pref sockaddr :sockaddr_in.sin_family) #$AF_INET)
      (setf (pref sockaddr :sockaddr_in.sin_addr.s_addr) #$INADDR_ANY)
      (setf (pref sockaddr :sockaddr_in.sin_port) 0)
      (setf (pref namelen :signed) (record-length :sockaddr_in))
      (%stack-block ((bufptr size))
	(setq ret-size (socket-call socket "recvfrom"
			 (with-eagain fd :input
			   (c_recvfrom fd bufptr size 0 sockaddr namelen))))
	(unless vec
	  (setq vec (make-array ret-size
				:element-type
				(ecase (socket-format socket)
				  ((:text) 'base-character)
				  ((:binary :bivalent) '(unsigned-byte 8))))
		vec-offset 0))
	(%copy-ptr-to-ivector bufptr 0 vec vec-offset ret-size))
      (values (cond ((null buffer)
		     vec)
		    ((or (not extract)
			 (and (eql 0 (or offset 0))
			      (eql ret-size (length buffer))))
		     buffer)
		    (t 
		     (subseq vec vec-offset (+ vec-offset ret-size))))
	      ret-size
	      (ntohl (pref sockaddr :sockaddr_in.sin_addr.s_addr))
	      (ntohs (pref sockaddr :sockaddr_in.sin_port))))))

(defmethod SHUTDOWN (socket &key direction)
  ;; TODO: should we ignore ENOTCONN error?  (at least make sure it
  ;; is a distinct, catchable error type).
  (let ((fd (socket-device socket)))
    (socket-call socket "shutdown"
      (c_shutdown fd (ecase direction
		       (:input 0)
		       (:output 1))))))

;; Accepts port as specified by user, returns port number in network byte
;; order.  Protocol should be one of "tcp" or "udp".  Error if not known.
(defun port-as-inet-port (port proto)
  (or (etypecase port
	(fixnum (htons port))
	(string (_getservbyname port proto))
	(symbol (_getservbyname (string-downcase (symbol-name port)) proto)))
      (socket-error nil "getservbyname" (- #$ENOENT))))

(defun LOOKUP-PORT (port proto)
  (if (fixnump port)
    port
    (#+ppc-target progn #-ppc-target #_ntohs (port-as-inet-port port proto))))

;; Accepts host as specified by user, returns host number in network byte
;; order.
(defun host-as-inet-host (host)
  (etypecase host
    (integer (htonl host))
    (string (or (and (every #'(lambda (c) (position c ".0123456789")) host)
		     (_inet_aton host))
		(multiple-value-bind (addr err) (c_gethostbyname host)
		  (or addr
		      (socket-error nil "gethostbyname" err t)))))))


(defun DOTTED-TO-IPADDR (name &key (errorp t))
  (let ((addr (_inet_aton name)))
    (if addr (ntohl addr)
      (and errorp (error "Invalid dotted address ~s" name)))))
    
(defun LOOKUP-HOSTNAME (host)
  (if (typep host 'integer)
    host
    (ntohl (host-as-inet-host host))))

(defun IPADDR-TO-DOTTED (addr &key values)
  (if values
      (values (ldb (byte 8 24) addr)
	      (ldb (byte 8 16) addr)
	      (ldb (byte 8  8) addr)
	      (ldb (byte 8  0) addr))
    (_inet_ntoa (htonl addr))))

(defun IPADDR-TO-HOSTNAME (ipaddr &key ignore-cache)
  (declare (ignore ignore-cache))
  (multiple-value-bind (name err) (c_gethostbyaddr (htonl ipaddr))
    (or name (socket-error nil "gethostbyaddr" err t))))
  

(defun int-getsockopt (socket level optname)
  (rlet ((valptr :signed)
         (vallen :signed))
    (setf (pref vallen :signed) 4)
    (let* ((err (c_getsockopt socket level optname valptr vallen)))
      (if (and (eql 0 err)
               (eql 4 (pref vallen :signed)))
        (pref valptr :signed)
	(socket-error socket "getsockopt" err)))))

(defun int-setsockopt (socket level optname optval)
  (rlet ((valptr :signed))
    (setf (pref valptr :signed) optval)
    (socket-call socket "setsockopt"
      (c_setsockopt socket level optname valptr (record-length :signed)))))

(defloadvar *h-errno* (foreign-symbol-address #+darwinppc-target "_h_errno"
                                              #-darwinppc-target "h_errno"))
#+darwinppc-target
(defun c_gethostbyaddr (addr)
  (rlet ((addrp :unsigned))
    (setf (pref addrp :unsigned) addr)
    (without-interrupts
     (let* ((hp (#_gethostbyaddr addrp (record-length :unsigned) #$AF_INET)))
       (declare (dynamic-extent hp))
       (if (not (%null-ptr-p hp))
	 (%get-cstring (pref hp :hostent.h_name))
	 (values nil (pref *h-errno* :signed)))))))

#+linuxppc-target
(defun c_gethostbyaddr (addr)
  (rlet ((hostent :hostent)
	 (hp (* (struct :hostent)))
	 (herr :signed)
	 (addrp :unsigned))
    (setf (pref addrp :unsigned) addr)
    (do* ((buflen 1024 (+ buflen buflen))) ()
      (declare (fixnum buflen))
      (%stack-block ((buf buflen))
	(let* ((res (#_gethostbyaddr_r addrp (record-length :unsigned) #$AF_INET
				       hostent buf buflen hp herr)))
	  (declare (fixnum res))
	  (unless (eql res #$ERANGE)
	    (return
	     (if (and (eql res 0) (not (%null-ptr-p (%get-ptr hp))))
		 (%get-cstring (pref (%get-ptr hp) :hostent.h_name))
	       (values nil (- (pref herr :signed)))))))))))

#+darwinppc-target
(defun c_gethostbyname (name)
  (with-cstrs ((name (string name)))
    (without-interrupts
     (let* ((hp (#_gethostbyname  name)))
       (declare (dynamic-extent hp))
       (if (not (%null-ptr-p hp))
	 (%get-unsigned-long
	  (%get-ptr (pref hp :hostent.h_addr_list)))
	 (values nil (pref *h-errno* :signed)))))))

#+linuxppc-target
(defun c_gethostbyname (name)
  (with-cstrs ((name (string name)))
    (rlet ((hostent :hostent)
           (hp (* (struct :hostent)))
           (herr :signed))
       (do* ((buflen 1024 (+ buflen buflen))) ()
         (declare (fixnum buflen))
         (%stack-block ((buf buflen))
           (let* ((res (#_gethostbyname_r name hostent buf buflen hp herr)))
             (declare (fixnum res))
             (unless (eql res #$ERANGE)
	       (return
		 (if (eql res 0)
		   (%get-unsigned-long
		    (%get-ptr (pref (%get-ptr hp) :hostent.h_addr_list)))
		   (values nil (- (pref herr :signed))))))))))))

(defun _getservbyname (name proto)
  (with-cstrs ((name (string name))
	       (proto (string proto)))
    (let* ((servent-ptr (%null-ptr)))
      (declare (dynamic-extent servent-ptr))
      (%setf-macptr servent-ptr (#_getservbyname name proto))
      (unless (%null-ptr-p servent-ptr)
	(pref servent-ptr :servent.s_port)))))

#+linuxppc-target
(defun _inet_ntoa (addr)
  (rlet ((addrp :unsigned))
    (setf (pref addrp :unsigned) addr)
    (with-macptrs ((p))
      (%setf-macptr p (#_inet_ntoa addrp))
      (unless (%null-ptr-p p) (%get-cstring p)))))

#+darwinppc-target
(defun _inet_ntoa (addr)
  (with-macptrs ((p))
    (%setf-macptr p (external-call "_inet_ntoa"
				   :unsigned-fullword addr
				   :address))
    (unless (%null-ptr-p p) (%get-cstring p))))				   


(defun _inet_aton (string)
  (with-cstrs ((name string))
    (rlet ((addr :in_addr))
      (let* ((result (#_inet_aton name addr)))
	(unless (eql result 0)
	  (pref addr :in_addr.s_addr))))))

(defun c_socket (domain type protocol)
  #+darwinppc-target
  (syscall syscalls::socket domain type protocol)
  #+linuxppc-target
  (%stack-block ((params 12))
    (setf (%get-long params 0) domain
          (%get-long params 4) type
          (%get-long params 8) protocol)
    (syscall syscalls::socketcall 1 params)))

(defun init-unix-sockaddr (addr path)
  (macrolet ((sockaddr_un-path-len ()
               (/ (ensure-foreign-type-bits
                    (foreign-record-field-type 
                     (%find-foreign-record-type-field
                      (parse-foreign-type '(:struct :sockaddr_un)) :sun_path)))
                  8)))
    (let* ((name (native-translated-namestring path))
           (namelen (length name))
           (pathlen (sockaddr_un-path-len))
           (copylen (min (1- pathlen) namelen)))
        (setf (pref addr :sockaddr_un.sun_family) #$AF_LOCAL)
        (%copy-ivector-to-ptr name 0
                              (pref addr :sockaddr_un.sun_path) 0
                              copylen))))

(defun bind-unix-socket (socketfd path)
  (rletz ((addr :sockaddr_un))
    (init-unix-sockaddr addr path)
    (socket-call
     nil
     "bind"
     (c_bind socketfd
             addr
             (+ 2
                (#_strlen
                 (pref addr :sockaddr_un.sun_path)))))))
      

(defun c_bind (sockfd sockaddr addrlen)
  #+darwinppc-target
  (progn
    (setf (pref sockaddr :sockaddr_in.sin_len) addrlen)
    (syscall syscalls::bind sockfd sockaddr addrlen))
  #+linuxppc-target
  (%stack-block ((params 12))
    (setf (%get-long params 0) sockfd
          (%get-ptr params 4) sockaddr
          (%get-long params 8) addrlen)
    (syscall syscalls::socketcall 2 params)))

(defun c_connect (sockfd addr len)
  #+darwinppc-target
  (syscall syscalls::connect sockfd addr len)
  #+linuxppc-target
  (%stack-block ((params 12))
     (setf (%get-long params 0) sockfd
           (%get-ptr params 4) addr
           (%get-long params 8) len)
     (syscall syscalls::socketcall 3 params)))

(defun c_listen (sockfd backlog)
  #+darwinppc-target
  (syscall syscalls::listen sockfd backlog)
  #+linuxppc-target
  (%stack-block ((params 8))
     (setf (%get-long params 0) sockfd
           (%get-long params 4) backlog)
     (syscall syscalls::socketcall 4 params)))

(defun c_accept (sockfd addrp addrlenp)
  #+darwinppc-target
  (syscall syscalls::accept sockfd addrp addrlenp)
  #+linuxppc-target
  (%stack-block ((params 12))
    (setf (%get-long params 0) sockfd
          (%get-ptr params 4) addrp
          (%get-ptr params 8) addrlenp)
    (syscall syscalls::socketcall 5 params)))

(defun c_getsockname (sockfd addrp addrlenp)
  #+darwinppc-target
  (syscall syscalls::getsockname sockfd addrp addrlenp)
  #+linuxppc-target
  (%stack-block ((params 12))
    (setf (%get-long params 0) sockfd
          (%get-ptr params 4) addrp
          (%get-ptr params 8) addrlenp)
    (syscall syscalls::socketcall 6 params)))

(defun c_getpeername (sockfd addrp addrlenp)
  #+darwinppc-target
  (syscall syscalls::getpeername sockfd addrp addrlenp)
  #+linuxppc-target
  (%stack-block ((params 12))
    (setf (%get-long params 0) sockfd
          (%get-ptr params 4) addrp
          (%get-ptr params 8) addrlenp)
    (syscall syscalls::socketcall 7 params)))

(defun c_socketpair (domain type protocol socketsptr)
  #+darwinppc-target
  (syscall syscalls::socketpair domain type protocol socketsptr)
  #+linuxppc-target
  (%stack-block ((params 16))
    (setf (%get-long params 0) domain
          (%get-long params 4) type
          (%get-long params 8) protocol
          (%get-ptr params 12) socketsptr)
    (syscall syscalls::socketcall 8 params)))

(defun c_send (sockfd msgptr len flags)
  #+darwinppc-target
  (syscall syscalls::sendto sockfd msgptr len flags (%null-ptr) 0)
  #+linuxppc-target
  (%stack-block ((params 16))
    (setf (%get-long params 0) sockfd
	  (%get-ptr params  4) msgptr
	  (%get-long params 8) len
	  (%get-long params 12) flags)
    (syscall syscalls::socketcall 9 params)))

(defun c_recv (sockfd bufptr len flags)
  #+darwinppc-target
  (syscall syscalls::recvfrom sockfd bufptr len flags (%null-ptr) (%null-ptr))
  #+linuxppc-target
  (%stack-block ((params 16))
    (setf (%get-long params 0) sockfd
	  (%get-ptr params  4) bufptr
	  (%get-long params 8) len
	  (%get-long params 12) flags)
    (syscall syscalls::socketcall 10 params)))

(defun c_sendto (sockfd msgptr len flags addrp addrlen)
  #+darwinppc-target
  (syscall syscalls::sendto sockfd msgptr len flags addrp addrlen)
  #+linuxppc-target
  (%stack-block ((params 24))
    (setf (%get-long params 0) sockfd
	  (%get-ptr params  4) msgptr
	  (%get-long params 8) len
	  (%get-long params 12) flags
	  (%get-ptr params  16) addrp
	  (%get-long params 20) addrlen)
    (syscall syscalls::socketcall 11 params)))

(defun c_recvfrom (sockfd bufptr len flags addrp addrlenp)
  #+darwinppc-target
  (syscall syscalls::recvfrom sockfd bufptr len flags addrp addrlenp)
  #+linuxppc-target
  (%stack-block ((params 24))
    (setf (%get-long params 0) sockfd
	  (%get-ptr params  4) bufptr
	  (%get-long params 8) len
	  (%get-long params 12) flags
	  (%get-ptr params  16) addrp
	  (%get-ptr params  20) addrlenp)
    (syscall syscalls::socketcall 12 params)))

(defun c_shutdown (sockfd how)
  #+darwinppc-target
  (syscall syscalls::shutdown sockfd how)
  #+linuxppc-target
  (%stack-block ((params 8))
    (setf (%get-long params 0) sockfd
	  (%get-long params 4) how)
    (syscall syscalls::socketcall 13 params)))

(defun c_setsockopt (sockfd level optname optvalp optlen)
  #+darwinppc-target
  (syscall syscalls::setsockopt sockfd level optname optvalp optlen)
  #+linuxppc-target
  (%stack-block ((params 20))
    (setf (%get-long params 0) sockfd
          (%get-long params 4) level
          (%get-long params 8) optname
          (%get-ptr params 12) optvalp
          (%get-long params 16) optlen)
    (syscall syscalls::socketcall 14 params)))

(defun c_getsockopt (sockfd level optname optvalp optlenp)
  #+darwinppc-target
  (syscall syscalls::getsockopt sockfd level optname optvalp optlenp)
  #+linuxppc-target
  (%stack-block ((params 20))
    (setf (%get-long params 0) sockfd
          (%get-long params 4) level
          (%get-long params 8) optname
          (%get-ptr params 12) optvalp
          (%get-ptr params 16) optlenp)
    (syscall syscalls::socketcall 15 params)))

(defun c_sendmsg (sockfd msghdrp flags)
  #+darwinppc-target
  (syscall syscalls::sendmsg sockfd msghdrp flags)
  #+linuxppc-target
  (%stack-block ((params 12))
    (setf (%get-long params 0) sockfd
	  (%get-ptr params 4) msghdrp
	  (%get-long params 8) flags)
    (syscall syscalls::socketcall 16 params)))

(defun c_recvmsg (sockfd msghdrp flags)
  #+darwinppc-target
  (syscall syscalls::recvmsg sockfd msghdrp flags)
  #+linuxppc-target
  (%stack-block ((params 12))
    (setf (%get-long params 0) sockfd
	  (%get-ptr params 4) msghdrp
	  (%get-long params 8) flags)
    (syscall syscalls::socketcall 17 params)))

;;; Return a list of currently configured interfaces, a la ifconfig.
(defstruct ip-interface
  name
  addr
  netmask
  flags
  address-family)

(defun dump-buffer (p n)
  (dotimes (i n (progn (terpri) (terpri)))
    (unless (logtest i 15)
      (format t "~&~8,'0x: " (%ptr-to-int (%inc-ptr p i))))
    (format t " ~2,'0x" (%get-byte p i))))

(defun %get-ip-interfaces ()
  (let* ((buffsize 4000))
    (%stack-block ((buff buffsize))
      (rlet ((conf :ifconf :ifc_len buffsize))
	(setf (pref conf :ifconf.ifc_ifcu.ifcu_buf) buff)
        (let* ((fd (c_socket #$AF_INET #$SOCK_DGRAM 0)))
	  (if (< fd 0)
	    (error "Error creating socket."))
	  (if (< (#_ioctl fd #$SIOCGIFCONF :address conf) 0)
	    (error "Can't get interfaces configuration"))
	  (do* ((n (pref conf :ifconf.ifc_len))
		(offset 0)
		(req (pref conf :ifconf.ifc_ifcu.ifcu_req))
		(res ()))
	       ((>= offset n) (progn (fd-close fd) (nreverse res)))
	    (declare (fixnum offset n))
	    (let* ((sa_len #+darwinppc-target (pref (pref req :ifreq.ifr_ifru.ifru_addr) :sockaddr_in.sin_len)
			   #+linuxppc-target
			   (external-call "__libc_sa_len"
					  :unsigned-halfword
					  (pref (pref req :ifreq.ifr_ifru.ifru_addr) :sockaddr_in.sin_family)
					  :signed-fullword))
		   (delta
		    (max (+ 16 sa_len)
			 (%foreign-type-or-record-size :ifreq :bytes))))
	      (declare (fixnum sa_len delta))
	      ;(dump-buffer req delta)
	      (let* ((name (%get-cstring (pref req
					       #+darwinppc-target :ifreq.ifr_name
					       #+linuxppc-target :ifreq.ifr_ifrn.ifrn_name
					       ))))
		(unless (member name res
				:test #'string=
				:key #'ip-interface-name)
		  (when (zerop (#_ioctl fd #$SIOCGIFADDR :address req))
		    (let* ((addr (pref (pref req :ifreq.ifr_ifru.ifru_addr)
				       :sockaddr_in.sin_addr.s_addr))
			   (af (pref (pref req :ifreq.ifr_ifru.ifru_addr)
				     :sockaddr_in.sin_family)))
		      (when (zerop (#_ioctl fd #$SIOCGIFFLAGS :address req))
			(let* ((flags (pref req :ifreq.ifr_ifru.ifru_flags)))
			  (when (zerop (#_ioctl fd #$SIOCGIFNETMASK :address req))
			    (push (make-ip-interface
				   :name name
				   :addr addr
				   :netmask (pref (pref req :ifreq.ifr_ifru.ifru_addr)
						  :sockaddr_in.sin_addr.s_addr)
				   :flags flags
				   :address-family af)
				  res))))))))
	      (%incf-ptr req delta)
	      (incf offset delta))))))))


(defloadvar *ip-interfaces* ())

(defun ip-interfaces ()
  (or *ip-interfaces*
      (setq *ip-interfaces* (%get-ip-interfaces))))

;;; This should presumably happen after a configuration change.
;;; How do we detect a configuration change ?
(defun %reset-ip-interfaces ()
  (setq *ip-interfaces* ()))

;;; Return the first non-loopback interface that's up and whose address
;;; family is #$AF_INET.  If no such interface exists, return
;;; the loopback interface.
(defun primary-ip-interface ()
  (let* ((ifaces (ip-interfaces)))
    (or (find-if #'(lambda (i)
		     (and (eq #$AF_INET (ip-interface-address-family i))
			  (let* ((flags (ip-interface-flags i)))
			    (and (not (logtest #$IFF_LOOPBACK flags))
				 (logtest #$IFF_UP flags)))))
		 ifaces)
	(car ifaces))))

(defun primary-ip-interface-address ()
  (let* ((iface (primary-ip-interface)))
    (if iface
      (ip-interface-addr iface)
      (error "Can't determine primary IP interface"))))
	  
	  
(defmethod stream-io-error ((stream socket) errno where)
  (socket-error stream where errno))
