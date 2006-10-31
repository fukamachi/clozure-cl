; -*- Mode: Lisp; Package: CCL; -*-
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

(eval-when (:compile-toplevel)
  #+linuxppc-target
  (require "PPC-LINUX-SYSCALLS")
  #+linuxx8664-target
  (require "X8664-LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWINPPC-SYSCALLS")
  #+darwinx8664-target
  (require "DARWINX8664-SYSCALLS")
  #+freebsd-target
  (require "X8664-FREEBSD-SYSCALLS")
  )


; write nbytes bytes from buffer buf to file-descriptor fd.
(defun fd-write (fd buf nbytes)
  (syscall syscalls::write fd buf nbytes))

(defun fd-read (fd buf nbytes)
  (loop
    (let* ((n  (syscall syscalls::read fd buf nbytes)))
      (unless (eql n (- #$EINTR)) (return n)))))


(defun fd-open (path flags &optional (create-mode #o666))
  (with-cstrs ((p path))
    (syscall syscalls::open p flags create-mode)))

(defun fd-chmod (fd mode)
  (syscall syscalls::fchmod fd mode))

;;; This should really be conditionalized on whether the seek system
;;; call supports 64-bit offsets or on whether one has to use some
;;; variant.
#+(and ppc32-target linux-target)
(defun fd-lseek (fd offset whence)
  (let* ((high (ldb (byte 32 32) offset))
	 (low (ldb (byte 32 0) offset)))
    (declare (type (unsigned-byte 32) high low))
    (%stack-block ((pos 8))
      (let* ((res (syscall syscalls::_llseek fd high low pos whence)))
	(declare (fixnum res))
	(if (< res 0)
	  res
	  (let* ((pos-high (%get-unsigned-long pos 0))
		 (pos-low (%get-unsigned-long pos 4)))
	    (declare (type (unsigned-byte 32) pos-high pos-low))
	    (if (zerop pos-high)
	      pos-low
	      (dpb pos-high (byte 32 32) pos-low))))))))

#-(and ppc32-target linux-target)
(defun fd-lseek (fd offset whence)
  #+freebsd-target
  (syscall syscalls::lseek fd 0 offset whence)
  #-freebsd-target
  (syscall syscalls::lseek fd offset whence))

(defun fd-close (fd)
  (syscall syscalls::close fd)) 

(defun fd-tell (fd)
  (fd-lseek fd 0 #$SEEK_CUR))

;;; Kernels prior to 2.4 don't seem to have a "stat" variant
;;; that handles 64-bit file offsets.
(defun fd-size (fd)
  (without-interrupts
   (let* ((curpos (fd-lseek fd 0 #$SEEK_CUR)))
     (unwind-protect
	  (fd-lseek fd 0 #$SEEK_END)
       (fd-lseek fd curpos #$SEEK_SET)))))

(defun fd-ftruncate (fd new)
  (syscall syscalls::ftruncate fd new))

(defun %string-to-stderr (str)
  (with-cstrs ((s str))
    (fd-write 2 s (length str))))

(defun pdbg (string)
  (%string-to-stderr string)
  (%string-to-stderr #.(string #\LineFeed)))



;;; Not really I/O, but ...
(defun malloc (size)
  (ff-call 
   (%kernel-import target::kernel-import-malloc)
   :unsigned-fullword size :address))

(defun free (ptr)
  (ff-call 
   (%kernel-import target::kernel-import-free)
   :address ptr :void))


;;; Yield the CPU, via a platform-specific syscall.
(defun yield ()
  (%syscall target::yield-syscall :signed-fullword))

