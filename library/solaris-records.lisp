;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Opensourced MCL.
;;;
;;;   Opensourced MCL is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Lesser General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2.1 of the License, or (at your option) any later version.
;;;
;;;   Opensourced MCL is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;   Lesser General Public License for more details.
;;;
;;;   You should have received a copy of the GNU Lesser General Public
;;;   License along with this library; if not, write to the Free Software
;;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;

;;; Record & constant defs for Solaris

(defrecord timespec
  (tv-sec :long)
  (tv-nsec :long))


(defrecord solaris-stat
  (st-dev :unsigned-long)
  (st-pad1 (:array :long 3))
  (st-ino :unsigned-long)
  (st-mode :unsigned-long)
  (st-nlink :unsigned-long)
  (st-uid :unsigned-long)
  (st-gid :unsigned-long)
  (st-rdev :unsigned-long)
  (st-pad2 (:array :long 2))
  (st-size :long)
  (st-pad3 :long)
  (st-atime :timespec)             ; time of last access
  (st-mtime :timespec)             ; time of last modification
  (st-ctime :timespec)             ; time of last status change
  (st-blksize :long)
  (st-blocks :long)
  (st-fstype (:array :character 16))
  (st-pad4 (:array :long 8))
  )


(defconstant	S_IFMT		#xF000) ; type of file 
(defconstant	S_IAMB		#x1FF) ; access mode bits 
(defconstant	S_IFIFO		#x1000) ; fifo 
(defconstant	S_IFCHR		#x2000) ; character special 
(defconstant	S_IFDIR		#x4000) ; directory 
(defconstant	S_IFNAM		#x5000 ) ; XENIX special named file 
(defconstant	S_INSEM 	#x1) ; XENIX semaphore subtype of IFNAM 
(defconstant	S_INSHD 	#x2) ; XENIX shared data subtype of IFNAM 
(defconstant	S_IFBLK		#x6000) ; block special 
(defconstant	S_IFREG		#x8000) ; regular 
(defconstant	S_IFLNK		#xA000) ; symbolic link 
(defconstant	S_IFSOCK	#xC000) ; socket 
(defconstant	S_IFDOOR	#xD000) ; door 
(defconstant	S_ISUID		#x800) ; set user id on execution 
(defconstant	S_ISGID		#x400) ; set group id on execution 
(defconstant	S_ISVTX		#x200) ; save swapped text even after use 
(defconstant	S_IREAD		00400) ; read permission, owner 
(defconstant	S_IWRITE	00200) ; write permission, owner 
(defconstant	S_IEXEC		00100) ; execute/search permission, owner 
(defconstant	S_ENFMT		S_ISGID) ; record locking enforcement flag 

; fcntl/open modes & flags:
(defconstant O_RDONLY 0)
(defconstant O_WRONLY 1)
(defconstant O_RDWR 2)

(defconstant solaris_O_CREAT #x100)
(defconstant solaris_O_TRUNC #x200)
(defconstant solaris_O_EXCL #x400)
(defconstant solaris_O_NOCTTY #x800)

#+sparc-target
(progn
(defmacro external (name)
  `(load-eep ,name))

(defmacro external-call (name &rest args)
  `(ff-call (eep.address (load-time-value (external ,name))) ,@args))

(make-built-in-class 'external-entry-point *istruct-class*)

(defmethod make-load-form ((eep external-entry-point))
  `(load-eep ,(eep.name eep)))

)

(defrecord timeval
  (tv_sec :unsigned-long)
  (tv_usec :unsigned-long))

(defrecord timezone
  (tz_minuteswest :signed-long)
  (tz_dsttime :unsigned-long))

(defrecord tm
  (tm_sec :unsigned-long)
  (tm_min :unsigned-long)
  (tm_hour :unsigned-long)
  (tm_mday :unsigned-long)
  (tm_mon :unsigned-long)
  (tm_year :unsigned-long)
  (tm_wday :unsigned-long)
  (tm_yday :unsigned-long)
  (tm_isdst :unsigned-long)
  (__tm_gmtoff :signed-long))


(defrecord rusage
  (ru_utime :timeval)
  (ru_stime :timeval)
  (ru_maxrss :long)
  (ru_ixrss :long)      ; integral shared memory size 
  (ru_idrss :long)      ; integral unshared data size 
  (ru_isrss :long)      ; integral unshared stack size 
  (ru_minflt :long)          ; page reclaims 
  (ru_majflt :long)          ; page faults 
  (ru_nswap :long)      ; swaps 
  (ru_inblock :long)         ; block input operations 
  (ru_oublock :long)         ; block output operations 
  (ru_msgsnd :long)          ; messages sent 
  (ru_msgrcv :long)          ; messages received 
  (ru_nsignals :long)        ; signals received 
  (ru_nvcsw :long)      ; voluntary context switches 
  (ru_nivcsw :long)          ; involuntary context switches 
)

(defconstant RUSAGE-SELF 0)

(defrecord termios
  (c_iflag :unsigned-long)
  (c_oflag :unsigned-long)
  (c_cflag :unsigned-long)
  (c_lflag :unsigned-long)
  (c_cc (:array :unsigned-byte 19))
  (c_line :unsigned-byte)
  (c_ispeed :unsigned-long)
  (c_ospeed :unsigned-long))

(defrecord ptaskstate
  (nexttick unsigned-long)
  (interval unsigned-long)
  (private pointer)
  (flags unsigned-integer))

(defconstant kNoThreadID 0)
(defconstant kCurrentThreadID 1)
(defconstant kApplicationThreadID 2)

(defconstant kCooperativeThread (ash 1 0))
(defconstant kPreemptiveThread (ash 1 1))

(defconstant kNewSuspend (ash 1 0))
(defconstant kUsePremadeThread (ash 1 1))
(defconstant kCreateIfNeeded (ash 1 2))
(defconstant kFPUNotNeeded (ash 1 3))
(defconstant kExactMatchThread (ash 1 4))

; This matches the (define-storage-layout area ...) form in "ccl:compiler;ppc;ppc-arch.lisp"
(defrecord gc-area
  (pred (:pointer :gc-area))
  (succ (:pointer :gc-area))
  (low :ptr)
  (high :ptr)
  (active :ptr)
  (softlimit :ptr)
  (hardlimit :ptr)
  (code :long)
  (markbits :ptr)
  (ndwords :long)
  (older (:pointer :gc-area))
  (younger (:pointer :gc-area))
  (h :ptr)
  (softprot (:pointer :protected-area))
  (hardprot (:pointer :protected-area))
  (owner :long)
  (refbits :ptr)
  (threshold :ptr)
  (gc-count :long))

(defrecord protected-area
  (next (:pointer protected-area))
  (start :ptr)                          ; first byte (page-aligned) that might be protected
  (end :ptr)                            ; last byte (page-aligned) that could be protected
  (nprot :unsigned-long)                ; Might be 0
  (protsize :unsigned-long)             ; number of bytes to protect
  (why :long))

; This matches the xframe-list struct definition in "ccl:pmcl;constants.h"
(defrecord xframe-list
  (this (:pointer :ExceptionInformation))
  (prev (:pointer :xframe-list)))

(defrecord solaris-dirent
  (d_ino :long)
  (d_offset :unsigned-long)
  (d_reclen :unsigned-short)
  (d_name (:array :unsigned-byte 256)))

(defrecord solaris-passwd
  (pw_name :ptr)
  (pw_passwd :ptr)
  (pw_uid :unsigned-long)
  (pw_gid :unsigned-long)
  (pw_age :ptr)
  (pw_comment :ptr)
  (pw_gecos :ptr)
  (pw_dir :ptr)
  (pw_shell :ptr))

(defconstant solaris-FIONREAD #x4004667f)

(defrecord fpregset
  (fpu-fr (:array :unsigned-long 32))	;also an array of 16 DOUBLE-FLOATs
  (fpu-q :ptr)				;pointer to q of pending FP insts
  (fpu-fsr :unsigned-long)
  (fpu-qcnt :unsigned-byte)		;# of entries in fpu-q
  (fpu-q-entsize :unsigned-byte)	;size of each fpu-q entry
  (fpu-en :unsigned-byte)		;FPU-enabled flag
  (pad :unsigned-byte))

(defrecord mcontext
  (gregs (:array :unsigned-long 19))
  (gwins :ptr)
  (fpregs :fpregset)
  (xrs :ptr)
  (filler (:array :long 19)))

(defrecord stack-t
  (ss-sp :ptr)
  (ss-size :unsigned-long)
  (ss-flags :long))

(defrecord ucontext
  (uc-flags :unsigned-long)
  (uc-link :ptr)
  (uc-sigmask (:array :unsigned-long 4))
  (uc-stack :stack-t)
  ; The mcontext struct that follows has to be aligned on a doubleword
  ; boundary, but there's no way to tell DEFRECORD that
  (pad :unsigned-long)			; well, OK, there's this way.
  (uc-mcontext :mcontext)
  (uc-filler (:array :long 23)))

(defconstant REG-PC 1)
(defconstant REG-NPC 2)
(defconstant REG-Y 3)
(defconstant REG-G1 4)

(provide "SOLARIS-RECORDS")
