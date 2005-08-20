;;;-*-Mode: LISP; Package: CCL -*-
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

(defpackage "DARWIN-SYSCALLS" (:use)
            #+darwinppc-target (:nicknames "SYSCALLS"))

(defvar *darwin-syscalls* (make-hash-table :test #'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SYSCALL"))

(define-syscall	darwin-syscalls::exit 1 (:int) :void )
(define-syscall	darwin-syscalls::fork 2 () :void)
(define-syscall	darwin-syscalls::read 3 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall	darwin-syscalls::write 4 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall	darwin-syscalls::open 5 (:address :unsigned-fullword :unsigned-fullword) :signed-fullword :min-args 2 )
(define-syscall	darwin-syscalls::close 6 (:unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::wait4 7 (:unsigned-fullword :address :signed-fullword :address) :unsigned-fullword )
				; 8 is old creat 
(define-syscall	darwin-syscalls::link 9 (:address :address) :signed-fullword )
(define-syscall	darwin-syscalls::unlink 10 (:address) :signed-fullword )
				; 11 is obsolete execv 
(define-syscall	darwin-syscalls::chdir 12 (:address) :signed-fullword )
(define-syscall	darwin-syscalls::fchdir 13 (:unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::mknod 14  (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword )
(define-syscall	darwin-syscalls::chmod 15 (:address :unsigned-fullword) :signed-fullword )
(define-syscall darwin-syscalls::lchown 16 (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword)
(define-syscall	darwin-syscalls::getpid 20 () :signed-fullword )
(define-syscall darwin-syscalls::setuid 23 (:unsigned-fullword) :signed-fullword )
(define-syscall darwin-syscalls::getuid 24 () :unsigned-fullword )
(define-syscall	darwin-syscalls::geteuid 25 () :unsigned-fullword )
(define-syscall	darwin-syscalls::recvmsg 27 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall	darwin-syscalls::sendmsg 28 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall	darwin-syscalls::recvfrom 29 (:unsigned-fullword :address :unsigned-long :unsigned-fullword :address :address) :signed-fullword )
(define-syscall	darwin-syscalls::accept 30 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall	darwin-syscalls::getpeername 31 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall	darwin-syscalls::getsockname 32 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall	darwin-syscalls::kill 37 (:signed-fullword :unsigned-fullword) :signed-fullword )
(define-syscall darwin-syscalls::sync 36 () :unsigned-fullword )
				; 38 is old stat 
(define-syscall	darwin-syscalls::getppid 39 ()  :unsigned-fullword)
(define-syscall darwin-syscalls::dup 41 (:unsigned-fullword) :signed-fullword )
(define-syscall darwin-syscalls::pipe 42 () :signed-doubleword )
(define-syscall	darwin-syscalls::getgid 47 ()  :unsigned-fullword)
(define-syscall darwin-syscalls::ioctl 54 (:unsigned-fullword :signed-fullword :address) :signed-fullword :min-args 2 )
(define-syscall	darwin-syscalls::dup2 90 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::fcntl 92 (:unsigned-fullword :signed-fullword :signed-fullword) :signed-fullword :min-args 2 )
(define-syscall	darwin-syscalls::select 93 (:unsigned-fullword :address :address
                                                  :address :address)
                :signed-fullword)
(define-syscall	darwin-syscalls::fsync 95 (:unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::socket 97 (:unsigned-fullword :unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::connect 98 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword)
(define-syscall	darwin-syscalls::bind 104 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::setsockopt 105 (:unsigned-fullword :signed-fullword :signed-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::listen 106 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::gettimeofday 116 (:address :address) :signed-fullword )
(define-syscall	darwin-syscalls::getrusage 117 (:signed-fullword :address) :signed-fullword )
(define-syscall	darwin-syscalls::getsockopt 118 (:unsigned-fullword :signed-fullword :unsigned-fullword :address :address) :signed-fullword )
(define-syscall	darwin-syscalls::fchmod 124 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::rename 128 (:address :address) :signed-fullword)
				; 129 is old truncate 
				; 130 is old ftruncate 
(define-syscall	darwin-syscalls::sendto 133 (:unsigned-fullword :address :unsigned-fullword :unsigned-fullword :address :unsigned-fullword) :signed-fullword )

(define-syscall	darwin-syscalls::shutdown 134 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::socketpair 135 (:unsigned-fullword :unsigned-fullword :unsigned-fullword :address) :signed-fullword )

(define-syscall	darwin-syscalls::mkdir 136 (:address :unsigned-fullword) :signed-fullword)
(define-syscall	darwin-syscalls::rmdir 137 (:address) :signed-fullword )
(define-syscall darwin-syscalls::mount 167 (:address :address :unsigned-fullword :address) :signed-fullword )
(define-syscall	darwin-syscalls::setgid 181 (:unsigned-fullword) :signed-fullword )
(define-syscall	darwin-syscalls::stat 188 (:address :address) :signed-fullword )
(define-syscall	darwin-syscalls::fstat 189 (:unsigned-fullword :address) :signed-fullword )
(define-syscall	darwin-syscalls::lstat 190 (:address :address) :signed-fullword )
(define-syscall	darwin-syscalls::lseek 199 (:unsigned-fullword :signed-doubleword :unsigned-fullword) :signed-doubleword )
(define-syscall	darwin-syscalls::truncate 200 (:address :unsigned-doubleword) :signed-fullword )
(define-syscall	darwin-syscalls::ftruncate 201 (:unsigned-fullword :unsigned-doubleword) :signed-fullword )

#+notdefinedyet
(progn
				; 17 is obsolete sbreak 
				; 18 is old getfsstat 
				; 19 is old lseek 
				; 21 is obsolete mount 
				; 22 is obsolete umount 
(define-syscall	darwin-syscalls::ptrace 26 () )
(define-syscall	darwin-syscalls::access 33 () )
(define-syscall	darwin-syscalls::chflags 34 () )
(define-syscall	darwin-syscalls::fchflags 35 () )
				; 40 is old lstat 
(define-syscall	darwin-syscalls::getegid 43 () )
(define-syscall	darwin-syscalls::profil 44 () )
(define-syscall	darwin-syscalls::ktrace 45 () )
(define-syscall	darwin-syscalls::sigaction 46 () )
(define-syscall	darwin-syscalls::sigprocmask 48 () )
(define-syscall	darwin-syscalls::getlogin 49 () )
(define-syscall	darwin-syscalls::setlogin 50 () )
(define-syscall	darwin-syscalls::acct 51 () )
(define-syscall	darwin-syscalls::sigpending 52 () )
(define-syscall	darwin-syscalls::sigaltstack 53 () )
(define-syscall	darwin-syscalls::reboot 55 () )
(define-syscall	darwin-syscalls::revoke 56 () )
(define-syscall	darwin-syscalls::symlink 57 () )
(define-syscall	darwin-syscalls::readlink 58 () )
(define-syscall	darwin-syscalls::execve 59 () )
(define-syscall	darwin-syscalls::umask 60 () )
(define-syscall	darwin-syscalls::chroot 61 () )
				; 62 is old fstat 
				; 63 is unused 
				; 64 is old getpagesize 
(define-syscall	darwin-syscalls::msync 65 () )
(define-syscall	darwin-syscalls::vfork 66 () )
				; 67 is obsolete vread 
				; 68 is obsolete vwrite 
(define-syscall	darwin-syscalls::sbrk 69 () )
(define-syscall	darwin-syscalls::sstk 70 () )
				; 71 is old mmap 
				; 72 is obsolete vadvise 
(define-syscall	darwin-syscalls::munmap 73 () )
(define-syscall	darwin-syscalls::mprotect 74 () )
(define-syscall	darwin-syscalls::madvise 75 () )
				; 76 is obsolete vhangup 
				; 77 is obsolete vlimit 
(define-syscall	darwin-syscalls::mincore 78 () )
(define-syscall	darwin-syscalls::getgroups 79 () )
(define-syscall	darwin-syscalls::setgroups 80 () )
(define-syscall	darwin-syscalls::getpgrp 81 () )
(define-syscall	darwin-syscalls::setpgid 82 () )
(define-syscall	darwin-syscalls::setitimer 83 () )
				; 84 is old wait 
(define-syscall	darwin-syscalls::swapon 85 () )
(define-syscall	darwin-syscalls::getitimer 86 () )
				; 87 is old gethostname 
				; 88 is old sethostname 
(define-syscall darwin-syscalls::getdtablesize 89 () )


				; 94 is obsolete setdopt 
(define-syscall	darwin-syscalls::setpriority 96 () )
				; 99 is old accept 
(define-syscall	darwin-syscalls::getpriority 100 () )
				; 101 is old send 
				; 102 is old recv 
(define-syscall	darwin-syscalls::sigreturn 103 () )
				; 107 is obsolete vtimes 
				; 108 is old sigvec 
				; 109 is old sigblock 
				; 110 is old sigsetmask 
(define-syscall	darwin-syscalls::sigsuspend 111 () )
				; 112 is old sigstack 
				; 113 is old recvmsg 
				; 114 is old sendmsg 
				; 115 is obsolete vtrace 
				; 119 is obsolete resuba 
(define-syscall	darwin-syscalls::readv 120 () )
(define-syscall	darwin-syscalls::writev 121 () )
(define-syscall	darwin-syscalls::settimeofday 122 () )
(define-syscall	darwin-syscalls::fchown 123 () )
				; 125 is old recvfrom 
				; 126 is old setreuid 
				; 127 is old setregid 
(define-syscall	darwin-syscalls::flock 131 () )
(define-syscall	darwin-syscalls::mkfifo 132 () )
(define-syscall	darwin-syscalls::utimes 138 () )
				; 139 is unused 
(define-syscall	darwin-syscalls::adjtime 140 () )
				; 141 is old getpeername 
				; 142 is old gethostid 
				; 143 is old sethostid 
				; 144 is old getrlimit 
				; 145 is old setrlimit 
				; 146 is old killpg 
(define-syscall	darwin-syscalls::setsid 147 () )
				; 148 is obsolete setquota 
				; 149 is obsolete quota 
				; 150 is old getsockname 
				; 151 is reserved 
(define-syscall darwin-syscalls::setprivexec 152 () )
				; 153 is reserved 
				; 154 is reserved 
(define-syscall	darwin-syscalls::nfssvc 155 () )
				; 156 is old getdirentries 
(define-syscall	darwin-syscalls::statfs 157 () )
(define-syscall	darwin-syscalls::fstatfs 158 () )
(define-syscall darwin-syscalls::unmount 159 () )
				; 160 is obsolete async_daemon 
(define-syscall	darwin-syscalls::getfh 161 () )
				; 162 is old getdomainname 
				; 163 is old setdomainname 
				; 164 is obsolete pcfs_mount 
(define-syscall darwin-syscalls::quotactl 165 () )
				; 166 is obsolete exportfs	

				; 168 is obsolete ustat 
				; 169 is unused 
(define-syscall darwin-syscalls::table 170 () )
				; 171 is old wait_3 
				; 172 is obsolete rpause 
				; 173 is unused 
				; 174 is obsolete getdents 
(define-syscall darwin-syscalls::gc_control 175 () )
(define-syscall darwin-syscalls::add_profil 176 () )
				; 177 is unused 
				; 178 is unused 
				; 179 is unused 
(define-syscall darwin-syscalls::kdebug_trace 180        () )
(define-syscall	darwin-syscalls::setegid 182 () )
(define-syscall	darwin-syscalls::seteuid 183 () )
(define-syscall	darwin-syscalls::lfs_bmapv 184 () )
(define-syscall	darwin-syscalls::lfs_markv 185 () )
(define-syscall	darwin-syscalls::lfs_segclean 186 () )
(define-syscall	darwin-syscalls::lfs_segwait 187 () )
(define-syscall	darwin-syscalls::pathconf 191 () )
(define-syscall	darwin-syscalls::fpathconf 192 () )
(define-syscall	darwin-syscalls::getrlimit 194 () )
(define-syscall	darwin-syscalls::setrlimit 195 () )
(define-syscall darwin-syscalls::getdirentries 196 () )
(define-syscall	darwin-syscalls::mmap 197 () )
(define-syscall	darwin-syscalls::__syscall 198 () )
(define-syscall	darwin-syscalls::__sysctl 202 () )
(define-syscall darwin-syscalls::mlock 203 () )
(define-syscall darwin-syscalls::munlock 204 () )
(define-syscall	darwin-syscalls::undelete 205 () )
(define-syscall	darwin-syscalls::ATsocket 206 () )
(define-syscall	darwin-syscalls::ATgetmsg 207 () )
(define-syscall	darwin-syscalls::ATputmsg 208 () )
(define-syscall	darwin-syscalls::ATPsndreq 209 () )
(define-syscall	darwin-syscalls::ATPsndrsp 210 () )
(define-syscall	darwin-syscalls::ATPgetreq 211 () )
(define-syscall	darwin-syscalls::ATPgetrsp 212 () )
				; 213-215 are reserved for AppleTalk 
(define-syscall darwin-syscalls::mkcomplex 216  () )
(define-syscall darwin-syscalls::statv 217		 () )
(define-syscall darwin-syscalls::lstatv 218 			 () )
(define-syscall darwin-syscalls::fstatv 219 			 () )
(define-syscall darwin-syscalls::getattrlist 220 		 () )
(define-syscall darwin-syscalls::setattrlist 221		 () )
(define-syscall darwin-syscalls::getdirentriesattr 222 	 () )
(define-syscall darwin-syscalls::exchangedata 223 				 () )
(define-syscall darwin-syscalls::checkuseraccess 224  () )
(define-syscall darwin-syscalls::searchfs 225 () )

       				; 226 - 230 are reserved for HFS expansion 
       				; 231 - 249 are reserved  
(define-syscall darwin-syscalls::minherit 250 () )
(define-syscall	darwin-syscalls::semsys 251 () )
(define-syscall	darwin-syscalls::msgsys 252 () )
(define-syscall	darwin-syscalls::shmsys 253 () )
(define-syscall	darwin-syscalls::semctl 254 () )
(define-syscall	darwin-syscalls::semget 255 () )
(define-syscall	darwin-syscalls::semop 256 () )
(define-syscall	darwin-syscalls::semconfig 257 () )
(define-syscall	darwin-syscalls::msgctl 258 () )
(define-syscall	darwin-syscalls::msgget 259 () )
(define-syscall	darwin-syscalls::msgsnd 260 () )
(define-syscall	darwin-syscalls::msgrcv 261 () )
(define-syscall	darwin-syscalls::shmat 262 () )
(define-syscall	darwin-syscalls::shmctl 263 () )
(define-syscall	darwin-syscalls::shmdt 264 () )
(define-syscall	darwin-syscalls::shmget 265 () )
(define-syscall	darwin-syscalls::shm_open 266 () )
(define-syscall	darwin-syscalls::shm_unlink 267 () )
(define-syscall	darwin-syscalls::sem_open 268 () )
(define-syscall	darwin-syscalls::sem_close 269 () )
(define-syscall	darwin-syscalls::sem_unlink 270 () )
(define-syscall	darwin-syscalls::sem_wait 271 () )
(define-syscall	darwin-syscalls::sem_trywait 272 () )
(define-syscall	darwin-syscalls::sem_post 273 () )
(define-syscall	darwin-syscalls::sem_getvalue 274 () )
(define-syscall	darwin-syscalls::sem_init 275 () )
(define-syscall	darwin-syscalls::sem_destroy 276 () )
       				; 277 - 295 are reserved  
(define-syscall darwin-syscalls::load_shared_file 296 () )
(define-syscall darwin-syscalls::reset_shared_file 297 () )
       				; 298 - 323 are reserved  
(define-syscall darwin-syscalls::mlockall 324 () )
(define-syscall darwin-syscalls::munlockall 325 () )
				; 326 is reserved 
(define-syscall darwin-syscalls::issetugid 327 () )
)
