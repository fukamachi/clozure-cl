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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SYSCALL"))

(define-syscall	os::exit 1 (:int) :void )
(define-syscall	os::fork 2 () :void)
(define-syscall	os::read 3 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall	os::write 4 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall	os::open 5 (:address :unsigned-fullword :unsigned-fullword) :signed-fullword :min-args 2 )
(define-syscall	os::close 6 (:unsigned-fullword) :signed-fullword )
(define-syscall	os::wait4 7 (:unsigned-fullword :address :signed-fullword :address) :unsigned-fullword )
				; 8 is old creat 
(define-syscall	os::link 9 (:address :address) :signed-fullword )
(define-syscall	os::unlink 10 (:address) :signed-fullword )
				; 11 is obsolete execv 
(define-syscall	os::chdir 12 (:address) :signed-fullword )
(define-syscall	os::fchdir 13 (:unsigned-fullword) :signed-fullword )
(define-syscall	os::mknod 14  (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword )
(define-syscall	os::chmod 15 (:address :unsigned-fullword) :signed-fullword )
(define-syscall os::lchown 16 (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword)
(define-syscall	os::getpid 20 () :signed-fullword )
(define-syscall os::setuid 23 (:unsigned-fullword) :signed-fullword )
(define-syscall os::getuid 24 () :unsigned-fullword )
(define-syscall	os::geteuid 25 () :unsigned-fullword )
(define-syscall	os::recvmsg 27 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall	os::sendmsg 28 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall	os::recvfrom 29 (:unsigned-fullword :address :unsigned-long :unsigned-fullword :address :address) :signed-fullword )
(define-syscall	os::accept 30 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall	os::getpeername 31 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall	os::getsockname 32 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall	os::kill 37 (:signed-fullword :unsigned-fullword) :signed-fullword )
(define-syscall os::sync 36 () :unsigned-fullword )
				; 38 is old stat 
(define-syscall	os::getppid 39 ()  :unsigned-fullword)
(define-syscall os::dup 41 (:unsigned-fullword) :signed-fullword )
(define-syscall os::pipe 42 () :signed-doubleword )
(define-syscall	os::getgid 47 ()  :unsigned-fullword)
(define-syscall os::ioctl 54 (:unsigned-fullword :signed-fullword :address) :signed-fullword :min-args 2 )
(define-syscall	os::dup2 90 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	os::fcntl 92 (:unsigned-fullword :signed-fullword :signed-fullword) :signed-fullword :min-args 2 )
(define-syscall	os::select 93 (:unsigned-fullword :address :address
                                                  :address :address)
                :signed-fullword)
(define-syscall	os::fsync 95 (:unsigned-fullword) :signed-fullword )
(define-syscall	os::socket 97 (:unsigned-fullword :unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	os::connect 98 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword)
(define-syscall	os::bind 104 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall	os::setsockopt 105 (:unsigned-fullword :signed-fullword :signed-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall	os::listen 106 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	os::gettimeofday 116 (:address :address) :signed-fullword )
(define-syscall	os::getrusage 117 (:signed-fullword :address) :signed-fullword )
(define-syscall	os::getsockopt 118 (:unsigned-fullword :signed-fullword :unsigned-fullword :address :address) :signed-fullword )
(define-syscall	os::fchmod 124 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	os::rename 128 (:address :address) :signed-fullword)
				; 129 is old truncate 
				; 130 is old ftruncate 
(define-syscall	os::sendto 133 (:unsigned-fullword :address :unsigned-fullword :unsigned-fullword :address :unsigned-fullword) :signed-fullword )

(define-syscall	os::shutdown 134 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall	os::socketpair 135 (:unsigned-fullword :unsigned-fullword :unsigned-fullword :address) :signed-fullword )

(define-syscall	os::mkdir 136 (:address :unsigned-fullword) :signed-fullword)
(define-syscall	os::rmdir 137 (:address) :signed-fullword )
(define-syscall os::mount 167 (:address :address :unsigned-fullword :address) :signed-fullword )
(define-syscall	os::setgid 181 (:unsigned-fullword) :signed-fullword )
(define-syscall	os::stat 188 (:address :address) :signed-fullword )
(define-syscall	os::fstat 189 (:unsigned-fullword :address) :signed-fullword )
(define-syscall	os::lstat 190 (:address :address) :signed-fullword )
(define-syscall	os::lseek 199 (:unsigned-fullword :signed-doubleword :unsigned-fullword) :signed-doubleword )
(define-syscall	os::truncate 200 (:address :unsigned-doubleword) :signed-fullword )
(define-syscall	os::ftruncate 201 (:unsigned-fullword :unsigned-doubleword) :signed-fullword )

#+notdefinedyet
(progn
				; 17 is obsolete sbreak 
				; 18 is old getfsstat 
				; 19 is old lseek 
				; 21 is obsolete mount 
				; 22 is obsolete umount 
(define-syscall	os::ptrace 26 () )
(define-syscall	os::access 33 () )
(define-syscall	os::chflags 34 () )
(define-syscall	os::fchflags 35 () )
				; 40 is old lstat 
(define-syscall	os::getegid 43 () )
(define-syscall	os::profil 44 () )
(define-syscall	os::ktrace 45 () )
(define-syscall	os::sigaction 46 () )
(define-syscall	os::sigprocmask 48 () )
(define-syscall	os::getlogin 49 () )
(define-syscall	os::setlogin 50 () )
(define-syscall	os::acct 51 () )
(define-syscall	os::sigpending 52 () )
(define-syscall	os::sigaltstack 53 () )
(define-syscall	os::reboot 55 () )
(define-syscall	os::revoke 56 () )
(define-syscall	os::symlink 57 () )
(define-syscall	os::readlink 58 () )
(define-syscall	os::execve 59 () )
(define-syscall	os::umask 60 () )
(define-syscall	os::chroot 61 () )
				; 62 is old fstat 
				; 63 is unused 
				; 64 is old getpagesize 
(define-syscall	os::msync 65 () )
(define-syscall	os::vfork 66 () )
				; 67 is obsolete vread 
				; 68 is obsolete vwrite 
(define-syscall	os::sbrk 69 () )
(define-syscall	os::sstk 70 () )
				; 71 is old mmap 
				; 72 is obsolete vadvise 
(define-syscall	os::munmap 73 () )
(define-syscall	os::mprotect 74 () )
(define-syscall	os::madvise 75 () )
				; 76 is obsolete vhangup 
				; 77 is obsolete vlimit 
(define-syscall	os::mincore 78 () )
(define-syscall	os::getgroups 79 () )
(define-syscall	os::setgroups 80 () )
(define-syscall	os::getpgrp 81 () )
(define-syscall	os::setpgid 82 () )
(define-syscall	os::setitimer 83 () )
				; 84 is old wait 
(define-syscall	os::swapon 85 () )
(define-syscall	os::getitimer 86 () )
				; 87 is old gethostname 
				; 88 is old sethostname 
(define-syscall os::getdtablesize 89 () )


				; 94 is obsolete setdopt 
(define-syscall	os::setpriority 96 () )
				; 99 is old accept 
(define-syscall	os::getpriority 100 () )
				; 101 is old send 
				; 102 is old recv 
(define-syscall	os::sigreturn 103 () )
				; 107 is obsolete vtimes 
				; 108 is old sigvec 
				; 109 is old sigblock 
				; 110 is old sigsetmask 
(define-syscall	os::sigsuspend 111 () )
				; 112 is old sigstack 
				; 113 is old recvmsg 
				; 114 is old sendmsg 
				; 115 is obsolete vtrace 
				; 119 is obsolete resuba 
(define-syscall	os::readv 120 () )
(define-syscall	os::writev 121 () )
(define-syscall	os::settimeofday 122 () )
(define-syscall	os::fchown 123 () )
				; 125 is old recvfrom 
				; 126 is old setreuid 
				; 127 is old setregid 
(define-syscall	os::flock 131 () )
(define-syscall	os::mkfifo 132 () )
(define-syscall	os::utimes 138 () )
				; 139 is unused 
(define-syscall	os::adjtime 140 () )
				; 141 is old getpeername 
				; 142 is old gethostid 
				; 143 is old sethostid 
				; 144 is old getrlimit 
				; 145 is old setrlimit 
				; 146 is old killpg 
(define-syscall	os::setsid 147 () )
				; 148 is obsolete setquota 
				; 149 is obsolete quota 
				; 150 is old getsockname 
				; 151 is reserved 
(define-syscall os::setprivexec 152 () )
				; 153 is reserved 
				; 154 is reserved 
(define-syscall	os::nfssvc 155 () )
				; 156 is old getdirentries 
(define-syscall	os::statfs 157 () )
(define-syscall	os::fstatfs 158 () )
(define-syscall os::unmount 159 () )
				; 160 is obsolete async_daemon 
(define-syscall	os::getfh 161 () )
				; 162 is old getdomainname 
				; 163 is old setdomainname 
				; 164 is obsolete pcfs_mount 
(define-syscall os::quotactl 165 () )
				; 166 is obsolete exportfs	

				; 168 is obsolete ustat 
				; 169 is unused 
(define-syscall os::table 170 () )
				; 171 is old wait_3 
				; 172 is obsolete rpause 
				; 173 is unused 
				; 174 is obsolete getdents 
(define-syscall os::gc_control 175 () )
(define-syscall os::add_profil 176 () )
				; 177 is unused 
				; 178 is unused 
				; 179 is unused 
(define-syscall os::kdebug_trace 180        () )
(define-syscall	os::setegid 182 () )
(define-syscall	os::seteuid 183 () )
(define-syscall	os::lfs_bmapv 184 () )
(define-syscall	os::lfs_markv 185 () )
(define-syscall	os::lfs_segclean 186 () )
(define-syscall	os::lfs_segwait 187 () )
(define-syscall	os::pathconf 191 () )
(define-syscall	os::fpathconf 192 () )
(define-syscall	os::getrlimit 194 () )
(define-syscall	os::setrlimit 195 () )
(define-syscall os::getdirentries 196 () )
(define-syscall	os::mmap 197 () )
(define-syscall	os::__syscall 198 () )
(define-syscall	os::__sysctl 202 () )
(define-syscall os::mlock 203 () )
(define-syscall os::munlock 204 () )
(define-syscall	os::undelete 205 () )
(define-syscall	os::ATsocket 206 () )
(define-syscall	os::ATgetmsg 207 () )
(define-syscall	os::ATputmsg 208 () )
(define-syscall	os::ATPsndreq 209 () )
(define-syscall	os::ATPsndrsp 210 () )
(define-syscall	os::ATPgetreq 211 () )
(define-syscall	os::ATPgetrsp 212 () )
				; 213-215 are reserved for AppleTalk 
(define-syscall os::mkcomplex 216  () )
(define-syscall os::statv 217		 () )
(define-syscall os::lstatv 218 			 () )
(define-syscall os::fstatv 219 			 () )
(define-syscall os::getattrlist 220 		 () )
(define-syscall os::setattrlist 221		 () )
(define-syscall os::getdirentriesattr 222 	 () )
(define-syscall os::exchangedata 223 				 () )
(define-syscall os::checkuseraccess 224  () )
(define-syscall os::searchfs 225 () )

       				; 226 - 230 are reserved for HFS expansion 
       				; 231 - 249 are reserved  
(define-syscall os::minherit 250 () )
(define-syscall	os::semsys 251 () )
(define-syscall	os::msgsys 252 () )
(define-syscall	os::shmsys 253 () )
(define-syscall	os::semctl 254 () )
(define-syscall	os::semget 255 () )
(define-syscall	os::semop 256 () )
(define-syscall	os::semconfig 257 () )
(define-syscall	os::msgctl 258 () )
(define-syscall	os::msgget 259 () )
(define-syscall	os::msgsnd 260 () )
(define-syscall	os::msgrcv 261 () )
(define-syscall	os::shmat 262 () )
(define-syscall	os::shmctl 263 () )
(define-syscall	os::shmdt 264 () )
(define-syscall	os::shmget 265 () )
(define-syscall	os::shm_open 266 () )
(define-syscall	os::shm_unlink 267 () )
(define-syscall	os::sem_open 268 () )
(define-syscall	os::sem_close 269 () )
(define-syscall	os::sem_unlink 270 () )
(define-syscall	os::sem_wait 271 () )
(define-syscall	os::sem_trywait 272 () )
(define-syscall	os::sem_post 273 () )
(define-syscall	os::sem_getvalue 274 () )
(define-syscall	os::sem_init 275 () )
(define-syscall	os::sem_destroy 276 () )
       				; 277 - 295 are reserved  
(define-syscall os::load_shared_file 296 () )
(define-syscall os::reset_shared_file 297 () )
       				; 298 - 323 are reserved  
(define-syscall os::mlockall 324 () )
(define-syscall os::munlockall 325 () )
				; 326 is reserved 
(define-syscall os::issetugid 327 () )
)