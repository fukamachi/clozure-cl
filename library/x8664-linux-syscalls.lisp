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






(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::read 0 (:unsigned-fullword :address :unsigned-fullword)
		:signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::write 1 (:unsigned-fullword :address :unsigned-fullword)
		:signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::open 2 (:address :unsigned-fullword :unsigned-fullword) :signed-fullword :min-args 2)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::close 3 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::stat 4 (:address :address) :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fstat 5 (:unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::lstat 6 (:address :address) :signed-fullword)

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::lseek 8 (:unsigned-fullword :signed-fullword :unsigned-fullword) :signed-fullword )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::exit 60 (:signed-fullword) :void)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fork 57 () :signed-fullword)




(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::creat 85 (:address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::link 86 (:address :address) :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::unlink 87 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::execve 59 (:address :address :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::chdir 80 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::time 201 (:address) :unsigned-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mknod 133 (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::chmod 90 (:address :unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::lchown 94 (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword)

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getpid 39 () :unsigned-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mount 165 (:address
				 :address
				 :address
				 :unsigned-fullword
				 :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::umount2 166 (:address) :signed-fullword )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setuid 105 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getuid 102 () :unsigned-fullword )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::ptrace 101 (:unsigned-fullword
				  :unsigned-fullword
				  :address
				  :address)
		:signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::alarm 37 (:unsigned-fullword) :unsigned-fullword )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::pause 34 () :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::utime 132 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::access 21 (:address :unsigned-fullword) :signed-fullword)

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sync 162 () :unsigned-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::kill 62 (:signed-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rename 82 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mkdir 83 (:address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rmdir 84 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::dup 32 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::pipe 22 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::times 100 (:address) :unsigned-fullword )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::brk 12 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setgid 106 (:unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getgid 104 () :unsigned-fullword )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::geteuid 107 () :unsigned-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getegid 108 () :unsigned-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::acct 163 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::umount2 166 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::ioctl 16 (:unsigned-fullword :signed-fullword :address) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fcntl 72 (:unsigned-fullword :signed-fullword :signed-fullword) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setpgid 109 (:signed-fullword :signed-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::umask 95 (:unsigned-fullword) :unsigned-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::chroot 161 (:address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::ustat 136 (:unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::dup2 33 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getppid 110 () :unsigned-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getpgrp 111 () :unsigned-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setsid 112 () :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt-sigaction 13 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getrusage 98 (:signed-fullword :address) :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::gettimeofday 96 (:address :address) :void)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::ftruncate 77 (:unsigned-fullword :unsigned-fullword)
		:signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fchmod 91 (:unsigned-fullword :unsigned-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fchmod 91 (:unsigned-fullword :unsigned-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::socket 41 (:signed-fullword :signed-fullword :signed-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::connect 42 (:signed-fullword :address :signed-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::accept 43 (:signed-fullword :address :address)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sendto 44 (:unsigned-fullword :address :unsigned-fullword :unsigned-fullword :address :unsigned-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64) syscalls::recvfrom 45 (:unsigned-fullword :address :unsigned-long :unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64) syscalls::sendmsg 46 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64) syscalls::recvmsg 47 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall  (logior platform-os-linux platform-cpu-x86 platform-word-size-64) syscalls::shutdown 48 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::bind 49 (:signed-fullword :address :signed-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::bind 49 (:signed-fullword :address :signed-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::listen 50 (:signed-fullword  :signed-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getsockname 51 (:signed-fullword :address :address)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getpeername 52 (:signed-fullword :address :address)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::socketpair 53 (:signed-fullword :signed-fullword :signed-fullword  :address)
		:signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64) syscalls::setsockopt 54 (:unsigned-fullword :signed-fullword :signed-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64) syscalls::getsockopt 55 (:unsigned-fullword :signed-fullword :unsigned-fullword :address :address) :signed-fullword )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fsync 118 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::uname 63  (:address) :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fchdir 133 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::_llseek 140 (:unsigned-fullword :unsigned-fullword :unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64) 	syscalls::select 23 (:unsigned-fullword :address :address
                                                  :address :address)
                :signed-fullword)
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getcwd 79 (:address :unsigned-fullword) :signed-fullword )


(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::futex 202 (:address :signed-fullword :signed-fullword :address :address :signed-fullword) :signed-fullword )

#+notdefinedyet
(progn
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sgetmask 68 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::ssetmask 69 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setreuid 70 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setregid 71 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sigsuspend 72 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sigpending 73 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sethostname 74 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setrlimit 75 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getrlimit 76 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::settimeofday 79 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getgroups 80 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setgroups 81 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::symlink 83 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::oldlstat 84 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::readlink 85 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::uselib 86 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::swapon 87 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::reboot 88 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::readdir 89 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::truncate 92 () )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fchown 95 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getpriority 96 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setpriority 97 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::statfs 99 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fstatfs 100 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::ioperm 101 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::syslog 103 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setitimer 38 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getitimer 36 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::olduname 109 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::iopl 110 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::vhangup 111 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::idle 112 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::vm86 113 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::wait4 114 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::swapoff 115 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sysinfo 116 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::ipc 117 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sigreturn 119 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::clone 120 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setdomainname 121 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::modify_ldt 123 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::adjtimex 124 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mprotect 10 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sigprocmask 126 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::create_module	127 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::init_module	128 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::delete_module	129 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::get_kernel_syms	130 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::quotactl 131 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getpgid 132 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::bdflush 134 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sysfs 135 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::personality 136 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setfsuid 138 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setfsgid 139 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getdents 141 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::_newselect 142 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::flock 143 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::msync 26 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::readv 19 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::writev 20 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getsid 147 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::fdatasync 148 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::_sysctl 149 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mlock 150 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::munlock 151 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mlockall 152 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::munlockall 153 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_setparam 154 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_getparam 155 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_setscheduler 156 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_getscheduler 157 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_yield 24 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_get_priority_max 159 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_get_priority_min 160 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sched_rr_get_interval 161 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::nanosleep 35 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mremap 25 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setresuid 164 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getresuid 165 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::query_module	166 () )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::nfsservctl 168 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::setresgid 169 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getresgid 170 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::prctl 171 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt_sigreturn 15 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt_sigaction 13 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt_sigprocmask 14 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt_sigpending 175 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt_sigtimedwait 176 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt_sigqueueinfo 177 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::rt_sigsuspend 178 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::pread 17 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::pwrite 18 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::chown 181 (:address) )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::capget 183 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::capset 184 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sigaltstack 185 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::sendfile 40 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::getpmsg 187	 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::putpmsg 188	 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::vfork 189 () )

(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::mmap 9 () )
(define-syscall (logior platform-os-linux platform-cpu-x86 platform-word-size-64)  syscalls::munmap 11 () )

)
