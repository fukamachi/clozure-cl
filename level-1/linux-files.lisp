;;;-*- Mode: Lisp; Package: CCL -*-
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

(eval-when (:compile-toplevel :execute)
  #+linuxppc-target
  (require "LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWIN-SYSCALLS"))


(defun nanoseconds (n)
  (check-type n (real 0 #xffffffff))
  (multiple-value-bind (q r)
      (floor n)
    (if (zerop r)
      (setq r 0)
      (setq r (floor (* r 1000000000))))
    (values q r)))
  
(defun semaphore-value (s)
  (if (istruct-typep s 'semaphore)
    (semaphore.value s)
    (semaphore-value (require-type s 'semaphore))))

(defun %wait-on-semaphore-ptr (s seconds nanoseconds)
   (zerop
    (the fixnum (ff-call
		 (%kernel-import ppc32::kernel-import-wait-on-semaphore)
		 :address s
		 :unsigned seconds
		 :unsigned nanoseconds
		 :signed))))

(defun %timed-wait-on-semaphore-ptr (s seconds nanoseconds)
  (process-wait "semaphore wait" #'%wait-on-semaphore-ptr s seconds nanoseconds))
  
(defun wait-on-semaphore (s)
  (%timed-wait-on-semaphore-ptr (semaphore-value s) 1 0))

(defun timed-wait-on-semaphore (s duration)
  (multiple-value-bind (secs nanos) (nanoseconds duration)
    (%wait-on-semaphore-ptr (semaphore-value s) secs nanos)))

(defun %signal-semaphore-ptr (p)
  (ff-call
   (%kernel-import ppc32::kernel-import-signal-semaphore)
   :address p
   :signed-fullword))

(defun signal-semaphore (s)
  (%signal-semaphore-ptr (semaphore-value s)))

(defun %os-getcwd (buf bufsize)
  ;; Return N < 0, if error
  ;;        N < bufsize: success, string is of length n
  ;;        N > bufsize: buffer needs to be larger.
  #+linuxppc-target
  (syscall os::getcwd buf bufsize)	; which is exactly what Linux does
  #+darwinppc-target
  (let* ((p (#_getcwd buf bufsize)))
    (declare (dynamic-extent p))
    (if (%null-ptr-p p)
      (let* ((err (%get-errno)))
	(if (eql err (- #$ERANGE))
	  (+ bufsize bufsize)
	  err))
      (dotimes (i bufsize (+ bufsize bufsize))
	(when (eql 0 (%get-byte buf i))
	  (return i))))))
    
    
(defun current-directory-name ()
  (flet ((try-getting-dirname (bufsize)
	   (%stack-block ((buf bufsize))
	     (let* ((len (%os-getcwd buf bufsize)))
	       (cond ((< len 0) (%errno-disp len bufsize))
		     ((< len bufsize)
		      (setf (%get-unsigned-byte buf len) 0)
		      (values (%get-cstring buf) len))
		     (t (values nil len)))))))
    (do* ((string nil)
	  (len 64)
	  (bufsize len len))
	 ((multiple-value-setq (string len) (try-getting-dirname bufsize))
	  string))))


(defun %chdir (dirname)
  (with-cstrs ((dirname dirname))
    (syscall os::chdir dirname)))

(defun %mkdir (name mode)
  (let* ((last (1- (length name))))
    (with-cstrs ((name name))
      (when (and (>= last 0)
		 (eql (%get-byte name last) (char-code #\/)))
	(setf (%get-byte name last) 0))
    (syscall os::mkdir name mode))))

(defun getenv (key)
  (with-cstrs ((key (string key)))
    (let* ((env-ptr (%null-ptr)))
      (declare (dynamic-extent env-ptr))
      (%setf-macptr env-ptr (#_getenv key))
      (unless (%null-ptr-p env-ptr)
	(%get-cstring env-ptr))))
  )

(defun setenv (key value &optional (overwrite t))
  (with-cstrs ((ckey key)
	       (cvalue value))
    (#_setenv ckey cvalue (if overwrite 1 0))))

(defun setuid (uid)
  (syscall os::setuid uid))

(defun setgid (uid)
  (syscall os::setgid uid))
  

;;; On Linux, "stat" & friends are implemented in terms of deeper,
;;; darker things that need to know what version of the stat buffer
;;; they're talking about.

(defun %stat-values (result stat)
  (if (eql 0 (the fixnum result)) 
      (values
       t
       (pref stat :stat.st_mode)
       (pref stat :stat.st_size)
       #+linuxppc-target
       (pref stat :stat.st_mtim.tv_sec)
       #+darwinppc-target
       (pref stat :stat.st_mtimespec.tv_sec)
       (pref stat :stat.st_ino)
       (pref stat :stat.st_uid))
      (values nil nil nil nil nil nil)))


(defun %%stat (name stat)
  (with-cstrs ((cname name))
    (%stat-values
     #+linuxppc-target
     (#_ __xstat #$_STAT_VER_LINUX cname stat)
     #+darwinppc-target
     (syscall os::stat cname stat)
     stat)))

(defun %%fstat (fd stat)
  (%stat-values
   #+linuxppc-target
   (#_ __fxstat #$_STAT_VER_LINUX fd stat)
   #+darwinppc-target
   (syscall os::fstat fd stat)
   stat))

(defun %%lstat (name stat)
  (with-cstrs ((cname name))
    (%stat-values
     #+linuxppc-target
     (#_ __lxstat #$_STAT_VER_LINUX cname stat)
     #+darwinppc-target
     (syscall os::lstat cname stat)
     stat)))


;;; Returns: (values t mode size mtime inode uid) on success,
;;;          (values nil nil nil nil nil nil) otherwise
(defun %stat (name &optional link-p)
  (rlet ((stat :stat))
    (if link-p
      (%%lstat name stat)
      (%%stat name stat))))

(defun %fstat (fd)
  (rlet ((stat :stat))
    (%%fstat fd stat)))


(defun %file-kind (mode)
  (when mode
    (let* ((kind (logand mode #$S_IFMT)))
      (cond ((eql kind #$S_IFDIR) :directory)
	    ((eql kind #$S_IFREG) :file)
	    ((eql kind #$S_IFLNK) :link)
	    ((eql kind #$S_IFIFO) :pipe)
	    ((eql kind #$S_IFSOCK) :socket)
	    ((eql kind #$S_IFCHR) :character-special)
	    (t :special)))))

(defun %unix-file-kind (path &optional check-for-link)
  (%file-kind (nth-value 1 (%stat path check-for-link))))

(defun %unix-fd-kind (fd)
  (if (isatty fd)
    :tty
    (%file-kind (nth-value 1 (%fstat fd)))))

(defun %uts-string (result idx buf)
  (if (eql 0 result)
    (%get-cstring (%inc-ptr buf (* #+linuxppc-target 65
				   #+darwinppc-target 256 idx)))
    "unknown"))


#+linuxppc-target
(defun %uname (idx)
  (%stack-block ((buf (* 65 6)))  
    (%uts-string (syscall os::uname buf) idx buf)))

#+darwinppc-target
(defun %uname (idx)
  (%stack-block ((buf (* 256 5)))
    (%uts-string (#_uname buf) idx buf)))

(defun fd-dup (fd)
  (syscall os::dup fd))

(defun fd-fsync (fd)
  (syscall os::fsync fd))

(defun fd-get-flags (fd)
  (syscall os::fcntl fd #$F_GETFL))

(defun fd-set-flags (fd new)
  (syscall os::fcntl fd #$F_SETFL new))

(defun fd-set-flag (fd mask)
  (let* ((old (fd-get-flags fd)))
    (if (< old 0)
      old
      (fd-set-flags fd (logior old mask)))))

(defun fd-clear-flag (fd mask)
  (let* ((old (fd-get-flags fd)))
    (if (< old 0) 
      old
      (fd-set-flags fd (logandc2 old mask)))))

;;; This doesn't seem to exist on VxWorks.  It's a POSIX
;;; function AFAIK, so the source should be somewhere ...

(defun %realpath (namestring)
  (%stack-block ((resultbuf 1024))
    (with-cstrs ((name namestring))
      (let* ((result (#_realpath name resultbuf)))
        (declare (dynamic-extent result))
        (unless (%null-ptr-p result)
          (%get-cstring result))))))

; Return fully resolved pathname & file kind, or (values nil nil)

(defun %probe-file-x (namestring)
  (let* ((realpath (%realpath namestring))
	 (kind (if realpath (%unix-file-kind realpath))))
    (if kind
      (values realpath kind)
      (values nil nil))))

(defun timeval->milliseconds (tv)
    (+ (* 1000 (pref tv :timeval.tv_sec)) (round (pref tv :timeval.tv_usec) 1000)))


(defun %add-timevals (result a b)
  (let* ((seconds (+ (pref a :timeval.tv_sec) (pref b :timeval.tv_sec)))
	 (micros (+ (pref a :timeval.tv_usec) (pref b :timeval.tv_usec))))
    (if (>= micros 1000000)
      (setq seconds (1+ seconds) micros (- micros 1000000)))
    (setf (pref result :timeval.tv_sec) seconds
	  (pref result :timeval.tv_usec) micros)
    result))




#-darwinppc-target
(defun %%rusage (usage &optional (who #$RUSAGE_SELF))
  (syscall os::getrusage who usage))
#+darwinppc-target
(defun %%rusage (usage &optional (who #$RUSAGE_SELF))
  (syscall os::getrusage who usage)
  (rlet ((count :natural_t #$TASK_THREAD_TIMES_INFO_COUNT))
    (#_task_info (#_mach_task_self) #$TASK_THREAD_TIMES_INFO usage count)))
    







(defconstant unix-to-universal-time 2208988800)

(defun %file-write-date (namestring)
  (let* ((date (nth-value 3 (%stat namestring))))
    (if date
      (+ date unix-to-universal-time))))

(defun %file-author (namestring)
  (let* ((uid (nth-value 5 (%stat namestring))))
    (if uid
      (with-macptrs ((pw (#_getpwuid uid)))
        (unless (%null-ptr-p pw)
          (without-interrupts
           (%get-cstring (pref pw :passwd.pw_name))))))))

(defun %utimes (namestring)
  (with-cstrs ((cnamestring namestring))
    (let* ((err (#_utimes cnamestring (%null-ptr))))
      (declare (fixnum err))
      (or (eql err 0)
          (%errno-disp err namestring)))))
         

(defun isatty (fd)
  (= 1 (#_isatty fd)))

(defun %open-dir (namestring)
  (with-cstrs ((name namestring))
    (let* ((DIR (#_opendir name)))
      (unless (%null-ptr-p DIR)
	DIR))))

(defun close-dir (dir)
  (#_closedir DIR))

(defun %read-dir (dir)
  (let* ((res (#_readdir dir)))
    (unless (%null-ptr-p res)	     
      (%get-cstring (pref res :dirent.d_name)))))

(defun tcgetpgrp (fd)
  (#_tcgetpgrp fd))

(defun getpid ()
  (syscall os::getpid))

(defun getuid ()
  (syscall os::getuid))

(defun get-user-home-dir (userid)
  (with-macptrs ((pw (#_getpwuid userid)))
    (unless (%null-ptr-p pw)
      (without-interrupts
       (%get-cstring (pref pw :passwd.pw_dir))))))

(defun %delete-file (name)
  (with-cstrs ((n name))
    (syscall os::unlink n)))

(defun os-command (string)
  (with-cstrs ((s string))
    (#_system s)))

(defun %strerror (errno)
  (declare (fixnum errno))
  (if (< errno 0)
    (setq errno (- errno)))
  (with-macptrs (p)
    (%setf-macptr p (#_strerror errno))
    (if (%null-ptr-p p)
      (format nil "OS Error %d" errno)
      (%get-cstring p))))

;;; Kind of has something to do with files, and doesn't work in level-0.
#+linuxppc-target
(defun close-shared-library (lib &key (completely t))
  (let* ((lib (if (typep lib 'string)
		(or (shared-library-with-name lib)
		    (error "Shared library ~s not found." lib))
		(require-type lib 'shlib)))
	 (map (shlib.map lib)))
    (unless (shlib.opened-by-lisp-kernel lib)
      (when map
	(let* ((found nil)
	       (base (shlib.base lib)))
	  (do* ()
	       ((progn		  
		  (#_dlclose map)
		  (or (not (setq found (shlib-containing-address base)))
		      (not completely)))))
	  (when (not found)
	    (setf (shlib.pathname lib) nil
	      (shlib.base lib) nil
	      (shlib.map lib) nil)
            (unload-foreign-variables lib)
	    (unload-library-entrypoints lib)))))))

#+darwinppc-target
;; completely specifies whether to remove it totally from our list
(defun close-shared-library (lib &key (completely nil))
  (let* ((lib (if (typep lib 'string)
		  (or (shared-library-with-name lib)
		      (error "Shared library ~s not found." lib))
		(require-type lib 'shlib))))
    ;; no possible danger closing libsystem since dylibs can't be closed
    (cond
     ((or (not (shlib.map lib)) (not (shlib.base lib)))
      (error "Shared library ~s uninitialized." (shlib.soname lib)))
     ((and (not (%null-ptr-p (shlib.map lib)))
	   (%null-ptr-p (shlib.base lib)))
      (warn "Dynamic libraries cannot be closed on Darwin."))
     ((and (%null-ptr-p (shlib.map lib))
	   (not (%null-ptr-p (shlib.base lib))))
      ;; we have a bundle type library
      ;; not sure what to do with the completely flag
      ;; when we open the same bundle more than once, Darwin gives back
      ;; a new module address, so we have multiple entries on *shared-libraries*
      ;; the best we can do is unlink the module asked for (or our best guess based on name)
      ;; and invalidate any entries which refer to this container
      (if (= 0 (#_NSUnLinkModule (shlib.base lib) #$NSUNLINKMODULE_OPTION_NONE))
	  (error "Unable to close shared library, NSUnlinkModule failed.")
	(progn
	  (setf (shlib.map lib) nil
		(shlib.base lib) nil)
	  (unload-library-entrypoints lib)
	  (when completely
	    (setq *shared-libraries* (delete lib *shared-libraries*)))))))))



;;; Foreign (unix) processes.

(defun call-with-string-vector (function strings)
  (let ((bufsize (reduce #'+ strings
			 :key #'(lambda (s) (1+ (length (string s))))))
	(argvsize (ash (1+ (length strings)) 2))
	(bufpos 0)
	(argvpos 0))
    (%stack-block ((buf bufsize) (argv argvsize))
      (flet ((init (s)
	     (multiple-value-bind (sstr start end) (get-sstring s)
	       (let ((len (- end start)))
		 (%copy-ivector-to-ptr sstr start buf bufpos len)
		 (setf (%get-byte buf (%i+ bufpos len)) 0)
		 (setf (%get-ptr argv argvpos) (%inc-ptr buf bufpos))
		 (setq bufpos (%i+ bufpos len 1))
		 (setq argvpos (%i+ argvpos 4))))))
	(declare (dynamic-extent #'init))
	(map nil #'init strings))
      (setf (%get-ptr argv argvpos) (%null-ptr))
      (funcall function argv))))

(defmacro with-string-vector ((var &rest strings) &body body)
  `(call-with-string-vector #'(lambda (,var) ,@body) ,@strings))

(defloadvar *max-os-open-files* (#_getdtablesize))

(defun %execvp (argv)
  (#_execvp (%get-ptr argv) argv)
  (#_exit #$EX_OSERR))

(defun exec-with-io-redirection (new-in new-out new-err argv)
  (#_setpgid 0 0)
  (if new-in (#_dup2 new-in 0))
  (if new-out (#_dup2 new-out 1))
  (if new-err (#_dup2 new-err 2))
  (do* ((fd 3 (1+ fd)))
       ((= fd *max-os-open-files*) (%execvp argv))
    (declare (fixnum fd))
    (#_close fd)))





#+linuxppc-target
(defun pipe ()
  (%stack-block ((pipes 8))
    (let* ((status (syscall os::pipe pipes)))
      (if (= 0 status)
	(values (%get-long pipes 0) (%get-long pipes 4))
	(%errno-disp status)))))

#+darwinppc-target
(defun pipe ()
  (let* ((ans (syscall os::pipe)))
    (if (< ans 0)
      (%errno-disp ans nil)
      (values (ldb (byte 32 32) ans) (ldb (byte 32 0) ans)))))


(defstruct external-process
  pid
  %status
  %exit-code
  pty
  input
  output
  error
  status-hook
  plist
  token
  core
  args
  (signal (make-semaphore))
  (completed (make-semaphore))
  watched-fd
  watched-stream
  )

(defmethod print-object ((p external-process) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (let* ((status (external-process-%status p)))
      (let* ((*print-length* 3))
	(format stream "~a" (external-process-args p)))
      (format stream "[~d] (~a" (external-process-pid p) status)
      (unless (eq status :running)
	(format stream " : ~d" (external-process-%exit-code p)))
      (format stream ")"))))

(defun get-descriptor-for (object proc close-in-parent close-on-error
				  &rest keys &key direction
				  &allow-other-keys)
  (etypecase object
    ((eql t)
     (values nil nil close-in-parent close-on-error))
    (null
     (let* ((fd (fd-open "/dev/null" (case direction
				       (:input #$O_RDONLY)
				       (:output #$O_WRONLY)
				       (t #$O_RDWR)))))
       (if (< fd 0)
	 (signal-file-error fd "/dev/null"))
       (values fd nil (cons fd close-in-parent) close-on-error)))
    ((eql :stream)
     (multiple-value-bind (read-pipe write-pipe) (pipe)
       (case direction
	 (:input
	  (values read-pipe
		  (make-fd-stream write-pipe
				  :direction :output
				  :interactive nil)
		  (cons read-pipe close-in-parent)
		  (cons write-pipe close-on-error)))
	 (:output
	  (values write-pipe
		  (make-fd-stream read-pipe
				  :direction :input
				  :interactive nil)
		  (cons write-pipe close-in-parent)
		  (cons read-pipe close-on-error)))
	 (t
	  (fd-close read-pipe)
	  (fd-close write-pipe)
	  (report-bad-arg direction '(member :input :output))))))
    ((or pathname string)
     (with-open-stream (file (apply #'open object keys))
       (values (fd-dup (ioblock-device (stream-ioblock file)))
	       nil
	       close-in-parent
	       close-on-error)))
    (fd-stream
       (values (fd-dup (ioblock-device (stream-ioblock object)))
	       nil
	       close-in-parent
	       close-on-error))
    (stream
     (ecase direction
       (:input
	(with-cstrs ((template "lisp-tempXXXXXX"))
	  (let* ((fd (#_mkstemp template)))
	    (if (< fd 0)
	      (%errno-disp fd))
	    (#_unlink template)
	    (loop
		(multiple-value-bind (line no-newline)
		    (read-line object nil nil)
		  (unless line
		    (return))
		  (let* ((len (length line)))
		    (%stack-block ((buf (1+ len)))
		      (%copy-ivector-to-ptr line 0 buf 0 len)
		      (fd-write fd buf len)
		      (if no-newline
			(return))
		      (setf (%get-byte buf) (char-code #\newline))
		      (fd-write fd buf 1)))))
	    (fd-lseek fd 0 #$SEEK_SET)
	    (values fd nil (cons fd close-in-parent) close-on-error))))
       (:output
	(multiple-value-bind (read-pipe write-pipe) (pipe)
          (setf (external-process-watched-fd proc) read-pipe
                (external-process-watched-stream proc) object)
          (incf (car (external-process-token proc)))
	  (values write-pipe
		  nil
		  (cons write-pipe close-in-parent)
		  (cons read-pipe close-on-error))))))))

(let* ((external-processes ())
       (external-processes-lock (make-lock)))
  (defun add-external-process (p)
    (with-lock-grabbed (external-processes-lock)
      (push p external-processes)))
  (defun remove-external-process (p)
    (with-lock-grabbed (external-processes-lock)
      (setq external-processes (delete p external-processes))))
  ;; Likewise
  (defun external-processes ()
    (with-lock-grabbed (external-processes-lock)
      (copy-list external-processes)))
  )



(defun monitor-external-process (p)
  (let* ((in-fd (external-process-watched-fd p))
         (out-stream (external-process-watched-stream p))
         (token (external-process-token p))
         (terminated))
    (loop
      (when (and terminated (null in-fd))
        (signal-semaphore (external-process-completed p))
        (return))
      (if in-fd
        (when (fd-input-available-p in-fd *ticks-per-second*)
          (%stack-block ((buf 1024))
            (let* ((n (fd-read in-fd buf 1024)))
              (declare (fixnum n))
              (if (<= n 0)
                (progn
                  (without-interrupts
                   (decf (car token))
                   (fd-close in-fd)
                   (setq in-fd nil)))
                (let* ((string (make-string 1024)))
                  (declare (dynamic-extent string))
                  (%copy-ptr-to-ivector buf 0 string 0 n)
                  (write-sequence string out-stream :end n)))))))
      (let* ((statusflags (check-pid (external-process-pid p)
                                     (logior
                                      (if in-fd #$WNOHANG 0)
                                      #$WUNTRACED)))
             (oldstatus (external-process-%status p)))
        (cond ((null statusflags)
               (remove-external-process p)
               (setq terminated t))
              ((eq statusflags t))	; Running.
              (t
               (multiple-value-bind (status code core)
                   (cond ((wifstopped statusflags)
                          (values :stopped (wstopsig statusflags)))
                         ((wifexited statusflags)
                          (values :exited (wexitstatus statusflags)))
                         (t
                          (let* ((signal (wtermsig statusflags)))
                            (declare (fixnum signal))
                            (values
                             (if (or (= signal #$SIGSTOP)
                                     (= signal #$SIGTSTP)
                                     (= signal #$SIGTTIN)
                                     (= signal #$SIGTTOU))
                               :stopped
                               :signaled)
                             signal
                             (logtest #$WCOREFLAG statusflags)))))
                 (setf (external-process-%status p) status
                       (external-process-%exit-code p) code
                       (external-process-core p) core)
                 (let* ((status-hook (external-process-status-hook p)))
                   (when (and status-hook (not (eq oldstatus status)))
                     (funcall status-hook p)))
                 (when (or (eq status :exited)
                           (eq status :signaled))
                   (remove-external-process p)
                   (setq terminated t)))))))))
      
(defun run-external-process (proc in-fd out-fd error-fd)
  (call-with-string-vector
   #'(lambda (argv)
       (let* ((child-pid (#_fork)))
	 (declare (fixnum child-pid))
	 (cond ((zerop child-pid)
		;; Running in the child; do an exec
		(without-interrupts
		 (exec-with-io-redirection
		  in-fd out-fd error-fd argv)))
	       ((> child-pid 0)
		;; Running in the parent: success
		(setf (external-process-pid proc) child-pid)
		(add-external-process proc)
		(signal-semaphore (external-process-signal proc))
                (monitor-external-process proc)))))
   (external-process-args proc)))

		
(defun run-program (program args &key
			    (wait t) pty
			    input if-input-does-not-exist
			    output (if-output-exists :error)
			    (error :output) (if-error-exists :error)
			    status-hook)
  (declare (ignore pty))
  (unless (every #'(lambda (a) (typep a 'simple-string)) args)
    (error "Program args must all be simple strings : ~s" args))
  (push program args)
  (let* ((token (list 0))
	 (in-fd nil)
	 (in-stream nil)
	 (out-fd nil)
	 (out-stream nil)
	 (error-fd nil)
	 (error-stream nil)
	 (close-in-parent nil)
	 (close-on-error nil)
	 (proc
          (make-external-process
           :pid nil
           :args args
           :%status :running
           :input nil
           :output nil
           :error nil
           :token token
           :status-hook status-hook)))
    (unwind-protect
	 (progn
	   (multiple-value-setq (in-fd in-stream close-in-parent close-on-error)
	     (get-descriptor-for input proc  nil nil :direction :input
				 :if-does-not-exist if-input-does-not-exist))
	   (multiple-value-setq (out-fd out-stream close-in-parent close-on-error)
	     (get-descriptor-for output proc close-in-parent close-on-error
				 :direction :output
				 :if-exists if-output-exists))
	   (multiple-value-setq (error-fd error-stream close-in-parent close-on-error)
	     (if (eq error :output)
	       (values out-fd out-stream close-in-parent close-on-error)
	       (get-descriptor-for error proc close-in-parent close-on-error
				   :direction :output
				   :if-exists if-error-exists)))
	   (setf (external-process-input proc) in-stream
                 (external-process-output proc) out-stream
                 (external-process-error proc) error-stream)
           (process-run-function
            (format nil "Monitor thread for external process ~a" args)
                    
            #'run-external-process proc in-fd out-fd error-fd)
           (wait-on-semaphore (external-process-signal proc))
      )

    (dolist (fd close-in-parent) (fd-close fd))
    (unless (external-process-pid proc)
      (dolist (fd close-on-error) (fd-close fd)))
    (when (and wait (external-process-pid proc))
      (wait-on-semaphore (external-process-completed proc))))
    (and (external-process-pid proc) proc)))

#|
       WIFEXITED(status)
              is non-zero if the child exited normally.

       WEXITSTATUS(status)
              evaluates to the least significant  eight  bits  of
              the  return  code  of  the  child which terminated,
              which may have been set as the argument to  a  call
              to exit() or as the argument for a return statement
              in the main program.  This macro can only be evalu­
           ated if WIFEXITED returned non-zero.

       WIFSIGNALED(status)
              returns true if the child process exited because of
              a signal which was not caught.

       WTERMSIG(status)
              returns the number of the signal  that  caused  the
              child  process to terminate. This macro can only be
              evaluated if WIFSIGNALED returned non-zero.

       WIFSTOPPED(status)
              returns true if the child process which caused  the
              return  is currently stopped; this is only possible
              if the call was done using WUNTRACED.

       WSTOPSIG(status)
              returns the number of the signal which  caused  the
              child to stop.  This macro can only be evaluated if
              WIFSTOPPED returned non-zero.

|#

(defmacro wtermsig (status)
  `(ldb (byte 7 0) ,status))

(defmacro wexitstatus (status)
  `(ldb (byte 8 8) (the fixnum ,status)))

(defmacro wstopsig (status)
  `(wexitstatus ,status))

(defmacro wifexited (status)
  `(eql (wtermsig ,status) 0))

(defmacro wifstopped (status)
  `(eql #x7f (ldb (byte 7 0) (the fixnum ,status))))

(defmacro wifsignaled (status)
  (let* ((statname (gensym)))
    `(let* ((,statname ,status))
      (and (not (wifstopped ,statname)) (not (wifexited ,statname))))))


(defun check-pid (pid &optional (flags (logior  #$WNOHANG #$WUNTRACED)))
  (declare (fixnum pid))
  (rlet ((status :signed))
    (let* ((retval (#_waitpid pid status flags)))
      (declare (fixnum retval))
      (if (= retval pid)
	(pref status :signed)
	(zerop retval)))))





(defun external-process-wait (proc &optional check-stopped)
  (process-wait "external-process-wait"
		#'(lambda ()
		    (case (external-process-%status proc)
		      (:running)
		      (:stopped
		       (when check-stopped
			 t))
		      (t
		       (when (zerop (car (external-process-token proc)))
			 t))))))

(defun external-process-status (proc)
  (require-type proc 'external-process)
  (values (external-process-%status proc)
	  (external-process-%exit-code proc)))

(defun external-process-input-stream (proc)
  (require-type proc 'external-process)
  (external-process-input proc))

(defun external-process-output-stream (proc)
  (require-type proc 'external-process)
  (external-process-output proc))

(defun external-process-error-stream (proc)
  (require-type proc 'external-process)
  (external-process-error proc))

(defun external-process-id (proc)
  (require-type proc 'external-process)
  (external-process-pid proc))
  
(defun signal-external-process (proc signal)
  (require-type proc 'external-process)
  (let* ((pid (external-process-pid proc))
	 (error (syscall os::kill pid signal)))
    (or (eql error 0)
	(%errno-disp error))))

;;; EOF on a TTY is transient, but I'm less sure of other cases.
(defun eof-transient-p (fd)
  (case (%unix-fd-kind fd)
    (:tty t)
    (t nil)))


(defstruct (shared-resource (:constructor make-shared-resource (name)))
  (name)
  (lock (make-lock))
  (primary-owner *current-process*)
  (primary-owner-notify (make-semaphore))
  (current-owner nil)
  (requestors (make-dll-header)))

(defstruct (shared-resource-request
	     (:constructor make-shared-resource-request (process))
	     (:include dll-node))
  process
  (signal (make-semaphore)))
	     

;; Returns NIL if already owned by calling thread, T otherwise
(defun %acquire-shared-resource (resource  &optional verbose)
  (let* ((current *current-process*))
    (with-lock-grabbed ((shared-resource-lock resource))
      (let* ((secondary (shared-resource-current-owner resource)))
	(if (or (eq current secondary)
		(and (null secondary)
		     (eq current (shared-resource-primary-owner resource))))
	  (return-from %acquire-shared-resource nil))))
    (let* ((request (make-shared-resource-request *current-process*)))
      (when verbose
	(format t "~%~%;;;~%;;; ~a requires access to ~a~%;;;~%~%"
		*current-process* (shared-resource-name resource)))
      (with-lock-grabbed ((shared-resource-lock resource))
	(append-dll-node request (shared-resource-requestors resource)))
      (wait-on-semaphore (shared-resource-request-signal request))
      (assert (eq current (shared-resource-current-owner resource)))
      (when verbose
	(format t "~%~%;;;~%;;; ~a is now owned by ~a~%;;;~%~%"
		(shared-resource-name resource) current))
      t)))

;;; If we're the primary owner and there is no secondary owner, do nothing.
;;; If we're the secondary owner, cease being the secondary owner.
(defun %release-shared-resource (r)
  (let* ((not-any-owner ()))
    (with-lock-grabbed ((shared-resource-lock r))
      (let* ((current *current-process*)
	     (primary (shared-resource-primary-owner r))
	     (secondary (shared-resource-current-owner r)))
	(unless (setq not-any-owner
		      (not (or (eq current secondary)
                               (and (null secondary)
                                    (eq current primary)))))
	  (when (eq current secondary)
	    (setf (shared-resource-current-owner r) nil)
	    (signal-semaphore (shared-resource-primary-owner-notify r))))))
    (when not-any-owner
      (signal-program-error "Process ~a does not own ~a" *current-process*
			    (shared-resource-name r)))))

;;; The current thread should be the primary owner; there should be
;;; no secondary owner.  Wakeup the specified (or first) requesting
;;; process, then block on our semaphore 
(defun %yield-shared-resource (r &optional to)
  (let* ((request nil))
    (with-lock-grabbed ((shared-resource-lock r))
      (let* ((current *current-process*)
	     (primary (shared-resource-primary-owner r)))
	(when (and (eq current primary)
		   (null (shared-resource-current-owner r)))
	  (setq request
		(let* ((header (shared-resource-requestors r)))
		  (if to 
		    (do-dll-nodes (node header)
		      (when (eq to (shared-resource-request-process node))
			(return node)))
		    (let* ((first (dll-header-first header)))
		      (unless (eq first header)
			first)))))
	  (when request
	    (remove-dll-node request)
            (setf (shared-resource-current-owner r)
                  (shared-resource-request-process request))
	    (signal-semaphore (shared-resource-request-signal request))))))
    (when request
      (wait-on-semaphore (shared-resource-primary-owner-notify r))
      (format t "~%;;;~%;;;control of ~a restored to ~a~%;;;~&"
	      (shared-resource-name r)
	      *current-process*))))


      

(defun %shared-resource-requestor-p (r proc)
  (with-lock-grabbed ((shared-resource-lock r))
    (do-dll-nodes (node (shared-resource-requestors r))
      (when (eq proc (shared-resource-request-process node))
	(return t)))))

