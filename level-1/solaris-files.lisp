;;;-*-Mode: LISP; Package: CCL -*-
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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "SOLARIS-RECORDS"))

(defun current-directory-name ()
  (do* ((bufsize 64 (+ bufsize bufsize)))
       ()
    (%stack-block ((buf bufsize))
      (unless (%null-ptr-p 
               (external-call "getcwd" 
                              :address buf :unsigned-fullword bufsize :address))
        (return (%get-cstring buf))))))

(defun %chdir (dirname)
  (with-cstrs ((dirname dirname))
    (external-call "chdir" :address dirname :signed-fullword)))

(defun %%stat (name stat)
  (with-cstrs ((cname name))
    (if (eql 0 (the fixnum (external-call "stat" 
                                          :address cname 
                                          :address stat 
                                          :signed-fullword)))
      (values t (pref stat :solaris-stat.st-mode) (pref stat :solaris-stat.st-size) (pref stat :solaris-stat.st-mtime.tv-sec))
      (values nil nil nil nil))))

(defun %%lstat (name stat)
  (with-cstrs ((cname name))
    (if (eql 0 (the fixnum (external-call "lstat" 
                                          :address cname 
                                          :address stat 
                                          :signed-fullword)))
      (values t (pref stat :solaris-stat.st-mode) (pref stat :solaris-stat.st-size) (pref stat :solaris-stat.st-mtime.tv-sec))
      (values nil nil nil nil))))

; Returns: (values t mode size mtime) on success, (values nil nil nil nil) otherwise
(defun %stat (name &optional link-p)
  (rlet ((stat :solaris-stat))
    (if link-p
      (%%lstat name stat)
      (%%stat name stat))))

(defun %unix-file-kind (path &optional check-for-link)
  (let* ((mode (nth-value 1 (%stat path check-for-link))))
    (when mode
      (let* ((kind (logand mode S_IFMT)))
        (cond ((eql kind S_IFDIR) :directory) 
             ((eql kind S_IFREG) :file)
              ((eql kind S_IFLNK) :link)
              (t :special))))))


(defun %realpath (namestring)
  (%stack-block ((resultbuf 1024))
    (with-cstrs ((name namestring))
      (let* ((result (external-call "realpath"
                                    :address name
                                    :address resultbuf
                                    :address)))
        (declare (dynamic-extent result))
        (unless (%null-ptr-p result)
          (%get-cstring result))))))

; Return fully resolved pathname & file kind, or (values nil nil)

(defun %probe-file-x (namestring)
  (let* ((realpath (%realpath namestring)))
    (if realpath
      (values realpath (%unix-file-kind realpath))
      (values nil nil))))

(defun timeval->milliseconds (tv)
    (+ (* 1000 (pref tv :timeval.tv_sec)) (round (pref tv :timeval.tv_usec) 1000)))

(defun get-internal-real-time ()
    (rlet ((tv :timeval))
      (external-call "gettimeofday" :address tv :address (%null-ptr) :void)
      (timeval->milliseconds tv)))

(defun %add-timevals (result a b)
    (let* ((seconds (+ (pref a :timeval.tv_sec) (pref b :timeval.tv_sec)))
           (millis (+ (pref a :timeval.tv_usec) (pref b :timeval.tv_usec))))
      (if (>= millis 1000000)
        (setq seconds (1+ seconds) millis (- millis 1000000)))
      (setf (pref result :timeval.tv_sec) seconds
            (pref result :timeval.tv_usec) millis)
      result))

(defun get-internal-run-time ()
  (rlet ((usage :rusage)
	 (total :timeval))
    (external-call "getrusage" 
		   :signed-fullword RUSAGE-SELF 
		   :address usage 
		   :signed-fullword)
    (timeval->milliseconds (%add-timevals total 
					  (pref usage :rusage.ru_utime) 
					  (pref usage :rusage.ru_stime)))))

(defconstant unix-to-universal-time 2208988800)

(defun %file-write-date (namestring)
  (let* ((date (nth-value 3 (%stat namestring))))
    (if date
      (+ date unix-to-universal-time))))

(defun isatty (fd)
  (= 1 (external-call "isatty" :signed-fullword fd :signed-fullword)))

(defun %open-dir (namestring)
  (with-cstrs ((name namestring))
    (let* ((DIR (external-call "opendir"
			       :address name
			       :address)))
      (unless (%null-ptr-p DIR)
	DIR))))

(defun close-dir (dir)
  (external-call "closedir" :address DIR :signed-fullword))

(defun %read-dir (dir)
  (let* ((res (external-call "readdir" :address dir
			     :address)))
    (unless (%null-ptr-p res)	     
      (%get-cstring (pref res :solaris-dirent.d_name)))))


(defun getuid ()
  (external-call "getuid" :unsigned-fullword))

(defun get-user-home-dir (userid)
  (with-macptrs ((pw (external-call "getpwuid"
				    :unsigned-fullword userid
				    :address)))
    (unless (%null-ptr-p pw)
      (without-interrupts
       (%get-cstring (pref pw :solaris-passwd.pw_dir))))))

(defun %delete-file (name)
  (with-cstrs ((n name))
    (external-call "unlink" :address n :signed-fullword)))

(defun os-command (string)
  (with-cstrs ((s string))
    (external-call "system" :address s :signed-fullword)))
