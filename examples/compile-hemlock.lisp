(in-package "CCL")

(defparameter *hemlock-src-dir-pathname* "ccl:hemlock;src;")

(defparameter *hemlock-binary-dir-pathname* "ccl:hemlock;bin;openmcl;")

(defparameter *hemlock-binary-file-extension*
  (pathname-type (compile-file-pathname "foo.lisp")))

(defun hemlock-source-pathname (name)
  (make-pathname :name name
                 :type "lisp"
                 :defaults *hemlock-src-dir-pathname*))

(defun hemlock-binary-pathname (name)
  (make-pathname :name name
                 :type *hemlock-binary-file-extension*
                 :defaults *hemlock-binary-dir-pathname*))

(defun compile-and-load-hemlock-file (name &optional force)
  (let* ((source-pathname (hemlock-source-pathname name))
	 (binary-pathname (hemlock-binary-pathname name)))
    (when (or force
	      (not (probe-file binary-pathname))
	      (> (file-write-date source-pathname)
		 (file-write-date binary-pathname)))
      (compile-file source-pathname :output-file binary-pathname :verbose t))
    (load binary-pathname :verbose t)))


(defparameter *hemlock-files*
  '("package"

    ;; Lisp implementation specific stuff goes into one of
    ;; the next two files.
    "lispdep"
    "hemlock-ext"                     
	       
    "decls"                             ;early declarations of functions and stuff
	       
    "struct"
    ;; "struct-ed"
    "charmacs"
    "key-event" 
    "keysym-defs"
    "cocoa-hemlock"
    "rompsite"
	       
    "input"
    "macros"
    "line"
    "ring"
    "vars"
    "interp"
    "syntax"
    "htext1"
    "buffer"  
    "htext2"
    "htext3"
    "htext4"
    "files"
    "search1"
    "search2"
    "table"
    #+clx
    "hunk-draw"
    #+clx
    "window"
    #-clx
    "modeline"
    #+clx
    "screen"
    #+clx
    "winimage"
    "linimage"
    #+clx
    "display"
    #+clx
    "bit-display"
	       
    #+nil "tty/termcap"
    #+nil "tty-disp-rt"
    #+nil "tty-display"
    "pop-up-stream"
    #+clx "bit-screen"
    #+nil "tty/tty-screen"
    "cursor"
    "font"
    "streams"
    #+nil "hacks"
    "main"
    "echo"
    "echocoms"
    "command"
    "indent"
    ;; moved     "comments"
    "morecoms"
    "undo"
    "killcoms"
    "searchcoms"
    "filecoms"
    "doccoms"
    "srccom"
    "group"
    "fill"
    "text"
    "lispmode"
    ;;     "ts-buf"
    ;;     "ts-stream"
    ;;     "eval-server"
    "lispbuf"
    ;;     "lispeval"
    ;;     "spell-rt"
    ;;     "spell-corr"
    ;;     "spell-aug"
    ;;     "spellcoms"
	       
    "comments"
    "overwrite"
    "abbrev"
    "icom"
    "kbdmac"
    "defsyn"
    #+why
    "scribe"
    #+what
    "pascal"
    #+who
    "dylan"
    "edit-defs"
    "auto-save"
    "register"
    "xcoms"
    ;;     "unixcoms"
    ;;     "mh"
    "highlight"
    ;;     "dired"
    ;;     "diredcoms"
    "bufed"
    "lisp-lib"
    "completion"
    ;;     "shell"
    ;;     "debug"
    ;;     "netnews"
    ;;     "rcs"
    "bindings"
    "bindings-gb"                       ;Gilbert's bindings
    ))  

(defun compile-hemlock (&optional force)
  (with-compilation-unit ()
    (dolist (name *hemlock-files*)
      (compile-and-load-hemlock-file name force)))
  (fasl-concatenate "ccl:library;hemlock"
                    (mapcar #'hemlock-binary-pathname *hemlock-files*)
                    :if-exists :supersede)
  )
