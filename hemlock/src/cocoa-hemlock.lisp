;;; -*- Mode: Lisp; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; Hemlock was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.

(in-package :hemlock-internals)

(defstruct command-interpreter-info
  (current-command (make-array 10 :fill-pointer 0 :adjustable t))
  (current-translation (make-array 10 :fill-pointer 0 :adjustable t))
  (last-command-type nil)
  (command-type-set nil)
  (prefix-argument nil)
  (prefix-argument-supplied nil)
  frame
  (function nil)
  )

(defvar *current-command-info* nil)

(defun buffer-windows (buffer)
  (let* ((doc (buffer-document buffer)))
    (when doc
      (document-panes doc))))

(defvar *current-window* ())

(defvar *window-list* ())
(defun current-window ()
  "Return the current window.  The current window is specially treated by
  redisplay in several ways, the most important of which is that is does
  recentering, ensuring that the Buffer-Point of the current window's
  Window-Buffer is always displayed.  This may be set with Setf."
  *current-window*)

(defun %set-current-window (new-window)
  #+not-yet
  (invoke-hook hemlock::set-window-hook new-window)
  #+clx
  (move-mark (window-point *current-window*)
	     (buffer-point (window-buffer *current-window*)))
  #+clx
  (move-mark (buffer-point (window-buffer new-window))
	     (window-point new-window))
  (setq *current-window* new-window))

;;; This is a public variable.
;;;
(defvar *last-key-event-typed* ()
  "This variable contains the last key-event typed by the user and read as
   input.")

(defvar *input-transcript* ())
