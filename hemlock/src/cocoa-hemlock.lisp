;;; -*- Mode: Lisp; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; Hemlock was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.

(in-package :hemlock-internals)

(defstruct (frame-event-queue (:include ccl::locked-dll-header))
  (signal (ccl::make-semaphore)))

(defstruct (buffer-operation (:include ccl::dll-node))
  (thunk nil))

(defstruct (event-queue-node (:include ccl::dll-node)
                             (:constructor make-event-queue-node (event)))
  event)

(defun event-queue-insert (q node)
  (ccl::locked-dll-header-enqueue node q)
  (ccl::signal-semaphore (frame-event-queue-signal q)))

(defun enqueue-key-event (q event)
  (event-queue-insert q (make-event-queue-node event)))

(defun dequeue-key-event (q)
  (unless (listen-editor-input q)
    (let* ((document (buffer-document (current-buffer))))
      (when document
        (document-set-point-position document))))
  (ccl::wait-on-semaphore (frame-event-queue-signal q))
  (ccl::locked-dll-header-dequeue q))


(defun unget-key-event (event q)
  (ccl::with-locked-dll-header (q)
    (ccl::insert-dll-node-after (make-event-queue-node  event) q))
  (ccl::signal-semaphore (frame-event-queue-signal q)))

(defun timed-wait-for-key-event (q seconds)
  (let* ((signal (frame-event-queue-signal q)))
    (when (ccl:timed-wait-on-semaphore signal seconds)
      (ccl:signal-semaphore signal)
      t)))



  

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
  (activate-hemlock-view new-window)
  (setq *current-window* new-window))

;;; This is a public variable.
;;;
(defvar *last-key-event-typed* ()
  "This variable contains the last key-event typed by the user and read as
   input.")

(defvar *input-transcript* ())

(defparameter editor-abort-key-events (list #k"Control-g" #k"Control-G"))

(defmacro abort-key-event-p (key-event)
  `(member (event-queue-node-event ,key-event) editor-abort-key-events))


(defun get-key-event (q &optional ignore-pending-aborts)
  (do* ((e (dequeue-key-event q) (dequeue-key-event q)))
       ((typep e 'event-queue-node)
        (unless ignore-pending-aborts
          (when (abort-key-event-p e)
            (beep)
            (throw 'editor-top-level-catcher nil)))
        (setq *last-key-event-typed* (event-queue-node-event e)))
    (if (typep e 'buffer-operation)
      (funcall (buffer-operation-thunk e)))))

(defun listen-editor-input (q)
  (ccl::with-locked-dll-header (q)
    (not (eq (ccl::dll-header-first q) q))))
