;;;-*-Mode: LISP; Package: CCL -*-
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


(defvar *inhibit-abort* nil)

;;; If any bits in the *periodic-task-mask* are set in the
;;; ptaskstate.flags word of a periodic task, it will not be run
(defvar *periodic-task-mask* 0)

(defmethod print-object ((p periodic-task) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~s ~d"
	    (ptask.name p)
	    (pref (ptask.state p) :ptaskstate.interval))))

(defvar *periodic-task-lock* (make-lock))

(defun find-named-periodic-task (name)
  (dolist (task *%periodic-tasks%*)
    (when (eq name (ptask.name task))
      (return task))))

(defun %install-periodic-task (name function interval &optional 
                                    (flags 0)
                                    (privatedata (%null-ptr)))
  (with-lock-grabbed (*periodic-task-lock*)
   (let* ((already (find-named-periodic-task name))
          (state (if already (ptask.state already) 
		     (malloc (record-length ptaskstate))))
          (task (or already (%istruct 'periodic-task state name nil))))
     (setf (ptask.function task) function)
     (setf (rref state ptaskstate.interval) interval
           (rref state ptaskstate.flags) flags
           (rref state ptaskstate.private) privatedata
           (rref state ptaskstate.nexttick) (+ (get-tick-count) interval))
     (unless already (push task *%periodic-tasks%*))
     (let* ((interval-in-seconds (/ interval (float *ticks-per-second*))))
       (if (< interval-in-seconds *periodic-task-interval*)
         (set-periodic-task-interval interval-in-seconds)))
     task)))

(defmacro with-periodic-task-mask ((mask) &body body)
  (let ((thunk (gensym)))
    `(let ((,thunk #'(lambda () ,@body)))
       (funcall-with-periodic-task-mask ,mask ,thunk))))

(defvar *periodic-task-masks* nil)

; All this hair is so that multiple processes can vote on the *periodic-task-mask*
(defun funcall-with-periodic-task-mask (mask  thunk)
  (let* ((cell (list mask)))
    (declare (dynamic-extent cell))
    (flet ((logior-list (list)
             (declare (type list list))
             (let ((res 0))
               (declare (fixnum res))
               (loop
                 (when (null list) (return res))
                 (setq res (%ilogior res (pop list)))))))
      (declare (inline logior-list))
      (unwind-protect
        (progn
          (without-interrupts
           (setf (cdr cell) *periodic-task-masks*
                 *periodic-task-masks* cell)
           (setq *periodic-task-mask* (logior-list *periodic-task-masks*))
)
          (funcall thunk))
        (without-interrupts
         (let* ((first *periodic-task-masks*)
                (this first)
                (last nil))
           (declare (type cons first this last))
           (loop
             (when (eq this cell)
               (if last
                 (setf (cdr last) (cdr this))
                 (pop first))
               (return (setq *periodic-task-masks* first)))
             (setq last this
                   this (cdr this))))
         (setq *periodic-task-mask* (logior-list *periodic-task-masks*)))))))


(defun force-break-in-listener (p)
  (process-interrupt p
		     #'(lambda ()
			 (ignoring-without-interrupts 
			  (break)
			  (clear-input *terminal-io*)))))




(defglobal *running-periodic-tasks* nil)

(defun cmain ()
  (thread-handle-interrupts))

(defun housekeeping ()
  (progn
    (handle-gc-hooks)
    (unless *inhibit-abort*
      (when (and (break-event-pending-p) *interactive-abort-process*)
        (force-break-in-listener *interactive-abort-process*)))
    (flet ((maybe-run-periodic-task (task)
             (let ((now (get-tick-count))
                   (state (ptask.state task)))
               (when (and (>= (- now (rref state ptaskstate.nexttick))
                              0)
                          (eql 0 (logand (the fixnum (rref state ptaskstate.flags))
                                         (the fixnum *periodic-task-mask*))))
                 (setf (rref state ptaskstate.nexttick) (+ now (rref state ptaskstate.interval)))
                 (funcall (ptask.function task))))))
      (let ((event-dispatch-task *event-dispatch-task*))
        (maybe-run-periodic-task event-dispatch-task)
        (with-lock-grabbed (*periodic-task-lock*)
          (bitclrf $gc-allow-stack-overflows-bit *gc-event-status-bits*)
          (unless *running-periodic-tasks*
            (let-globally ((*running-periodic-tasks* t))
              (dolist (task *%periodic-tasks%*)
                (unless (eq task event-dispatch-task)
                  (maybe-run-periodic-task task))))))))))


(defun %remove-periodic-task (name)
  (with-lock-grabbed (*periodic-task-lock*)
    (let ((task (find-named-periodic-task name)))
      (when task
        (if (setq *%periodic-tasks%* (delete task *%periodic-tasks%*))
          (let* ((min-ticks most-positive-fixnum))
            (dolist (other *%periodic-tasks%*
                     (set-periodic-task-interval (/ min-ticks (float *ticks-per-second*))))
              (let* ((other-ticks
                      (pref (ptask.state other) :ptaskstate.interval)))
                (if (< other-ticks min-ticks)
                  (setq min-ticks other-ticks)))))
          (set-periodic-task-interval 1)))
      task)))


(defun event-poll ()
  (with-lock-grabbed-maybe (*auto-flush-streams-lock*)
    (dolist (s *auto-flush-streams*)
      (when (open-stream-p s)
	(stream-maybe-force-output s)))))


; Is it really necessary to keep this guy in a special variable ?
(defloadvar *event-dispatch-task* 
  (%install-periodic-task 
   'event-poll
   'event-poll
   33
   (+ $ptask_draw-flag $ptask_event-dispatch-flag)))


(defun event-ticks ()
  (let ((task *event-dispatch-task*))
    (when task (rref (ptask.state task) ptaskstate.interval))))

(defun set-event-ticks (n)
  (setq n (require-type n '(integer 0 3767)))   ;  Why this weird limit ?
  (let ((task *event-dispatch-task*))
    (when task (setf (rref (ptask.state task) ptaskstate.interval) n))))

;; Making the *initial-process* quit will cause an exit(),
;; though it might be nicer if all processes were shut down
;; in an orderly manner first.  This is the not-so-nice way
;; of quitting ...
(defun %quit ()
  (quit))



; end of L1-events.lisp

