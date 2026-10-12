;;; Copyright 2026 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Worker thread pool for gRPC

(in-package #:grpc)

(defvar *default-thread-pool-size* 16
  "Default maximum number of worker threads in a gRPC thread pool.")

(defstruct (thread-pool (:constructor %make-thread-pool))
  "A thread-safe pool of bordeaux-threads worker threads executing queued tasks."
  (name "gRPC Worker" :type string)
  (max-threads *default-thread-pool-size* :type fixnum)
  (tasks-head nil :type list)
  (tasks-tail nil :type list)
  (pending-tasks 0 :type fixnum)
  (lock (bordeaux-threads:make-lock "grpc-thread-pool-lock"))
  (condvar (bordeaux-threads:make-condition-variable :name "grpc-thread-pool-cv"))
  (idle-condvar (bordeaux-threads:make-condition-variable :name "grpc-thread-pool-idle-cv"))
  (threads nil :type list)
  (num-threads 0 :type fixnum)
  (idle-threads 0 :type fixnum)
  (active-tasks 0 :type fixnum)
  (next-thread-id 0 :type fixnum)
  (shutdown-p nil :type boolean))

(defun %enqueue-task (pool task-fn)
  "Appends TASK-FN to the tail of POOL's task queue. Must be called with
POOL's lock held."
  (let ((cell (list task-fn)))
    (if (thread-pool-tasks-tail pool)
        (setf (cdr (thread-pool-tasks-tail pool)) cell
              (thread-pool-tasks-tail pool) cell)
        (setf (thread-pool-tasks-head pool) cell
              (thread-pool-tasks-tail pool) cell))
    (incf (thread-pool-pending-tasks pool))))

(defun %dequeue-task (pool)
  "Pops and returns the next task function from POOL's task queue, or NIL if
empty. Must be called with POOL's lock held."
  (let ((head (thread-pool-tasks-head pool)))
    (when head
      (setf (thread-pool-tasks-head pool) (cdr head))
      (unless (thread-pool-tasks-head pool)
        (setf (thread-pool-tasks-tail pool) nil))
      (decf (thread-pool-pending-tasks pool))
      (car head))))

(defun %thread-pool-worker-loop (pool)
  "Worker loop that pulls and executes tasks from POOL until shut down."
  (unwind-protect
       (loop
         (let ((task nil))
           (bordeaux-threads:with-lock-held ((thread-pool-lock pool))
             (loop
               (setf task (%dequeue-task pool))
               (when task
                 (incf (thread-pool-active-tasks pool))
                 (when (and (thread-pool-tasks-head pool)
                            (plusp (thread-pool-idle-threads pool)))
                   (bordeaux-threads:condition-notify (thread-pool-condvar pool)))
                 (return))
               (when (thread-pool-shutdown-p pool)
                 (bordeaux-threads:condition-notify (thread-pool-condvar pool))
                 (return))
               (incf (thread-pool-idle-threads pool))
               (unwind-protect
                    (bordeaux-threads:condition-wait
                     (thread-pool-condvar pool)
                     (thread-pool-lock pool))
                 (decf (thread-pool-idle-threads pool)))))
           (unless task
             (return))
           (unwind-protect
                (ignore-errors (funcall task))
             (bordeaux-threads:with-lock-held ((thread-pool-lock pool))
               (decf (thread-pool-active-tasks pool))
               (when (and (null (thread-pool-tasks-head pool))
                          (zerop (thread-pool-active-tasks pool)))
                 (bordeaux-threads:condition-notify
                  (thread-pool-idle-condvar pool)))))))
    (bordeaux-threads:with-lock-held ((thread-pool-lock pool))
      (decf (thread-pool-num-threads pool))
      (bordeaux-threads:condition-notify (thread-pool-condvar pool))
      (bordeaux-threads:condition-notify (thread-pool-idle-condvar pool)))))

(defun %spawn-thread-pool-worker-locked (pool)
  "Spawns a new worker thread in POOL. Must be called with POOL's lock held."
  (let ((thread-id (incf (thread-pool-next-thread-id pool))))
    (incf (thread-pool-num-threads pool))
    (handler-case
        (let ((thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (%thread-pool-worker-loop pool))
                 :name (format nil "~A ~D" (thread-pool-name pool) thread-id))))
          (push thread (thread-pool-threads pool))
          thread)
      (error (e)
        (decf (thread-pool-num-threads pool))
        (error e)))))

(defun make-thread-pool (&key (name "gRPC Worker")
                              (max-threads *default-thread-pool-size*)
                              (min-threads 0))
  "Creates and returns a THREAD-POOL with up to MAX-THREADS workers and
MIN-THREADS initially spawned workers."
  (assert (and (integerp max-threads) (plusp max-threads))
          (max-threads)
          "MAX-THREADS must be a positive integer, got ~S." max-threads)
  (assert (and (integerp min-threads) (<= 0 min-threads max-threads))
          (min-threads)
          "MIN-THREADS must be between 0 and MAX-THREADS (~D), got ~S."
          max-threads min-threads)
  (let ((pool (%make-thread-pool :name name :max-threads max-threads)))
    (when (plusp min-threads)
      (bordeaux-threads:with-lock-held ((thread-pool-lock pool))
        (dotimes (i min-threads)
          (%spawn-thread-pool-worker-locked pool))))
    pool))

(defun thread-pool-submit (pool task-fn)
  "Submits TASK-FN (a zero-argument function) to POOL for execution.
Returns T if the task was queued, or NIL if POOL has already been shut down."
  (declare (type thread-pool pool)
           (type function task-fn))
  (bordeaux-threads:with-lock-held ((thread-pool-lock pool))
    (unless (thread-pool-shutdown-p pool)
      (%enqueue-task pool task-fn)
      (let ((free-workers (- (thread-pool-num-threads pool)
                             (thread-pool-active-tasks pool))))
        (when (plusp (thread-pool-idle-threads pool))
          (bordeaux-threads:condition-notify (thread-pool-condvar pool)))
        (when (and (> (thread-pool-pending-tasks pool) free-workers)
                   (< (thread-pool-num-threads pool)
                      (thread-pool-max-threads pool)))
          (%spawn-thread-pool-worker-locked pool)))
      t)))

(defun wait-for-thread-pool (pool)
  "Blocks until all queued and active tasks in POOL have completed."
  (declare (type thread-pool pool))
  (bordeaux-threads:with-lock-held ((thread-pool-lock pool))
    (loop while (or (thread-pool-tasks-head pool)
                    (plusp (thread-pool-active-tasks pool)))
          do (bordeaux-threads:condition-wait
              (thread-pool-idle-condvar pool)
              (thread-pool-lock pool)))
    (bordeaux-threads:condition-notify (thread-pool-idle-condvar pool)))
  t)

(defun shutdown-thread-pool (pool &key (wait-p t))
  "Initiates shutdown of POOL so no new tasks are accepted and workers exit
once all queued tasks drain. When WAIT-P is true, joins all worker threads."
  (declare (type thread-pool pool))
  (let ((threads nil))
    (bordeaux-threads:with-lock-held ((thread-pool-lock pool))
      (setf (thread-pool-shutdown-p pool) t)
      (dotimes (i (max 1 (thread-pool-num-threads pool)))
        (bordeaux-threads:condition-notify (thread-pool-condvar pool)))
      (when wait-p
        (setf threads (thread-pool-threads pool)
              (thread-pool-threads pool) nil)))
    (when wait-p
      (let ((current (bordeaux-threads:current-thread)))
        (dolist (thread threads)
          (unless (eq thread current)
            (bordeaux-threads:join-thread thread))))))
  t)
