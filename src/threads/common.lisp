(cl:in-package #:cl-data-structures.threads)


(defclass task-queue ()
  ((%cv
    :initarg :cv
    :accessor cv)
   (%lock
    :initarg :lock
    :accessor lock)
   (%tasks
    :initarg :tasks
    :accessor tasks)
   (%buffer
    :initarg :buffer
    :accessor buffer)
   (%queue-size
    :initarg :queue-size
    :accessor queue-size)
   (%callback
    :initarg :callback
    :accessor callback)
   (%batch-size
    :initarg :batch-size
    :accessor batch-size))
  (:default-initargs
   :cv (bt2:make-condition-variable)
   :lock (bt2:make-lock)
   :tasks (make-hash-table)
   :buffer (vect)))

(defun task-queue-push (task-queue task)
  (with-accessors ((buffer buffer)
                   (lock lock)
                   (cv cv)
                   (queue-size queue-size)
                   (callback callback)
                   (tasks tasks)
                   (batch-size batch-size))
      task-queue
    (vector-push-extend task (buffer task-queue))
    (when (= (fill-pointer buffer) batch-size)
      (bt2:with-lock-held (lock)
        (setf (gethash buffer tasks)
              (cl-ds.utils:with-rebind (buffer)
                (lparallel:future
                  (cl-ds.utils:rebind
                   (map nil callback buffer)
                   (bt2:with-lock-held (lock)
                     (remhash buffer tasks)
                     (bt2:condition-notify cv)))))))
      (setf buffer (make-array batch-size :adjustable t :fill-pointer 0))
      (bt2:with-lock-held (lock)
        (iterate
          (while (>= (hash-table-count tasks) queue-size))
          (bt2:condition-wait cv lock))))))

(defun task-queue-finalize (task-queue)
  (with-accessors ((buffer buffer)
                   (tasks tasks)
                   (function callback))
      task-queue
    (let ((final (lparallel:future (map nil function buffer))))
      (maphash-values (lambda (x) (lparallel:force x))
                      tasks)
      (lparallel:force final))))
