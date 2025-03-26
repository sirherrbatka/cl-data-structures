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
    :accessor batch-size)
   (%stored-error
    :initarg :stored-error
    :accessor stored-error)
   (%stored-error-lock
    :initarg :stored-error-lock
    :accessor stored-error-lock))
  (:default-initargs
   :cv (bt2:make-condition-variable)
   :lock (bt2:make-lock)
   :tasks (make-hash-table)
   :stored-error-lock (bt2:make-lock)
   :stored-error nil
   :buffer (vect)))

(defun force-everything (task-queue)
  (with-accessors ((lock lock)
                   (tasks tasks))
      task-queue
    (iterate
      (for task in (bt2:with-lock-held (lock)
                     (hash-table-values tasks)))
      (lparallel:force task))))

(defun task-queue-push (task-queue task)
  (with-accessors ((buffer buffer)
                   (lock lock)
                   (cv cv)
                   (queue-size queue-size)
                   (callback callback)
                   (tasks tasks)
                   (stored-error stored-error)
                   (stored-error-lock stored-error-lock)
                   (batch-size batch-size))
      task-queue
    (vector-push-extend task (buffer task-queue))
    (when (= (fill-pointer buffer) batch-size)
      (let ((task (cl-ds.utils:with-rebind (buffer)
                    (lparallel:future
                      (handler-case
                          (when (bt2:with-lock-held (stored-error-lock)
                                  (null stored-error))
                            (cl-ds.utils:rebind
                             (map nil callback buffer)
                             (bt2:with-lock-held (lock)
                               (remhash buffer tasks)
                               (bt2:condition-notify cv))))
                        (error (e)
                          (bt2:with-lock-held (stored-error-lock)
                            (bt2:with-lock-held (lock)
                              (setf stored-error e)
                              (bt2:condition-notify cv)))))))))
        (bt2:with-lock-held (lock)
          (setf (gethash buffer tasks) task)))
      (setf buffer (make-array batch-size :adjustable t :fill-pointer 0))
      (bt2:with-lock-held (lock)
        (iterate
          (bt2:with-lock-held (stored-error-lock)
            (unless (null stored-error)
              (error stored-error)))
          (while (>= (hash-table-count tasks) queue-size))
          (bt2:condition-wait cv lock))))))

(defun task-queue-finalize (task-queue)
  (with-accessors ((buffer buffer)
                   (tasks tasks)
                   (lock lock)
                   (stored-error stored-error)
                   (stored-error-lock stored-error-lock)
                   (function callback))
      task-queue
    (let ((final (lparallel:future (map nil function buffer))))
      (force-everything task-queue)
      (lparallel:force final)
      (bt2:with-lock-held (stored-error-lock)
        (unless (null stored-error)
          (error stored-error))))))
