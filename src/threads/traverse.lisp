(cl:in-package #:cl-data-structures.threads)


(defun traverse (object function &key (batch-size 64) (queue-size 128))
  (bind ((tasks (make-instance 'task-queue
                               :queue-size queue-size
                               :callback function
                               :batch-size batch-size)))
    (cl-ds:traverse object (lambda (x) (task-queue-push tasks x)))
    (task-queue-finalize tasks)
    object))

(defun across (object function &key (batch-size 64) (queue-size 128))
  (bind ((tasks (make-instance 'task-queue
                               :queue-size queue-size
                               :callback function
                               :batch-size batch-size)))
    (cl-ds:across object (lambda (x) (task-queue-push tasks x)))
    (task-queue-finalize tasks)
    object))
