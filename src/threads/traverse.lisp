(cl:in-package #:cl-data-structures.threads)


(defun traverse (object function &key (chunk-size 64) (maximum-queue-size 128))
  (bind ((tasks (make-instance 'task-queue
                               :queue-size maximum-queue-size
                               :callback function
                               :batch-size chunk-size)))
    (cl-ds:traverse object (lambda (x) (task-queue-push tasks x)))
    (task-queue-finalize tasks)
    object))

(defun across (object function &key (chunk-size 64) (maximum-queue-size 128))
  (bind ((tasks (make-instance 'task-queue
                               :queue-size maximum-queue-size
                               :callback function
                               :batch-size chunk-size)))
    (cl-ds:across object (lambda (x) (task-queue-push tasks x)))
    (task-queue-finalize tasks)
    object))
