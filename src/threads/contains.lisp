(cl:in-package #:cl-data-structures.threads)


(defun parallel-contains-p (object test-function &key (chunk-size 64) (maximum-queue-size 128))
  (bind ((found nil)
         (found-lock (bt2:make-lock))
         (tasks (make-instance 'task-queue
                               :queue-size maximum-queue-size
                               :callback (lambda (x &aux (p (funcall test-function x)))
                                           (when p
                                             (bt2:with-lock-held (found-lock)
                                               (setf found t))))
                               :batch-size chunk-size)))
    (block nil
      (cl-ds:across object
                    (lambda (x)
                      (bt2:with-lock-held (found-lock)
                        (when found
                          (return t)))
                      (task-queue-push tasks x))))
    (task-queue-finalize tasks)
    found))
