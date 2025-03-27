(cl:in-package #:cl-ds.alg)


(defun containsp (range test-function)
  (cl-ds:across (lambda (elt)
                  (when (funcall test-function elt)
                    (return-from containsp t)))
                range)
  nil)
