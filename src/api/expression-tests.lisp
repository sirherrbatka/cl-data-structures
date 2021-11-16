(cl:in-package #:cl-user)
(defpackage expression-tests
  (:use :cl :prove :cl-data-structures.aux-package))
(cl:in-package #:expression-tests)

(plan 5)
(let ((data nil)
      (expression (cl-ds:xpr (:iteration 1)
                    (when (< iteration 5)
                      (cl-ds:send-recur iteration :iteration (1+ iteration))))))
  (is (cl-ds:peek-front expression) 1)
  (cl-ds:across expression
                (lambda (x) (push x data)))
  (is data '(4 3 2 1) :test #'equal)
  (setf data nil)
  (iterate
    (for (values value not-finished) = (cl-ds:consume-front expression))
    (while not-finished)
    (push value data))
  (is data '(4 3 2 1) :test #'equal))

(let* ((data '(1 2 (3 4) (5 (6 7))))
       (expression (cl-ds:xpr (:stack (list data))
                     (unless (endp stack)
                       (let ((front (first stack)))
                         (cond ((atom front)
                                (cl-ds:send-recur front :stack (rest stack)))
                               (t (cl-ds:recur :stack (append front (rest stack))))))))))
  (let ((result nil))
    (cl-ds:traverse expression (lambda (x) (push x result)))
    (is (sort result #'<) '(1 2 3 4 5 6 7) :test #'equal)))

(let* ((data '(1 2 (3 4) (5 (6 7))))
       (expression (cl-ds:xpr (:stack (list data))
                     (unless (endp stack)
                       (destructuring-bind (front . stack) stack
                         (cond ((atom front)
                                (cl-ds:send-recur front :stack stack))
                               (t (cl-ds:recur
                                   :stack (iterate
                                            (for elt in front)
                                            (push elt stack)
                                            (finally (return stack)))))))))))
  (let ((result nil))
    (cl-ds:traverse expression (lambda (x) (push x result)))
    (is (sort result #'<) '(1 2 3 4 5 6 7) :test #'equal)))

(finalize)
