(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload :documentation-utils-extensions :silent t)
(setf documentation-utils-extensions:*documentation*
      (documentation-utils-extensions:make-documentation-collection))
(quicklisp:quickload :cl-data-structures-tests :silent t)

(unwind-protect
     (handler-bind
         ((lparallel.kernel:no-kernel-error
            (lambda (c)
              (declare (ignore c))
              (invoke-restart 'lparallel.kernel:make-kernel 8))))
       (let ((*error-output* (make-broadcast-stream))
             (prove:*test-result-output* *standard-output*)
             (*standard-output* (make-broadcast-stream))
             (prove:*enable-colors* t)
             (prove:*default-reporter* :dot))
         (prove:run :cl-data-structures-tests)))
  (cl-user::quit))
