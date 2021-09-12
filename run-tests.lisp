(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload :cl-data-structures-tests :silent t)

(setf prove:*enable-colors* nil)

(unwind-protect
     (handler-bind
         ((lparallel.kernel:no-kernel-error
            (lambda (c)
              (declare (ignore c))
              (invoke-restart 'lparallel.kernel:make-kernel 8))))
       (prove:run :cl-data-structures-tests))
  (cl-user::quit))
