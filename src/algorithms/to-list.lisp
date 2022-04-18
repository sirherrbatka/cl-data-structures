(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    to-list to-list-function

  (:range &key key after)
  (:range &key (key #'identity) (after #'identity))

  (%list %tail)

  ((setf %list '()
         %tail %list))

  ((element)
   (if (null %list)
       (setf %list (list element)
             %tail %list)
       (let ((next-cell (list element)))
         (setf (cdr %tail) next-cell
               %tail next-cell))))

  (%list))
