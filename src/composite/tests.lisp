(cl:in-package #:cl-ds.composite)


(let ((composite-container (make 'mutable-composite-container
                                 :root (cl-ds.dicts.srrb:make-mutable-sparse-rrb-vector)
                                 :make-bucket-callbacks (list (make-bucket-callback
                                                               (cl-ds.dicts.srrb:make-mutable-sparse-rrb-vector))))))
  (position-modification (list #'cl-ds:insert! #'cl-ds:insert!)
                         composite-container
                         (list 1 1)
                         5)
  (prove:is (cl-ds:at composite-container 1 1) 5))
