(cl:in-package #:cl-ds.composite)

(prove:plan 5)

(let ((composite-container (make 'mutable-composite-container
                                 :root (cl-ds.dicts.srrb:make-mutable-sparse-rrb-vector)
                                 :make-bucket-callbacks (list (make-bucket-callback
                                                               (cl-ds.dicts.srrb:make-mutable-sparse-rrb-vector))))))
  (position-modification! (list #'cl-ds:insert! #'cl-ds:insert!)
                          composite-container
                          (list 1 1)
                          5)
  (prove:is (cl-ds:at composite-container 1 1) 5)
  (position-modification! (list #'cl-ds:insert!
                                (list #'cl-ds:update-if!
                                      :condition-fn (constantly nil)))
                          composite-container
                          (list 1 1)
                          7)
  (prove:is (cl-ds:at composite-container 1 1) 5)
  (position-modification! (list #'cl-ds:insert!
                                (list #'cl-ds:update-if!
                                      :condition-fn (constantly t)))
                          composite-container
                          (list 1 1)
                          7)
  (prove:is (cl-ds:at composite-container 1 1) 7))


(let* ((composite-container (make 'functional-composite-container
                                 :root (cl-ds.dicts.srrb:make-functional-sparse-rrb-vector)
                                 :make-bucket-callbacks (list (make-bucket-callback
                                                               (cl-ds.dicts.srrb:make-functional-sparse-rrb-vector)))))
       (fresh-container (position-modification (list #'cl-ds:insert #'cl-ds:insert)
                                               composite-container
                                               (list 1 1)
                                               5)))
  (prove:is (cl-ds:at composite-container 1 1) nil)
  (prove:is (cl-ds:at fresh-container 1 1) 5))

(prove:finalize)
