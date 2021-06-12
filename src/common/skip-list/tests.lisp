(cl:in-package #:cl-user)
(defpackage skip-list-tests (:use :cl :cl-data-structures.aux-package))
(cl:in-package #:skip-list-tests)

(prove:plan 13)

(let* ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 3))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       (found (cl-ds.common.skip-list:locate-node
               (vector node3)
               2
               #'<))
       (pointers (cl-ds.common.skip-list::gather-pointers node1 (vector nil))))
  (prove:is (aref found 0) node2)
  (prove:is pointers (vector node1) :test 'equalp))

(bind ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 3))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       ((:values found prev) (cl-ds.common.skip-list:locate-node
                              (vector node3)
                              5
                              #'<)))
  (prove:is (aref found 0) nil)
  (prove:is (aref prev 0) node1))

(let* ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 3))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       (found (cl-ds.common.skip-list:locate-node
               (vector node3)
               0
               #'<)))
  (prove:is (aref found 0) node3))

(bind ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil)
               :content 5))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 2))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2)
               :content 1))
       ((:values found prev)
        (cl-ds.common.skip-list:locate-node
         (vector node3)
         3
         #'<)))
  (prove:is (aref found 0) node1)
  (prove:is (aref prev 0) node2))


(bind ((node1 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector nil nil)
               :content 7))
       (node2 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node1)
               :content 5))
       (node3 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node2 node1)
               :content 3))
       (node4 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node3)
               :content 2))
       (node5 (cl-ds.common.skip-list:make-skip-list-node
               :pointers (vector node4)
               :content 1))
       ((:values found prev)
        (cl-ds.common.skip-list:locate-node
         (vector node5 node3)
         6
         #'<))
       (pointers (cl-ds.common.skip-list::gather-pointers node5 (vector nil nil)))
       (new-node (cl-ds.common.skip-list:make-skip-list-node
                          :pointers (vector nil nil)
                          :content 6)))
  (prove:is pointers (vector node5 node1) :test 'equalp)
  (prove:is (aref found 0) node1)
  (prove:is (aref found 1) node1)
  (prove:is (aref prev 0) node2)
  (prove:is (aref prev 1) node3)
  (cl-ds.common.skip-list:insert-node-between! found prev #'< new-node)
  (prove:is (~> (vector node5 node3)
                (cl-ds.common.skip-list:locate-node 6 #'<)
                (aref 0))
            new-node)
  )


(prove:finalize)
