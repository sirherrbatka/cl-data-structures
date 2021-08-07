(cl:in-package #:cl-user)


(defpackage :cl-data-structures.common.skip-list
  (:use #:common-lisp
        #:cl-data-structures.common.abstract
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.skip-list)
  (:export
   #:access-test-function
   #:assoc-skip-list-node-value
   #:delete-node-between!
   #:fundamental-skip-list
   #:fundamental-skip-list-range
   #:insert-node-between!
   #:insert-or
   #:level
   #:locate-node
   #:make-range
   #:make-skip-list-node-of-level
   #:make-skip-list-node-of-random-level
   #:maximum-level
   #:new-node-update-pointers!
   #:pointers
   #:random-level
   #:read-ordering-function
   #:read-pointers
   #:size
   #:skip-list-locate-node
   #:skip-list-node
   #:skip-list-node-at
   #:skip-list-node-clone
   #:skip-list-node-compare
   #:skip-list-node-content
   #:skip-list-node-level
   #:skip-list-node-pointers
   #:skip-list-node-update-pointers!
   #:update-head-pointers!))
