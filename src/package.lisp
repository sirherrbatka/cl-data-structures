(in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp #:docstample #:docstample.mechanics #:alexandria
        #:iterate #:alexandria #:serapeum)
  (:nicknames #:cl-ds)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:*traverse-callback*
   #:*documentation*
   #:accumulate
   #:accumulation-function
   #:add
   #:add!
   #:add!-function
   #:add-function
   #:aggregate
   #:apply-layer
   #:argument-out-of-bounds
   #:at
   #:become-functional
   #:become-lazy
   #:become-mutable
   #:become-transactional
   #:clone
   #:consume-back
   #:consume-front
   #:delay
   #:delayed
   #:destructive-counterpart
   #:destructive-function
   #:destructive-function
   #:drop-back
   #:drop-front
   #:empty-clone
   #:empty-clone-of-inner-container
   #:erase
   #:erase!
   #:erase!-function
   #:erase-function
   #:erase-if
   #:erase-if!
   #:erase-if!-function
   #:erase-if-function
   #:expression
   #:force
   #:found
   #:functional
   #:functional-add-function
   #:functional-counterpart
   #:functional-erase-function
   #:functional-erase-if-function
   #:functional-function
   #:functional-insert-function
   #:functional-update-function
   #:functionalp
   #:fundamental-assignable-forward-range
   #:fundamental-assignable-range
   #:fundamental-bidirectional-range
   #:fundamental-container
   #:fundamental-forward-range
   #:fundamental-forward-range
   #:fundamental-modification-operation-status
   #:fundamental-random-access-range
   #:fundamental-range
   #:group-by
   #:group-by-function
   #:grow-bucket
   #:grow-bucket!
   #:grow-function
   #:hash-content
   #:hash-content-hash
   #:hash-content-location
   #:hash-dict-content
   #:hash-dict-content-value
   #:initialization-error
   #:initialization-out-of-bounds
   #:insert
   #:insert!-function
   #:insert-function
   #:invalid-argument
   #:key-value-range
   #:lazy
   #:make-bucket
   #:make-state
   #:mod-bind
   #:morep
   #:mutable
   #:mutablep
   #:near
   #:not-implemented
   #:out-of-bounds
   #:peek-back
   #:peek-front
   #:position-modification
   #:put
   #:put!
   #:put!-function
   #:put-function
   #:read-arguments
   #:read-bounds
   #:read-class
   #:read-value
   #:send
   #:shrink-bucket
   #:shrink-bucket!
   #:shrink-function
   #:size
   #:special-traverse
   #:textual-error
   #:transaction
   #:transactional
   #:transactionalp
   #:traversable
   #:traverse
   #:traverse-through
   #:update
   #:update!
   #:update!-function
   #:update-function
   #:value
   #:whole-range
   #:xpr))

(in-package #:cl-ds)
(docstample:define-accumulated-docs *documentation*)
