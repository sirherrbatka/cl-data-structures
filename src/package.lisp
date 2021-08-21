(cl:in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds)
  (:export
   #:across
   #:add
   #:add!
   #:argument-error
   #:argument-value-not-in-allowed-set
   #:argument-value-out-of-bounds
   #:assert-one-dimension
   #:at
   #:between*
   #:become-functional
   #:become-lazy
   #:become-mutable
   #:become-transactional
   #:changed
   #:chunked
   #:chunked-range
   #:chunking-mixin
   #:clone
   #:consume-back
   #:consume-front
   #:define-validation-for-fields
   #:delay
   #:delayed
   #:dimensionality
   #:dimensionality-error
   #:drop-back
   #:drop-front
   #:empty-clone
   #:empty-container
   #:invalid-argument-value
   #:empty-range
   #:erase
   #:check-argument-bounds
   #:erase!
   #:erase*!
   #:erase*
   #:erase-if
   #:erase-if!
   #:expression
   #:field
   #:file-releated-error
   #:finish
   #:force
   #:forward-call
   #:found
   #:functional
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
   #:hash-content
   #:hash-content-hash
   #:hash-content-location
   #:incompatible-arguments
   #:initialization-error
   #:initialization-out-of-bounds
   #:insert
   #:invalid-argument
   #:invalid-value
   #:iota-range
   #:key-value-range
   #:lazy
   #:make-delay
   #:make-from-traversable
   #:make-of-size
   #:mod-bind
   #:modulo-range
   #:mutable
   #:mutablep
   #:near
   #:not-implemented
   #:not-in-allowed-set
   #:operation-not-allowed
   #:out-of-bounds
   #:path
   #:peek-back
   #:peek-front
   #:put
   #:put!
   #:put-back
   #:put-back!
   #:put-front
   #:put-front!
   #:read-arguments
   #:read-arguments
   #:read-bounds
   #:read-class
   #:read-text
   #:read-value
   #:recur
   #:replica
   #:reset!
   #:send-finish
   #:send-recur
   #:size
   #:take-out
   #:take-out!
   #:take-out-back
   #:take-out-back!
   #:take-out-front
   #:take-out-front!
   #:textual-error
   #:too-many-dimensions
   #:transactional
   #:transactionalp
   #:traversable
   #:traverse
   #:traverse-multiple
   #:type-specialization
   #:unexpected-argument
   #:update
   #:update!
   #:update-if
   #:update-if!
   #:validate-field
   #:validate-fields
   #:value
   #:whole-range
   #:xpr))

(defpackage :cl-data-structures.meta
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.meta)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in #:sum)
  (:export
   #:add!-function
   #:add-function
   #:destructive-counterpart
   #:destructive-function
   #:erase!-function
   #:erase-function
   #:erase*!-function
   #:erase*-function
   #:erase-if!-function
   #:erase-if-function
   #:functional-add-function
   #:functional-counterpart
   #:functional-erase-function
   #:functional-erase-if-function
   #:functional-function
   #:functional-insert-function
   #:functional-put-back-function
   #:functional-put-front-function
   #:functional-put-function
   #:functional-take-out-back-function
   #:functional-take-out-front-function
   #:functional-take-out-function
   #:functional-update-function
   #:functional-update-if-function
   #:grow-bucket
   #:grow-bucket!
   #:grow-function
   #:grow-function
   #:insert!-function
   #:insert-function
   #:make-bucket
   #:make-bucket-from-multiple
   #:map-bucket
   #:null-bucket
   #:null-bucket-p
   #:pass-bucket-operation
   #:pass-bucket-query
   #:position-modification
   #:put!-function
   #:put-back!-function
   #:put-front!-function
   #:put-function
   #:shrink-bucket
   #:shrink-bucket!
   #:shrink-function
   #:take-out!-function
   #:take-out-back!-function
   #:take-out-front!-function
   #:take-out-function
   #:update!-function
   #:update-function
   #:update-if!-function
   #:update-if-function))
