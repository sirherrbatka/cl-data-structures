(cl:in-package :cl-user)


(defpackage :cl-data-structures.file-system
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.fs)
  (:shadow #:find)
  (:export
   #:close-inner-stream
   #:find
   #:line-by-line
   #:file-range-mixin
   #:close-stream
   #:open-stream-designator
   #:access-reached-end
   #:access-current-position
   #:ensure-stream
   #:tokenize
   #:read-path
   #:with-file-ranges))
