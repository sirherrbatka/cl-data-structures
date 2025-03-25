(cl:defpackage #:cl-data-structures.threads
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.threads)
  (:export
   #:thread-buffer
   #:parallel-multiplex
   #:parallel-on-each
   #:parallel-group-by
   #:parallel-traverse
   #:parallel-across
   #:parallel-contains-p
   ))
