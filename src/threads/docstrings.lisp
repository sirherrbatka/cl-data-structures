(cl:in-package #:cl-data-structures.threads)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function parallel-multiplex
    (:description "Like CL-DS.ALG:MULTIPLEX but when aggregation is performed, multiplexing function is applied in parallel. Obtained results are then passed with a queue into the original thread."
     :arguments-and-values ((range "Input range.")
                            (:key "Key function.")
                            (:function "Function that should return traversable range. Defaults to #'CL-DS:WHOLE-RANGE")
                            (:maximum-queue-size "The maximal size of the queue used internally. Setting this to a low value reduces memory overhead."))
     :exceptional-situations ("Will raise a TYPE-ERROR when MAXIMAL-QUEUE-SIZE is not integer."
                              "Will raise CL-DS:ARGUMENT-VALUE-OUT-OF-BOUNDS when MAXIMAL-QUEUE-SIZE is not at least 16."
                              "Will raise a TYPE-ERROR when FUNCTION is not funcallable."
                              "Will raise a TYPE-ERROR when FUNCTION is not key.")
     :notes ("Outside of aggregation behaves exactly like MULTIPLEX.")
     :see-also (cl-ds.alg:multiplex)
     :returns "Instance of PARALLEL-FORWARD-MULTIPLEX-PROXY."))

  (function thread-buffer
    (:description "Creates a proxy range that will present exactly the same content as the original range. However, when calling an aggregation function internal thread with queue will created. This allows operations performed before and after thread-buffer to run in parallel."
     :exceptional-situations ("Will raise a TYPE-ERROR when MAXIMAL-QUEUE-SIZE is not integer."
                              "Will raise a TYPE-ERROR when CHUNK-SIZE is not integer."
                              "Will raise CL-DS:ARGUMENT-VALUE-OUT-OF-BOUNDS when CHUNK-SIZE is not at least 1."
                              "Will raise CL-DS:ARGUMENT-VALUE-OUT-OF-BOUNDS when MAXIMAL-QUEUE-SIZE is not at least 16."
                              "Exceptions raised during aggregation will be transfered to the original thread.")
     :notes ("Thread buffer only changes the aggregation process."
             "For optimal performance results, operations performed before and after THREAD-BUFFER should take approximately the same ammount of time."
             "Unlike PARALLEL-ON-EACH or PARALLEL-MULTIPLEX, THREAD-BUFFER does not use lparallel workers."
             "Because of the above, be cautious when using THREAD-BUFFER in conjuction with GROUP-BY.")
     :arguments-and-values ((range "The input range.")
                            (:transform "This function will be called with each constructed group and the result will be placed in the result range in place of the original group. Defaults to IDENTITY.")
                            (:having "This function will called with each constructed, and transformed group. If true is returned; group is kept in the result range, otherwise group is discarded.")
                            (:chunk-size "Number of elements grouped before being sent to the queue.")
                            (:maximum-queue-size "The maximal size of the queue used internally. Setting this to a low value reduces memory overhead."))
     :returns "Instance of BUFFER-RANGE subclass. Depending on the class of the RANGE it may be a FORWARD-BUFFER-RANGE, BIDIRECTIONAL-BUFFER-RANGE or RANDOM-ACCESS-BUFFER-RANGE."))

  (function parallel-on-each
    (:description "Like on-each but when performing aggregation FUNCTION will be applied in parallel, on lparallel worker threads. To reduce overhead of scheduling lparallel task, use large CHUNK-SIZE."
     :exceptional-situations ("Will raise a TYPE-ERROR when MAXIMAL-QUEUE-SIZE is not integer."
                              "Will raise a TYPE-ERROR when FUNCTION is not a (OR FUNCTION SYMBOL)."
                              "Will raise a TYPE-ERROR when KEY is not a (OR FUNCTION SYMBOL)."
                              "Will raise a TYPE-ERROR when CHUNK-SIZE is not integer."
                              "Will raise CL-DS:ARGUMENT-VALUE-OUT-OF-BOUNDS when CHUNK-SIZE is not at least 1."
                              "Will raise CL-DS:ARGUMENT-VALUE-OUT-OF-BOUNDS when MAXIMAL-QUEUE-SIZE is not at least 16."
                              "Exceptions raised during aggregation or when funcalling the callback will be transfered to the original thread.")
     :notes "This function is unsuited for aggregations that depend on the order of the elements."
     :arguments-and-values ((range "Input range.")
                            (:maximum-queue-size "Maximal size of queue used internally. Setting this to low value reduces memory overhead.")
                            (:chunk-size "Number of elements grouped for individual lparallel task."))
     :returns "Instance of PARALLEL-ON-EACH-PROXY.")))
