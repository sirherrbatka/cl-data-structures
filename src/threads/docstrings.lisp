(cl:in-package #:cl-data-structures.threads)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function thread-buffer
    (:description "Creates a proxy range that will present exactly the same content as the original range. However, when calling an aggregation function internal thread with queue will created. Original thread will pass elements of the range to the queue, and the fresh thread will pass it further. This allows operations performed before and after thread-buffer to run in parallel."
     :exceptional-situations ("Will raise a TYPE-ERROR when MAXIMAL-QUEUE-SIZE is not integer."
                              "Will raise a TYPE-ERROR when CHUNK-SIZE is not integer."
                              "Will raise CL-DS:ARGUMENT-OUT-OF-BOUNDS when CHUNK-SIZE is not at least 1."
                              "Will raise CL-DS:ARGUMENT-OUT-OF-BOUNDS when MAXIMAL-QUEUE-SIZE is not at least 16."
                              "Exceptions raised during aggregation will be transfered to the original thread.")
     :notes ("Thread buffer only changes the aggregation process."
             "For optimal performance results, operations performed before and after THREAD-BUFFER should take approximately the same ammount of time."
             "Unlike PARALLEL-ON-EACH or PARALLEL-MULTIPLEX, THREAD-BUFFER does not use lparallel workers.")
     :arguments-and-values ((range "The input range.")
                            (:chunk-size "Number of elements grouped before being sent to the queue.")
                            (:maximal-queue-size "The maximal size of the queue used internally. Setting this to a low value reduces memory overhead."))
     :returns "Instance of BUFFER-RANGE subclass. Depending on the class of the RANGE it may be a FORWARD-BUFFER-RANGE, BIDIRECTIONAL-BUFFER-RANGE or RANDOM-ACCESS-BUFFER-RANGE."))

  (function parallel-on-each
    (:description "Like on-each but when performing aggregation FUNCTION will be applied in parallel, on lparallel worker threads. To reduce overhead of scheduling lparallel task, use large CHUNK-SIZE."
     :exceptional-situations ("Will raise a TYPE-ERROR when MAXIMAL-QUEUE-SIZE is not integer."
                              "Will raise a TYPE-ERROR when FUNCTION is not a (OR FUNCTION SYMBOL)."
                              "Will raise a TYPE-ERROR when KEY is not a (OR FUNCTION SYMBOL)."
                              "Will raise a TYPE-ERROR when CHUNK-SIZE is not integer."
                              "Will raise CL-DS:ARGUMENT-OUT-OF-BOUNDS when CHUNK-SIZE is not at least 1."
                              "Will raise CL-DS:ARGUMENT-OUT-OF-BOUNDS when MAXIMAL-QUEUE-SIZE is not at least 16."
                              "Exceptions raised during aggregation or when funcalling the callback will be transfered to the original thread.")
     :arguments-and-values ((range "Input range.")
                            (:maximal-queue-size "Maximal size of queue used internally. Setting this to low value reduces memory overhead.")
                            (:chunk-size "Number of elements grouped for individual lparallel task."))
     :returns "Instance of PARALLEL-ON-EACH-PROXY.")))
