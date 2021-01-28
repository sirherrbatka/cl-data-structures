(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun make-bubble-fn (tree)
  (lambda (leaf)
    (make 'bubble
          :content (leaf-content tree leaf)
          :clusteroid (clusteroid tree leaf))))


(defun build-tree (data distance-function sampling-rate
                   sample-size subtree-maximum-arity
                   leaf-maximum-size leaf-maximum-radius parallel
                   parallel-sample-size parallel-samples-count parallel-reference-size)
  (ensure-functionf distance-function)
  (check-type data vector)
  (check-type parallel-samples-count positive-fixnum)
  (check-type parallel-sample-size positive-fixnum)
  (check-type sampling-rate real)
  (check-type sample-size positive-fixnum)
  (check-type subtree-maximum-arity positive-fixnum)
  (check-type leaf-maximum-size positive-fixnum)
  (check-type leaf-maximum-radius number)
  (check-type parallel boolean)
  (let ((tree (make 'cf-tree :distance-function distance-function
                             :sampling-rate sampling-rate
                             :subtree-maximum-arity subtree-maximum-arity
                             :leaf-maximum-size leaf-maximum-size
                             :leaf-maximum-radius leaf-maximum-radius
                             :subtree-sample-size sample-size
                             :parallel-sample-size parallel-sample-size
                             :parallel-reference-size parallel-reference-size
                             :parallel-samples-count parallel-samples-count)))
    (values tree
            (if parallel
                (parallel-bubble-grouping tree data)
                (single-thread-bubble-grouping tree data)))))


(defun bubble-grouping (data
                        distance-function
                        sampling-rate
                        sample-size
                        subtree-maximum-arity
                        leaf-maximum-size
                        leaf-maximum-radius
                        &key
                          (parallel nil)
                          (parallel-sample-size 100)
                          (parallel-samples-count 500)
                          (parallel-reference-size 10000))
  (bind (((:values tree root)
          (build-tree data distance-function sampling-rate sample-size
                      subtree-maximum-arity leaf-maximum-size leaf-maximum-radius
                      parallel parallel-sample-size parallel-samples-count
                      parallel-reference-size)))
    (gather-leafs tree root :key (make-bubble-fn tree))))


(defun bubble-clusteroid (bubble)
  (check-type bubble bubble)
  (read-clusteroid bubble))


(defun bubble-content (bubble)
  (check-type bubble bubble)
  (read-content bubble))
