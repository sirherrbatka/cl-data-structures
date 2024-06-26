(asdf:defsystem cl-data-structures-tests
  :name "cl-data-structures-tests"
  :version "0.0.0"
  :license "BSD simplified"
  :author "Marek Kochanowicz"
  :maintainer "Marek Kochanowicz"
  :description "Tests for the cl-data-structures system."
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:prove-asdf :prove :cl-data-structures)
  :serial T
  :pathname "src"
  :components ((:module "utils"
                :components ((:test-file "ordered-algorithms-tests")
                             (:test-file "lazy-shuffle-tests")))
               (:module "api"
                :components ((:test-file "expression-tests")))
               (:module "adapters"
                :components ((:test-file "vector-tests")))
               (:module "common"
                :components ((:test-file "sequence-window-tests")
                             (:test-file "qp-trie-tests")
                             (:module "2-3-tree"
                              :components ((:test-file "tests")))
                             (:module "skip-list"
                              :components ((:test-file "tests")))
                             (:module "egnat"
                              :components ((:test-file "tests")))))
               (:module "dicts"
                :components ((:module "hamt"
                              :components ((:test-file "transactions-tests")
                                           (:test-file "range-test")
                                           (:test-file "lazy-tests")))
                             (:module "skip-list"
                              :components ((:test-file "tests")))
                             (:module "srrb"
                              :components ((:test-file "tests")))
                             (:test-file "functional-dictionary-test-suite")
                             (:test-file "mutable-dictionary-test-suite")
                             (:test-file "transactional-dictionary-test-suite")))
               (:module "sequences"
                :components ((:module "rrb"
                              :components ((:test-file "tests")))))
               (:module "queues"
                :components ((:module "2-3-tree"
                              :components ((:test-file "tests")))))
               (:module "sets"
                :components ((:module "skip-list"
                                      :components ((:test-file "tests")))
                             (:module "qp-trie"
                                      :components ((:test-file "tests")))))
               (:module "metric-space"
                :components ((:module "egnat"
                              :components ((:test-file "tests")))))
               (:module "algorithms"
                :components ((:module "meta"
                              :components ((:test-file "meta-tests")))
                             (:test-file "split-into-chunks-test")
                             (:test-file "partition-if-test")
                             (:test-file "without-test")
                             (:test-file "distinct-test")
                             (:test-file "extrema-test")
                             (:test-file "summary-test")
                             (:test-file "chain-test")
                             (:test-file "on-each-test")
                             (:test-file "zip-test")))
               (:module "composite"
                :components ((:test-file "tests")))
               (:module "math"
                :components ((:test-file "moments-tests")
                             (:test-file "mutual-information-tests")
                             (:test-file "simple-linear-regression-tests")))
               (:module "streaming-algorithms"
                :components ((:test-file "approximated-histogram-tests")))))
