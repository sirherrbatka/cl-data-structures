#|
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2) Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(asdf:defsystem cl-data-structures
  :name "cl-data-structures"
  :version "1.3.0"
  :license "BSD simplified"
  :author "Marek Kochanowicz"
  :maintainer "Marek Kochanowicz"
  :homepage "https://github.com/sirherrbatka/cl-data-structures"
  :description "Data structures, ranges, ranges algorithms."
  :depends-on ( #:iterate         #:alexandria
                #:serapeum        #:documentation-utils-extensions
                #:more-conditions #:closer-mop
                #:lparallel       #:flexichain
                #:metabang-bind   #:bordeaux-threads
                #:uiop            #:cl-ppcre
                #:trivial-garbage)
  :serial T
  :pathname "src"
  :components ((:file "aux-package")
               (:file "package")
               (:module "utils"
                :components ((:file "package")
                             (:file "macros")
                             (:file "types")
                             (:file "higher-order")
                             (:file "cartesian")
                             (:file "ordered-algorithms")
                             (:file "lists")
                             (:file "trivial")
                             (:file "modification-algorithms")
                             (:file "distances")
                             (:file "lazy-shuffle")
                             (:file "arrays")
                             (:file "trees")
                             (:file "bind")
                             (:file "parallel-tools")
                             (:file "lambda-lists")
                             (:file "embedding")
                             (:file "cloning")
                             (:file "numbers")
                             (:file "bucket-sort")
                             (:file "hashing")
                             (:file "docstrings")
                             ))
               (:module "api"
                :components ((:file "meta")
                             (:file "meta-docstrings")
                             (:file "fundamental-classes")
                             (:file "trait-classes")
                             (:file "generics")
                             (:file "conditions")
                             (:file "expression-wrapper")
                             (:file "delay")
                             (:file "macros")
                             (:file "functions")
                             (:file "field")
                             (:file "auxilary")
                             (:file "docstrings")
                             ))
               (:module "adapters"
                :components ((:file "package")
                             (:file "hash-table")
                             (:file "vector")
                             (:file "list")
                             ))
               (:module "common"
                :components ((:file "package")
                             (:file "modification-operation-status")
                             (:file "eager-modification-operation-status")
                             (:file "lazy-box")
                             (:file "lazy-range")
                             (:file "content-tuple")
                             (:file "ranges")
                             (:file "sequence-window")
                             (:file "docstrings")
                             (:module "abstract"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "2-3-tree"
                              :components ((:file "package")
                                           (:file "common")
                                           ))
                             (:module "hamt"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "rrb"
                              :components ((:file "package")
                                           (:file "common")))
                             (:module "skip-list"
                              :components ((:file "package")
                                           (:file "common")
                                           ))
                             (:module "egnat"
                              :components ((:file "package")
                                           (:file "classes")
                                           (:file "generics")
                                           (:file "common")
                                           (:file "methods")
                                           (:file "docstrings")
                                           ))
                             (:file "qp-trie")
                             (:file "meta")))
               (:module "dicts"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "common")
                             (:file "api")
                             (:file "docstrings")
                             (:module "skip-list"
                              :components ((:file "api")))
                             (:module "hamt"
                              :components ((:file "internal")
                                           (:file "api")
                                           (:file "docstrings")))
                             (:module "srrb"
                              :components ((:file "types")
                                           (:file "internal")
                                           (:file "api")
                                           (:file "docstrings")))))
               (:module "sequences"
                :components ((:file "packages")
                             (:file "common")
                             (:module "rrb"
                              :components ((:file "api")
                                           (:file "docstrings")
                                           ))))
               (:module "queues"
                :components ((:file "packages")
                             (:file "common")
                             (:file "docstrings")
                             (:module "2-3-tree"
                              :components ((:file "api")
                                           (:file "docstrings")
                                           ))))
               (:module "sets"
                :components ((:file "packages")
                             (:file "common")
                             (:file "docstrings")
                             (:module "qp-trie"
                              :components ((:file "api")
                                           (:file "docstrings")))
                             (:module "skip-list"
                              :components ((:file "api")
                                           (:file "docstrings")))))
               (:module "metric-space"
                :components ((:file "packages")
                             (:file "trait-classes")
                             (:file "common")
                             (:file "api")
                             (:file "docstrings")
                             (:module "egnat"
                              :components ((:file "api")
                                           ))))
               (:module "composite"
                :components ((:file "package")
                             (:file "implementation")))
               (:module "algorithms"
                :components ((:file "package")
                             (:module "meta"
                              :components ((:file "macros")
                                           (:file "classes")
                                           (:file "generics")
                                           (:file "methods")
                                           (:file "docstrings")
                                           ))
                             (:file "common")
                             (:file "array-elementwise")
                             (:file "hash-table-elementwise")
                             (:file "on-each")
                             (:file "translation")
                             (:file "count")
                             (:file "to-vector")
                             (:file "to-list")
                             (:file "rate")
                             (:file "to-hash-table")
                             (:file "enumerate")
                             (:file "shuffled-range")
                             (:file "filtering")
                             (:file "common-range-category")
                             (:file "summary")
                             (:file "accumulate")
                             (:file "group-by")
                             (:file "without")
                             (:file "multiplex")
                             (:file "only")
                             (:file "only-different")
                             (:file "cartesian")
                             (:file "restrain-size")
                             (:file "reservoir-sample")
                             (:file "repeat")
                             (:file "latch")
                             (:file "establish-special")
                             (:file "extrema")
                             (:file "extremum")
                             (:file "cumulative-accumulate")
                             (:file "chain")
                             (:file "frequency")
                             (:file "zip")
                             (:file "flatten-lists")
                             (:file "partition-if")
                             (:file "distinct")
                             (:file "first-element")
                             (:file "in-batches")
                             (:file "sliding-window")
                             (:file "with-previous-element")
                             (:file "docstrings")
                             ))
               (:module "file-system"
                :components ((:file "package")
                             (:file "common")
                             (:file "line-by-line")
                             (:file "words")
                             (:file "tokenize")
                             (:file "find")
                             (:file "unix")
                             (:file "docstrings")))
               (:module "threads"
                :components ((:file "package")
                             (:file "common")
                             (:file "parallel-multiplex")
                             (:file "parallel-group-by")
                             (:file "parallel-on-each")
                             (:file "buffer-range")
                             (:file "traverse")
                             (:file "docstrings")))
               (:module "math"
                :components ((:file "package")
                             (:file "absolute-value-norm")
                             (:file "average")
                             (:file "variance")
                             (:file "mutual-information")
                             (:file "simple-linear-regression")
                             (:file "median-absolute-deviation")
                             (:file "hodges-lehmann")
                             (:file "co-occurence-table")
                             (:file "standard-deviation")
                             (:file "moments")
                             ;; (:file "chi-squared")
                             (:file "bootstrap")
                             (:file "moving-average")
                             (:file "hmm")
                             (:file "gini-impurity")
                             (:file "entropy")
                             (:file "fast-map")
                             (:file "sum")
                             (:file "docstrings")
                             ))
               (:module "streaming-algorithms"
                :components ((:file "hyperloglog")
                             (:file "polynomial-hashing")
                             (:file "package")
                             (:file "common")
                             (:file "approximated-histogram")
                             (:file "approximated-set-cardinality")
                             (:file "approximated-counts")
                             (:file "approximated-top-k")
                             (:file "bloom-filter")
                             (:file "minhash")
                             (:file "docstrings")))))
