(cl:in-package #:cl-data-structures.streaming-algorithms)

(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function union
    (:description "Creates new data-sketch from the provided. Can be used to join sketches built on different data chunks."))

  (function approximated-top-k
    (:description "Attempts to find the top most common elements in a stream. Uses count-min sketch to accomplish that, so it will need only constant memory."
     :arguments ((range "Input for scanning.")
                 (k "Determines the maximum size of the result.")
                 (:hashes "Optional hashes vector. Needs to be supplied in order to ensure that the same hash values are generated between different filters.")
                 (:test "Test to check elements equality.")
                 (:hash-fn "Hashing function, defaults to sxhash.")
                 (:depth "Depth for count min sketch (APPROXIMATED-COUNTS). Higher value increases confidence.")
                 (:width "Width for count min sketch (APPROXIMATED-COUNTS). Higher value decreases error."))
     :returns "Range of pairs. CAR of pair is the object, CDR of pair is estimated count. Range is ordered, with the most frequent element at zero and at most it has K values."))

  (function minhash
    (:description "Calculates minhash for the ELEMENTS with the use of the CORPUS"
     :arguments ((corpus "Object constructed with the GATHER-MINHASH-CORPUS function.")
                 (elements "List of objects that need are being hashed."))
     :exceptional-situations ("Objects in the ELEMENTS that can't be find in the CORPUS are ignored."
                              "Will signal TYPE-ERROR when CORPUS is not of the type MINHASH-CORPUS or ELEMENTS is not of the type CL:LIST.")
     :notes ("Think about minhash as a fingerprint function."
             "Returned minhash vector can be used to quickly calculate approximated Jaccard distance."
             "Can be used for near duplicate detection."
             "Empty set will be hashed to array of MOST-POSITIVE-FIXNUMs.")
     :returns "An one dimensional SIMPLE-ARRAY specialized for FIXNUM of the size equal to the K parameter passed to the GATHER-MINHASH-CORPUS function."))

  (function gather-minhash-corpus
    (:description "Constructs the MINHASH-CORPUS by gathering all objects in the input range. Corpus can be then used  in the MINHASH function to calculate minhash vectors."
     :arguments ((range "Object to aggregate.")
                 (k "What is the length of the minhash vectors? Should be the positive-fixnum.")
                 (key "Function used to extract value for aggregation."))
     :exceptional-situations ("Will signal TYPE-ERROR wthen K is not of the type POSITIVE-FIXNUM.")
     :notes ("Larger K values usually allow for higher precision of the Jaccard distance estimation."
             "Uses HASH-TABLE with EQUAL :TEST underneath to gather elements. Therefore it is required fore elements in the RANGE to be comparable with EQUAL function."
             "To produce stream of elements from individual sets in a range you can use either FLATTEN-LISTS or MULTIPLEX functions.")
     :returns "Instance of the MINHASH-CORPUS class."))

  (function minhash-jaccard/fixnum
    (:description "Calculates distance between two minhash vectors."
     :notes ("Although function is called Jaccard, technically it does not calculate the Jaccard distance because Jaccard distance is bound between 0 and 1."
             "Minhashes by it's very nature are just efficient approximation of the sets, and so the Jaccard distance calculated between minhashes may differ from the precise Jaccard distance between sets.")
     :exceptional-situations ("Will signal INCOMPATIBLE-ARGUMENTS when input vectors are of a different lengths."
                              "Will signal TYPE-ERROR if either A or B is not of the type (SIMPLE-ARRAY FIXNUM (*)).")
     :returns "Number of positions in the vectors that hold different values (as a FIXNUM)."))

  (function minhash-jaccard/double-float
    (:description "Calculates Jaccard distance as a double-float between two minhash vectors."
     :notes ("Minhashes by it's very nature are just efficient approximation of the sets, and so the Jaccard distance calculated between minhashes may differ from the precise Jaccard distance between sets.")
     :exceptional-situations ("Will signal INCOMPATIBLE-ARGUMENTS when input vectors are of a different lengths."
                              "Will signal TYPE-ERROR if either A or B is not of the type (SIMPLE-ARRAY FIXNUM (*)).")
     :returns "Jaccard distance between two minhash vectors (as a DOUBLE-FLOAT)."))

  (function minhash-jaccard/single-float
    (:description "Calculates Jaccard distance as a single-float between two minhash vectors."
     :notes ("Minhashes by it's very nature are just efficient approximation of the sets, and so the Jaccard distance calculated between minhashes may differ from the precise Jaccard distance between sets.")
     :exceptional-situations ("Will signal INCOMPATIBLE-ARGUMENTS when input vectors are of a different lengths."
                              "Will signal TYPE-ERROR if either A or B is not of the type (SIMPLE-ARRAY FIXNUM (*)).")
     :returns "Jaccard distance between two minhash vectorsa (as a SINGLE-FLOAT)."))

  (function approximated-set-cardinality
    (:description "Calculates the estimated set cardinality using the HyperLogLog algorithm. This requires only a constant (and modest) amount of memory."
     :exceptional-situations ("Will signal a TYPE-ERROR if HASH-FN is not funcallable.")
     :arguments ((range "Object to aggregate.")
                 (:hash-fn "Hashing function. SXHASH will do for strings.")
                 (:data-sketch "Instead of the bits and the hash-fn, the user can pass a data-sketch argument.")
                 (:key "A function used to extract value from each element."))
     :notes ("This algorithm gives a solid estimate for large sets, not so good for small sets."
             "Sensitive to a hash function. Large avalanche factor is very helpful. Needs all 64 bits so sxhash won't be fine."
             "Can be used to (for instance) estimate number of keys before creating a hash table. A good estimate of size minimizes rehashing and therefore reduces both memory allocation and time required to fill the hash table.")
     :returns "Instance of the fundamental-data-sketch class. Use CL-DS:VALUE to extract estimate from it."
     :examples "(let ((data (cl-ds:xpr (:i 0)
                              (when (< i 500000)
                                (cl-ds:send-recur (random 99999999999) :i (1+ i))))))
                  (prove:ok (< 490000
                               (cl-ds:value
                                (cl-data-structures.streaming-algorithms:approximated-set-cardinality
                                 data
                                 :hash-fn #'cl-data-structures.utils:hash-integer))
                               510000)))"))

  (function clean-sketch
    (:description "Creates a new, empty data-sketch that would be produced by the function. New data-sketch can be cloned and passed as :data-sketch. This allows to keep compatibility between results of call to the streaming function."))

  (type fundamental-data-sketch
    (:description "The base class of all data sketches. Instances of this class can be passed to streaming algorihms as initial states, cloned and combined into unions."))

  (function approximated-counts
    (:description "Calculates estimated counts using Min-Count sketch algorithm. This requires only a constant amount of memory."
     :exceptional-situations ("Will signal a TYPE-ERROR when either COUNT or SPACE is not integer."
                              "Will signal a TYPE-ERROR when HASH-FN is not funcallable."
                              "Will signal a TYPE-ERROR when HASHES is not either NIL or (SIMPLE-ARRAY FIXNUM (*))."
                              "Will signal a CL-DS:ARGUMENT-VALUE-OUT-OF-BOUNDS if either COUNT or SPACE is not above zero.")
     :arguments ((range "Object to aggregate.")
                 (:hash-fn "Hashing function. SXHASH will do for strings.")
                 (:space "Positive integer. Size of the counters array")
                 (:count "Number of hashing functions used.")
                 (:data-sketch "Instead of the bits and the hash-fn, the user can pass a data-sketch argument."))
     :returns "Instance of the fundamental-data-sketch class. Use CL-DS:AT to extract count estimate for element from it."
     :notes ("Quality of the estimate directly depends on DEPTH and WIDTH."
             "Sensitive to a hash function. Large avalanche factor is very helpful.")))

  (function approximated-counts-distance
    (:description "Estimates bhattacharyya-distance from two count-min-sketches."
     :returns "SINGLE-FLOAT"
     :exceptional-situations ("Will signal a TYPE-ERROR if either A-SKETCH or B-SKETCH are not of the type APPROXIMATED-COUNTS."
                              "Will signal a INCOMPATIBLE-ARGUMENTS if sketches are not compatible.")

             :notes "Result is biased toward the higher values."))

  (function bloom-filter
    (:description "Creates bloom filter out of elements in the range. Bloom filter is a memory efficient data structure allowing to check if an item is absent from the range. If AT returns nil, the item is certainly absent. If AT returns T item either present or not."
     :returns "Instance of the fundamental-data-sketch class. Use cl-ds:at to check if element is present. False positives are possible, false negatives are not possible."
     :exceptional-situations ("Will signal a TYPE-ERROR if either SPACE or COUNT is not integer."
                              "Will signal a CL-DS:ARGUMENT-VALUE-OUT-OF-BOUNDS if either SPACE or COUNT is not above 0."
                              "Will signal a TYPE-ERROR if HASH-FN is not funcallable."
                              "Will signal a TYPE-ERROR if HASHES is not (OR NULL (SIMPLE-ARRAY FIXNUM (* 2)).")
     :arguments ((range "Input for the creation of the bloom filter.")
                 (:space "What is the bloom vector size?")
                 (:hash-fn "Hashing function. SXHASH will do for strings.")
                 (:count "How many bits are used for each item?")
                 (:key "Function used to extract value for hashing.")
                 (:hashes "Optional hashes vector. Needs to be supplied in order to ensure that the same hash values are generated between different filters.")
                 (:data-sketch "Instead of the bits and the hash-fn, the user can pass a data-sketch argument.")))))
