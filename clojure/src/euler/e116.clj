(ns euler.e116
  (require [euler.util :as util]
           [clojure.math.combinatorics :as c]))


;; This is very similar to normal count int partitions, but
;; we need to account for different orders, and we don't count the all
;; 1's cases

(c/count-permutations [1 1 1 1])

(defn valid? [col]
  (let [f (frequencies col)
        cf (count f)]
    (cond
      (> cf 2) false
      (< cf 2) true
      :else (contains? f 1))))

(defn e116 [n]
  (dec
   (transduce (comp (filter valid?) (map c/count-permutations)) + (util/integer-partitions n 4))))

(println  (filter valid? (util/integer-partitions 5 4)))

(e116 5)

;; (time (println (e116 50)))
;; 20492570929N
;; "Elapsed time: 9.143311 msecs"
;; (but the integer-partitions up to 50 had already been computed)

