(ns euler.e117
  (require [euler.util :as util]
           [clojure.math.combinatorics :as c]))


(defn e117 [n]
  (reduce (fn [t s] (+ t (c/count-permutations s))) 0 (util/integer-partitions n 4)))

(time (println (e117 50)))


