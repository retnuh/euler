(ns euler.e114
  (require [euler.util :as util]
           [clojure.math.combinatorics :as c]))

(defn valid? [s]
  (let [x (first s)
        y (second s)]
    (and
     (or (empty? s) (valid? (next s)))
     (or (nil? x)
         (= x 1)
         (and (>= x 3) (or (nil? y) (= y 1)))))
    ))

(defn e114 [n]
  (count (filter valid? (mapcat c/permutations (filter #(every? (fn [x] (not= 2 x)) %) (util/integer-partitions n))))))

(println (mapcat c/permutations (filter #(every? (fn [x] (not= 2 x)) %) (util/integer-partitions 7))))

(time (println (e114 7)))

;; (time (println (e114 50)))
;; too slow!  need some way to compute number of valid permutations
;; rather than actually expanding them.  Probably necessary for e115 anyway.



