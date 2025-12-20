(ns euler.e124
  (use clojure.test)
  (require [euler.util :refer [the-sievinator primes digits composites factors pow]]
           [clojure.set :as sets]
           [clojure.core.reducers :as r]
           [clojure.math.combinatorics :as c]
           [euler.util.heap :as h]
           [criterium.core :refer [bench quick-bench]]))

(defn distinct-factors [n]
  (distinct (factors n)))

(distinct-factors 8)

(defn rad [n]
  (apply * (distinct-factors n)))

(rad 1)

(def e124  (->> (range 1 100001)
                (map (fn [n] [(rad n) n]))
                sort
                (map-indexed (fn [i [r n]] [n r (inc i)]))))

;; (time (last e124))

(println (take-last 10 e124))

(println (nth e124 9999))
