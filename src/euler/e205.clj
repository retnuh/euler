(ns euler.e205
  (:use [clojure.test])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]))

(defn combine-probabilities
  [a b]
  (let [sa (seq a)
        sb (seq b)]
    (->> (c/cartesian-product sa sb)
         (map (fn [[[va pa] [vb pb]]] {(+ va vb) (*' pa pb)}))
         (apply merge-with +' (sorted-map)))))

(defn dice-distribution
  [side-count times]
  (let [sides (range 1 (inc side-count))
        single-distribution (into {} (map #(vector % (/ 1 side-count))) sides)]
    (reduce combine-probabilities (repeat times single-distribution))))

(deftest test-dice-distribution
  (is (= [[2 1/16] [3 2/16] [4 3/16] [5 4/16] [6 3/16] [7 2/16] [8 1/16]]
         (seq (dice-distribution 4 2))))
  (is (= {3 1/216, 4 3/216, 5 6/216, 6 10/216, 7 15/216, 8 21/216, 9 25/216, 10 27/216,
          18 1/216, 17 3/216, 16 6/216, 15 10/216, 14 15/216, 13 21/216, 12 25/216, 11 27/216}
         (dice-distribution 6 3))))

(defn e205
  "compute the probability of the roles where a is higher than b"
  ([] (e205 9 4 6 6))
  ([times-a sides-a times-b sides-b]
   (let [da (dice-distribution sides-a times-a)
         db (dice-distribution sides-b times-b)]
     (println "a:" (take 5 da))
     (println "b:" (take 5 db))
     (reduce +' (for [[va pa] da
                     [vb pb] (take-while #(< (first %) va) db)]
                 (*' pa pb))))))

;; (time (printf "e205: %.7f%n" (double (e205))))
