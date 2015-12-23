(ns euler.e301
  (:use [clojure.test])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [criterium.core :refer [bench quick-bench]]))

;; Nim game 3 stones

(defn X-1 [stack]
  "return 0 if player with next turn is boned, 1 if current player can win next turn, -1 else"
  (match [(vec (sort stack))]
         [[0 a :guard pos? b :guard pos?] :guard (fn [[_ x y :as all]] (= x y))] 0
         [[0 a :guard pos? b :guard pos?] :guard (fn [[_ x y :as all]]  (not= x y))]  1
         [[0 0 b :guard pos?]] 1
         [[1 2 3]] 0
         [[1 2 x :guard #(> % 3)]] 1
         [[x y _]  :guard (fn [[x y _]] (= x y))] 1
         [[_ x y] :guard (fn [[_ x y ]] (= x y))] 1
         :else -1
         ))


(X-1 [0 0 0])
(X-1 [0 1 2])
(X-1 [0 0 2])
(X-1 [1 2 3])
(X-1 )

(deftest X-1-test
  (are [x y] (is (= (X-1 x) y))
    [0 1 1] 0
    [0 1 2] 1
    [0 0 2] 1
    [1 2 3] 0
    [1 2 5] 1
    [2 2 3] 1
    [1 1 3] 1
    [1 3 3] 1
    [3 3 3] 1
    [2 3 4] -1))

(defn moves-for-stack [p stack]
  (let [vs (range (stack p))]
    (eduction (map #(assoc-in stack [p] %)) vs)))

(defn moves [stacks]
  (let [xf (comp (mapcat #(moves-for-stack % stacks))
                 (map (juxt identity X-1)))
        grouped (group-by peek (into [] xf [0 1 2]))]
    grouped
    ))

(println  (moves [1 2 4]))
(println (moves [2 4 6]))

(defn best-move [current-player other-player stacks]
  (let [grouped-moves (moves stacks)]
    (cond
      (contains? grouped-moves 0) [current-player :win (grouped-moves 0)]
      (contains? grouped-moves -1) [current-player :other (grouped-moves -1)]
      :else [current-player :lost (grouped-moves 1)])))

(println  (best-move :p1 :p2 [1 2 4]))
(println (best-move :p1 :p2 [2 4 6]))
