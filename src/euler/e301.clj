(ns euler.e301
  (:use [clojure.test])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]
            [criterium.core :refer [bench quick-bench]]))

;; Nim game 3 stones

(defn X-1 [stack]
  "return 0 if player with next turn is boned, 1 if current player can win next turn, -1 else"
  (match [(vec (sort stack))]
         [[0 0 0]] 0
         [[0 a :guard pos? b :guard pos?] :guard (fn [[_ x y :as all]] (= x y))] 0
         [[0 a :guard pos? b :guard pos?] :guard (fn [[_ x y :as all]]  (not= x y))]  1
         [[0 0 b :guard pos?]] 1
         [[1 2 3]] 0
         [[1 2 x :guard #(> % 3)]] 1
         [[1 3 x :guard #(> % 3)]] 1
         [[2 3 x :guard #(> % 3)]] 1
         [[x y _]  :guard (fn [[x y _]] (= x y))] 1
         [[_ x y] :guard (fn [[_ x y ]] (= x y))] 1
         :else -1
         ))


(X-1 [0 0 0])
(X-1 [0 1 2])
(X-1 [0 0 2])
(X-1 [1 2 3])


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
    [2 3 4] 1))

(defn moves-for-stack [p stack]
  (let [vs (range (stack p))]
    (eduction (map #(assoc-in stack [p] %)) vs)))

(defn moves [stacks]
  (let [xf (comp (mapcat #(moves-for-stack % stacks))
;;                 (map (comp vec sort))
;;                 (distinct)
                 (map (juxt identity X-1)))
        grouped (group-by peek (into [] xf [0 1 2]))]
    grouped
    ))

(println  (moves [1 2 4]))
(println (moves [2 4 6]))

(declare best-move-m)

(defn best-look-ahead [moves]
  #_(println "look-ahead:" moves)
  (when-let [wins (doall (some (fn [[m [_ d _] :as x]] (when (= d :lost) m)) moves))]
    #_(println "wins:" wins)
    wins))



(defn best-move [current-player other-player stacks]
  (let [grouped-moves (moves stacks)]
    (cond
      (contains? grouped-moves 0) [current-player :win (grouped-moves 0)]
      (contains? grouped-moves -1) (let [moves (map #(vector (first %) (best-move-m other-player current-player (first %))) (grouped-moves -1))]
                                     (if-let [move (best-look-ahead moves)]
                                       [current-player :win move]
                                       [current-player :lost #_[moves (grouped-moves 1)]]))
      :else [current-player :lost  #_(grouped-moves 1)])))

(def best-move-m (memoize best-move))

(println  (best-move :p1 :p2 [1 2 4]))
(pprint (best-move :p1 :p2 [2 4 6]))
(pprint (best-move :p1 :p2 [2 4 5]))

(pprint (best-move :p1 :p2 [3 6 9]))

(defn foo [n]
  (let [x [n (* 2 n) (* 3 n)]]
    [x  (apply bit-xor x) (bit-xor n (* 2 n)) (best-move :p1 :p2 x)]))

(doseq [i (range 2 50)] (pprint (foo i)))

;; Read Wikipedia article on nim, it basically boils down to xor-ing
;; the count of the stacks together
(defn ez [n]
  (zero? (bit-xor n (* 2 n) (* 3 n))))

(defn oz [n]
  (let [a (bit-shift-left n 1)]
    (if (= (+ a n) (bit-xor a n))
      1
      0)))

(println (mapv oz (range 20)))
(println (mapv ez (range 20)))

;; (quick-bench (doseq [i (range 100000)] (oz i)))
;; (quick-bench (doseq [i (range 100000)] (ez i)))

;; (time (println (transduce (map oz) + 0 (range 1 (inc (util/pow 2 30))))))
;; 2178309
;; "Elapsed time: 39618.948371 msecs"

