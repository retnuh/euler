(ns euler.e493
  (:use [clojure.test])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]))

(defn make-urn [s]
  (let [counts (vec s)]
    {:total (apply + counts) :counts counts}))

(def the-urn (make-urn (repeat 7 10)))
(def start-state [0 0 0 0 0 0 0])

(defn drawn-ball-probability
  "Draw a ball.  Since we don't really care about specific colors, when we draw a new ball, we'll always consider it the next highest slot.  We combine all the probabilities of drawing a ball that hasn't been seen."
  [urn drawn-counts]
  (let [first-unseen (util/count-while pos? drawn-counts)
        existing (into []
                       (map
                        (fn [i] (let [remaining (- (get-in urn [:counts i])
                                                  (drawn-counts i))
                                     total-remaining (- (:total urn) (apply + drawn-counts))]
                                 (/  remaining total-remaining))))
                       (range first-unseen))
        all (apply conj existing (- 1 (apply + existing))
                   (repeat (- (count drawn-counts) (inc first-unseen)) 0))]
    #_(when-not (= 1 (apply +' all))
      (println "Hmm wtf: " drawn-counts all first-unseen (apply +' all)))
    all))

(deftest test-drawn-ball-probabilities
  (is (= [1 0 0 0 0 0 0] (drawn-ball-probability the-urn [0 0 0 0 0 0 0])))
  (is (= [9/69 60/69 0 0 0 0 0] (drawn-ball-probability the-urn [1 0 0 0 0 0 0])))
  (is (= [9/68 9/68 50/68 0 0 0 0] (drawn-ball-probability the-urn [1 1 0 0 0 0 0])))
  (is (= [9/67 8/67 50/67 0 0 0 0] (drawn-ball-probability the-urn [1 2 0 0 0 0 0])))
  )

(defn my-inc [x]
  (if-not (number? x)
    (do  (println "wtf" x) 1)
    (inc x)))

(defn draw-ball-given-state
  "Given the current state (ball counts), return a seq of the next states reachable from this state"
  [urn state state-prob]
  (let [probs (drawn-ball-probability urn state)
        all (->> probs
                 (map-indexed vector)
                 (filter (fn [[i p]] (pos? p)))
                 (map (fn [[i p]] {(update-in state [i] inc) (*' p state-prob)}))
                 (into []))]
    #_(when-not (= 1 (apply +' probs))
      (println "Hmm dbgs probs wtf: " state state-prob probs (apply +' probs)))
    #_(when-not (= state-prob (apply +' (map second (apply merge-with +' all))))
      (println "Hmm dbgs all wtf: " state state-prob probs (count all) (apply +' (map second (apply merge-with +' all)))))
    all))

(deftest test-draw-ball-given-state
  (is (= [{[1 0 0 0 0 0 0] 1}]
         (draw-ball-given-state the-urn [0 0 0 0 0 0 0] 1)))
  (is (= [{[2 0 0 0 0 0 0] 9/69} {[1 1 0 0 0 0 0] 60/69}]
         (draw-ball-given-state the-urn [1 0 0 0 0 0 0] 1)))
  (is (= [{[2 0 0 0 0 0 0] 3/69} {[1 1 0 0 0 0 0] 20/69}]
         (draw-ball-given-state the-urn [1 0 0 0 0 0 0] 1/3)))
  (is (= [{[10 1 0 0 0 0 0] 1}]
         (draw-ball-given-state the-urn [10 0 0 0 0 0 0] 1)))
  )

(defn compute-next-states
  [urn states-and-probabilities]
  (->> states-and-probabilities
       (mapcat (fn [[state prob]] (draw-ball-given-state urn state prob)))
       (apply merge-with +')))

(deftest test-compute-next-states
  (is (= {[1 0 0 0 0 0 0] 1}
         (compute-next-states the-urn {start-state 1})))
  (is (= {[2 0 0 0 0 0 0] 9/69, [1 1 0 0 0 0 0] 60/69}
         (compute-next-states the-urn {[1 0 0 0 0 0 0] 1})))
  (is (= {[3 0 0 0 0 0 0] (*  9/69 8/68), [2 1 0 0 0 0 0] 90/391,
          [1 2 0 0 0 0 0] 45/391, [1 1 1 0 0 0 0] 250/391}
         (compute-next-states the-urn {[2 0 0 0 0 0 0] 9/69, [1 1 0 0 0 0 0] 60/69}))))

(defn run-states
  [urn n]
  (loop [states-and-probabilities {(vec (repeat (count (:counts urn)) 0)) 1}
         i n]
    (println i (apply +' (map second states-and-probabilities)))
    (if-not (pos? i)
      states-and-probabilities
      (recur (compute-next-states urn states-and-probabilities) (dec i)))))

(defn e493
  []
  (let [probs (run-states the-urn 20)
        grouped (group-by (fn [[s p]] (util/count-while pos? s)) probs)
        simplified (into {} (map (fn [[c l]] [c (apply +' (map second l))]) grouped))
        expected (reduce (fn [t [c p]] (+ t (*' c p))) 0 simplified)]
    (printf "Expected: %.10f" (double expected))
    (println)
    (println "Sum of simplified:" (apply +' (map second simplified)))
    [grouped simplified expected]
    nil))

;; (time (e493))
