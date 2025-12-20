(ns euler.e84
  (use clojure.test)
  (require [euler.util :refer [the-sievinator primes digits composites factors pow]]
           [clojure.core.matrix :as matrix]
           [incanter.core :as ic]
           [criterium.core :refer [bench quick-bench]]))

;; Nice Markov chain article about Monopoly (of all things) here:
;; http://www.codeproject.com/Articles/36025/Markov-Monopoly

(comment
;; GO	A1	CC1	A2	T1	R1	B1	CH1	B2	B3	JAIL
;; H2									 	C1
;; T2	 									U1
;; H1	 									C2
;; CH3	 									C3
;; R4	 									R2
;; G3	 									D1
;; CC3	 									CC2
;; G2	 									D2
;; G1	 									D3
;; G2J	F3	U2	F2	F1	R3	E3	E2	CH2	E1	FP
  )

(declare community-chest)
(declare chance)
(declare go-to-jail)

(def override-fns {:community-chest community-chest :chance chance :go-to-jail go-to-jail})

(def board
  (into {} (map (fn [p] [(first p) p]))
        [["GO" 0] ["A1" 01] ["CC1" 2 :community-chest] ["A2" 3] ["T1" 4]
         ["R1" 5 :rail-road] ["B1" 6] ["CH1" 7 :chance] ["B2" 8] ["B3" 9]
         ["JAIL" 10 :jail] ["C1" 11] ["U1" 12] ["C2" 13] ["C3" 14]
         ["R2" 15 :rail-road] ["D1" 16] ["CC2" 17 :community-chest] ["D2" 18] ["D3" 19]
         ["FP" 20] ["E1" 21] ["CH2" 22 :chance] ["E2" 23] ["E3" 24]
         ["R3" 25 :rail-road] ["F1" 26] ["F2" 27] ["U2" 28] ["F3" 29]
         ["G2J" 30 :go-to-jail] ["G1" 31] ["G2" 32] ["CC3" 33 :community-chest] ["G3" 34]
         ["R4" 35 :rail-road] ["CH3" 36 :chance] ["H1" 37] ["T2" 38] ["H2" 39]]))
(def board-by-num (into {} (map (fn [p] [(second p) p]) (vals board))))

(defn square-number [square-name]
  (second (board square-name)))

(defn next-utility [n]
  (condp = n
    7  (square-number "U1")
    22 (square-number "U2")
    36 (square-number "U1")
    :else (throw (IllegalStateException. "Whu Whu Wut??"))))

(defn next-railroad [n]
  (condp = n
    7  (square-number "R2")
    22 (square-number "R3")
    36 (square-number "R1")
    :else (throw (IllegalStateException. "Whu Whu Huh Wut??"))))

(defn go-to-jail [_]
  [[1 10]])

(comment
  ;; Community Chest (2/16 cards):
  ;; Advance to GO
  ;; Go to JAIL
  )

(defn community-chest [n]
  [[1/16 0] [1/16 10] [14/16 n]])

(comment
  ;; Chance (10/16 cards):
  ;; Advance to GO
  ;; Go to JAIL
  ;; Go to C1
  ;; Go to E3
  ;; Go to H2
  ;; Go to R1
  ;; Go to next R (railway company)
  ;; Go to next R
  ;; Go to next U (utility company)
  ;; Go back 3 squares.
  )

(defn chance [n]
  [[1/16 0]
   [1/16 10]
   [1/16 (square-number "C1")]
   [1/16 (square-number "E3")]
   [1/16 (square-number "H2")]
   [1/16 (square-number "R1")]
   [2/16 (next-railroad n)]
   [1/16 (next-utility n)]
   [1/16 (- n 3)]
   [6/16 n]])

(defn square-probability
  "Given a square, return a seq of [prob square] tuples of where the piece will actually land."
  [n]
  (let [[name nn & [override] :as sq] (board-by-num n)]
    (println "square:" sq override (override-fns override))
    (if-let [override-fn (override-fns override)]
      (mapcat (fn [[p x :as s]]
                (if-not (= x n)
                  (map (fn [[p2 y]] [(* p p2) y]) (square-probability x))
                  [s])) (override-fn n))
      [[1 n]])))

;; (square-probability 36)

;; (community-chest 2)

(defn dominant-eigenvector
  [m]
  (let [{:keys [vectors values]} (ic/decomp-eigenvalue m)
        max-index (apply max-key second (map-indexed vector values))
        dom (matrix/slice vectors 1 (first max-index))
        s (apply + dom)]
    (println "max-index:" max-index)
    (vec (map #(/ % s) dom))))

(defn dice-distribution
  [sides]
  (let [sq (* sides sides)
        r (range 1 (inc sides))
        fr (->> r
                (mapcat (fn [x] (map #(+ % x) r)))
                frequencies
                (map (fn [[k v]] [(/ v sq) k]))
                (sort-by second))]
    fr))

(defn movement-matrix
  [sides]
  (let [m (matrix/new-matrix :clatrix 40 40)
        dist (dice-distribution sides)
        triple-doubles (/ (* sides sides sides))
        normal (- 1 triple-doubles)]
    (println "triple-doubles:" triple-doubles "normal:" normal)
    (doseq [c (range 40)
            [p ro] dist :let [r (mod (+ c ro) 40)]]
      (do
        (matrix/mset! m r c (+ p (matrix/mget m r c)))
        #_(matrix/mset! m r c (+ (* p normal) (matrix/mget m r c)))
        #_(matrix/mset! m 10 c (+ triple-doubles (matrix/mget m r c)))))
    m))

(defn special-matrix
  [sides]
   (let [m (matrix/new-matrix :clatrix 40 40)]
    (doseq [c (range 40)
            [p r] (square-probability c)]
      (matrix/mset! m r c (+ p (matrix/mget m r c))))
    m))


;; (movement-matrix 6)
;; (doseq [s (matrix/slices (movement-matrix 4) 1)] (println s))

;; (square-probability 0)
;; (dice-distribution 4)

(defn e84
  [sides]
  (let [mm (movement-matrix sides)
        sm (special-matrix sides)
        pm (matrix/mmul sm mm)
        de (dominant-eigenvector pm)]
    (->> de
         (map-indexed (fn [i p] [i p]))
         (sort-by second)
         reverse)))

;; (println (e84 4))
;; (println (e84 6))
