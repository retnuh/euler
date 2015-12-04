(ns euler.e206
  (:use [incanter.interpolation :only [interpolate]])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]))

(defn e206int [v]
  [(util/seq->int [1 (get v 0) 2 (get v 1) 3 (get v 2) 4 (get v 3) 5 (get v 4) 6 (get v 5) 7 (get v 6) 8 (get v 7) 9 3 0])
   (util/seq->int [1 (get v 0) 2 (get v 1) 3 (get v 2) 4 (get v 3) 5 (get v 4) 6 (get v 5) 7 (get v 6) 8 (get v 7) 9 7 0])])

(def MIN 1020304050607080900)
(def MAX 1929394959697989990)

(defn e206-matches? [n]
  (let [x (util/digits n)]
    (and 
     (>= n 1020304050607080900)
     (<= n 1929394959697989990)
     (= 9 (get x 16))
     (= 8 (get x 14))
     (= 7 (get x 12))
     (= 6 (get x 10))
     (= 5 (get x 8))
     (= 4 (get x 6))
     (= 3 (get x 4))
     (= 2 (get x 2))
     (= 1 (get x 0)))))

(e206-matches? MIN)
(e206-matches? MAX)


(e206int [1 1 1 1 1 1 1 1 1])

(= 1.1 (Math/floor 1.1))

;; (time (->> (c/selections (range 10) 9)
;;            (map vec)
;;            (map e206int)
;;            (filter util/square-of-int?)
;;            first
;;            (Math/sqrt)))

;; "Elapsed time: 186457.447559 msecs"

(defn e206t []
  (time (let [xf  (comp (map vec)
                        (mapcat e206int)
                        (filter util/square-of-int?)
                        (take 1))]
          (Math/sqrt (first (into [] xf (c/selections (range 10) 8)))))))

;; (e206t)
;; "Elapsed time: 166394.184962 msecs"

;; (/ (- (Math/floor (Math/sqrt (e206int (vec (repeat 9 9))))) (Math/floor (Math/sqrt (e206int (vec (repeat 9 0)))))) 50)

(->> (range 10 990 10)
     (map #(* % %))
     (filter #(let [x (util/digits %)] (and (zero? (peek x)) (= 9 (get x (- (count x) 3))))))
     (map #(Math/sqrt %)))

(->> (range 10 990 10)
     (map #(* % %))
     (filter #(let [x (util/digits %)] (and (zero? (peek x)) (= 9 (get x (- (count x) 3))))))
     (map #(Math/sqrt %)))


(->> (range 0 10000000 100)
     (mapcat #(vector (+ 30 %) (+ 70 %)))
     (map #(* % %))
     (filter #(let [x (util/digits %)] (and (zero? (peek x))
                                            (= 9 (get x (- (count x) 3)))
                                            (= 8 (get x (- (count x) 5)))
                                            (= 7 (get x (- (count x) 7))))))
     (map #(vector (let [y (util/digits (int (Math/sqrt %)))] (butlast (take-last 3 y))) (Math/sqrt %) %))
     (map first)
     (into #{}))

(defn e206-gen
  []
  (let [min-root (int (Math/sqrt MIN))
        max-root (inc (int (Math/sqrt MAX)))
        start (* 100 (int (/ min-root 100)))
        xf (comp (mapcat #(vector (+ 30 %) (+ 70 %)))
                 (filter #(e206-matches? (* % %)))
                 (take 1))]
    (->> (into [] xf (range start max-root 100))
         first)))

;; (time (println (e206-gen)))
