(ns euler.e146
  (:require [euler.util :as util]
            [clojure.set :as sets]))

(defn is-match? [primes]
  (let [p (first primes)
        s (take 6 primes)]
    (and (= (+ 2 p) (nth s 1))
         (= (+ 6 p) (nth s 2))
         (= (+ 8 p) (nth s 3))
         (= (+ 12 p) (nth s 4))
         (= (+ 26 p) (nth s 5))
         )))

(defn match? [n]
  (let [p (+ 1 (* n n))
        s (into [] (map #(+ p %)) [2 6 8 12 26])]
    (every? util/prime? s)))

(defn next-match [primes]
  (if (is-match? primes)
    (let [p (- (first primes) 1.0)
          sqrt (Math/sqrt p)]
          (println "matching:" p sqrt)
          (if (== sqrt (int sqrt))
            [primes sqrt]
            (recur (next primes)))
          )
    (recur (next primes))))

;; (next-match util/primes)

(defn first-match [] (first (next-match util/primes)))

#_(let [[fp fpn] (next-match util/primes)
      [np npn] (next-match (next fp))
      ]
  (println "hmm" (first fp) fpn (fnext fp)
           (first np) npn
           )
  )

(defn brute-dude [start end step]
  (let [s (range start end step)]
    (into [] (filter match?) s)))

(def dudes (brute-dude 10 1000000 10))
