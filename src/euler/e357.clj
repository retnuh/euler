(ns euler.e357
  (use clojure.test)
  (require [euler.util :as util]
           [clojure.set :as sets]
           [clojure.core.reducers :as r]
           [clojure.math.combinatorics :as c]
           [euler.util.heap :as h]
           [criterium.core :refer [bench quick-bench]]))

(defn prime-generating-integer?
  [n]
  (let [dv (util/divisors n)
        c (Math/ceil (/ (count dv) 2))
        fh (drop 2 (take c dv))
        sh (drop 2 (reverse (drop c dv)))]
    #_(println c fh sh)
    (and (= (count fh) (count sh))
         (every? #(util/prime? %) (map + fh sh)))))

(defn candidates [n] (map dec (util/primes-up-to n)))

(defn e357
  [n]
  (time (let [c1 (time (set (candidates n)))
              c2 (time (set (map #(* 2 (- % 2)) (util/primes-up-to (* n 3/5)))))
              c (time (sets/intersection c1 c2))
              f (time (lazy-cat [1 2] (filter prime-generating-integer? c)))]
          (println "n:" n "c1:" (count c1) "c2:" (count c2) "candidates: " (count c))
          (println  "filtered: " (count f))
          #_(println f)
          (time (println "ans:" (apply + f))))))

;; (time (e357 100000000))

(comment
;; "Elapsed time: 6434.920478 msecs"
;; "Elapsed time: 7200.278024 msecs"
;; "Elapsed time: 4161.21878 msecs"
;; "Elapsed time: 0.491186 msecs"
;; n: 100000000 c1: 5761455 c2: 3562115 candidates:  458462
;; filtered:  39628
;; ans: 1739023853139
;; "Elapsed time: 9.618538 msecs"
;; "Elapsed time: 563802.186927 msecs"
;; "Elapsed time: 563802.605912 msecs"
  )
