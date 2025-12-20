(ns euler.e120
  (use clojure.test)
  (require [euler.util :refer [the-sievinator primes digits composites factors pow]]
           [clojure.set :as sets]
           [clojure.core.reducers :as r]
           [clojure.math.combinatorics :as c]
           [euler.util.heap :as h]
           [criterium.core :refer [bench quick-bench]]))

(defn e120-formula [a n]
(println (str (dec a) "^" n) "+" (str (inc a) "^" n) "="
           (+' (pow (dec a) n) (pow (inc a) n)) "%" (* a a))
  (mod (+' (pow (dec a) n) (pow (inc a) n)) (* a a)))

#_(doseq [n (range 1 20)]
  (println "a:" 3 "n:" n "r:" (e120-formula 3 n)))

#_(doseq [n (range 1 20) :let [a 4]]
  (println "a:" a "n:" n "r:" (e120-formula a n)))


#_(doseq [n (range 1 110 2) :let [a 31]]
  (println "a:" a "n:" n "r:" (e120-formula a n)))


(defn odd-max [a]
  (* a (dec a)))

(defn even-max [a]
  (* a (- a 2)))

(defn e120-max [a]
  (if (even? a) (even-max a) (odd-max a)))

(println (reduce + (map e120-max (range 3 1001))))

;; e123 is very similar
(defn nth-prime-rem [n]
  (let [p (nth primes n)]
    (e120-formula p (inc n) #_(mod n (* 2 p)))))

;; 10^9 is 7037
;; I should get 7036 b/c indexing
;; (time (println (first (drop-while #(<= (nth-prime-rem %) 1000000000) (range 2 1000000 2)))))

;; I got 10^10 as 21034 so submit 21035
;; (time (println (first (drop-while #(<= (nth-prime-rem %) 10000000000) (range 7036 1000000 2)))))
;; 
;; "Elapsed time: 163302.486318 msecs"


