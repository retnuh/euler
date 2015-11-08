(ns euler.e120
  (use clojure.test)
  (require [euler.util :refer [the-sievinator primes digits composites factors]]
           [clojure.set :as sets]
           [clojure.core.reducers :as r]
           [clojure.math.combinatorics :as c]
           [euler.util.heap :as h]
           [criterium.core :refer [bench quick-bench]]))

(defn pow [x n]
  (loop [i n t 1]
    (if (zero? i)
      t
      (recur (dec i) (*' t x)))))

(defn e120-formula [a n]
#_(println (str (dec a) "^" n) "+" (str (inc a) "^" n) "="
           (+' (pow (dec a) n) (pow (inc a) n)) "%" (* a a))
  (mod (+' (pow (dec a) n) (pow (inc a) n)) (* a a)))

(doseq [n (range 1 20)]
  (println "a:" 3 "n:" n "r:" (e120-formula 3 n)))

(doseq [n (range 1 20) :let [a 4]]
  (println "a:" a "n:" n "r:" (e120-formula a n)))


(doseq [n (range 1 30 2) :let [a 16]]
  (println "a:" a "n:" n "r:" (e120-formula a n)))


(defn odd-max [a]
  (* a (dec a)))

(defn even-max [a]
  (* a (- a 2)))

(defn e120-max [a]
  (if (even? a) (even-max a) (odd-max a)))

(println (reduce + (map e120-max (range 3 1001))))
