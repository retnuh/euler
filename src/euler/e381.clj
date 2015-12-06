(ns euler.e381
  (:use [clojure.test])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
             [criterium.core :refer [bench quick-bench]]))

;; Interesting stuff here:
;; http://stackoverflow.com/questions/9727962/fast-way-to-calculate-n-mod-m-where-m-is-prime#
;;
;; Also, I've notices for (p-1)! it is always (p - 1) (also from
;; Wilson's theorem)
;; for (p-2)! mod p It is always 1
;; for (p-3)! mod p It is always floor p/2
;; Haven't figured out anything obvious for (p-4) or (p-5), but can
;; use stuff from the stack overflow to do it in just a few
;; multiplications and a modulo inverse...

(defn S [p]
  (mod (->> (range 1 6)
            (map #(- p %))
            (map util/factorial)
            (map #(do (println "mod:" % p (mod % p) (- (mod % p) p)) %))
            (map #(mod % p))
            (apply +))
       p))

(defn S2 [p]
  (mod (->> (range 4 6)
            (map #(- p %))
            (map #(util/factorial-mod-p % p))
            #_(map #(do (println "mod2:" % p (mod % p) (- (mod % p) p)) %))
            (apply + (- p 1) 1 (/ (- p 1) 2)))
       p))


(deftest test-S
  (is (= 480N (transduce (map S) + (drop 2 (util/primes-up-to 100))))))

(deftest test-S2
  (is (= 480N (transduce (map S2) + (drop 2 (util/primes-up-to 100))))))

(defn e381
  []
  (transduce (map S2) + (drop 2 (util/primes-up-to 100000000))))

;; (time (println (e381)))
