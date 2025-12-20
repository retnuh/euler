(ns euler.e129
  (:use [clojure.test])
  (:require [clojure.pprint :refer [pprint]]
            [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
            [criterium.core :refer [bench quick-bench]]))

(def repunit-of-length
  (memoize (fn [l]
             (if (<= l 1)
               1
               (inc (*' 10 (repunit-of-length (dec l))))))))

(repunit-of-length 50)

(for [l (range 2 21)] (/ (repunit-of-length l) 17))

(defn A [n]
  (let [ks (iterate inc 1)]
    (first (drop-while #(instance? clojure.lang.Ratio (/ (repunit-of-length %) n)) ks))))

(* 101 11)
(* 101010101010101 11)

(defn gcd-with-10-is-1-seq
  ([] (gcd-with-10-is-1-seq (iterate #(+ 10 %) 0)))
  ([base-seq]
   (let [b (first base-seq)]
     (lazy-cat (map (partial + b) [1 3 7 9]) (gcd-with-10-is-1-seq (rest base-seq))))))

(take 10 (gcd-with-10-is-1-seq))


(for [[a b c d] (partition 4 (take 100 (gcd-with-10-is-1-seq)))]
  (printf (str (apply str (repeat 8 "  %3d")) "%n") a (A a) b (A b) c (A c) d (A d)))

(A 243)

(util/factors 131)

;; Interestingly, for all powers of three n, A(n) = n

(defn pow-seq [n]
  (iterate #(*' n %) n))

(take 7 (pow-seq 3))
(mapv A (take 7 (pow-seq 3)))

;; We'll guess the first power of 3 > 1000000
(first (drop-while #(< % 1000000) (pow-seq 3)))

;; Nope!  Okay, so many primes p seem to have A(p) = p-1
;; Try some primes > 1000000?

(def primes-over-1M (drop-while #(< % 1000000) util/primes))

(take 10 primes-over-1M)

(for [[a b c d] (partition 4 (take 40 (drop 3 util/primes)))]
  (printf (str (apply str (repeat 8 "  %3d")) "%n") a (A a) b (A b) c (A c) d (A d)))

(A 17)

(for [i (range 2 12)
      :let [r (repunit-of-length i)
            f (util/factors r)]]
  (println i r f))

