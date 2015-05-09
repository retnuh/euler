(ns euler.e407
  (:use [criterium.core])
  (:require [euler.util :as util]
            [clojure.set :as sets]))

(defn s [& args] (map #(* % %) args))

(defn a-squared-mod-n [n]
  (->> n
       (range 1)
       (map #(vector % (mod (* % %) n)))
       (filter (fn [[x y]] (= x y)))
       (map (fn [[x y]] y))
       vec
       ))

(defn M [n]
  (apply max  1 (a-squared-mod-n n)))

(M 6)

(defn sigmaM [n]
  (reduce + 0 (map M (range 2 (inc n)))))

(sigmaM 1000)

(defn possible-final-digits []
  (dorun (for [i (range 10)]
           ())))
(dorun (for [i (range 55)]
         (println (inc i) (a-squared-mod-n (inc i)))))

(dorun (for [i (range 100)]
         (let [mx (apply max -1 (a-squared-mod-n (inc i)))
               [mx2 i2] (s mx (inc i))]
           (println (inc i) mx mx2 i2 (- (inc i) mx) (int (/ mx2 (inc i)))))))

(quick-bench (dorun (map (fn [^long x] * x x) (range 100000000))))

(quick-bench (dorun (map (fn [^long x] (long (Math/sqrt x))) (range 100000000))))

(quick-bench (dorun (map (fn [^long x] (long (Math/pow x 2))) (range 100000000))))
