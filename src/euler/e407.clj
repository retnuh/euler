(ns euler.e407
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

(defn possible-final-digits []
  (dorun (for [i (range 10)]
           ())))
(dorun (for [i (range 55)]
         (println (inc i) (a-squared-mod-n (inc i)))))

(dorun (for [i (range 100)]
         (let [mx (apply max -1 (a-squared-mod-n (inc i)))
               [mx2 i2] (s mx (inc i))]
           (println (inc i) mx mx2 i2 (- (inc i) mx) (int (/ mx2 (inc i)))))))
