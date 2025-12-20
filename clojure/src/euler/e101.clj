(ns euler.e101
  (:use [incanter.interpolation :only [interpolate]])
  (:require [euler.util :as util]
            [clojure.set :as sets]))

(defn e101fn [n]
  (-> (- 1  n)
      (+ (Math/pow n 2))
      (- (Math/pow n 3))
      (+ (Math/pow n 4))
      (- (Math/pow n 5))
      (+ (Math/pow n 6))
      (- (Math/pow n 7))
      (+ (Math/pow n 8))
      (- (Math/pow n 9))
      (+ (Math/pow n 10))
      long))

((interpolate [[1 1]] :polynomial) 2)
((interpolate [[1 1] [2 8]] :polynomial) 3) 
((interpolate [[1 1] [2 8] [3 27]] :polynomial) 4) 

(defn e103 [vals]
  (let [points (vec (map-indexed (fn [i v] [(inc i) v]) vals))]
    (reduce (fn [[tot pts] pt]
              (let [all-pts (conj pts pt)
                    x (inc (first pt))
                    f (interpolate all-pts :polynomial)]
                (println pt (long (f x)))
                [(+ tot (f x)) all-pts])) [0 []] points)))

(e103 [1 8 27])

(println (long (first (e103 (map e101fn (range 1 11))))))
