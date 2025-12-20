(ns euler.e108
  (:require [euler.util :as util]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as sets]
            [clojure.math.combinatorics :as c]))

(pprint (util/divisors 1260))

(pprint (util/count-divisors 1260))

;; From
;; http://www.cut-the-knot.org/arithmetic/ShortEquationInReciprocals.shtml
;; we can break down the equation to z = k * m * n, where we use z
;; instead of n in the problem description.

(defn print-count-triples [z]
  (let [f (util/factors z)
        p3 (c/partitions f :max 3)
        g3 (group-by count p3)
        g2-nums (->> (get g3 2)
                     (map (fn [x] (mapv #(apply * %) x))))
        g3-nums (->> (get g3 3)
                     (map (fn [triple] (into [] (map #(apply * %) triple)))))
        doit (fn [[k m n]] (into [(* k m n)] (sort [(* k m (+ m n)) (* k n (+ m n))])))
        g2-left-done (map (fn [[l r]] (doit [l r 1])) g2-nums)
        g2-right-done (map (fn [[l r]] (doit [r l 1])) g2-nums)
        g2-ones-done (map (fn [[l r]] (doit [1 l r])) g2-nums)
        g2-done (concat g2-left-done g2-right-done g2-ones-done)
        g3-left-done (map (fn [[l m r]] (doit [l m r])) g3-nums)
        g3-middle-done (map (fn [[l m r]] (doit [m l r])) g3-nums)
        g3-right-done (map (fn [[l m r]] (doit [r l m ])) g3-nums)
        g3-done (concat g3-left-done g3-middle-done g3-right-done )
        all-none [(doit [(apply * f) 1 1]) (doit [1 (apply * f) 1])]
        all-done (concat all-none g2-left-done g2-right-done g2-ones-done g3-done)]
    (pprint (mapv (fn [[c cl]] [c (count cl)]) g3))
    (println (get g3 2))
    (println g2-left-done)
    (println " ")
    (println (set all-done))
    (println " ")
    (println (count g3-done) (count (set g3-done)))
    (println (count g2-done) (count (set g2-done)))
    (println (count g2-left-done) (count (set g2-left-done)))
    (println (count g2-right-done) (count (set g2-right-done)))
    (println (count g2-ones-done) (count (set g2-ones-done)))
    (println (count g2-ones-done) (count (set g2-ones-done)))
    (println (count all-done) (count (set all-done)))
    (println (count (sets/intersection (set g2-left-done) (set all-none)))
             (count (sets/intersection (set g2-left-done) (set g2-right-done)))
             (count (sets/intersection (set g2-left-done) (set g2-ones-done)))
             (count (sets/intersection (set g2-left-done) (set g3-done))))
    (println (count (sets/intersection (set g2-right-done) (set all-none)))
             (count (sets/intersection (set g2-right-done) (set g2-left-done)))
             (count (sets/intersection (set g2-right-done) (set g2-ones-done)))
             (count (sets/intersection (set g2-right-done) (set g3-done))))
    (println (count (sets/intersection (set g2-done) (set g3-done)))
             (count (sets/intersection (set g2-done) (set all-none)))
             (count (sets/intersection (set all-none) (set g3-done))))
    ))

(print-count-triples 6)

(defn count-triples [[z f]]
  (let [doit (fn [[k m n]] (into [(* k m n)] (sort [(* k m (+ m n)) (* k n (+ m n))])))
        do-partition (fn [[x y z :as v]]
                       (condp = (count v)
                         1 [[x 1 1] [1 x 1]]
                         2 [[x y 1] [y x 1] [1 x y]]
                         3 [[x y z] [y x z] [z x y]]))]
    [z (->> (c/partitions f :max 3)
            (map (fn [x] (mapv #(apply * %) x)))
            (mapcat do-partition)
            (map doit)
            distinct
            count)]
    ))

(take 5 (map count-triples (util/factors-seq)))

(time (println (first (drop-while (fn [[z c]] (< c 1000)) (map count-triples (util/factors-seq))))))

;; [180180 1013]
;; "Elapsed time: 64819.614109 msecs"
;;
;; Bit brute-forcy, but I'll take it.  Won't scale for e110 though.

(util/count-divisors 180180)
(util/factors 180180)

(util/count-divisors 1260)
(util/factors 1260)

(count-triples [(* 1260 5) (concat [7] (util/factors 1260))])

(defn foo [& args]
  (count-triples [(apply * 1260 args) (concat args (util/factors 1260))]))

(foo 2 3 5 7)

;; See e110 for a more efficient solution!
