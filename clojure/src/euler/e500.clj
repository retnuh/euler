(ns euler.e500
  (:use [clojure.test])
  (:require [euler.util :as util]
            [euler.util.heap :as heap]
            [clojure.pprint :refer [pprint]]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [criterium.core :refer [bench quick-bench]]))


(util/count-divisors 120)
(util/count-divisors 840)
(util/divisors 24)
(util/factors 120)
(util/factors 840)
(util/factors 7560)
(util/factors 83160)
(util/factors 1081080)
(util/factors 17297280)
()

(filter util/power-of-2? (range 100))

(defn just-once
  [munge]
  (fn [xf]
    (let [seen (atom #{})]
      (fn
       ([] (xf))
       ([result] (xf result))
        ([result input]
         #_(println "result:" result "input:" input)
         (if-not (@seen (munge input))
           (do
             (println "just seen: " input)
             (swap! seen conj (munge input))
             (xf result input))
           result))))))

(into [] (just-once identity) [1 1 1 2 2 2 1 3 1 2 1 2 4 1 4 2 3 5])
(into [] (distinct) [1 1 1 2 2 2 1 3 1 2 1 2 4 1 4 2 3 5])

#_(println  (into []  (comp  
                     (map (juxt identity util/count-divisors))
                     (filter (comp util/power-of-2? peek))
                     (just-once peek))
                (range 2 (*' 83160 13 16 17))))

(defn squares-seq
  ([] (squares-seq (iterate inc 2)))
  ([s] (cons (*' (first s) (first s)) (lazy-seq (squares-seq (rest s))))))

(take 5 (squares-seq))

(defn primes-and-squares-of-seen-seq
  ([] (cons [2] (lazy-seq (primes-and-squares-of-seen-seq (rest util/primes) (heap/create-heap [4 [2 2]])))))
  ([ps h]
   (let [fp (first ps) [fh vh] (heap/find-min h)]
     (if (< fp fh)
       (cons [fp] (lazy-seq (primes-and-squares-of-seen-seq
                             (rest ps) (heap/insert h [(*' fp fp) [fp fp]]))))
       (cons vh (lazy-seq (primes-and-squares-of-seen-seq
                           ps (heap/insert (heap/delete-min h) [(*' fh fh) (concat vh vh)]))))))))

(pprint (take 20 (primes-and-squares-of-seen-seq)))

(defn factors->divisor-count [factors]
  (let [ff (frequencies factors)]
    (apply *' (map inc (vals ff)))))

(factors->divisor-count (util/factors 17297280))

(defn e500-seq
 ([] (e500-seq [] (primes-and-squares-of-seen-seq)))
 ([acc s]
  (let [f (apply conj acc (first s))]
    (cons f (lazy-seq (e500-seq f (rest s)))))))

(def the-e500-seq (e500-seq))
(pprint (take 20 the-e500-seq))

(mod (* 1001 234 987) 53)
(mod (* (mod 1001 53) (mod 234 53) (mod 987 53)) 53)

(mod (* (mod (* (mod 1001 53) (mod 234 53)) 53) (mod 987 53)) 53)

(factors->divisor-count [2 3 2 2 5 7 3 3 11 13 2 2 2 2 17 19 23 5 5 29 31 37])

(time (println "e500 test: " (reduce (fn [t i] (mod (*' t i) 500500507)) 1 (nth the-e500-seq 3))))
(time (println "e500: " (reduce (fn [t i] (mod (*' t i) 500500507)) 1
                                (nth the-e500-seq 500499))))
;; e500:  35407281
;; "Elapsed time: 1585.760728 msecs"

