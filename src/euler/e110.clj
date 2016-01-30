(ns euler.e110
  (:require [euler.util :as util]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as sets]
            [euler.util.heap :as heap]            
            [clojure.math.combinatorics :as c]))

;; From
;; http://www.cut-the-knot.org/arithmetic/ShortEquationInReciprocals.shtml
;; we can break down the equation to z = k * m * n, where we use z
;; instead of n in the problem description.

;; In e108 we more or less brute forced it, by generating factors,
;; partitioning them, etc.  Quite a bit of work.  That method is here
;; for convenience.

(defn doit [[k m n]] (into [(*' k m n)] (sort [(*' k m (+ m n)) (*' k n (+ m n))])))

(defn count-triples [[z f]]
  (let [
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

(take 10 (map count-triples (util/factors-seq)))

;; (time (println (first (drop-while (fn [[z c]] (< c 1000)) (map count-triples (util/factors-seq))))))

;; [180180 1013]
;; "Elapsed time: 64819.614109 msecs"
;;

;; Instead, we're going to just generate composites, and keep their
;; triple-form factors along the way; each step should be relatively
;; cheap to compute.

(defn prime-triples [p]
  [[p 1 1] [1 p 1]])

(defn mult-prime-triples [num p triples h]
  (let [s (->>  triples
                (mapcat (fn [[l m r]] [[(*' p l) m r] [l (*' m p) r] [l m (*' p r)]]))
                distinct)]
    #_(println "inserting triples for " num p (*' p num) (first (heap/find-min h)))
    #_(println "  triples:" triples)
    (heap/insert h [(*' p num) s])))



(defn mult-factors [num primes triples h]
  (if (empty? primes)
    h
    (recur num (next primes) triples (mult-prime-triples num (first primes) triples h))))

(defn numbers-with-triples-seq
  ([] (numbers-with-triples-seq (drop 2 (range)) nil []))
  ([numbers todo-heap primes]
   (let [num (first numbers)]
     (if (or (heap/is-empty? todo-heap) (< num (first (heap/find-min todo-heap))))
       (recur numbers (heap/insert todo-heap [num (prime-triples num)]) (conj primes num))
       (let [[todo triples-seq] (heap/find-min todo-heap)
             next-heap (heap/delete-min-dedup todo-heap)
             grouped-by-doit (group-by doit triples-seq)
             deduped-by-doit (map first (vals grouped-by-doit))]
         (if (= todo num)
           (cons [todo (count grouped-by-doit)]
                 (lazy-seq (numbers-with-triples-seq (next numbers)
                                                     (mult-factors num primes deduped-by-doit next-heap)
                                                 primes)))
           (throw (ex-info "Oops!" {:num num :todo todo :triples grouped-by-doit}))))))))

(take 15 (map butlast (numbers-with-triples-seq)))

(time (println (take 2 (first (drop-while #(< (second %) 1000) (numbers-with-triples-seq))))))

(println (nth (numbers-with-triples-seq) 6))

;; hmm this is not faster than e108, let alone fast enough for e110...
