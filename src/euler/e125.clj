(ns euler.e125
  (use clojure.test)
  (require [euler.util.palindrome :as p]
           [clojure.math.combinatorics :as c]
           [clojure.pprint :refer [pprint]]
           [criterium.core :refer [bench quick-bench]]))

(defn sum-of-consecutive-squares-for-digit
  ([i squares n] (sum-of-consecutive-squares-for-digit i squares n i))
  ([i squares n tot]
   (when-let [x (first squares)]
     (let [ntot (+ x tot)]
       (when (< ntot n)
         ;; (println i squares n tot ntot)
         (cons ntot (lazy-seq (sum-of-consecutive-squares-for-digit i (rest squares) n ntot))))))))

(defn sum-of-consecutive-squares
  ([n]
   (let [limit (inc (Math/ceil (Math/sqrt n)))
         squares (mapv #(* % %) (range 1 limit))]
     (sum-of-consecutive-squares squares n)))
  ([squares n]
   (when-let [i (first squares)]
     (lazy-cat (sum-of-consecutive-squares-for-digit i (rest squares) n)
               (sum-of-consecutive-squares (rest squares) n)))))



(defn e125 [n]
  (let [pals (into #{} (take-while #(< % n) (p/palindrome-seq)))
        s (into [] (comp (filter pals) (distinct)) (sum-of-consecutive-squares n))]
    (pprint (into [] (partition-all 5 (sort s))))
    [(count s) (apply + s)]
    ))

(sum-of-consecutive-squares 50)

(e125 1000)

;; (time (println (e125 1000000)))

;; (time (println (e125 100000000)))
;; [166 2906969179]
;; "Elapsed time: 347.660161 msecs"

