(ns euler.e301
  (:use [clojure.test])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [criterium.core :refer [bench quick-bench]]))

;; Nim game 3 stones

(defn X [a b c]
  "return 0 if player with next turn is boned, 1 otherwise"
  (match [a b c]
         ;; this is wrong - if the remaining two stacks are equal i am boned
         [0 _ _] 1
         [_ 0 _] 1
         [_ _ 0] 1
         ))

(defn two-x? [a b] (= (* 2 a) b))

(let [x 2 y 2 z 3]
  (match [x y z]
         [a b _] :guard #(= %1 %2)
         :else :no))
(util/pow 2 30)


(defn my-range
  ([n] (let [cur (atom 0)] [cur (my-range cur n)]))
  ([cur n]
   (if (= @cur n)
     nil
     (cons @cur (lazy-seq (swap! cur inc) (my-range cur n))))))


(defmacro check-highest
  [n coll-sym & body]
  `(let [[h# ~coll-sym] (my-range ~n)
         r# (do ~@body)]
     (println "highest:" @h#)
     [@h# r#]))


(check-highest 10 s (dorun (take 5 s))) ;; => 4


(defn some-stuff ([coll] (eduction (filter even?) (map inc) coll)))


;; This computes up to 8
(check-highest 100 s (transduce (take 5) + (some-stuff s))) ;; => 8

(check-highest 100 s (transduce (take 5) conj (some-stuff s))) 

;; These compute up to 64 b/c chunked-iter
(check-highest 100 s (transduce identity + 0 (take 5 (some-stuff s))))  ;; => 64
(check-highest 100 s (reduce + 0 (take 5 (some-stuff s))))  ;; => 64
