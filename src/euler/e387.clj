(ns euler.e387
  (:use [clojure.test])
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]))

#_(def prepare-small-primes (future (util/prime? 10001)))

#_(def prepare-large-primes (future (util/prime? (inc (util/pow 10 14)))))

(defn prime?
  "Quick hack while fix mem issues on sievinator"
  [n]
  (.isProbablePrime (biginteger n) 20))

;; Problem about Harshad numbers or Niven numbers

(defn harshad-digits? [n v]
  (let [q (/ n (apply + v))]
    (and (not (ratio? q)) q)))

(defn harshad? [n]
  (harshad-digits? n (util/digits n)))


(defn strong-harshad? [n]
  (let [d (util/digits n) s (apply + d)
        q (/ n s)]
    (and (not (ratio? q)) (prime? q))))

(deftest test-harshad-numbers
  (are [x] (harshad? x)
    201 20 21 27)
  (are [x] (strong-harshad? x)
    201 27 21)
  (are [x] (not (strong-harshad? x))
    23 25 31 36))

(defn generate-rt-harshad
  "Given a harshad vector [n [digits] n/sum of digits], return a coll of right truncatable 
harshad numbers, where the given number is the leftmost digits"
  [[n digits :as hv]]
  (let [x10 (*' 10 n)]
    (eduction (comp (map (partial conj digits))
                    (keep (fn [nd] (let [s  (+' x10 (peek nd))]
                                    (when-let [q (harshad-digits? s nd)]
                                      [s nd q])))))
              (range 10))))

(defn generate-harshad-prime
  "Given a harshad vector, generate all the primes possible (if any) by adding a digit to the right"
  [[n digits :as hv]]
  (let [x10 (*' 10 n)]
    (eduction (comp (map (partial conj digits))
                    (keep (fn [nd] (let [s  (+' x10 (peek nd))]
                                    (when (prime? s)
                                      (conj hv s))))))
              (range 1 10 2))))


(defn rt-harshad-seq
  "Given a coll of harshad vectors, return a new coll of right truncatable harshad vectors that can be generated from those in the given coll"
  [hs]
  (eduction (mapcat generate-rt-harshad) hs))

(defn rt-harshad-stream
  "Given the seeds, create an infinite-seq of right truncatable harshad numbers"
  ([] (harshad-stream (mapv #(vector % [%] 1) (range 1 10))))
  ;; not entirely sure why I need this mapcat indentity step, tbh...
  ([seeds] (eduction (comp (mapcat identity) (drop (count seeds))) (iterate rt-harshad-seq seeds))))

(defn strong-rt-harshad-stream
  []
  (eduction (filter #(prime? (peek %))) (rt-harshad-stream)))

(defn strong-rt-harshad-prime-stream
  []
  (eduction (mapcat generate-harshad-prime) (strong-rt-harshad-stream)))


(deftest test-generate-harshad 
  (is (= [[20 [2 0] 10] [21 [2 1] 7] [24 [2 4] 4] [27 [2 7] 3]]
         (vec (generate-rt-harshad [2 [2]]))))
  (is (= [[200 [2 0 0] 100] [201 [2 0 1] 67] [204 [2 0 4] 34] [207 [2 0 7] 23] [209 [2 0 9] 19]]
         (vec (generate-rt-harshad [20 [2 0] 2]))))
  (is (= (filterv harshad? (range 10 100)) (into [] (comp (map #(vector % [%] 1))
                                                          (mapcat generate-rt-harshad)
                                                          (map first)) (range 1 10))))
  )

(def harshad-below-100 (into [] (filter harshad?) (range 10 100)))
(count harshad-below-100)
(count (into [] (filter harshad?) (range 10 1000)))
(filterv harshad? (range 200 210))

(defn pargs
  [fsym]
  (fn [& args]
    (println (name fsym) args)
    (apply (eval fsym) args)))

(do
  (println "pre")
  (let [e (eduction (comp (filter (pargs 'even?)) (map (pargs 'inc))) (range 10))]
    (println "defined")
    (println (vec e))))

(defn inc-seq
  [s]
  (eduction (map (fn [n] (mapv #(+ % n) s))) s))

(take 10 (strong-rt-harshad-prime-stream))

;; You are given that the sum of the strong, right truncatable Harshad primes less than 10000 is 90619.


(defn e387
  [upto]
  (transduce (comp (map peek) (take-while #(<= % upto))) + (strong-rt-harshad-prime-stream)))

;; (time (e387 10000))
;; (time (println (e387 100000000000000)))

;; 696067597313468
;; "Elapsed time: 176.170217 msecs"

