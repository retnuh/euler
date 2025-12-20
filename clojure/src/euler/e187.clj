(ns euler.e187
  (require [euler.util :as util]
           [clojure.math.combinatorics :as c]
           ))

(take 5 (util/factors-seq))

(defn e187 [n]
  (->> (util/factors-seq)
       (take-while (fn [[c p]] (< c n)))
       (filter (fn [[_ p]] (= 2 (count p))))
       count))

(println (e187 30))

;; (time (println (e187 100000000)))

;; would have been nice but too slow :P

(def counter (completing (fn ([] 0) ([t _] (inc t)))))

(defn pairs-for-prime [n primes]
  (let [p (first primes)
        limit (/ n p)]
    (transduce (take-while #(<= % limit)) counter primes)))

(pairs-for-prime 30 (util/primes-up-to 15))
(util/primes-up-to 15)

(defn e187 [n]
  (let [primes (util/primes-up-to (/ n 2))
        root (Math/sqrt n)]
    #_(reduce + (map (partial pairs-for-prime n) (take-while #(<= % root) primes)))
    (loop [primes primes
           t 0]
      (if (> (first primes) root)
        t
        (recur (rest primes) (+ t (pairs-for-prime n primes)))))))

(e187 30)

;; (time (println (e187 100000000)))
;; 17427258
;; "Elapsed time: 2471.371014 msecs"

