(ns euler.e118
  (require [euler.util :refer [the-sievinator primes digits prime? integer-partitions]]
           [clojure.set :as sets]
           [clojure.core.reducers :as r]
           [clojure.math.combinatorics :as c]))

#_((:ensure-n the-sievinator) 1000000000 false)

(time (do (println (count e118-relevant-prime-vecs))
          (def e118-relevant-prime-vecs
            (->> primes
                 #_(r/take-while #(<= % 1000000))
                 (r/take-while #(<= % 1000000000))
                 (r/map digits)
                 (r/remove #(some #{0} %))
                 (r/filter #(= (count %) (count (set %))))
                 r/foldcat
                 ))
          (println (count e118-relevant-prime-vecs))))

(def e118-by-count
  (group-by count e118-relevant-prime-vecs))

#_(def e118-by-digit
  (reduce (fn [m p] (reduce (fn [m1 d] (update m1 d (fnil conj #{}) p)) m p))
          {} e118-relevant-prime-vecs))

#_(count e118-relevant-prime-vecs)
#_(mapv (fn [[k v]] [k (count v)]) e118-by-count)

(defn does-not-contain
  [set-of-sets]
  (apply set (set-of-sets)))

(defn valid-candidates
  [vec-of-digits]
  (let [s (set (flatten vec-of-digits))]
    (fn [x] (not-any? s x))))



(defn e118
  ([] (->> (r/mapcat #(e118 [] %) (keys e118-by-count))
           (r/map set)
           (into #{})))
  ([primes-so-far next-size]
   (let [total-size (count (flatten primes-so-far))
         remaining-sizes (range (- 10 total-size next-size) 0 -1)]
     (if (= 9 total-size)
       (do
         #_(println "found:" primes-so-far)
         [primes-so-far])
       (when-let [candidates (filter (valid-candidates primes-so-far) (e118-by-count next-size))]
         #_(println "combos:" primes-so-far next-size remaining-sizes candidates)
         (->> candidates
              (r/mapcat (fn [c] (r/map (fn [size] (e118 (conj primes-so-far c) size))
                                      remaining-sizes)))
              (r/mapcat identity)
              ))))))

(time (def it (e118)))
(println (count it))
(take 5 it)

(r/foldcat (r/mapcat (fn [y] (r/map (fn [x] (+ x y)) (range 1 5))) [20 30 40]))

(into #{} (map (fn [s] (apply + (map count s))) it))

(into #{} (map (fn [s] (set (flatten (seq s)))) it))

(def digits->int (memoize (fn ([v] (digits->int v 0))
                            ([v t] (if (empty? v) t (recur (rest v) (+ (* 10 t) (first v))))))))

(def e118-primes (set (map digits->int e118-relevant-prime-vecs)))
(time (def e118-unfiltered-primes (set (take-while #(<= % 1000000000) primes))))

(defn cartesian-permutations [s]
  (apply c/cartesian-product (map c/permutations s)))

(cartesian-permutations [[1 2] [4 6] [3 5]])

(defn my-prime? [n]
  (or (e118-primes n)
      (when (e118-unfiltered-primes n)
        (println "hmm found in unfiltered" n)
        n)))

(defn e118-perms []
  (->> (c/partitions [1 2 3 4 5 6 7 8 9])
       (r/mapcat cartesian-permutations)
       (r/map #(map digits->int %))
       #_(r/map #(do (println "foo:" %) %))
       (r/filter #(every? my-prime? %))
       r/foldcat))

(time (println (count (e118-perms))))
