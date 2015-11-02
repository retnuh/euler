(ns euler.e118
  (require [euler.util :refer [the-sievinator primes digits integer-partitions]]
           [clojure.set :as sets]
           [clojure.core.reducers :as r]))

#_((:ensure-n the-sievinator) 1000000000 false)

(time (def e118-relevant-prime-vecs
        (->> primes
             #_(r/take-while #(<= % 1000000000))
             (r/take-while #(<= % 1000000000))
             (r/map digits)
             (r/remove #(some #{0} %))
             (r/filter #(= (count %) (count (set %))))
             r/foldcat
             )))

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
              r/foldcat))))))

(time (def it (e118)))
(println (count it))
(take 5 it)

(r/foldcat (r/mapcat (fn [y] (r/map (fn [x] (+ x y)) (range 1 5))) [20 30 40]))

(into #{} (map (fn [s] (apply + (map count s))) it))

(into #{} (map (fn [s] (set (flatten (seq s)))) it))
