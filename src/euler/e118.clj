(ns euler.e118
  (require [euler.util :refer [primes digits]]))

(time (def e118-relevant-prime-vecs
        (->> primes
             ;; (take-while #(<= % 1000))
             (take-while #(<= % 1000000000))
             (map digits)
             (remove #(some #{0} %))
             (filter #(= (count %) (count (set %))))
             (group-by count)
             )))

(count e118-relevant-prime-vecs)
(apply + (map count (vals e118-relevant-prime-vecs)))
(mapv (fn [[k v]] [k (count v)]) e118-relevant-prime-vecs)
