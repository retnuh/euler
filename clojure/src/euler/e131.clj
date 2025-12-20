(ns euler.e131
  (require [euler.util :as util]))

(def primes-below-1M (future (util/primes-up-to 1000000)))

(def primes-below-100 (future (util/primes-up-to 100)))

(defn cubes-seq [n]
  (map #(*' % % %) (range 1 (inc n))))

(defn squares-and-cubes-of-cubics [s]
  (eduction (map (fn [i] (let [s (*' i i)] [s (*' s i) i]))) s))

(defn e131-brute [n]
  (let [cs (cubes-seq n)
        cubes (into #{} cs)
        primes (util/primes-up-to n)
        snc (squares-and-cubes-of-cubics cs)
        ]
    (println " ")
    (println n)
    (println " ")
    (doseq [p primes
            s-n-c snc]
      (when-let [[sq cube n] s-n-c]
        (when-let [d3 (cubes (+' (*' p sq) cube))]
          (let [i (Math/cbrt n) d (int (Math/cbrt d3))]
            (println p sq cube d3 n i d (/ d n) (*' i i (inc i)) )))))
    ))

(e131-brute 100)


(defn e131 [x]
  (loop [nums (range 1 (inc x))
         cubes (map #(*' % % %) (rest (range)))
         found []]
    (let [i (first nums)
          i2 (*' i i)
          d (*' i2 (inc i))
          d3 (*' d d d)
          dropped-cubes (drop-while #(< % d3) cubes)]
      ;; (println i i2 d d3 (first dropped-cubes))
      (if-not (== d3 (first dropped-cubes))
        (recur (next nums) dropped-cubes found)
        (let [n (*' i2 i)
              n2 (*' n n)
              n3 (*' n2 n)
              p (/ (- d3 n3) n2)]
          ;; (println "found: " p i i2 n n2 n3 d d3)
          (if (> p x)
            found
            (recur (next nums) dropped-cubes (if (util/prime? p) (conj found p) found)))
          )))))

(time (println (e131 1000)))
(time (e131-brute 10000))

;; (time (println (count (e131 1000000))))
