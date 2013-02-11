(ns euler.e186
  (:use clojure.algo.monads
        euler.algo.uf))

(defn lazy-lagged-fibonacci-seq
  ([] (map first (next (iterate lazy-lagged-fibonacci-seq [0 1 (clojure.lang.PersistentQueue/EMPTY) (clojure.lang.PersistentQueue/EMPTY)]))))
  ([[_ i q24 q55]]
    (let [x
          (if (< i 56)
            (+' 100003 (*' -1 200003 i) (*' 300007 i i i))
            (+' (peek q24) (peek q55)))
          n (mod x 1000000)
          nq24 (cond (>= i 56) (conj (pop q24) n) (>= i 32) (conj q24 n) :else q24)
          nq55 (cond (>= i 56) (conj (pop q55) n) :else (conj q55 n))]
;      (when (= i 56) (println i " n " n " x " x ", " (peek q24), ", " (peek nq24)  ", " (count nq24) ", " (peek q55) ", " (peek nq55) ", " (count nq55) ", " (take 5 q55)))
;      (when (= i 32) (println i " n " n " x " x ", " (peek nq24), ", " (peek q55) ", " (count q55) ", " (take 5 q55)))
;      (when (<= i 60) (println i " n " n ", " (count nq24) ", " (peek nq24) ", " (count nq55) ", " (peek nq55)))
      [n (inc i) nq24 nq55])))

(defn e186-loop [call-no gen state dropped connections]
  (let [[caller callee] (take 2 gen)]
    (if (= caller callee)
      (recur call-no (drop 2 gen) state (inc dropped) connections)
      (let [calc
            (domonad state-m
              [u (union caller callee)
               sz (component-size 524287)]
              [sz u])
            [[csize u] nstate] (calc state)
            ratio (/ (* 1.0 csize) 1000000)]
;        (when (>= call-no 1840550) (println call-no ", " csize ", " (count nstate) ", " ratio ", " caller ", " callee ", " dropped ", " u ", " connections))
        (if (or (<= 0.99 ratio) (> call-no 2500000))
          [call-no csize (count nstate) ratio dropped connections]
          (recur (inc call-no) (drop 2 gen) nstate dropped ((if (nil? u) identity inc) connections)))))))

(defn e186 []
  (e186-loop 1 (lazy-lagged-fibonacci-seq) {} 0 0))

(time (println "calls: " (e186)))

;(println (str "3681161: " (first (drop 3681160 (lazy-lagged-fibonacci-seq)))))
