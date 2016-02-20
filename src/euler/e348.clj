(ns euler.e348
  (use clojure.test)
  (require [euler.util.palindrome :as p]
           [clojure.math.combinatorics :as c]
           [clojure.pprint :refer [pprint]]
           [criterium.core :refer [bench quick-bench]]))

(defn sums-for-palindrome
  ([cubes squares p]
   (loop [cubes cubes sums []]
     (if (or (empty? cubes) (= 5 (count sums)))
       (do (when (= 4 (count sums)) (println [p sums])) [p sums])
       (let [c (first cubes) r (- p c)]
         #_(println p c r (squares r))
         (recur (rest cubes) (if (squares r) (conj sums [c r]) sums)))))))


(defn cubes [n]
  (mapv #(* % % %) (range 1 (inc (Math/ceil (Math/cbrt n))))))

(defn squares [n]
  (into #{} (map #(* % %)) (range 1 (inc (Math/ceil (Math/sqrt n))))))

(defn e348 [n]
  (let [pals (into [] (take-while #(< % n) (p/palindrome-seq)))
        c (cubes n)
        s (squares n)
        xf (comp (map #(sums-for-palindrome c s %))
                 (filter #(= (count (second %)) 4))
                 (map first)
                 (take 5))
        answer (into [] xf pals)]
    (println answer)
    [(count answer) (apply + answer)]))


(sums-for-palindrome (cubes 5229225) (squares 5229225) 5229225)

((squares 5229225) 5221225)

;; (time (println (e348 1000000000)))
;; [5 1004195061]
;; "Elapsed time: 9714.688639 msecs"


