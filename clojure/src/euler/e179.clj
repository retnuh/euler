(ns euler.e179
  (require [euler.util :refer [count-divisors]]))

(def pre-load (future (count-divisors 10000000)))

(defn e179 [n]
  (loop [x 2 y 3 dcx (count-divisors x) tot 0]
    (if (>= x n)
      tot
      (let [dcy (count-divisors y)]
        (recur y (inc y) dcy (if (= dcx dcy) (do #_(println x y) (inc tot)) tot))))))

(e179 100)

;; (time (println (e179 10000000)))
;; 986262
;; "Elapsed time: 16446.811112 msecs"

