(ns euler.e113
  (use [euler.e112 :only [bouncy?]]))

(defn non-bouncy-upto [pow10]
  (loop [i 1
         last-round 0
         total 0
         v (mapv #(- 10 (inc %)) (range 9))]
    ;; (println i last-round total v)
    (if (= i pow10)
      (+ (* 9 pow10) last-round total)
      (let [inc-or-dec (* 2 (apply + v))
            zeros (v 0)
            this-round (- inc-or-dec zeros)]
        ;; (println "\t" this-round inc-or-dec zeros)
        (recur (inc i)
               (+ this-round last-round)
               (+ total last-round)
               (mapv #(apply + (subvec v %)) (range 9)))))))


(defn brute-force
  ([pow10] (brute-force pow10 1))
  ([pow10 start]
   (reduce #(+ %1 (if (bouncy? %2) 0 (do (println %2) 1))) 0 (range start (Math/pow 10 pow10)))))

(defn diff [n]
  (let [nbu (non-bouncy-upto n)
        bf (brute-force n)]
    [nbu bf (- bf nbu)]))

(diff 6)
(diff 5)
(diff 4)
(diff 3)

(non-bouncy-upto 6)
(non-bouncy-upto 10) 


(brute-force 3 100)


(println (time (non-bouncy-upto 100)))
