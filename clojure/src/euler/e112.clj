(ns euler.e112)

(defn bouncy? [n]
  (loop [s (partition 2 1 (seq (str n)))
         non-increasing false
         non-decreasing false]
    (cond (and non-increasing non-decreasing) true
          (empty? s) false
          :else (let [[a b] (map int (first s))]
                  (recur (rest s) (or non-increasing (> a b)) (or non-decreasing (> b a)))))))

(defn e112 [pct]
  (reduce (fn [tot i]
            (let [ntot (+ tot (if (bouncy? i) 1 0))
                  p (/ ntot i)]
              ;;(println i ntot p)
              (if (= pct p)
                (reduced i)
                ntot)))
          0 (iterate inc 1)))

(println (time (e112 (/ 99 100))))
