(ns euler.e346
  (:use [clojure.test])
  (:require [euler.util :as util]
            [criterium.core :refer [bench quick-bench]]))

(defn base-repunits-upto
  [upto base]
  (loop [acc [] p (* base base) total (+ 1 base p)]
    (if (>= total upto)
      (do
        #_(println base upto acc)
        acc)
      (let [np (* p base)]
        (recur (conj acc total) np (+ total np))))))

(base-repunits-upto 50 3)

(defn sum-of-strong-repunits-below [n]
  (let [s (inc (Math/floor (Math/sqrt n)))]
    #_(println "s:" s (range 2 s))
    (inc (transduce (comp (mapcat (partial base-repunits-upto n))
                          (distinct))
                    + (range 2 s)))))



;; (time (println (sum-of-strong-repunits-below (util/pow 10 3))))
;; 15864
;; "Elapsed time: 0.378878 msecs"


;; (time (println (sum-of-strong-repunits-below (util/pow 10 12))))
;; 336108797689259276
;; "Elapsed time: 1150.309413 msecs"

