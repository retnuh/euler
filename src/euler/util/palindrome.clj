(ns euler.util.palindrome
  (require [clojure.pprint :refer [pprint]]
           [euler.util :refer [digits->int]]))

(defn- wrap [x]
  (map (fn [w] (into [w] (concat x [w]))) (range 10)))

(wrap [0])

(defn palindrome-digits-seq
  ([] (palindrome-digits-seq (lazy-cat (map vector (range 10))
                                       (map vector (range 10) (range 10)))))
  ([s] (lazy-cat (remove #(zero? (first %)) s)
                 (palindrome-digits-seq (sort (mapcat wrap s))))))

(pprint (take 121 (palindrome-digits-seq)))

(defn palindrome-seq [] (map digits->int (palindrome-digits-seq)))

(println (take 121 (palindrome-seq)))
