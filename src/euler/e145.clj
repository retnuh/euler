(ns euler.e145
  (:use [clojure.test])
  (:require [clojure.pprint :refer [pprint]]
            [euler.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
            [criterium.core :refer [bench quick-bench]]))

(defn reversible? [n]
  (let [d (util/digits n)
        rd (reverse d)
        r (util/digits->int rd)
        s (+ n r)
        sd (util/digits s)]
    (and (pos? (peek rd)) (every? odd? sd))))

(reversible? 14)

(pprint (filterv reversible? (range 10 1000)))

;; (println (take 250 (filter reversible? (range 1000000 20000000))))


;; (time (println  (count (filter reversible? (range 10 1000000000)))))
;; 608720
;; "Elapsed time: 1606540.012282 msecs"
;; Bit Cheezy, 27 minutes.  Should prolly do it for realz...

(reversible? 10002)
