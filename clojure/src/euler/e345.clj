(ns euler.e345
  (:use [clojure.test]
        [clojure.core.matrix])
  (:require [euler.util :as util]
            [clojure.core.matrix.select :as sel]
            [clojure.math.combinatorics :as c]
            [clojure.set :as sets]
            [clojure.core.match :refer [match]]
            [criterium.core :refer [bench quick-bench]]))

(defn square-matrix [n data]
  (mapv vec (partition n data)))

(def little (square-matrix 5 [
                              7  53 183 439 863
                              497 383 563  79 973
                              287  63 343 169 583
                              627 343 773 959 943
                              767 473 103 699 303
                              ]))

(def big (square-matrix 15 [
                              7  53 183 439 863 497 383 563  79 973 287  63 343 169 583
                            627 343 773 959 943 767 473 103 699 303 957 703 583 639 913
                            447 283 463  29  23 487 463 993 119 883 327 493 423 159 743
                            217 623   3 399 853 407 103 983  89 463 290 516 212 462 350
                            960 376 682 962 300 780 486 502 912 800 250 346 172 812 350
                            870 456 192 162 593 473 915  45 989 873 823 965 425 329 803
                            973 965 905 919 133 673 665 235 509 613 673 815 165 992 326
                            322 148 972 962 286 255 941 541 265 323 925 281 601  95 973
                            445 721  11 525 473  65 511 164 138 672  18 428 154 448 848
                            414 456 310 312 798 104 566 520 302 248 694 976 430 392 198
                            184 829 373 181 631 101 969 613 840 740 778 458 284 760 390
                            821 461 843 513  17 901 711 993 293 157 274  94 192 156 574
                             34 124   4 878 450 476 712 914 838 669 875 299 823 329 699
                            815 559 813 459 522 788 168 586 966 232 308 833 251 631 107
                            813 883 451 509 615  77 281 613 459 205 380 274 302  35 805
                            ]))

(defn benefit [m]
  (emap-indexed (fn [[r c] e]
                  (let [rw (slice m 0 r) cl (slice m 1 c)
                        bfits (mapv #(- e %) (concat rw cl))
                        avg-bfit (/ (apply + bfits) (- (count bfits) 2))]
                    #_(println e r c bfits avg-bfit)
                    avg-bfit))
                m))

(defn max-index [m]
  (reduce (fn [[_ m :as best] [_ e :as cur]]
            (when (and (pos? e) (= e m)) (println "Warning possible tie:" e))
            (if (> e m) cur best))
          [[-1 -1] Integer/MIN_VALUE]
          (map vector (index-seq m) (eseq m))))

(defn max-benefit
  ([m] (max-benefit m 0))
  ([m tot]
   (let [bm (benefit m)
         [[r c :as i] e] (max-index bm)
         s (get-in m i)]
     (println "Tot:" tot)
     ;; (pm bm)
     (if (= 2 (count m))
       (let [all (vec (flatten m)) ad (+ (get all 0) (get all 3)) bc (+ (get all 1) (get all 2))]
         (+ tot (if (> ad bc) ad bc)))
       (do
         (println "Selecting: " s i (double e))
         (recur (sel/sel m (sel/exclude r) (sel/exclude c)) (+ tot (get-in m [r c]))))))))

;; (time (println (max-benefit little)))
;; 3315
;; "Elapsed time: 3.506279 msecs"

;; (time (println (max-benefit big)))
;; 13891
;; "Elapsed time: 51.109635 msecs"
;; Hmm not correct sadly

(def brute-force (memoize (fn [m]
                            (if (number? m)
                              [m]
                              (let [row (first m)]
                                (reduce (fn [best cur] (if (> (esum best) (esum cur)) best cur))
                                        (for [[i v] (map-indexed vector row)]
                                          (conj (brute-force (sel/sel m (sel/exclude 0) (sel/exclude i))) v))))))))

;; (time (println (brute-force little)))
;; 3315
;; "Elapsed time: 11.146504 msecs"

;; (time (println (brute-force big)))
;; 13938
;; "Elapsed time: 4146.593392 msecs"
;; correct but boooooring
;; wonder why "benefit" guy didn't work


