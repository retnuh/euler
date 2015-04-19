(ns euler.e103
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :as combo]
            [clojure.test :as test]))

(defn totalo [domain coll total-var]
  (let [vars (repeatedly (dec (count coll)) lvar)]
    ;(println "totalo" coll total-var vars)
    (if-let [final (last vars)]
      (all
        (everyg #(fd/in % domain) vars)
        (== total-var final)
        ((fn total* [a cs vs]
           ;(println "\ttotalo" a cs vs)
           (if-let [b (first cs)]
             (all
               (fd/+ a b (first vs))
               (total* (first vs) (rest cs) (rest vs)))
             succeed)) (first coll) (rest coll) vars))
      (== (first coll) total-var))))


(defn special-subsets
  ([d s] (special-subsets d (first s) (second s)))
  ([domain b c]
    (let [cb (count b)
          cc (count c)]
      ;(println "b" b cb)
      ;(println "c" c cc)
      (fresh [bt ct]
             (fd/in bt ct domain)
             (totalo domain b bt)
             (totalo domain c ct)
             (cond
               (> cb cc) (fd/> bt ct)
               (< cb cc) (fd/< bt ct)
               :else (fd/!= bt ct)
               )))))

(defn dominated? [[a b]]
  (if (empty? a)
    true
    (and (< (first a) (first b)) (recur [(rest a) (rest b)]))))

(defn subsets [vars]
  (->> vars
       combo/partitions
       rest
       (mapcat #(combo/combinations % 2))
       (remove #(= 1 (count (first %)) (count (second %))))
       (map vec)
       sort
       dedupe
       ))

(defn relevant-subsets [coll]
  (let [all (subsets coll)
        grouped (group-by #(= (count (first %)) (count (second %))) all)
        uneven (grouped false)
        undominated (->> (grouped true)
                       (filter #(= (count (first %)) (count (second %))))
                       (remove dominated?))
        relevant (concat undominated uneven)]
    (println "subset counts" (count all) (count uneven) (count undominated) (count relevant))
    relevant))

(defn lvar-subsets [n vars-vec]
  (let [coll (range n)
        rs (relevant-subsets coll)]
    (map (fn [[l r]] [(map #(get vars-vec %) l) (map #(get vars-vec %) r)]) rs)))

;; (println (lvar-subsets 4 (vec (take 4 (map (comp lvar str char) (iterate inc (int \a)))))))
;; (def dom7 (relevant-subsets (range 1 8)))
;; (println (count dom7))


(defn e103
  ([n minint] (e103 n minint nil))
  ([n minint constraints]
  (println n minint constraints)
  (let [vars (vec (take n (map (comp lvar str char) (iterate inc (int \a)))))
        subset-pairs (lvar-subsets n vars)
        domain (fd/interval minint (inc (int (Math/ceil (* 3.5 minint)))))
        totals-domain (fd/interval minint (if constraints (apply + constraints) (* n 4 minint)))
        results (run 1 [q]
                      (== q vars)
                      (everyg #(fd/in % domain) (butlast vars))
                      (fd/in (last vars) (fd/interval (dec (* 2 minint)) (* n 4 minint)))
                      (fd/distinct vars)
                      (if constraints
                        (do
                          (println "constraints" constraints)
                          (fresh [vt]
                                 (totalo totals-domain vars vt)
                                 #_(== vars [22, 33, 39, 42, 44, 45, 46])
                                 (fd/< vt (apply + constraints))))
                        (do (println "other" n) succeed))
                      (everyg #(fd/< (first %) (second %)) (partition 2 1 vars))
                      (everyg #(special-subsets totals-domain %) subset-pairs))]
    ;(println vars)
    ;(dorun (for [p subset-pairs] (println p)))
    ;(println domain)
    ;(println totals-domain)
    (println "results" (type results) results)
    (if-let [r (time (first results))]
      (do
        (println "recur" n minint r)
        (recur n minint r))
      constraints)
    #_(apply min-key #(apply + %) results)
    #_(run* [q]
          (fresh [qt]
                 (membero q results)
                 (totalo totals-domain q qt)
                 (fd/in qt totals-domain)
                 (everyg (fn [o]
                           (fresh [ot]
                                  (totalo totals-domain o ot)
                                  (fd/in totals-domain ot)
                                  (fd/<= qt ot)))
                         results)))
    )))


;;(println (e103 3 10))

(let [s (System/currentTimeMillis)
      ;r (e103 5 5)
      ;r (e103 6 11 [11, 17, 20, 22, 23, 24])
      ;r (e103 7 17 [20, 31, 38, 39, 40, 42, 45])
      r (e103 7 20)
      ]
  (println "result" r (double (/ (- (System/currentTimeMillis) s) 60000)) "min"))

; result [20 31 38 39 40 42 45] 93.17848333333333 min
; so, didn't find anything...

; Quite irritatingly, the answer _is_ the "near optimal" one suggested
; in the problem (i.e. the constraints I've been plugging in).  The
; Solver can find that pretty dang fast; in a few milliseconds.  It
; then spends another 12 minutes "proving" that there aren't any other
; better solutions.
