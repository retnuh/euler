(ns euler.e347
  (require [euler.util :as util]
           [euler.util.heap :as heap]
           [clojure.pprint :refer [pprint]]
           [clojure.math.combinatorics :as c]))

(defn M-queue
  ([p q n] (M-queue p q n (conj clojure.lang.PersistentQueue/EMPTY (* p q)) 0))
  ([p q n queue best]
   (if (empty? queue)
     best
     (let [x (peek queue)]
       ;; (println x best (seq queue))
       (if (<= x n)
         (recur p q n (conj (pop queue) (* x p) (* x q))  (max best x))
         (recur p q n (pop queue) best))
       
       ))))

(defn M-vec
  ([p q n] (M-vec p q n [(* p q)] 0))
  ([p q n queue best]
   (if (empty? queue)
     best
     (let [x (first queue)]
       ;; (println x best (seq queue))
       (if (<= x n)
         (recur p q n (conj (rest queue) (* x p) (* x q))  (max best x))
         (recur p q n (rest queue) best))
       
       ))))

(defn M-queue-set
  ([p q n] (M-queue-set p q n (conj clojure.lang.PersistentQueue/EMPTY (* p q)) #{} 0))
  ([p q n queue seen best]
   (if (empty? queue)
     best
     (let [x (peek queue)]
       ;; (println x best (seq queue))
       (if (> x n)
         best
         (let [a (* x p) b (* x q)
           next-queue (cond-> (pop queue)
                          (and (<= a n) (not (seen a))) (conj a)
                          (and (<= b n) (not (seen b))) (conj b))]
           (recur p q n next-queue (conj seen a b) (max x best))))
       ))))

(defn M-left-heap
  ([p q n] (M-left-heap p q n (heap/create-heap (* p q)) 0))
  ([p q n queue best]
   (let [x (heap/find-min queue)]
     ;; (println x best (seq queue))
     (if (<= x n)
       (recur p q n (-> (heap/delete-min-dedup queue) (heap/merge (heap/create-heap (* x p) (* x q)))) x)
       best))))

(defn M-binomial-heap
  ([p q n] (M-binomial-heap p q n (heap/mk-binomial-heap (* p q)) 0))
  ([p q n queue best]
   (let [[x nq] (heap/extract-min-dedup-binomial-heap queue)]
     ;; (println x best (seq queue))
     (if (<= x n)
       (recur p q n (->> nq
                         (heap/insert-into-binomial-heap (* x p))
                         (heap/insert-into-binomial-heap (* x q)))
              x)
       best))))

(defn M-set
  ([p q n] (M-set p q n (sorted-set (* p q)) 0))
  ([p q n s best]
   (if (empty? s)
     best
     (let [x (first s)]
       (println x best s)
       (if (<= x n)
         (recur p q n (conj (rest s) (* x p) (* x q)) x)
         best)))))

(M-binomial-heap 2 5 100)
(M-binomial-heap 2 3 100)

(def M M-queue-set)

(defn S
  ([n] (S n M))
  ([n m-fn]
   (let [primes (util/primes-up-to (/ n 2))
         root (Math/sqrt n)
         pp (take-while #(<= (first %) root) (c/combinations primes 2))]
     (println "starting calc")
     (reduce + (distinct (map (fn [[p q]] #_(m-fn p q n) (let [x (m-fn p q n)]
                                                          (if (pos? x) (println "pps:" p q x (util/factors x)))
                                                          x)) pp))))))

;; Should be 2262

(S 1000 M-queue-set)

;; (time (println (S 1000000 M-queue)))
;; (time (println (S 1000000 M-vec)))
;; (time (println (S 1000000 M-queue-set)))
;; (time (println (S 1000000 M-left-heap)))
;; (time (println (S 1000000 M-binomial-heap)))

;; (time (println (S 10000000)))


(- (S 100) 2262)

(util/factors 96)

(defn sieve [n]
  (let [size (- n 2)
        half (/ size 2)]
    (loop [nums (vec (repeat (inc size) []))
           inds (range half)]
      (if (empty? inds)
        nums
        (let [ind (first inds)
              p (+ 2 ind)
              factors (get nums ind)
              nnums (if-not (empty? factors)
                      nums
                      (reduce (fn [nnums i] (update-in nnums [i] conj p))
                              nums (rest (range ind (inc size) p))))]
          #_(println ind p factors (range ind root p))
          (recur nnums (rest inds))
          )))))

(dorun (map-indexed #(println (+ 2 %1) %2) (sieve 100)))
(let [s100 (sieve 100)]
  (println (peek s100) (type (peek s100)))
  (println (get s100 97) (type (get s100 97))))

;; (time (let [x (sieve 10000000)]))

(defn ppp
  "products of prime pairs"
  [n]
  (let [nums (sieve n)]
    (reduce
     (fn [prods [i factors]]
       (let [x (+ 2 i)]
         (if (= 2 (count factors))
           (assoc prods factors x)
           prods)))
     {} (map-indexed vector nums))
    ))

(defn S-sieve [n]
  (apply + (vals (ppp n))))

(S-sieve 100)

;; (time (println (S-sieve 10000000)))
;; 11109800204052
;; "Elapsed time: 32020.21054 msecs"


