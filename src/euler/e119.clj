(ns euler.e119
  (use clojure.test)
  (require [euler.util :refer [the-sievinator primes digits composites factors]]
           [clojure.set :as sets]
           [clojure.core.reducers :as r]
           [clojure.math.combinatorics :as c]
           [euler.util.heap :as h]
           [criterium.core :refer [bench quick-bench]]))

(def composites-seq (composites))

#_(defn- do-curried
  [name doc meta args body]
  (let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

#_(defmacro ^:private defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))

#_(defn- do-rfn [f1 k fkv]
  `(fn
     ([] (~f1))
     ~(clojure.walk/postwalk
       #(if (sequential? %)
          ((if (vector? %) vec identity)
           (remove #{k} %))
          %)
       fkv)
     ~fkv))

#_(defmacro ^:private rfn
  "Builds 3-arity reducing fn given names of wrapped fn and key, and k/v impl."
  [[f1 k] fkv]
  (do-rfn f1 k fkv))

#_(defcurried r-keep
  "Applies f to every value in the reduction of coll, and keeps computed
values which are non-nil. Foldable."
  {:added "1.5"}
  [f coll]
  (r/folder coll
   (fn [f1]
     (rfn [f1 k]
          ([ret k v]
           (if-let [r (f k v)]
             (f1 ret r)
             ret))))))


(defn power-of? [x n & first-digit-x]
  (when-not (= n 1)
    (loop [c n]
      ;; (println "checking" x n c)
      (cond
        (> c x) nil
        (= c x) [x n]
        :else (recur (* c n))))))

(def mod-10-table (make-array Integer/TYPE 0xFFFF))
(doseq [i (range 0xFFFF)]
  (aset mod-10-table i (mod i 10)))

(def digit-table (make-array Boolean/TYPE 16 10))
(letfn [(init-table [d & positions]
          (doseq [p positions]
            (aset digit-table d p true)))]
  (init-table 0 0)
  (init-table 1 1)
  (init-table 2 2 4 8 6)
  (init-table 3 3 9 7 1)
  (init-table 4 4 6)
  (init-table 5 5)
  (init-table 6 6)
  (init-table 7 7 9 3 1)
  (init-table 8 8 4 2 6)
  (init-table 9 9 1)
  )
;; (clojure.pprint/pprint digit-table)

(defn table-mod-10 [n]
  (let [t (bit-and n 0xFFFF)]
    (if (zero? t)
      (aget mod-10-table t)
      (mod n 10))))

(defn table-power-of?
  "This first checks if the first digit of x is one of the first digits
in the cycle of digits when raising a number to multiple powers"
  [x n first-digit-x]
  (when (aget digit-table (table-mod-10 n) first-digit-x)
    (power-of? x n)))

(deftest power-of-test
  (testing "Basic power-of? functionality"
    (is (= [512 8] (power-of? 512 8)))
    (is (= [512 2] (power-of? 512 2)))
    (is (= nil (power-of? 512 4)))
    (is (= [614656 28] (power-of? 614656 28))))
  (testing "tableified power-of functionality"
    (is (= [512 8] (table-power-of? 512 8 2)))
    (is (= [512 2] (table-power-of? 512 2 2)))
    (is (= nil (table-power-of? 512 4 2)))
    (is (= [614656 28] (table-power-of? 614656 28 6))))
  )

(defn check-e119 [f x]
  (let [d (digits x)]
    (f x (apply + d) (peek d))))

(deftest check-e119-test
  (is (= [512 8] (check-e119 power-of? 512)))
  (is (= [614656 28] (check-e119 power-of? 614656)))
  (is (= nil (check-e119 power-of? 11))))

(defn e119-seq
  [f]
  (->> (range)
       (drop 10)
       (keep (partial check-e119 f))))

(def the-e119-seq (e119-seq power-of?))

(defn take-n-e119
  ([n] (take-n-e119 n [] (map-indexed (fn [i n] [(inc i) n]) the-e119-seq)))
  ([n acc s]
   (when-not (zero? n)
     (let [x (time (first s))]
       (println x)
       (recur (dec n) (conj acc x) (rest s))))))

;; (time (take-n-e119 30))

#_(def blat (into [] (range 100000000)))
#_(defn r-take-n-e119
  ([n] (take-n-e119 n power-of?))
  ([n f]
   (->> #_(drop 4 composites-seq)
        blat
        (r/drop 10)
        (r-keep (partial check-e119 f))
        (r/take n)
        (into []))))

;; (quick-bench (take-n-e119 11 power-of?))
;; (quick-bench (r-take-n-e119 11 power-of?))

;; Hmm table not great; multiplications faster.  Probably better cache
;; characteristics.
;; (time (println (take-n-e119 30)))

;; This is kinda cool but gets slower and slower presumeably b/c it
;; has to hold on to the results each time.  I wonder if it could be
;; transducer-ified to get rid of the lazy/memory caching aspect?
(defn digit-sum-seq
  ([]
   ;; We have to not reproduce the first part of the sequence we pass
   ;; to neato usually, so we just do it the first time
   (lazy-cat (range 10) (digit-sum-seq (range 10))))
  ([s]
   ;; (println "neato-iter: " (count s))
   (let [n (digit-sum-seq s s (range 0 10))]
     ;; The first (count s) are basically duplicates of s;
     ;; but we've already produced them; so skip them.
     (lazy-cat (drop (count s) n) (digit-sum-seq n))))
  ([s h ex]
   (if (empty? ex)
     nil
     (if-let [n (first s)]
       (cons (+ n (first ex)) (lazy-seq (digit-sum-seq (rest s) h ex)))
       (recur h h (rest ex))))))

;; This is cute and all but sadly much slower than the take-n-e119 sequence.
(defn e119-lazy-seq
  ([] (drop 10 (e119-seq (range) (flatten (repeat (range 10))) (digit-sum-seq))))
  ([nums digits dss]
   (if-let [p (power-of? (first nums) (first dss))]
     (cons p (lazy-seq (e119-lazy-seq (rest nums) (rest digits) (rest dss))))
     (recur (rest nums) (rest digits) (rest dss)))))

;; (time (println (take-n-e119 11)))

;; (quick-bench (take 10 (e119-seq)))
;; (quick-bench (take-n-e119 10))


;;; Totally different approach - use a min-heap to just quickly raise
;;; numbers to powers.  It takes a lot of digits to go over 300 (33+)
;;; so just run from 2 - 299, keep raising them, pick off the
;;; qualifying dudes

(defn digit-sum [x]
  (apply + (digits x)))

(defn e119? [[n base pow]]
  (= (digit-sum n) base))

(deftest e119?-test
  (testing "Basic e119? functionality"
    (is (= true (e119? [512 8 3])))
    (is (= false (e119? [512 2 9])))
    (is (= false (e119? [512 4 6])))
    (is (= true (e119? [614656 28 4])))))

(defn next-pow [[n base pow]]
  [(* n base) base (inc pow)])

(defn e119-pow-seq
  ([] (drop-while #(< (first %) 10)
                  (e119-pow-seq (apply h/create-heap (map #(vector (* % %) % 2) (range 2 100))))))
  ([heap]
   (let [t (h/find-min heap)]
     (cons t (lazy-seq (e119-pow-seq (h/insert (h/delete-min heap) (next-pow t))))))))

(defn fast-e119-seq []
  (->> (e119-pow-seq)
       (filter e119?)))

;; (time (take-n-e119 30 [] (fast-e119-seq)))
;; [248155780267521 63 8]
;; "Elapsed time: 15.651225 msecs"

;; Wow, super fast.  Didn't even go near 300.  Running it with 100
;; instead of 300 - 12 millis.  More than enough, highest seen was 68.
