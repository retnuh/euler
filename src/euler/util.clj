(ns euler.util)

(defn- pthread [& args]
  (apply println (.getName (Thread/currentThread)) args))

(defn precompute1-seq [#^clojure.lang.ISeq s]
  (when (seq s)
    (let [f (first s)
          n (future
              ;; (pthread "future next" f)
              (let [sn (.next s)]
                (and sn (do
                          ;; (pthread "future next" f "result" (first sn))
                          sn))))]
      (lazy-seq
       (cons (first s)
             (precompute1-seq @n))))))

(defn nonchunked-sequence
  "Coerces coll to a (possibly empty) sequence, if it is not already
  one. Will not force a lazy seq. Will not use chunks. 
  (sequence nil) yields (), When a
  transducer is supplied, returns a lazy sequence of applications of
  the transform to the items in coll(s), i.e. to the set of first
  items of each coll, followed by the set of second
  items in each coll, until any one of the colls is exhausted.  Any
  remaining items in other colls are ignored. The transform should accept
  number-of-colls arguments"
  {:static true}
  ([coll]
     (if (seq? coll) coll
         (or (seq coll) ())))
  ([xform coll]
     (or (clojure.lang.IteratorSeq/create
         (clojure.lang.TransformerIterator/create xform (clojure.lang.RT/iter coll)))
       ()))
  ([xform coll & colls]
     (or (clojure.lang.IteratorSeq/create
         (clojure.lang.TransformerIterator/createMulti
           xform
           (map #(clojure.lang.RT/iter %) (cons coll colls))))
       ())))

(defn precompute1-sequence
  "combine precompute1-seq with nonchunked-sequence to create a precompute
  1 at a time lazy transducer thingy"
  {:static true}
  [& args]
  (precompute1-seq (apply nonchunked-sequence args)))

(defn remove-evenly-divisible [n]
  (remove #(do
             ;; (println "pre" n %)
             (zero? (mod % n))             
             )))

(defn- reduce-divisors [num divisor]
  ;; (println "rd" divisor num)
  (if (zero? (mod num divisor))
    (reduced nil)
    num))


;; Sieve that just collects the primes so far, using smart reduction
;; stuff to efficiently "loop" through existing primes. Simpler and
;; presumeably faster than comp-sieve below
(defn- vec-reduce-sieve
  ([] (vec-reduce-sieve []))
  ([init-coll]
   (fn [xf]
    (let [c (java.util.ArrayList. (vec init-coll))]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if-let [val (reduce reduce-divisors input c)]
           (do
             ;; (pthread "vec-reduce-sieve" val input result)
             (.add c input)
             (xf result input))
           result)))))))

#_(println "result" (reduce reduce-divisors 15 [3 7 5]))

#_(take 10 (sequence  (comp  (comp
                            (remove-evenly-divisible 3)
                            (remove-evenly-divisible 5))
                           (remove-evenly-divisible 7))
                    (iterate #(do (println "iter " (+ 2 %)) (+ 2 %)) 3)))

;; A sieve that uses an internal transducer chain, that grows at each
;; step using comp.  Works but probably builds a lot of thunks along
;; the way!
(defn- comp-sieve []
  (fn [xf]
    (let [rfun (fn ([current] current) ([_ input] input))
          xforms (volatile! (map identity))
          xducer (volatile! (@xforms rfun))]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if-let [c (@xducer nil input)]
           (when-not (or (reduced? c) (nil? c))
             (let [ret (xf result c)]
               ;; (println "comp-sieve" c input result ret xf)
               (vreset! xforms (comp @xforms (remove-evenly-divisible c)))
               (vreset! xducer (@xforms rfun))
               ret))
           (do
             ;; (println "nsieve" input result xf)
             result)))))))


(defn primes-seq []
  (cons 2 (nonchunked-sequence (vec-reduce-sieve []) (iterate #(+ 2 %) 3))))

(def primes (primes-seq))

(def pprimes (cons 2 (precompute1-sequence (vec-reduce-sieve []) (iterate #(+ 2 %) 3))))

(defn primes-up-to [n]
  (take-while #(<= % n) pprimes))

(defn- connected-within1
  [smaller larger] (or (= smaller (rest larger)) (= smaller (drop-last larger))))

(defn- connected-same-length
  [seq1 seq2 cost-so-far]
  (let [cost (if (= (first seq1) (first seq2)) 0 1)
        total-cost (+ cost-so-far cost)]
    (cond
      (empty? seq1) (= 1 cost-so-far)
      (>= total-cost 2) false
      :else (recur (rest seq1) (rest seq2) total-cost))))

(defmulti connected? (fn [a b] (type a)))
(defmethod connected? java.lang.Number [x y] (connected? (seq (str x)) (seq (str y))))
(defmethod connected? java.lang.String [x y] (connected? (seq x) (seq y)))
(defmethod connected? clojure.lang.Sequential [seq1 seq2]
  (cond
    (= (count seq1) (count seq2)) (connected-same-length seq1 seq2 0)
    (>= (Math/abs (- (count seq1) (count seq2))) 2) false
    (> (count seq1) (count seq2)) (connected-within1 seq2 seq1)
    :else (connected-within1 seq1 seq2)))

