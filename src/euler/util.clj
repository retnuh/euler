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

(defn- reduce-divisors [[^long num ^long sqrt :as n] ^long divisor]
  ;; (println "rd" divisor num)
  (cond
    (> divisor sqrt) (reduced num)
    (zero? (mod num divisor)) (reduced nil)
    :else n))

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
          (if-let [val (reduce reduce-divisors [input (long (Math/sqrt input))] c)]
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

;; Precompute primes in the background
(future (last (primes-up-to 10000000)))

(defn prime? [n]
  (let [upto (int (Math/sqrt n))
        coll (primes-up-to upto)]
    (some? (reduce reduce-divisors n coll))))


;; Introducing - the sievinator!!

(defn vec-extend [v size val]
  (let [vb (transient v)]
    (while (< (count vb) size) 
      (conj! vb val))
    (persistent! vb)))

(defn sievinator []
  (let [primes-seen (atom [])
        odd-numbers-seen (atom [])]
    (letfn [(state [] {:primes-seen @primes-seen
                       :odd-numbers-seen @odd-numbers-seen})
            (number-for-index [^long c] (inc (* 2 (inc c))))
            ;; this is not pretty
            (fill-up-to! [^long n]
              (when (< (dec (count @odd-numbers-seen)) n)
                (let [numbers (transient (vec-extend @odd-numbers-seen (quot n 2) 0))
                      primes @primes-seen
                      next-index (count @odd-numbers-seen)
                      s (range next-index (inc (quot (int (Math/sqrt n)) 2)))
                      next-number (number-for-index next-index)]
                  (dorun (for [p primes
                               :let [d (quot next-number p) nd (if (even? d) (inc d) d)]
                               m (range (dec (quot (* p nd) 2)) (count numbers) p)]
                           (and (> nd 1) (assoc! numbers m p))))
                  
                  (dorun (for [i s
                               :when (zero? (get numbers i))
                               :let [c (number-for-index i)]
                               m (rest (range
                                        (max i (quot (* i (quot next-number c)) 2))
                                        (count numbers)
                                        c))]
                           (assoc! numbers m c)))
                  (reset! odd-numbers-seen (persistent! numbers))
                  (dorun (for [i (range next-index (count @odd-numbers-seen))
                               :when (zero? (get @odd-numbers-seen i))]
                           (swap! primes-seen conj (number-for-index i)))))))
            (primes-up-to [^long n]
              (fill-up-to! n)
              (take-while #(<= % n) (cons 2 @primes-seen)))
            (factors [^long n]
              (fill-up-to! n)
              (loop [n n f [] numbers @odd-numbers-seen]
                (if (even? n)
                  (recur (unsigned-bit-shift-right n 1) (conj f 2) numbers)
                  (let [i (get numbers (dec (quot n 2)))]
                    (if (zero? i)
                      (sort (conj f n))
                      (recur (/ n i) (conj f i) numbers))))))]
      {:state state
       :primes-up-to primes-up-to
       :factors factors})))
