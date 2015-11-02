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


(defn old-primes-seq []
  (cons 2 (nonchunked-sequence (vec-reduce-sieve []) (iterate #(+ 2 %) 3))))

(def pprimes (cons 2 (precompute1-sequence (vec-reduce-sieve []) (iterate #(+ 2 %) 3))))

;; Introducing - the sievinator!!

(defn vec-extend [v size val]
  (let [vb (transient v)]
    (while (< (count vb) size) 
      (conj! vb val))
    (persistent! vb)))

(defn sievinator []
  (let [the-agent (agent {:primes [] :odd-numbers-seen []})
        default-block-size 500000]
    (letfn [(state [] @the-agent)
            (number-for-index [^long c] (inc (* 2 (inc c))))
            (ensure-n [n wait?]
              (let [{:keys [odd-numbers-seen]} @the-agent]
                (when (< (count odd-numbers-seen) (quot n 2))
                  (send the-agent fill-up-to n)
                  (and wait? (await the-agent)))))
            ;; this is not pretty
            (fill-up-to [{:keys [odd-numbers-seen primes] :as current-state} ^long n]
              (if-not (< (count odd-numbers-seen) (quot n 2))
                current-state
                (let [numbers (transient (vec-extend odd-numbers-seen (quot n 2) 0))
                      next-index (count odd-numbers-seen)
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
                  (let [odd-numbers-seen (persistent! numbers)
                        new-primes (reduce (fn [np i] (conj np (number-for-index i)))
                                           primes
                                           (filter #(zero? (get odd-numbers-seen %))
                                                   (range next-index (count odd-numbers-seen))))]
                    {:odd-numbers-seen odd-numbers-seen :primes new-primes}))))
            (primes-up-to [^long n]
              (ensure-n n false)
              (take-while #(<= % n) (cons 2 (lazy-seq (do (await the-agent)
                                                          (:primes @the-agent))))))
            (factors [^long n]
              (ensure-n n true)
              (loop [n n f [] numbers (:odd-numbers-seen @the-agent)]
                (cond
                  (even? n) (recur (unsigned-bit-shift-right n 1) (conj f 2) numbers)
                  (= 1 n) f
                  :else (let [i (get numbers (dec (quot n 2)))]
                          (if (zero? i)
                            (sort (conj f n))
                            (recur (/ n i) (conj f i) numbers))))))
            (blocked-seq
              ([f block-size]
               (ensure-n block-size false)
               (lazy-seq (blocked-seq f block-size 0)))
              ([f block-size start-index]
               (await the-agent)
               (let [{:keys [primes odd-numbers-seen] :as s} @the-agent
                     size (* 2 (count odd-numbers-seen))
                     [sq nsize] (f s start-index)]
                 (ensure-n (+ block-size size) false)
                 (lazy-cat sq (blocked-seq f block-size nsize)))))
            (primes-seq
              ([] (primes-seq default-block-size))
              ([block-size]
               (cons 2 (blocked-seq (fn [{primes :primes} start-index]
                                      [(map primes (range start-index (count primes))) (count primes)])
                                    block-size))))
             (composites-seq
              ([] (composites-seq default-block-size))
               ([block-size]
                (let [odd-composites 
                      (blocked-seq (fn [{numbers :odd-numbers-seen} start-index]
                                     [(->> (range start-index (count numbers))
                                            (filter #(pos? (numbers %)))
                                            (map number-for-index))
                                      (count numbers)])
                                   block-size)
                      even-composites (iterate #(+ 2 %) 4)
                      ]
                  (letfn [(sfn [e o] 
                                (if (< (first e) (first o))
                                  (cons (first e) (lazy-seq (sfn (rest e) o)))
                                  (cons (first o) (lazy-seq (sfn e (rest o))))))]
                    (lazy-seq (sfn even-composites odd-composites))
                    ))))
             (factors-seq
               ([] (factors-seq (composites-seq)))
               ([cs] (map #(vector % (factors %)) cs)))
             (prime? [n]
               (if (even? n)
                 (= n 2)
                 (do 
                   (ensure-n n true)
                   (let [{:keys [odd-numbers-seen]} @the-agent]
                     (= 0 (odd-numbers-seen (dec (quot n 2))))))))
            ]
      {:wait #(await the-agent) :state state :primes-up-to primes-up-to
       :primes-seq primes-seq :composites-seq composites-seq :factors-seq factors-seq
       :factors factors :prime? prime? :ensure-n ensure-n})))

(def the-sievinator (sievinator))

(def primes ((:primes-seq the-sievinator)))

(def primes-up-to (:primes-up-to the-sievinator))

(def prime? (:prime? the-sievinator))

(def factors (:factors the-sievinator))

(def composites (:composites-seq the-sievinator))

(def factors-seq (:factors-seq the-sievinator))

(defn digits [n]
  (mapv #(- (int %) (int \0)) (seq (str n))))

;; Integer partitions -
;; https://en.wikipedia.org/wiki/Partition_(number_theory)#Algorithm

(declare integer-partitions)

(defn integer-partitions-actual
  "Create all the integer partitions of N"
  ([n] (if (<= n 1)
         (list (list 1))
         (apply list (list n) (for [x (range (dec n) 0 -1)
                                    s (integer-partitions (- n x) x)]
                                (conj s x)))))
  ([n m]
   (let [s (integer-partitions n)]
     (if (>= m n)
       s)
     (filter #(<= (first %) m) s))))

(def integer-partitions (memoize integer-partitions-actual))
