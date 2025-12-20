(ns euler.e425
  (:require [euler.util :as util]
            [clojure.set :as sets]))

(defn- connected-within1
  [smaller larger] (= smaller (rest larger)))

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

(defn update-in!
  "update-in that expects a transient guy at top level"
  {:added "1.0"
   :static true}
  ([m [k & ks] f & args]
   (if ks
     (assoc! m k (apply update-in (get m k) ks f args))
     (assoc! m k (apply f (get m k) args)))))

(defn make-smaller-connected [prime-strings number]
  (let [number-string (str number)
        mutations (for [index (range (count number-string))
                        :let [pre (.substring number-string 0 index)
                              c (.charAt number-string index)
                              post (.substring number-string (inc index))]
                        other (range (int \0) (inc (int \9))) :when (not (== other (int c)))]
                    (str pre (char other) post))
        possibles (cons (.substring number-string 1) mutations)
        ]
    ;; (println possibles)
    (sequence (comp
               (filter prime-strings)
               (map #(Integer/parseInt %))
               (filter #(< % number))
               )
              possibles)))

(let [primes (util/primes-up-to 200)
      prime-strings (set (map str primes))]
  ;; (println primes)
  (println (vec (make-smaller-connected prime-strings 103))))


(defn build-graph [primes]
  (loop [primes primes
         prime-strings #{}
         graph (transient {})]
    (if-not primes
      (persistent! graph)
      (let [number (first primes)]
        (recur (next primes)
               (conj prime-strings (str number))
               (assoc! graph number (make-smaller-connected prime-strings number)))
        ))))

(defn lookup-root [m val]
  (let [found (get m val val)]
    (if (= found val)
      val
      (recur m found))))

(defn update-root [m val new-root]
  (let [old-root (get m val val)
        nm (assoc m val new-root)]
    (if (= old-root val)
      nm
      (recur nm old-root new-root))))

(defn partition-relatives [primes graph]
  (loop [todo primes
         roots {2 2}
         unrelated #{}]
    (if (empty? todo)
      unrelated
      (let [val (first todo)
            nodes (graph val)
            lowest-root (if (empty? nodes) val (apply min (map #(lookup-root roots %) nodes)))]
        ;; (println val nodes lowest-root)
        ;; (println roots)
        (recur
         (rest todo)
         (reduce (fn [m n] (update-root m n lowest-root)) roots (cons val nodes))
         (if (= 2 lowest-root) unrelated (conj unrelated val)))))))

(defn e425 [n]
  (let [primes (util/primes-up-to (int n))
        graph (time (build-graph primes)) 
        unrelated (time (partition-relatives primes graph))]
    ;; (println "graph" graph)
    [(reduce + unrelated) unrelated]))

(time (e425 200))
(time (e425 (Math/pow 10 3)))
(time (e425 (Math/pow 10 4)))
(time (println (first (e425 (Math/pow 10 7)))))

;; (let [tm (java.util.TreeMap. {3 :blart 1 :foo})
;;       [k v] (.pollFirstEntry tm)]
;;   (println "k" k "v" v "tm" tm))
