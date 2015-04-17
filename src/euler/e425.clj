(ns euler.e425
  (:require [euler.util :as util]
            [clojure.set :as sets]))

(defn update-in!
  "update-in that expects a transient guy at top level"
  {:added "1.0"
   :static true}
  ([m [k & ks] f & args]
   (if ks
     (assoc! m k (apply update-in (get m k) ks f args))
     (assoc! m k (apply f (get m k) args)))))

(defn build-node-reduction [start coll length-map offset]
  (reduce (fn [m num]
            (let [others (get length-map (+ offset (count num)))
                  num-int (Integer/parseInt num)]
              ;; (println "nl" num (filter #(util/connected? num %) others))
              (reduce (fn [mm onum]
                        ;; (println "\t" num onum)
                        (let [onum-int (Integer/parseInt onum)]
                          (if (> onum-int num-int)
                            mm
                            (update-in! mm [num-int] (fnil conj []) onum-int))))
                      m
                      (filter #(util/connected? num %) others))))
          start
          coll))


(defn build-graph [numbers]
  (let [number-strings (map str numbers)
        length-map (time (group-by count number-strings))
        foo (mapv (fn [[l v]] (println l (count v)) l) length-map)
        tmap (time (reduce #(build-node-reduction %1 number-strings length-map %2) (transient {}) [-1 0]))]
    
    [(persistent! tmap)
     #_[(filter #(.contains % "0") number-strings)
      (filter #(.endsWith % "1") number-strings)]
     ]))

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
  (let [all (set primes)]
    (loop [todo primes
           roots {2 2}
           unrelated #{}]
      (if (empty? todo)
        [(sets/difference all unrelated) unrelated]
        (let [val (first todo)
              nodes (graph val)
              lowest-root (if nodes (apply min (map #(lookup-root roots %) nodes)) val)]
          ;; (println val nodes lowest-root)
          ;; (println roots)
          (recur
           (rest todo)
           (reduce (fn [m n] (update-root m n lowest-root)) roots (cons val nodes))
           (if (= 2 lowest-root) unrelated (conj unrelated val))))))))

(defn e425 [n]
  (let [primes (util/primes-up-to (int n))
        [graph #_[has-zero ends-one]] (time (build-graph primes)) 
        [related unrelated] (partition-relatives primes graph)]
    ;; (println "graph" graph)
    #_(do 
      (println (sort has-zero))
      (println (sort ends-one))
      (println (sort unrelated)))
    [(reduce + unrelated) unrelated]))

(time (e425 200))
(time (e425 (Math/pow 10 3)))
(time (e425 (Math/pow 10 4)))
(time (first (e425 (Math/pow 10 5))))

;; (let [tm (java.util.TreeMap. {3 :blart 1 :foo})
;;       [k v] (.pollFirstEntry tm)]
;;   (println "k" k "v" v "tm" tm))
