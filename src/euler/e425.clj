(ns euler.e425
  (:require [euler.util :as util]
            [clojure.set :as sets]))

(defn build-node-reduction [start coll length-map offset]
  (reduce (fn [m num]
            (let [others (get length-map (+ offset (count num)))]
              ;; (println "nl" num (filter #(util/connected? num %) others))
              (reduce (fn [mm onum]
                        ;; (println "\t" num onum)
                        (update-in mm [(Integer/parseInt num)]
                                   (fnil conj []) (Integer/parseInt onum)))
                      m
                      (filter #(util/connected? num %) others))))
          start
          coll))

(defn build-graph [numbers]
  (let [number-strings (map str numbers)
        length-map (group-by count number-strings)]
    [(reduce #(build-node-reduction %1 number-strings length-map %2) {} [-1 0 1])
     (filter #(.contains % "0") number-strings)
     (filter #(.endsWith % "1") number-strings)
     ]))

(defn partition-relatives [graph start]
  (let [all (set (keys graph))
        queue (java.util.TreeMap. {start -1})]
    (loop [seen #{start}
           unrelated all]
      (if (empty? queue)
        [(sets/difference all unrelated) unrelated]
        (let [[val mx] (.pollFirstEntry queue)
              nodes (graph val)
              unseen (remove seen nodes)
              nmx (max val mx)]
          ;; (println val mx nmx unseen)
          (dorun (for [node unseen]
                   (.put queue node nmx)))
          (recur
           (apply conj seen val unseen)
           (if (> val mx)
             (disj unrelated val)
             unrelated)))))))

(defn e425 [n]
  (let [[graph has-zero ends-one] (build-graph (util/primes-up-to (int n))) 
        [related unrelated] (partition-relatives graph 2)]
    #_(do 
      (println (sort has-zero))
      (println (sort ends-one))
      (println (sort unrelated)))
    [(reduce + unrelated) unrelated]))

(time (e425 (Math/pow 10 3)))
(time (e425 (Math/pow 10 4)))
;; (time (first (e425 (Math/pow 10 5))))

;; (let [tm (java.util.TreeMap. {3 :blart 1 :foo})
;;       [k v] (.pollFirstEntry tm)]
;;   (println "k" k "v" v "tm" tm))
