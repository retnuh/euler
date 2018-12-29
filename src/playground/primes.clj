(ns playground.primes
  )

(use '[clojure.string :only (split triml)])
(require '[clojure.set :as sets])

(defn sieve-candidate [[primes-map remaining highest :as cur] c]
  #_(println "candidate:" c primes-map)
  (if (contains? primes-map c)
    cur
    (let [increment (* 2 c)
          composites (into #{} (filter #(= 0 (mod % c))) remaining)
          np (into primes-map (map #(vector % (= % c)) composites))
          nr (apply disj remaining c composites)
          nh (or (empty? nr) (if (contains? composites highest) (apply max nr) highest))
          ans [np nr nh]]
      (if (empty? nr)
        (reduced ans)
        ans))))

(defn sieve-odds [primes odds]
  (let [highest (apply max odds)
        candidates (range 3 (inc (Math/sqrt highest)) 2)
        ; _ (println "candidates:" candidates)
        [pm rm] (reduce sieve-candidate [primes (disj (into #{} odds) 1) highest] candidates)]
    (into pm (map #(vector % true)) rm)))

(defn partition-primes [nums]
  (let [odd-or-even (group-by even? (sort nums))
        evens (odd-or-even true)
        odds (odd-or-even false)
        prime-map (reduce (fn [m i] (assoc m i (= 2 i))) (assoc (sorted-map) 1 false) evens)]
    (if (empty? odds)
        prime-map
        (sieve-odds prime-map odds))))

#_(let [p_t (read-line)
      p (Integer/parseInt p_t)
      nums (loop [a0 p
                  nums []]
             (if (> a0 0)
               (recur (dec a0) (conj nums (Integer/parseInt (read-line))))
               nums))
      prime-map (partition-primes nums)]
    (doseq [num nums]
      (println (if (prime-map num) "Prime" "Not prime"))))
