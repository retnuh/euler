(ns euler.e109)

(def multiplier {:s 1 :single 1 :d 2 :double 2 :t 3 :triple 3})

(defn value [[mult v]] (* (multiplier mult) v))

(def single-darts (conj (mapv #(vector :single %) (range 1 21)) [:single 25]))
(def double-darts (conj (mapv #(vector :double %) (range 1 21)) [:double 25]))
(def triple-darts (mapv #(vector :triple %) (range 1 21)))

(def all (vec (concat single-darts double-darts triple-darts)))

(def no-dup-pairs
  (let [l (count all)]
    (for [i (range l) j (range i l)]
      [(get all i) (get all j)])))

(def value-map
  (let [m1 (reduce (fn [m dart] (update-in m [(value dart)] (fnil conj []) dart)) {} all)]
    (reduce (fn [m [d1 d2]] (update-in m [(+ (value d1) (value d2))] (fnil conj []) [d1 d2]))
            m1 no-dup-pairs)))

(defn checkouts-for-target-number [n]
  (reduce (fn [t dub]
            (let [diff (- n (value dub))]
              ;; (println "checking" n dub diff (count (value-map diff)))
              (cond (zero? diff) (reduced (inc t))
                    (neg? diff) (reduced t)
                    :else (+ t (count (value-map diff)))))) 0 double-darts))

(checkouts-for-target-number 6)

(defn e109 []
  (reduce #(+ %1 (checkouts-for-target-number %2)) 0 (range 2 100)))

(println (time (e109)))
