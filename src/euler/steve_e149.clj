(ns euler.steve-e149)

(defn lfg [m]
  (let [sa (into [0] (map (fn [k] (- (rem (+ 100003 (- (* 200003 k)) (* 300007 k k k)) 1000000) 500000)) (range 1 56)))]
    (reduce
      (fn [s k]
        (conj s (- (rem (+ (nth s (- k 24)) (nth s (- k 55)) 1000000) 1000000) 500000)))
      sa (range 56 (inc m)))))

(defn table [x s]
  (let [p (fn [r c] (nth s (+ c (* x r))))]
    {:row (fn [r] (map #(p r %) (range 0 x)))
     :col (fn [c] (map #(p % c) (range 0 x)))
     :diag (fn [d] (if (> x d)
                     (map #(p % (- d %)) (range 0 (inc d)))
                     (map #(p (- d %) %) (range (inc (- d x)) x))))
     :adiag (fn [a] (if (> x a)
                      (map #(p % (+ % (- x a 1))) (range 0 (inc a)))
                      (map #(p % (+ % (- x a 1))) (range (inc (- a x)) x))))
     }))

(defn max-run [v]
  (first
    (reduce (fn [[m s] e]
              (let [ns (+ s e)]
                [(if (> ns m) ns m) (if (pos? ns) ns 0)]))
      [0 0] v)))

(time
  (println
    (let [x 2000
          s (subvec (lfg (* x x)) 1)
          {:keys [row col diag adiag]} (table x s)]
      (apply max (map max-run (concat
                                (map row (range 0 x))
                                (map col (range 0 x))
                                (map diag (range 0 (dec (+ x x))))
                                (map adiag (range 0 (dec (+ x x))))
                                ))))))

