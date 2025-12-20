(ns euler.e149)

(defn- within? [w h x y]
  (and (>= x 0) (>= y 0) (< x w) (< y h)))

(defn- diag [w h fx fy [x y]]
  (when (within? w h x y)
    (lazy-seq (cons [x y] (diag w h fx fy [(fx x) (fy y)])))))

(defn generate-diagonals [w h]
  (->> (lazy-cat (map-indexed #(vector %1 %2) (repeat w 0)) (map-indexed #(vector %2 (inc %1)) (repeat (dec h) 0)))
    (map #(diag w h inc inc %))
    ))

(defn generate-anti-diagonals [w h]
  (->> (lazy-cat (map-indexed #(vector %1 %2) (repeat w 0)) (map-indexed #(vector %2 (inc %1)) (repeat (dec h) (dec w))))
    (map #(diag w h dec inc %))
    ))

(defn generate-rows [w h]
  (map (fn [col] (map-indexed #(vector %1 %2) (repeat w col))) (range h)))

(defn generate-cols [w h]
  (map (fn [row] (map-indexed #(vector %2 %1) (repeat h row))) (range w)))

(defn lagged-fibonacci-seq [n]
  (loop [i 1 v (transient [])]
    (if (> i n)
      (persistent! v)
      (let [x (if (< i 56)
        (+ 100003 (* -1 200003 i) (* 300007 i i i))
        (+ (get v (- i 25)) (get v (- i 56)) 1000000))]
      (conj! v (- (mod x 1000000) 500000))
        (recur (inc i) v)))))

(defn p149 [grid]
  (let [h (count grid)
        w (count (first grid))
        runs (lazy-cat (generate-rows w h) (generate-cols w h) (generate-diagonals w h) (generate-anti-diagonals w h))]
    (apply max
      (map first
        (map
          #(reduce
             (fn [[m c] i]
               (let [t (+ c  (get-in grid (reverse i)))]
                 (cond
                   (< t 0) [m 0]
                   (> t m) [t t]
                   :else [m t])))
             [0 0] %)
          runs)))))

(defn e149 []
  (let [p (partition 2000 (lagged-fibonacci-seq 4000000))
        grid (vec (map vec p))]
    (p149 grid)))

; (time (println (e149)))
