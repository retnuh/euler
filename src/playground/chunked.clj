(ns playground.chunked)

(defn my-range
  ([n] (let [cur (atom 0)] [cur (my-range cur n)]))
  ([cur n]
   (if (= @cur n)
     nil
     (cons @cur (lazy-seq (swap! cur inc) (my-range cur n))))))

(defmacro check-highest
  [n coll-sym & body]
  `(let [[h# ~coll-sym] (my-range ~n)]
     ~@body
     (println "highest:" @h#)
     @h#))

(check-highest 10 s (dorun (take 5 s)))

(defn some-stuff ([coll] (eduction (filter even?) (map inc) coll)))

;; This computes up to 8
(check-highest 100 s (transduce (take 5) + (some-stuff s)))

;; These compute up to 64 b/c chunked-iter
(check-highest 100 s (transduce identity + 0 (take 5 (some-stuff s))))
(check-highest 100 s (reduce + 0 (take 5 (some-stuff s))))
