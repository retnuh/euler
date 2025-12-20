(ns euler.e111
  (:require [euler.util :as util])
  (:require [clojure.string :refer [join]]))

(defn first-digits-except [d]
  (map str (remove #{d} (range 1 10))))

(defn digits-except [d]
  (map str  (remove #{d} (range 10))))


(defn generate-runs-of-length
  ([digit total-length run-length]
   (generate-runs-of-length digit total-length run-length true))
  ([digit total-length run-length first?]
   (cond
     (and (= 1 total-length) (zero? run-length)) (digits-except digit)
     (zero? run-length) (do ;;(println "zero" digit total-length run-length)
                            (for [de ((if first? first-digits-except digits-except) digit)
                                  r (generate-runs-of-length digit (dec total-length) 0 false)]
                             (str de r)))
     (= total-length run-length)
     (do
       #_(println "terminate" digit total-length run-length (join "" (repeat total-length digit)))
       (vector (join "" (repeat total-length digit))))
     :else
     (let [srl (generate-runs-of-length digit (dec total-length) run-length false)
           same-run-length (for [de ((if first? first-digits-except digits-except) digit) r srl]
                             (str de r))
           drl (generate-runs-of-length digit (dec total-length) (dec run-length) false)
           diff-run-length (map #(str digit %) drl)]
       ;; (println "recur" digit total-length run-length srl same-run-length drl diff-run-length)
       (concat same-run-length diff-run-length)))))

(println "foo" (generate-runs-of-length 0 4 2))

(defn prime-runs-of-length [digit l run-length]
  (->> (generate-runs-of-length digit l run-length)
;;       (map #(do (println %) %))
       (remove #(.startsWith % "0"))
       (map #(Long/parseLong %))
       (filter util/prime?)))

(defn max-run-primes-with
  ([digit l] (max-run-primes-with digit l (dec l)))
  ([digit l run-length]
   (let [primes (prime-runs-of-length digit l run-length)]
     (if-not (zero? (count primes))
       primes
       (recur digit l (dec run-length))))))

(let [p (max-run-primes-with 5 5)
      missing (remove (set p) (runs-five 5))]
  (println (count p) (reduce + p) p)
  (println "missing" missing))

(defn max-run-primes-of-length [l]
  (->> (range 10)
      (mapcat #(max-run-primes-with % l))))

(def four-digit-primes
  (->> util/pprimes
       (drop-while #(< % 1000))
       (take-while #(<= % 9999))))

(def five-digit-primes
  (->> util/pprimes
       (drop-while #(< % 10000))
       (take-while #(<= % 99999))))

(count four-digit-primes)

(defn digit-counts [num]
  (->> num
       str
       seq
       frequencies
       (map (fn [[k v]] (vector (- (int k) (int \0)) v)))))

(digit-counts 1134511345)

(defn all-runs [nseq]
  (let [max-so-far (vec (repeat 10 0))]
    (reduce (fn [cum prime]
              (reduce (fn [[runs msf :as cum1] [d c]]
                        (cond (> c (msf d)) [(assoc runs d [prime]) (assoc msf d c)]
                              (= c (msf d)) [(update-in runs [d] conj prime) msf]
                              :else cum1)) cum (digit-counts prime)))
            [{} max-so-far] nseq)))

;; (all-runs four-digit-primes)

(defn mns-s [[digit-map max-count]]
  (let [mns (reduce (fn [m d]
                      (assoc m d [(max-count d) (count (digit-map d)) (reduce + 0 (digit-map d))]))
                    {} (range 10))]
    [mns (reduce + (map last (vals mns)))]))

;; (mns-s (all-runs four-digit-primes))

(def runs-five (first (all-runs five-digit-primes)))
(def mnss-five (mns-s (all-runs five-digit-primes)))

(def runs-five-gen (first (all-runs (max-run-primes-of-length 5))))
(def mnss-five-gen (mns-s (all-runs (max-run-primes-of-length 5))))

(= mnss-five mnss-five-gen)


(time (let [run (all-runs (max-run-primes-of-length 10))
            [mns s] (mns-s run)]
        (dorun (for [i (range 10)]
                 (println i (mns i))))
        (println "total: " s)))
