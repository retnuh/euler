(ns euler.e104
  (:require ;[euler.util :as util]
            [clojure.set :as sets]))

(def LOG2 (Math/log10 2))

;; From http://stackoverflow.com/questions/6827516/logarithm-for-biginteger
(defn log10BigInteger [val]
  (let [blex (- (.bitLength val) 1022)
        nval (if (> blex 0) (.shiftRight val blex) val)
        res (Math/log10 (.doubleValue nval))]
    (if (> blex 0) (+ res (* blex LOG2)) res)))

(log10BigInteger 100N)

;; public static double logBigInteger(BigInteger val) {
;;     int blex = val.bitLength() - 1022; // any value in 60..1023 is ok
;;     if (blex > 0)
;;         val = val.shiftRight(blex);
;;     double res = Math.log(val.doubleValue());
;;     return blex > 0 ? res + blex * LOG2 : res;
;; }

(defn fib [n]
  (loop [tot 0
         prev 1
         i 1]
    (if (= i n)
      (+' tot prev)
      (recur (+' tot prev) tot (inc i)))))

(defn fibb [most]
  (let [set-fn (comp set seq str)
        digits (set-fn 987654321)]
    (loop [tot 0N prev 1N i 1]
      ;; (when (= 0 (mod i 1000)) (println i))
      (let [v (+' tot prev)
            bvm (mod v 1000000000)
            bpd (= digits (set-fn bvm))]
        ;; (when (> i 2745) (println i v bvm tv (set tvm) tpd))
        (if bpd
          (let [tvm (take 9 (seq (str v)))
                tpd (and tvm (= digits (set tvm)))]
            (if (or (and bpd tpd) (> i most))
              (do (println i  bvm tvm bpd tpd) [i v])
              (recur v tot (inc i))))
          (recur v tot (inc i)))))))

(defn fib-eval [f]
  (loop [tot 0
         prev 1
         i 1]
    (let [v (+' tot prev)]
      (if (f i v)
        [i v]
        (recur v tot (inc i))))))

(fib-eval (fn [i v] (= i 10)))

(map fib (range 1 11))

(println  (fib 541))

(println (take 10 (seq (str (fib 2749)))))

(fib 10)

; (println ) (first  (fib-eval (fn [i v] (= (set (range 1 10)) (take-last 9 (seq (str v)))))))

(println (first (time (fibb 1000000))))

