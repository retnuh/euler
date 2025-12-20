(ns euler.e191
  (:require [euler.util :as util]
            [clojure.core.match :as m]
            [clojure.set :as sets]))

(defn gen-next-day [s]
  (map #(str s %) ["A" "O" "L"]))

(defn valid? [s]
  (not (re-find #"(?:L.*L)|(?:AAA)" s)))

(valid? "OOO")
(valid? "LOL")
(valid? "OAAA")
(valid? "AAOA")

(defn gen-valid-day []
  (comp (mapcat gen-next-day)
        (filter valid?)))

;; (gen-valid-day "")

;; (gen-valid-day "LAA")

(defn counter
  ([] 0)
  ([t i] (inc t)))

(defn count-valid-days [n]
  (loop [days-xf identity i n]
    (if (zero? i)
      (transduce days-xf (completing counter) [""])
      (recur (comp (gen-valid-day) days-xf) (dec i)))))

(defn gen-valid-days [n]
  (loop [days-xf identity i n]
    (if (zero? i)
      (into [] days-xf [""])
      (recur (comp (gen-valid-day) days-xf) (dec i)))))

(count-valid-days 10)
(println (gen-valid-days 2))

(into [] (gen-valid-day) [""])

(into [] (comp (gen-valid-day) (gen-valid-day)) [""])


;; (time (println (count-valid-days 30)))
;; Too slow!

(defn gen-next-states [[[l a] c]]
  [[[(inc l) 0] c], [[l (inc a)] c],[[l 0] c]])

(gen-next-states [[0 0] 1])

(defn valid-state? [[[l a] c]]
  ;; (println l a c (< l 2) (< a 3))
  (and (< l 2) (< a 3)))

(defn gen-states []
  (partial transduce
           (comp (mapcat gen-next-states) (filter valid-state?) (map (partial apply hash-map)))
           (completing (fn ([] {}) ([m s] (merge-with + m s))))))

(defn valid-states [n]
  (loop [days-xf identity i n]
    (if (zero? i)
      (days-xf [[[0 0] 1]])
      (recur (comp (gen-states) days-xf) (dec i)))))

(defn count-valid-states [n]
  (apply + (vals (valid-states n))))

(count-valid-states 10)
(println (valid-states 2))

((gen-states) [[[1 2] 1] [[1 2] 3] [[1 1] 4]])

(into [] (comp (mapcat gen-next-states)
               (filter valid-state?)
               (map (partial apply hash-map)))  [[[1 2] 1]])

;; (time (println (count-valid-states 30)))
;; 1918080160
;; "Elapsed time: 1.883512 msecs"


