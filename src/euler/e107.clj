(ns euler.e107
  (:import [java.io StringReader])
  (:require ;[euler.util :as util]
   [clojure.set :as sets]
   [clojure.java.io :as io]
   [clojure.string :as str]))

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

(def example-matrix-string
  "-	16	12	21	-	-	-
16	-	-	17	20	-	-
12	-	-	28	-	31	-
21	17	28	-	18	19	23
-	20	-	18	-	-	11
-	-	31	19	-	-	27
-	-	-	23	11	27	-")

(defn parse-matrix [matrix-string]
  (with-open [r (io/reader matrix-string)]
    (loop [row 1
           edges (transient [])]
      (let [line (.readLine r)]
        (if (str/blank? line)
          [(dec row) (persistent! edges)]
          (recur (inc row)
                 (second (reduce (fn [[col e] weight-str]
                                   (println weight-str)
                                   (if (or (= "-" weight-str) (> col row))
                                     [(inc col) e]
                                     (let [weight (Integer/parseInt weight-str)]
                                       [(inc col) (conj! e [weight row col])])))
                                 [1 edges]
                                 (str/split line #",\s*|\s+")))))))))

(def example-matrix (second (parse-matrix (StringReader. example-matrix-string))))

(defn e107-min-spanning-tree-kruskal [edges]
  (loop [todo (sort-by first edges)
         roots {}
         result #{}]
    (if (empty? todo)
      result
      (let [[weight a b :as edge] (first todo)
            root-a (lookup-root roots a)
            root-b (lookup-root roots b)]
        (if (= root-a root-b)
          (recur (rest todo) roots result)
          (recur (rest todo) (update-root roots a root-b) (conj result edge)))
        ))))

(defn e107-saving [edges]
  (let [total (reduce + (map first edges))
        min-tree (e107-min-spanning-tree-kruskal edges)
        mt-total (reduce + (map first min-tree))]
    (- total mt-total)))

(e107-saving example-matrix)

(def e107-problem-matrix-url "https://projecteuler.net/project/resources/p107_network.txt")
(def e107-problem-matrix-parse-results (parse-matrix e107-problem-matrix-url))

(time (e107-saving (second e107-problem-matrix-parse-results)))

