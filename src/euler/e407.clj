(ns euler.e407
  (:require [euler.util :as util]
            [clojure.set :as sets]))

(defn a-squared-mod-n [a n]
  (mapv #(mod (* % %) n)) (range n))
