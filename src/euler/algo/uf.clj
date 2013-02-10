(ns euler.algo.uf
  (:use clojure.algo.monads))

(with-monad state-m
  (defn get-or-add [key]
    (fn [s]
      (if-let [v (get s key)]
        [v s]
        [[key 1] (assoc s key [key 1])])))


  (defn root [key]
    (domonad
      [[r c :as res] (update-val key (fn [cur] ))
       :if (= r key)
       :then [ret (m-result res)]
       :else [ret (root r)
              _ (set-val key [(first ret) -1])]]
      ret))

  (defn- join [[childKey childCount :as c] [parentKey parentCount :as p]]
    (fn [s]
      (let [newParent [parentKey (+ parentCount childCount)]
            newChild [parentKey -1]]
        [newParent (assoc s childKey newChild parentKey newParent)])))

  (defn union [a b]
    (domonad
      [[ra ca :as rootA] (root a)
       [rb cb :as rootB] (root b)
       :if (= ra rb)
       :then [res (m-result nil)]
       :else [
        :if (<= ca cb)
        :then [res (join rootA rootB)]
        :else [res (join rootB rootA)]]]
      res))

  (defn connected? [a b]
    (domonad
      [[ra _] (root a)
       [rb _] (root b)]
      (= ra rb)))

  (defn component-size [a]
    (domonad [[ra ca] (root a)] ca))

  )