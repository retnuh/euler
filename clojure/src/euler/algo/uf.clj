(ns euler.algo.uf
  (:use clojure.algo.monads
        euler.util.monads))

(with-monad state-m

  (defn root [key]
    (domonad
      [[r c :as res] (fetch-val-default key [key 1])
       :if (= r key)
       :then [ret (m-result res)]
       :else [ret (root r)
              ; updating the lookup key to flatten tree gives us ln* behavior.  Sweet!
              _ (set-val key [(first ret) -1])]]
      ret))

  (defn union [a b]
    (domonad
      [[ra ca :as rootA] (root a)
       [rb cb :as rootB] (root b)
       :if (= ra rb)
       :then [res (m-result nil)]
       :else [
        :if (<= ca cb)
        :then [_ (set-val ra [rb -1])
               _ (set-val rb [rb (+ cb ca)])
               res (fetch-val rb)]
        :else [_ (set-val rb [ra -1])
               _ (set-val ra [ra (+ cb ca)])
               res (fetch-val ra)]]]
      res))

  (defn connected? [a b]
    (domonad
      [[ra _] (root a)
       [rb _] (root b)]
      (= ra rb)))

  (defn component-size [a]
    (domonad [[ra ca] (root a)] ca))

  )
