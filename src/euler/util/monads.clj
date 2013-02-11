(ns euler.util.monads)

(defn fetch-val-default
  "Return a state-monad function that assumes the state to be a map and
   returns the value corresponding to the given key. If the value is nil, default is installed in the state.
   If the value is non nil, the state is not modified."
  [key default]
  (fn [s]
    (if-let [v (get s key)]
      [v s]
      [default (assoc s key default)])))
