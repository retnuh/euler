(ns euler.e103-test
  (:use clojure.test
        euler.e103))

(deftest the-tests
  (is (= [2 3 4] (e103 3 2)))
  (is (= [3 5 6 7] (e103 4 3)))
  (is (= [6 9 11 12 13] (e103 5 5)))
  (is (= [11 18 19 20 22 25] (e103 6 11 [11, 17, 20, 22, 23, 24])))
  )

(run-tests)
