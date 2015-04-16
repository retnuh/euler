(ns euler.util-test
  (:use clojure.test
        euler.util))

(deftest prime-test
  (testing "Primes"
    (is (= [2 3 5 7 11 13 17 19] (vec (take 8 primes))))))

(deftest connected-test
  (testing "same length, one mutation"
    (are [x y] (false? (connected? x y))
         2 2
         1001 1111)
    (are [x y] (true? (connected? x y))
         2 3
         3 2
         3 9
         1001 1011
         1001 1101
         ))
  (testing "different length, in front"
    (are [x y] (false? (connected? x y))
         2 13
         1001 10001
         1001 11101
         )
    (are [x y] (true? (connected? x y))
         2 12
         2 22
         2 32
         12 2
         22 2
         32 2
         1001 11001
         1001 21001
         ))
  (testing "different length, in back"
    (are [x y] (false? (connected? x y))
         2 31
         1001 10001
         1001 10111
         2 23
         2 21
         1001 10011
         1001 10012
         ))
  (testing "different length, too far apart"
    (are [x y] (false? (connected? x y))
         2 113
         113 2
         2 103
         103 2))
  )

(run-tests)
