(ns euler.util-test
  (:use clojure.test
        euler.util))

(deftest prime-test
  (testing "Primes"
    (is (= [2 3 5 7 11 13 17 19] (vec (take 8 primes))))))

(deftest sievinator-test
  (let [{:keys [state primes-up-to factors]} (sievinator)]
    (testing "Primes"
      (is (= [2 3 5 7 11 13 17 19 23] (vec (primes-up-to 26))))
      (is (= [29 31 37 41 43 47 53 59] (vec (drop 9 (primes-up-to 60)))))
      (is (= [61 67 71 73 79] (vec (drop 17 (primes-up-to 80)))))
      )
    (testing "Factors"
      (is (= [2 2 5 5] (factors 100)))
      (is (= [7 7] (factors 49)))
      (is (= [17] (factors 17)))
      (is (= [101] (factors 101)))
      )
    (testing "State")
    ))

(run-tests)
