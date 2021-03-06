(ns euler.util-test
  (:use clojure.test
        euler.util))

(deftest prime-test
  (testing "Primes"
    (is (= [2 3 5 7 11 13 17 19] (vec (take 8 primes))))))

(deftest sievinator-test
  (let [{:keys [primes-up-to factors prime?]} (sievinator)]
    (testing "Primes"
      (is (= [2 3 5 7 11 13 17 19 23] (vec (primes-up-to 26))))
      (is (= [29 31 37 41 43 47 53 59] (vec (drop 9 (primes-up-to 60)))))
      (is (= [29 31 37 41 43 47 53 59] (vec (drop 9 (primes-up-to 60)))))
      (is (= [61 67 71 73 79] (vec (drop 17 (primes-up-to 80)))))
      )
    (testing "Factors"
      (is (= [2 2 5 5] (factors 100)))
      (is (= [7 7] (factors 49)))
      (is (= [17] (factors 17)))
      (is (= [101] (factors 101)))
      (is (= [2 2 2] (factors 8)))
      )
    (testing "Prime?"
      (is (= true (prime? 2)))
      (is (= true (prime? 53)))
      (is (= true (prime? 101)))
      (is (= false (prime? 8)))
      (is (= false (prime? 51)))
      (is (= false (prime? 99)))
      )
    )
  (testing "Sequence chunking"
    (let [{:keys [wait state primes-seq]} (sievinator)
          s (primes-seq 10)]
      (wait)
      (is (= [3 5 7 11] (:primes (state))))
      (is (= 2 (first s)))
      (wait)
      (is (= [3 5 7 11] (:primes (state))))
      (is (= 3 (fnext s)))
      (wait)
      (is (= [3 5 7 11 13 17 19] (:primes (state))))
      (is (= 5 (nth s 2)))
      (wait)
      (is (= [3 5 7 11 13 17 19] (:primes (state))))
      (is (= 13 (nth s 5)))
      (wait)
      (is (= [3 5 7 11 13 17 19 23 29 31] (:primes (state))))
      (is (= 17 (nth s 6)))
      (wait)
      (is (= [3 5 7 11 13 17 19 23 29 31] (:primes (state))))
      ))
  (testing "Composite seqs"
    (let [{:keys [composites-seq factors-seq]} (sievinator)
          cs (composites-seq 10)
          fs (factors-seq cs)
          ]
      (is (= [4 6 8 9 10 12 14 15 16 18 20 21 22]
             (vec (take-while #(< % 23) cs))))
      (is (= [[4 [2 2]] [6 [2 3]] [8 [2 2 2]] [9 [3 3]] [10 [2 5]]]
             (vec (take-while #(<= (first %) 10) fs))))
      ))
  )

(deftest integer-partitions-test
  (are [n v] (is (= v (integer-partitions n)))
    1 '((1))
    2 '((2) (1 1))
    3 '((3) (2 1) (1 1 1))
    4 '((4) (3 1) (2 2) (2 1 1) (1 1 1 1))
    7 '((7) (6 1) (5 2) (5 1 1) (4 3) (4 2 1) (4 1 1 1) (3 3 1) (3 2 2) (3 2 1 1) (3 1 1 1 1) (2 2 2 1) (2 2 1 1 1) (2 1 1 1 1 1) (1 1 1 1 1 1 1))
    ))

(deftest pow-test
  (testing "log2 based pow smartness"
    (are [x n y] (is (= y (pow x n)))
      2 7 128
      2 8 256
      2 9 512
      2 10 1024
      2 11 2048
      2 12 4096
      2 100 1267650600228229401496703205376N
      3 1 3
      3 2 9
      3 3 27
      3 4 81
      3 10 59049
      )))
