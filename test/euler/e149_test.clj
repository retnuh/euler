(ns euler.e149-test
  (:use clojure.test
        euler.e149))

(deftest diag-generator-test
  (testing "A 2x2"
    (is (= (seq [[[0 0] [1 1]]
                 [[1 0]]
                 [[0 1]]])
          (generate-diagonals 2 2))))
  (testing "A 3x3"
    (is (= (seq [[[0 0] [1 1] [2 2]]
                 [[1 0] [2 1]]
                 [[2 0]]
                 [[0 1] [1 2]]
                 [[0 2]]])
          (generate-diagonals 3 3)))))

(deftest antidiag-generator-test
  (testing "A 2x2"
    (is (= (seq [[[0 0]]
                 [[1 0] [0 1]]
                 [[1 1]]])
          (generate-anti-diagonals 2 2))))
  (testing "A 3x3"
    (is (= (seq [[[0 0]]
                 [[1 0] [0 1]]
                 [[2 0] [1 1] [0 2]]
                 [[2 1] [1 2]]
                 [[2 2]]])
          (generate-anti-diagonals 3 3)))))

(deftest row-generator-test
  (testing "A 3x3"
    (is (= (seq [[[0 0] [1 0] [2 0]]
                 [[0 1] [1 1] [2 1]]
                 [[0 2] [1 2] [2 2]]])
          (generate-rows 3 3)))))

(deftest col-generator-test
  (testing "A 3x3"
    (is (= (seq [[[0 0] [0 1] [0 2]]
                 [[1 0] [1 1] [1 2]]
                 [[2 0] [2 1] [2 2]]])
          (generate-cols 3 3)))))

(deftest e149-test)

(deftest lagged-fibonacci-generator-test
  (is (= (nth (lagged-fibonacci-seq 10) 9) -393027))
  (is (= (nth (lagged-fibonacci-seq 100) 99) 86613)))

(def e149-sample-grid
  [[-2 5 3 2]
   [9 -6 5 1]
   [3 2 7 3]
   [-1 8 -4 8]])

(deftest p149-sample-test
  (is (= 16 (p149 e149-sample-grid))))

(deftest go-test
  (is (< 0 (e149))))

(run-tests)