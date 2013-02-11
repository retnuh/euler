(ns euler.algo.uf-test
  (:use clojure.test
        clojure.algo.monads
        euler.algo.uf))

(with-monad state-m

  (deftest nav-ops
    (testing "root non-existing elt"
      (let [uf (domonad [a (root :a )] a)]
        (is (= '([:a 1] {:a [:a 1]}) (uf {})))
        )
      )
    (testing "root existing elt"
      (let [m {:a [:a 3]}
            uf (domonad [a (root :a )] a)]
        (is (= [[:a 3] m] (uf m)))
        )
      )
    (testing "child and parent exist elts"
      (let [m {:a [:b -1], :b [:b 2]}
            uf (domonad [a (root :a )] a)]
        (is (= [[:b 2] m] (uf m)))
        )
      )
    (testing "reading grandchild reparents to root"
      (let [m {:a [:b -1], :b [:c -1], :c [:d -1], :d [:d 4]}
            uf (domonad [a (root :a )] a)]
        (is (= [[:d 4] {:a [:d -1], :b [:d -1], :c [:d -1], :d [:d 4]}] (uf m)))
        )
      )
    (testing "component size"
      (let [m {:a [:b -1], :b [:b 2]}
            uf (domonad [a (component-size :a )] a)]
        (is (= [2 m] (uf m)))
        )
      )
    )

  (deftest union-ops
    (testing "union two non existant elts"
      (let [uf (domonad [u (union :a :b )] u)]
        (is (= [[:b 2] {:a [:b -1], :b [:b 2]}] (uf {})))
        )
      )
    (testing "union one non existant elt"
      (let [uf (domonad [u (union :a :a )] u)]
        (is (= [nil {:a [:a 1]}] (uf {})))
        )
      )
    (testing "union two children common parent"
      (let [uf (domonad [ac (union :a :c ) bc (union :b :c ) ab (union :a :b )] ab)]
        (is (= [nil {:a [:c -1] :b [:c -1] :c [:c 3]}] (uf {})))
        )
      )
    (testing "connected two children common parent"
      (let [uf (domonad [ac (union :a :c ) bc (union :b :c ) ab (connected? :a :b )] ab)]
        (println (uf {}))
        (is (= [true {:a [:c -1] :b [:c -1] :c [:c 3]}] (uf {})))
        )
      )
    (testing "connected two non-existant elts"
      (let [uf (domonad [ab (connected? :a :b )] ab)]
        (is (= [false {:a [:a 1] :b [:b 1]}] (uf {})))
        )
      )
    )

  )

;(defn test-ns-hook []
;  (foo))

(run-tests)
