(ns tokamak.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.core.matrix :as m]
            [tokamak.core :as t]
            [tokamak.gradient :as g]
            [tokamak.backends.core-matrix :as b])
  (:refer-clojure :exclude [compile]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


(defn run-1 []
  (let [a (t/tensor :int64 2 :a)
        b (t/tensor :int64 2 :b)
        ;;f (t/function [a b] (t/exp (t/add 1 a b)))
        ;;f (t/function [a b] (t/add (t/named a :aa) b))
        f (t/function [a b] (t/add (t/add 2 a) b))
        grad (g/gradient f a)
        _ (pprint f)
        _ (pprint grad)
        f (b/compile f)
        grad (b/compile grad)
        ]
    (pprint (f (m/array [[1 1] [1 1]]) (m/array [[1 1] [1 1]])))
    (pprint (grad (m/array [[2 2] [2 2]]) (m/array [[1 1] [1 1]])))))

(defn run-2 []

  (let [a (t/tensor :int64 2 :a)
        b (t/matrix :int64 :b)
        c (t/add a b)
        e (t/named (t/mul a b c) :e)
        d (t/add a e)]
    (pprint (t/mul a c 2 (t/exp d)))

    (b/compile (t/function [a b] d))

    ))
