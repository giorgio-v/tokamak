(ns tokamak.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.core.matrix :as m]
            [tokamak.core :as t]
            [tokamak.gradient :as g]
            [tokamak.backends.core-matrix :as b]
            [tokamak.backends.eigen :as e])
  (:refer-clojure :exclude [compile]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(defn run-1 []
  (let [a (t/tensor :int64 2)
        b (t/tensor :int64 2)
        ;;f (t/function [a b] (t/add (t/add 2 a) b))
        f (t/function [a b] (t/exp (t/add a b)))
        grad (g/gradient f a)
        _ (pprint f)
        _ (pprint grad)
        f (b/compile f)
        grad (b/compile grad)
        ;;f (e/compile f)
        ;;grad (e/compile grad)
        ]
    #_(pprint (f (m/array [[1 1] [1 1]]) (m/array [[1 1] [1 1]])))
    #_(pprint (grad (m/array [[2 2] [2 2]]) (m/array [[1 1] [1 1]])))))

