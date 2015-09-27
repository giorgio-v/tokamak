(ns tokamak.core-test
  (:require [clojure.test :refer :all]
            [fipp.clojure :refer [pprint]]
            [clojure.core.matrix :as m]
            [tokamak.core :refer :all]
            [tokamak.ops :refer :all]
            [tokamak.gradient :refer :all]
            [tokamak.backends.core-matrix :as b]
            [tokamak.backends.eigen :as e]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(defn run-1 []
  (let [a (tensor :int64 2)
        b (tensor :int64 2)
        f (function [a b] (exp (add a b)))
        grad (gradient f a)
        _ (pprint f)
        _ (pprint grad)
        f* (b/compile f)
        grad* (b/compile grad)
        f** (e/compile f)
        grad** (e/compile grad)
        ]
    (pprint (f* (m/array [[1 1] [1 1]]) (m/array [[1 1] [1 1]])))
    (pprint (grad* (m/array [[2 2] [2 2]]) (m/array [[1 1] [1 1]])))))

