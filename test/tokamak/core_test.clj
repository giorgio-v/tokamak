(ns tokamak.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.core.matrix :as m]
            [tokamak.core :as t]
            [tokamak.backends.core-matrix :as b])
  (:refer-clojure :exclude [compile]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


(defn run-1 []
  (let [a (t/tensor :int64 2 :a)
        b (t/tensor :int64 2 :b)
        f (b/compile (t/function [a b] (t/exp (t/+ a b))))]
    (pprint (f (m/array [[1 1] [1 1]]) (m/array [[1 1] [1 1]])))))

(defn run-2 []

  (let [a (t/tensor :int64 2 :a)
        b (t/matrix :int64 :b)
        c (t/+ a b)
        e (t/named (t/* a b c) :e)
        d (t/+ a e)]
    (pprint (t/* a c 2 (t/exp d)))

    (b/compile (t/function [a b] d))

    ))
