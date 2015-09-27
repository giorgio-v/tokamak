(ns tokamak.gradient
  (:require [clojure.set :as s]
            [tokamak.core :as t]
            [tokamak.ops :refer :all]
            [tokamak.graph :as graph]))

(defprotocol IDiff
  (-gradient [this wrt]))

(defrecord Grad [name arg wrt]
  IOp
  (-args [_] [arg wrt]))

(defn dname [x wrt]
  (keyword (str "d" (name x) "_d" (name wrt))))

(defn grad [x wrt]
  (let [x (or (:name x) x)]
    (t/variable (Grad. (dname x wrt) x wrt) {})))

(defn gradient [{:keys [graph args ret]} {wrt :name}]
  (loop [grad-ret (grad ret wrt)]
    (if-let [grad-node (->> (:graph grad-ret) vals
                            (filter #(= (type %) Grad))
                            first)]
      (recur
       (update-in grad-ret [:graph]
                  (fn [grad-graph]
                    (let [{grad-name :name node-name :arg wrt :wrt} grad-node
                          {:keys [name graph]} (-gradient (graph node-name) wrt)]
                      (merge grad-graph
                             (graph/rename-node graph name grad-name))))))
      (t/function (map graph args)
                (update-in grad-ret [:graph] merge graph)))))

(extend-protocol IDiff

  tokamak.core.Tensor
  (-gradient [this wrt]
    (if (= (:name this) wrt)
      (t/constant 1)
      (t/constant 0)))

  tokamak.core.Constant
  (-gradient [this wrt]
    (t/constant 0))

  tokamak.ops.Add
  (-gradient [this wrt]
    (apply add (map #(grad % wrt) (var-args this))))

  tokamak.ops.Mul
  (-gradient [this wrt]
    (let [arg-set (into #{} (-args this))]
      (apply add (map (fn [arg]
                        (apply mul (conj (s/difference arg-set arg)
                                         (grad arg wrt))))
                      (var-args this)))))

  tokamak.ops.Exp
  (-gradient [this wrt]
    (mul (exp (:x this)) (grad (:x this) wrt))))

