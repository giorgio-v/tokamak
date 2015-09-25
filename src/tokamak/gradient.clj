(ns tokamak.gradient
  (:require [clojure.set :as s]
            [tokamak.core :refer :all]
            [tokamak.ops :refer [IOp]]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [vector]))

(defprotocol IDiff
  (-gradient [this dx]))

(defrecord Grad [name args] IOp)

(defn grad [arg dx]
  (operation ->Grad [arg dx]
             (keyword (str "d" (name arg) "_d" (name dx)))))

(defn gradient [{:keys [graph args ret]} {dx :name}]
  (loop [grad-ret (grad ret dx)]
    (if-let [grad-node (->> (:graph grad-ret) vals
                            (filter #(= (type %) Grad))
                            first)]
      (recur
       (update-in grad-ret [:graph]
                  (fn [grad-graph]
                    (let [{grad-name :name [node-name dx] :args} grad-node
                          {:keys [name graph]} (-gradient (graph node-name) dx)]
                      (merge grad-graph
                             (graph/rename-node graph name grad-name))))))
      (function (map graph args)
                (update-in grad-ret [:graph] merge graph)))))

(extend-protocol IDiff

  tokamak.core.Tensor
  (-gradient [this dx]
    (if (= (:name this) dx)
      (ones this)
      (zeros this)))

  tokamak.ops.Add
  (-gradient [this dx]
    ;; todo: replace keyword?
    (apply add (map #(grad % dx) (filter keyword? (:args this)))))

  tokamak.ops.Mul
  (-gradient [this dx]
    (let [arg-set (into #{} (:args this))]
      (apply add (map (fn [arg]
                        (apply mul (conj (s/difference arg-set arg)
                                         (grad arg dx))))
                      (filter keyword? (:args this))))))

  tokamak.ops.Exp
  (-gradient [this dx]
    (let [[arg] (:args this)]
      (mul (exp arg) (grad arg dx)))))

