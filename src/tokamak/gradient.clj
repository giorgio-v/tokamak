(ns tokamak.gradient
  (:require [clojure.set :as s]
            [tokamak.core :refer :all]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [vector]))

(defn grad [arg dx]
  (operation :grad [arg dx] (keyword (str "d" (name arg) "_d" (name dx)))))

(defmulti node-gradient (fn [v _] (or (:op v) (:type v))))

(defn gradient [{:keys [graph args ret]} {dx :name}]
  (loop [grad-ret (grad ret dx)]
    (if-let [grad-node (->> (:graph grad-ret) vals
                            (filter #(= (:op %) :grad))
                            first)]
      (recur
       (update-in grad-ret [:graph]
                  (fn [grad-graph]
                    (let [{grad-name :name [node-name dx] :args} grad-node
                          {:keys [name graph]} (node-gradient (graph node-name) dx)]
                      (merge grad-graph
                             (graph/rename-node graph name grad-name))))))
      (function (map graph args)
                (update-in grad-ret [:graph] merge graph)))))

(defmethod node-gradient :add [{args :args} dx]
  (apply add (map #(grad % dx) (filter keyword? args))))

(defmethod node-gradient :mul [{args :args} dx]
  (let [arg-set (into #{} args)]
    (apply add (map (fn [arg]
                      (apply mul (conj (s/difference arg-set arg)
                                       (grad arg dx))))
                    (filter keyword? args)))))

(defmethod node-gradient :exp [{[arg] :args} dx]
  (mul (exp arg) (grad arg dx)))

(defmethod node-gradient :tensor [node dx]
  (if (= (:name node) dx)
    (ones node)
    (zeros node)))

