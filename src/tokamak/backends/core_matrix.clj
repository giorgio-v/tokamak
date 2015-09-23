(ns tokamak.backends.core-matrix
  (:require [clojure.core.matrix :as m]
            [tokamak.core :refer :all]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [compile]))

(defn- keyword->symbol [k]
  (symbol (name k)))

(defn- vars [v]
  (map #(if (keyword? %) (keyword->symbol %) %) (:args v)))

(defprotocol ICoreMatrix
  (-compile [_]))

(defn compile
  [{:keys [graph ret args given]} & [opts]]
  (let [path (->> (graph/backtrack graph ret)
                  (filter (comp not (into #{} (concat args (keys given))))))
        fn-form `(fn [{:keys ~(vec (map keyword->symbol args))}]
                   (let ~(vec
                          (concat
                           (mapcat (juxt (comp keyword->symbol key) val) given)
                           (interleave (map keyword->symbol path)
                                       (->> path (map graph) (map compile-node)))))
                     ;; Add updates to memory atom before returning
                     ~(keyword->symbol ret)))
        _ (clojure.pprint/pprint fn-form)
        compiled-fn (eval fn-form)]
    (fn [& fn-args]
      (compiled-fn (apply hash-map (interleave args fn-args))))))


(extend-protocol ICoreMatrix

  Add
  (-compile [this]
    `(m/add ~@(vars this)))

  Mul
  (-compile [this]
    `(m/mul ~@(vars node)))

  Exp
  (-compile [this]
    `(m/emap #(Math/exp %) ~@(vars node)))

  Ones
  (-compile [this]
    `(m/fill ~@(vars node) 1.0))

  Zeros
  (-compile [this]
    `(m/fill ~@(vars node) 0.0))

  Tensor
  (-compile [this]
    (throw (Exception. (str "Unable to resolve symbol: "
                            (name (:name this)))))))


