(ns tokamak.backends.core-matrix
  (:require [clojure.core.matrix :as m]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [compile]))

(defn- keyword->symbol [k]
  (symbol (name k)))

(defn- vars [v]
  (map #(if (keyword? %) (keyword->symbol %) %) (:args v)))

(defmulti compile-node (fn [node] (or (:op node) (:type node))))

(defmethod compile-node :tensor
  [node]
  (throw (Exception. (str "Unable to resolve symbol: "
                          (name (:name node))))))

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

(defmethod compile-node :add
  [node]
  `(m/add ~@(vars node)))

(defmethod compile-node :mul
  [node]
  `(m/mul ~@(vars node)))

(defmethod compile-node :exp
  [node]
  `(m/emap #(Math/exp %) ~@(vars node)))

(defmethod compile-node :ones
  [node]
  `(m/fill ~@(vars node) 1.0))

(defmethod compile-node :zeros
  [node]
  `(m/fill ~@(vars node) 0.0))
