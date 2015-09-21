(ns tokamak.backends.eigen
  (:require [clojure.string :as s]
            [clojure.core.strint :refer [<<]]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [compile]))

(defn- variable [arg]
  (if (keyword? arg) (name arg) arg))

(defn- indent [n body]
  (let [indentation (apply str (repeat n " "))]
    (map #(str indentation %) body)))

(def ctypes
  {:int64 "long"})

(defn tensor-type
  ([node]
   (tensor-type (:dtype node) (:dim node)))
  ([dtype dim]
   (<< "Tensor<~(ctypes dtype),~{dim}>")))

(defn compile-fn [args ret body]
  (let [arg-types (map tensor-type args)
        arg-vars (map (comp variable :name) args)
        arg-str (s/join ", " (map #(str %1 "& " %2) arg-types arg-vars))
        ;; TODO: actually infer type, this here is WRONG
        ret-type (tensor-type (first args))
        ret-var (variable (:name ret))
        ret-str (<< "  return ~{ret-var};")
        body-str (s/join "\n" (indent 2 body))]
    ;; TODO: we need an explicit var to realize the computation.
    ;; Plus we need an extra c function that wraps the Eigen one for clj-jna
    ;; Possibility is pass in a plain struct representing a Tensor (bytes, dtype, size)
    (<< "
#include <Eigen/CXX11/Tensor>

~{ret-type}& fn(~{arg-str})
{
~{body-str}
~{ret-str}
}
"))
  )

(defmulti compile-node (fn [node] (or (:op node) (:type node))))

(defmethod compile-node :tensor
  [node]
  (throw (Exception. (str "Unable to resolve symbol: "
                          (name (:name node))))))

(defn compile
  [{:keys [graph ret args given]} & [opts]]
  (let [path (->> (graph/backtrack graph ret)
                  (filter (comp not (into #{} (concat args (keys given))))))
        arguments (map graph args)
        ;; TODO: given
        body (->> path (map graph) (map compile-node))
        code (compile-fn (map graph args) (graph ret) body)
        _ (println code)
        ]
    ;; TODO: build and open with JNI
    #_(fn [& fn-args]
      (compiled-fn (apply hash-map (interleave args fn-args))))))


(defmethod compile-node :add
  [node]
  (let [lhs (variable (:name node))
        rhs (s/join "+" (map variable (:args node)))]
    (<< "auto ~{lhs} = ~{rhs};")))

(defmethod compile-node :mul
  [node]
  (let [lhs (variable (:name node))
        rhs (s/join "*" (map variable (:args node)))]
    (<< "auto ~{lhs} = ~{rhs};")))

(defmethod compile-node :exp
  [node]
  (let [lhs (variable (:name node))
        arg (variable (first (:args node)))]
    (<< "auto ~{lhs} = ~{arg}.exp();")))

(defmethod compile-node :ones
  [node]
  (let [lhs (variable (:name node))
        arg (variable (first (:args node)))]
    (<< "auto ~{lhs} = ~{arg}.ones();")))

(defmethod compile-node :zeros
  [node]
  (let [lhs (variable (:name node))
        arg (variable (first (:args node)))]
    (<< "auto ~{lhs} = ~{arg}.zeros();")))

