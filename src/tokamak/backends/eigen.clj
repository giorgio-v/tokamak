(ns tokamak.backends.eigen
  (:require [clojure.string :as s]
            [clojure.core.strint :refer [<<]]
            [tokamak.ops :as ops]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [compile]))

(defn- var-name [arg]
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
        arg-vars (map (comp var-name :name) args)
        arg-str (s/join ", " (map #(str %1 "& " %2) arg-types arg-vars))
        ;; TODO: actually infer type, this here is WRONG
        ret-type (tensor-type (first args))
        ret-var (var-name (:name ret))
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

(defprotocol IEigen
  (-compile [_]))

(defn compile
  [{:keys [graph ret args given]} & [opts]]
  (let [path (->> (graph/backtrack graph ret)
                  (filter (comp not (into #{} (concat args (keys given))))))
        arguments (map graph args)
        ;; TODO: given
        body (->> path (map graph) (map -compile))
        code (compile-fn (map graph args) (graph ret) body)
        _ (println code)
        ]
    ;; TODO: build and open with JNI
    #_(fn [& fn-args]
      (compiled-fn (apply hash-map (interleave args fn-args))))))


(extend-protocol IEigen

  tokamak.core.Tensor
  (-compile [this]
    (throw (Exception. (str "Unable to resolve symbol: "
                            (name (:name this))))))

  tokamak.core.Constant
  (-compile [this]
    (let [lhs (var-name (:name this))
          rhs (str (:value this))]
      (<< "auto ~{lhs} = ~{rhs};")))

  tokamak.ops.Add
  (-compile [this]
    (let [lhs (var-name (:name this))
          rhs (s/join " + " (map var-name (ops/-args this)))]
      (<< "auto ~{lhs} = ~{rhs};")))

  tokamak.ops.Mul
  (-compile [this]
    (let [lhs (var-name (:name this))
          rhs (s/join " * " (map var-name (ops/-args this)))]
      (<< "auto ~{lhs} = ~{rhs};")))

  tokamak.ops.Exp
  (-compile [this]
    (let [lhs (var-name (:name this))
          arg (var-name (:x this))]
      (<< "auto ~{lhs} = ~{arg}.exp();")))

  ;;tokamak.ops.Ones
  ;;(-compile [this]
  ;;  (let [lhs (var-name (:name this))
  ;;        arg (var-name (first (:args this)))]
  ;;    (<< "auto ~{lhs} = ~{arg}.ones();")))

  ;;tokamak.ops.Zeros
  ;;(-compile [this]
  ;;  (let [lhs (var-name (:name this))
  ;;        arg (var-name (first (:args this)))]
  ;;    (<< "auto ~{lhs} = ~{arg}.zeros();")))

  )

