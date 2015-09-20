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
  ([v]
   (tensor-type (:dtype v) (:dim v)))
  ([dtype dim]
   (<< "Tensor<~(ctypes dtype),~{dim}>")))

(defn compile-fn [args ret body]
  (let [arg-types (map tensor-type args)
        arg-vars (map (comp variable :name) args)
        arg-str (s/join "," (map #(str %1 "& " %2) arg-types arg-vars))
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
~{ret-str};
}
"))
  )

(defmulti compile* (fn [v] (or (:op v) (:type v))))

(defmethod compile* :tensor
  [v]
  (throw (Exception. (str "Unable to resolve symbol: "
                          (name (:name v))))))

(defn compile
  ;; opts is for backend-specific options
  [{:keys [graph ret args given]} & [opts]]
  (let [path (->> (graph/backtrack graph ret)
                  (filter (comp not (into #{} (concat args (keys given))))))
        arguments (map graph args)
        ;; TODO: given
        body (->> path (map graph) (map compile*))
        code (compile-fn (map graph args) (graph ret) body)
        _ (println code)
        ]
    ;; TODO: build and open with JNI
    #_(fn [& fn-args]
      (compiled-fn (apply hash-map (interleave args fn-args))))))


(defmethod compile* :alias
  [v]
  (let [lhs (variable (:name v))
        rhs (variable (first (:args v)))]
    (<< "auto ~{lhs} = ~{rhs};")))

(defmethod compile* :add
  [v]
  (let [lhs (variable (:name v))
        rhs (s/join "+" (map variable (:args v)))]
    (<< "auto ~{lhs} = ~{rhs};")))

(defmethod compile* :mul
  [v]
  (let [lhs (variable (:name v))
        rhs (s/join "*" (map variable (:args v)))]
    (<< "auto ~{lhs} = ~{rhs};")))

(defmethod compile* :exp
  [v]
  (let [lhs (variable (:name v))
        arg (variable (first (:args v)))]
    (<< "auto ~{lhs} = ~{arg}.exp();")))

(defmethod compile* :ones
  [v]
  (let [lhs (variable (:name v))
        arg (variable (first (:args v)))]
    (<< "auto ~{lhs} = ~{arg}.ones();")))

(defmethod compile* :zeros
  [v]
  (let [lhs (variable (:name v))
        arg (variable (first (:args v)))]
    (<< "auto ~{lhs} = ~{arg}.zeros();")))

