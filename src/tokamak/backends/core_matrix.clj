(ns tokamak.backends.core-matrix
  (:require [tokamak.multi :refer :all]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [compile]))

(defmulti compile-op (fn [op _] op))

(defmethod compile-op :+
  [_ v]
  #_(fn [args] (apply m/add args))
  `(apply m/add (:args v)))

(defmethod compile-op :*
  [_ v]
  `(apply m/mul (:args v)))

(defmethod compile-op :exp
  [_ v]
  `(m/emap #(Math/exp %) (:args v)))


(defmethod compile :core-matrix
  [_ function]
  (let [;;inputs (filter (fn [[_ v]] (= (:kind v) :variable)) graph)
        stack (graph/compute-stack (:graph function) (:ret function))]
    ;; here it would be good to compile to quoted clojure code and eval
    ;; just to prove we can emit efficient code, instead of reducing (see below)
    ;; Using stack, we can essentially construct a series of quoted single assignment
    ;; statements, embed them in a let form and return the last value. That easy.
    ;; In the let form we can safely use the pre-generated gensyms, since we know it's
    ;; going to be the only content and we don't use any other gensym
    (fn [kv]
      #_(reduce
         (fn [out name]
           (if-let [input (get kv name)]
             (assoc out name input)
             (assoc out name (compile-op ))))
         {}
         stack))))

