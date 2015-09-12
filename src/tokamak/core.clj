(ns tokamak.core
  (:require [tokamak.multi :as multi])
  (:refer-clojure :exclude [+ * compile vector]))

(def ^:dynamic *default-backend* :core-matrix)

(defn- genkey []
  (keyword (gensym "V__")))

(defn- variable
  ([name m]
   (variable name m {}))
  ([name m graph]
   {:name name
    :graph (assoc graph name m)}))

(defn- operation
  [op args]
  (let [name (genkey)
        graph (apply merge (filter map? (map :graph args)))
        op-args (mapv (fn [arg]
                        (cond
                          (map? arg) (:name arg)
                          :else arg))
                      args)]
    (variable name {:name name :kind :op :op op :args op-args} graph)))


;; Public API

(defn tensor
  ([dtype dim]
   (tensor (genkey)))
  ([dtype dim name]
   (variable name {:name name
                   :kind :variable
                   :type :tensor
                   :dim dim
                   :dtype dtype})))

(defn matrix
  ([dtype]
   (tensor dtype 2))
  ([dtype name]
   (tensor dtype 2 name)))

(defn vector
  ([dtype]
   (tensor dtype 1))
  ([dtype name]
   (tensor dtype 1 name)))

(defn scalar
  ([dtype]
   (tensor dtype 0))
  ([dtype name]
   (tensor dtype 0 name)))

(defn named
  [v name]
  {:name name
   :graph (assoc (:graph v) name (:name v))})

(defn function
  [args ret]
  {:args (map :name args)
   :ret (:name ret)
   :graph (:graph ret)})

;; TODO: add support for given, updates and shared
(defn compile
  ([function]
   (compile {:backend *default-backend*} function))
  ([backend function]
   (let [compiled-fn (multi/compile backend function)
         arg-names (:args function)]
     (fn [& args]
       (compiled-fn (apply hash-map (interleave arg-names args)))))))


;; Operations

(defn +
  [& args]
  (operation :+ args))

(defn *
  [& args]
  (operation :* args))

(defn exp
  [arg]
  (operation :exp [arg]))

