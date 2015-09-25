(ns tokamak.core
  (:require [tokamak.ops :refer :all]))

(defn genkey []
  (keyword (gensym "V")))

(defrecord Tensor [name dim dtype])

(defrecord Variable [name graph])

(defrecord Function [args ret graph])

(defn variable
  [name v graph]
  (Variable. name (assoc graph name v)))

(defn operation
  ([op-fn args]
   (operation op-fn args (genkey)))
  ([op-fn args name]
   (variable name
             (op-fn name (mapv #(or (:name %) %) args))
             (apply merge (map :graph args)))))

(defn tensor
  ([dtype dim]
   (tensor dtype dim (genkey)))
  ([dtype dim name]
   (variable name (Tensor. name dim dtype) {})))

(defn function
  [args ret]
  (Function. (mapv :name args)
             (:name ret)
             (:graph ret)))

(defn add [& args]
  (operation ->Add args))

(defn mul [& args]
  (operation ->Mul args))

(defn exp [arg]
  (operation ->Exp [arg]))

(defn ones [arg]
  (operation ->Ones [arg]))

(defn zeros [arg]
  (operation ->Zeros [arg]))

