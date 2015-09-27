(ns tokamak.core)

(defn genkey []
  (keyword (gensym "V")))

(defprotocol ITensor
  (-dim [_]))

(defprotocol IFixedTensor
  (-shape [_]))

(defrecord Tensor [name dim dtype]
  ITensor
  (-dim [_] dim)
  (-dtype [_] dtype))

(defrecord FixedTensor [name shape dtype]
  ITensor
  (-dim [_] (count shape))
  (-dtype [_] dtype)
  IFixedTensor
  (-shape [_] shape))

(defrecord Constant [name value])

(defrecord Variable [name graph])

(defrecord Function [args ret graph])

(defn variable
  [v graph]
  (Variable. (:name v)
             (assoc graph (:name v) v)))

(defn constant
  [value]
  (variable (Constant. (genkey) value) {}))

(defn tensor
  [dtype dim]
  (variable (Tensor. (genkey) dim dtype) {}))

(defn fixed-tensor
  [dtype shape]
  (variable (FixedTensor. (genkey) shape dtype) {}))

(defn function
  [args ret]
  (Function. (mapv :name args)
             (:name ret)
             (:graph ret)))

(defn infer-shape
  [variable]
  )
