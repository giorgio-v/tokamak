(ns tokamak.ops)

(defprotocol IOp)

(defrecord Add [name args]
  IOp)

(defrecord Mul [name args]
  IOp)

(defrecord Exp [name args]
  IOp)

(defrecord Ones [name args]
  IOp)

(defrecord Zeros [name args]
  IOp)


