(ns tokamak.core)

(defn genkey []
  (keyword (gensym "V")))

(defrecord Tensor [name dim dtype])

(defrecord Variable [name graph])

(defrecord Function [args ret graph])

(defprotocol IOp)

(defrecord Add [name args] IOp)

(defrecord Mul [name args] IOp)

(defrecord Exp [name args] IOp)

(defrecord Ones [name args] IOp)

(defrecord Zeros [name args] IOp)


(defprotocol Foo
  (foo [_]))

(extend-protocol Foo
  Tensor
  (foo [_] :a))

(defn variable
  [name v graph]
  (Variable. name (assoc graph name v)))

(defn operation
  ([op-fn args]
   (operation op-fn (genkey) args))
  ([op-fn args name]
   (variable name
             (op-fn name (mapv :name args))
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

