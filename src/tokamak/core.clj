(ns tokamak.core
  (:refer-clojure :exclude [+ * vector]))

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

(defn- get-node
  ([v]
   (get-node v (:name v)))
  ([v k]
   (get (:graph v) k)))

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

;; Operations

(defn + [& args]
  (operation :+ args))

(defn * [& args]
  (operation :* args))

(defn exp [arg]
  (operation :exp [arg]))

