(ns tokamak.core
  (:refer-clojure :exclude [vector]))

(defn- genkey []
  (keyword (gensym "VAR")))

(defn variable
  ([name]
   (variable name {:name name}))
  ([name m]
   (variable name m {}))
  ([name m graph]
   {:name name
    :graph (assoc graph name m)}))

(defn- operation
  ([op args]
   (operation op args nil))
  ([op args name]
   (let [name (or name (genkey))
         graph (apply merge (filter map? (map :graph args)))
         op-args (mapv (fn [arg]
                         ;; TODO: check: is this still needed?
                         (cond
                           (map? arg) (:name arg)
                           :else arg))
                       args)]
     (variable name {:name name :type :op :op op :args op-args} graph))))

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

(defn function
  [args ret]
  {:args (mapv :name args)
   :ret (:name ret)
   :graph (:graph ret)})

;; Operations

(defn named
  [v name]
  (operation :alias [v] name))

(defn add [& args]
  (operation :add args))

(defn mul [& args]
  (operation :mul args))

(defn exp [arg]
  (operation :exp [arg]))

(defn ones [arg]
  (operation :ones [arg]))

(defn zeros [arg]
  (operation :zeros [arg]))
