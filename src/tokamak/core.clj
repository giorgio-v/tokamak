(ns tokamak.core
  (:require [clojure.set :as s])
  (:refer-clojure :exclude [+ * compile vector]))

(alias 'p 'clojure.pprint)

(defn- genkey []
  (keyword (gensym "V__")))

(defn forward-edges
  [graph]
  (->> graph
       (map val)
       (filter #(= (:kind %) :op))
       (reduce
        (fn [out v]
          (reduce
           (fn [out arg]
             (update-in out [arg] conj (:name v)))
           out
           (:args v))) {})))

(defn compute-stack
  [graph back-from]
  (let [fwd-edges (forward-edges graph)]
    (loop [stack []
           visited #{}
           nodes #{back-from}]
      (let [args (->> (map graph nodes)
                      (map :args)
                      flatten
                      (filter identity)
                      (filter keyword?)
                      (into #{}))
            args (s/difference args visited)
            new-visited (filter
                         (fn [n] (every? visited (get fwd-edges n)))
                         nodes)
            new-identities (filter keyword? (map graph new-visited))
            new-visited (concat new-visited new-identities)
            new-stack (concat stack new-visited)]
        (if (empty? args)
          (reverse new-stack)
          (recur new-stack
                 (apply conj visited new-visited)
                 (apply conj (s/difference nodes new-visited) args)))))))


(defmulti compile-op (fn [backend v] [backend (:op v)]))

(defmethod compile-op [:core.matrix :+]
  [_ v]
  #_(fn [args] (apply m/add args))
  `(apply m/add (:args v)))

(defmethod compile-op [:core.matrix :*]
  [_ v]
  `(apply m/mul (:args v)))

(defmethod compile-op [:core.matrix :exp]
  [_ v]
  `(m/emap #(Math/exp %) (:args v)))


(defmulti compile (fn [backend _] backend))

;; returns a function of the arguments, specified as a kv map

(defmethod compile :core.matrix
  [_ v]
  (let [graph (:graph v)
        inputs (filter (fn [[_ v]] (= (:kind v) :variable)) graph)
        stack (compute-stack graph (:name v))]
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


(defn variable
  ([name m]
   (variable name m {}))
  ([name m graph]
   {:name name
    :graph (assoc graph name m)}))

(defn named
  [v name]
  {:name name
   :graph (assoc (:graph v) name (:name v))})

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

(defn operation
  [op args]
  (let [name (genkey)
        graph (apply merge (filter map? (map :graph args)))
        op-args (mapv (fn [arg]
                        (cond
                          (map? arg) (:name arg)
                          :else arg))
                      args)]
    (variable name {:name name :kind :op :op op :args op-args} graph)))

(defn function
  [arguments out & [{:keys {backend :backend
                            given :given
                            updates :updates
                            shared :shared}}]]
  ;; TODO: implement givens and updates
  (let [backend (or backend :core.matrix)
        compiled (compile backend out)
        arg-names (map :name arguments)]
    (fn [& args]
      (compiled (apply hash-map (interleave arg-names args))))))

(defn +
  [& args]
  (operation :+ args))

(defn *
  [& args]
  (operation :* args))

(defn exp
  [arg]
  (operation :exp [arg]))

(defn do-it []

  #_(let [a (tensor :int64 2 :a)
        b (tensor :int64 2 :b)]
    (p/pprint (+ a b))
    (function [a b] (+ a b) {:backend :core.matrix}))

  (let [a (tensor :int64 2 :a)
        b (matrix :int64 :b)
        c (+ a b)
        e (named (* a b c) :e)
        d (+ a e)]
    #_(p/pprint (* a c 2 (exp d)))
    (let [z (* a c 2 (exp d))]
      (p/pprint z)
      (p/pprint (forward-edges (:graph z)))
      (p/pprint (compute-stack (:graph z) (:name z))))
    (function [a b] d)

    #_(comment
      (let [f (function [a b] d)]
        (f :a [[1 1] [2 2]]
           :b [[1 1] [2 2]]))
      (let [f (function [a b] d {:backend :core.matrix})]
        (f [[1 1] [2 2]]
           [[1 1] [2 2]])))

      ))

