(ns tokamak.graph
  (:require [clojure.set :as s]))

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

