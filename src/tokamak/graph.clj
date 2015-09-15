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

;; TODO: instead of producing a list, produce a list of lists,
;; where the inner lists are all the vars that can be evaluated
;; in parallel
(defn backtrack
  [graph output-node]
  (let [fwd-edges (forward-edges graph)]
    (loop [path []
           visited #{}
           nodes #{output-node}]
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
            new-path (concat path new-visited)]
        (if (empty? args)
          (reverse new-path)
          (recur new-path
                 (apply conj visited new-visited)
                 (apply conj (s/difference nodes new-visited) args)))))))

