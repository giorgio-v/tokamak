(ns tokamak.graph
  (:require [tokamak.ops :as ops]
            [clojure.set :as s]))

(defn forward-edges
  [graph]
  (->> graph
       (map val)
       (filter #(extends? ops/IOp (type %)))
       (reduce
        (fn [out op]
          (reduce
           (fn [out arg]
             (update-in out [arg] conj (:name op)))
           out
           (ops/-args op))) {})))

(defn rename-node
  [graph name new-name]
  (-> graph
      (assoc-in [name :name] new-name)
      (s/rename-keys {name new-name})))

(defn reach
  [graph from]
  (let [fwd-edges (forward-edges graph)]
    (loop [reached #{}
           nodes #{from}]
      (let [args (->> (map graph nodes)
                      (filter #(extends? ops/IOp (type %)))
                      (map ops/-args)
                      flatten
                      ;; TODO: replace keyword?
                      (filter keyword?)
                      (into #{}))
            args (s/difference args reached)
            reached (apply conj reached nodes)]
        (if (empty? args)
          reached
          (recur reached args))))))

(defn clean
  [graph from]
  (let [reached (reach graph from)]
    (into {} (filter (comp reached key) graph))))

;; TODO: instead of producing a list, produce a list of lists,
;; where the inner lists are all the vars that can be evaluated
;; in parallel
(defn backtrack
  [graph from]
  (let [graph (clean graph from)
        fwd-edges (forward-edges graph)]
    (loop [path []
           visited #{}
           nodes #{from}]
      (let [args (->> (map graph nodes)
                      (filter #(extends? ops/IOp (type %)))
                      (map ops/-args)
                      flatten
                      ;; TODO: replace keyword?
                      (filter keyword?)
                      (into #{}))
            args (s/difference args visited)
            new-visited (filter
                         (fn [node] (every? visited (get fwd-edges node)))
                         nodes)
            new-path (concat path new-visited)]
        (if (empty? args)
          (reverse new-path)
          (recur new-path
                 (apply conj visited new-visited)
                 (apply conj (s/difference nodes new-visited) args)))))))

