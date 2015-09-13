(ns tokamak.backends.core-matrix
  (:require [clojure.core.matrix :as m]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [compile]))

(defn- keyword->symbol [k]
  (symbol (name k)))

(defn- args->vars [v]
  (map keyword->symbol (:args v)))

(defmulti compile* (fn [v] (or (:op v) (:kind v))))

(defmethod compile* :+
  [v]
  `(m/add ~@(args->vars v)))

(defmethod compile* :*
  [v]
  `(m/mul ~@(args->vars v)))

(defmethod compile* :exp
  [v]
  `(m/emap #(Math/exp %) ~@(args->vars v)))

(defmethod compile* :variable
  [v]
  (throw (Exception. (str "Unable to resolve symbol: "
                          (name (:name v))))))

(defn compile
  ;; opts is for backend-specific options
  [{:keys [graph ret args given]} & [opts]]
  (let [path (->> (graph/backward-path graph ret)
                   (filter (comp not (into #{} (concat args (keys given))))))
        fn-form `(fn [{:keys ~(vec (map keyword->symbol args))}]
                   (let ~(vec
                          (concat
                           (mapcat (juxt (comp keyword->symbol key) val) given)
                           (interleave (map keyword->symbol path)
                                       (->> path (map graph) (map compile*)))))
                     ~(keyword->symbol ret)))
        compiled-fn (eval fn-form)]
    ;; TODO: possibly avoid this outer function, just build the args
    ;; thing into the inner one
    (fn [& fn-args]
      (compiled-fn (apply hash-map (interleave args fn-args))))))

