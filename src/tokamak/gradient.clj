(ns tokamak.gradient
  (:require [tokamak.core :refer :all]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [vector]))

(defn- grad-key [key wrt]
  (keyword (str "d" (name key) "_d" (name wrt))))

(defmulti gradient* (fn [v _ _] (or (:op v) (:type v))))

(defn- grad-args [args wrt]
  (map (fn [arg]
         (if (keyword? arg)
           (grad-key arg wrt)
           arg)) args))

(defn- map-from [ks vs]
  (apply hash-map (interleave ks vs)))

;; TODO: refactor gargs-all gargs, (filter keyword? args), map-from etc
(defn gradient [{:keys [graph args ret]} {wrt :name}]
  (let [grad-ret (grad-key ret wrt)]
    (loop [stack [grad-ret]
           grad-vars (hash-map grad-ret ret)
           grad-graph graph]
      (if (empty? stack)
        (function (vals (select-keys graph args))
                  {:name grad-ret :graph grad-graph})
        (let [node (graph (grad-vars (peek stack)))
              args (:args node)
              gargs-all (grad-args args wrt)
              gargs (grad-args (filter keyword? args) wrt)
              grad* (named
                     (gradient* node (map-from args gargs-all) wrt)
                     (peek stack))]
          (recur (apply conj (pop stack) gargs)
                 (merge grad-vars (map-from gargs (filter keyword? args)))
                 (merge grad-graph (:graph grad*))))))))

(defmethod gradient* :alias [{[arg] :args} dmap wrt]
  (dmap arg))

(defmethod gradient* :add [{args :args} dmap wrt]
  (apply add (map dmap (filter keyword? args))))

#_(defmethod gradient* :mul [{args :args} dmap wrt]
    )

#_(defmethod gradient* :exp [{[arg] :args} dmap wrt]
    )

(defmethod gradient* :tensor [v dmap wrt]
  (if (= (:name v) wrt) (ones v) (zeros v)))

