(ns tokamak.multi
  (:refer-clojure :exclude [compile]))

;; returns a function of the arguments, specified as a kv map
(defmulti compile (fn [{:keys [backend]} _] backend))

