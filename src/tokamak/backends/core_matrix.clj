(ns tokamak.backends.core-matrix
  (:require [clojure.core.matrix :as m]
            [tokamak.ops :as ops]
            [tokamak.graph :as graph])
  (:refer-clojure :exclude [compile]))

(defn- keyword->symbol [k]
  (symbol (name k)))

(defn- vars [v]
  (map #(if (keyword? %) (keyword->symbol %) %) (ops/-args v)))

(defprotocol ICoreMatrix
  (-compile [_]))

(defn compile
  [{:keys [graph ret args given]} & [opts]]
  (let [path (->> (graph/backtrack graph ret)
                  (filter (comp not (into #{} (concat args (keys given))))))
        fn-form `(fn [{:keys ~(vec (map keyword->symbol args))}]
                   (let ~(vec
                          (concat
                           (mapcat (juxt (comp keyword->symbol key) val) given)
                           (interleave (map keyword->symbol path)
                                       (->> path (map graph) (map -compile)))))
                     ;; Add updates to memory atom before returning
                     ~(keyword->symbol ret)))
        _ (clojure.pprint/pprint fn-form)
        compiled-fn (eval fn-form)]
    (fn [& fn-args]
      (compiled-fn (apply hash-map (interleave args fn-args))))))


(extend-protocol ICoreMatrix

  tokamak.core.Tensor
  (-compile [this]
    (throw (Exception. (str "Unable to resolve symbol: "
                            (name (:name this))))))

  tokamak.core.Constant
  (-compile [this]
    (:value this))

  tokamak.ops.Add
  (-compile [this]
    `(m/add ~@(vars this)))

  tokamak.ops.Mul
  (-compile [this]
    `(m/mul ~@(vars this)))

  tokamak.ops.EMul
  (-compile [this]
    `(m/emul ~@(vars this)))

  tokamak.ops.Exp
  (-compile [this]
    `(m/emap #(Math/exp %) ~@(vars this)))

  tokamak.ops.Abs
  (-compile [this]
    `(m/emap #(Math/abs %) ~@(vars this)))

  tokamak.ops.Ceil
  (-compile [this]
    `(m/emap #(Math/ceil %) ~@(vars this)))

  tokamak.ops.Cos
  (-compile [this]
    `(m/emap #(Math/cos %) ~@(vars this)))

  tokamak.ops.IntCeil
  (-compile [this]
    `(m/emap #(long (Math/ceil %)) ~@(vars this)))

  tokamak.ops.IntCeil
  (-compile [this]
    `(m/emap #(long (Math/floor %)) ~@(vars this)))

  tokamak.ops.Log
  (-compile [this]
    `(m/emap #(Math/log %) ~@(vars this)))

  tokamak.ops.Negate
  (-compile [this]
    `(m/negate ~@(vars this)))

  tokamak.ops.Sigmoid
  (-compile [this]
    `(m/emap #(/ 1.0 (+ 1.0 (Math/exp (- x)))) ~@(vars this)))

  tokamak.ops.Sign
  (-compile [this]
    `(m/emap #(Math/signum %) ~@(vars this)))

  tokamak.ops.Sin
  (-compile [this]
    `(m/emap #(Math/sin %) ~@(vars this)))

  tokamak.ops.Sqrt
  (-compile [this]
    `(m/emap #(Math/sqrt %) ~@(vars this)))

  tokamak.ops.Square
  (-compile [this]
    `(m/square ~@(vars this)))

  tokamak.ops.Tanh
  (-compile [this]
    `(m/emap #(Math/tanh %) ~@(vars this)))

  tokamak.ops.Div
  (-compile [this]
    `(m/div ~@(vars this)))

  tokamak.ops.Equal
  (-compile [this]
    `(m/emap = ~@(vars this)))

  tokamak.ops.Greater
  (-compile [this]
    `(m/emap > ~@(vars this)))

  tokamak.ops.GreaterEqual
  (-compile [this]
    `(m/emap >= ~@(vars this)))

  tokamak.ops.Less
  (-compile [this]
    `(m/emap < ~@(vars this)))

  tokamak.ops.LessEqual
  (-compile [this]
    `(m/emap <= ~@(vars this)))

  tokamak.ops.NotEqual
  (-compile [this]
    `(m/emap not= ~@(vars this)))

  tokamak.ops.Power
  (-compile [this]
    `(m/pow ~@(vars this) (:y this)))

  tokamak.ops.Sub
  (-compile [this]
    `(m/sub ~@(vars this)))

  tokamak.ops.ArgMax
  (-compile [this]
    (assert false))

  tokamak.ops.BatchedMM
  (-compile [this]
    (assert false))

  tokamak.ops.Broadcast
  (-compile [this]
    (assert false))

  tokamak.ops.Cast
  (-compile [this]
    (assert false))

  tokamak.ops.CeilDiv
  (-compile [this]
    (assert false))

  tokamak.ops.EinSum
  (-compile [this]
    (assert false))

  tokamak.ops.Fill
  (-compile [this]
    `(m/fill ~@(vars this) (:value this)))

  tokamak.ops.Flatten
  (-compile [this]
    (assert false))

  tokamak.ops.Flip
  (-compile [this]
    (assert false))

  tokamak.ops.FloorDiv
  (-compile [this]
    (assert false))

  tokamak.ops.Subtensor
  (-compile [this]
    (assert false))

  tokamak.ops.IncSubtensor
  (-compile [this]
    (assert false))

  tokamak.ops.Max
  (-compile [this]
    `(m/emax ~@(vars this)))

  tokamak.ops.Mean
  (-compile [this]
    `(/ (m/esum ~@(vars this)) (m/ecount ~@(vars this))))

  tokamak.ops.Norm
  (-compile [this]
    (assert false))

  tokamak.ops.Ones
  (-compile [this]
    (assert false))

  tokamak.ops.OnesLike
  (-compile [this]
    (assert false))

  tokamak.ops.Rand
  (-compile [this]
    (assert false))

  tokamak.ops.Randn
  (-compile [this]
    (assert false))

  tokamak.ops.Repeat
  (-compile [this]
    (assert false))

  tokamak.ops.Reshape
  (-compile [this]
    (assert false))

  tokamak.ops.Shape
  (-compile [this]
    (assert false))

  tokamak.ops.Sise
  (-compile [this]
    (assert false))

  tokamak.ops.Stack
  (-compile [this]
    (assert false))

  tokamak.ops.Prod
  (-compile [this]
    (assert false))

  tokamak.ops.Sum
  (-compile [this]
    (assert false))

  tokamak.ops.Transpose
  (-compile [this]
    (assert false))

  tokamak.ops.TupleIndex
  (-compile [this]
    (assert false))

  tokamak.ops.Zeros
  (-compile [this]
    (assert false))

  tokamak.ops.ZerosLike
  (-compile [this]
    (assert false))

  tokamak.ops.DimShuffle
  (-compile [this]
    (assert false))






  ;;tokamak.ops.Ones
  ;;(-compile [this]
  ;;  `(m/fill ~@(vars this) 1.0))

  ;;tokamak.ops.Zeros
  ;;(-compile [this]
  ;;  `(m/fill ~@(vars this) 0.0))

  )


