(ns tokamak.ops
  (:require [tokamak.core :refer :all]))

(defn operation* [op-fn args]
  (variable (op-fn (genkey) (mapv #(or (:name %) %) args))
            (apply merge (map :graph args))))

(defn operation [op-fn & args]
  (variable (apply op-fn (genkey) (mapv #(or (:name %) %) args))
            (apply merge (map :graph args))))

(defprotocol IOp
  (-args [_])
  (-infer-dim [_ graph])
  (-infer-shape [_ graph])
  (-infer-dtype [_ graph]))

(defn var-args [op]
  (filter keyword? (-args op)))

(defn default-op-fns [& args]
  {:-args (apply juxt args)
   :-infer-dim (fn [this graph] (-> this (first args) graph -dim))
   :-infer-shape (fn [this graph] (-> this (first args) graph -shape))
   :-infer-dtype (fn [this graph] (-> this (first args) graph -dtype))})

(defn nary-op-fns []
  {:-args (fn [this] (:args this))
   :-infer-dim (fn [this graph] (-> this :args first graph -dim))
   :-infer-shape (fn [this graph] (-> this :args first graph -shape))
   :-infer-dtype (fn [this graph] (-> this :args first graph -dtype))})

(defrecord Add [name args])
(extend Add IOp (nary-op-fns))

(defrecord Mul [name args]
  IOp
  (-args [_] args)
  (-infer-dim [_ graph] (assert false))
  (-infer-shape [_ graph] (assert false))
  (-infer-dtype [_ graph] (assert false)))

(defrecord EMul [name args])
(extend EMul IOp (nary-op-fns))

(defrecord Exp [name x])
(extend Exp IOp (default-op-fns :x))

(defrecord Abs [name x])
(extend Abs IOp (default-op-fns :x))

(defrecord Ceil [name x])
(extend Ceil IOp (default-op-fns :x))

(defrecord Cos [name x])
(extend Cos IOp (default-op-fns :x))

(defrecord IntCeil [name x])
(extend IntCeil IOp (default-op-fns :x))

(defrecord IntFloor [name x])
(extend IntFloor IOp (default-op-fns :x))

(defrecord Log [name x])
(extend Log IOp (default-op-fns :x))

(defrecord Negate [name x])
(extend Negate IOp (default-op-fns :x))

(defrecord Sigmoid [name x])
(extend Sigmoid IOp (default-op-fns :x))

(defrecord Sign [name x])
(extend Sign IOp (default-op-fns :x))

(defrecord Sin [name x])
(extend Sin IOp (default-op-fns :x))

(defrecord Sqrt [name x])
(extend Sqrt IOp (default-op-fns :x))

(defrecord Square [name x])
(extend Square IOp (default-op-fns :x))

(defrecord Tanh [name x])
(extend Tanh IOp (default-op-fns :x))

(defrecord Div [name args])
(extend Div IOp (nary-op-fns))

(defrecord Equal [name x y])
(extend Equal IOp (default-op-fns :x :y))

(defrecord Greater [name x y])
(extend Greater IOp (default-op-fns :x :y))

(defrecord GreaterEqual [name x y])
(extend GreaterEqual IOp (default-op-fns :x :y))

(defrecord Less [name x y])
(extend Less IOp (default-op-fns :x :y))

(defrecord LessEqual [name x y])
(extend LessEqual IOp (default-op-fns :x :y))

(defrecord NotEqual [name x y])
(extend NotEqual IOp (default-op-fns :x :y))

(defrecord Power [name x y])
(extend Power IOp (default-op-fns :x))

(defrecord Sub [name args])
(extend Add IOp (nary-op-fns))

(defrecord ArgMax [name x axis keep-dims])
(extend ArgMax IOp (default-op-fns :x))

(defrecord BatchedMM [name x y])
(extend BatchedMM IOp (default-op-fns :x :y))

(defrecord Broadcast [x])
(extend Broadcast IOp (default-op-fns :x))

(defrecord Cast [x dtype])
(extend Cast IOp (default-op-fns :x))

(defrecord CeilDiv [x y])
(extend CeilDiv IOp (default-op-fns :x :y))

(defrecord EinSum [desc x y]
  IOp
  (-args [_] [x y])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Fill [x value])
(extend Fill IOp (default-op-fns :x))

(defrecord Flatten [x]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Flip [x]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord FloorDiv [x y]
  IOp
  (-args [_] [x y])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Subtensor [x slis]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord IncSubtensor [x slis y]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Max [x axis keep-dims])
(extend Max IOp (default-op-fns :x))

(defrecord Mean [x axis keep-dims])
(extend Mean IOp (default-op-fns :x))

(defrecord Norm [x axis p keep-dims])
(extend Norm IOp (default-op-fns :x))

(defrecord Ones [shape dtype]
  IOp
  (-args [_] [])
  (-infer-dim [_ _] (count shape))
  (-infer-shape [_ _] shape)
  (-infer-dtype [_ _] dtype))

(defrecord OnesLike [x])
(extend OnesLike IOp (default-op-fns :x))

(defrecord Rand [shape]
  IOp
  (-args [_] [])
  (-infer-dim [_ _] (count shape))
  (-infer-shape [_ _] shape)
  (-infer-dtype [_ _] (assert false)))

(defrecord Randn [shape]
  IOp
  (-args [_] [])
  (-infer-dim [_ _] (count shape))
  (-infer-shape [_ _] shape)
  (-infer-dtype [_ _] (assert false)))

(defrecord Repeat [x repeats axis]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ graph] (-dtype (graph x))))

(defrecord Reshape [x shape]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ graph] (-dtype (graph x))))

(defrecord Shape [x]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Size [x axis]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Stack [scalars]
  IOp
  (-args [_])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Prod [x axis keep-dims]
  IOp
  (-args [_])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Sum [x axis keep-dims]
  IOp
  (-args [_])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Transpose [x axes]
  IOp
  (-args [_])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord TupleIndex [x i]
  IOp
  (-args [_])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))

(defrecord Zeros [shape dtype]
  IOp
  (-args [_] [])
  (-infer-dim [_ _] (count shape))
  (-infer-shape [_ _] shape)
  (-infer-dtype [_ _] dtype))

(defrecord ZerosLike [x])
(extend ZerosLike IOp (default-op-fns :x))

(defrecord DimShuffle [x pattern]
  IOp
  (-args [_] [x])
  (-infer-dim [_ _] (assert false))
  (-infer-shape [_ _] (assert false))
  (-infer-dtype [_ _] (assert false)))




(defn add [& args]
  (operation* ->Add args))

(defn mul [& args]
  (operation* ->Mul args))

(defn emul [& args]
  (operation* ->EMul args))

(defn exp [arg]
  (operation ->Exp arg))

(defn abs [x]
  (operation ->Abs x))

(defn ceil [x]
  (operation ->Ceil x))

(defn cos [x]
  (operation ->Cos x))

(defn iceil [x]
  (operation ->IntCeil x))

(defn ifloor [x]
  (operation ->IntFloor x))

(defn log [x]
  (operation ->Log x))

(defn negate [x]
  (operation ->Negate x))

(defn sigmoid [x]
  (operation ->Sigmoid x))

(defn sign [x]
  (operation ->Sign x))

(defn sin [x]
  (operation ->Sin x))

(defn sqrt [x]
  (operation ->Sqrt x))

(defn square [x]
  (operation ->Square x))

(defn tanh [x]
  (operation ->Tanh x))

(defn div [x y]
  (operation ->Div x y))

(defn equal [x y]
  (operation ->Equal x y))

(defn greater [x y]
  (operation ->Greater x y))

(defn greater-equal [x y]
  (operation ->GreaterEqual x y))

(defn less [x y]
  (operation ->Less x y))

(defn less-equal [x y]
  (operation ->LessEqual x y))

(defn not-equal [x y]
  (operation ->NotEqual x y))

(defn power [x y]
  (operation ->Power x y))

(defn sub [x y]
  (operation ->Sub x y))

;;(defn arange [start stop step dtype]
;;  (operation ->ARange start stop step dtype))

(defn argmax [x axis keep-dims]
  (operation ->ArgMax x axis keep-dims))

(defn batched-mm [x y]
  (operation ->BatchedMM x y))

(defn broadcast [x pattern]
  (operation ->Broadcast x pattern))

(defn cast [x dtype]
  (operation ->Cast x dtype))

(defn ceil-div [x y]
  (operation ->CeilDiv x y))

(defn concatenate [xs axis]
  (operation ->Concatenate xs axis))

(defn dot [x y]
  (operation ->Dot x y))

(defn einsum [desc x y]
  (operation ->EinSum desc x y))

(defn fill [val shape]
  (operation ->Fill val shape))

(defn flatten [x]
  (operation ->Flatten x))

(defn flip [x axes]
  (operation ->Flip x axes))

(defn floor-div [x y]
  (operation ->FloorDiv x y))

(defn subtensor [x slis]
  (operation ->Subtensor x slis))

(defn inc-subtensor [x slis y]
  (operation ->IncSubtensor x slis y))

(defn max [x axis keep-dims]
  (operation ->Max x axis keep-dims))

(defn mean [x axis keep-dims]
  (operation ->Mean x axis keep-dims))

(defn norm [x axis p keep-dims]
  (operation ->Norm x axis p keep-dims))

(defn ones [shape dtype]
  (operation ->Ones shape dtype))

(defn ones-like [x]
  (operation ->OnesLike x))

(defn outer [x]
  (operation ->Outer x))

(defn rand [shape]
  (operation ->Rand shape))

(defn randn [shape]
  (operation ->Randn shape))

#_(defn real [x]
  (operation ->Real x))

(defn repeat [x repeats axis]
  (operation ->Repeat x repeats axis))

(defn reshape [x shape]
  (operation ->Reshape x shape))

(defn shape [x]
  (operation ->Shape x))

(defn size [x axis]
  (operation ->Size x axis))

(defn stack [scalars]
  (operation ->Stack scalars))

(defn prod [x axis keep-dims]
  (operation ->Prod x axis keep-dims))

(defn sum [x axis keep-dims]
  (operation ->Sum x axis keep-dims))

(defn transpose [x axes]
  (operation ->Transpose x axes))

(defn tuple-index [x i]
  (operation ->TupleIndex x i))

(defn zeros [shape dtype]
  (operation ->Zeros shape dtype))

(defn zeros-like [x]
  (operation ->ZerosLike x))

(defn dim-shuffle [x pattern]
  (operation ->DimShuffle x pattern))

