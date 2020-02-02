(ns hypercrud.types.QueryRequest)


(defrecord QueryRequest [query params opts])
(defrecord EvalRequest [form pid route])
