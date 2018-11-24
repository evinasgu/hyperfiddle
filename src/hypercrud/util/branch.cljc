(ns hypercrud.util.branch
  (:require [clojure.string :as str]
            [contrib.string :refer [blank->nil]]
            [hypercrud.types.DbVal :refer [#?(:cljs DbVal)]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest #?(:cljs EntityRequest)]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest #?(:cljs QueryRequest)]])
  #?(:clj
     (:import (hypercrud.types.DbVal DbVal)
              (hypercrud.types.EntityRequest EntityRequest)
              (hypercrud.types.QueryRequest QueryRequest))))


(defn decode-parent-branch "nil is a valid branch" [branch]
  (some->> branch (re-find #"(.*)`.*") second blank->nil))

(defn encode-branch-child
  "child-id-str can be anything, you coordinate it yourself, but you should choose it to be unique"
  [parent-branch child-id-str]
  (if (str/blank? child-id-str)
    parent-branch
    (str parent-branch "`" child-id-str)))

(defn get-all-branches [branch]
  (if branch
    (conj (get-all-branches (decode-parent-branch branch)) branch)
    [branch]))

(defn branch-val [uri branch partitions]
  (->> (get-all-branches branch)
       (reduce (fn [tx branch]
                 (concat tx (get-in partitions [branch :stage uri])))
               nil)
       hash))

(defn request->dbvals [request]
  (condp = (type request)
    EntityRequest [(:db request)]
    QueryRequest (filter #(= (type %) DbVal) (:params request))))
