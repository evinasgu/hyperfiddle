(ns hyperfiddle.schema
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [cats.monad.exception :as exception]
    [contrib.datomic]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.runtime :as runtime]
    [promesa.core :as p]))


(defn hydrate-schemas [rt pid local-basis partitions]
  (let [dbnames (-> (domain/databases (runtime/domain rt))
                    keys
                    vec)
        requests (mapv (fn [dbname]
                         (->QueryRequest '[:find (pull ?attr [*
                                                              {:db/valueType [:db/ident]
                                                               :db/cardinality [:db/ident]
                                                               :db/unique [:db/ident]}])
                                           :where [:db.part/db :db.install/attribute ?attr]]
                                         [(runtime/db rt pid dbname)]
                                         {:limit -1}))
                       dbnames)]
    (-> (io/hydrate-requests (runtime/io rt) local-basis partitions requests)
        (p/then (fn [{:keys [pulled-trees]}]
                  (->> pulled-trees                         ; add local schemas from spec here?
                       (map (fn [pulled-tree+]
                              (-> (cats/fmap #(contrib.datomic/indexed-schema (mapv first %)) pulled-tree+)
                                  (either/branch exception/failure exception/success))))
                       (zipmap dbnames)))))))
