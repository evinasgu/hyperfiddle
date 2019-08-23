(ns hyperfiddle.io.datomic.hydrate-requests
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [cats.monad.exception :as exception]
    [clojure.set :as set]
    [clojure.string :as string]
    [contrib.data :refer [cond-let map-values parse-query-element]]
    [contrib.datomic]
    [contrib.pprint :refer [pprint-str]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.types.EntityRequest]
    [hypercrud.types.QueryRequest]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.bindings]                               ; userland
    [hyperfiddle.io.datomic :as d]
    [taoensso.timbre :as timbre])
  (:import
    (hypercrud.types.DbRef DbRef)
    (hypercrud.types.EntityRequest EntityRequest)
    (hypercrud.types.QueryRequest QueryRequest)))


(defrecord SecureDbWith [db id->tempid])

(defmulti parameter (fn [this & args] (class this)))

(defmethod parameter :default [this & args] this)

(defmethod parameter DbRef [dbval get-secure-db-with]
  (-> (get-secure-db-with (:dbname dbval) (:branch dbval)) :db))

(defmulti hydrate-request* (fn [this & args] (class this)))

(defmethod hydrate-request* EntityRequest [{:keys [e db pull-exp]} domain get-secure-db-with]
  (let [{pull-db :db} (get-secure-db-with (:dbname db) (:branch db))]
    (cond
      (nil? e) nil                                          ; This is probably an error, report it? Datomic says: (d/pull $ [:db/id] nil) => #:db{:id nil}
      (contrib.datomic/tempid? e) {:db/id e}                ; This introduces sloppy thinking about time!   https://github.com/hyperfiddle/hyperfiddle/issues/584
      :happy (d/pull pull-db {:selector pull-exp :eid e}))))

(defmethod hydrate-request* QueryRequest [{:keys [query params opts]} domain get-secure-db-with]
  (assert query "hydrate: missing query")
  (let [q (d/qf domain params)
        arg-map (-> (select-keys opts [:limit :offset])
                    (assoc :query query
                           :args (map #(parameter % get-secure-db-with) params)))]
    (q arg-map)))

; todo i18n
(def ERROR-BRANCH-PAST ":hyperfiddle.error/basis-stale Branching the past is currently unsupported, please refresh your basis by refreshing the page")

(defn build-get-secure-db-with+ [domain partitions-f db-with-lookup local-basis]
  {:pre [(map? local-basis)
         (not-any? nil? (vals local-basis))]}
  (letfn [(get-secure-db-from-branch+ [partitions pid dbname]
            {:pre [pid]}
            (if-not (get-in partitions [pid :is-branched])
              (get-secure-db-from-branch+ partitions (get-in partitions [pid :parent-pid]) dbname)
              (or (get-in @db-with-lookup [pid dbname])
                  (let [tx (get-in partitions [pid :stage dbname])
                        db-with+ (mlet [t (or (some-> (get local-basis dbname) exception/success)
                                              (exception/failure (ex-info (str "Basis not found") {:dbname dbname :local-basis local-basis})))
                                        init-db-with (if-let [parent-pid (get-in partitions [pid :parent-pid])]
                                                       (get-secure-db-from-branch+ partitions parent-pid dbname)
                                                       (exception/try-on
                                                         (let [$ (->> (domain/connect domain dbname)
                                                                      (d/with-db))]
                                                           {:db-with $
                                                            :secure-db (d/as-of $ t)})))]
                                   ; is it a history query? (let [db (if (:history? dbval) (d/history db) db)])
                                   (if (empty? tx)
                                     (cats/return init-db-with)
                                     (mlet [:let [{:keys [db-with id->tempid with?]} init-db-with]
                                            _ (if (and (not with?) (not= t (d/basis-t db-with)))
                                                ; can only run this assert once, on the first time a user d/with's
                                                ; every subsequent d/with, the new db's basis will never again match the user submitted basis
                                                ; however this is fine, since the original t is already known good
                                                (exception/failure (RuntimeException. ERROR-BRANCH-PAST))
                                                (exception/success nil))
                                            _ (let [validate-tx (constantly true)] ; todo look up tx validator
                                                (if (validate-tx tx)
                                                  (exception/success nil)
                                                  (exception/failure (RuntimeException. (str "staged tx for " dbname " failed validation")))))
                                            {:keys [db-after tempids]} (exception/try-on (d/with db-with {:tx-data tx}))]
                                       ; as-of/basis-t gymnastics:
                                       ; https://gist.github.com/dustingetz/39f28f148942728c13edef1c7d8baebf/ee35a6af327feba443339176d371d9c7eaff4e51#file-datomic-d-with-interactions-with-d-as-of-clj-L35
                                       ; https://forum.datomic.com/t/interactions-of-d-basis-t-d-as-of-d-with/219
                                       (exception/try-on
                                         {:db-with db-after
                                          :secure-db (d/as-of db-after (d/basis-t db-after))
                                          :with? true
                                          ; todo this merge is excessively duplicating data to send to the client
                                          :id->tempid (merge id->tempid (set/map-invert tempids))}))))]
                    (swap! db-with-lookup assoc-in [pid dbname] db-with+)
                    db-with+))))]
    (fn [dbname pid]
      {:pre [dbname pid]}
      (->> (get-secure-db-from-branch+ (partitions-f) pid dbname)
           (cats/fmap (fn [internal-secure-db]
                        (->SecureDbWith (:secure-db internal-secure-db) (:id->tempid internal-secure-db))))))))

; this is legacy garbage
; todo just catch and generate exceptions inside get-secure-db-with and hydrate-request*
(defn- error-cleaner [e req]
  (timbre/error e)                                          ; log it before this "cleaner" destroys all useful information, shouldn't be necessary
  (let [error-str (str e)
        parsed-soup (cond-let
                      [msg #_(re-find #"(?s)^.+ :db.error/datoms-conflict (.+)$" error-str)
                       (some-> (string/split error-str #"^.+ :db\.error/datoms-conflict ") second)]
                      [:db.error/datoms-conflict msg
                       "Hint: Hyperfiddle has generated an invalid Datomic transaction. If you are using the staging area, this is
                       probably a 'merge conflict' which must be reconciled by editing the staging area by hand. If it is a buggy
                       widget, please file an issue."]
                      [[match msg] (re-find #"^.+ :db.error/invalid-entity-id (.+)$" error-str)] [:db.error/invalid-entity-id msg]
                      [[match msg] (re-find #"^.+ :db.error/insufficient-binding (.+)$" error-str)]
                      [:db.error/insufficient-binding msg
                       (str (some-> req .-query pprint-str))] ; query as whitespace string should be available
                      [[match msg] (re-find #"^.+ :db.error/not-a-data-function (.+)$" error-str)] [:db.error/not-a-data-function msg]
                      [[match msg] (re-find #"^.+ :db.error/not-an-entity (.+)$" error-str)]
                      [:db.error/not-an-entity msg
                       "Hint: If this is a schema attribute, does it exist?
                       This can happen if you both create a schema entity and use it in the
                       same transaction. Transact the schema before using it. Also try auto-transact."]
                      [[match msg] (re-find #"^.+ :db.error/wrong-type-for-attribute (.+)$" error-str)] [:db.error/wrong-type-for-attribute msg]
                      [[match msg] (re-find #"^.+ :hyperfiddle.error/basis-stale (.+)$" error-str)] [:hyperfiddle.error/basis-stale msg]

                      ; It is the Clojure way.
                      [[match msg] (re-find #"^com.google.common.util.concurrent.UncheckedExecutionException: java.lang.IllegalArgumentException: (.+)$" error-str)] [:hyperfiddle.error/invalid-pull msg]
                      [[match msg] (re-find #"^.+ message: Unable to find data source: (.+)$" error-str)]
                      (let [expected (parse-query-element (some-> req .-query) :in)]
                        [:hyperfiddle.error/query-arity
                         (str "Query argument missing: " msg " which corresponds with " expected ".\n"
                              "Hint: add a link/formula or edit the URL.")]))]
    (if-let [[ident error-msg human-hint] parsed-soup]
      (ex-info (str ident) {:ident ident
                            :error-msg error-msg
                            :human-hint human-hint})

      (ex-info (str :hyperfiddle.error/unrecognized)
               {:ident :hyperfiddle.error/unrecognized
                :error-msg error-str
                :human-hint
                (str "Please comment this error at [hyperfiddle#170](https://github.com/hyperfiddle/hyperfiddle/issues/170) so we can match it."
                     "\n\n```\n" (pr-str req) "\n```")}))))

(defn- extract-tempid-lookup+ [internal-secure-db+]
  ; todo function should fall out with error-cleaning
  (if (exception/success? internal-secure-db+)
    (cats/fmap :id->tempid internal-secure-db+)
    (exception/failure (error-cleaner (cats/extract internal-secure-db+) nil))))

(defn extract-tempid-lookups [db-with-lookup pid]
  (->> (get @db-with-lookup pid)
       (map-values extract-tempid-lookup+)))

(defn hydrate-request [domain get-secure-db-with+ request ?subject]
  {:pre [(or (instance? EntityRequest request) (instance? QueryRequest request))]}
  (binding [hyperfiddle.io.bindings/*subject* ?subject]
    (either/branch-left
      (try-either (hydrate-request* request domain (comp exception/extract get-secure-db-with+)))
      (fn [e] (either/left (error-cleaner e request))))))

(defn hydrate-requests [domain local-basis requests partitions ?subject]
  {:pre [requests
         (not-any? nil? requests)
         (every? #(or (instance? EntityRequest %) (instance? QueryRequest %)) requests)]}
  (let [db-with-lookup (atom {})
        local-basis (into {} local-basis)                   ; :: ([dbname 1234]), but there are some duck type shenanigans happening
        get-secure-db-with+ (build-get-secure-db-with+ domain (constantly partitions) db-with-lookup local-basis)
        pulled-trees (->> requests
                          (map #(hydrate-request domain get-secure-db-with+ % ?subject))
                          (doall))
        tempid-lookups (map-values #(map-values extract-tempid-lookup+ %) @db-with-lookup)]
    {:pulled-trees pulled-trees
     :tempid-lookups tempid-lookups}))
