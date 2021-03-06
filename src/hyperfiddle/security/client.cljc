(ns hyperfiddle.security.client
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.maybe :as maybe]
    [cats.monad.either :as either :refer [right]]
    [contrib.ct :refer [maybe]]
    [contrib.datomic]
    [contrib.eval :refer [eval-expr-str!+]]
    [contrib.reactive :as r]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security :as security]
    [taoensso.timbre :as timbre]))


; These security models don't compose yet, eventually they will need to

(def allow-anonymous
  {:subject-can-transact? (constantly (either/right))
   :can-create? (constantly true)
   :writable-entity? (constantly true)})

(def authenticated-users-only
  {:subject-can-transact? (fn [hf-db subject] (if (some? subject)
                                                (either/right)
                                                (either/left "Please login")))
   :can-create? (fn [hf-db subject ctx] (some? subject))
   :writable-entity? (fn [hf-db subject ctx] (some? subject))})

(let [owned-by? (fn [hf-db subject]
                  (-> (into #{} (:hyperfiddle/owners hf-db))
                      (contains? subject)))]
  (def owner-only
    {:subject-can-transact? (fn [hf-db subject] (if (owned-by? hf-db subject)
                                                  (either/right)
                                                  (either/left "Writes restricted")))
     :can-create? (fn [hf-db subject ctx] (owned-by? hf-db subject))
     :writable-entity? (fn [hf-db subject ctx] (owned-by? hf-db subject))}))

(def attr-whitelist
  {:subject-can-transact? (constantly (either/right))
   :can-create? (constantly false)
   :writable-entity? (fn [hf-db subject ctx]
                       ; Attribute whitelist is not implemented here, this is about entity level writes
                       true)})

(let [parent-m (fn parent-m [ctx]
                 (if (:db/isComponent (context/attr ctx (context/a ctx)))
                   (parent-m (:hypercrud.browser/parent ctx))
                   (hypercrud.browser.context/data ctx)))
      new-entity? (fn new-entity? [rt pid dbname dbid]
                    (or (contrib.datomic/tempid? dbid)
                        (some-> (get (runtime/get-tempid-lookup! rt pid dbname) dbid)
                                some?)
                        (if-let [parent-id (runtime/parent-pid rt pid)]
                          (new-entity? rt parent-id dbname dbid)
                          false)))]
  (def entity-ownership
    {:subject-can-transact? (fn [hf-db subject] (if (some? subject)
                                                  (either/right)
                                                  (either/left "Please login")))
     :can-create? (fn [hf-db subject ctx] (some? subject))
     :writable-entity? (fn [hf-db subject ctx]
                         (and (some? subject)
                              (or (contains? (set (:hyperfiddle/owners hf-db)) subject)
                                  (-> (mlet [m (maybe (parent-m ctx))
                                             dbname (maybe (context/dbname ctx))]
                                        (return (or (new-entity? (:runtime ctx) (:partition-id ctx) dbname (:db/id m))
                                                    (contains? (set (:hyperfiddle/owners m)) subject))))
                                      ; ui probably in an invalid/error state when m or uri are nil
                                      (maybe/from-maybe false)))))}))

(let [memoized-safe-eval-string (memoize eval-expr-str!+)]
  (defn- eval-client-sec [hf-db]
    (case (get-in hf-db [:database/write-security :db/ident] ::security/allow-anonymous) ; todo yank this default
      ::security/attr-whitelist (right attr-whitelist)
      ::security/allow-anonymous (right allow-anonymous)
      ::security/authenticated-users-only (right authenticated-users-only)
      ::security/owner-only (right owner-only)
      ::security/custom (memoized-safe-eval-string (:database.custom-security/client hf-db)))))

(defn subject-can-transact? [hf-db subject]
  (mlet [{:keys [subject-can-transact?]} (eval-client-sec hf-db)]
    (if subject-can-transact?
      (either/branch (try-either (subject-can-transact? hf-db subject))
                     (constantly (either/left "Misconfigured db security"))
                     identity)
      (either/right))))

(defn can-create? [ctx]
  (-> (mlet [:let [dbname (context/dbname ctx)
                   hf-db (-> (runtime/domain (:runtime ctx))
                             (domain/database dbname))
                   subject (runtime/get-user-id (:runtime ctx))]
             client-sec (eval-client-sec hf-db)
             :let [f (or (:can-create? client-sec) (constantly true))]]
        (try-either (f hf-db subject ctx)))
      (either/branch
        (fn [e]
          (timbre/error e)
          false)
        identity)))

(defn writable-entity? [ctx]
  (-> (mlet [:let [dbname (context/dbname ctx)
                   hf-db (-> (runtime/domain (:runtime ctx))
                             (domain/database dbname))
                   subject (runtime/get-user-id (:runtime ctx))]
             client-sec (eval-client-sec hf-db)
             :let [f (or (:writable-entity? client-sec) (constantly true))]]
        (try-either (f hf-db subject ctx)))
      (either/branch
        (fn [e]
          (timbre/error e)
          false)
        identity)))
