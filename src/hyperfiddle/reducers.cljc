(ns hyperfiddle.reducers
  (:require [contrib.data :refer [map-values]]
            [contrib.datomic-tx :as tx]
            [contrib.pprint :refer [pprint-str]]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.util.branch :as branch]
            [hyperfiddle.route :as route]
            [hyperfiddle.state :as state]))


(defn- serializable-error [e]
  ; need errors to be serializable, so crapily pr-str
  (let [?message #?(:clj (.getMessage e) :cljs (ex-message e))]
    (cond
      (string? e) e
      ?message (assoc (->Err (str ?message)) :data (pprint-str (ex-data e)))
      :else (pr-str e))))

(defn fatal-error-reducer [error action & args]
  (case action
    :set-global-basis nil
    :set-error (serializable-error (first args))
    error))

(defn pressed-keys-reducer [v action & args]
  (or v #{}))

(defn global-basis-reducer [global-basis action & args]
  (case action
    :hydrate!-route-success (let [[branch ptm tempid-lookups new-local-basis] args]
                              (if (nil? branch)
                                (map-values (fn [sub-basis]
                                              (reduce-kv (fn [sub-basis uri t]
                                                           (if (contains? sub-basis uri)
                                                             (assoc sub-basis uri t)
                                                             sub-basis))
                                                         sub-basis
                                                         new-local-basis))
                                            global-basis)
                                global-basis))
    :set-global-basis (first args)
    global-basis))

(defn domain-reducer [domain action & args]
  (case action
    :hyperfiddle.runtime/set-domain (first args)
    domain))

(defn display-mode-reducer [display-mode action & args]
  (case action
    :toggle-display-mode (case display-mode
                           :hypercrud.browser.browser-ui/xray :hypercrud.browser.browser-ui/user
                           :hypercrud.browser.browser-ui/user :hypercrud.browser.browser-ui/xray)
    :set-display-mode (first args)
    (or display-mode :hypercrud.browser.browser-ui/user)))

(defn ide-focus-reducer [target-fiddle action & [fiddle :as args]]
  (case action
    :hyperfiddle.ide/focus-fiddle fiddle
    ; default target fiddle is extracted from content-domain route, which isn't known yet ?
    (or target-fiddle nil)))

(defn partitions-reducer [partitions action & args]
  (let [with (fn [partition uri tx]
               (let [schema (get-in partition [:schemas uri])]
                 (update-in partition [:stage uri] (partial tx/into-tx schema) tx)))]
    (->> (case action
           :transact!-success
           (let [[uris] args]
             (-> partitions
                 (assoc-in [nil :hydrate-id] "hack; dont flicker while page rebuilds")
                 (update-in [nil :stage] #(apply dissoc % uris))))

           :add-partition (let [[branch route branch-aux] args]
                            (update partitions branch
                                    (fn [current-branch]
                                      (if (route/compare-routes route (:route current-branch))
                                        (assoc current-branch :route route)
                                        {:route route
                                         :hyperfiddle.runtime/branch-aux branch-aux}))))

           :discard-partition (let [[branch] args]
                                (dissoc partitions branch))

           :partition-basis (let [[branch local-basis] args]
                              (assoc-in partitions [branch :local-basis] local-basis))

           :partition-schema (let [[branch schemas] args]
                               (assoc-in partitions [branch :schemas] schemas))

           :partition-route (let [[branch route] args]
                              (if (= route (get-in partitions [branch :route]))
                                partitions
                                (-> partitions
                                    (assoc-in [branch :route] route)
                                    #_(dissoc :error :ptm :tempid-lookups))))

           :with (let [[branch uri tx] args]
                   (update partitions branch with uri tx))

           :merge (let [[branch] args
                        parent-branch (branch/decode-parent-branch branch)]
                    (-> (reduce (fn [partitions [uri tx]]
                                  (update partitions parent-branch with uri tx))
                                partitions
                                (get-in partitions [branch :stage]))
                        (update branch dissoc :stage)))

           :hydrate!-start (let [[branch] args]
                             (update partitions branch
                                     (fn [partition]
                                       (assoc partition
                                         :hydrate-id
                                         (hash (select-keys partition [:hyperfiddle.runtime/branch-aux :route :stage :local-basis]))))))

           :hydrate!-success (let [[branch ptm tempid-lookups] args]
                               (update partitions branch
                                       (fn [partition]
                                         (-> partition
                                             (dissoc :error :hydrate-id)
                                             (assoc :ptm ptm
                                                    :tempid-lookups tempid-lookups)))))

           :hydrate!-shorted (let [[branch] args]
                               (update partitions branch dissoc :hydrate-id))

           :hydrate!-route-success (let [[branch ptm tempid-lookups new-basis] args]
                                     (update partitions branch
                                             (fn [partition]
                                               (-> partition
                                                   (dissoc :error :hydrate-id)
                                                   (assoc :local-basis new-basis
                                                          :ptm ptm
                                                          :tempid-lookups tempid-lookups)))))

           :partition-error (let [[branch error] args]
                              (update partitions branch
                                      (fn [partition]
                                        (-> partition
                                            (dissoc :hydrate-id)
                                            (assoc :error (serializable-error error))))))

           :open-popover (let [[branch popover-id] args]
                           (update-in partitions [branch :popovers] conj popover-id))

           :close-popover (let [[branch popover-id] args]
                            (update-in partitions [branch :popovers] disj popover-id))

           :reset-stage-branch (let [[branch v] args]
                                 (assoc-in partitions [branch :stage] v))

           :reset-stage-uri (let [[branch uri tx] args]
                              (assoc-in partitions [branch :stage uri] tx))

           (or partitions {}))
         (map-values (fn [partition]
                       ; apply defaults
                       (->> {:hydrate-id identity
                             :popovers #(or % #{})
                             :schemas identity

                             ; data needed to hydrate a partition
                             :route identity
                             :hyperfiddle.runtime/branch-aux identity
                             :stage (fn [multi-color-tx]
                                      (->> multi-color-tx
                                           (remove (fn [[uri tx]] (empty? tx)))
                                           (into {})))
                             :local-basis identity

                             ; response data of hydrating a partition
                             :error identity
                             :ptm identity
                             :tempid-lookups identity}
                            (reduce (fn [v [k f]] (update v k f)) partition)))))))

(defn auto-transact-reducer [auto-tx action & args]
  (case action
    :set-auto-transact (first args)
    :toggle-auto-transact (let [[uri] args]
                            (update auto-tx uri not))
    (or auto-tx {})))

(defn user-id-reducer [user-id action & args]
  (case action
    :set-user-id (first args)
    user-id))

(defn user-reducer [user action & args]
  (case action
    :set-user (first args)
    user))

(defn selected-uri-reducer [uri action & args]
  (case action
    :select-uri (first args)
    uri))

(def reducer-map {:hyperfiddle.runtime/fatal-error fatal-error-reducer
                  :hyperfiddle.runtime/domain domain-reducer
                  :hyperfiddle.runtime/global-basis global-basis-reducer
                  :hyperfiddle.runtime/partitions partitions-reducer
                  :hyperfiddle.runtime/auto-transact auto-transact-reducer
                  :hyperfiddle.runtime/user-id user-id-reducer
                  :hyperfiddle.runtime/user user-reducer

                  :hyperfiddle.ide/focused-fiddle ide-focus-reducer

                  ; user
                  :display-mode display-mode-reducer
                  :pressed-keys pressed-keys-reducer
                  :staging/selected-uri selected-uri-reducer})

(def root-reducer (state/combine-reducers reducer-map))
