(ns hypercrud.browser.query
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbId ->DbVal ->Entity]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form :as form]))


(defn pull-resultset [super-graph {find-elements :link/find-element :as link} create-new-find-elements resultset]
  (let [find-element-lookup (->> (mapv (juxt :find-element/name identity) find-elements)
                                 (into {}))]
    (->> (if (and (:link/single-result-as-entity? link) (= 0 (count resultset)))
           (let [local-result (->> find-elements
                                   (mapv (juxt :find-element/name #(get create-new-find-elements (:find-element/name %))))
                                   (into {}))]
             [local-result])
           resultset)
         (mapv (fn [result]
                 (->> result
                      (mapv (fn [[find-element-symbol entity-dbid]]
                              (let [connection (get-in find-element-lookup [find-element-symbol :find-element/connection :db/id :id])
                                    dbval (->DbVal connection nil)]
                                [find-element-symbol (hc/entity (hc/get-dbgraph super-graph dbval) entity-dbid)])))
                      (into {})))))))


(defn resultset-custom [resultset link param-ctx]
  (let [{resultset-renderer :value error :error} (eval (:link/renderer link))
        repeating-link-ctxs (->> (:link/link-ctx link)
                                 (mapv (juxt #(-> % :link-ctx/ident) identity))
                                 (into {}))
        param-ctx (assoc param-ctx
                    :link-fn (fn [ident label param-ctx]
                               (let [link-ctx (get repeating-link-ctxs ident)
                                     props (links/query-link link-ctx param-ctx)]
                                 [(:navigate-cmp param-ctx) props label param-ctx])))]
    [:div
     (if error
       [:pre (pprint/pprint error)]
       (try
         (resultset-renderer resultset link param-ctx)
         (catch :default e (pr-str e))))]))


(defn ui
  ([link-ctx param-ctx]
   (let [params-map (links/build-params-map link-ctx param-ctx)]
     [ui (:link-ctx/link link-ctx) params-map param-ctx]))
  ([{find-elements :link/find-element :as link}
    {query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
    {:keys [super-graph] :as param-ctx}]
   (let [q (some-> link :link/query reader/read-string)
         params-map (merge query-params (q-util/build-dbhole-lookup link))
         param-ctx (assoc param-ctx :query-params query-params)
         query-hole-names (q-util/parse-holes q)]
     (if-not (links/holes-filled? query-hole-names params-map)    ;todo what if we have a user hole?
       [:div
        [:div "Unfilled query holes"]
        [:pre (doall (with-out-str
                       (binding [pprint/*print-miser-width* 1] ; not working
                         (pprint/pprint (->> query-hole-names
                                             (mapv (juxt identity #(get params-map %)))
                                             (into {}))))))]]
       (let [resultset (pull-resultset super-graph link create-new-find-elements
                                       (let [params (q-util/build-params #(get params-map %) link param-ctx)
                                             pull-exp (form/query-pull-exp find-elements)
                                             query-value [q params pull-exp]]
                                         (hc/select super-graph (hash query-value))))]
         (if (empty? (:link/renderer link))
           (auto-control/resultset resultset link param-ctx)
           (resultset-custom resultset link param-ctx)))))))


(declare query)


(defn field-queries [param-ctx field]
  (let [{:keys [:attribute/valueType :attribute/isComponent]} (:field/attribute field)
        is-ref (= (:db/ident valueType) :db.type/ref)]
    ; if we are a ref we ALWAYS need the query from the field options
    ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
    (if (and is-ref (not isComponent))
      (if-let [options-link-ctx (:field/options-link-ctx field)]
        (let [param-ctx (assoc param-ctx :debug (str "field-options:" (:db/id field)))]
          (query options-link-ctx param-ctx false))))))


(defn form-option-queries "get the form options recursively for all expanded forms"
  [form param-ctx]
  (apply merge
         (mapv #(field-queries param-ctx %) (:form/field form))))


(defn dependent-queries [{find-elements :link/find-element :as link} resultset param-ctx]
  (let [inline-link-ctxs (->> (:link/link-ctx link)
                              (filter :link-ctx/render-inline?))]
    (->> resultset
         (mapcat (fn [result]
                   (let [param-ctx (assoc param-ctx :result result)
                         option-queries (mapv (fn [{form :find-element/form :as find-element}]
                                                (form-option-queries form param-ctx))
                                              find-elements)
                         inline-queries (mapv (fn [inline-link-ctx]
                                                (let [param-ctx (assoc param-ctx :debug (str "inline-query:" (.-dbid inline-link-ctx)))]
                                                  (query inline-link-ctx param-ctx true)))
                                              inline-link-ctxs)]
                     (concat option-queries inline-queries))))
         (apply merge))))


(defn query
  ([link-ctx param-ctx skip-recurse?]
   (let [params-map (links/build-params-map link-ctx param-ctx)]
     (query (:link-ctx/link link-ctx) params-map param-ctx skip-recurse?)))
  ([{find-elements :link/find-element :as link}
    {query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
    {:keys [super-graph] :as param-ctx}
    recurse?]
    ;"No need for link-ctx anymore by the time we are here, it is in the url as query params and stuff"
   (let [q (some-> link :link/query reader/read-string)
         params-map (merge query-params (q-util/build-dbhole-lookup link))
         param-ctx (assoc param-ctx :query-params query-params)]
     (if-not (links/holes-filled? (q-util/parse-holes q) params-map)
       (.log js/console (pr-str (->> (q-util/parse-holes q)
                                     (mapv (juxt identity #(get params-map %)))
                                     (into {}))))
       (let [result-query [q
                           (q-util/build-params #(get params-map %) link param-ctx)
                           (form/query-pull-exp find-elements)]]
         (merge
           {(hash result-query) result-query}
           (if recurse?
             (if-let [resultset (some->> (hc/select super-graph (hash result-query))
                                         (pull-resultset super-graph link create-new-find-elements))]
               (dependent-queries link resultset param-ctx)))))))))
