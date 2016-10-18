(ns hypercrud.browser.core
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.dump :as dump]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.export-datoms :as export-datoms]
            [hypercrud.browser.pages.field :as field]
            [hypercrud.browser.pages.hydrate :as hydrate]
            [hypercrud.browser.pages.transact :as transact]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]))


(defn route [page-rel-path {:keys [query-fn entity-fn field-fn index-fn dump-fn export-fn hydrate-fn transact-fn else]}]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (and (= (second path-params) "query") (= 3 (count path-params)))
      (let [[form-id _ qp] path-params
            form-id (js/parseInt form-id 10)
            qp (reader/read-string (base64/decode qp))]
        (query-fn form-id qp))

      (and (= (second path-params) "entity"))
      (condp = (count path-params)
        3 (let [[form-id _ eid] path-params
                form-id (js/parseInt form-id 10)
                eid (js/parseInt eid 10)]
            (entity-fn eid form-id))
        4 (let [[form-id _ eid field-ident] path-params
                form-id (js/parseInt form-id 10)
                eid (js/parseInt eid 10)
                field-ident (keyword (subs (base64/decode field-ident) 1))]
            (field-fn eid form-id field-ident))
        :else (else))

      (and (= (first path-params) "dump") (= 2 (count path-params)))
      (dump-fn (js/parseInt (second path-params) 10))

      (and (= (first path-params) "export") (= 1 (count path-params)))
      (export-fn)

      (and (= (first path-params) "hydrate") (= 1 (count path-params)))
      (hydrate-fn)

      (and (= (first path-params) "transact") (= 1 (count path-params)))
      (transact-fn)

      (and (= (first path-params) "") (= 1 (count path-params)))
      (index-fn)
      :else (else))))


(defn ui [cur transact! graph forms queries links page-rel-path navigate! navigate-cmp param-ctx]
  [:div
   [:div.hc-node-view
    (route page-rel-path
           {:query-fn (fn [form-id qp]
                        (query/ui cur transact! graph forms queries form-id qp navigate! navigate-cmp param-ctx))
            :entity-fn (fn [eid form-id]
                         (entity/ui cur transact! graph eid forms queries form-id navigate! navigate-cmp param-ctx))
            :field-fn (fn [eid form-id field-ident]
                        (field/ui cur transact! graph eid forms queries form-id field-ident navigate-cmp))
            :index-fn #(index/ui links queries navigate-cmp param-ctx)
            :dump-fn (fn [id] (dump/ui graph id))
            :export-fn (fn [] (export-datoms/ui cur graph))
            :hydrate-fn (fn [] (hydrate/ui cur graph))
            :transact-fn (fn [] (transact/ui cur transact! graph))
            :else (constantly [:div "no route for: " page-rel-path])})]
   #_[:pre (pr-str param-ctx)]
   #_[:hr]
   #_[:pre (with-out-str (pprint/pprint @cur))]])


(defn query [schema forms queries state page-rel-path param-ctx]
  (route page-rel-path
         {:query-fn (fn [form-id qp]
                      (query/query state forms queries qp form-id param-ctx))
          :entity-fn (fn [eid form-id]
                       (entity/query state eid forms queries form-id param-ctx))
          :field-fn (fn [eid form-id field-ident]
                      (field/query state eid forms queries form-id field-ident param-ctx))
          :index-fn #(index/query)
          :dump-fn (fn [id] (dump/query id))
          :export-fn (fn [] (export-datoms/query state schema))
          :hydrate-fn (fn [] (hydrate/query state))
          :transact-fn (fn [] (transact/query))
          :else (constantly {})}))
