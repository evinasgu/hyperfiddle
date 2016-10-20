(ns hypercrud.client.http
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.graph :as graph]
            [hypercrud.client.internal :as internal]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]))


(def content-type-transit "application/transit+json;charset=UTF-8")
(def content-type-edn "application/edn;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (internal/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (internal/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-edn) [{:keys [form-params]}]
  (binding [pprint/*print-miser-width* nil
            pprint/*print-right-margin* 200]
    (with-out-str (pprint/pprint form-params))))


(defmethod kvlt.middleware/from-content-type (keyword content-type-edn) [resp]
  (let [decoded-val (reader/read-string (:body resp))]
    (assoc resp :body decoded-val)))


(defn resolve-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))


; graph is always assumed to be touched
(deftype Client [entry-uri schemas ^:mutable super-graph temp-id-atom]
  hc/Client
  (graphs [this]
    #_(assert (not (empty? super-graph)) "invariant - runtime must call hydrate! first")
    super-graph)


  (hydrate! [this named-queries]
    ;; compare our pre-loaded state with the graph dependencies
    (let [graph-we-want (graph/superGraph schemas named-queries)]
      (if (= super-graph graph-we-want)
        (p/resolved super-graph)
        #_(do
            (doseq [[q p _] (vals named-queries)]
              (assert (= (count (q-util/parse-holes q))
                         (count p))
                      (str "Missing parameters for " q))))
        (-> (kvlt/request!
              {:url (resolve-relative-uri entry-uri (goog.Uri. "hydrate"))
               :content-type content-type-edn               ; helps debugging to view as edn
               :accept content-type-edn                     ; needs to be fast so transit
               :method :post
               :form (into [] (vals named-queries))
               :as :auto})
            (p/then (fn [resp]
                      (let [{:keys [t pulled-trees-map]} (-> resp :body :hypercrud)]
                        (graph/set-state! graph-we-want pulled-trees-map t)
                        (set! super-graph graph-we-want)
                        super-graph)))))))


  (temp-id! [this conn-id]
    (swap! temp-id-atom update conn-id dec))


  (transact! [this tx]
    (-> (kvlt/request!
          {:url (resolve-relative-uri entry-uri (goog.Uri. "transact"))
           :content-type content-type-edn
           :accept content-type-edn
           :method :post
           :form tx
           :as :auto})
        (p/then (fn [resp]
                  (if (:success resp)
                    (p/resolved (-> resp :body :hypercrud))
                    (p/rejected resp)))))))
