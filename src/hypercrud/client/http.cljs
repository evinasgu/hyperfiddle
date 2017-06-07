(ns hypercrud.client.http
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [clojure.set :as set]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.internal :as internal]
            [hypercrud.client.response :as response]
            [hypercrud.util.core :as util]
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

(defn- hydrate! [entry-uri requests stage-val]
  (-> (kvlt/request! {:url (resolve-relative-uri entry-uri (goog.Uri. "hydrate"))
                      :content-type content-type-transit    ; helps debugging to view as edn
                      :accept content-type-transit          ; needs to be fast so transit
                      :method :post
                      :form {:staged-tx stage-val :request (into #{} requests)}
                      :as :auto})
      (p/then (fn [http-response]
                (let [{:keys [t pulled-trees-map]} (-> http-response :body :hypercrud)]
                  (response/->Response (into #{} requests) pulled-trees-map stage-val))))))

(deftype Peer [entry-uri stage ^:mutable last-response]
  hc/Peer
  ; why did 'force?' behavior change?
  (hydrate! [this request]
    #_(if (hc/hydrated? this request)                       ; this if check should be higher?
        (p/resolved last-response))
    (-> (hydrate! entry-uri request @stage)
        (p/then (fn [response]
                  (set! last-response response)
                  last-response))))

  ; for clone link - is this bad? yeah its bad since it can never be batched.
  (hydrate-one! [this request]
    (-> (hydrate! entry-uri #{request} @stage)
        (p/then (fn [peer] (hypercrud.client.core/hydrate peer request)))))


  (hydrated? [this requests]
    ; compare our pre-loaded state with the peer dependencies
    (set/subset? (set requests) (some-> last-response .-requests)))


  (transact! [this]
    (-> (kvlt/request!
          {:url (resolve-relative-uri entry-uri (goog.Uri. "transact"))
           :content-type content-type-edn
           :accept content-type-edn
           :method :post
           :form (->> @stage
                      (util/map-values (fn [branch-tx]
                                         (->> (get branch-tx nil)
                                              (filter (fn [[op e a v]]
                                                        (not (and (or (= :db/add op) (= :db/retract op))
                                                                  (nil? v)))))))))
           :as :auto})
        (p/then (fn [resp]
                  (if (:success resp)
                    ; clear master stage
                    ; but that has to be transactional with a redirect???
                    (p/resolved (-> resp :body :hypercrud))
                    (p/rejected resp)))))))
