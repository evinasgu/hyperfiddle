(ns hyperfiddle.ide.fiddles.schema-editor
  (:require
    [hyperfiddle.runtime :as runtime]))


(defn renderer' [_ ctx props]
  (let [route-state (contrib.reactive/atom {:hide-datomic false
                                            :hide-archived false
                                            :needle nil})
        is-edn (contrib.reactive/atom false)]
    (fn [_ {:keys [:hypercrud.browser/route] :as ctx} props]
      [:<>
       (let [{:keys [hide-datomic hide-archived needle]} @route-state
             swap-route-state! (fn [k f]
                                 (let [{:keys [hide-datomic hide-archived needle]} (swap! route-state update k f)
                                       where (->> (cond->> []
                                                    hide-archived (cons '(not-join [?ident]
                                                                                   [(namespace ?ident) ?ns]
                                                                                   ; "zzz/" and "zzz.", we are inconsistent
                                                                                   [(.startsWith ?ns "zzz")]))
                                                    hide-datomic (cons '[(> ?attr 62)])
                                                    (seq needle) (into ['[(str ?ident) ?si]
                                                                        '[(.toLowerCase ?si) ?si']
                                                                        [(list '.contains '?si' (.toLowerCase needle))]]))
                                                  vec)]
                                   (->> (if (seq where)
                                          (assoc @route :hyperfiddle.route/where where)
                                          (dissoc @route :hyperfiddle.route/where))
                                        (runtime/set-route (:runtime ctx) (:partition-id ctx)))))]
         [:div.container-fluid.-hyperfiddle-ide-schema-editor props
          [:h3 (str "Datomic schema for " (let [ide-dbname (-> @(:hypercrud.browser/route ctx) :hyperfiddle.route/fiddle name (subs (count "editor")))]
                                            (-> (hyperfiddle.runtime/domain (:runtime ctx))
                                                :hyperfiddle.ide.domain/user-dbname->ide
                                                clojure.set/map-invert
                                                (get ide-dbname))))]
          [:div [:label [:input {:type "checkbox" :checked hide-datomic :on-change #(swap-route-state! :hide-datomic not)}] " hide Datomic system attributes"]]
          [:div [:label [:input {:type "checkbox" :checked hide-archived :on-change #(swap-route-state! :hide-archived not)}] " hide Hyperfiddle archived attributes"]]
          [:div [:label [:input {:type "checkbox" :checked @is-edn :on-change #(swap! is-edn not)}] " EDN view"]]
          [contrib.ui/debounced
           {:value needle
            :placeholder ":task/title"
            :on-change (fn [_ new-v] (swap-route-state! :needle (constantly new-v)))}
           contrib.ui/text]
          (let [ctx (assoc ctx :hyperfiddle.ui/layout :hyperfiddle.ui.layout/table)]
            (if @is-edn
              [contrib.ui/code {:value (-> (hyperfiddle.api/data ctx)
                                           (->> (map first)
                                                (sort-by :db/ident)
                                                (map #(dissoc % :db/id)))
                                           (contrib.pprint/pprint-str 1000))
                                :read-only true}]
              [hyperfiddle.ui/table
               (fn [ctx]
                 [(hyperfiddle.ui/field [0 :db/ident] ctx)
                  (hyperfiddle.ui/field [0 :db/valueType] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:disabled true})
                  (hyperfiddle.ui/field [0 :db/cardinality] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:disabled true})
                  (hyperfiddle.ui/field [0 :db/unique] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:disabled true})
                  (hyperfiddle.ui/field [0 :db/isComponent] ctx)
                  (hyperfiddle.ui/field [0 :db/fulltext] ctx nil {:disabled true})
                  (hyperfiddle.ui/field [0 :db/doc] ctx)])
               ctx
               {:hyperfiddle.ui.sort/initial-sort [[0 :db/ident] :asc]}]))])
       [hyperfiddle.ide/ide-stage ctx]])))

(defn renderer [& [_ ctx :as args]]
  (if (= "nodejs" *target*)
    (hyperfiddle.ui.loading/page (hyperfiddle.runtime/domain (:runtime ctx)))
    (into [renderer'] args)))
