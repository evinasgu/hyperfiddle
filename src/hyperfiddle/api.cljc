(ns hyperfiddle.api                                         ; cljs can always import this
  (:require
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [unqualify]]
    [contrib.reactive :as r]
    [taoensso.timbre :as timbre]))

(defprotocol Browser
  (a [ctx])
  (attr [ctx])
  (browse-element [ctx i])
  (data [ctx])
  (eav [ctx])
  (e [ctx])
  (element [ctx])
  (element-type [ctx])
  (fiddle [ctx])
  (identity? [ctx])
  (link-tx [ctx])
  (qfind [ctx])
  (qfind-level? [ctx])
  (id [ctx pulltree])
  (row-key [ctx row])
  (tempid! [ctx] [ctx dbname])
  (v [ctx]))

(defprotocol UI
  (display-mode [ctx])
  (display-mode? [ctx k]))

(declare render-dispatch)

(defmulti tx (fn [ctx eav props]
               (let [dispatch-v (hyperfiddle.api/link-tx ctx)]
                 ; UX - users actually want to see this in console
                 (timbre/info "hf/tx: " dispatch-v " eav: " (pr-str eav))
                 dispatch-v)))

; Dispatch is a set
(defmulti render (fn [ctx props]
                   (render-dispatch ctx props)))

(defn extract-set [ctx & fs]
  (->> ctx ((apply juxt fs)) set))

(defn render-dispatch [ctx props]
  ; Is there a method which is a subset of what we've got?
  (or
    (if (hyperfiddle.api/display-mode? ctx :user)
      (or
        (let [d (extract-set ctx hyperfiddle.api/fiddle hyperfiddle.api/a)]
          (if (contains? (methods render) d)
            d))
        (let [d (extract-set ctx hyperfiddle.api/a)]
          (if (contains? (methods render) d)
            d))))
    ; Legacy compat - options by fiddle/renderer explicit props route to select via ref renderer
    (if (:options props)
      (extract-set (hyperfiddle.api/attr ctx) :db/valueType :db/cardinality))
    (if (hyperfiddle.api/identity? ctx) #{:db.unique/identity})
    (if-let [attr (hyperfiddle.api/attr ctx)]
      (extract-set attr :db/valueType :db/cardinality))
    (if (hyperfiddle.api/element ctx)
      (extract-set ctx hyperfiddle.api/element-type))                    ; :hf/variable, :hf/aggregate, :hf/pull
    ;(contrib.datomic/parser-type (context/qfind ctx))       ; :hf/find-rel :hf/find-scalar
    ;:hf/blank
    ))

#?(:clj (def ^:dynamic *route* nil))
#?(:clj (def ^:dynamic *$* nil))

(defmethod tx :default [ctx eav props]
  nil)

(defmethod tx :default [ctx eav props]
  nil)

(defmethod tx :zero [ctx eav props]
  [])                                                       ; hack to draw as popover

(defmethod tx :db/add [ctx [e a v] props]
  {:pre [e a v]}
  [[:db/add e a v]])

(defmethod tx :db/retract [ctx [e a v] props]
  {:pre [e a v]}
  [[:db/retract e a v]])

(defmethod tx :db.fn/retractEntity [ctx [e a v] props]
  {:pre [v]}
  [[:db.fn/retractEntity v]])

; Compat

(defmulti txfn (fn [user-txfn e a v ctx] user-txfn))

(defmethod txfn :default [_ e a v ctx]
  nil)

(defmethod txfn :zero [_ e a v ctx]
  [])                                                       ; hack to draw as popover

(defmethod txfn :db/add [_ e a v ctx]
  {:pre [e a v]}
  [[:db/add e a v]])

(defmethod txfn :db/retract [_ e a v ctx]
  {:pre [e a v]}
  [[:db/retract e a v]])

(defmethod txfn :db.fn/retractEntity [_ _ _ v ctx]
  {:pre [v]}
  [[:db.fn/retractEntity v]])
