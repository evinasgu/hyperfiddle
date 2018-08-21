(ns hypercrud.browser.system-link
  (:require
    [clojure.string :as string]
    [contrib.string :refer [blank->nil]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.system-fiddle :as system-fiddle]))


(defn ^:export system-link? [link-id]
  (and (keyword? link-id)
       (some-> (namespace link-id) (string/starts-with? "hyperfiddle.browser.system-link"))))

(def retract-formula
  "(fn [ctx multi-color-tx modal-route]
  {:tx {(:uri ctx) [[:db.fn/retractEntity @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:db/id])]]}})")

(defn body-links-for-field
  ([parent-fiddle dbname schema field path parent-has-id?]
   (let [path (or (some->> (::field/path-segment field) (conj path)) path) ; fe wrapping causes spaces in paths
         ?spath (blank->nil (string/join " " path))]
     (-> (->> (::field/children field)
              (filter ::field/data-has-id?)
              (mapcat (fn [child-field]
                        (body-links-for-field parent-fiddle dbname schema child-field path (::field/data-has-id? field)))))
         (cond->>
           (and (::field/data-has-id? field)
                (not= '* (::field/path-segment field)))
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "remove-" (hash path)))
                  :hypercrud/sys? true
                  :link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hyperfiddle/remove
                  :link/path ?spath
                  :link/render-inline? true
                  :link/fiddle system-fiddle/fiddle-blank-system-remove
                  :link/managed? true
                  :link/tx-fn retract-formula})

           (and (::field/data-has-id? field)
                (or (or (not (nil? (::field/path-segment field)))
                        (not= :entity (:fiddle/type parent-fiddle)))
                    (and (context/attribute-segment? (::field/path-segment field))
                         (not= '* (::field/path-segment field)))))
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "edit-" (hash path)))
                  :hypercrud/sys? true
                  :link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hyperfiddle/edit
                  :link/path ?spath
                  :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                  :link/managed? false})

           parent-has-id?
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                  :hypercrud/sys? true
                  :link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hyperfiddle/new
                  :link/path ?spath
                  :link/render-inline? true
                  :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                  :link/create? true
                  :link/managed? true}))))))

(defn- system-links-impl [parent-fiddle fields schemas]
  (->> fields
       (filter ::field/source-symbol)
       (mapcat (fn [field]
                 (let [dbname (str (::field/source-symbol field))
                       schema (get schemas dbname)]
                   (cond->> (body-links-for-field parent-fiddle dbname schema field [] false)
                     (not= :entity (:fiddle/type parent-fiddle))
                     (cons (let [path (::field/path-segment field)
                                 ?spath (blank->nil (str path)) #_(string/join " " path)]
                             {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                              :hypercrud/sys? true
                              :link/rel :hyperfiddle/new
                              :link/path ?spath
                              :link/render-inline? true
                              :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                              :link/create? true
                              :link/managed? true}))))))))

(defn system-links
  "All sys links can be matched and merged with user-links. Matching is determined by link/rel and link/path"
  [parent-fiddle field schemas]
  (if (::field/source-symbol field)
    (system-links-impl parent-fiddle [field] schemas)       ; karl was lazy and never untangled the fe wrapping
    (system-links-impl parent-fiddle (::field/children field) schemas)))
