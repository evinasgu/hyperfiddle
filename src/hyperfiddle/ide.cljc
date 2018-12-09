(ns hyperfiddle.ide
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [cats.labs.promise]
    #?(:cljs [contrib.css :refer [css]])
    [contrib.ct :refer [unwrap]]
    [contrib.ednish :refer [decode-ednish]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string+]]
    #?(:cljs [contrib.ui :refer [easy-checkbox radio-with-label]])
    [contrib.uri :refer [->URI]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :refer [request-from-route]]
    [hypercrud.browser.context :as context]
    #?(:cljs [hypercrud.ui.error :as ui-error])
    [hyperfiddle.actions :as actions]
    [hyperfiddle.data]
    [hyperfiddle.foundation :as foundation]
    #?(:cljs [hyperfiddle.ide.hf-live :as hf-live])
    [hyperfiddle.ide.system-fiddle :refer [system-fiddle?]]
    [hyperfiddle.io.hydrate-requests :refer [hydrate-one!]]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.schema :as schema]
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.ui.iframe :refer [iframe-cmp]])
    #?(:cljs [hypercrud.ui.stale :as stale])
    [taoensso.timbre :as timbre]

    ; pull in the entire ide app for reference from user-land
    #?(:cljs [hyperfiddle.ide.fiddles.domain])
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-src :refer [fiddle-src-renderer]])
    [hyperfiddle.ide.fiddles.schema]
    #?(:cljs [hyperfiddle.ide.fiddles.schema-attribute])
    #?(:cljs [hyperfiddle.ide.fiddles.topnav :as topnav])))


(defn domain [rt domain-eid]
  (let [domains-basis (->> (if-let [global-basis @(runtime/state rt [::runtime/global-basis])]
                             (:domain global-basis)
                             (->> @(runtime/state rt [::runtime/partitions])
                                  (some (fn [[_ partition]]
                                          (->> (:local-basis partition)
                                               (filter (fn [[k _]] (= foundation/domain-uri k)))
                                               seq)))))
                           (into {}))
        stage nil]
    (mlet [raw-domain (->> (foundation/domain-request domain-eid rt)
                           (hydrate-one! rt domains-basis stage))
           :let [domain (if (nil? (:db/id raw-domain))
                          ; terminate when domain not found
                          (throw (ex-info "Domain not found" {:hyperfiddle.io/http-status-code 404
                                                              :domain-eid domain-eid}))
                          (foundation/process-domain raw-domain))]]
      (return domain))))

(defn magic-ide-fiddle? [fiddle-ident domain-ident]
  (and (not= foundation/source-domain-ident domain-ident)
       ; Write this as s/conform
       (if (keyword? fiddle-ident) (= "hyperfiddle.ide" (namespace fiddle-ident)))))

(defn topnav-route [[fiddle datomic-args service-args frag :as route] ctx]
  ; Don't impact the request! Topnav can always use :target-route
  (let [ide-domain (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
        ?target-fiddle (if-not ide-domain [#entity["$" (base/legacy-fiddle-ident->lookup-ref fiddle)]])]
    [:hyperfiddle/topnav ?target-fiddle]))

(defn ide-route [[fiddle-ident :as route] ctx]
  (let [fiddle-ident (or @(runtime/state (:peer ctx) [::focused-fiddle])
                         fiddle-ident)
        params (when-not (magic-ide-fiddle? fiddle-ident (get-in ctx [:hypercrud.browser/domain :domain/ident]))
                 [[:fiddle/ident fiddle-ident]])]
    [:hyperfiddle/ide params]))

; ide is overloaded, these ide-context functions are exclusive to (top)
; despite being in the namespace (hyperfiddle.ide) which encompasses the union of target/user (bottom) and ide (top)
(defn- *-ide-context [ctx]
  (-> ctx
      (assoc :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))
      (context/source-mode)))

(defn leaf-ide-context [ctx]
  (*-ide-context ctx))

(defn page-ide-context [ctx]
  (-> (assoc ctx ::runtime/branch-aux {::foo "ide"})
      (*-ide-context)))

#?(:cljs
   (defn frame-on-click [rt [fiddle :as route] event]
     (when (and route (.-altKey event))                     ; under what circumstances is route nil?
       (let [native-event (.-nativeEvent event)
             anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
             anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
         (when-not (or anchor anchor-descendant)
           (.stopPropagation event)
           (runtime/dispatch! rt (partial actions/ide-focus fiddle))
           #_(js/window.open (foundation/route-encode rt route) "_blank"))))))

(defn- *-target-context [ctx]
  (assoc ctx
    :hyperfiddle.ui.iframe/on-click #?(:cljs (r/partial frame-on-click (:peer ctx)) :clj nil)
    :hypercrud.ui/display-mode (runtime/state (:peer ctx) [:display-mode])
    :hyperfiddle.ui/debug-tooltips (:active-ide? (runtime/host-env (:peer ctx)))))

(defn leaf-target-context [ctx]
  (*-target-context ctx))

(defn page-target-context [ctx]
  (-> (assoc ctx ::runtime/branch-aux {::foo "user"})
      (*-target-context)))

(defn local-basis [global-basis route ctx]
  ;local-basis-ide and local-basis-user
  (let [{:keys [ide user]} global-basis
        basis (case (get-in ctx [::runtime/branch-aux ::foo])
                "page" (concat ide user)
                "ide" (concat ide user)
                "user" (if (:active-ide? (runtime/host-env (:peer ctx)))
                         (merge user (select-keys ide [(->URI "datomic:free://datomic:4334/hyperfiddle-users")]))
                         user))
        basis (sort basis)]                                 ; Userland api-fn should filter irrelevant routes
    (timbre/debug (pr-str basis))
    #_(determine-local-basis (hydrate-route route ...))
    basis))

(defn hydrate-schemas [rt branch]
  (if (:active-ide? (runtime/host-env rt))
    (let [uris (concat [foundation/domain-uri
                        (->URI "datomic:free://datomic:4334/hyperfiddle-users")
                        @(runtime/state rt [::runtime/domain :domain/fiddle-database :database/uri])]
                       @(r/fmap->> (runtime/state rt [::runtime/domain :domain/databases])
                                   (map (fn [hf-db] (get-in hf-db [:domain.database/record :database/uri])))))]
      (schema/hydrate-schemas-for-uris rt branch uris))
    (schema/hydrate-schemas rt branch)))

; todo should summon route via context/target-route. but there is still tension in the data api for deferred popovers
(defn api [[fiddle :as route] ctx]
  {:pre [route (not (string? route))]}
  (case (get-in ctx [::runtime/branch-aux ::foo])
    "page" (let [ide-ctx (page-ide-context ctx)]
             (into
               (cond
                 (:active-ide? (runtime/host-env (:peer ctx)))
                 (concat (request-from-route (topnav-route route ctx) ide-ctx)
                         (request-from-route (ide-route route ctx) ide-ctx))

                 @(runtime/state (:peer ctx) [::runtime/domain :domain/environment :enable-hf-live?])
                 (request-from-route (ide-route route ctx) ide-ctx))
               (if (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
                 (request-from-route route ide-ctx)
                 (request-from-route route (page-target-context ctx)))))
    "ide" (request-from-route route (leaf-ide-context ctx))
    "user" (request-from-route route (leaf-target-context ctx))))

(defn read-fragment-only-hf-src [frag]
  (let [frag (some-> frag decode-ednish read-edn-string+ (->> (unwrap (constantly nil))))]
    (if (#{:hf.src} (some-> frag namespace keyword))
      frag)))

#?(:cljs
   (defn primary-content-ide [ide-ctx content-ctx route]
     (let [state (r/atom {:edn-fiddle false})]
       (fn [ide-ctx content-ctx route]
         [:div.row.hyperfiddle.hf-live.unp.no-gutters {:key "primary-content"}
          [:div.result.col-sm
           [:div "Result:"
            (into [:span.hyperfiddle.hf-live.radio-group]
                  (->> [{:label "edn" :tooltip "What the API client sees" :value :hypercrud.browser.browser-ui/api}
                        {:label "data" :tooltip "Ignore :fiddle/renderer" :value :hypercrud.browser.browser-ui/xray}
                        {:label "view" :tooltip "Use :fiddle/renderer" :value :hypercrud.browser.browser-ui/user}]
                       (map (fn [props]
                              [radio-with-label
                               (assoc props :checked (= (:value props) @(runtime/state (:peer ide-ctx) [:display-mode]))
                                            :on-change (r/comp (r/partial runtime/dispatch! (:peer ide-ctx)) actions/set-display-mode))]))))]
           [iframe-cmp content-ctx
            {:route route
             :class (css "hyperfiddle-user"
                         "hyperfiddle-ide"
                         "hf-live"
                         (some-> content-ctx :hypercrud.ui/display-mode deref name (->> (str "display-mode-"))))}]]
          (let [as-edn (r/cursor state [:edn-fiddle])]
            [:div.src.col-sm
             [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " edn" as-edn {:class "hyperfiddle hf-live"}]]
             [iframe-cmp ide-ctx
              {:route (ide-route route content-ctx)
               :initial-tab (let [[_ _ _ frag] route] (read-fragment-only-hf-src frag))
               :user-renderer (if @as-edn hf-live/result-edn fiddle-src-renderer)
               :class (css "devsrc" "hf-live")}]])]))))

#?(:cljs
   (defn primary-content-production "No ide layout markup" [content-ctx route]
     ^{:key :primry-content}
     [iframe-cmp content-ctx
      {:route (route/dissoc-frag route)
       :class (css "hyperfiddle-user"
                   "hyperfiddle-ide"
                   (some-> content-ctx :hypercrud.ui/display-mode deref name (->> (str "display-mode-"))))}]
     ))

#?(:cljs
   (defn view-page [[fiddle :as route] ctx]
     (let [ide-ctx (page-ide-context ctx)
           is-magic-ide-fiddle (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
           {:keys [:active-ide?]} (runtime/host-env (:peer ctx))
           is-editor (and active-ide?
                          (not is-magic-ide-fiddle)
                          (not (system-fiddle? fiddle))     ; Schema editor, console links
                          )]

       [:<> {:key "view-page"}

        ; Topnav
        (when active-ide?
          [iframe-cmp
           (assoc ide-ctx :hypercrud.ui/error (r/constantly ui-error/error-inline))
           {:route (topnav-route route ctx)
            :class "hidden-print"}])

        ; Primary content area
        (cond
          ; tunneled ide route like /hyperfiddle.ide/domain - primary, blue background (IDE),
          is-magic-ide-fiddle ^{:key :primary-content} [iframe-cmp ide-ctx {:route route :class "devsrc"}]

          is-editor [primary-content-ide ide-ctx (page-target-context ctx) route]

          :else [primary-content-production (page-target-context ctx) route]
          )])))

#?(:cljs
   (defn- view2 [ctx]
     (let [route (context/target-route ctx)]
       (case (get-in ctx [::runtime/branch-aux ::foo])
         "page" (view-page route ctx)                       ; component, seq-component or nil
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" [iframe-cmp (leaf-ide-context ctx) {:route route}]
         "user" [iframe-cmp (leaf-target-context ctx) {:route route}]))))

#?(:cljs
   (defn view [ctx]
     (let [code+ (foundation/eval-domain-code!+ (get-in ctx [:hypercrud.browser/domain :domain/code]))]
       [:<>
        (when (and (either/left? code+) (:active-ide? (runtime/host-env (:peer ctx))))
          (let [e @code+]
            (timbre/error e)
            (let [href (foundation/route-encode (:peer ctx) [:hyperfiddle.ide/domain [[:domain/ident (get-in ctx [:hypercrud.browser/domain :domain/ident])]]])
                  message (if-let [cause-message (some-> e ex-cause ex-message)]
                            cause-message
                            (ex-message e))]
              [:h6 {:style {:text-align "center" :background-color "lightpink" :margin 0 :padding "0.5em 0"}}
               "Exception evaluating " [:a {:href href} [:code ":domain/code"]] ": " message])))

        (if (and (either/left? code+) (not (:active-ide? (runtime/host-env (:peer ctx)))))
          ; todo this branch is fatal, so just stop the show, the admin needs to fix their app
          ; technically belongs in foundation, but debt
          [:div [:h2 {:style {:margin-top "10%" :text-align "center"}} "Misconfigured domain"]]
          ^{:key "view"}
          [view2 ctx])])))