(ns hyperfiddle.ide.edit
  (:require
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [hypercrud.ui.error :as error-cmps]
    [cats.monad.either :as either :refer [branch]]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.preview.view :as preview]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.staging :as staging]))


(defn domain-error-display [ctx]
  (let [e (::domain-misconfigured ctx)]
    [:<>
     [:h2 "Domain misconfigured"]                           ; todo improve me
     [error-cmps/error-block e]]))

(defn load-user-domain-without-crashing [ctx]
  (let [user-domain+ (::ide-domain/user-domain+ (runtime/domain (:peer ctx)))]
    (branch
      user-domain+
      #(assoc ctx ::domain-misconfigured %)
      #(assoc ctx ::preview/user-domain %))))

(defn view [_ ctx props]                                    ; ctx better not be changing this high
  (let [ctx (load-user-domain-without-crashing ctx)
        user-runtime (preview/create-user-runtime ctx)
        preview-state (r/atom {:initial-render true
                               :is-refreshing true
                               :is-hovering-refresh-button false
                               :alt-key-pressed false
                               :display-mode :hypercrud.browser.browser-ui/user
                               ; specifically deref and re-wrap this ref on mount because we are tracking deviation from this value
                               :staleness (preview/ide-branch-reference user-runtime (:branch ctx))})
        user-ctx {:peer user-runtime
                  :branch (preview/build-user-branch-id (:branch ctx)) ; user-branch
                  ::preview/ide-branch (:branch ctx)
                  ::preview/preview-state preview-state
                  :hyperfiddle.ui/debug-tooltips true
                  :hypercrud.ui/display-mode (r/cursor preview-state [:display-mode])
                  :hyperfiddle.ui.iframe/on-click (r/partial preview/frame-on-click user-runtime)}]
    (fn [_ _ props]
      [:<>
       (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle/topnav)]
         ; Presence of ::user-domain signals to topnav to render preview controls
         [hyperfiddle.ide.fiddles.topnav/renderer ctx
          {:class (hyperfiddle.ui.iframe/auto-ui-css-class ctx)}
          (if (::preview/user-domain ctx)
            [preview/preview-toolbar user-ctx preview-state])])

       [:div (select-keys props [:class])
        (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle.ide/preview)]
          [:div {:class (hyperfiddle.ui.iframe/auto-ui-css-class ctx)}
           [staging/inline-stage user-ctx]
           (cond
             (::preview/user-domain ctx)
             [preview/preview-effects user-ctx (preview/compute-user-route ctx)]

             (::domain-misconfigured ctx)
             [domain-error-display ctx])])

        (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle/ide)]
          [:div.fiddle-editor-col
           [hyperfiddle.ide/ide-stage ctx]
           [hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer
            nil ctx
            {:initial-tab (-> @(:hypercrud.browser/route ctx) (get 3) hyperfiddle.ide/parse-ide-fragment)
             ;:initial-tab @(contrib.reactive/fmap-> (:hypercrud.browser/route ctx) (get 3) hyperfiddle.ide/parse-ide-fragment)
             :class (css "fiddle-editor devsrc" (hyperfiddle.ui.iframe/auto-ui-css-class ctx))}]])
        ]])))
