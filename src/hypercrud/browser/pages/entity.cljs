(ns hypercrud.browser.pages.entity
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :refer [cj-form]]
            [promesa.core :as p]))


(defn view [cur transact! graph metatype forms cmd-chan eid]
  "hypercrud values just get a form, with ::update and ::delete."
  (let [local-datoms (cur [:form] [])
        graph (hc/with graph @local-datoms)
        local-transact! #(swap! local-datoms tx-util/into-tx %)
        tempid! (hc/tempid!-factory)]
    [:div
     [cj-form graph eid metatype forms local-transact! tempid!]
     (if (tx-util/tempid? eid)
       ;[:button {:on-click #(go (>! cmd-chan [::create-item client href @form-cur]))} "Create"]
       nil
       [:button {:on-click #(go (>! cmd-chan [::update-item transact! @local-datoms]))}
        "Update"])]))


;; controller actions, are these "Commands" ?
;; can a command execute more commands? I think no
;; does a command have write access to global app state? i think yes,
;; for example, a command might trigger a page navigation.
;; does a command have access to encapsulated state? E.g. a component
;; might be on the page multiple times, and a command could change it's state.
;; So, maybe two types of commands, one that has encapsulation, and one that doesn't?

(def commands
  {::update-item
   (fn [transact! datoms]
     (->> (transact! datoms)
          (p/map (fn [resp]
                   (if (:success resp)
                     (js/alert "ok")
                     (js/alert "error"))))))

   ;::create-item
   #_(fn [client href cj-form]                              ; it would be great if the href was deducible from the cj-form
       (let [body {:template (-> cj-form :data)}]
         (->> (hypercrud/create client href body)
              (fmap (fn [resp]
                      (if (:success resp)
                        (js/alert "ok")
                        (js/alert "error")))))))})

