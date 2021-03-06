(ns hyperfiddle.ui.docstring
  (:require
    [contrib.string :refer [blank->nil]]
    [cuerdas.core :as str]
    [hypercrud.browser.context :as context]))


(defn fqn->name [s]
  (-> s
      (str/split "\n" 2)
      (first)

      ; both cljs and js work with cljs eval
      (as-> s (if (str/includes? s "/")
                (str/split s "/")
                (str/split s ".")))
      last))

(defn attribute-schema-human [attr]
  ((juxt
     #_:db/ident
     #(some-> % :db/valueType name)
     #(some-> % :db/cardinality name)
     #(some-> % :db/isComponent (if :component) name)
     #(some-> % :db/unique name))
    attr))

(defn semantic-docstring [ctx & [doc-override]]
  (let [attr (context/attr ctx)
        typedoc (some->> (attribute-schema-human attr) (interpose " ") (apply str))
        help-md (blank->nil
                  ; Use path over a because it could have flattened the nesting and attr is ambiguous
                  (str (if typedoc (str "`" (:db/ident attr) " " typedoc "`\n\n")) ; markdown needs double line-break
                       (or doc-override (-> attr :db/doc blank->nil))))]
    help-md))
