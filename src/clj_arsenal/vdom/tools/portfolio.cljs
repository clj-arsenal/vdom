(ns clj-arsenal.vdom.tools.portfolio
  (:require-macros clj-arsenal.vdom.tools.portfolio)
  (:require
    [clj-arsenal.vdom :refer [render!]]
    [clj-arsenal.vdom.browser :as browser-vdom]
    [portfolio.adapter :as adapter]))

(defn oset!
  [^js/Object obj k v]
  (js* "(~{}[~{}] = ~{})" obj (name k) v))

(def ^:private component-impl
  {`adapter/render-component
   (fn [{:keys [component links scripts] :as opts} el]
     (when el
       (let [doc ^js/Document (.-ownerDocument el)
             body ^js/HTMLBodyElement (.-body doc)]
         (doseq [props links]
           (let [link-el (.createElement doc "link")]
             (doseq [[k v] props]
               (oset! link-el k v))
             (.appendChild body link-el)))
         (doseq [props scripts]
           (let [script-el (.createElement doc "script")]
             (doseq [[k v] props]
               (oset! script-el k v))
             (.appendChild body script-el)))

         (render! (browser-vdom/driver doc) el component)))
     nil)})

(defn create-scene
  [scene]
  (adapter/prepare-scene scene component-impl))
