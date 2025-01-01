(ns clj-arsenal.vdom.tools.portfolio
  (:require
    [portfolio.core :as portfolio]
    [clj-arsenal.log :refer [spy]]))

 (def ^:private deps
  (some-> (requiring-resolve 'clojure.java.basis/current-basis)
    (apply nil)
    :argmap))

(defmacro defscene
  {:clj-kondo/lint-as 'clj-kondo.lint-as/def-catch-all}
  [id & opts]
  (when (portfolio/portfolio-active?)
    `(portfolio.data/register-scene!
       (clj-arsenal.vdom.tools.portfolio/create-scene
         ~(merge-with
            (fn [x y]
              (if (and (or (nil? x) (map? x)) (or (nil? y) (map? y)))
                (merge x y)
                y))
            (::opts deps)
            (portfolio/get-options-map id (:line &env) opts))))))

(defmacro configure-scenes
  [& opts]
  (when (portfolio/portfolio-active?)
    `(portfolio.data/register-collection!
       ~@(portfolio/get-collection-options opts))))
