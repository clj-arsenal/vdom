(ns clj-arsenal.vdom.browser
  (:require
   [clojure.string :as str]
   [clj-arsenal.vdom :refer [Driver] :as vdom]
   [clj-arsenal.burp :as burp]
   [clj-arsenal.log :refer [spy]]
   [clj-arsenal.basis :refer [signal? sig-listen sig-unlisten]]
   [clj-arsenal.basis.protocols.dispose :refer [Dispose]]))

(defn node-type-keyword->element-name
  [kw]
  (->
    (if-some [ns' (namespace kw)]
      (str ns' "." (name kw))
      (name kw))
    (str/replace #"[^A-Za-z0-9._-]+" munge)
    str/lower-case))

(defn- set-node-data!
  [^js/Node node data]
  (set! (.-cljArsenalVDomNodeData node) data))

(defn switch-focus!
  [^js/Node from ^js/Node to]
  (let [FocusEvent (-> to .-ownerDocument .-defaultView .-FocusEvent)]
    (when from
      (.dispatchEvent from (FocusEvent. "blur" #js{:relatedTarget to}))
      (.dispatchEvent from (FocusEvent. "focusout" #js{:bubbles true :composed true :relatedTarget to})))
    (.focus to)
    (.dispatchEvent to (FocusEvent. "focus" #js{:relatedTarget from}))
    (.dispatchEvent to (FocusEvent. "focusin" #js{:bubbles true :composed true :relatedTarget from})))
  nil)

(defn- try-pass-focus!
  [^js/Node from ^js/Node to]
  (let [tab-index (.-tabIndex to)
        focus-fn (.-focus to)]
    (cond
      (and (some? tab-index) (not (neg? tab-index)) (fn? focus-fn))
      (switch-focus! from to)
      

      (= 11 (.-nodeType to))
      (some->> to .-host (try-pass-focus! from))

      (or (nil? to) (= 9 (.-nodeType to)))
      nil

      :else
      (some->> to .-parentNode (try-pass-focus! from)))))

(defn- action-event-listener-fn
  [^js/Node node action]
  (fn [^js/Event event]
    (when-some [CustomEvent (some-> node .-ownerDocument .-defaultView .-CustomEvent)]
      (let [headers (:headers action)
            action-event (new CustomEvent
                           "clj-arsenal.action"
                           #js{:bubbles true
                               :composed true
                               :cancelable true
                               :detail {:action action :source-event event}})]

        (when-not (:no-prevent-default headers)
          (.preventDefault event))
        (when-not (:no-stop-propagation headers)
          (.stopPropagation event))

        (.dispatchEvent node action-event)))))

(defn driver
  [^js/Document doc]
  (let [!parent-node->focused-child (volatile! {})
        aborter (js/AbortController.)]
    (.addEventListener doc "focusin"
      (fn [^js/Event event]
        (vreset! !parent-node->focused-child
          (into {}
            (map vec)
            (partition 2 1 (reverse (.composedPath event))))))
      #js{:signal (.-signal aborter)
          :capture true})
    (reify
      Dispose
      (-dispose
        [_]
        (.abort aborter))

      Driver
      (-create-node
        [_ burp-key parent-node data]
        (let [node-type (:operator burp-key)]
          (when-not (keyword? node-type)
            (throw (ex-info "invalid node type, must be a keyword" {:type node-type})))

          (doto
            (case node-type
              ::vdom/text (.createTextNode doc "")
              :svg (.createElementNS doc "http://www.w3.org/2000/svg" "svg")
              (.createElementNS doc
                (or (some-> parent-node .-namespaceURI)
                  "http://www.w3.org/1999/xhtml")
                (node-type-keyword->element-name node-type)))
            (set-node-data! (assoc data ::vdom/key burp-key)))))

      (-before-update-node
        [_ node]
        (when (= (.-nodeType ^js/Node node) 1)
          (set! (.-cljArsenalVDomOldClasses ^js/Node node) (.-cljArsenalVDomClasses ^js/Node node))
          (set! (.-cljArsenalVDomClasses ^js/Node node) #{}))
        nil)

      (-after-update-node
        [_ node]
        (when (= (.-nodeType ^js/Node node) 1)
          (let [new-classes (.-cljArsenalVDomClasses ^js/Node node)]
            (cond
              (empty? new-classes)
              (.removeAttribute ^js/Node node "class")

              :else
              (.setAttribute ^js/Node node "class" (str/join " " new-classes)))))
        nil)

      (-set-prop!
        [_ node k v]
        (cond
          (= "a" (namespace k))
          (if (some? v)
            (.setAttribute node (name k) (str v))
            (.removeAttribute node (name k)))

          :else
          (case k
            ::vdom/value (set! (.-nodeValue ^js/Node node) v)
            (:class ::burp/classes) (set! (.-cljArsenalVDomClasses ^js/Node node)
                                      (into (.-cljArsenalVDomClasses ^js/Node node)
                                        (comp (map name) (remove str/blank?))
                                        (if (coll? v) (flatten (seq v)) (str/split (str v) #"\s+"))))
            ::burp/id (set! (.-id node) (some-> v str))
            :style (let [old-v (-> ^js/Node node .-cljArsenalVDomNodeData ::vdom/props :style)
                         style-obj (.-style ^js/Node node)]
                     (cond
                       (and (map? v) (map? old-v))
                       (do
                         (doseq [[style-k style-v] v :when (not= style-v (get old-v style-k))]
                           (if (nil? style-v)
                             (.removeProperty style-obj (name style-k))
                             (.setProperty style-obj
                               (name style-k)
                               (if (keyword? style-v) (name style-v) (str style-v)))))
                         (doseq [old-style-k (keys old-v) :when (not (contains? v old-style-k))]
                           (.removeProperty style-obj (name old-style-k))))

                       (map? v)
                       (do
                         (set! (.-cssText style-obj) "")
                         (doseq [[style-k style-v] v :when (some? v)]
                           (.setProperty style-obj
                             (name style-k)
                             (if (keyword? style-v) (name style-v) (str style-v)))))

                       (nil? v)
                       (.removeAttribute ^js/Node node "style")

                       :else
                       (set! (.-cssText style-obj) (str v))))
            (aset node (name k) v))))

      (-listen!
        [_ node k listener opts]
        (let [f (cond
                  (fn? listener)
                  listener

                  (ifn? listener)
                  #(listener %)

                  :else
                  (or
                    (when-some [action? (resolve 'clj-arsenal.action/action?)]
                      (when (action? listener)
                        (action-event-listener-fn node listener)))
                    (throw
                      (ex-info "invalid listener value, must be a function or an action"
                        {:listener listener}))))]
          (cond
            (signal? k)
            (let [listen-key (gensym)]
              (sig-listen k listen-key #(f #js{:target node :signal k}))
              #(sig-unlisten k listen-key))

            (keyword? k)
            (let [aborter (js/AbortController.)]
              (.addEventListener node (name k) f
                #js{:capture (:capture opts)
                    :signal (.-signal aborter)})
              #(.abort aborter))

            :else
            (throw (ex-info "invalid listener key, must be keyword or signal" {:k k})))))

      (-place-node!
        [_ parent-node child-node index]
        (let [current-child-at-index (.item (.-childNodes ^js/Node parent-node) index)]
          (when-not (= child-node current-child-at-index)
            (if (nil? current-child-at-index)
              (.appendChild ^js/Node parent-node child-node)
              (.insertBefore ^js/Node parent-node child-node current-child-at-index)))))

      (-remove-node!
        [_ parent-node child-node]
        (when (= (get @!parent-node->focused-child parent-node) child-node)
          (try-pass-focus! (some-> child-node .getRootNode .-activeElement) parent-node))
        (.removeChild ^js/Node parent-node child-node))

      (-node-data
        [_ node]
        (.-cljArsenalVDomNodeData ^js/Node node))

      (-set-node-data!
        [_ node data]
        (set-node-data! node data)
        nil)

      (-node-children
        [_ node]
        (array-seq (.-childNodes ^js/Node node)))

      (-focused-child
        [_ node]
        (get @!parent-node->focused-child node)))))
