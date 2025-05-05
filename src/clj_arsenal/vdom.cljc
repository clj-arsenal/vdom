(ns clj-arsenal.vdom
  (:require
   [clj-arsenal.basis :refer [dispose!]]
   [clj-arsenal.basis.once :refer [once]]
   [clj-arsenal.burp :as burp]
   [clj-arsenal.log :refer [spy]]
   #?(:cljd [cljd.core :refer [IFn IWatchable]]))
  (:import
   #?@(:cljd [] :clj [clojure.lang.IRef])))

(defprotocol Driver
  (-create-node [d burp-key parent-node data])
  (-before-update-node [d node])
  (-after-update-node [d node])
  (-set-prop! [d node k v])
  (-listen! [d node k listener opts])
  (-place-node! [d parent-node child-node index])
  (-remove-node! [d parent-node child-node])
  (-node-data [d node])
  (-set-node-data! [d node v])
  (-node-children [d node])
  (-focused-child [d node]))

(defprotocol DeriveWatchable
  (-derive-watchable [x node]))

(defprotocol DeriveListener
  (-derive-listener [x node]))

(extend-protocol DeriveWatchable
  #?(:cljs default :clj Object)
  (-derive-watchable
    [x _]
    (if #?(:cljs (satisfies? IWatchable x) :cljd (satisifes? IWatchable x) :clj (instance? IRef x))
      x
      (throw (ex-info "value does not satsify IWatchable or DeriveWatchable" {:value x})))))

(def ^:no-doc action?
  ; use ns-publics instead of resolve to avoid compiler warning
  (let [delayed-is-action-fn
        (delay
          (or (get (ns-publics 'clj-arsenal.action) 'action?)
            (constantly false)))]
    (fn action? [x]
      (@delayed-is-action-fn x))))

(extend-protocol DeriveListener
  #?(:cljs default :clj Object)
  (-derive-listener
    [x node]
    (if (or (ifn? x) (action? x))
      x
      (throw
        (ex-info "value does not satisfy IFn or DeriveListener, and is not an action"
          {:value x})))))

(defrecord ^:private BindValue [watchable-source opts])
(defrecord ^:private ListenKey [k opts])

(defn bind
  [watchable-source & {:as opts}]
  (->BindValue watchable-source opts))

(defn on
  [k & {:as opts}]
  (->ListenKey k opts))

(declare ^:private render-body! render-props! render-node!)

(defn- markup-value->burp
  [markup-value]
  (cond
    (string? markup-value)
    (burp/$ ::text {::value markup-value} nil)
    
    (burp/element? markup-value)
    markup-value

    :else
    (throw (ex-info "invalid markup value; expected string, seq, or burp element" {:value markup-value}))))

(defn render!
  [driver target markup]
  (render-body! driver target (cond-> markup (not (seq? markup)) list true burp/flatten-body)))

(defn- index-of
  [x coll]
  (reduce
    (fn [i next-val]
      (if (= next-val x)
        (reduced i)
        (inc i)))
    0
    coll))

(defn- render-body!
  [driver node children-markup]
  (let
    [children-markup (keep markup-value->burp children-markup)
     source-layout (vec (-node-children driver node))
     key->child-nodes (group-by #(::key (-node-data driver %)) source-layout)
     child-node-pools (update-vals key->child-nodes (once #(volatile! (seq %))))

     take-node
     (fn take-node [burp-element]
       (let [burp-key (:key burp-element)
             node-pool (get child-node-pools burp-key)]
         (if (or (nil? node-pool) (empty? @node-pool))
           (-create-node driver burp-key node {})
           (let [element-node (first @node-pool)]
             (vswap! node-pool rest)
             element-node))))

     target-layout (mapv take-node children-markup)]

    (doseq
      [[child-node markup] (map vector target-layout children-markup)]
      (render-node! driver child-node markup))

    (doseq
      [child-node-pool (vals child-node-pools)
       unused-child-node @child-node-pool]
      (-remove-node! driver node unused-child-node))

    (let
      [focused-child (-focused-child driver node)
       focused-child-target-index (when focused-child (index-of focused-child target-layout))]
      (cond
        (some? focused-child-target-index)
        (do
          (loop
            [child-nodes (subvec target-layout 0 focused-child-target-index)
             target-index 0]
            (when-some
              [[next-child-node & rest-child-nodes] (seq child-nodes)]
              (-place-node! driver node next-child-node target-index)
              (recur rest-child-nodes (inc target-index))))
          (when (< focused-child-target-index (count target-layout))
            (loop
              [child-nodes (subvec target-layout (inc focused-child-target-index))
               target-index (inc focused-child-target-index)]
              (when-some [[next-child-node & rest-child-nodes] (seq child-nodes)]
                (-place-node! driver node next-child-node target-index)
                (recur rest-child-nodes (inc target-index))))))

        :else
        (loop
          [child-nodes target-layout
           target-index 0]
          (when-some
            [[next-child-node & rest-child-nodes] (seq child-nodes)]
            (-place-node! driver node next-child-node target-index)
            (recur rest-child-nodes (inc target-index)))))))
  nil)

(defn- render-props!
  [driver node props]
  (let
    [data (-node-data driver node)
     old-props (::props data)
     unwatch-fns (volatile! (transient (or (::unwatch-fns data) {})))
     unlisten-fns (volatile! (transient (or (::unlisten-fns data) {})))]
    (doseq
      [[k v] props
       :let [old-v (get old-props k)]
       :when (not= v old-v)]
      (cond
        (instance? BindValue old-v)
        (do
          ((get @unwatch-fns k))
          (when (-> old-v :opts :dispose)
            (dispose! old-v))
          (vswap! unwatch-fns dissoc! k))

        (and (instance? ListenKey k) (some? old-v))
        (do
          ((get @unlisten-fns k))
          (vswap! unlisten-fns dissoc! k)))

      (cond
        (instance? BindValue v)
        (let
          [watchable (-derive-watchable (:watchable-source v) node)
           watch-k (gensym)]
          (-set-prop! driver node k @watchable)
          (add-watch watchable watch-k
            (fn [_ _ _ new-val]
              (-set-prop! driver node k new-val)))
          (vswap! unwatch-fns assoc! k
            #(remove-watch watchable watch-k)))

        (and (instance? ListenKey k) (some? v))
        (let
          [listener (-derive-listener v node)
           unlisten-fn (-listen! driver node (:k k) listener (:opts k))]
          (vswap! unlisten-fns assoc! k unlisten-fn))

        (keyword? k)
        (-set-prop! driver node k v)))
    (doseq
      [[old-k old-v] old-props 
       :when (not (contains? props old-k))]
      (cond
        (instance? BindValue old-v)
        (do
          ((get @unwatch-fns old-k))
          (when (-> old-v :opts :dispose)
            (dispose! old-v))
          (vswap! unwatch-fns dissoc! old-k)
          (-set-prop! driver node old-k nil))
        
        (and (instance? ListenKey old-k) (some? old-v))
        (do
          ((get @unlisten-fns old-k))
          (vswap! unlisten-fns dissoc! old-k))
        
        (keyword? old-k)
        (-set-prop! driver node old-k nil)))
    (-set-node-data! driver node
      (assoc data
        ::props props
        ::unwatch-fns (persistent! @unwatch-fns)
        ::unlisten-fns (persistent! @unlisten-fns))))
  nil)

(defn- render-node!
  [driver node burp-element]
  (-before-update-node driver node)
  (render-props! driver node (:props burp-element))
  (render-body! driver node (:body burp-element))
  (-after-update-node driver node)
  nil)
