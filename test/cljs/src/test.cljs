(ns test
  (:require
   ["happy-dom" :as happy-dom]
   [clj-arsenal.vdom :refer [bind on] :as vdom]
   [clj-arsenal.vdom.browser :refer [switch-focus!] :as vdom-browser]
   [clj-arsenal.burp :refer [burp $]]
   [clj-arsenal.basis :refer [chainable chain]]
   [clj-arsenal.check :refer [check expect] :as check]
   [clj-arsenal.action :refer [act]]))

(defn with-doc
  [f]
  (chainable
    (fn [continue]
      (try
        (let [win (happy-dom/Window.)]
          (chain (f (vdom-browser/driver (.-document win)) (.-document win))
            (fn [_]
              (-> win .-happyDOM .waitUntilComplete
                (.then
                  (fn []
                    (-> win .-happyDOM .close)
                    (continue nil)))))))
        (catch :default ex
          (continue ex))))))

(check ::simple
  (with-doc
    (fn [driver ^js/Document doc]
      (vdom/render! driver (.-body doc)
        ($ :body#my-body.my-class {:foo "bar"} "Hello, World!"))

      (expect = (-> doc .-body .-foo) "bar")
      (expect = (-> doc .-body .-textContent) "Hello, World!")
      (expect = (-> doc .-body .-id) "my-body")
      (expect true? (-> doc .-body .-classList (.contains "my-class")))

      (vdom/render! driver (.-body doc)
        ($ :body#other-id.other-class "Hello, Other!"))

      (expect = (-> doc .-body .-foo) nil)
      (expect = (-> doc .-body .-textContent) "Hello, Other!")
      (expect = (-> doc .-body .-id) "other-id")
      (expect = (-> doc .-body .-className) "other-class")

      (vdom/render! driver (.-body doc)
        ($ :body.class-1.class-2 {:class [:class-3 [:class-4]]}))

      (expect = (-> doc .-body .-textContent) "")
      (expect true? (-> doc .-body .-classList (.contains "class-1")))
      (expect true? (-> doc .-body .-classList (.contains "class-2")))
      (expect true? (-> doc .-body .-classList (.contains "class-3")))
      (expect true? (-> doc .-body .-classList (.contains "class-4"))))))

(check ::listeners
  (with-doc
    (fn [driver ^js/Document doc]
      (let [!click-count (atom 0)]
        (vdom/render! driver (.-body doc)
          (burp
            [:button
             {(on :click) #(swap! !click-count inc)}
             "Click Me!"]))
        (let [btn (.querySelector doc "button")]
          (expect some? btn)
          (dotimes [_ 7]
            (.click btn))
          (expect = @!click-count 7))))))

(check ::binding
  (with-doc
    (fn [driver ^js/Document doc]
      (let [!value (atom "hi")]
        (vdom/render! driver (.-body doc)
          (burp
            [:input {:value (bind !value)}]))
        (let [input (.querySelector doc "input")]
          (expect = @!value (.-value input))
          (reset! !value "bye")
          (expect = @!value (.-value input))
          
          (vdom/render! driver (.-body doc)
            (burp
              [:input]))
          
          (expect = input (.querySelector doc "input"))
          (expect = "" (.-value input)))))))

(check ::pass-focus
  (with-doc
    (fn [driver ^js/Document doc]
      (vdom/render! driver (.-body doc)
        (burp [:div {:tabIndex 0} [:input]]))
      (let [input (.querySelector doc "input")
            div (.querySelector doc "div")]
        (switch-focus! nil input)
        (expect = input (.-activeElement doc))

        (vdom/render! driver (.-body doc)
          (burp [:div {:tabIndex 0}]))

        (expect = div (.-activeElement doc))))))

(check ::keys
  (with-doc
    (fn [driver ^js/Document doc]
      (vdom/render! driver (.-body doc)
        (burp
          ^{:key 1} [:input#i1]
          ^{:key 2} [:input#i2]
          [:div]
          ^{:key 3} [:input#i3]))
      (let [[i1 i2 d i3] (array-seq (.-childNodes (.-body doc)))]
        (vdom/render! driver (.-body doc)
          (burp
            ^{:key 2} [:input#i2]
            [:div]
            ^{:key 3} [:input#i3]
            ^{:key 1} [:input#i1]))
        (let [[i2' d' i3' i1'] (array-seq (.-childNodes (.-body doc)))]
          (expect = i1 i1')
          (expect = i2 i2')
          (expect = i3 i3')
          (expect = d d'))))))

(check ::action-listener
  (let [!detail (atom nil)]
    (with-doc
      (fn [driver ^js/Document doc]
        (vdom/render! driver (.-body doc)
          (burp
            [:div
             [:button#b
              {(on :click) (act [:foo 1] [:bar 2])}
              "Click Me!"]]))

        (let [div (.querySelector doc "div")
              btn (.querySelector doc "button")]
          (.addEventListener div "clj-arsenal.action"
            (fn [^js/Event event]
              (reset! !detail (.-detail event))))

          (expect some? btn)

          (.click btn)
          (expect = (:action @!detail) (act [:foo 1] [:bar 2]))
          (expect = "click" (.-type ^js/Event (:source-event @!detail))))))))

(defn run
  []
  (check/report-all-checks-and-exit!))
