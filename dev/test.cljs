(ns test
  (:require
   ["happy-dom" :as happy-dom]
   [clj-arsenal.vdom :refer [bind on] :as vdom]
   [clj-arsenal.vdom.browser :refer [switch-focus!] :as vdom-browser]
   [clj-arsenal.burp :refer [burp $]]
   [clj-arsenal.basis.protocols.chain :refer [chainable]]
   [clj-arsenal.check :refer [check expect]]
   [clj-arsenal.log :refer [spy]]))

(comment
  (require 'shadow.cljs.devtools.api)
  (shadow.cljs.devtools.api/repl :dev))

(defn with-doc
  [f]
  (chainable
    (fn [continue]
      (try
        (let [win (happy-dom/Window.)]
          (f (vdom-browser/driver (.-document win)) (.-document win))
          (-> win .-happyDOM .waitUntilComplete
            (.then
              (fn []
                (-> win .-happyDOM .close)
                (continue nil)))))
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
            (.dispatchEvent btn (js/Event. "click")))
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

(defn run
  []
  )
