Virtual DOM rendering.  Includes driver implementation for the
browser DOM, but the main vDOM rendering logic is target and
Clojure-dialect agnostic, just implement a driver for your
render target.

```clojure
(require '[clj-arsenal.vdom :refer [render! on bind]]')
(require '[clj-arsenal.vdom.browser :as browser-vdom])
(require '[clj-arsenal.burp :refer [burp]])

(def driver (browser-vdom/driver js/document))

(def !count (atom 0))

(render! driver js/document.body
  (burp
    [:button
      {:style {:display :flex}
       (on :click) #(swap! !count inc)}
      "Clicks: "
      [:input {:value (bind !count)}]]))
```

**Event Listeners:** attach event listeners by adding a `(on :event-key)`
key to the props map, with something that satisfies `DeriveListener` as
a value.  Functions satisfy this protocol, and actions satisfy it by default
when rendering to the browser DOM.

**Bindings:** bind any watchable and derefable thing, or anything that
satisfies `DeriveWatchable` to a non-listener property by wrapping it
in `(bind the-thing)`.  This sets the prop value to the current deref
value of `the-thing`, and updates the prop whenever `the-thing`'s value
changes.

The browser DOM driver renders most props as JavaScript properties on
the DOM node; with a few exceptions like `:style` and `:class`, which
are treated specially.  To render a prop to the node's attribute instead,
add an `a` namespace.  For example `:a/class`, `:a/href`, etc.  Most props
on SVG nodes and their children need to be rendered as attributes this way.
