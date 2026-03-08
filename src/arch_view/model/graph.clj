;; mutation-tested: 2026-03-08
(ns arch-view.model.graph)

(defn make-graph
  [nodes edges]
  {:nodes (set nodes)
   :edges (set edges)})
