(ns arch-view.model.components)

(defn- patterns
  [rule]
  (let [m (:match rule)]
    (cond
      (string? m) [m]
      (vector? m) m
      :else [])))

(defn- rule-matches?
  [module rule]
  (some #(re-find (re-pattern %) module)
        (patterns rule)))

(defn- module-component
  [rules module]
  (some (fn [rule]
          (when (rule-matches? module rule)
            (:component rule)))
        rules))

(defn assign-components
  [guidance modules]
  (let [rules (:component-rules guidance)]
    (into {}
          (for [module modules]
            [module (module-component rules module)]))))
