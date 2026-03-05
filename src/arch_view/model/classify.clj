(ns arch-view.model.classify)

(defn- rule-patterns
  [rule]
  (let [m (:match rule)]
    (cond
      (string? m) [m]
      (vector? m) m
      :else [])))

(defn- matches-rule?
  [module rule]
  (some #(re-find (re-pattern %) module)
        (rule-patterns rule)))

(defn- module-kind
  [guidance module]
  (let [rules (:component-rules guidance)]
    (or (some (fn [rule]
                (when (matches-rule? module rule)
                  (:kind rule)))
              rules)
        :concrete)))

(defn classify-edges
  [guidance graph]
  (->> (:edges graph)
       (map (fn [{:keys [from to]}]
              {:from from
               :to to
               :type (if (= :abstract (module-kind guidance to))
                       :abstract
                       :direct)}))
       set))
