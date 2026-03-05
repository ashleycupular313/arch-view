(ns arch-view.model.components-spec
  (:require [arch-view.model.components :as sut]
            [speclj.core :refer :all]))

(describe "component assignment"
  (it "assigns modules to first matching component rule"
    (let [guidance {:component-rules [{:component :config :kind :concrete :match ["^my\\.app\\.config(\\..+)?$" "my.app.units"]}
                                      {:component :core :kind :abstract :match "^my\\.app\\.core(\\..+)?$"}
                                      {:component :outer :kind :concrete :match "^my\\.app(\\..+)?$"}]}
          modules #{"my.app.config.db" "my.app.core.api" "my.app.feature" "external.lib"}
          assignment (sut/assign-components guidance modules)]
      (should= :config (get assignment "my.app.config.db"))
      (should= :core (get assignment "my.app.core.api"))
      (should= :outer (get assignment "my.app.feature"))
      (should= nil (get assignment "external.lib")))))
