(ns arch-view.render.ui.quil.view-wait-spec
  (:require [arch-view.render.ui.quil.view :as sut]
            [speclj.core :refer :all]))

(describe "quil view helper coverage"
  (it "checks safe displayability and stop condition"
    (should= true (#'sut/safe-displayable? nil true))
    (should= false (#'sut/safe-displayable? (Object.) false))
    (let [looping-calls (atom 0)]
      (with-redefs [sut/safe-looping? (fn [_] (swap! looping-calls inc) true)
                    sut/safe-displayable? (fn [_ _] false)]
        (sut/wait-until-closed! :sketch))
      (should= 1 @looping-calls)
      (sut/wait-until-closed! nil)))

  (it "builds sketch options in show"
    (let [captured (atom nil)
          scene {:layer-rects [{:x 0.0 :y 0.0 :width 300.0 :height 120.0}]
                 :module-positions []
                 :edge-drawables []}]
      (with-redefs [quil.core/sketch (fn [& args] (reset! captured (apply hash-map args)) :ok)]
        (should= :ok (sut/show! scene {:title "T"})))
      (should= "T" (:title @captured))
      (should= true (contains? @captured :mouse-released))))

  (it "covers route-helper branches"
    (let [bounds {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}]
      (should= 200.0 (#'sut/choose-route-column 100.0 100.0 300.0 300.0 {:all-rects [] :from-rect nil :to-rect nil} bounds))
      (should= [0.0 0.0] (first (#'sut/nudge-path [[0.0 0.0] [10.0 0.0] [10.0 10.0]] 0.0 10.0)))
      (should= nil (#'sut/place-non-overlapping-path [[0.0 0.0] [50.0 0.0] [100.0 0.0]]
                                                     {:all-rects []}
                                                     [[[0.0 0.0] [100.0 0.0]]])))))
