(ns arch-view.render.ui.quil.view-wait-spec
  (:require [arch-view.render.ui.quil.view :as sut]
            [arch-view.render.ui.util.quil-lifecycle :as lifecycle]
            [speclj.core :refer :all]))

(describe "quil view helper coverage"
  (it "delegates wait-until-closed to lifecycle utility"
    (let [called? (atom nil)]
      (with-redefs [lifecycle/wait-until-closed! (fn [sketch]
                                                   (reset! called? sketch))]
        (sut/wait-until-closed! :s))
      (should= :s @called?)))

  (it "builds sketch options in show"
    (let [captured (atom nil)
          scene {:layer-rects [{:x 0.0 :y 0.0 :width 300.0 :height 120.0}]
                 :module-positions []
                 :edge-drawables []}]
      (with-redefs [quil.core/sketch (fn [& args] (reset! captured (apply hash-map args)) :ok)]
        (should= :ok (sut/show! scene {:title "T"})))
      (should= "T" (:title @captured))
      (should= true (contains? @captured :mouse-released)))))
