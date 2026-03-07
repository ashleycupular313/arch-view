(ns arch-view.render.ui.swing.source-window-spec
  (:require [arch-view.render.ui.swing.source-window :as sut]
            [speclj.core :refer :all]))

(describe "swing source window"
  (it "does nothing when source file is missing"
    (let [scheduled (atom false)]
      (with-redefs [sut/existing-source-file? (fn [_] false)
                    sut/schedule-ui! (fn [_] (reset! scheduled true))]
        (should= nil (sut/open-source-file-window! "/tmp/missing.clj")))
      (should= false @scheduled)))

  (it "loads content and schedules frame creation when source exists"
    (let [scheduled (atom false)
          built (atom nil)]
      (with-redefs [sut/existing-source-file? (fn [_] true)
                    clojure.core/slurp (fn [_] "(ns sample)")
                    sut/build-source-frame! (fn [path content] (reset! built [path content]))
                    sut/schedule-ui! (fn [f] (reset! scheduled true) (f))]
        (sut/open-source-file-window! "/tmp/sample.clj"))
      (should= true @scheduled)
      (should= ["/tmp/sample.clj" "(ns sample)"] @built))))
