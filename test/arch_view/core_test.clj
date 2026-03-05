(ns arch-view.core-test
  (:require [clojure.test :refer [deftest is]]
            [arch-view.core :as sut]))

(deftest load-architecture-builds-guidance-and-graph
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-project" (make-array java.nio.file.attribute.FileAttribute 0)))
        src-dir (doto (java.io.File. root "src") .mkdirs)
        dep-file (java.io.File. root "dependency-checker.edn")
        a-file (java.io.File. src-dir "my/app/a.clj")
        b-file (java.io.File. src-dir "my/app/b.clj")]
    (.mkdirs (.getParentFile a-file))
    (spit dep-file "{:source-paths [\"src\"] :component-rules [{:component :all :kind :concrete :match \"my.app.*\"}]}")
    (spit a-file "(ns my.app.a (:require [my.app.b :as b]))")
    (spit b-file "(ns my.app.b)")
    (let [architecture (sut/load-architecture (.getAbsolutePath root))]
      (is (= ["src"] (get-in architecture [:guidance :source-paths])))
      (is (= #{"my.app.a" "my.app.b"}
             (get-in architecture [:graph :nodes])))
      (is (= #{{:from "my.app.a" :to "my.app.b"}}
             (get-in architecture [:graph :edges]))))))
