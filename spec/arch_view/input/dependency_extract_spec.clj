(ns arch-view.input.dependency-extract-spec
  (:require [arch-view.input.dependency-extract :as sut]
            [speclj.core :refer :all]))

(describe "dependency extraction"
  (it "builds a module graph from ns requires"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-src" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          a-file (java.io.File. src-dir "my/app/a.clj")
          b-file (java.io.File. src-dir "my/app/b.clj")]
      (.mkdirs (.getParentFile a-file))
      (spit a-file "(ns my.app.a (:require [my.app.b :as b] [clojure.string :as str]))")
      (spit b-file "(ns my.app.b)")
      (let [graph (sut/build-module-graph (.getAbsolutePath root) ["src"])]
        (should= #{"my.app.a" "my.app.b"}
                 (:nodes graph))
        (should= #{{:from "my.app.a" :to "my.app.b"}}
                 (:edges graph))))))
