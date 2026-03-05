(ns arch-view.core-spec
  (:require [arch-view.core :as sut]
            [arch-view.render.quil-view :as render]
            [speclj.core :refer :all]))

(describe "core architecture loader"
  (it "loads guidance and derived graph"
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
        (should= ["src"] (get-in architecture [:guidance :source-paths]))
        (should= #{"my.app.a" "my.app.b"}
                 (get-in architecture [:graph :nodes]))
        (should= #{{:from "my.app.a" :to "my.app.b"}}
                 (get-in architecture [:graph :edges]))
        (should= [{:index 0 :modules ["my.app.b"]}
                  {:index 1 :modules ["my.app.a"]}]
                 (get-in architecture [:layout :layers]))
        (should= {"my.app.a" 1 "my.app.b" 0}
                 (get-in architecture [:layout :module->layer]))
        (should= #{{:from "my.app.a" :to "my.app.b" :type :direct}}
                 (:classified-edges architecture))
        (should= {"my.app.a" :all "my.app.b" :all}
                 (:module->component architecture))
        (should= 2 (count (get-in architecture [:scene :layer-rects])))
        (should= 2 (count (get-in architecture [:scene :module-positions])))
        (should= 1 (count (get-in architecture [:scene :edge-drawables]))))))

  (it "parses cli options with no-gui flag"
    (should= {:project-path "/tmp/demo" :no-gui true :out nil}
             (sut/parse-args ["--project-path" "/tmp/demo" "--no-gui"])))

  (it "parses output file option"
    (should= {:project-path "/tmp/demo" :no-gui true :out "/tmp/scene.edn"}
             (sut/parse-args ["--project-path" "/tmp/demo" "--no-gui" "--out" "/tmp/scene.edn"])))

  (it "does not invoke quil rendering when --no-gui is present"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-cli" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          dep-file (java.io.File. root "dependency-checker.edn")
          a-file (java.io.File. src-dir "my/app/a.clj")
          b-file (java.io.File. src-dir "my/app/b.clj")
          called? (atom false)]
      (.mkdirs (.getParentFile a-file))
      (spit dep-file "{:source-paths [\"src\"] :component-rules [{:component :all :kind :concrete :match \"my.app.*\"}]}")
      (spit a-file "(ns my.app.a (:require [my.app.b :as b]))")
      (spit b-file "(ns my.app.b)")
      (with-redefs [render/show! (fn [& _] (reset! called? true))]
        (sut/-main "--project-path" (.getAbsolutePath root) "--no-gui"))
      (should= false @called?)))

  (it "invokes viewer and wait loop when gui mode is enabled"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-gui" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          dep-file (java.io.File. root "dependency-checker.edn")
          a-file (java.io.File. src-dir "my/app/a.clj")
          b-file (java.io.File. src-dir "my/app/b.clj")
          showed? (atom false)
          waited? (atom false)]
      (.mkdirs (.getParentFile a-file))
      (spit dep-file "{:source-paths [\"src\"] :component-rules [{:component :all :kind :concrete :match \"my.app.*\"}]}")
      (spit a-file "(ns my.app.a (:require [my.app.b :as b]))")
      (spit b-file "(ns my.app.b)")
      (with-redefs [render/show! (fn [& _] (reset! showed? true) :fake-sketch)
                    render/wait-until-closed! (fn [sketch]
                                                (when (= :fake-sketch sketch)
                                                  (reset! waited? true)))]
        (sut/-main "--project-path" (.getAbsolutePath root)))
      (should= true @showed?)
      (should= true @waited?)))

  (it "falls back to default guidance when dependency-checker.edn is missing"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-no-guide" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          a-file (java.io.File. src-dir "my/app/a.clj")
          b-file (java.io.File. src-dir "my/app/b.clj")]
      (.mkdirs (.getParentFile a-file))
      (spit a-file "(ns my.app.a (:require [my.app.b :as b]))")
      (spit b-file "(ns my.app.b)")
      (let [architecture (sut/load-architecture (.getAbsolutePath root))]
        (should= ["src"] (get-in architecture [:guidance :source-paths]))
        (should= #{"my.app.a" "my.app.b"} (get-in architecture [:graph :nodes])))))

  (it "writes architecture data to out file when --out is provided"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-out" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          dep-file (java.io.File. root "dependency-checker.edn")
          out-file (java.io.File. root "scene.edn")
          a-file (java.io.File. src-dir "my/app/a.clj")
          b-file (java.io.File. src-dir "my/app/b.clj")]
      (.mkdirs (.getParentFile a-file))
      (spit dep-file "{:source-paths [\"src\"] :component-rules [{:component :all :kind :concrete :match \"my.app.*\"}]}")
      (spit a-file "(ns my.app.a (:require [my.app.b :as b]))")
      (spit b-file "(ns my.app.b)")
      (with-redefs [render/show! (fn [& _] nil)]
        (sut/-main "--project-path" (.getAbsolutePath root) "--no-gui" "--out" (.getAbsolutePath out-file)))
      (should= true (.exists out-file))
      (should-not= nil (re-find #"classified-edges" (slurp out-file))))))
