(ns arch-view.core
  (:require [clojure.java.io :as io]
            [arch-view.input.dependency-checker :as checker]
            [arch-view.input.dependency-extract :as extract]
            [arch-view.layout.layers :as layers]
            [arch-view.model.classify :as classify]
            [arch-view.render.quil-view :as render]))

(defn load-architecture
  [project-path]
  (let [guidance-path (.getAbsolutePath (io/file project-path "dependency-checker.edn"))
        guidance (checker/read-guidance guidance-path)
        source-paths (or (:source-paths guidance) ["src"])
        graph (extract/build-module-graph project-path source-paths)
        layout (layers/assign-layers graph)
        classified-edges (classify/classify-edges guidance graph)
        scene (render/build-scene {:layout layout
                                   :classified-edges classified-edges})]
    {:guidance guidance
     :graph graph
     :layout layout
     :classified-edges classified-edges
     :scene scene}))

(defn -main [& [project-path]]
  (let [project-path (or project-path ".")
        {:keys [graph]} (load-architecture project-path)]
    (println "Architecture loaded")
    (println "Nodes:" (count (:nodes graph)))
    (println "Edges:" (count (:edges graph)))))
