(ns arch-view.layout.layers-spec
  (:require [arch-view.layout.layers :as sut]
            [speclj.core :refer :all]))

(describe "layer assignment"
  (it "assigns modules to vertical levels from roots to leaves"
    (let [graph {:nodes #{"a" "b" "c" "d"}
                 :edges #{{:from "a" :to "b"}
                          {:from "c" :to "b"}
                          {:from "b" :to "d"}}}
          layout (sut/assign-layers graph)]
      (should= [["a" "c"] ["b"] ["d"]]
               (mapv :modules (:layers layout)))
      (should= 0 (get-in layout [:module->layer "a"]))
      (should= 0 (get-in layout [:module->layer "c"]))
      (should= 1 (get-in layout [:module->layer "b"]))
      (should= 2 (get-in layout [:module->layer "d"]))))

  (it "handles dependency cycles without recursion overflow"
    (let [graph {:nodes #{"a" "b" "c"}
                 :edges #{{:from "a" :to "b"}
                          {:from "b" :to "a"}
                          {:from "b" :to "c"}}}
          layout (sut/assign-layers graph)]
      (should= #{"a" "b" "c"}
               (set (keys (:module->layer layout))))
      (should= true (every? integer? (vals (:module->layer layout))))
      (should= true (boolean (seq (:feedback-edges layout))))
      (should= true (empty? (:feedback-edges (sut/assign-layers {:nodes #{"a" "b"}
                                                                 :edges #{{:from "a" :to "b"}}})))))

  (it "ignores edges that reference nodes outside the graph"
    (let [layout (sut/assign-layers {:nodes #{"a" "b"}
                                     :edges #{{:from "a" :to "b"}
                                              {:from "a" :to "missing"}
                                              {:from "missing" :to "b"}}})]
      (should= #{{:from "a" :to "b"}} (:acyclic-edges layout))
      (should= #{} (:feedback-edges layout))
      (should= 0 (get-in layout [:module->layer "a"]))
      (should= 1 (get-in layout [:module->layer "b"]))))

  (it "treats self-loops as cyclic and removes them as feedback edges"
    (let [layout (sut/assign-layers {:nodes #{"a"}
                                     :edges #{{:from "a" :to "a"}}})]
      (should= #{{:from "a" :to "a"}} (:feedback-edges layout))
      (should= #{} (:acyclic-edges layout))
      (should= 0 (get-in layout [:module->layer "a"]))))

  (it "handles larger strongly connected components via heuristic feedback"
    (let [nodes (set (map #(str "n" %) (range 9)))
          edges (set (map (fn [i]
                            {:from (str "n" i)
                             :to (str "n" (mod (inc i) 9))})
                          (range 9)))
          layout (sut/assign-layers {:nodes nodes :edges edges})]
      (should= true (seq (:feedback-edges layout)))
      (should= true (every? integer? (vals (:module->layer layout))))
      (should= 9 (count (:module->layer layout))))))
)
