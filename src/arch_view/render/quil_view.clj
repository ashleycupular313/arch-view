(ns arch-view.render.quil-view
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn- layer-y
  [index layer-height layer-gap]
  (* index (+ layer-height layer-gap)))

(defn- dominant-component
  [modules module->component]
  (->> modules
       (map module->component)
       (remove nil?)
       frequencies
       (sort-by (juxt (comp - val) (comp str key)))
       ffirst))

(defn- module-positions-for-layer
  [layer-index modules canvas-width layer-height layer-gap]
  (let [count-modules (count modules)
        spacing (/ canvas-width (inc count-modules))
        y (+ (layer-y layer-index layer-height layer-gap) (/ layer-height 2))]
    (map-indexed (fn [idx module]
                   {:module module
                    :layer layer-index
                    :x (* (inc idx) spacing)
                    :y y})
                 modules)))

(defn- arrowhead-for
  [edge-type]
  (if (= :abstract edge-type)
    :closed-triangle
    :standard))

(defn build-scene
  ([architecture]
   (build-scene architecture {}))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
   (let [layers (get-in architecture [:layout :layers])
         module->component (or (:module->component architecture) {})
         layer-rects (mapv (fn [{:keys [index]}]
                             (let [modules (get-in architecture [:layout :layers index :modules])
                                   component (dominant-component modules module->component)]
                               {:index index
                                :x 0
                                :y (layer-y index layer-height layer-gap)
                                :width canvas-width
                                :height layer-height
                                :label (if component
                                         (name component)
                                         (str "layer-" index))}))
                           layers)
         module-positions (->> layers
                               (mapcat (fn [{:keys [index modules]}]
                                         (module-positions-for-layer index modules canvas-width layer-height layer-gap)))
                               vec)
         edge-drawables (->> (:classified-edges architecture)
                             (map (fn [{:keys [from to type]}]
                                    {:from from
                                     :to to
                                     :arrowhead (arrowhead-for type)}))
                             vec)]
     {:layer-rects layer-rects
      :module-positions module-positions
      :edge-drawables edge-drawables})))

(defn- module-point-map
  [scene]
  (into {}
        (map (fn [{:keys [module x y]}]
               [module {:x x :y y}])
             (:module-positions scene))))

(defn- draw-arrowhead
  [x y arrowhead]
  (case arrowhead
    :closed-triangle (do
                       (q/fill 0)
                       (q/triangle x y (- x 12) (- y 6) (- x 12) (+ y 6)))
    (do
      (q/no-fill)
      (q/triangle x y (- x 10) (- y 5) (- x 10) (+ y 5)))))

(defn- draw-edge
  [points {:keys [from to arrowhead]}]
  (let [{x1 :x y1 :y} (get points from)
        {x2 :x y2 :y} (get points to)]
    (when (and x1 y1 x2 y2)
      (q/stroke 40 40 40)
      (q/line x1 y1 x2 y2)
      (draw-arrowhead x2 y2 arrowhead))))

(defn- draw-scene
  [scene]
  (q/background 250 250 250)
  (doseq [{:keys [x y width height label]} (:layer-rects scene)]
    (q/fill 225 233 242)
    (q/stroke 120 140 160)
    (q/rect x y width height)
    (q/fill 45 60 80)
    (q/text-align :left :top)
    (q/text label (+ x 8) (+ y 6)))
  (doseq [{:keys [x y module]} (:module-positions scene)]
    (q/fill 15 20 30)
    (q/no-stroke)
    (q/text-align :center :center)
    (q/text module x y))
  (let [points (module-point-map scene)]
    (doseq [edge (:edge-drawables scene)]
      (draw-edge points edge))))

(defn show!
  ([scene]
   (show! scene {}))
  ([scene {:keys [title]
           :or {title "architecture-viewer"}}]
   (let [height (if (seq (:layer-rects scene))
                  (->> (:layer-rects scene)
                       (map (fn [{:keys [y height]}] (+ y height)))
                       (apply max)
                       (+ 40))
                  400)
         width (if (seq (:layer-rects scene))
                 (:width (first (:layer-rects scene)))
                 1200)]
     (q/sketch
       :title title
       :size [width height]
       :setup (fn [] scene)
       :draw draw-scene
       :middleware [m/fun-mode]))))
