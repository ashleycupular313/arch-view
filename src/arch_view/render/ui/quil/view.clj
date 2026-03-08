(ns arch-view.render.ui.quil.view
  (:require [clojure.string :as str]
            [arch-view.domain.architecture-projection :as projection]
            [arch-view.render.ui.quil.canvas :as canvas]
            [arch-view.render.ui.quil.events :as events]
            [arch-view.render.ui.util.functional :as functional]
            [arch-view.render.ui.swing.source-window :as source-window]
            [quil.core :as q]
            [quil.middleware :as m]))

(def ^:private scene-top-padding 42.0)

(defn- layer-y
  [index layer-height layer-gap]
  (functional/layer-y index layer-height layer-gap))

(defn- dominant-component
  [modules module->component]
  (functional/dominant-component modules module->component))

(defn- module-positions-for-layer
  [layer-index modules layer-rect module->kind module->full-name]
  (functional/module-positions-for-layer layer-index modules layer-rect module->kind module->full-name))

(defn abbreviate-module-name
  [module]
  (functional/abbreviate-module-name module))

(defn strip-top-namespace
  [module]
  (functional/strip-top-namespace module))

(defn- label-width
  [label]
  (functional/label-width label))

(defn- rendered-label
  [{:keys [display-label label]}]
  (functional/rendered-label {:display-label display-label :label label}))

(defn- rendered-label-lines
  [{:keys [display-label label max-label-chars]}]
  (functional/rendered-label-lines {:display-label display-label
                                    :label label
                                    :max-label-chars max-label-chars}))

(def ^:private toolbar-height 38.0)
(def ^:private back-button-width 360.0)
(def ^:private declutter-button-width 240.0)
(def ^:private button-height 26.0)

(def declutter-modes functional/declutter-modes)

(defn next-declutter-mode
  [mode]
  (functional/next-declutter-mode mode))

(defn declutter-label
  [mode]
  (functional/declutter-label mode))

(defn- back-button-label
  [{:keys [namespace-path nav-stack]}]
  (if-not (seq namespace-path)
    "Back"
    (str "Back: " (last (vec namespace-path)))))

(defn- back-button-rect
  []
  {:x 10.0 :y 6.0 :width back-button-width :height button-height})

(defn- declutter-button-rect
  []
  (let [{:keys [x width]} (back-button-rect)]
    {:x (+ x width 10.0) :y 6.0 :width declutter-button-width :height button-height}))

(defn- overlap?
  [a b]
  (< (Math/abs (double (- (:x a) (:x b))))
     (/ (+ (label-width (rendered-label a))
           (label-width (rendered-label b)))
        2.0)))

(defn- needs-stagger?
  [modules]
  (functional/needs-stagger? modules))

(defn- stagger-offset
  [idx]
  (functional/stagger-offset idx))

(defn- apply-layer-stagger
  [modules]
  (functional/apply-layer-stagger modules))

(defn- arrowhead-for
  [edge-type]
  (functional/arrowhead-for edge-type))

(def ^:private racetrack-count 5)
(def ^:private racetrack-margin 24.0)
(def ^:private racetrack-gap 24.0)

(defn- track-width-for
  [canvas-width]
  (functional/track-width-for canvas-width))

(defn- track-x-for
  [track canvas-width]
  (functional/track-x-for track canvas-width))

(defn- dependency-pairs-by-layer
  [classified-edges module->layer]
  (functional/dependency-pairs-by-layer classified-edges module->layer))

(defn- incoming-counts-by-layer
  [layer-indexes layer-pairs]
  (functional/incoming-counts-by-layer layer-indexes layer-pairs))

(defn- edge-point
  [placement node]
  (functional/edge-point placement node))

(defn- orientation
  [[ax ay] [bx by] [cx cy]]
  (functional/orientation [ax ay] [bx by] [cx cy]))

(defn- segment-crosses?
  [p1 p2 q1 q2]
  (functional/segment-crosses? p1 p2 q1 q2))

(defn- edge-cross?
  [placement [a b] [c d]]
  (functional/edge-cross? placement [a b] [c d]))

(defn- edge-crossing-count
  [placement edge-pairs]
  (let [pairs (vec edge-pairs)]
    (count
      (for [i (range (count pairs))
            j (range (inc i) (count pairs))
            :when (edge-cross? placement (nth pairs i) (nth pairs j))]
        true))))

(defn- assign-layer-slots
  [ordered-layer-indexes pairs incoming-counts]
  (functional/assign-layer-slots ordered-layer-indexes pairs incoming-counts))

(defn build-scene
  ([architecture]
   (functional/build-scene architecture))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
   (functional/build-scene architecture {:canvas-width canvas-width
                                         :layer-height layer-height
                                         :layer-gap layer-gap})))

(defn- module-point-map
  [scene]
  (into {}
        (map (fn [{:keys [module x y]}]
               [module {:x x :y y}])
             (:module-positions scene))))

(defn arrowhead-points
  [x1 y1 x2 y2 arrowhead]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len (max 0.001 (Math/sqrt (+ (* dx dx) (* dy dy))))
        ux (/ dx len)
        uy (/ dy len)
        back 10.0
        half 5.0
        bx (- x2 (* ux back))
        by (- y2 (* uy back))
        px (- uy)
        py ux]
    {:tip [x2 y2]
     :center [bx by]
     :left [(+ bx (* px half)) (+ by (* py half))]
     :right [(- bx (* px half)) (- by (* py half))]
     :closed? (= :closed-triangle arrowhead)}))

(defn- draw-arrowhead
  [x1 y1 x2 y2 arrowhead]
  (let [{:keys [tip left right closed?]} (arrowhead-points x1 y1 x2 y2 arrowhead)
        [tx ty] tip
        [lx ly] left
        [rx ry] right]
    (if closed?
      (do
        (q/no-fill)
        (q/triangle tx ty lx ly rx ry))
      (do
        (q/no-fill)
        (q/line tx ty lx ly)
        (q/line tx ty rx ry)))))

(defn edge-line-endpoint
  [x1 y1 x2 y2 arrowhead]
  (let [{:keys [tip center closed?]} (arrowhead-points x1 y1 x2 y2 arrowhead)]
    (if closed?
      center
      tip)))

(def ^:private label-clearance 12.0)

(defn dependency-tip-point
  [x1 y1 x2 y2]
  (let [dy (- y2 y1)]
    (cond
      (> dy 0.1) [(double x2) (- (double y2) label-clearance)]
      (< dy -0.1) [(double x2) (+ (double y2) label-clearance)]
      :else [(double x2) (double y2)])))

(defn dependency-start-point
  [x1 y1 x2 y2]
  (let [dy (- y2 y1)]
    (cond
      (> dy 0.1) [(double x1) (+ (double y1) label-clearance)]
      (< dy -0.1) [(double x1) (- (double y1) label-clearance)]
      :else [(double x1) (double y1)])))

(defn- label-hitbox
  [{:keys [x y] :as module-position}]
  (let [lines (rendered-label-lines module-position)
        width (apply max 0.0 (map label-width lines))
        line-count (max 1 (count lines))
        pad-x 8.0
        pad-y 6.0
        half-w (+ (/ width 2.0) pad-x)
        half-h (+ (/ (* 14.0 line-count) 2.0) pad-y)]
    {:left (- x half-w)
     :right (+ x half-w)
     :top (- y half-h)
     :bottom (+ y half-h)}))

(defn hovered-module
  [module-positions mx my]
  (some (fn [{:keys [module] :as m}]
          (let [{:keys [left right top bottom]} (label-hitbox m)]
            (when (and (<= left mx right)
                       (<= top my bottom))
              module)))
        module-positions))

(defn- layer-label-hitbox
  [{:keys [x y label]}]
  (let [width (label-width label)]
    {:left (+ x 6.0)
     :right (+ x 10.0 width)
     :top (+ y 4.0)
     :bottom (+ y 22.0)}))

(defn hovered-layer-label
  [layer-rects mx my]
  (some (fn [{:keys [label] :as layer-rect}]
          (let [{:keys [left right top bottom]} (layer-label-hitbox layer-rect)]
            (when (and (<= left mx right)
                       (<= top my bottom))
              layer-rect)))
        layer-rects))

(defn- hovered-module-position
  [module-positions mx my]
  (some (fn [m]
          (let [{:keys [left right top bottom]} (label-hitbox m)]
            (when (and (<= left mx right)
                       (<= top my bottom))
              m)))
        module-positions))

(declare view-architecture
         drilldown-scene
         push-nav-state
         apply-zoom-click
         world-x-at-screen
         world-y-at-screen
         scroll-for-world-x
         scroll-for-world-y
         point-in-toolbar?)

(defn- drillable?
  [state hovered]
  (let [candidate (conj (or (:namespace-path state) []) (:module hovered))
        child-view (view-architecture (:architecture state) candidate)]
    (seq (get-in child-view [:graph :nodes]))))

(defn attach-drillable-markers
  [scene architecture namespace-path]
  (if-not architecture
    scene
    (update scene :module-positions
            (fn [positions]
              (mapv (fn [position]
                      (let [drillable (boolean (drillable? {:architecture architecture
                                                            :namespace-path namespace-path}
                                                           position))
                            label (:label position)]
                        (assoc position
                               :drillable? drillable
                               :display-label label)))
                    positions)))))

(defn scroll-range
  [content-height viewport-height]
  (max 0.0 (- (double content-height) (double viewport-height))))

(defn clamp-scroll
  [scroll-y content-height viewport-height]
  (let [max-scroll (scroll-range content-height viewport-height)]
    (-> scroll-y double (max 0.0) (min max-scroll))))

(defn clamp-scroll-x
  [scroll-x content-width viewport-width]
  (let [max-scroll (scroll-range content-width viewport-width)]
    (-> scroll-x double (max 0.0) (min max-scroll))))

(defn scrollbar-rect
  [content-height viewport-height scroll-y viewport-width]
  (when (> content-height viewport-height)
    (let [track-height (- (double viewport-height) 24.0)
          ratio (/ (double viewport-height) (double content-height))
          thumb-height (max 30.0 (* track-height ratio))
          max-scroll (scroll-range content-height viewport-height)
          thumb-y (if (zero? max-scroll)
                    12.0
                    (+ 12.0 (* (- track-height thumb-height) (/ scroll-y max-scroll))))]
      {:x (- (double viewport-width) 12.0)
       :y thumb-y
       :width 8.0
       :height thumb-height})))

(defn- content-height-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [y height]}] (+ y height)))
       (apply max 0)
       (+ 40)))

(defn- scaled-content-height
  [scene zoom]
  (* (double (or zoom 1.0))
     (content-height-for-scene scene)))

(defn- content-width-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [x width]}] (+ x width)))
       (apply max 0)
       (+ racetrack-margin 20.0)))

(defn- scaled-content-width
  [scene zoom]
  (* (double (or zoom 1.0))
     (content-width-for-scene scene)))

(defn thumb-y->scroll
  [thumb-y content-height viewport-height]
  (if (<= content-height viewport-height)
    0.0
    (let [track-height (- (double viewport-height) 24.0)
          ratio (/ (double viewport-height) (double content-height))
          thumb-height (max 30.0 (* track-height ratio))
          max-scroll (scroll-range content-height viewport-height)
          max-thumb-travel (max 1.0 (- track-height thumb-height))
          normalized (/ (- thumb-y 12.0) max-thumb-travel)]
      (clamp-scroll (* normalized max-scroll) content-height viewport-height))))

(defn- point-in-rect?
  [{:keys [x y width height]} px py]
  (and (<= x px (+ x width))
       (<= y py (+ y height))))

(declare rect-center-x
         rect-bottom
         draw-dependency-indicators)

(defn- draw-scene-content
  [scene viewport-width dependency-indicators]
  (canvas/draw-scene-content scene viewport-width dependency-indicators
                             {:rendered-label rendered-label
                              :rendered-label-lines rendered-label-lines
                              :draw-dependency-indicators draw-dependency-indicators}))

(def ^:private dependency-triangle-side 12.0)
(def ^:private dependency-triangle-height
  (* dependency-triangle-side 0.8660254037844386))
(def ^:private dependency-triangle-hover-tolerance 5.0)

(defn- triangle-points
  [rect direction]
  (let [cx (rect-center-x rect)
        half-side (/ dependency-triangle-side 2.0)
        edge-y (if (= direction :incoming)
                 (:y rect)
                 (rect-bottom rect))]
    [[(- cx half-side) edge-y]
     [(+ cx half-side) edge-y]
     [cx (+ edge-y dependency-triangle-height)]]))

(defn- abbreviated-deps
  [dependency-entries full-name-by-module]
  (->> dependency-entries
       (reduce (fn [acc {:keys [module cycle-break?]}]
                 (update acc module (fn [existing]
                                      {:module module
                                       :cycle? (or cycle-break? (get existing :cycle? false))})))
               {})
       vals
       (sort-by :module)
       (mapv (fn [{:keys [module cycle?]}]
               {:text (or (get full-name-by-module module)
                          (strip-top-namespace (str module)))
                :cycle? (boolean cycle?)}))))

(defn dependency-indicators
  [scene declutter-mode]
  (let [edge-drawables (functional/declutter-edge-drawables scene declutter-mode)
        full-name-by-module (into {}
                                  (keep (fn [{:keys [module full-name]}]
                                          (when module [module full-name])))
                                  (:module-positions scene))
        incoming-by-module (reduce (fn [acc {:keys [from to cycle-break?]}]
                                     (update acc to (fnil conj [])
                                             {:module from :cycle-break? cycle-break?}))
                                   {}
                                   edge-drawables)
        outgoing-by-module (reduce (fn [acc {:keys [from to cycle-break?]}]
                                     (update acc from (fnil conj [])
                                             {:module to :cycle-break? cycle-break?}))
                                   {}
                                   edge-drawables)]
    (->> (:layer-rects scene)
         (mapcat (fn [{:keys [module] :as rect}]
                   (let [incoming (abbreviated-deps (get incoming-by-module module)
                                                    full-name-by-module)
                         outgoing (abbreviated-deps (get outgoing-by-module module)
                                                    full-name-by-module)]
                     (cond-> []
                       (seq incoming)
                       (conj {:module module
                              :direction :incoming
                              :triangle (triangle-points rect :incoming)
                              :cycle? (boolean (some :cycle? incoming))
                              :tooltip-lines incoming})

                       (seq outgoing)
                       (conj {:module module
                              :direction :outgoing
                              :triangle (triangle-points rect :outgoing)
                              :cycle? (boolean (some :cycle? outgoing))
                              :tooltip-lines outgoing})))))
         (sort-by (juxt :module :direction))
         vec)))

(defn- rect-bottom
  [{:keys [y height]}]
  (+ y height))

(defn- rect-center-x
  [rect]
  (+ (:x rect) (/ (:width rect) 2.0)))

(defn- draw-dependency-indicators
  [indicators]
  (q/no-stroke)
  (doseq [{:keys [triangle cycle?]} indicators
          :let [[[x1 y1] [x2 y2] [x3 y3]] triangle]]
    (if cycle?
      (q/fill 180 0 0)
      (q/fill 0 0 0))
    (q/triangle x1 y1 x2 y2 x3 y3)))

(defn- draw-toolbar
  [{:keys [namespace-path nav-stack]}]
  (canvas/draw-toolbar {:namespace-path namespace-path
                        :nav-stack nav-stack}
                       {:back-button-rect back-button-rect
                        :back-button-label back-button-label
                        :toolbar-height toolbar-height}))

(defn- draw-tooltip
  [full-name mx my]
  (canvas/draw-tooltip full-name mx my))

(defn- draw-tooltip-lines
  [lines mx my]
  (canvas/draw-tooltip-lines lines mx my))

(defn- point->segment-distance
  [px py x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len2 (+ (* dx dx) (* dy dy))]
    (if (< len2 0.001)
      (Math/sqrt (+ (* (- px x1) (- px x1))
                    (* (- py y1) (- py y1))))
      (let [t (-> (/ (+ (* (- px x1) dx)
                        (* (- py y1) dy))
                     len2)
                  double
                  (max 0.0)
                  (min 1.0))
            proj-x (+ x1 (* t dx))
            proj-y (+ y1 (* t dy))]
        (Math/sqrt (+ (* (- px proj-x) (- px proj-x))
                      (* (- py proj-y) (- py proj-y))))))))

(defn- triangle-sign
  [[px py] [ax ay] [bx by]]
  (- (* (- px bx) (- ay by))
     (* (- ax bx) (- py by))))

(defn- point-in-triangle?
  [mx my [[ax ay :as a] [bx by :as b] [cx cy :as c]]]
  (let [p [mx my]
        d1 (triangle-sign p a b)
        d2 (triangle-sign p b c)
        d3 (triangle-sign p c a)
        has-neg? (or (neg? d1) (neg? d2) (neg? d3))
        has-pos? (or (pos? d1) (pos? d2) (pos? d3))]
    (not (and has-neg? has-pos?))))

(defn hovered-dependency
  [indicators mx my]
  (let [mx (double mx)
        my (double my)]
    (->> indicators
         (map (fn [{:keys [triangle] :as indicator}]
                (let [inside? (point-in-triangle? mx my triangle)
                      edges (map vector triangle (concat (rest triangle) [(first triangle)]))
                      nearest-edge (reduce min Double/MAX_VALUE
                                           (map (fn [[[x1 y1] [x2 y2]]]
                                                  (point->segment-distance mx my x1 y1 x2 y2))
                                                edges))]
                  (when (or inside? (<= nearest-edge dependency-triangle-hover-tolerance))
                    (assoc indicator :hover-distance nearest-edge)))))
         (remove nil?)
         (sort-by :hover-distance)
         first)))

(defn- html-escape
  [s]
  (source-window/html-escape s))

(defn- colorize-clojure-html
  [source]
  (source-window/colorize-clojure-html source))

(defn- expand-tabs
  [line]
  (source-window/expand-tabs line))

(defn- source-lines->html
  [source]
  (source-window/source-lines->html source))

(defn- source->html
  [title source]
  (source-window/source->html title source))

(defn open-source-file-window!
  [source-file]
  (source-window/open-source-file-window! source-file))

(defn- draw-scrollbar
  [content-height viewport-height scroll-y viewport-width]
  (canvas/draw-scrollbar content-height viewport-height scroll-y viewport-width
                         {:scrollbar-rect scrollbar-rect}))

(defn- draw-scene
  [{:keys [scene declutter-mode scroll-x scroll-y viewport-height viewport-width zoom] :as state}]
  (canvas/draw-scene state
                     {:scaled-content-height scaled-content-height
                      :point-in-toolbar? point-in-toolbar?
                      :dependency-indicators dependency-indicators
                      :hovered-dependency hovered-dependency
                      :hovered-module-position hovered-module-position
                      :hovered-layer-label hovered-layer-label
                      :draw-scene-content draw-scene-content
                      :draw-toolbar draw-toolbar
                      :draw-tooltip draw-tooltip
                      :draw-tooltip-lines draw-tooltip-lines
                      :draw-scrollbar draw-scrollbar}))

(defn- plus-key?
  [k]
  (events/plus-key? k))

(defn- minus-key?
  [k]
  (events/minus-key? k))

(defn- zoom-in-at-screen-pos
  [{:keys [zoom zoom-stack scene viewport-height viewport-width] :as state} screen-x screen-y]
  (events/zoom-in-at-screen-pos state screen-x screen-y
                                {:scaled-content-width scaled-content-width
                                 :scaled-content-height scaled-content-height
                                 :clamp-scroll clamp-scroll
                                 :clamp-scroll-x clamp-scroll-x}))

(defn- zoom-out-at-screen-pos
  [{:keys [zoom zoom-stack scene viewport-height viewport-width] :as state} screen-x screen-y]
  (events/zoom-out-at-screen-pos state screen-x screen-y
                                 {:scaled-content-width scaled-content-width
                                  :scaled-content-height scaled-content-height
                                  :clamp-scroll clamp-scroll
                                  :clamp-scroll-x clamp-scroll-x}))

(defn handle-key-pressed
  [state event]
  (events/handle-key-pressed state event
                             {:scaled-content-width scaled-content-width
                              :scaled-content-height scaled-content-height
                              :clamp-scroll clamp-scroll
                              :clamp-scroll-x clamp-scroll-x}))

(defn handle-mouse-wheel
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event]
  (events/handle-mouse-wheel state event
                             {:scaled-content-height scaled-content-height
                              :clamp-scroll clamp-scroll}))

(defn- mouse-pos
  [event]
  (events/mouse-pos event))

(defn handle-mouse-pressed
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event]
  (events/handle-mouse-pressed state event
                               {:scaled-content-height scaled-content-height
                                :scrollbar-rect scrollbar-rect
                                :point-in-rect? point-in-rect?}))

(defn handle-mouse-dragged
  [{:keys [scene viewport-height dragging-scrollbar? drag-offset zoom] :as state} event]
  (events/handle-mouse-dragged state event
                               {:scaled-content-height scaled-content-height
                                :thumb-y->scroll thumb-y->scroll}))

(defn- apply-drilldown-click
  [{:keys [scene scroll-x scroll-y namespace-path zoom] :as state} event]
  (events/apply-drilldown-click state event
                                {:hovered-module-position hovered-module-position
                                 :view-architecture view-architecture
                                 :push-nav-state push-nav-state
                                 :drilldown-scene drilldown-scene
                                 :open-source-file-window! open-source-file-window!}))

(defn- navigate-up
  [{:keys [nav-stack] :as state}]
  (events/navigate-up state {:drilldown-scene drilldown-scene}))

(defn- toolbar-click-target
  [state mx my]
  (events/toolbar-click-target state mx my {:point-in-rect? point-in-rect?
                                            :back-button-rect back-button-rect
                                            :declutter-button-rect declutter-button-rect
                                            :toolbar-height toolbar-height}))

(defn- point-in-toolbar?
  [mx my]
  (events/point-in-toolbar? mx my toolbar-height))

(defn- apply-toolbar-click
  [state event]
  (events/apply-toolbar-click state event
                              {:point-in-rect? point-in-rect?
                               :back-button-rect back-button-rect
                               :declutter-button-rect declutter-button-rect
                               :next-declutter-mode next-declutter-mode
                               :drilldown-scene drilldown-scene
                               :toolbar-height toolbar-height}))

(defn- control-down?
  [event]
  (events/control-down? event))

(defn- button-kind
  [event]
  (events/button-kind event))

(defn- scaled-scroll-for-zoom
  [scroll-y old-zoom new-zoom]
  (if (<= old-zoom 0.0)
    scroll-y
    (* (double (or scroll-y 0.0))
       (/ (double new-zoom) (double old-zoom)))))

(defn- world-y-at-screen
  [screen-y scroll-y zoom]
  (events/world-y-at-screen screen-y scroll-y zoom))

(defn- world-x-at-screen
  [screen-x scroll-x zoom]
  (events/world-x-at-screen screen-x scroll-x zoom))

(defn- scroll-for-world-y
  [world-y screen-y zoom]
  (events/scroll-for-world-y world-y screen-y zoom))

(defn- scroll-for-world-x
  [world-x screen-x zoom]
  (events/scroll-for-world-x world-x screen-x zoom))

(defn- apply-zoom-click
  [{:keys [zoom zoom-stack scene viewport-height] :as state} event]
  (events/apply-zoom-click state event
                           {:scaled-content-height scaled-content-height
                            :clamp-scroll clamp-scroll}))

(defn handle-mouse-released
  [{:keys [dragging-scrollbar?] :as state} event]
  (events/handle-mouse-released state event
                                {:point-in-rect? point-in-rect?
                                 :back-button-rect back-button-rect
                                 :drilldown-scene drilldown-scene
                                 :toolbar-height toolbar-height
                                 :hovered-module-position hovered-module-position
                                 :view-architecture view-architecture
                                 :push-nav-state push-nav-state
                                 :open-source-file-window! open-source-file-window!}))

(defn handle-mouse-clicked
  [state event]
  (events/handle-mouse-clicked state event
                               {:point-in-rect? point-in-rect?
                                :back-button-rect back-button-rect
                                :drilldown-scene drilldown-scene
                                :toolbar-height toolbar-height
                                :hovered-module-position hovered-module-position
                                :view-architecture view-architecture
                                :push-nav-state push-nav-state
                                :open-source-file-window! open-source-file-window!}))

(defn view-architecture
  [architecture namespace-path]
  (projection/view-architecture architecture namespace-path))

(defn- drilldown-scene
  [state path scroll-x scroll-y]
  (let [view (view-architecture (:architecture state) path)
        scene (-> (build-scene view)
                  (attach-drillable-markers (:architecture state) path))]
    (assoc state
           :namespace-path path
           :scroll-x (double (or scroll-x 0.0))
           :scroll-y (double (or scroll-y 0.0))
           :scene scene
           :routed-edges nil)))

(defn initial-scene-for-show
  [scene architecture]
  (if architecture
    (let [initial-view (view-architecture architecture [])]
      (-> (build-scene initial-view)
          (attach-drillable-markers architecture [])))
    scene))

(defn- push-nav-state
  [{:keys [namespace-path scroll-x scroll-y nav-stack] :as state}]
  (assoc state :nav-stack (conj (vec (or nav-stack []))
                                {:path (vec (or namespace-path []))
                                 :scroll-x (double (or scroll-x 0.0))
                                 :scroll-y (double (or scroll-y 0.0))})))

(defn show!
  ([scene]
   (show! scene {}))
  ([scene {:keys [title architecture]
           :or {title "architecture-viewer"}}]
   (let [effective-architecture (or architecture {:scene scene})
         initial-scene (initial-scene-for-show scene architecture)
         content-height (if (seq (:layer-rects initial-scene))
                          (->> (:layer-rects initial-scene)
                               (map (fn [{:keys [y height]}] (+ y height)))
                               (apply max)
                               (+ 40))
                          400)
         viewport-height (int (min 900 (max 400 content-height)))
         width (int (max 1200 (content-width-for-scene initial-scene)))]
     (q/sketch
       :title title
       :size [width viewport-height]
      :setup (fn []
                {:scene initial-scene
                 :architecture effective-architecture
                 :namespace-path (when architecture [])
                 :nav-stack []
                 :declutter-mode :all
                 :zoom 1.0
                 :zoom-stack []
                 :suppress-next-click? false
                 :scroll-x 0.0
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :routed-edges nil
                 :viewport-height viewport-height
                 :viewport-width width})
       :draw draw-scene
       :key-pressed handle-key-pressed
       :mouse-wheel handle-mouse-wheel
       :mouse-pressed handle-mouse-pressed
       :mouse-dragged handle-mouse-dragged
        :mouse-released handle-mouse-released
        :middleware [m/fun-mode]))))

(defn- safe-looping?
  [sketch]
  (try
    (.isLooping sketch)
    (catch Throwable _
      false)))

(defn- safe-displayable?
  [sketch fallback]
  (try
    (let [native (some-> sketch .getSurface .getNative)]
      (if (instance? java.awt.Component native)
        (.isDisplayable ^java.awt.Component native)
        fallback))
    (catch Throwable _
      fallback)))

(defn wait-until-closed!
  [sketch]
  (when sketch
    (loop []
      (let [looping? (safe-looping? sketch)
            displayable? (safe-displayable? sketch looping?)]
        (when (and looping? displayable?)
        (Thread/sleep 100)
        (recur))))))
