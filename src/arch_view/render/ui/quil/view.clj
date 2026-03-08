(ns arch-view.render.ui.quil.view
  (:require [arch-view.domain.architecture-projection :as projection]
            [arch-view.render.ui.quil.canvas :as canvas]
            [arch-view.render.ui.quil.events :as events]
            [arch-view.render.ui.util.dependency-indicators :as dependency-indicators]
            [arch-view.render.ui.util.functional :as functional]
            [arch-view.render.ui.util.module-hover :as module-hover]
            [arch-view.render.ui.util.quil-lifecycle :as quil-lifecycle]
            [arch-view.render.ui.util.scene-state :as scene-state]
            [arch-view.render.ui.util.view-bootstrap :as view-bootstrap]
            [arch-view.render.ui.util.viewport :as viewport]
            [arch-view.render.ui.swing.source-window :as source-window]
            [quil.core :as q]
            [quil.middleware :as m]))

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

(def ^:private racetrack-margin 24.0)

(defn build-scene
  ([architecture]
   (functional/build-scene architecture))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
   (functional/build-scene architecture {:canvas-width canvas-width
                                         :layer-height layer-height
                                         :layer-gap layer-gap})))

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

(defn hovered-module
  [module-positions mx my]
  (module-hover/hovered-module module-positions mx my rendered-label-lines label-width))

(defn hovered-layer-label
  [layer-rects mx my]
  (module-hover/hovered-layer-label layer-rects mx my label-width))

(defn- hovered-module-position
  [module-positions mx my]
  (module-hover/hovered-module-position module-positions mx my rendered-label-lines label-width))

(declare view-architecture
         drilldown-scene
         push-nav-state
         point-in-toolbar?)

(defn attach-drillable-markers
  [scene architecture namespace-path]
  (scene-state/attach-drillable-markers scene architecture namespace-path view-architecture))

(defn scroll-range
  [content-height viewport-height]
  (viewport/scroll-range content-height viewport-height))

(defn clamp-scroll
  [scroll-y content-height viewport-height]
  (viewport/clamp-scroll scroll-y content-height viewport-height))

(defn clamp-scroll-x
  [scroll-x content-width viewport-width]
  (viewport/clamp-scroll-x scroll-x content-width viewport-width))

(defn scrollbar-rect
  [content-height viewport-height scroll-y viewport-width]
  (viewport/scrollbar-rect content-height viewport-height scroll-y viewport-width))

(defn- content-height-for-scene
  [scene]
  (viewport/content-height-for-scene scene))

(defn- scaled-content-height
  [scene zoom]
  (viewport/scaled-content-height scene zoom))

(defn- content-width-for-scene
  [scene]
  (viewport/content-width-for-scene scene racetrack-margin))

(defn- scaled-content-width
  [scene zoom]
  (viewport/scaled-content-width scene zoom racetrack-margin))

(defn thumb-y->scroll
  [thumb-y content-height viewport-height]
  (viewport/thumb-y->scroll thumb-y content-height viewport-height))

(defn- point-in-rect?
  [{:keys [x y width height]} px py]
  (viewport/point-in-rect? {:x x :y y :width width :height height} px py))

(defn- draw-scene-content
  [scene viewport-width dependency-indicators]
  (canvas/draw-scene-content scene viewport-width dependency-indicators
                             {:rendered-label rendered-label
                              :rendered-label-lines rendered-label-lines
                              :draw-dependency-indicators dependency-indicators/draw-dependency-indicators}))

(defn dependency-indicators
  [scene declutter-mode]
  (dependency-indicators/dependency-indicators scene declutter-mode strip-top-namespace))

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

(defn hovered-dependency
  [indicators mx my]
  (dependency-indicators/hovered-dependency indicators mx my))

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
                                 :open-source-file-window! source-window/open-source-file-window!}))

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
                                :open-source-file-window! source-window/open-source-file-window!}))

(defn view-architecture
  [architecture namespace-path]
  (projection/view-architecture architecture namespace-path))

(defn- drilldown-scene
  [state path scroll-x scroll-y]
  (scene-state/drilldown-scene state path scroll-x scroll-y
                               {:view-architecture view-architecture
                                :build-scene build-scene
                                :attach-drillable-markers attach-drillable-markers}))

(defn initial-scene-for-show
  [scene architecture]
  (scene-state/initial-scene-for-show scene architecture
                                      {:view-architecture view-architecture
                                       :build-scene build-scene
                                       :attach-drillable-markers attach-drillable-markers}))

(defn- push-nav-state
  [{:keys [namespace-path scroll-x scroll-y nav-stack] :as state}]
  (scene-state/push-nav-state state))

(defn show!
  ([scene]
   (show! scene {}))
  ([scene {:keys [title architecture]
           :or {title "architecture-viewer"}}]
   (let [effective-architecture (or architecture {:scene scene})
         initial-scene (initial-scene-for-show scene architecture)
         viewport-height (view-bootstrap/viewport-height-for-scene initial-scene content-height-for-scene)
         width (view-bootstrap/viewport-width-for-scene initial-scene content-width-for-scene)]
     (q/sketch
       :title title
       :size [width viewport-height]
      :setup (fn []
                (view-bootstrap/initial-sketch-state {:scene initial-scene
                                                      :architecture effective-architecture
                                                      :has-architecture? (boolean architecture)
                                                      :viewport-height viewport-height
                                                      :viewport-width width}))
       :draw draw-scene
       :key-pressed handle-key-pressed
       :mouse-wheel handle-mouse-wheel
       :mouse-pressed handle-mouse-pressed
       :mouse-dragged handle-mouse-dragged
        :mouse-released handle-mouse-released
        :middleware [m/fun-mode]))))

(defn wait-until-closed!
  [sketch]
  (quil-lifecycle/wait-until-closed! sketch))
