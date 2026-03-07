(ns arch-view.render.ui.quil.canvas
  (:require [quil.core :as q]))

(defn draw-scene-content
  [scene viewport-width spaced-edges {:keys [rendered-label module-point-map content-height-for-scene draw-edge]}]
  (q/background 250 250 250)
  (doseq [{:keys [x y width height label abstract?]} (:layer-rects scene)]
    (if abstract?
      (q/fill 226 242 226)
      (q/fill 225 233 242))
    (q/stroke 120 140 160)
    (q/rect x y width height)
    (q/fill 45 60 80)
    (q/text-align :left :top)
    (q/text label (+ x 8) (+ y 6)))
  (doseq [{:keys [x y kind] :as module-position} (:module-positions scene)]
    (if (= :abstract kind)
      (q/fill 0 128 0)
      (q/fill 15 20 30))
    (q/no-stroke)
    (q/text-align :center :center)
    (q/text (rendered-label module-position) x y))
  (let [points (module-point-map scene)
        bounds {:min-x 14.0
                :max-x (- (double viewport-width) 20.0)
                :min-y 14.0
                :max-y (- (content-height-for-scene scene) 14.0)}]
    (doseq [edge spaced-edges]
      (draw-edge points bounds edge))))

(defn draw-toolbar
  [{:keys [namespace-path declutter-mode nav-stack]}
   {:keys [back-button-rect declutter-button-rect back-button-label declutter-label toolbar-height]}]
  (let [back-rect (back-button-rect)
        declutter-rect (declutter-button-rect)
        can-go-back? (seq namespace-path)
        back-label (back-button-label {:namespace-path namespace-path
                                       :nav-stack nav-stack})]
    (q/no-stroke)
    (q/fill 238 242 246)
    (q/rect 0 0 3000 toolbar-height)
    (q/fill (if can-go-back? 225 205))
    (q/rect (:x back-rect) (:y back-rect) (:width back-rect) (:height back-rect))
    (if can-go-back?
      (q/fill 0 0 0)
      (q/fill 120 120 120))
    (q/text-align :center :center)
    (q/text back-label (+ (:x back-rect) (/ (:width back-rect) 2.0)) (+ (:y back-rect) (/ (:height back-rect) 2.0)))
    (q/fill 225)
    (q/rect (:x declutter-rect) (:y declutter-rect) (:width declutter-rect) (:height declutter-rect))
    (q/fill 0 0 0)
    (q/text-align :center :center)
    (q/text (declutter-label declutter-mode)
            (+ (:x declutter-rect) (/ (:width declutter-rect) 2.0))
            (+ (:y declutter-rect) (/ (:height declutter-rect) 2.0)))))

(defn draw-tooltip
  [full-name mx my]
  (q/fill 255 255 225)
  (q/stroke 80 80 80)
  (q/rect (+ mx 12) (+ my 12) (+ 12 (* 7 (count full-name))) 20)
  (q/fill 0 0 0)
  (q/no-stroke)
  (q/text-align :left :center)
  (q/text full-name (+ mx 18) (+ my 22)))

(defn draw-scrollbar
  [content-height viewport-height scroll-y viewport-width {:keys [scrollbar-rect]}]
  (when-let [{:keys [x y width height]} (scrollbar-rect content-height viewport-height scroll-y viewport-width)]
    (q/no-stroke)
    (q/fill 220 220 220)
    (q/rect (- x 1.0) 10.0 (+ width 2.0) (- viewport-height 20.0))
    (q/fill 120 120 120)
    (q/rect x y width height)))

(defn- world-bounds
  [sx sy viewport-width viewport-height z]
  (let [world-left (/ sx z)
        world-right (/ (+ sx (double viewport-width)) z)
        world-top (/ sy z)
        world-bottom (/ (+ sy (double viewport-height)) z)]
    {:min-x (+ world-left (/ 14.0 z))
     :max-x (- world-right (/ 20.0 z))
     :min-y (+ world-top (/ 14.0 z))
     :max-y (- world-bottom (/ 14.0 z))}))

(defn- draw-zoomed-scene
  [scene viewport-width spaced-edges z sx sy {:keys [draw-scene-content]}]
  (q/background 250 250 250)
  (q/push-matrix)
  (q/scale z)
  (q/translate (- (/ sx z)) (- (/ sy z)))
  (draw-scene-content scene viewport-width spaced-edges)
  (q/pop-matrix))

(defn- draw-hover-tooltip
  [hovered hovered-layer hovered-arrow mx my {:keys [draw-tooltip edge-hover-label]}]
  (cond
    hovered (draw-tooltip (:full-name hovered) mx my)
    (:full-name hovered-layer) (draw-tooltip (:full-name hovered-layer) mx my)
    hovered-arrow (draw-tooltip (edge-hover-label hovered-arrow) mx my)))

(defn draw-scene
  [{:keys [scene declutter-mode scroll-x scroll-y viewport-height viewport-width zoom] :as state}
   {:keys [scaled-content-height point-in-toolbar? module-point-map prepare-edge-drawables
           hovered-edge hovered-module-position hovered-layer-label edge-hover-label
           draw-scene-content draw-toolbar draw-tooltip draw-scrollbar]}]
  (let [z (double (or zoom 1.0))
        content-height (scaled-content-height scene z)
        mx (double (q/mouse-x))
        my (double (q/mouse-y))
        interactive-canvas? (not (point-in-toolbar? mx my))
        sx (double (or scroll-x 0.0))
        sy (double (or scroll-y 0.0))
        world-mx (+ (/ mx z) (/ sx z))
        world-my (+ (/ my z) (/ sy z))
        points (module-point-map scene)
        bounds (world-bounds sx sy viewport-width viewport-height z)
        spaced-edges (prepare-edge-drawables scene declutter-mode)
        hovered-arrow (when interactive-canvas?
                        (hovered-edge spaced-edges points bounds world-mx world-my (/ 8.0 z)))
        hovered (when interactive-canvas?
                  (hovered-module-position (:module-positions scene) world-mx world-my))
        hovered-layer (when interactive-canvas?
                        (hovered-layer-label (:layer-rects scene) world-mx world-my))]
    (q/cursor (if (:drillable? hovered)
                :cross
                :arrow))
    (draw-zoomed-scene scene viewport-width spaced-edges z sx sy
                       {:draw-scene-content draw-scene-content})
    (draw-toolbar state)
    (draw-hover-tooltip hovered hovered-layer hovered-arrow mx my
                        {:draw-tooltip draw-tooltip
                         :edge-hover-label edge-hover-label})
    (draw-scrollbar content-height viewport-height scroll-y viewport-width)))
