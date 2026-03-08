;; mutation-tested: 2026-03-08
(ns arch-view.render.ui.util.viewport)

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

(defn point-in-rect?
  [{:keys [x y width height]} px py]
  (and (<= x px (+ x width))
       (<= y py (+ y height))))

(defn content-height-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [y height]}] (+ y height)))
       (apply max 0)
       (+ 40)))

(defn scaled-content-height
  [scene zoom]
  (* (double (or zoom 1.0))
     (content-height-for-scene scene)))

(defn content-width-for-scene
  [scene racetrack-margin]
  (->> (:layer-rects scene)
       (map (fn [{:keys [x width]}] (+ x width)))
       (apply max 0)
       (+ racetrack-margin 20.0)))

(defn scaled-content-width
  [scene zoom racetrack-margin]
  (* (double (or zoom 1.0))
     (content-width-for-scene scene racetrack-margin)))
