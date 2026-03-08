;; mutation-tested: 2026-03-08
(ns arch-view.render.ui.util.quil-lifecycle)

(defn safe-looping?
  [sketch]
  (try
    (.isLooping sketch)
    (catch Throwable _
      false)))

(defn safe-displayable?
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
