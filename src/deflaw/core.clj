(ns deflaw.core
  (:use seesaw.core
        deflaw.circles))

(native!)

(defn draw-world [c g]
  (doseq [ent (vals (:entities @world))]
    (draw-entity g ent)))

(defn update-world []
  (doseq [id (keys (:entities @world))]
    (swap! world act-entity id)))

(defn canvas-resized [e]
  (swap! world assoc :width (.getWidth (to-widget e)))
  (swap! world assoc :height (.getHeight (to-widget e))))

(defonce my-canvas (canvas :background :black
                           :paint      draw-world
                           :listen     [:component-resized canvas-resized]))

(defonce my-frame (frame :title "deflaw" :content my-canvas))

(show! my-frame)
(canvas-resized my-canvas)
(reset-world!)

(defonce my-timer (timer (fn [time]
                           (update-world)
                           (.repaint my-canvas)
                           (inc time))
                         :initial-value 0
                         :delay 40))
