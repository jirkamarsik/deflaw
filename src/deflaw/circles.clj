(ns deflaw.circles
  (:refer-clojure :exclude [==])
  (:use seesaw.graphics
        clojure.core.logic
        deflaw.aux-rels)
  (:require [clojure.core.logic.arithmetic :as la]))

(defonce world (atom {:entities {0 {:type   :circle
                                    :x      200
                                    :y      200
                                    :r      25
                                    :color  :red
                                    :stroke 3}}}))

(defmulti draw-entity (fn [g ent] (:type ent)))

(defmethod draw-entity :circle [g ent]
  (draw g (circle (:x ent) (:y ent) (:r ent))
          (style :foreground (:color ent)
                 :background nil
                 :stroke (:stroke ent))))

(defmulti self-esteem (fn [state id]
                        (get-in state [:entities id :type])))

(defmethod self-esteem :circle [state id]
  (rand))

(defn in-boundso [state id]
  (fresh [x y w h]
    (get-ino state [:entities id :x] x)
    (get-ino state [:entities id :y] y)
    (geto state :width w)
    (geto state :height h)
    (la/<= 0 x)
    (la/<= x w)
    (la/<= 0 y)
    (la/<= y h)))

(defn possibleo [old id new]
  (fresh [dim dir speed]
   (membero dim [:x :y])
   (membero dir [+ -])
   (get-ino old [:entities id :stroke] speed)
   (update-ino old [:entities id dim] new dir speed)
   (in-boundso new id)))

(defn act-entity [old-state id]
  (apply max-key #(self-esteem % id)
         (shuffle (cons old-state
                        (run* [new-state]
                          (possibleo old-state id new-state))))))
