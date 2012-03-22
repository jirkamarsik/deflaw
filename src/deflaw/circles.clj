(ns deflaw.circles
  (:refer-clojure :exclude [==])
  (:use (seesaw graphics color)
        clojure.core.logic
        deflaw.aux-rels)
  (:require [clojure.core.logic.arithmetic :as la]))

(defonce world (atom {}))

(defn pick [coll]
  (first (shuffle coll)))

(defn init-pop [width height]
  (apply merge
         (repeatedly (rand 10)
                     #(let [type (pick [::loner ::seeker])]
                        {(gensym (name type))
                         {:type   type
                          :x      (rand width)
                          :y      (rand height)
                          :r      (+ 10 (rand 40))
                          :color  (color (int (+ 56 (rand 200)))
                                         (int (+ 56 (rand 200)))
                                         (int (+ 56 (rand 200))))
                          :stroke (+ 2 (rand 4))}}))))

(defn reset-world! []
  (swap! world (fn [{:keys [width height]}]
                 {:width    width
                  :height   height
                  :entities (init-pop width height)})))

(defmulti draw-entity (fn [g ent] (:type ent)))

(defmethod draw-entity ::circle [g ent]
  (draw g (circle (:x ent) (:y ent) (:r ent))
          (style :foreground (:color ent)
                 :background nil
                 :stroke (:stroke ent))))

(defmulti self-esteem (fn [state id]
                        (get-in state [:entities id :type])))

(defmethod self-esteem ::circle [state id]
  (rand))

(defn distances [ents id]
  (letfn [(distance-from [other-id]
            (let [x1 (get-in ents [id       :x])
                  x2 (get-in ents [other-id :x])
                  y1 (get-in ents [id       :y])
                  y2 (get-in ents [other-id :y])]
              (apply + (map #(* % %) [(- x2 x1) (- y2 y1)]))))]
    (map distance-from (keys (dissoc ents id)))))

(derive ::loner ::circle)
(defmethod self-esteem ::loner [state id]
  (apply min (distances (:entities state) id)))

(derive ::seeker ::circle)
(defmethod self-esteem ::seeker [state id]
  (- (apply min (distances (:entities state) id))))

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
