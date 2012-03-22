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
                     #(let [type (pick [::loner ::seeker ::consumer])]
                        {(gensym (name type))
                         {:type   type
                          :x      (rand width)
                          :y      (rand height)
                          :r      (if (= type ::consumer)
                                    (+ 30 (rand 60))
                                    (+ 10 (rand 40)))
                          :color  (if (= type ::consumer)
                                    :red
                                    (color (+ 56 (rand-int 200))
                                           (+ 56 (rand-int 200))
                                           (+ 56 (rand-int 200))))
                          :stroke (if (= type ::consumer)
                                    10
                                    (+ 2 (rand 4)))}}))))

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

(defn distance2 [x1 y1 x2 y2]
  (apply + (map #(* % %) [(- x2 x1) (- y2 y1)])))

(defn distances [ents id]
  (letfn [(distance-from [other-id]
            (let [x1 (get-in ents [id       :x])
                  x2 (get-in ents [other-id :x])
                  y1 (get-in ents [id       :y])
                  y2 (get-in ents [other-id :y])]
              (distance2 x1 y1 x2 y2)))]
    (map distance-from (keys (dissoc ents id)))))

(derive ::loner ::circle)
(defmethod self-esteem ::loner [state id]
  (apply min (distances (:entities state) id)))

(derive ::seeker ::circle)
(defmethod self-esteem ::seeker [state id]
  (- (apply min (distances (:entities state) id))))

(derive ::consumer ::circle)
(defmethod self-esteem ::consumer [state id]
  (get-in state [:entities id :r]))

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

(defn toucho [state id1 id2]
  (fresh [e1 x1 y1 r1 e2 x2 y2 r2]
    (get-ino state [:entities id1] e1)
    (get-ino state [:entities id2] e2)
    (geto e1 :x x1)
    (geto e1 :y y1)
    (geto e1 :r r1)
    (geto e2 :x x2)
    (geto e2 :y y2)
    (geto e2 :r r2)
    (project [x1 y1 r1 x2 y2 r2]
      (la/<= (distance2 x1 y1 x2 y2) (* (+ r1 r2) (+ r1 r2))))))

(defn possibleo [old id new]
  (conde
    [(get-ino old [:entities id :type] ::consumer)
     (fresh [r oid or inter]
       (get-ino old [:entities  id :r]  r)
       (get-ino old [:entities oid :r] or)
       (!= id oid)
       (la/> r or)
       (la/> or 0)
       (toucho old id oid)
       (update-ino old   [:entities  id :r] inter inc)
       (update-ino inter [:entities oid :r] new   dec))]
    [(fresh [type dim dir speed]
       (get-ino old [:entities id :type] type)
       (membero dim [:x :y])
       (membero dir [+ -])
       (get-ino old [:entities id :stroke] speed)
       (update-ino old [:entities id dim] new dir speed)
       (in-boundso new id))]))

(defn act-entity [old-state id]
  (apply max-key #(self-esteem % id)
         (shuffle (cons old-state
                        (run* [new-state]
                          (possibleo old-state id new-state))))))
