(ns deflaw.aux-rels
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

(defn geto
  "`k' is associated to `v' in the map `m'. (I**))"
  [m k v]
  (project [m]
    (if-not (or (map? m) (nil? m))
      fail
      (conda
        [(nonlvaro k) (project [k]
                        (if-let [[_ fv] (find m k)]
                          (== v fv)
                          fail))]
        [(fresh [kvp]
           (membero kvp (seq m))
           (== kvp [k v]))]))))

(defn assoco
  "`nm' is the map resulting from associng
 `k' to 'v' in the map `m'. (II**)"
  [m k v nm]
  (project [m k]
    (if-not (or (map? m) (nil? m))
      fail
      (== nm (assoc m k v)))))

(defn get-ino
  "Within a hierarchy of maps rooted in `m',
  the path of keys `ks' is associated to `v'. (I**)"
  [m ks v]
  (matche [ks]
    ([[]]       (== v m))
    ([[k . mks]] (fresh [im]
                  (geto    m  k  im)
                  (get-ino im mks v)))))

(defn assoc-ino
  "`nm' is the root of a hierarchy of maps resulting
  from associating the path of keys `ks' to `v' in `m'. (II**)"
  [m ks v nm]
  (matche [ks]
    ([[]]       (== nm v))
    ([[k . mks]] (fresh [im inm]
                   (conda
                     [(geto m k im)]
                     [(nilo im)])
                   (assoc-ino im mks  v inm)
                   (assoco     m   k inm nm)))))

(defn update-ino
  "`nm' is the hierarchy of maps resulting from replacing the value
  associated with `ks' in `m' with the value of applying `f'
  to the original value and the additional arguments `as'. (II*I...)"
  [m ks nm f & as]
  (matche [ks]
    ([[]]        (project [f m as]
                   (== nm (apply f (list* m as)))))
    ([[k . mks]] (fresh [im inm]
                   (geto m k im)
                   (apply update-ino (list* im mks inm f as))
                   (assoco m k inm nm)))))