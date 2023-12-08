(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [add array matrix mmul distance]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [euclidean.math.vector :as v]
            [euclidean.math.quaternion :as q]
            [dactyl-keyboard.dactyl.ellipse :as e]
            ))

(defrecord Finger [width metacarpal proximal intermediate distal])
(defrecord Thumb [width metacarpal proximal distal])
(defrecord Wrist [width length height])
(defrecord Hand [wrist thumb fingers])

; For (what I hope is) ease of reasoning, we represent the cylinder
; much like a line segment, but with a radius.
(defrecord Cylinder [point1 point2 radius])

(defn thumb-wrist-attachment [thumb wrist]
  (v/vector (-(:width thumb)) (/ (:length wrist) 2.0) 0.0))

(defn finger-wrist-attachment [hand idx]
  (let [
        wrist (:wrist hand)
        divided (split-at idx (:fingers hand))
        prior-fingers (first divided)
        finger (first (second divided))
        offset (reduce + (map :width prior-fingers))
        center-x (/ (:width finger) 2.0)
        center-y (/ (:height wrist) 2.0)
        ]
    ; (v/vector (+ offset center-x) (:length wrist) center-z)
    (v/vector offset center-y (:length wrist))
    ))

(def line-radius 1)
(defn line
  "This function will use all the fancy to draw a line from point A to
  point B."
  [from to]
  (if (= from to)
    (sphere line-radius)
    (let [diff (map - to from)
          norm (distance from to)
          rotate-angle (Math/acos (/ (last diff) norm))
          rotate-axis [(- (nth diff 1)) (nth diff 0) 0]]
      (union
       (sphere line-radius)
       (translate [0 0 norm]
                  (sphere line-radius))
       (->> (cylinder line-radius norm)
         (translate [0 0 (/ norm 2)])
         (rotate rotate-angle rotate-axis)
         (translate from))))))

(def pcb-color [9/255 77/255 28/255 1])
(defn catenary [a]
  (let [f (fn [x] (* a (Math/cosh (/ x a))))
        xs (range -100 100 0.1)
        xyzs (map (fn [x] [x (f x) 0.0]) xs)
        segments (partition 2 1 xyzs)
        lines (map (fn [[p1 p2]] (line p1 p2)) segments)
        ]
    (apply union lines)))
; (defn radial-scaphoid-column [wrist]
;   (


(defn catenary-a [width height]
  (/ (- (* width width) (* height height)) (* 2 height)))

; s = arc-length, a = scale
(defn catenary-derivative [s a] (/ s a))

(defn render-hand [hand]
  (let [
        thumb (:thumb hand)
        fingers (:fingers hand)
        wrist (:wrist hand)
        rendered-thumb (->> (cylinder (/ (:width thumb) 2) (:metacarpal thumb))
                            (translate [(/ (:width thumb) 2) 0 (/ (:metacarpal thumb) 2)])
                            (translate (thumb-wrist-attachment thumb wrist)))
        rendered-wrist (->> (cube (:width wrist) (:height wrist) (:length wrist))
                            (translate [(/ (:width wrist) 2) (/ (:height wrist) 2) (/ (:length wrist) 2)]))
        incremental-knuckle-distance [0 35 28 23]
        cumulative-knuckle-distance (reductions + incremental-knuckle-distance)
        carpal-arch-width 89
        carpal-arch-a (catenary-a 80 53)
        carpal-arch (->> (catenary carpal-arch-a)
                         (rotate π [0 0 1])
                         (translate [(/ carpal-arch-width 2) (* 2 carpal-arch-a) 0])
                         )
        metacarpal-arch-height 53
        metacarpal-arch-a (catenary-a 140 metacarpal-arch-height)
        metacarpal-arch (->> (catenary metacarpal-arch-a)
                             (rotate π [0 0 1])
                             (translate [0 (+ metacarpal-arch-a metacarpal-arch-height) 87])
                             )
        render-finger (fn [idx finger]
                        (let [attachment (finger-wrist-attachment hand idx)]
                          (->> (cylinder (/ (:width finger) 2) (:metacarpal finger))
                               (translate [(/ (:width finger) 2) 0 (/ (:metacarpal finger) 2)])
                               (translate attachment))))


        ]
    (apply union
           (concat [rendered-wrist rendered-thumb carpal-arch metacarpal-arch]
                   (map-indexed render-finger fingers)
                   ))))

(def my-hand
  (let [
        wrist (->Wrist 83.5 42.6 21.2)
        thumb (->Thumb 23 53.6 44.3 38)
        index-finger (->Finger 18.5 63.6 45.1 32.0 22.1)
        middle-finger (->Finger 17.5 64.3 49.7 35.6 24.3)
        ]
    (->Hand wrist thumb [index-finger middle-finger])
    ))

(spit "things/hand.scad"
      (write-scad (render-hand my-hand)))
