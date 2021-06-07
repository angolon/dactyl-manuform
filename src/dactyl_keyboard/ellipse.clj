(ns dactyl-keyboard.dactyl.ellipse
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [distance div add sub length dot]]
            [unicode-math.core :refer :all]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            ))

(defrecord Ellipse [a b])

(defn x-coordinate [ellipse θ] (* (:a ellipse) (Math/cos θ)))
(defn y-coordinate [ellipse θ] (* (:b ellipse) (Math/sin θ)))
(defn coordinates [ellipse θ] [(x-coordinate ellipse θ) (y-coordinate ellipse θ)])
(defn chord [ellipse α β] [(coordinates ellipse α) (coordinates ellipse β)])
(defn chord-length [ellipse α β] (apply distance (chord ellipse α β)))
(defn chord-midpoint [ellipse α β]
  (let [[α-xy β-xy] (chord ellipse α β)
        α-to-β (sub β-xy α-xy)
        halfway (div α-to-β 2)]
    (add α-xy halfway)
    ))

; helper functions for embed - copypasta from the hacky versions in the main
; dactyl script, but modified for two dimensions.
(def x-axis [1 0])
(def y-axis [0 1])

(defn angle-between [v1 v2]
  (let [mag-v1 (length v1)
        mag-v2 (length v2)
        denom  (* mag-v1 mag-v2)
        numer  (dot v1 v2)]
    (->> (/ numer denom)
         Math/acos
         )))

;returns nil if the vector is zero (at the origin)... I think ⁻\_(ツ)_/⁻
(defn angle-to-x [v]
  (let [θ (angle-between x-axis v)]
    (if (>= (get v 1) 0) ; y is non-negative
      θ
      (- (* 2 π) θ)
      )))

; semimajor axis, semiminor axis, angle of the starting point parametrically
(defn chord-next-point [ellipse target-chord-length initial-θ]
  (let [
        ; binary search
        improve (fn [[lower-bound upper-bound θ]]
                  (let [d (chord-length ellipse initial-θ θ)]
                    (if (<= d target-chord-length)
                      [θ upper-bound (+ θ (/ (- upper-bound θ) 2))] 
                      [lower-bound θ (- θ (/ (- θ lower-bound) 2))]
                       )))
        initial-guess (+ initial-θ π)
        improvements (iterate improve [initial-θ (+ initial-θ (* 2 π)) initial-guess])
        terminate (fn [[_ _ α] [_ _ β]]
                    (or
                      (= α β)
                      (< (Math/abs (- α β)) 1e-10)
                      ))]

    (->> improvements
        (reduce #(if (terminate %1 %2) (reduced %2) %2))
        last
        )
    ))

(defn embed [ellipse initial-θ length shape index]
  (let [chordfn (partial chord-next-point ellipse length)
        chords (iterate chordfn initial-θ)
        [α β] (take 2 (drop index chords)) ; the two angles to the points on the ellpise
        [coordinates-a coordinates-b] (chord ellipse α β)
        chord-vector (sub coordinates-b coordinates-a)
        rotation (angle-to-x chord-vector)
        midpoint (chord-midpoint ellipse α β)
        ]
      (->> shape
           ; rotate about z-axis to match the angle of the chord.
           (rotate rotation [0 0 1])
           (translate midpoint)
           )
    ))
; (defn ellipse-embed [a b shape]
 
