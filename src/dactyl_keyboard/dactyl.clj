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


(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 9))            ; or, change this for more precise tenting control
(def column-style
  (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed
; (def column-style :fixed)

(defn column-offset [column] (cond
  (= column 2) [0 2.82 -4.5]
  (>= column 4) [0 -12 5.64]            ; original [0 -5.8 5.64]
  :else [0 0 0]))

(def thumb-offsets [6 -3 -6])

(def keyboard-z-offset 7)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -15)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 8)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

; Angus' hax variables
(def pcb-z-clearance 5)
(def pcb-thickness 1.5)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.4) ;; Was 14.1, then 14.25
(def keyswitch-width 14.4)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 plate-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ plate-thickness 2)]))))
        plate-half (union top-wall left-wall (with-fn 100 side-nub))]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ sa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PCB shapes and offset functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Approximate model of switch, for visualisation (and maybe even collision detection)
; purposes
(def cherry-switch-pin-height 3.3)
(def cherry-switch-pin (->> (cylinder [0.75 0.75] cherry-switch-pin-height)
                            (translate [0 0 (/ cherry-switch-pin-height 2)])
                            (color [203/255 109/255 81/255 1])))

(def cherry-switch-body-color [0.2 0.2 0.2 1])
(def cherry-switch-pcb-centre-column
  (let [top-cylinder-height 2.25
        top-cylinder-offset (- cherry-switch-pin-height top-cylinder-height)
        top-cylinder (->> (cylinder [2 2] top-cylinder-height)
                         (translate [0 0 (+ (/ top-cylinder-height 2) top-cylinder-offset)]))
        bottom-disc (->> (circle 1)
                         (extrude-linear {:height 0.1 :twist 0 :convexivity 0})
                         (translate [0 0 0.05]))]
    (->> bottom-disc
         (hull top-cylinder)
         (color cherry-switch-body-color)
         )))

(def cherry-switch-underside
  (let [underside-height 5.0
        top-cube-height 1.0
        top-cube (->> (cube keyswitch-width keyswitch-width top-cube-height)
                      (translate [0 0 (- underside-height (/ top-cube-height 2))]))
        bottom-cube-width (- keyswitch-width 2)
        bottom-cube (->> (cube bottom-cube-width bottom-cube-width 0.1)
                         (translate [0 0 0.05]))
        ]
  (->> bottom-cube
       (hull top-cube)
       (color cherry-switch-body-color)
       )))

(def cherry-switch-border
  (let [border-height 0.6
        border-width 15.6
        gap-width 7.6
        ; make the height and length of the gaps bigger to eliminate weird artifacting?
        border-gaps (->> (cube gap-width (+ 1 border-width) (* 2 border-height))
                         (union (cube (+ 1 border-width) gap-width (* 2 border-height))))
        border-centre (cube keyswitch-width keyswitch-width border-height)
        border-corners (->> (cube border-width border-width border-height)
                            (#(difference % border-gaps)))
        ]
    (->> border-centre
         (union border-corners)
         (translate [0 0 (/ border-height 2)])
         (color cherry-switch-body-color)
         )))

(def cherry-switch-topside
  (let [topside-height 6
        topside-width (- keyswitch-width 3)
        bottom-cube (->> (cube keyswitch-width keyswitch-width 0.1)
                      (translate [0 0 0.05]))
        top-cube (->> (cube topside-width topside-width 0.1)
                      (translate [0 0 (- topside-height 0.05)]))
        ]
    (->> top-cube
         (hull bottom-cube)
         (color cherry-switch-body-color)
         )))

(def cherry-switch-stem
  (let [height 3.6
        width 4
        length 1
        wall-1 (cube width length height)
        wall-2 (cube length width height)]
    (->> wall-1
         (union wall-2)
         (translate [0 0 (/ height 2)])
         (color [67/255 5/255 65/255 1])
        ))) 

(def cherry-switch
  (let [spec-gridline-width 1.27
        pin-1 (translate [(* -3 spec-gridline-width) (* 2 spec-gridline-width) 0] cherry-switch-pin)
        pin-2 (translate [(* 2 spec-gridline-width) (* 4 spec-gridline-width) 0] cherry-switch-pin)
        pcb-interface (union cherry-switch-pcb-centre-column pin-1 pin-2)]
    (union
      pcb-interface
      (translate [0 0 cherry-switch-pin-height] cherry-switch-underside)
      (translate [0 0 (+ cherry-switch-pin-height 5)] cherry-switch-border)
      (translate [0 0 (+ cherry-switch-pin-height 5 0.6)] cherry-switch-topside)
      (translate [0 0 (+ 11.6 cherry-switch-pin-height)] cherry-switch-stem)
      )))

; (def cherry-switch
;   (let [pin (->>cylinder 
;; Shape of the flat section of the pcb for each key
(def pcb-color [9/255 77/255 28/255 1])
(def keycap-space-buffer 0.5)
(def pcb-key-width (+ sa-length keycap-space-buffer))
(def pcb-offset (- cherry-switch-pin-height pcb-thickness))

(def single-key-pcb
  (->> (cube pcb-key-width pcb-key-width pcb-thickness)
       (translate [0 0 (/ pcb-thickness 2)])
       (translate [0 0 pcb-offset])
       (#(difference % cherry-switch))
       (color pcb-color)
       ))

(def pcb-web-thickness pcb-thickness)
(def pcb-post-size 0.1)
(def pcb-web-post (->> (cube pcb-post-size pcb-post-size pcb-web-thickness)
                       (translate [0 0 (/ pcb-web-thickness 2)])
                       (translate [0 0 pcb-offset])
                       (color pcb-color)))

(def pcb-post-adj (/ pcb-post-size 2))
(def pcb-web-post-tr
  (translate
    [(- (/ pcb-key-width 2) pcb-post-adj)
     (- (/ pcb-key-width 2) pcb-post-adj)
     0] pcb-web-post))

(def pcb-web-post-tl
  (translate
    [(+ (/ pcb-key-width -2) pcb-post-adj)
     (- (/ pcb-key-width 2) pcb-post-adj)
     0] pcb-web-post))

(def pcb-web-post-bl
  (translate
    [(+ (/ pcb-key-width -2) pcb-post-adj)
     (+ (/ pcb-key-width -2) pcb-post-adj)
     0] pcb-web-post))

(def pcb-web-post-br
  (translate
    [(- (/ pcb-key-width 2) pcb-post-adj)
     (+ (/ pcb-key-width -2) pcb-post-adj)
     0] pcb-web-post))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))
(def column-base-angle (* β (- centercol 2)))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0])
                                )]
    (->> (case column-style
          :orthographic placed-shape-ortho
          :fixed        placed-shape-fixed
                        placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
    (fn [angle obj] (rotate angle [1 0 0] obj))
    (fn [angle obj] (rotate angle [0 1 0] obj))
    column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))


(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> (sa-cap (if (= column 5) 1 1))
                (key-place column row)))))

; (pr (rotate-around-y π [10 0 1]))
; (pr (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0]))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
         thumb-offsets))
; (pr thumborigin)

(defn thumb-tr-place [shape]
  (->> shape
      ;  (rotate (deg2rad  10) [1 0 0])
      ;  (rotate (deg2rad -23) [0 1 0])
      ;  (rotate (deg2rad  -3) [0 0 1])
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-12 -16 3])
       ))
(defn thumb-tl-place [shape]
  (->> shape
      ;  (rotate (deg2rad  10) [1 0 0])
      ;  (rotate (deg2rad -23) [0 1 0])
      ;  (rotate (deg2rad  -3) [0 0 1])
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-32 -15 -2])))
(defn thumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  48) [0 0 1])
       (translate thumborigin)
       (translate [-29 -40 -13])
       ))
(defn thumb-ml-place [shape]
  (->> shape
       (rotate (deg2rad   6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  40) [0 0 1])
       (translate thumborigin)
       (translate [-51 -25 -12])))
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad -16) [1 0 0])
       (rotate (deg2rad -33) [0 1 0])
       (rotate (deg2rad  54) [0 0 1])
       (translate thumborigin)
       (translate [-37.8 -55.3 -25.3])
       ))
(defn thumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad  -4) [1 0 0])
       (rotate (deg2rad -35) [0 1 0])
       (rotate (deg2rad  52) [0 0 1])
       (translate thumborigin)
       (translate [-56.3 -43.3 -23.5])
       ))

(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-ml-place shape)
   (thumb-br-place shape)
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)
   (thumb-tl-place shape)))

(def larger-plate
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        ]
    (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))


(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)
   (thumb-15x-layout larger-plate)
   ))

(def pcb-thumb
  (union
   (thumb-1x-layout single-key-pcb)
   (thumb-15x-layout single-key-pcb)
   ))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  1.15) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  1.15) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -1.15) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -1.15) post-adj) 0] web-post))

(def thumb-connectors
  (union
      (triangle-hulls    ; top two
             (thumb-tl-place thumb-post-tr)
             (thumb-tl-place thumb-post-br)
             (thumb-tr-place thumb-post-tl)
             (thumb-tr-place thumb-post-bl))
      (triangle-hulls    ; bottom two on the right
             (thumb-br-place web-post-tr)
             (thumb-br-place web-post-br)
             (thumb-mr-place web-post-tl)
             (thumb-mr-place web-post-bl))
      (triangle-hulls    ; bottom two on the left
             (thumb-bl-place web-post-tr)
             (thumb-bl-place web-post-br)
             (thumb-ml-place web-post-tl)
             (thumb-ml-place web-post-bl))
      (triangle-hulls    ; centers of the bottom four
             (thumb-br-place web-post-tl)
             (thumb-bl-place web-post-bl)
             (thumb-br-place web-post-tr)
             (thumb-bl-place web-post-br)
             (thumb-mr-place web-post-tl)
             (thumb-ml-place web-post-bl)
             (thumb-mr-place web-post-tr)
             (thumb-ml-place web-post-br))
      (triangle-hulls    ; top two to the middle two, starting on the left
             (thumb-tl-place thumb-post-tl)
             (thumb-ml-place web-post-tr)
             (thumb-tl-place thumb-post-bl)
             (thumb-ml-place web-post-br)
             (thumb-tl-place thumb-post-br)
             (thumb-mr-place web-post-tr)
             (thumb-tr-place thumb-post-bl)
             (thumb-mr-place web-post-br)
             (thumb-tr-place thumb-post-br))
      (triangle-hulls    ; top two to the main keyboard, starting on the left
             (thumb-tl-place thumb-post-tl)
             (key-place 0 cornerrow web-post-bl)
             (thumb-tl-place thumb-post-tr)
             (key-place 0 cornerrow web-post-br)
             (thumb-tr-place thumb-post-tl)
             (key-place 1 cornerrow web-post-bl)
             (thumb-tr-place thumb-post-tr)
             (key-place 1 cornerrow web-post-br)
             (key-place 2 lastrow web-post-tl)
             (key-place 2 lastrow web-post-bl)
             (thumb-tr-place thumb-post-tr)
             (key-place 2 lastrow web-post-bl)
             (thumb-tr-place thumb-post-br)
             (key-place 2 lastrow web-post-br)
             (key-place 3 lastrow web-post-bl)
             (key-place 2 lastrow web-post-tr)
             (key-place 3 lastrow web-post-tl)
             (key-place 3 cornerrow web-post-bl)
             (key-place 3 lastrow web-post-tr)
             (key-place 3 cornerrow web-post-br)
             (key-place 4 cornerrow web-post-bl))
      (triangle-hulls
             (key-place 1 cornerrow web-post-br)
             (key-place 2 lastrow web-post-tl)
             (key-place 2 cornerrow web-post-bl)
             (key-place 2 lastrow web-post-tr)
             (key-place 2 cornerrow web-post-br)
             (key-place 3 cornerrow web-post-bl)
             )
      (triangle-hulls
             (key-place 3 lastrow web-post-tr)
             (key-place 3 lastrow web-post-br)
             (key-place 3 lastrow web-post-tr)
             (key-place 4 cornerrow web-post-bl))
  ))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 10)
(def left-wall-z-offset  3)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))


(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
    (hull
      (place1 post1)
      (place1 (translate (wall-locate1 dx1 dy1) post1))
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 post2)
      (place2 (translate (wall-locate1 dx2 dy2) post2))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))
    (bottom-hull
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))
      ))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def case-walls
  (union
   ; back wall
   (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   ; right wall
   (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y       1 0 web-post-br))
   (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))
   (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 1 0 web-post-br)
   ; left wall
   (for [y (range 0 lastrow)] (union (wall-brace (partial left-key-place y 1)       -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
                                     (hull (key-place 0 y web-post-tl)
                                           (key-place 0 y web-post-bl)
                                           (left-key-place y  1 web-post)
                                           (left-key-place y -1 web-post))))
   (for [y (range 1 lastrow)] (union (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
                                     (hull (key-place 0 y       web-post-tl)
                                           (key-place 0 (dec y) web-post-bl)
                                           (left-key-place y        1 web-post)
                                           (left-key-place (dec y) -1 web-post))))
   (wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) 0 1 web-post)
   (wall-brace (partial left-key-place 0 1) 0 1 web-post (partial left-key-place 0 1) -1 0 web-post)
   ; front wall
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   (key-wall-brace 3 lastrow   0 -1 web-post-bl 3 lastrow 0.5 -1 web-post-br)
   (key-wall-brace 3 lastrow 0.5 -1 web-post-br 4 cornerrow 1 -1 web-post-bl)
   (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
   (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
   ; thumb walls
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-tr-place  0 -1 thumb-post-br)
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-mr-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-ml-place -0.3  1 web-post-tr thumb-ml-place  0  1 web-post-tl)
   (wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
   (wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)
   ; thumb corners
   (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)
   ; thumb tweeners
   (wall-brace thumb-mr-place  0 -1 web-post-bl thumb-br-place  0 -1 web-post-br)
   (wall-brace thumb-ml-place  0  1 web-post-tl thumb-bl-place  0  1 web-post-tr)
   (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
   (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr)))
   (hull
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
     (thumb-tl-place thumb-post-tl))
   (hull
     (left-key-place cornerrow -1 web-post)
     (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-tl-place thumb-post-tl))
   (hull
     (left-key-place cornerrow -1 web-post)
     (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
     (key-place 0 cornerrow web-post-bl)
     (key-place 0 cornerrow (translate (wall-locate1 -1 0) web-post-bl))
     (thumb-tl-place thumb-post-tl))
   (hull
     (thumb-ml-place web-post-tr)
     (thumb-ml-place (translate (wall-locate1 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
     (thumb-tl-place thumb-post-tl))
  ))


(def rj9-start  (map + [0 -3  0] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def rj9-position  [(first rj9-start) (second rj9-start) 11])
(def rj9-cube   (cube 14.78 13 22.38))
(def rj9-space  (translate rj9-position rj9-cube))
(def rj9-holder (translate rj9-position
                  (difference rj9-cube
                              (union (translate [0 2 0] (cube 10.78  9 18.38))
                                     (translate [0 0 5] (cube 10.78 13  5))))))

(def usb-holder-position (key-position 1 0 (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0])))
(def usb-holder-size [6.5 10.0 13.6])
(def usb-holder-thickness 4)
(def usb-holder
    (->> (cube (+ (first usb-holder-size) usb-holder-thickness) (second usb-holder-size) (+ (last usb-holder-size) usb-holder-thickness))
         (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))
(def usb-holder-hole
    (->> (apply cube usb-holder-size)
         (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))

(def teensy-width 20)
(def teensy-height 12)
(def teensy-length 33)
(def teensy2-length 53)
(def teensy-pcb-thickness 2)
(def teensy-holder-width  (+ 7 teensy-pcb-thickness))
(def teensy-holder-height (+ 6 teensy-width))
(def teensy-offset-height 5)
(def teensy-holder-top-length 18)
(def teensy-top-xy (key-position 0 (- centerrow 1) (wall-locate3 -1 0)))
(def teensy-bot-xy (key-position 0 (+ centerrow 1) (wall-locate3 -1 0)))
(def teensy-holder-length (- (second teensy-top-xy) (second teensy-bot-xy)))
(def teensy-holder-offset (/ teensy-holder-length -2))
(def teensy-holder-top-offset (- (/ teensy-holder-top-length 2) teensy-holder-length))

(def teensy-holder
    (->>
        (union
          (->> (cube 3 teensy-holder-length (+ 6 teensy-width))
               (translate [1.5 teensy-holder-offset 0]))
          (->> (cube teensy-pcb-thickness teensy-holder-length 3)
               (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-offset (- -1.5 (/ teensy-width 2))]))
          (->> (cube 4 teensy-holder-length 4)
               (translate [(+ teensy-pcb-thickness 5) teensy-holder-offset (-  -1 (/ teensy-width 2))]))
          (->> (cube teensy-pcb-thickness teensy-holder-top-length 3)
               (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-top-offset (+ 1.5 (/ teensy-width 2))]))
          (->> (cube 4 teensy-holder-top-length 4)
               (translate [(+ teensy-pcb-thickness 5) teensy-holder-top-offset (+ 1 (/ teensy-width 2))])))
        (translate [(- teensy-holder-width) 0 0])
        (translate [-1.4 0 0])
        (translate [(first teensy-top-xy)
                    (- (second teensy-top-xy) 1)
                    (/ (+ 6 teensy-width) 2)])
           ))

(defn screw-insert-shape [bottom-radius top-radius height]
   (union (cylinder [bottom-radius top-radius] height)
          (translate [0 0 (/ height 2)] (sphere top-radius))))

(defn screw-insert [column row bottom-radius top-radius height]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                       (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                        (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                       (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))
        ]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate [(first position) (second position) (/ height 2)])
    )))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 0         bottom-radius top-radius height)
         (screw-insert 0 (- lastrow 0.6)   bottom-radius top-radius height)
         (screw-insert 2 (+ lastrow 0.35)  bottom-radius top-radius height)
         (screw-insert 3 0         bottom-radius top-radius height)
         (screw-insert lastcol 1   bottom-radius top-radius height)
         ))
(def screw-insert-height 3.8)
(def screw-insert-bottom-radius (/ 5.31 2))
(def screw-insert-top-radius (/ 5.1 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.6) (+ screw-insert-top-radius 1.6) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(def wire-post-height 7)
(def wire-post-overhang 3.5)
(def wire-post-diameter 2.6)
(defn wire-post [direction offset]
   (->> (union (translate [0 (* wire-post-diameter -0.5 direction) 0] (cube wire-post-diameter wire-post-diameter wire-post-height))
               (translate [0 (* wire-post-overhang -0.5 direction) (/ wire-post-height -2)] (cube wire-post-diameter wire-post-overhang wire-post-diameter)))
        (translate [0 (- offset) (+ (/ wire-post-height -2) 3) ])
        (rotate (/ α -2) [1 0 0])
        (translate [3 (/ mount-height -2) 0])))

(def wire-posts
  (union
     (thumb-ml-place (translate [-5 0 -2] (wire-post  1 0)))
     (thumb-ml-place (translate [ 0 0 -2.5] (wire-post -1 6)))
     (thumb-ml-place (translate [ 5 0 -2] (wire-post  1 0)))
     (for [column (range 0 lastcol)
           row (range 0 cornerrow)]
       (union
        (key-place column row (translate [-5 0 0] (wire-post 1 0)))
        (key-place column row (translate [0 0 0] (wire-post -1 6)))
        (key-place column row (translate [5 0 0] (wire-post  1 0)))))))


(def model-right (difference
                   (union
                    key-holes
                    connectors
                    thumb
                    thumb-connectors
                    (difference (union case-walls
                                       screw-insert-outers
                                       teensy-holder)
                                       ; usb-holder)
                                ; rj9-space
                                ; usb-holder-hole
                                screw-insert-holes)
                    ; rj9-holder
                    wire-posts
                    ; thumbcaps
                    ; caps
                    )
                   (translate [0 0 -20] (cube 350 350 40))
                  ))

;;;;;;;;;;;;;;;;;
;; Thumb Hacks ;;
;;;;;;;;;;;;;;;;;
(def thumb-length 78)
(def thumb-column-radius thumb-length)
(def thumb-row-radius thumb-length)
(def thumb-start-angle (/ π 4)) ; offset row count for rotation, because thumbs are... steeper.

; Angle of chord on a circle of the given radius.
(defn chord-θ [radius chord-length]
  (->> radius
       (* 2)
       (/ chord-length)
       (Math/asin)
       (* 2)
       ))

(def thumb-row-circumference (* 2 π thumb-row-radius))
(def thumb-column-circumference (* 2 π thumb-column-radius))

(def x-axis (v/vector 1 0 0))
(def y-axis (v/vector 0 1 0))
(def z-axis (v/vector 0 0 1))

(defn angle-between [v1 v2]
  (let [mag-v1 (v/magnitude v1)
        mag-v2 (v/magnitude v2)
        denom  (* mag-v1 mag-v2)
        numer  (v/dot v1 v2)]
    (->> (/ numer denom)
         Math/acos
         )))

(def angle-to-x (partial angle-between x-axis))
(def angle-to-y (partial angle-between y-axis))
(def angle-to-z (partial angle-between z-axis))

(defn rotate-to [axis normal]
  (let [c (v/cross (v/normalize axis) (v/normalize normal)) ; just in case ;-)
        θ (->> c (v/magnitude) (Math/asin))]
    (partial rotate θ c)
    ))

(defn rotate-to-vector [axis normal vv]
  (let [c (v/cross (v/normalize axis) (v/normalize normal)) ; just in case ;-)
        θ (->> c (v/magnitude) (Math/asin))
        rotation (q/from-angle-axis θ c)]
    (q/rotate rotation vv)
    ))

; Algorithm to find a perpendicular vector thanks to:
; https://math.stackexchange.com/a/413235
; I have no fucking idea what I'm doing ⁻\_(ツ)_/⁻
(defn perpendicular-vector [v]
  (let [m-index (->> v
                     (map-indexed (fn [i vi] {:index i :non-zero (not= vi 0.0)}))
                     (filter (fn [x] (get x :non-zero)))
                     first
                     (#(get % :index)))
        n-index (mod (+ m-index 1) 3)
        o-index (mod (+ n-index 1) 3)
        perpendicular-v {m-index (- (get v n-index))
                         n-index (get v m-index)
                         o-index 0.0
                         }]
    (v/into-vector (for [i (range 0 3)] (get perpendicular-v i)))
    ))

(defn rotate-to-v [axis normal]
  (let [c (v/cross (v/normalize axis) (v/normalize normal)) ; just in case ;-)
        θ (->> c (v/magnitude) (Math/asin))
        rotation (q/from-angle-axis θ c)
        u (q/rotate rotation normal)
        m-index (->> u
                     (map-indexed (fn [i vi] {:index i :non-zero (not= vi 0.0)}))
                     (filter (fn [x] (get x :non-zero)))
                     first
                     (#(get % :index)))
        n-index (mod (+ m-index 1) 3)
        o-index (mod (+ n-index 1) 3)
        perpendicular-v {m-index (- (get u n-index))
                         n-index (get u m-index)
                         o-index 0.0
                         }]
    (for [i (range 0 3)] (get perpendicular-v i))
    ))

(def rotate-to-x (partial rotate-to x-axis))
(def rotate-to-y (partial rotate-to y-axis))
(def rotate-to-z (partial rotate-to z-axis))

(defn circle-embed-plane [plane-normal radius]
  (let [next-axis (rotate-to-v x-axis plane-normal)]
  ; (->> (cylinder radius 1)
  (->> (cube (* 2 radius) 1 1)
       ((rotate-to-x plane-normal))
       ; (rotate (/ π 4) next-axis)
       ; ((rotate-to-y plane-normal))
       ; ((rotate-to-z plane-normal))
       )))

(defn plane-align [plane-normal shape]
  (let [x-y-projection (v/vector (get plane-normal 0) (get plane-normal 1) 0)
        yaw-angle (angle-between x-axis x-y-projection)
        ; offset the angle by 2π if the rotation should be more than π radians
        ; (90 degrees)
        yaw-α (if (<= 0 (get plane-normal 1)) yaw-angle (- (* 2 π) yaw-angle))
        yaw-q (q/from-angle-axis yaw-α z-axis)
        yawed-x-axis (q/rotate yaw-q x-axis)
        roll-angle (angle-between yawed-x-axis plane-normal)
        ; offset the angle by 2π if the rotation should be more than π radians
        ; (90 degrees)
        roll-β (if (<= 0 (get plane-normal 2)) roll-angle (- (* 2 π) roll-angle))
        yawed-y-axis (q/rotate yaw-q (v/vector 0 -1 0))]
    (->> shape
         (rotate yaw-α z-axis)
         (rotate roll-β yawed-y-axis)
         )))

; embeds the shape in the perimeter of a circle on the plane with the given radius.
; TODO: shoud we rotate the shape into the orientation around the correct axis in this
; function, or leave that up to the caller? ⁻\_(ツ)_/⁻
(defn circle-embed [plane-normal circle-origin-normal radius chord-length index shape]
  (let [θ (chord-θ radius chord-length)
        to-origin (v/scale circle-origin-normal radius)
        rot-y-axis-1 (rotate-to-vector x-axis plane-normal y-axis)
        rot-z-axis-1 (rotate-to-vector x-axis plane-normal z-axis)
        rot-z-axis-2 (rotate-to-vector rot-y-axis-1 plane-normal rot-z-axis-1)]
    (->> shape
         (translate [0 0 (- radius)])
         (plane-align plane-normal)
         ; ((rotate-to rot-y-axis-1 plane-normal))
         ; ((rotate-to rot-z-axis-2 plane-normal))
         ; (translate to-origin)
         (rotate (* θ index) plane-normal)
         )))

(defn thumb-place [column row shape]
  (let [
        β (chord-θ thumb-row-radius mount-width)
        ]
    (->> shape
         (translate [0, 0, (- thumb-row-radius)])
         (rotate (+ (* β row) thumb-start-angle) [1 0 0]) 
         ; (translate [0 0 row-radius])
         ; (translate [0 0 (- column-radius)])
         ; (rotate (* column β) [0 1 0])
         ; (translate [0 0 column-radius])
         ; (translate [mount-width 0 0])
         ; (rotate (* π (- 1/4 3/16)) [0 0 1])
         ; (rotate (/ π 12) [1 1 0])
         ; (translate [-52 -45 40])
       )))

(def thumb-test (cube 15 10 5))

; plane-normal describes the plane in which the finger moves.
; origin of to-knuckle at approximately at the inner wrist...?
(defrecord Finger [length width semiminor plane-normal to-first-key])
(def index-finger (->Finger
                    79.9 ;length
                    18.5 ;width
                    50 ;semiminor
                    (v/normalize (v/vector 1 0.15 0.1)) ;plane-normal
                    ; [-11 0 0])) ;to-first-key
                    [-11 60 12])) ; to-first-key

(def middle-finger (->Finger
                     89 ;length
                     17.5 ; width
                     74 ; semiminor
                     (v/normalize (v/vector 1 0.1 0.1)) ;plane-normal
                     ; [11 0 0])) ;to-first-key
                     [12 55 6])) ;to-first-key

(def ring-finger (->Finger
                     86 ;length
                     17.5 ; width
                     57 ; semiminor
                     (v/normalize (v/vector 1 -0.1 -0.1)) ;plane-normal
                     [41 50 8])) ;to-first-key

(def pinky-finger (->Finger
                     69 ;length
                     17.5 ; width
                     44 ; semiminor
                     (v/normalize (v/vector 1 -0.2 -0.2)) ;plane-normal
                     [63 40 15])) ;to-first-key

(def fingers [index-finger middle-finger ring-finger pinky-finger])

(def switch-travel 4)
(def switch-travel-buffer 1)
(def ellipse-adjustment (+ switch-travel switch-travel-buffer 6))
(def initial-key-chord-angle (* π -0.75))
; assumes that the circumference of the ellipse should produce the chords which align with
; the top of the object being embedded. In order to do this it translates the input shape
; down in the z-axis by the provided height.
; So, assuming my horrible model for how a finger moves actually works, this should produce
; a thing where the finger naturally rests on the top of the object being placed.
(defn finger-key-place [shape-height shape-length shape finger row-idx]
  (let [semimajor (/ (* 4/3 (:length finger)) 2)
        semiminor (:semiminor finger)
        ellipse (e/->Ellipse semimajor semiminor)
        ; translate the ellipse so that, hypothetically, the ellipse we've
        ; just used should approximately intersect the centre of this finger's
        ; knuckle, at the angle π, (i.e. at y = 0, and x = -semimajor)
        ; All the weasel words, because I'm making this up as I go along.
        to-knuckle-translation (add [0 0 semiminor] (:to-first-key finger))]
    (->> shape
         (translate [0 0 (- shape-height)])
         ; The ellipse embedding does so in the x-y plane, but all the dactyl
         ; code works in terms of rotations in y-z about the x-axis
         ; so we need to pre-rotate the input shape to work with the ellipse
         ; embedding, then rotate again to have the ellipse be in the y-z plane.
         (rotate (/ π -2) [0 0 1])
         (rotate (/ π -2) [1 0 0])
         (#(e/embed ellipse initial-key-chord-angle shape-length % row-idx))
         (rotate (/ π 2) [1 0 0])
         (rotate (/ π 2) [0 0 1])
         (translate [0 semimajor 0])
         (plane-align (:plane-normal finger))
         (translate to-knuckle-translation)
         )
  ))

;copy pasta from source code
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

(def thumb-cluster
  (let [key-cap (translate [0 0 (+ switch-travel 8.8)] (sa-cap 1))
  ; (let [key-cap (translate [0 0 8.8] (sa-cap 1))
        translated-plate (translate [0 0 (- 8.3 plate-thickness)] single-plate)
        key-assembly (union
                       key-cap
                       translated-plate
                       cherry-switch
                       single-key-pcb)
        key-assembly-height (+ sa-profile-key-height
                               switch-travel
                               switch-travel-buffer
                               8.8) ; hacks... should be derived from switch spec.
        pseudo-height (- key-assembly-height ellipse-adjustment)
        pseudo-width (+ keycap-space-buffer sa-length)
        place-shape (partial finger-key-place pseudo-height pseudo-width)
        pcb-hull (comp (partial color pcb-color) triangle-hulls)
        ]
    (apply union
           (concat
             [key-assembly pcb-web-post-tl pcb-web-post-tr pcb-web-post-bl pcb-web-post-br]
             ; [(translate [0 0 (- pseudo-height)] key-assembly)]

             ; full assembly of key switch, plate, pcb, keycap.
             (for [finger fingers
                   row rows]
               (place-shape key-assembly finger row)
               )
                  
            ;; Row connections
            (for [[f1 f2] (partition 2 1 fingers)
                  row rows]
              (pcb-hull
                (place-shape pcb-web-post-tl f2 row)
                (place-shape pcb-web-post-tr f1 row)
                (place-shape pcb-web-post-bl f2 row)
                (place-shape pcb-web-post-br f1 row)
                ))

            ;; Column connections
            (for [finger fingers
                  row (range 0 lastrow)]
              (pcb-hull
                (place-shape pcb-web-post-tl finger row)
                (place-shape pcb-web-post-tr finger row)
                (place-shape pcb-web-post-bl finger (inc row))
                (place-shape pcb-web-post-br finger (inc row))
                ))

            ; ;; Diagonal connections
            (for [[f1 f2] (partition 2 1 fingers)
                  [r1 r2] (partition 2 1 rows)]
              (pcb-hull
                (place-shape pcb-web-post-tr f1 r1)
                (place-shape pcb-web-post-tl f2 r1)
                (place-shape pcb-web-post-br f1 r2)
                (place-shape pcb-web-post-bl f2 r2)
                ))
            ))))

; My Hacks: a template for moulding a PCB, maybe?
; (def model-pcb-mould-right (plane-align (v/vector 1 1 1) single-key-pcb))
(def model-pcb-mould-right (union
                       ; key-holes
                       ; key-pcb-connectors
                       ; pcb-thumb
                       thumb-cluster
                       thumb
                       ))
                       
(spit "things/right-pcb-mould.scad"
      (write-scad model-pcb-mould-right))

(spit "things/right.scad"
      (write-scad model-right))

(spit "things/left.scad"
      (write-scad (mirror [-1 0 0] model-right)))

(spit "things/right-test.scad"
      (write-scad
                   (union
                    key-holes
                    connectors
                    thumb
                    thumb-connectors
                    case-walls
                    thumbcaps
                    caps
                    teensy-holder
                    ; rj9-holder
                    ; usb-holder-hole
                    ; usb-holder-hole
                    ; ; teensy-holder-hole
                    ;             screw-insert-outers
                    ;             teensy-screw-insert-holes
                    ;             teensy-screw-insert-outers
                    ;             usb-cutout
                    ;             rj9-space
                                ; wire-posts
                  )))

(spit "things/right-plate.scad"
      (write-scad
                   (cut
                     (translate [0 0 -0.1]
                       (difference (union case-walls
                                          teensy-holder
                                          ; rj9-holder
                                          screw-insert-outers)
                                   (translate [0 0 -10] screw-insert-screw-holes))
                  ))))

(spit "things/test.scad"
      (write-scad
         (difference usb-holder usb-holder-hole)))



(defn -main [dum] 1)  ; dummy to make it easier to batch
