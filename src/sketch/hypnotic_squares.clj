(ns sketch.hypnotic-squares
  (:require [sketch.core :as core]
            [quil.core :as q]))

(defn squares [x y w x-movement y-movement step steps]
  (when (pos? w)
    (q/rect x y w w)
    (let [w' (- w step)
          offset (/ (- w w') 2)
          x' (+ x offset)
          y' (+ y offset)
          x'-adjust (- x' (* (/ (- x x') (+ 2 steps)) x-movement))
          y'-adjust (- y' (* (/ (- y y') (+ 2 steps)) y-movement))]
      (squares x'-adjust y'-adjust w' x-movement y-movement step (dec steps)))))

(defn draw []
  (q/background 360)
  (let [final-size (core/w 3/320)
        offset (core/w 2/320)
        n-tiles 7
        tile-size (/ (- core/*size* (* 2 offset)) n-tiles)
        tiles (for [x (range offset (- core/*size* offset) tile-size)
                    y (range offset (- core/*size* offset) tile-size)]
                [x y])]
    (doseq [[x y] tiles]
     (let [steps     (+ 2 (Math/ceil (core/random 3)))
           step-size (Math/floor (/ (- tile-size final-size) steps))
           directions [-1 0 1]
           x-dir (nth directions (core/random (count directions)))
           y-dir (nth directions (core/random (count directions)))]
       (squares x y tile-size x-dir y-dir step-size steps)))))

(defonce sketch
  (core/create-sketch {:draw #'draw}))

(comment
  (core/save sketch)
  )
