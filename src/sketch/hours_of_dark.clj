(ns sketch.hours-of-dark
  (:require [sketch.core :as core]
            [quil.core :as q]))

(defn draw []
  (q/background 360)
  (let [cols 23
        rows 16
        days 365
        grid-w (core/w 0.9)
        grid-h (core/w 0.7)
        cell-w (/ grid-w cols)
        cell-h (/ grid-h rows)
        marg-x (* 0.5 (- core/*size* grid-w))
        marg-y (* 0.5 (- core/*size* grid-h))]
    (doseq [i (range days)]
      (let [col (Math/floor (/ i rows))
            row (mod i rows)
            x (+ marg-x (* col cell-w))
            y (+ marg-y (* row cell-h))
            w (core/w 1/320)
            h (core/w 30/320)
            phi (* q/PI (/ i days))
            theta (+ 0.85 (* 0.45 q/PI (q/sin phi)))
            scale (+ 1 (* 2 (q/abs (q/cos phi))))]
        (q/with-translation [x y]
          (q/clip 0 0 cell-w cell-h)
          (q/with-translation [(* cell-w 0.5) (* cell-h 0.5)]
            (q/with-rotation [theta]
              (q/scale scale 1)
              (q/fill 0)
              (q/rect (* w -0.5) (* h -0.5) w h)
              (q/scale (/ 1 scale) 1))))))))

(defonce sketch
  (core/create-sketch {:draw #'draw}))

(comment
  (core/save sketch)
  )
