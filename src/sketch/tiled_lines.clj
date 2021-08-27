(ns sketch.tiled-lines
  (:require [quil.core :as q]
            [quil.applet :as qa]))

(def ^:dynamic *size* 640)

(defn w
  ([] (w 1.0))
  ([scale] (int (* *size* scale))))

(def ^:dynamic *random* clojure.core/rand)

(defn random
  ([] (random 1.0))
  ([n] (* (*random*) n)))

(defn setup []
  (q/frame-rate 1)
  (q/smooth)
  (q/color-mode :hsb 360 100 100 1.0))

(defn diagonal-line [x y w h]
  (if (< 0.5 (random))
    (q/line x y (+ x w) (+ y h))
    (q/line (+ x w) y x (+ y h))))

(defn tiled-lines []
  (let [n      20
        step   (/ *size* n)
        points (for [x (range n)
                     y (range n)]
                 [(* step x) (* step y)])]
    (doseq [[x y] points]
      (diagonal-line x y step step))))

(def ^:dynamic *seed* 1050879010481460)

(defn draw [size]
  (binding [*size*   size
            *random* (partial q/random 1.0)]
    (q/background 360)
    (q/random-seed *seed*)
    (tiled-lines)))

(defn create-sketch
  ([] (create-sketch *size*))  ; 640px
  ([size] (q/sketch
            :title "Sketch"
            :size [size size]
            :setup setup
            :draw (partial draw size))))

(defonce sketch (create-sketch))

(comment

  (def sketch (create-sketch))
  (def sketch (create-sketch 10800))
  (qa/with-applet sketch
    (q/save (format
              "img-%s-%s-%s.png"
              (System/currentTimeMillis)
              1050879010481460
              (q/width))))
  )
