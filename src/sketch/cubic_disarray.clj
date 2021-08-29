(ns sketch.cubic-disarray
  "A Quil sketch based on generative artistry's tutorial - Cubic Disarray.
  See original post at https://generativeartistry.com/tutorials/cubic-disarray/"
  (:require [quil.core :as q]
            [quil.applet :as qa]
            [quil.middleware :as qm]))

(def ^:dynamic *size* 640)
(def ^:dynamic *random* clojure.core/rand)
(def ^:dynamic *seed* 1050879010481460)

(defn w
  ([] (w 1))
  ([n] (int (* *size* n))))

(defn setup []
  (q/frame-rate 1)
  (q/smooth)
  (q/color-mode :hsb 360 100 100 1.0))

(defn draw-square [x y size]
  (let [rotate-amt    (* y
                         (/ 1 *size*)
                         Math/PI
                         1/180
                         (if (< (*random*) 0.5) -1 1)
                         (*random*)
                         20)            ; rotateMultiplier
        translate-amt (* y
                         (/ 1 *size*)
                         (if (< (*random*) 0.5) -1 1)
                         (*random*)
                         (w 0.02))]     ; randomDisplacement
    (q/with-translation [(+ x translate-amt) y]
      (q/with-rotation [rotate-amt]
        (q/rect (* -0.5 size)
                (* -0.5 size)
                size
                size)))))

(defn draw [size]
  (binding [*size*   size
            *random* (partial q/random 1.0)]
    (q/background 360)
    (q/random-seed *seed*)
    (let [square-size  (w 0.1)
          +square-size (partial + square-size)]
      (doseq [x      (iterate +square-size square-size)
              :while (<= x (- *size* square-size))]
        (doseq [y      (iterate +square-size square-size)
                :while (<= y (- *size* square-size))]
          (draw-square x y square-size))))))

(defn create-sketch
  [{:keys [size]
    :or   {size *size*}}]
  (q/sketch :title "Sketch"
            :size [size size]
            :setup setup
            :draw (partial #'draw size)))

(defonce sketch (create-sketch {}))

(comment

  ;; Generate a new sketch
  (def sketch (create-sketch {:size 320}))

  ;; Save working image to disk
  (qa/with-applet sketch
    (q/save (format "img-%s-%s.png" (System/currentTimeMillis) *seed*)))

  ;; Generate a production image (10800 x 10800 pixels)
  (create-sketch {:size  10800
                  :once? true})
  )
