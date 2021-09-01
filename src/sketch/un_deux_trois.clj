(ns sketch.un-deux-trois
  (:require [quil.core :as q]
            [quil.middleware :as qm]))

(def ^:dynamic *size* 640)
(def ^:dynamic *seed* 1050879010481460)

(defn w [n]
  (* *size* n))

(defn use-bindings [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn []
                           (binding [*size* (q/width)]
                             (draw))))))

(defn use-seed [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn []
                           (q/random-seed *seed*)
                           (draw)))))

(defn setup []
  (q/frame-rate 1)
  (q/smooth)
  (q/stroke-weight (w (/ 1 80)))
  (q/stroke-cap :round))

(defn draw []
  (q/background 360)
  (let [step (w (/ 1 16))
        half-step (w (/ 1 32))]
    (doseq [y (range step (- *size* step) step)
            :let [positions
                  (cond
                    (< y (w (/ 1 3))) [0.5]
                    (< y (w (/ 2 3))) [0.2 0.8]
                    :else [0.1 0.5 0.9])]]
      (doseq [x (range step (- *size* step) step)]
        (q/with-translation [(+ x half-step) (+ y half-step)]
          (q/with-rotation [(* (q/random 1.0) 5)]
            (q/with-translation [(* -1 half-step) (* -1 half-step)]
              (doseq [pos positions]
                (q/line [(* pos step) 0]
                        [(* pos step) step])))))))))

(defn create-sketch
  [{:keys [size]
    :or   {size *size*}}]
  (q/sketch :title "Sketch"
            :size [size size]
            :setup setup
            :draw #'draw
            :middleware [qm/pause-on-error use-bindings use-seed]))

(defonce sketch (create-sketch {}))

(comment
  (.exit sketch)

  ;; Save working image to disk
  (require '[quil.applet :as qa])
  (qa/with-applet sketch
    (q/save (format "img-%s-%s-%s.png" (System/currentTimeMillis) *seed* (q/width))))

  )
