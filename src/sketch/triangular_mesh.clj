(ns sketch.triangular-mesh
  "A Quil sketch based on generative artistry's tutorial - Triangular Mesh.
  See original post at
  https://generativeartistry.com/tutorials/triangular-mesh/"
  (:require [quil.core :as q]))

(def ^:dynamic *size* 640)
(def ^:dynamic *seed* 1050879010481460)

(defn w [n]
  (* *size* n))

(defn with-bindings [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn bindings []
                           (binding [*size* (q/width)]
                             (draw))))))

(defn with-re-seed [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn re-seed []
                           (q/random-seed *seed*)
                           (draw)))))

(defn setup []
  (q/frame-rate 1)
  (q/smooth)
  (q/stroke-weight (w (/ 1 320)))
  (q/color-mode :hsb 16))               ; To draw 16 shades of grays

(defn draw-triangle [p1 p2 p3]
  (q/begin-shape :triangle)
  (q/vertex ))

(defn draw []
  (q/background 16)
  (q/color-mode :hsb 16)
  (let [gap  (/ *size* 7)
        +gap (partial + gap)
        grid (for [i      (iterate inc 0)
                   :let   [y (+ (* i gap) (/ gap 2))]
                   :while (<= y *size*)]
               (for [x      (iterate +gap (/ gap 4))
                     :while (<= x *size*)]
                 [(+ x
                     (* (- (q/random 0.5) 0.25) gap)
                     (if (odd? i) 0 (/ gap 2)))
                  (+ y
                     (* (- (q/random 0.5) 0.25) gap))]))]
    (doseq [[line1 line2] (partition 2 1 grid)
            :let          [points (->> (concat line1 line2)
                                       (sort-by first))]]
      (q/begin-shape :triangle-strip)
      (doseq [[x y] points
              :let  [gray (Math/floor (q/random 16))]]
        (q/fill gray)
        (q/vertex x y))
      (q/end-shape))))

(defn create-sketch
  [{:keys [size]
    :or   {size *size*}}]
  (q/sketch :title "Sketch"
            :size [size size]
            :setup setup
            :draw #'draw
            :middleware [with-bindings with-re-seed]))

(defonce sketch (create-sketch {}))


(comment

  ;; Generate a new sketch
  (def sketch (create-sketch {}))

  ;; Save working image to disk
  (require '[quil.applet :as qa])
  (qa/with-applet sketch
    (q/save (format "img-%s-%s-%s.png" (System/currentTimeMillis) *seed* (q/width))))

  ;; Generate a production image (10800 x 10800 pixels)
  (create-sketch {:size  10800
                  :once? true})
  )
